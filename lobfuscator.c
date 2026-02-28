#include "lobfuscator.h"
#include "lopcodes.h"
#include "lstring.h"
#include "lmem.h"
#include <stdlib.h>
#include <string.h>

static void virtualize_proto_internal(Proto *f) {
    int i;
    for (i = 0; i < f->sizecode; i++) {
        Instruction *pi = &f->code[i];
        OpCode op = GET_OPCODE(*pi);

        switch (op) {
            case OP_ADD: SET_OPCODE(*pi, OP_VADD); break;
            case OP_SUB: SET_OPCODE(*pi, OP_VSUB); break;
            case OP_MUL: SET_OPCODE(*pi, OP_VMUL); break;
            case OP_BAND: SET_OPCODE(*pi, OP_VAND); break;
            case OP_BOR: SET_OPCODE(*pi, OP_VOR); break;
            case OP_BXOR: SET_OPCODE(*pi, OP_VXOR); break;
            default: break;
        }
    }
}

static void flatten_2parts(lua_State *L, Proto *f) {
    if (f->sizecode < 6 || f->sizecode > 1000) return;

    int oldsize = f->sizecode;
    int mid = oldsize / 2;

    while (mid > 0 && mid < oldsize) {
        Instruction inst = f->code[mid - 1];
        OpCode op = GET_OPCODE(inst);
        int forbidden = 0;
        if (testTMode(luaP_opmodes[op])) forbidden = 1;
        else if (op == OP_TFORCALL) forbidden = 1;
        else if (op == OP_LOADKX) forbidden = 1;
        else if (op == OP_SETLIST && GETARG_C(inst) == 0) forbidden = 1;

        if (forbidden) mid++;
        else break;
    }
    if (mid >= oldsize - 1) return;

    int newsize = oldsize + 3;
    Instruction *nc = luaM_newvector(L, newsize, Instruction);
    int *old_to_new = (int *)malloc((oldsize + 1) * sizeof(int));
    if (!old_to_new) return;

    int i;
    int n2 = oldsize - mid;
    for (i = 0; i < mid; i++) old_to_new[i] = i + n2 + 2;
    for (i = mid; i < oldsize; i++) old_to_new[i] = i - mid + 1;
    old_to_new[oldsize] = newsize;

    Instruction j_start = 0;
    SET_OPCODE(j_start, OP_JMP);
    SETARG_sBx(j_start, old_to_new[0] - 1);
    nc[0] = j_start;

    for (i = mid; i < oldsize; i++) nc[old_to_new[i]] = f->code[i];

    Instruction j_end = 0;
    SET_OPCODE(j_end, OP_JMP);
    SETARG_sBx(j_end, newsize - (n2 + 2));
    nc[n2 + 1] = j_end;

    for (i = 0; i < mid; i++) nc[old_to_new[i]] = f->code[i];

    Instruction j_p2 = 0;
    SET_OPCODE(j_p2, OP_JMP);
    SETARG_sBx(j_p2, old_to_new[mid] - (oldsize + 3));
    nc[oldsize + 2] = j_p2;

    for (i = 0; i < newsize; i++) {
        Instruction d = nc[i];
        OpCode op = GET_OPCODE(d);
        if (op == OP_JMP || op == OP_FORLOOP || op == OP_FORPREP || op == OP_TFORLOOP) {
            int orig_pc = -1;
            int k;
            for (k = 0; k <= oldsize; k++) {
                if (old_to_new[k] == i) { orig_pc = k; break; }
            }
            if (orig_pc != -1) {
                int offset = GETARG_sBx(d);
                int orig_target = orig_pc + 1 + offset;
                if (orig_target >= 0 && orig_target <= oldsize) {
                    int new_target = old_to_new[orig_target];
                    int new_offset = new_target - (i + 1);
                    SETARG_sBx(nc[i], new_offset);
                }
            }
        }
    }

    luaM_freearray(L, f->code, oldsize);
    f->code = nc;
    f->sizecode = newsize;
    free(old_to_new);
}

void obfuscate_proto(lua_State *L, Proto *f) {
    int i;
    if (f->is_obfuscated) return;

    f->source = luaS_new(L, "=[混淆代码]");
    if (f->locvars) {
        luaM_freearray(L, f->locvars, f->sizelocvars);
        f->locvars = NULL;
    }
    f->sizelocvars = 0;
    if (f->upvalues) {
        for (i = 0; i < f->sizeupvalues; i++) f->upvalues[i].name = NULL;
    }
    if (f->lineinfo) {
        luaM_freearray(L, f->lineinfo, f->sizelineinfo);
        f->lineinfo = NULL;
    }
    f->sizelineinfo = 0;
    f->linedefined = 0;
    f->lastlinedefined = 0;

    virtualize_proto_internal(f);

    if (f->sizecode >= 6 && f->sizecode <= 1000) {
        flatten_2parts(L, f);
    }

    for (i = 0; i < f->sizecode; i++) {
        Instruction inst = f->code[i];
        OpCode op = GET_OPCODE(inst);
        SET_OPCODE_I(inst, op);
        f->code[i] = ENCRYPT_INST(inst);
    }

    f->is_obfuscated = 1;

    for (i = 0; i < f->sizep; i++) {
        obfuscate_proto(L, f->p[i]);
    }
}
