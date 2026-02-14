#include "lobfuscator.h"
#include "lopcodes.h"
#include "lstring.h"
#include "lmem.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

static void virtualize_proto_internal(Proto *f) {
    int i;
    for (i = 0; i < f->sizecode; i++) {
        Instruction *pi = &f->code[i];
        OpCode op = GET_OPCODE_I(*pi);

        switch (op) {
            case OP_ADD: SET_OPCODE_I(*pi, OP_VADD); break;
            case OP_SUB: SET_OPCODE_I(*pi, OP_VSUB); break;
            case OP_MUL: SET_OPCODE_I(*pi, OP_VMUL); break;
            case OP_BAND: SET_OPCODE_I(*pi, OP_VAND); break;
            case OP_BOR: SET_OPCODE_I(*pi, OP_VOR); break;
            case OP_BXOR: SET_OPCODE_I(*pi, OP_VXOR); break;
            default: break;
        }
    }
}

static void flatten_2parts(lua_State *L, Proto *f) {
    if (f->sizecode < 6 || f->sizecode > 1000) return;

    int oldsize = f->sizecode;
    int mid = oldsize / 2;

    /* Ensure we don't split between instruction pairs */
    while (mid > 0 && mid < oldsize) {
        Instruction inst = f->code[mid - 1];
        OpCode op = GET_OPCODE_I(inst);
        int forbidden = 0;
        if (testTMode(luaP_opmodes[op])) forbidden = 1;
        else if (op == OP_TFORCALL) forbidden = 1;
        else if (op == OP_LOADKX) forbidden = 1;
        else if (op == OP_SETLIST && GETARG_C_I(inst) == 0) forbidden = 1;
        else if (op == OP_TFOREACH) forbidden = 1; /* Custom foreach prep */

        if (forbidden) mid++;
        else break;
    }
    if (mid >= oldsize - 1) return; /* Too close to end */

    int newsize = oldsize + 3;
    Instruction *nc = luaM_newvector(L, newsize, Instruction);
    int *old_to_new = luaM_newvector(L, oldsize + 1, int);

    int i;
    int n2 = oldsize - mid;
    /*
    ** New Layout:
    ** 0: JMP to Part 1 (nc[n2+2])
    ** 1 .. n2: Part 2 (orig [mid .. oldsize-1])
    ** n2+1: JMP to End (nc[newsize])
    ** n2+2 .. oldsize+1: Part 1 (orig [0 .. mid-1])
    ** oldsize+2: JMP to Part 2 (nc[1])
    */
    for (i = 0; i < mid; i++) old_to_new[i] = i + n2 + 2;
    for (i = mid; i < oldsize; i++) old_to_new[i] = i - mid + 1;
    old_to_new[oldsize] = newsize;

    /* nc[0] jumps to nc[old_to_new[0]] */
    Instruction j_start = 0;
    SET_OPCODE_I(j_start, OP_JMP);
    SETARG_sBx_I(j_start, old_to_new[0] - (0 + 1));
    nc[0] = j_start;

    /* Copy Part 2 */
    for (i = mid; i < oldsize; i++) nc[old_to_new[i]] = f->code[i];

    /* Jump from end of Part 2 to End */
    Instruction j_end = 0;
    SET_OPCODE_I(j_end, OP_JMP);
    SETARG_sBx_I(j_end, newsize - (n2 + 1 + 1));
    nc[n2 + 1] = j_end;

    /* Copy Part 1 */
    for (i = 0; i < mid; i++) nc[old_to_new[i]] = f->code[i];

    /* Jump from end of Part 1 to start of Part 2 */
    Instruction j_p2 = 0;
    SET_OPCODE_I(j_p2, OP_JMP);
    SETARG_sBx_I(j_p2, old_to_new[mid] - (oldsize + 2 + 1));
    nc[oldsize + 2] = j_p2;

    /* Fix all relative jumps */
    for (i = 0; i < newsize; i++) {
        Instruction d = nc[i];
        OpCode op = GET_OPCODE_I(d);
        if (op == OP_JMP || op == OP_FORLOOP || op == OP_FORPREP || op == OP_TFORLOOP || op == OP_TFOREACH) {
            int orig_pc = -1;
            int k;
            for (k = 0; k <= oldsize; k++) {
                if (old_to_new[k] == i) { orig_pc = k; break; }
            }
            if (orig_pc != -1) {
                int offset = GETARG_sBx_I(d);
                int orig_target = orig_pc + 1 + offset;
                if (orig_target >= 0 && orig_target <= oldsize) {
                    int new_target = old_to_new[orig_target];
                    int new_offset = new_target - (i + 1);
                    SETARG_sBx_I(nc[i], new_offset);
                }
            }
        }
    }

    luaM_freearray(L, f->code, oldsize);
    f->code = nc;
    f->sizecode = newsize;
    luaM_freearray(L, old_to_new, oldsize + 1);
}

void obfuscate_proto(lua_State *L, Proto *f) {
    int i;
    if (f->is_obfuscated) return;

    /* Metadata stripping */
    f->source = luaS_new(L, "=[混淆代码]");
    f->sizelocvars = 0;
    f->linedefined = 0;
    f->lastlinedefined = 0;
    if (f->upvalues) {
        for (i = 0; i < f->sizeupvalues; i++) f->upvalues[i].name = NULL;
    }
    f->sizelineinfo = 0;
    f->lineinfo = NULL;

    /* Decrypt for processing */
    for (i = 0; i < f->sizecode; i++) f->code[i] = DECRYPT_INST(f->code[i]);

    /* Instruction Virtualization */
    virtualize_proto_internal(f);

    /* Flattening */
    flatten_2parts(L, f);

    /* Re-encrypt */
    for (i = 0; i < f->sizecode; i++) f->code[i] = ENCRYPT_INST(f->code[i]);

    for (i = 0; i < f->sizep; i++) {
        obfuscate_proto(L, f->p[i]);
    }

    f->is_obfuscated = 1;
}
