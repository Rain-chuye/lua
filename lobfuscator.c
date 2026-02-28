#include "lobfuscator.h"
#include "lopcodes.h"
#include "lfunc.h"
#include "ldebug.h"
#include "lmem.h"
#include "lstate.h"
#include "lstring.h"
#include <string.h>

#define GET_OPCODE_PLAIN(i)	(cast(OpCode, (luaP_op_decode[cast(lu_byte, ((i)>>POS_OP) & MASK1(SIZE_OP,0))]) ^ LUA_OP_XOR))

static void virtualize_proto_internal(lua_State *L, Proto *f) {
    if (f->sizecode == 0) return;

    int old_sizecode = f->sizecode;

    lu_byte *is_target = luaM_newvector(L, old_sizecode, lu_byte);
    memset(is_target, 0, old_sizecode);

    // 1. Identify jump targets
    for (int i = 0; i < old_sizecode; i++) {
        Instruction inst = f->code[i];
        OpCode op = GET_OPCODE(inst);
        if (op == OP_JMP || op == OP_FORLOOP || op == OP_FORPREP || op == OP_TFORLOOP) {
            int target = i + 1 + GETARG_sBx(inst);
            if (target >= 0 && target < old_sizecode) is_target[target] = 1;
        }
        if (op == OP_EQ || op == OP_LT || op == OP_LE || op == OP_TEST || op == OP_TESTSET || op == OP_TFORCALL) {
            if (i + 1 < old_sizecode) is_target[i + 1] = 1;
        }
        if (op == OP_LOADKX || op == OP_SETLIST) {
            if (op == OP_LOADKX || (op == OP_SETLIST && GETARG_C(inst) == 0)) {
                if (i + 1 < old_sizecode) is_target[i + 1] = 1;
            }
        }
    }

    Instruction *temp_vcode = luaM_newvector(L, old_sizecode * 2, Instruction);
    int vcode_ptr = 0;

    for (int i = 0; i < old_sizecode; ) {
        int start = i;
        int count = 0;
        while (i < old_sizecode) {
            Instruction inst = f->code[i];
            OpCode op = GET_OPCODE(inst);

            // Strictly safe instructions that never yield or skip instructions
            int is_safe = (op == OP_MOVE || op == OP_LOADK || op == OP_LOADNIL ||
                           op == OP_GETUPVAL || op == OP_SETUPVAL);

            if (!is_safe) break;
            if (i > start && is_target[i]) break;

            i++;
            count++;
            if (count >= 255) break;
        }

        if (count > 1 && vcode_ptr < (1 << 26)) {
            int vindex = vcode_ptr;
            temp_vcode[vcode_ptr++] = (Instruction)count;
            for (int j = 0; j < count; j++) {
                temp_vcode[vcode_ptr++] = f->code[start + j];
            }
            f->code[start] = CREATE_Ax(OP_VIRTUAL, vindex);
            for (int j = 1; j < count; j++) {
                f->code[start + j] = CREATE_Ax(OP_EXTRAARG, 0);
            }
        } else {
            i = start + 1;
        }
    }

    if (vcode_ptr > 0) {
        f->vcode = luaM_newvector(L, vcode_ptr, Instruction);
        memcpy(f->vcode, temp_vcode, vcode_ptr * sizeof(Instruction));
        f->sizevcode = vcode_ptr;
    }

    luaM_freearray(L, temp_vcode, old_sizecode * 2);
    luaM_freearray(L, is_target, old_sizecode);
}

void obfuscate_proto(lua_State *L, Proto *f, int encrypt_k) {
    if (f->obfuscated) return;

    // Increase stack size to provide slots for dynamic decryption (RK values)
    if (f->maxstacksize <= 253) f->maxstacksize += 2;
    else f->maxstacksize = 255;

    if (encrypt_k) {
        for (int i = 0; i < f->sizek; i++) {
            TValue *o = &f->k[i];
            if (ttisinteger(o)) {
                o->value_.i = ENCRYPT_INT(o->value_.i);
            } else if (ttisfloat(o)) {
                o->value_.n = ENCRYPT_FLT_VAL(o->value_.n);
            }
        }
    }

    virtualize_proto_internal(L, f);
    f->obfuscated = 1;

    for (int i = 0; i < f->sizep; i++) {
        obfuscate_proto(L, f->p[i], encrypt_k);
    }
}
