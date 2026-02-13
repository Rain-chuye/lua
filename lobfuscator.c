#include "lobfuscator.h"
#include "lopcodes.h"
#include "lfunc.h"
#include "ldebug.h"
#include "lmem.h"
#include "lstate.h"

#define PACK_VIRT(op,a,b,c) (((unsigned int)(op) << 24) | ((unsigned int)(a) << 16) | ((unsigned int)(b) << 8) | (unsigned int)(c))

void virtualize_proto(lua_State *L, Proto *f) {
    int i;
    for (i = 0; i < f->sizecode; i++) {
        Instruction inst = DECRYPT_INST(f->code[i]);
        lu_byte op_val = cast(lu_byte, (inst >> POS_OP) & 0x3F);
        OpCode op = cast(OpCode, (luaP_op_decode[op_val]) ^ LUA_OP_XOR);

        if (op == OP_ADD || op == OP_SUB || op == OP_MUL) {
            int a = getarg(inst, POS_A, SIZE_A);
            int b = getarg(inst, POS_B, SIZE_B);
            int c = getarg(inst, POS_C, SIZE_C);
            /* Only virtualize if B and C are registers ( < 256 ) */
            if (b < 256 && c < 256) {
                int vop = (op == OP_ADD) ? 0 : (op == OP_SUB ? 1 : 2);
                f->code[i] = CREATE_Ax(OP_VIRTUAL, PACK_VIRT(vop, a, b, c));
            }
        }
    }
    for (i = 0; i < f->sizep; i++) {
        virtualize_proto(L, f->p[i]);
    }
}

void obfuscate_proto(lua_State *L, Proto *f) {
    virtualize_proto(L, f);
}
