#include "lobfuscator.h"
#include "lopcodes.h"
#include "lfunc.h"
#include "ldebug.h"
#include "lmem.h"
#include "lstate.h"
#include "lstring.h"

#define PACK_VIRT(op,a,b,c) (((unsigned int)(op) << 24) | ((unsigned int)(a) << 16) | ((unsigned int)(b) << 8) | (unsigned int)(c))

static void virtualize_proto_internal(lua_State *L, Proto *f) {
    int i;
    for (i = 0; i < f->sizecode; i++) {
        Instruction inst = DECRYPT_INST(f->code[i]);
        lu_byte op_val = cast(lu_byte, (inst >> POS_OP) & 0x3F);
        OpCode op = cast(OpCode, (luaP_op_decode[op_val]) ^ LUA_OP_XOR);

        if (op == OP_ADD || op == OP_SUB || op == OP_MUL || op == OP_BAND || op == OP_BOR || op == OP_BXOR) {
            int a = getarg(inst, POS_A, SIZE_A);
            int b = getarg(inst, POS_B, SIZE_B);
            int c = getarg(inst, POS_C, SIZE_C);
            if (b < 256 && c < 256) {
                int vop;
                switch(op) {
                    case OP_ADD: vop = 0; break;
                    case OP_SUB: vop = 1; break;
                    case OP_MUL: vop = 2; break;
                    case OP_BAND: vop = 3; break;
                    case OP_BOR: vop = 4; break;
                    case OP_BXOR: vop = 5; break;
                    default: vop = 0;
                }
                f->code[i] = CREATE_Ax(OP_VIRTUAL, PACK_VIRT(vop, a, b, c));
            }
        }
    }
    for (i = 0; i < f->sizep; i++) {
        virtualize_proto_internal(L, f->p[i]);
    }
}

void obfuscate_proto(lua_State *L, Proto *f, int encrypt_k) {
    /* String encryption now handled directly in ldump.c/lundump.c */
    virtualize_proto_internal(L, f);
}
