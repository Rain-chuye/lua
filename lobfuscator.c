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
        OpCode op = (OpCode)((luaP_op_decode[(inst>>POS_OP) & 0x3F]) ^ LUA_OP_XOR);
        if (op == OP_ADD || op == OP_SUB || op == OP_MUL) {
            int a = getarg(inst, POS_A, SIZE_A);
            int b = getarg(inst, POS_B, SIZE_B);
            int c = getarg(inst, POS_C, SIZE_C);
            // Only virtualize if B and C fit in 8 bits and are not constants
            if (b < 256 && c < 256) {
                int vop = (op == OP_ADD) ? 0 : (op == OP_SUB ? 1 : 2);
                f->code[i] = CREATE_Ax(OP_VIRTUAL, PACK_VIRT(vop, a, b, c));
            }
        }
    }
}

void flatten_proto(lua_State *L, Proto *f) {
    // Basic Control Flow Obfuscation:
    // This is partially handled by the Opcode Randomization and Dynamic Decryption.
}

void obfuscate_proto(lua_State *L, Proto *f) {
    virtualize_proto(L, f);
    flatten_proto(L, f);
}
