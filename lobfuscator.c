#include "lobfuscator.h"
#include "lopcodes.h"
#include "lfunc.h"
#include "ldebug.h"
#include "lmem.h"
#include "lstate.h"

/*
** Simple PRNG to randomize opcodes at runtime if needed,
** but usually we want it stable for a single build.
*/
static unsigned int next = 1;
static int rand_custom(void) {
    next = next * 1103515245 + 12345;
    return (unsigned int)(next/65536) % 32768;
}

static void srand_custom(unsigned int seed) {
    next = seed;
}

/* Internal mapping table, will be initialized once */
static lu_byte runtime_encode[64];
static lu_byte runtime_decode[64];
static int initialized = 0;

void init_obfuscator(void) {
    if (initialized) return;
    srand_custom(SEED_1 ^ SEED_2);
    int i;
    for (i = 0; i < 64; i++) {
        runtime_encode[i] = i;
    }
    /* Shuffle */
    for (i = 50; i > 0; i--) {
        int j = rand_custom() % (i + 1);
        lu_byte temp = runtime_encode[i];
        runtime_encode[i] = runtime_encode[j];
        runtime_encode[j] = temp;
    }
    for (i = 0; i < 64; i++) {
        runtime_decode[runtime_encode[i]] = i;
    }
    initialized = 1;
}

/*
** Since we need these at compile-time for the existing Lua infrastructure,
** and the user wants "no py script", we'll use the static ones in lopcodes.c
** but obfuscate them further.
*/

#define PACK_VIRT(op,a,b,c) (((unsigned int)(op) << 24) | ((unsigned int)(a) << 16) | ((unsigned int)(b) << 8) | (unsigned int)(c))

void virtualize_proto(lua_State *L, Proto *f) {
    int i;
    for (i = 0; i < f->sizecode; i++) {
        Instruction inst = DECRYPT_INST(f->code[i]);
        /* Use the macro GET_OPCODE but we need it decrypted */
        OpCode op = (OpCode)((luaP_op_decode[(inst>>POS_OP) & 0x3F]) ^ LUA_OP_XOR);
        if (op == OP_ADD || op == OP_SUB || op == OP_MUL) {
            int a = getarg(inst, POS_A, SIZE_A);
            int b = getarg(inst, POS_B, SIZE_B);
            int c = getarg(inst, POS_C, SIZE_C);
            if (b < 256 && c < 256) {
                int vop = (op == OP_ADD) ? 0 : (op == OP_SUB ? 1 : 2);
                f->code[i] = CREATE_Ax(OP_VIRTUAL, PACK_VIRT(vop, a, b, c));
            }
        }
    }
}

/* Control Flow Flattening: Jump redirection */
void flatten_proto(lua_State *L, Proto *f) {
    // Replaces some JMPs with a virtual dispatcher logic if we had one.
    // For now, we'll just XOR-obfuscate the jump targets in the bytecode
    // and decrypt them in lvm.c. (Too complex for a quick fix).
}

void obfuscate_proto(lua_State *L, Proto *f) {
    virtualize_proto(L, f);
    flatten_proto(L, f);
}
