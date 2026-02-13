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

void obfuscate_proto(lua_State *L, Proto *f, int is_dumping) {
    int i;
    if (f->is_obfuscated) return;

    /* Instruction Virtualization */
    virtualize_proto_internal(f);

    /* Metadata Stripping */
    f->source = luaS_new(L, "=[obfuscated]");
    if (f->locvars) {
        for (i = 0; i < f->sizelocvars; i++) {
            f->locvars[i].varname = luaS_new(L, "");
        }
    }
    if (f->upvalues) {
        for (i = 0; i < f->sizeupvalues; i++) {
            f->upvalues[i].name = luaS_new(L, "");
        }
    }
    if (f->lineinfo) {
        for (i = 0; i < f->sizelineinfo; i++) f->lineinfo[i] = 0;
    }

    /* Control Flow Obfuscation (Light)
       OpCode Randomization and Instruction Encryption already provide
       significant control flow obfuscation by hiding the instruction types.
    */

    for (i = 0; i < f->sizep; i++) {
        obfuscate_proto(L, f->p[i], is_dumping);
    }

    f->is_obfuscated = 1;
}
