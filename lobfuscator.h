#ifndef lobfuscator_h
#define lobfuscator_h

#include "llimits.h"
#include "lua.h"
#include "lobject.h"

/* Fixed keys for consistency */
#define LUA_OP_XOR    0
#define LUA_INST_KEY  0x7B2D3F5AU
#define LUA_CONST_XOR 0x5C

/* Encryption/Decryption macros for instructions */
#define ENCRYPT_INST(i) ((i) ^ (Instruction)LUA_INST_KEY)
#define DECRYPT_INST(i) ((i) ^ (Instruction)LUA_INST_KEY)

/* Obfuscator functions */
void obfuscate_proto(lua_State *L, Proto *f);

#endif
