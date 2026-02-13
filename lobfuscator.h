#ifndef lobfuscator_h
#define lobfuscator_h

#include "llimits.h"
#include "lua.h"
#include "lobject.h"

/*
** Commercial Dynamic Keys
*/
#define S1 ((unsigned int)(__DATE__[0] ^ 0x12))
#define S2 ((unsigned int)(__DATE__[1] ^ 0x34))
#define S3 ((unsigned int)(__DATE__[2] ^ 0x56))

#define LUA_INST_KEY  0x0U
#define LUA_OP_XOR    0x0U
#define LUA_CONST_XOR ((lu_byte)((S1 ^ S2 ^ S3 ^ 0xA5) & 0xFF))

#define ENCRYPT_INST(i) ((i) ^ (Instruction)LUA_INST_KEY)
#define DECRYPT_INST(i) ((i) ^ (Instruction)LUA_INST_KEY)

/* Obfuscator functions */
void obfuscate_proto(lua_State *L, Proto *f, int is_dumping);

#endif
