#ifndef lobfuscator_h
#define lobfuscator_h

#include "llimits.h"
#include "lua.h"
#include "lobject.h"

/* Helper to mix bits from __DATE__ string "Mmm dd yyyy" */
#define D_BIT(i) ((unsigned int)__DATE__[i])

#define LUA_OP_XOR    ((lu_byte)((D_BIT(0) ^ D_BIT(4) ^ D_BIT(10)) & 0x3F))
#define LUA_INST_KEY  (0xDEADBEEFU ^ (D_BIT(0) << 24) ^ (D_BIT(7) << 16) ^ (D_BIT(10) << 8))
#define LUA_CONST_XOR ((lu_byte)((D_BIT(0) ^ D_BIT(1) ^ D_BIT(2) ^ 0xA5) & 0xFFU))

/* Encryption/Decryption macros for instructions */
#define ENCRYPT_INST(i) ((i) ^ (Instruction)LUA_INST_KEY)
#define DECRYPT_INST(i) ((i) ^ (Instruction)LUA_INST_KEY)

/* Obfuscator functions */
void obfuscate_proto(lua_State *L, Proto *f);

#endif
