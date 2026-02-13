#ifndef lobfuscator_h
#define lobfuscator_h

#include "llimits.h"
#include "lua.h"
#include "lobject.h"

/*
** Dynamic Key generation using compile-time constants.
** These will change whenever the file is recompiled.
*/
#define SEED_1 ((unsigned int)(__TIME__[7] + __TIME__[6]*10 + __TIME__[4]*60 + __TIME__[3]*600))
#define SEED_2 ((unsigned int)(__DATE__[0] + __DATE__[1] + __DATE__[2]))

#define LUA_INST_KEY (0xDEADBEEF ^ SEED_1 ^ (SEED_2 << 16))
#define LUA_OP_XOR ((lu_byte)(0x2A ^ (SEED_1 & 0x3F)))

#define ENCRYPT_INST(i) ((i) ^ LUA_INST_KEY)
#define DECRYPT_INST(i) ((i) ^ LUA_INST_KEY)

/* Obfuscator functions */
void obfuscate_proto(lua_State *L, Proto *f);

#endif
