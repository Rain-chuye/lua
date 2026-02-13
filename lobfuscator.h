#ifndef lobfuscator_h
#define lobfuscator_h

#include "llimits.h"
#include "lua.h"
#include "lobject.h"

/*
** Dynamic Key generation using compile-time constants.
** These will change whenever the files are recompiled.
** We use __TIME__ and __DATE__ to ensure entropy per build.
*/
#define SEED_TIME ((unsigned int)(__TIME__[7] + __TIME__[6]*10 + __TIME__[4]*60 + __TIME__[3]*600))
#define SEED_DATE ((unsigned int)(__DATE__[0] + __DATE__[1] + __DATE__[2] + __DATE__[4]))

#define LUA_INST_KEY  (0xAB8271C3U ^ SEED_TIME ^ (SEED_DATE << 16))
#define LUA_OP_XOR    ((lu_byte)(0x1DU ^ (SEED_TIME & 0x3F)))
#define LUA_CONST_XOR ((lu_byte)(0x8FU ^ (SEED_DATE & 0xFF)))

#define ENCRYPT_INST(i) ((i) ^ LUA_INST_KEY)
#define DECRYPT_INST(i) ((i) ^ LUA_INST_KEY)

/* Obfuscator functions */
void obfuscate_proto(lua_State *L, Proto *f, int encrypt_strings);

#endif
