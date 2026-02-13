#ifndef lobfuscator_h
#define lobfuscator_h

#include "llimits.h"
#include "lua.h"
#include "lobject.h"

/*
** Commercial Obfuscation Keys
*/
#define LUA_INST_KEY  0xAB8271C3U
#define LUA_OP_XOR    0x1DU
#define LUA_CONST_XOR 0x8F

#define ENCRYPT_INST(i) ((i) ^ LUA_INST_KEY)
#define DECRYPT_INST(i) ((i) ^ LUA_INST_KEY)

/* Obfuscator functions */
void obfuscate_proto(lua_State *L, Proto *f, int encrypt_strings);

#endif
