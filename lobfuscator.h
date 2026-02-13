#ifndef lobfuscator_h
#define lobfuscator_h

#include "llimits.h"
#include "lua.h"
#include "lobject.h"

/*
** Obfuscation Keys
*/
#define LUA_INST_KEY 0x55555555U
#define LUA_OP_XOR 0x12U

#define ENCRYPT_INST(i) ((i) ^ LUA_INST_KEY)
#define DECRYPT_INST(i) ((i) ^ LUA_INST_KEY)

/* Obfuscator functions */
void obfuscate_proto(lua_State *L, Proto *f);

#endif
