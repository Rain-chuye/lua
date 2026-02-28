#ifndef lobfuscator_h
#define lobfuscator_h

#include "llimits.h"
#include "lua.h"
#include "lobject.h"

#include "lopcodes.h"

/* Obfuscator functions */
void obfuscate_proto(lua_State *L, Proto *f);

#endif
