import random
import re

def randomize():
    with open('lopcodes.h', 'r') as f:
        content = f.read()

    # Find the OpCode enum
    match = re.search(r'typedef enum \{(.*?)\} OpCode;', content, re.DOTALL)
    if not match:
        print("Could not find OpCode enum")
        return

    enum_content = match.group(1)
    opcodes = []
    for line in enum_content.split('\n'):
        line = line.strip()
        if not line: continue
        if line.startswith('OP_'):
            op = line.split(',')[0].strip()
            if '/*' in op:
                op = op.split('/*')[0].strip()
            opcodes.append(op)

    num_opcodes = len(opcodes)
    shuffled_indices = list(range(num_opcodes))
    random.shuffle(shuffled_indices)

    XOR_KEY = random.randint(0, 63)
    INST_KEY = random.randint(0, 0xFFFFFFFF)

    with open('lobfuscator.h', 'w') as f:
        f.write(f'#ifndef lobfuscator_h\n')
        f.write(f'#define lobfuscator_h\n\n')
        f.write(f'#include "llimits.h"\n')
        f.write(f'#include "lua.h"\n')
        f.write(f'#include "lobject.h"\n\n')
        f.write(f'#define LUA_INST_KEY {hex(INST_KEY)}U\n\n')
        f.write(f'#define ENCRYPT_INST(i) ((i) ^ LUA_INST_KEY)\n')
        f.write(f'#define DECRYPT_INST(i) ((i) ^ LUA_INST_KEY)\n\n')
        f.write(f'void obfuscate_proto(lua_State *L, Proto *f);\n\n')
        f.write(f'#endif\n')

    with open('lopcodes.h', 'r') as f:
        h_content = f.read()
    h_content = re.sub(r'#define LUA_OP_XOR 0x[0-9A-Fa-f]+', f'#define LUA_OP_XOR {hex(XOR_KEY)}', h_content)
    with open('lopcodes.h', 'w') as f:
        f.write(h_content)

    encode = [0] * 64
    decode = [0] * 64

    for original_idx, shuffled_idx in enumerate(shuffled_indices):
        decode[shuffled_idx] = original_idx ^ XOR_KEY
        encode[original_idx ^ XOR_KEY] = shuffled_idx

    # Generate new lopcodes.c parts
    encode_str = ", ".join(map(str, encode))
    decode_str = ", ".join(map(str, decode))

    with open('lopcodes.c', 'r') as f:
        c_content = f.read()

    c_content = re.sub(r'const lu_byte luaP_op_encode\[64\] = \{.*?\};',
                       f'const lu_byte luaP_op_encode[64] = {{{encode_str}}};',
                       c_content, flags=re.DOTALL)
    c_content = re.sub(r'const lu_byte luaP_op_decode\[64\] = \{.*?\};',
                       f'const lu_byte luaP_op_decode[64] = {{{decode_str}}};',
                       c_content, flags=re.DOTALL)

    with open('lopcodes.c', 'w') as f:
        f.write(c_content)

    print(f"Randomized {num_opcodes} opcodes and applied XOR obfuscation (Key: 0x2A).")

if __name__ == "__main__":
    randomize()
