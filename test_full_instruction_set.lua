local Obfuscator = require("obfuscator_engine")
local source = [[
-- LOADK, GETVAR, SETVAR
local a = 10
local b = 20
a = a + b

-- GETTABLE, SETTABLE, NEWTABLE, SETTABLE_IMM
local t = { x = 1, [2] = "y" }
t.z = 3
print(t.x, t[2], t.z)

-- SETTABLE_MULTI, CALL_M
local function multi() return 4, 5 end
local t2 = { 0, multi() }
print(t2[1], t2[2], t2[3])

-- CALL, RET
local function add(x, y) return x + y end
print(add(a, b))

-- RET_M
local function multi_ret() return multi() end
local r1, r2 = multi_ret()
print(r1, r2)

-- VARARG, VARARG_M, LOAD_VA
local function va(first, ...)
    local args = {...}
    print(first, #args, args[1])
    local function inner(...)
        return ...
    end
    return inner(...)
end
print(va("va_test", 7, 8))

-- JMP, JMP_IF_FALSE, JMP_IF_TRUE, EQ, NE, LT, GT, LE, GE
if a == 30 and b ~= 10 then
    print("Compare OK")
end
if a < 10 or a > 20 then
    print("Compare 2 OK")
end

-- ADD, SUB, MUL, DIV, MOD, POW, IDIV
print(1+2, 5-3, 2*3, 10/2, 10%3, 2^3, 10//3)

-- BOR, BXOR, BAND, SHL, SHR, BNOT
print(1|2, 1~3, 7&3, 1<<2, 8>>2, ~1)

-- CONCAT, NOT, LEN, UNM
print("a".."b", not false, #"abc", -5)

-- CLOSURE (already tested but good to have)
local x = 0
local function inc() x = x + 1; return x end
print(inc(), inc())

-- BREAK, While, ForRange (desugared to While)
for i=1, 10 do
    if i == 3 then break end
    print("loop", i)
end

-- DUP, SWAP, POP, PICK_RESULT (implicitly used in assignments and logic)
local p1, p2 = 1, 2
p1, p2 = p2, p1
print(p1, p2)
]]

local ok, result = pcall(Obfuscator.obfuscate, source)
if not ok then
    print("Obfuscation failed: " .. tostring(result))
else
    local f, err = load(result)
    if not f then
        print("Load failed: " .. tostring(err))
    else
        local ok2, err2 = pcall(f)
        if not ok2 then
            print("Run failed: " .. tostring(err2))
        else
            print("Run successful!")
        end
    end
end
