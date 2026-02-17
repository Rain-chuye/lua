local Obfuscator = require("obfuscator_engine")
local source = [[
local t = { 1, 2, 3 }
local sum = 0
for i=1, #t do sum = sum + t[i] end
print("Sum is", sum)

local function factorial(n)
    if n == 0 then return 1 else return n * factorial(n-1) end
end
print("Fact(5)", factorial(5))

local a, b = 10, 20
a, b = b, a
print("Swap", a, b)

local s = "Hello" .. " " .. "World"
if #s > 5 and s:match("World") then
    print("String OK")
end
]]

local res = Obfuscator.obfuscate(source)
local f = load(res)
f()
