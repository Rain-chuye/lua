local Obfuscator = require("Obfuscator")
local source = [[
local function add(a, b)
    return a + b
end
print("Add(10, 20) = " .. add(10, 20))
local s = 0
for i=1,10 do s = s + i end
print("Sum 1-10 = " .. s)
]]
local obf = Obfuscator.obfuscate(source, 2)
local f = load(obf)
f()
