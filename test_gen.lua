local Obfuscator = require("obfuscator_engine")
local src = [[
local s = "hello"
print(s:upper())
local ok, err = pcall(function() return string.upper(s) end)
if not ok then print("string library hidden") else print("string library NOT hidden") end
]]
local obf = Obfuscator.obfuscate(src)
local f = io.open("test_hide_obf.lua", "wb")
f:write(obf)
f:close()
print("Generated test_hide_obf.lua")
