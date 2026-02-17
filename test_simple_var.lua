local Obfuscator = require("obfuscator_engine")
local source = [[local x = 10; print("Value is", x)]]
local result = Obfuscator.obfuscate(source)
local f = load(result)
f()
