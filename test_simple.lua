local Obfuscator = require("obfuscator_engine")
local source = [[print("Hello World")]]
local result = Obfuscator.obfuscate(source)
local f = load(result)
f()
