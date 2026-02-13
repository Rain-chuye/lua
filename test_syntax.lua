local Obfuscator = require("Obfuscator")
local source = "print('Hello World')"
local obf = Obfuscator.obfuscate(source, 1)
local f, err = load(obf)
if not f then print("Error: " .. err) else print("Syntax OK") end
