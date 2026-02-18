local Lexer = require("obfuscator.lexer")
local Parser = require("obfuscator.parser")

local source = [[
$a = [1, 2, 3]
function hello()
  print "hi"
end
if a[1] == 1 print("ok")
switch a[1] do
  case 1: print("one")
  case 2: print("two")
  default: print("none")
end
defer print("done")
local *f = io.open("test")
print(!false)
print(1 != 2)
]]

local lex = Lexer.new(source)
local parser = Parser.new(lex)
local ast = parser:parse()

local function dump(node, indent)
  indent = indent or ""
  if type(node) ~= "table" then return tostring(node) end
  local s = "{\n"
  for k, v in pairs(node) do
    s = s .. indent .. "  " .. k .. " = " .. dump(v, indent .. "  ") .. ",\n"
  end
  return s .. indent .. "}"
end

print(dump(ast))
