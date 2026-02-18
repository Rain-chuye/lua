local Lexer = require("obfuscator.lexer")
local Parser = require("obfuscator.parser")
local Virtualizer = require("obfuscator.virtualizer")
local Engine = require("obfuscator.engine")
local vm_template = require("obfuscator.vm_template")
local BytecodeParser = require("obfuscator.bytecode_parser")

local ObfuscatorTool = {}

function ObfuscatorTool.obfuscate(input, options)
  local proto
  local deps_comments = ""
  local engine = Engine.new(options)
  local virtualizer = Virtualizer.new()

  if input:sub(1, 4) == "\27Lua" then
    -- Bytecode input
    local parser = BytecodeParser.new(input)
    proto = parser:parse()
  else
    -- Source input
    local lex = Lexer.new(input)
    local parser = Parser.new(lex)
    local ast = parser:parse()

    if options.identify_deps then
      deps_comments = engine:identify_dependencies(ast)
    end

    proto = virtualizer:virtualize(ast)
  end

  -- Apply obfuscation to proto
  engine:obfuscate_proto(proto)
  local packed = virtualizer:pack_proto(proto)

  -- Serialize packed proto to Lua table string
  local function serialize(t)
    if type(t) == "table" then
      if t.data and t.keys then -- Encrypted string
        local data_s = "{" .. table.concat(t.data, ",") .. "}"
        local keys_s = "{" .. table.concat(t.keys, ",") .. "}"
        return "{data=" .. data_s .. ",keys=" .. keys_s .. "}"
      elseif t.type and t.v and t.x then -- MBA Number
        return "{type='" .. t.type .. "',v=" .. t.v .. ",x=" .. t.x .. "}"
      end

      local res = "{"
      for k, v in pairs(t) do
        local key = type(k) == "number" and ("[" .. k .. "]=") or (k .. "=")
        res = res .. key .. serialize(v) .. ","
      end
      return res .. "}"
    elseif type(t) == "string" then
      return string.format("%q", t)
    else
      return tostring(t)
    end
  end

  local packed_s = serialize(packed)
  -- Use gsub to avoid string.format limits and issues with %
  local final_code = vm_template:gsub("%%s", function()
    local res = packed_s
    packed_s = tostring(virtualizer.xor_key) -- sequential replacement
    return res
  end)
  final_code = deps_comments .. final_code

  return final_code
end

-- Androlua+ UI logic (only runs if 'activity' exists)
if activity then
  local layout = require("layout")
  activity.setContentView(loadlayout(layout))

  btn_obfuscate.onClick = function()
    -- In a real app, you'd get source from an EditText or file
    local source = "print('Hello World')" -- placeholder
    local options = {
      int_rate = sb_int_rate.progress / 100,
      junk_rate = cb_junk.checked and (sb_junk_rate.progress / 100) or 0,
      identify_deps = cb_dep.checked,
    }

    local success, result = pcall(ObfuscatorTool.obfuscate, source, options)
    if success then
       print("混淆成功！")
       -- Save to file or show result
    else
       print("混淆失败: " .. result)
    end
  end
end

return ObfuscatorTool
