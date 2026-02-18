local Lexer = require("obfuscator.lexer")
local Parser = require("obfuscator.parser")
local Virtualizer = require("obfuscator.virtualizer")
local Engine = require("obfuscator.engine")
local vm_template = require("obfuscator.vm_template")
local BytecodeParser = require("obfuscator.bytecode_parser")

local ObfuscatorTool = {}

function ObfuscatorTool.obfuscate_file(input_path, options)
  local f = io.open(input_path, "rb")
  if not f then return nil, "无法读取文件" end
  local content = f:read("*a")
  f:close()

  local success, result = pcall(ObfuscatorTool.obfuscate, content, options)
  if not success then return nil, result end

  local output_path = input_path:gsub("%.lua$", "obf.lua")
  if output_path == input_path then
     output_path = input_path .. "obf.lua"
  end

  local f_out = io.open(output_path, "w")
  if not f_out then return nil, "无法写入文件" end
  f_out:write(result)
  f_out:close()

  return output_path
end

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
    -- In Androlua+, you might use a path from an input field
    -- Here we use a placeholder or previous input
    local input_path = "/sdcard/test.lua" -- placeholder
    if edit_path then input_path = tostring(edit_path.text) end

    local options = {
      int_rate = sb_int_rate.progress / 100,
      junk_rate = cb_junk.checked and (sb_junk_rate.progress / 100) or 0,
      identify_deps = cb_dep.checked,
    }

    local output_path, err = ObfuscatorTool.obfuscate_file(input_path, options)
    if output_path then
       print("混淆成功！输出至: " .. output_path)
    else
       print("混淆失败: " .. tostring(err))
    end
  end
end

-- CLI support
if arg and arg[1] then
  local options = {
    int_rate = 0.5,
    junk_rate = 0.1,
    identify_deps = true,
  }
  local out, err = ObfuscatorTool.obfuscate_file(arg[1], options)
  if out then
    print("Obfuscated: " .. out)
  else
    print("Error: " .. tostring(err))
  end
end

return ObfuscatorTool
