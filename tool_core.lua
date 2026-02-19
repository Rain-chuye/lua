local Lexer = require("obfuscator.lexer")
local Parser = require("obfuscator.parser")
local Virtualizer = require("obfuscator.virtualizer")
local Engine = require("obfuscator.engine")
local vm_template = require("obfuscator.vm_template")
local BytecodeParser = require("obfuscator.bytecode_parser")
local Wrapper = require("obfuscator.wrapper")

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
  local final_code = Wrapper.generate(vm_template, packed_s, virtualizer.xor_key, virtualizer.xor_key)

  -- String escaping to \ddd with segmentation
  local segments = {}
  local current = ""
  for i = 1, #final_code do
    current = current .. "\\" .. string.format("%03d", final_code:byte(i))
    if #current > 4000 then
      table.insert(segments, "\"" .. current .. "\"")
      current = ""
    end
  end
  if #current > 0 then table.insert(segments, "\"" .. current .. "\"") end

  final_code = deps_comments .. "load(" .. table.concat(segments, "..") .. ")()"

  return final_code
end

return ObfuscatorTool
