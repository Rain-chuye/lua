require "import"
import "android.widget.*"
import "android.view.*"
import "androidx.cardview.widget.CardView"

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

-- Androlua+ UI logic (only runs if 'activity' exists)
if activity then
  local layout = require("layout")
  local views = {}
  activity.setContentView(loadlayout(layout, views))

  views.btn_obfuscate.onClick = function()
    local input_path = tostring(views.edit_path.text)
    if input_path == "" then print("请先输入路径") return end

    local options = {
      int_rate = views.sb_int_rate.progress / 100,
      junk_rate = views.cb_junk.checked and (views.sb_junk_rate.progress / 100) or 0,
      identify_deps = views.cb_dep.checked,
    }

    views.btn_obfuscate.setEnabled(false)
    views.btn_obfuscate.setText("正在混淆...")

    thread(function(path, opts, tool_path)
      require "import"
      package.path = tool_path .. "/?.lua;" .. package.path
      local tool = require("main")
      local ok, res, err = pcall(tool.obfuscate_file, path, opts)

      call("obf_done", ok, res, err)
    end, input_path, options, activity.getLuaDir())
  end

  function obf_done(ok, res, err)
    views.btn_obfuscate.setEnabled(true)
    views.btn_obfuscate.setText("开始混淆")
    if ok and res then
       print("混淆成功！输出至: " .. res)
    else
       print("混淆失败: " .. tostring(res or err))
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
