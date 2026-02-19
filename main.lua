require "import"
import "android.widget.*"
import "android.view.*"
pcall(function() import "androidx.cardview.widget.CardView" end)
pcall(function() import "android.support.v7.widget.CardView" end)

local ObfuscatorTool = require("tool_core")

-- UI logic (only runs if 'activity' exists)
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
    views.prog_bar.setVisibility(0) -- View.VISIBLE

    -- Multi-threading using code string for maximum stability
    local thread_code = [[
      local path, opts, tool_path = ...
      require "import"
      package.path = tool_path .. "/?.lua;" .. package.path
      local tool = require("tool_core")
      local ok, res = pcall(tool.obfuscate_file, path, opts)
      if ok and res then
        call("obf_done", true, res)
      else
        call("obf_done", false, tostring(res))
      end
    ]]

    thread(thread_code, input_path, options, tostring(activity.getLuaDir()))
  end

  -- Result callback from thread
  function _G.obf_done(success, result)
    views.btn_obfuscate.setEnabled(true)
    views.btn_obfuscate.setText("开始混淆")
    views.prog_bar.setVisibility(8) -- View.GONE
    if success then
       print("混淆成功！输出至: " .. tostring(result))
    else
       print("混淆失败: " .. tostring(result))
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
