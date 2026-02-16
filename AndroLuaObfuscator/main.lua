require "import"
import "android.widget.*"
import "android.view.*"
import "android.content.*"
import "android.graphics.*"
import "com.google.android.material.card.MaterialCardView"
local engine = require "obfuscator.engine"

-- Theme Color
local themeColor = 0xFF00796B
local bgLight = 0xFFF5F5F5

layout = {
  LinearLayout,
  orientation="vertical",
  layout_width="fill",
  layout_height="fill",
  background=bgLight,
  {
    Toolbar,
    layout_width="fill",
    layout_height="56dp",
    background=themeColor,
    elevation="4dp",
    title="Lua 5.3 终极混淆器",
    titleColor=0xFFFFFFFF,
  },
  {
    ScrollView,
    layout_width="fill",
    layout_height="fill",
    {
      LinearLayout,
      orientation="vertical",
      layout_width="fill",
      padding="16dp",
      {
        CardView,
        layout_width="fill",
        layout_height="wrap",
        radius="8dp",
        cardBackgroundColor=0xFFFFFFFF,
        elevation="2dp",
        {
          LinearLayout,
          orientation="vertical",
          padding="16dp",
          {
            TextView,
            text="源文件路径",
            textSize="14sp",
            textColor=0xFF666666,
          },
          {
            EditText,
            id="filePath",
            hint="/sdcard/test.lua",
            layout_width="fill",
            singleLine=true,
            textSize="16sp",
          },
        }
      },
      {
        TextView,
        text="混淆配置",
        layout_marginTop="16dp",
        textColor=0xFF666666,
        textSize="14sp",
      },
      {
        LinearLayout,
        orientation="vertical",
        layout_width="fill",
        layout_marginTop="8dp",
        {
          CheckBox,
          id="chkHash",
          text="标识符哈希 (Identifier Hashing)",
          checked=true,
        },
        {
          CheckBox,
          id="chkCFF",
          text="控制流平展 (Control Flow Flattening)",
          checked=true,
        },
        {
          CheckBox,
          id="chkVM",
          text="指令虚拟化 (Instruction Virtualization)",
          checked=true,
        },
      },
      {
        Button,
        id="btnObfuscate",
        text="立即执行混淆",
        layout_width="fill",
        layout_height="48dp",
        layout_marginTop="24dp",
        background=themeColor,
        textColor=0xFFFFFFFF,
      },
      {
        CardView,
        layout_width="fill",
        layout_height="wrap",
        layout_marginTop="24dp",
        radius="8dp",
        cardBackgroundColor=0xFFFFFFFF,
        elevation="2dp",
        {
          LinearLayout,
          orientation="vertical",
          padding="16dp",
          {
            TextView,
            text="输出预览 (前1000字符)",
            textSize="14sp",
            textColor=0xFF666666,
          },
          {
            TextView,
            id="previewText",
            text="等待操作...",
            layout_width="fill",
            layout_marginTop="8dp",
            textSize="12sp",
            textColor=0xFF333333,
            typeface=Typeface.MONOSPACE,
          },
        }
      },
      {
        TextView,
        id="statusText",
        text="状态: 准备就绪",
        layout_marginTop="16dp",
        layout_gravity="center",
        textColor=0xFF999999,
      }
    }
  }
}

activity.setContentView(loadlayout(layout))

function readFile(path)
    local f = io.open(path, "r")
    if not f then return nil end
    local content = f:read("*all")
    f:close()
    return content
end

function writeFile(path, content)
    local f = io.open(path, "w")
    if not f then return false end
    f:write(content)
    f:close()
    return true
end

btnObfuscate.onClick = function()
    local path = filePath.Text
    if path == "" then
        print("请输入文件路径")
        return
    end

    local source = readFile(path)
    if not source then
        print("无法读取文件: " .. path)
        return
    end

    local config = {
        hash_ids = chkHash.Checked,
        cff = chkCFF.Checked,
        virtualize = chkVM.Checked,
    }

    statusText.Text = "状态: 正在混淆..."

    task(function(src, cfg)
        local ok, res = pcall(engine.obfuscate, src, cfg)
        return ok, res
    end, source, config, function(ok, res)
        if ok then
            statusText.Text = "状态: 混淆完成"
            previewText.Text = res:sub(1, 1000) .. (res:len() > 1000 and "\n..." or "")

            local outPath = path:gsub("%.lua$", "") .. "_obfuscated.lua"
            if writeFile(outPath, res) then
                print("混淆成功，已保存至: " .. outPath)
            else
                print("保存失败")
            end
        else
            statusText.Text = "状态: 混淆出错"
            print("错误: " .. tostring(res))
            previewText.Text = tostring(res)
        end
    end)
end
