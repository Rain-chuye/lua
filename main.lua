require "import"
import "android.widget.*"
import "android.view.*"
import "android.graphics.drawable.*"

local Obfuscator = require("obfuscator_engine")

activity.setTitle("Advanced Lua 5.3.3 Obfuscator")
activity.setTheme(android.R.style.Theme_DeviceDefault_Light)

function create_card(layout)
  return {
    CardView,
    layout_width="fill",
    layout_margin="8dp",
    elevation="4dp",
    radius="8dp",
    {
      LinearLayout,
      orientation="vertical",
      padding="16dp",
      layout_width="fill",
      unpack(layout)
    }
  }
end

layout = {
  ScrollView,
  layout_width="fill",
  layout_height="fill",
  fillViewport=true,
  {
    LinearLayout,
    orientation="vertical",
    layout_width="fill",
    background="#F5F5F5",
    create_card{
      { TextView, text="File Configuration", textSize="18sp", textColor="#212121", textStyle="bold" },
      { TextView, text="Input Path (File or Directory):", layout_marginTop="8dp" },
      { EditText, id="in_path", hint="/sdcard/Documents/myscript.lua", text="/sdcard/Documents/input.lua" },
      { TextView, text="Output Path:", layout_marginTop="8dp" },
      { EditText, id="out_path", hint="/sdcard/Documents/output.lua", text="/sdcard/Documents/output.lua" },
    },
    create_card{
      { TextView, text="Obfuscation Options", textSize="18sp", textColor="#212121", textStyle="bold" },
      {
        LinearLayout,
        orientation="horizontal",
        layout_marginTop="8dp",
        { CheckBox, id="mba_check", text="MBA Transformations", checked=true },
        { CheckBox, id="integrity_check", text="Integrity Check", checked=true, layout_marginLeft="16dp" },
      },
    },
    {
      Button,
      text="Start Obfuscation",
      layout_gravity="center",
      layout_margin="16dp",
      backgroundColor="#2196F3",
      textColor="#FFFFFF",
      onClick=function()
        local in_p = in_path.getText().toString()
        local out_p = out_path.getText().toString()
        local options = {
          mba = mba_check.isChecked(),
          integrity = integrity_check.isChecked(),
        }

        task(function(in_p, out_p, options)
          local Obfuscator = require("obfuscator_engine")
          local function process(ip, op)
            local f = io.open(ip, "r")
            if not f then error("Cannot open input file: " .. ip) end
            local src = f:read("*all")
            f:close()

            local ok, res = pcall(Obfuscator.obfuscate, src, options)
            if not ok then error("Obfuscation error: " .. tostring(res)) end

            local f = io.open(op, "w")
            if not f then error("Cannot open output file: " .. op) end
            f:write(res)
            f:close()
          end

          -- Check if directory
          local f = io.open(in_p, "r")
          local is_dir = false
          if f then
            local _, err = f:read(0)
            if err == "Is a directory" then is_dir = true end
            f:close()
          end

          if is_dir then
            -- Note: Simple directory traversal for demo,
            -- real implementation would use Luajava to list files
            return false, "Directory processing not fully implemented in demo"
          else
            local ok, err = pcall(process, in_p, out_p)
            return ok, err
          end
        end, in_p, out_p, options, function(ok, res)
          if ok then
            print("Successfully obfuscated to " .. out_p)
            log_text.append("\n[SUCCESS] " .. out_p)
          else
            print("Error: " .. tostring(res))
            log_text.append("\n[ERROR] " .. tostring(res))
          end
        end)
      end
    },
    create_card{
      { TextView, text="Log", textSize="18sp", textColor="#212121", textStyle="bold" },
      {
        EditText,
        id="log_text",
        layout_width="fill",
        layout_height="200dp",
        gravity="top",
        editable=false,
        text="Ready.",
        background="#EEEEEE",
        textSize="12sp",
      },
    }
  }
}

activity.setContentView(loadlayout(layout))
