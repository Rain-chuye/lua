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
      { TextView, text="Input Path (File):", layout_marginTop="8dp" },
      { EditText, id="in_path", hint="/sdcard/input.lua", text="/sdcard/Documents/input.lua" },
      { TextView, text="Output Path:", layout_marginTop="8dp" },
      { EditText, id="out_path", hint="/sdcard/output.lua", text="/sdcard/Documents/output.lua" },
    },
    create_card{
      { TextView, text="Obfuscation Options", textSize="18sp", textColor="#212121", textStyle="bold" },
      {
        LinearLayout,
        orientation="vertical",
        layout_marginTop="8dp",
        { CheckBox, id="mba_check", text="MBA Transformations (Arithmetic Obfuscation)", checked=true },
        { CheckBox, id="integrity_check", text="Integrity Check (Anti-Tamper Sum)", checked=true },
        { CheckBox, id="fake_check", text="Fake Branch Injection (Anti-Analysis)", checked=true },
        { CheckBox, id="commercial_check", text="Commercial Grade VM (Virtualization & Fusion)", checked=true },
      },
    },
    {
      Button,
      text="START OBFUSCATION",
      layout_gravity="center",
      layout_margin="16dp",
      layout_width="fill",
      backgroundColor="#2196F3",
      textColor="#FFFFFF",
      onClick=function()
        local in_p = in_path.getText().toString()
        local out_p = out_path.getText().toString()
        local options = {
          mba = mba_check.isChecked(),
          integrity = integrity_check.isChecked(),
          fake = fake_check.isChecked(),
          commercial = commercial_check.isChecked(),
        }

        log_text.setText("Starting...")

        task(function(in_p, out_p, options)
          local Obfuscator = require("obfuscator_engine")
          local f = io.open(in_p, "r")
          if not f then return false, "Cannot open input file" end
          local src = f:read("*all")
          f:close()

          local ok, res = pcall(Obfuscator.obfuscate, src, options)
          if not ok then return false, "Obfuscation error: " .. tostring(res) end

          local f = io.open(out_p, "w")
          if not f then return false, "Cannot open output file" end
          f:write(res)
          f:close()
          return true, out_p
        end, in_p, out_p, options, function(ok, res)
          if ok then
            print("Done!")
            log_text.append("\n[SUCCESS] Saved to " .. tostring(res))
            Toast.makeText(activity, "Obfuscation Complete!", Toast.LENGTH_SHORT).show()
          else
            print("Failed")
            log_text.append("\n[ERROR] " .. tostring(res))
            AlertDialog.Builder(activity)
            .setTitle("Error")
            .setMessage(tostring(res))
            .setPositiveButton("OK", nil)
            .show()
          end
        end)
      end
    },
    create_card{
      { TextView, text="Execution Log", textSize="18sp", textColor="#212121", textStyle="bold" },
      {
        EditText,
        id="log_text",
        layout_width="fill",
        layout_height="200dp",
        gravity="top",
        editable=false,
        text="System Ready.\nAdvanced Lua 5.3.3 Obfuscator Engine Active.",
        background="#EEEEEE",
        textSize="12sp",
        textColor="#444444",
      },
    }
  }
}

activity.setContentView(loadlayout(layout))
