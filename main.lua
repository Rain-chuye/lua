require "import"
import "android.widget.*"
import "android.view.*"

local Obfuscator = require("obfuscator_engine")

activity.setTitle("Lua 5.3.3 Obfuscator")
layout = {
  LinearLayout,
  orientation="vertical",
  layout_width="fill",
  layout_height="fill",
  {
    TextView,
    text="Source Code:",
    layout_margin="10dp",
  },
  {
    EditText,
    id="src_edit",
    layout_width="fill",
    layout_height="300dp",
    gravity="top",
    hint="Enter Lua source here...",
    text=[[
local function fib(n)
  if n < 2 then return n end
  return fib(n-1) + fib(n-2)
end
print("fib(10) =", fib(10))
]],
  },
  {
    LinearLayout,
    orientation="horizontal",
    layout_gravity="center",
    {
      CheckBox,
      id="mba_check",
      text="MBA",
      checked=true,
    },
    {
      CheckBox,
      id="integrity_check",
      text="Integrity",
      checked=true,
    },
  },
  {
    Button,
    text="Obfuscate",
    layout_gravity="center",
    onClick=function()
      local src = src_edit.getText().toString()
      local options = {
        mba = mba_check.isChecked(),
        integrity = integrity_check.isChecked(),
      }
      local ok, res = pcall(Obfuscator.obfuscate, src, options)
      if ok then
        out_edit.setText(res)
        print("Obfuscation successful!")
      else
        print("Error: " .. tostring(res))
      end
    end
  },
  {
    Button,
    text="Copy Result",
    layout_gravity="center",
    onClick=function()
      local res = out_edit.getText().toString()
      if res and #res > 0 then
        import "android.content.*"
        activity.getSystemService(Context.CLIPBOARD_SERVICE).setText(res)
        print("Copied to clipboard!")
      end
    end
  },
  {
    TextView,
    text="Obfuscated Output:",
    layout_margin="10dp",
  },
  {
    EditText,
    id="out_edit",
    layout_width="fill",
    layout_height="fill",
    gravity="top",
    editable=false,
  },
}

activity.setContentView(loadlayout(layout))
