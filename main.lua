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
    Button,
    text="Obfuscate",
    layout_gravity="center",
    onClick=function()
      local src = src_edit.getText().toString()
      local ok, res = pcall(Obfuscator.obfuscate, src)
      if ok then
        out_edit.setText(res)
        print("Obfuscation successful!")
      else
        print("Error: " .. tostring(res))
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
