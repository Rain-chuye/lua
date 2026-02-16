require "import"
import "android.widget.*"
import "android.view.*"

local Obfuscator = require("obfuscator_engine")

local layout = {
    LinearLayout,
    orientation="vertical",
    layout_width="fill",
    layout_height="fill",
    {
        TextView,
        text="Lua 5.3.3 AST Obfuscator",
        textSize="20sp",
        layout_gravity="center",
        padding="10dp",
    },
    {
        EditText,
        id="input_code",
        hint="Paste your Lua code here...",
        layout_width="fill",
        layout_height="0dp",
        layout_weight=1,
        gravity="top",
    },
    {
        Button,
        text="Obfuscate",
        id="btn_obf",
        layout_width="fill",
    },
    {
        EditText,
        id="output_code",
        hint="Obfuscated code will appear here...",
        layout_width="fill",
        layout_height="0dp",
        layout_weight=1,
        gravity="top",
        readOnly=true,
    },
}

activity.setContentView(loadlayout(layout))

btn_obf.onClick = function()
    local source = input_code.text
    if source == "" then
        print("Please enter some code")
        return
    end

    local status, result = pcall(Obfuscator.obfuscate, source)
    if status then
        output_code.text = result
        print("Obfuscation successful!")
    else
        print("Error: " .. tostring(result))
    end
end
