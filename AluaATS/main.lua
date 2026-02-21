require "import"
import "android.widget.*"
import "android.view.*"
import "android.graphics.*"
import "android.graphics.drawable.*"

-- Try to import CardView from both support and androidx
pcall(function() import "android.support.v7.widget.CardView" end)
pcall(function() import "androidx.cardview.widget.CardView" end)

local Obfuscator = require("obfuscator_engine")

-- Design System
local PRIMARY = "#1E88E5" -- Vibrant Blue
local PRIMARY_DARK = "#1565C0"
local ACCENT = "#D81B60"
local BG = "#F8F9FA"
local CARD_BG = "#FFFFFF"
local SUCCESS = "#43A047"
local ERROR = "#E53935"

activity.setTheme(android.R.style.Theme_DeviceDefault_Light_NoActionBar)
activity.getWindow().setStatusBarColor(Color.parseColor(PRIMARY_DARK))

function create_card(title, icon, layout_inner)
    local items = {
        LinearLayout,
        orientation="vertical",
        padding="20dp",
        layout_width="fill",
        {
            LinearLayout,
            orientation="horizontal",
            gravity="center_vertical",
            layout_marginBottom="16dp",
            {
                TextView,
                text=title,
                textSize="18sp",
                textColor=PRIMARY,
                textStyle="bold",
                layout_weight=1,
            },
        },
        unpack(layout_inner)
    }
    return {
        CardView,
        layout_width="fill",
        layout_margin="10dp",
        elevation="4dp",
        radius="12dp",
        cardBackgroundColor=CARD_BG,
        items
    }
end

function create_input(id, label, default)
    return {
        LinearLayout,
        orientation="vertical",
        layout_width="fill",
        layout_marginBottom="12dp",
        { TextView, text=label, textSize="12sp", textColor="#9E9E9E", layout_marginLeft="4dp" },
        {
            EditText,
            id=id,
            text=default,
            textSize="15sp",
            layout_width="fill",
            background="#F1F3F4",
            padding="12dp",
            singleLine=true,
        },
    }
end

layout = {
    LinearLayout,
    orientation="vertical",
    layout_width="fill",
    layout_height="fill",
    background=BG,
    {
        LinearLayout,
        layout_width="fill",
        layout_height="64dp",
        backgroundColor=PRIMARY,
        gravity="center",
        elevation="8dp",
        {
            TextView,
            text="ATS VIRTUALIZER PRO",
            textColor="#FFFFFF",
            textSize="22sp",
            textStyle="bold",
            letterSpacing=0.1,
        }
    },
    {
        ScrollView,
        layout_width="fill",
        layout_height="fill",
        {
            LinearLayout,
            orientation="vertical",
            layout_width="fill",
            padding="8dp",
            create_card("Input Settings", nil, {
                create_input("in_path", "SOURCE PATH", "/sdcard/input.lua"),
                create_input("out_path", "OUTPUT PATH", "/sdcard/output.lua"),
                {
                    CheckBox,
                    id="batch_check",
                    text="Folder Batch Processing",
                    textColor="#424242",
                    textSize="14sp",
                }
            }),
            create_card("Protection Engine", nil, {
                { CheckBox, id="mba_check", text="Arithmetic Morphing (MBA)", checked=true, textColor="#424242" },
                { CheckBox, id="integrity_check", text="Runtime Integrity Shield", checked=true, textColor="#424242" },
                { CheckBox, id="fake_check", text="Control Flow Obfuscation", checked=true, textColor="#424242" },
                { CheckBox, id="vm_check", text="Ultra Virtualization Engine", checked=true, textColor="#424242" },
                { CheckBox, id="antihook_check", text="Anti-Debugging & Anti-Frida", checked=true, textColor="#424242" },
            }),
            {
                Button,
                id="start_btn",
                text="EXECUTE PROTECTION",
                layout_width="fill",
                layout_margin="10dp",
                height="56dp",
                backgroundColor=PRIMARY,
                textColor="#FFFFFF",
                textSize="16sp",
                textStyle="bold",
                onClick=function()
                    start_obfuscation()
                end
            },
            create_card("Protection Log", nil, {
                {
                    EditText,
                    id="log_text",
                    layout_width="fill",
                    layout_height="200dp",
                    gravity="top",
                    editable=false,
                    text=">> Engine initialized.\n>> Ready for deployment.",
                    background="#212121",
                    textSize="12sp",
                    textColor="#00E676", -- Console Green
                    padding="12dp",
                    typeface=Typeface.MONOSPACE,
                }
            })
        }
    }
}

function log(msg)
    activity.runOnMainThread(Runnable{
        run=function()
            log_text.append("\n>> " .. msg)
        end
    })
end

function start_obfuscation()
    local in_p = in_path.getText().toString()
    local out_p = out_path.getText().toString()
    local is_batch = batch_check.isChecked()
    local options = {
        mba = mba_check.isChecked(),
        integrity = integrity_check.isChecked(),
        fake = fake_check.isChecked(),
        commercial = vm_check.isChecked(),
        antihook = antihook_check.isChecked(),
    }

    log_text.setText(">> Starting Task...")
    start_btn.setEnabled(false)
    start_btn.setText("PROCESSING...")
    start_btn.setBackgroundColor(Color.parseColor("#90CAF9"))

    task(function(in_p, out_p, is_batch, options)
        local Obfuscator = require("obfuscator_engine")
        local File = luajava.bindClass("java.io.File")

        local function process_file(inp, outp)
            local f = io.open(inp, "r")
            if not f then return false, "Input File Not Found: " .. inp end
            local src = f:read("*all")
            f:close()

            local ok, res = pcall(Obfuscator.obfuscate, src, options)
            if not ok then return false, "Engine Error: " .. tostring(res) end

            local f = io.open(outp, "w")
            if not f then return false, "Permission Denied: " .. outp end
            f:write(res)
            f:close()
            return true
        end

        if is_batch then
            local dir = File(in_p)
            if not dir.isDirectory() then return false, "Path is not a directory" end
            local outDir = File(out_p)
            if not outDir.exists() then outDir.mkdirs() end

            local files = dir.listFiles()
            local count = 0
            for i=0, #files-1 do
                local file = files[i]
                if file.isFile() and file.getName().endsWith(".lua") then
                    local ok, err = process_file(file.getAbsolutePath(), out_p .. "/" .. file.getName())
                    if ok then count = count + 1 else return false, err end
                end
            end
            return true, count .. " files successfully encrypted."
        else
            local ok, err = process_file(in_p, out_p)
            if ok then return true, "Script secured: " .. out_p else return false, err end
        end
    end, in_p, out_p, is_batch, options, function(ok, res)
        start_btn.setEnabled(true)
        start_btn.setText("EXECUTE PROTECTION")
        start_btn.setBackgroundColor(Color.parseColor(PRIMARY))
        if ok then
            log("[SUCCESS] " .. tostring(res))
            Toast.makeText(activity, "Task Finished Successfully", Toast.LENGTH_SHORT).show()
        else
            log("[FAILED] " .. tostring(res))
            AlertDialog.Builder(activity)
            .setTitle("Protection Error")
            .setMessage(tostring(res))
            .setPositiveButton("OK", nil)
            .show()
        end
    end)
end

activity.setContentView(loadlayout(layout))
