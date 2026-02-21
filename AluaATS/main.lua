require "import"
import "android.widget.*"
import "android.view.*"
import "android.graphics.*"
import "android.graphics.drawable.*"
pcall(function() import "android.support.v7.widget.CardView" end); pcall(function() import "androidx.cardview.widget.CardView" end)

local Obfuscator = require("obfuscator_engine")

-- Color Palette
local PRIMARY = "#2196F3"
local PRIMARY_DARK = "#1976D2"
local ACCENT = "#FF4081"
local BG = "#F0F2F5"
local TEXT_MAIN = "#212121"
local TEXT_SUB = "#757575"

activity.setTheme(android.R.style.Theme_DeviceDefault_Light_NoActionBar)
activity.getWindow().setStatusBarColor(Color.parseColor(PRIMARY_DARK))

function create_card(title, layout_inner)
    return {
        CardView,
        layout_width="fill",
        layout_margin="12dp",
        elevation="6dp",
        radius="12dp",
        cardBackgroundColor="#FFFFFF",
        {
            LinearLayout,
            orientation="vertical",
            padding="20dp",
            layout_width="fill",
            {
                TextView,
                text=title,
                textSize="18sp",
                textColor=PRIMARY,
                textStyle="bold",
                layout_marginBottom="12dp",
            },
            unpack(layout_inner)
        }
    }
end

function create_input_field(id, label, hint, default_text)
    return {
        LinearLayout,
        orientation="vertical",
        layout_width="fill",
        layout_marginBottom="12dp",
        { TextView, text=label, textSize="14sp", textColor=TEXT_SUB },
        {
            EditText,
            id=id,
            hint=hint,
            text=default_text,
            textSize="16sp",
            layout_width="fill",
            background="#00000000",
            padding="8dp",
        },
        { View, layout_width="fill", layout_height="1dp", background="#E0E0E0" },
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
        layout_height="56dp",
        backgroundColor=PRIMARY,
        gravity="center_vertical",
        paddingLeft="16dp",
        {
            TextView,
            text="ATS Advanced Obfuscator",
            textColor="#FFFFFF",
            textSize="20sp",
            textStyle="bold",
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
            create_card("Configuration", {
                create_input_field("in_path", "Input Path (File or Folder)", "/sdcard/input.lua", "/sdcard/androlua/project/StandardATS/input.lua"),
                create_input_field("out_path", "Output Path", "/sdcard/output.lua", "/sdcard/androlua/project/StandardATS/output.lua"),
                {
                    CheckBox,
                    id="batch_check",
                    text="Batch Processing Mode (Process all .lua in folder)",
                    textColor=TEXT_MAIN,
                }
            }),
            create_card("Virtualization Settings", {
                { CheckBox, id="mba_check", text="Advanced MBA Transformations", checked=true, textColor=TEXT_MAIN },
                { CheckBox, id="integrity_check", text="Anti-Tamper Integrity Check", checked=true, textColor=TEXT_MAIN },
                { CheckBox, id="fake_check", text="Opaque Predicate Injection", checked=true, textColor=TEXT_MAIN },
                { CheckBox, id="vm_check", text="Multi-Tier Virtualization (Ultra)", checked=true, textColor=TEXT_MAIN },
                { CheckBox, id="antihook_check", text="Scattered Anti-Hook Protection", checked=true, textColor=TEXT_MAIN },
            }),
            {
                Button,
                id="start_btn",
                text="START OBFUSCATION",
                layout_width="fill",
                layout_margin="16dp",
                padding="12dp",
                backgroundColor=PRIMARY,
                textColor="#FFFFFF",
                textSize="16sp",
                textStyle="bold",
                onClick=function()
                    start_obfuscation()
                end
            },
            create_card("Execution Console", {
                {
                    EditText,
                    id="log_text",
                    layout_width="fill",
                    layout_height="250dp",
                    gravity="top",
                    editable=false,
                    text="[SYSTEM] Engine v2.0 ready.\n[INFO] Waiting for user input...",
                    background="#F9F9F9",
                    textSize="13sp",
                    textColor="#263238",
                    padding="10dp",
                }
            })
        }
    }
}

function log(msg)
    activity.runOnMainThread(Runnable{
        run=function()
            log_text.append("\n" .. msg)
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

    log_text.setText("[PROCESS] Starting Task...")
    start_btn.setEnabled(false)
    start_btn.setBackgroundColor(Color.parseColor("#BDBDBD"))

    task(function(in_p, out_p, is_batch, options)
        local Obfuscator = require("obfuscator_engine")
        local File = luajava.bindClass("java.io.File")

        local function process_file(inp, outp)
            local f = io.open(inp, "r")
            if not f then return false, "Cannot open: " .. inp end
            local src = f:read("*all")
            f:close()

            local ok, res = pcall(Obfuscator.obfuscate, src, options)
            if not ok then return false, "Error in " .. inp .. ": " .. tostring(res) end

            local f = io.open(outp, "w")
            if not f then return false, "Cannot save: " .. outp end
            f:write(res)
            f:close()
            return true
        end

        if is_batch then
            local dir = File(in_p)
            if not dir.isDirectory() then return false, "Input is not a directory" end
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
            return true, "Successfully processed " .. count .. " files."
        else
            local ok, err = process_file(in_p, out_p)
            if ok then return true, "Saved to " .. out_p else return false, err end
        end
    end, in_p, out_p, is_batch, options, function(ok, res)
        start_btn.setEnabled(true)
        start_btn.setBackgroundColor(Color.parseColor(PRIMARY))
        if ok then
            log("[SUCCESS] " .. tostring(res))
            Toast.makeText(activity, "Obfuscation Complete!", Toast.LENGTH_SHORT).show()
        else
            log("[ERROR] " .. tostring(res))
            AlertDialog.Builder(activity)
            .setTitle("Error")
            .setMessage(tostring(res))
            .setPositiveButton("OK", nil)
            .show()
        end
    end)
end

activity.setContentView(loadlayout(layout))
