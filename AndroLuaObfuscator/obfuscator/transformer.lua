local transformer = {}
local utils = require "obfuscator.utils"

function transformer.hash_identifiers(ast)
    local local_map = {}
    local function walk(node)
        if not node then return end
        if type(node) ~= "table" then return end

        if node.type == "LocalAssign" then
            for i, name in ipairs(node.names) do
                local h = utils.hash_name(name)
                local_map[name] = h
                node.names[i] = h
            end
        elseif node.type == "LocalFunction" then
            local h = utils.hash_name(node.name)
            local_map[node.name] = h
            node.name = h
            local old_map = {}
            if node.params then
                for i, p in ipairs(node.params) do
                    if p ~= "..." then
                        old_map[p] = local_map[p]
                        local nh = utils.hash_name(p)
                        local_map[p] = nh
                        node.params[i] = nh
                    end
                end
            end
            if node.body then for _, s in ipairs(node.body) do walk(s) end end
            for k, v in pairs(old_map) do local_map[k] = v end
            return
        elseif node.type == "ForIn" then
            local old_map = {}
            for i, name in ipairs(node.names) do
                old_map[name] = local_map[name]
                local h = utils.hash_name(name)
                local_map[name] = h
                node.names[i] = h
            end
            if node.expressions then for _, e in ipairs(node.expressions) do walk(e) end end
            if node.body then for _, s in ipairs(node.body) do walk(s) end end
            for k, v in pairs(old_map) do local_map[k] = v end
            return
        elseif node.type == "ForRange" then
            local old_map = {}
            old_map[node.name] = local_map[node.name]
            local h = utils.hash_name(node.name)
            local_map[node.name] = h
            node.name = h
            if node.start then walk(node.start) end
            if node.stop then walk(node.stop) end
            if node.step then walk(node.step) end
            if node.body then for _, s in ipairs(node.body) do walk(s) end end
            for k, v in pairs(old_map) do local_map[k] = v end
            return
        elseif node.type == "Identifier" then
            if local_map[node.name] then
                node.name = local_map[node.name]
            end
        elseif node.type == "Function" then
            local old_map = {}
            if node.params then
                for i, p in ipairs(node.params) do
                    if p ~= "..." then
                        old_map[p] = local_map[p]
                        local h = utils.hash_name(p)
                        local_map[p] = h
                        node.params[i] = h
                    end
                end
            end
            if node.body then for _, s in ipairs(node.body) do walk(s) end end
            for k, v in pairs(old_map) do local_map[k] = v end
            return
        end

        for k, v in pairs(node) do
            if type(v) == "table" and k ~= "body" then walk(v) end
        end
        if node.body and type(node.body) == "table" then
            for _, s in ipairs(node.body) do walk(s) end
        end
    end
    walk(ast)
end

-- rest of transformer.lua...
function transformer.flatten_control_flow(block)
    if not block or #block < 3 then return block end

    for _, stmt in ipairs(block) do
        if stmt.type == "Return" or stmt.type == "Break" or stmt.type == "Goto" then
            return block
        end
    end

    local locals_to_hoist = {}
    local processed_block = {}

    for _, stmt in ipairs(block) do
        if stmt.type == "LocalAssign" then
            for _, name in ipairs(stmt.names) do
                table.insert(locals_to_hoist, name)
            end
            if stmt.values and #stmt.values > 0 then
                table.insert(processed_block, {
                    type = "Assignment",
                    targets = (function()
                        local t = {}
                        for _, n in ipairs(stmt.names) do table.insert(t, {type="Identifier", name=n}) end
                        return t
                    end)(),
                    values = stmt.values
                })
            end
        elseif stmt.type == "LocalFunction" then
            table.insert(locals_to_hoist, stmt.name)
            table.insert(processed_block, {
                type = "Assignment",
                targets = {{type="Identifier", name=stmt.name}},
                values = {{
                    type = "Function",
                    params = stmt.params,
                    body = stmt.body
                }}
            })
        else
            table.insert(processed_block, stmt)
        end
    end

    local dispatcher = utils.hash_name("state")
    local states = {}
    for i, stmt in ipairs(processed_block) do
        table.insert(states, {id = i * 137, stmt = stmt})
    end

    local new_block = {}
    if #locals_to_hoist > 0 then
        table.insert(new_block, {
            type = "LocalAssign",
            names = locals_to_hoist,
            values = {}
        })
    end
    table.insert(new_block, {
        type = "LocalAssign",
        names = {dispatcher},
        values = {{type = "Number", value = states[1].id}}
    })
    local body = {}
    local if_node = nil
    for i, state in ipairs(states) do
        local condition = {
            type = "Binary", op = "==",
            left = {type = "Identifier", name = dispatcher},
            right = {type = "Number", value = state.id}
        }
        local next_id = states[i+1] and states[i+1].id or 0
        local then_block = {
            state.stmt,
            {
                type = "Assignment",
                targets = {{type = "Identifier", name = dispatcher}},
                values = {{type = "Number", value = next_id}}
            }
        }
        if not if_node then
            if_node = {type = "If", condition = condition, then_block = then_block, else_ifs = {}}
        else
            table.insert(if_node.else_ifs, {condition = condition, block = then_block})
        end
    end
    table.insert(body, if_node)
    table.insert(new_block, {
        type = "While",
        condition = {
            type = "Binary", op = "~=",
            left = {type = "Identifier", name = dispatcher},
            right = {type = "Number", value = 0}
        },
        body = body
    })
    return new_block
end

function transformer.apply_cff_recursive(ast)
    local function walk(node)
        if not node or type(node) ~= "table" then return end
        if node.body and type(node.body) == "table" then
            node.body = transformer.flatten_control_flow(node.body)
            for _, s in ipairs(node.body) do walk(s) end
        elseif node.then_block then
            node.then_block = transformer.flatten_control_flow(node.then_block)
            for _, s in ipairs(node.then_block) do walk(s) end
            if node.else_block then
                node.else_block = transformer.flatten_control_flow(node.else_block)
                for _, s in ipairs(node.else_block) do walk(s) end
            end
            if node.else_ifs then
                for _, ei in ipairs(node.else_ifs) do
                    ei.block = transformer.flatten_control_flow(ei.block)
                    for _, s in ipairs(ei.block) do walk(s) end
                end
            end
        else
            for _, v in pairs(node) do
                if type(v) == "table" then walk(v) end
            end
        end
    end
    if ast.type == "Chunk" then
        ast.body = transformer.flatten_control_flow(ast.body)
    end
    walk(ast)
end

function transformer.to_lua(node)
    if not node then return "" end
    if node.type == "Chunk" then
        local res = {}
        for _, s in ipairs(node.body) do table.insert(res, transformer.to_lua(s)) end
        return table.concat(res, "\n")
    elseif node.type == "LocalAssign" then
        local names = table.concat(node.names, ", ")
        local values = {}
        if node.values then for _, v in ipairs(node.values) do table.insert(values, transformer.to_lua(v)) end end
        if #values > 0 then
            return "local " .. names .. " = " .. table.concat(values, ", ")
        else
            return "local " .. names
        end
    elseif node.type == "LocalFunction" then
        local params = table.concat(node.params, ", ")
        local s = "local function " .. node.name .. "(" .. params .. ")\n"
        for _, stmt in ipairs(node.body) do s = s .. transformer.to_lua(stmt) .. "\n" end
        return s .. "end"
    elseif node.type == "Function" then
        local params = table.concat(node.params, ", ")
        local s = "function(" .. params .. ")\n"
        for _, stmt in ipairs(node.body) do s = s .. transformer.to_lua(stmt) .. "\n" end
        return s .. "end"
    elseif node.type == "Number" then
        return tostring(node.value)
    elseif node.type == "String" then
        return string.format("%q", node.value)
    elseif node.type == "Boolean" then
        return tostring(node.value)
    elseif node.type == "Nil" then
        return "nil"
    elseif node.type == "Vararg" then
        return "..."
    elseif node.type == "Identifier" then
        return node.name
    elseif node.type == "Binary" then
        return "(" .. transformer.to_lua(node.left) .. " " .. node.op .. " " .. transformer.to_lua(node.right) .. ")"
    elseif node.type == "Unary" then
        return "(" .. node.op .. " " .. transformer.to_lua(node.argument) .. ")"
    elseif node.type == "If" then
        local s = "if " .. transformer.to_lua(node.condition) .. " then\n"
        for _, stmt in ipairs(node.then_block) do s = s .. transformer.to_lua(stmt) .. "\n" end
        if node.else_ifs then
            for _, ei in ipairs(node.else_ifs) do
                s = s .. "elseif " .. transformer.to_lua(ei.condition) .. " then\n"
                for _, stmt in ipairs(ei.block) do s = s .. transformer.to_lua(stmt) .. "\n" end
            end
        end
        if node.else_block then
            s = s .. "else\n"
            for _, stmt in ipairs(node.else_block) do s = s .. transformer.to_lua(stmt) .. "\n" end
        end
        return s .. "end"
    elseif node.type == "While" then
        local s = "while " .. transformer.to_lua(node.condition) .. " do\n"
        for _, stmt in ipairs(node.body) do s = s .. transformer.to_lua(stmt) .. "\n" end
        return s .. "end"
    elseif node.type == "Break" then
        return "break"
    elseif node.type == "ForRange" then
        local s = "for " .. node.name .. " = " .. transformer.to_lua(node.start) .. ", " .. transformer.to_lua(node.stop)
        if node.step then s = s .. ", " .. transformer.to_lua(node.step) end
        s = s .. " do\n"
        for _, stmt in ipairs(node.body) do s = s .. transformer.to_lua(stmt) .. "\n" end
        return s .. "end"
    elseif node.type == "ForIn" then
        local names = table.concat(node.names, ", ")
        local exps = {}
        for _, e in ipairs(node.expressions) do table.insert(exps, transformer.to_lua(e)) end
        local s = "for " .. names .. " in " .. table.concat(exps, ", ") .. " do\n"
        for _, stmt in ipairs(node.body) do s = s .. transformer.to_lua(stmt) .. "\n" end
        return s .. "end"
    elseif node.type == "Call" then
        local args = {}
        for _, a in ipairs(node.args) do table.insert(args, transformer.to_lua(a)) end
        return transformer.to_lua(node.base) .. "(" .. table.concat(args, ", ") .. ")"
    elseif node.type == "MethodCall" then
        local args = {}
        for _, a in ipairs(node.args) do table.insert(args, transformer.to_lua(a)) end
        return transformer.to_lua(node.base) .. ":" .. node.method .. "(" .. table.concat(args, ", ") .. ")"
    elseif node.type == "Return" then
        local vals = {}
        for _, v in ipairs(node.values) do table.insert(vals, transformer.to_lua(v)) end
        return "return " .. table.concat(vals, ", ")
    elseif node.type == "ExpressionStatement" then
        return transformer.to_lua(node.expression)
    elseif node.type == "Assignment" then
        local targets = {}
        for _, t in ipairs(node.targets) do table.insert(targets, transformer.to_lua(t)) end
        local values = {}
        for _, v in ipairs(node.values) do table.insert(values, transformer.to_lua(v)) end
        return table.concat(targets, ", ") .. " = " .. table.concat(values, ", ")
    elseif node.type == "Member" then
        return transformer.to_lua(node.base) .. "." .. node.property
    elseif node.type == "Index" then
        return transformer.to_lua(node.base) .. "[" .. transformer.to_lua(node.index) .. "]"
    elseif node.type == "Table" then
        local fields = {}
        for _, f in ipairs(node.fields) do
            if f.key then
                table.insert(fields, "[" .. transformer.to_lua(f.key) .. "] = " .. transformer.to_lua(f.value))
            else
                table.insert(fields, transformer.to_lua(f.value))
            end
        end
        return "{" .. table.concat(fields, ", ") .. "}"
    end
    return "-- [[ Unknown node type: " .. (node.type or "nil") .. " ]]"
end

return transformer
