local compiler = {}

local OP = {
    LOADK = 1, LOADNIL = 2, MOVE = 3, GETG = 4, SETG = 5,
    GETTAB = 6, SETTAB = 7, NEWTAB = 8, SELF = 9,
    ADD = 10, SUB = 11, MUL = 12, DIV = 13, MOD = 14, POW = 15,
    UNM = 16, NOT = 17, LEN = 18, CONCAT = 19,
    JMP = 20, JMPF = 21, JMPT = 22,
    CALL = 23, RET = 24, VARARG = 25, CLOSURE = 26,
    TFORCALL = 27, TFORLOOP = 28,
    LT = 29, LE = 30, EQ = 31,
    GETUP = 32, SETUP = 33,
    BAND = 34, BOR = 35, BXOR = 36, SHL = 37, SHR = 38, BNOT = 39,
}
compiler.OP = OP

local function create_proto()
    return {
        code = {},
        constants = {},
        protos = {},
        upvalues = {},
        num_params = 0,
        is_vararg = false,
        locals = {},
        next_reg = 0,
    }
end

function compiler.compile(ast)
    local function compile_func(body, params, parent_scope)
        local proto = create_proto()
        local current_scope = {
            locals = {},
            upvalues = {},
            proto = proto,
            parent = parent_scope
        }

        if params then
            proto.num_params = #params
            for i, p in ipairs(params) do
                if p == "..." then proto.is_vararg = true else
                    current_scope.locals[p] = i - 1
                end
            end
            proto.next_reg = #params
        end

        local function add_const(v)
            for i, c in ipairs(proto.constants) do if c == v then return i - 1 end end
            table.insert(proto.constants, v)
            return #proto.constants - 1
        end

        local function emit(op, a, b, c, flags)
            local inst = {op = op, a = a or 0, b = b or 0, c = c or 0}
            if flags then for k,v in pairs(flags) do inst[k] = v end end
            table.insert(proto.code, inst)
            return #proto.code
        end

        local function resolve_var(name)
            if current_scope.locals[name] then return "local", current_scope.locals[name] end
            if current_scope.upvalues[name] then return "upval", current_scope.upvalues[name] end
            if current_scope.parent then
                local vtype, vidx = current_scope.parent.resolve(name)
                if vtype == "local" then
                    local up_idx = #proto.upvalues
                    table.insert(proto.upvalues, {is_local = true, index = vidx})
                    current_scope.upvalues[name] = up_idx
                    return "upval", up_idx
                elseif vtype == "upval" then
                    local up_idx = #proto.upvalues
                    table.insert(proto.upvalues, {is_local = false, index = vidx})
                    current_scope.upvalues[name] = up_idx
                    return "upval", up_idx
                end
            end
            return "global", nil
        end
        current_scope.resolve = resolve_var

        local compile_exp, compile_stmt, compile_block

        function compile_exp(node, reg)
            if node.type == "Number" or node.type == "String" or node.type == "Boolean" then
                emit(OP.LOADK, reg, add_const(node.value))
            elseif node.type == "Nil" then
                emit(OP.LOADNIL, reg)
            elseif node.type == "Vararg" then
                emit(OP.VARARG, reg, 2)
            elseif node.type == "Identifier" then
                local vtype, vidx = resolve_var(node.name)
                if vtype == "local" then emit(OP.MOVE, reg, vidx)
                elseif vtype == "upval" then emit(OP.GETUP, reg, vidx)
                else emit(OP.GETG, reg, add_const(node.name)) end
            elseif node.type == "Binary" then
                compile_exp(node.left, reg)
                compile_exp(node.right, reg + 1)
                local op_map = {
                    ["+"]=OP.ADD, ["-"]=OP.SUB, ["*"]=OP.MUL, ["/"]=OP.DIV, ["%"]=OP.MOD, ["^"]=OP.POW,
                    [".."]=OP.CONCAT, ["<"]=OP.LT, ["<="]=OP.LE, ["=="]=OP.EQ,
                    ["&"]=OP.BAND, ["|"]=OP.BOR, ["~"]=OP.BXOR, ["<<"]=OP.SHL, [">>"]=OP.SHR
                }
                if op_map[node.op] then emit(op_map[node.op], reg, reg, reg + 1)
                elseif node.op == ">" then emit(OP.LT, reg, reg + 1, reg)
                elseif node.op == ">=" then emit(OP.LE, reg, reg + 1, reg)
                elseif node.op == "~=" then emit(OP.EQ, reg, reg, reg + 1); emit(OP.NOT, reg, reg) end
            elseif node.type == "Unary" then
                compile_exp(node.argument, reg)
                local op_map = {["-"]=OP.UNM, ["not"]=OP.NOT, ["#"]=OP.LEN, ["~"]=OP.BNOT}
                emit(op_map[node.op], reg, reg)
            elseif node.type == "Call" then
                compile_exp(node.base, reg)
                for i, arg in ipairs(node.args) do compile_exp(arg, reg + i) end
                emit(OP.CALL, reg, #node.args + 1, 2)
            elseif node.type == "Member" then
                compile_exp(node.base, reg)
                emit(OP.GETTAB, reg, reg, add_const(node.property), {c_is_k = true})
            elseif node.type == "Index" then
                compile_exp(node.base, reg)
                compile_exp(node.index, reg + 1)
                emit(OP.GETTAB, reg, reg, reg + 1)
            elseif node.type == "Table" then
                emit(OP.NEWTAB, reg)
                for i, field in ipairs(node.fields) do
                    if field.key then
                        compile_exp(field.key, reg + 1)
                        compile_exp(field.value, reg + 2)
                        emit(OP.SETTAB, reg, reg + 1, reg + 2)
                    else
                        compile_exp(field.value, reg + 1)
                        emit(OP.SETTAB, reg, add_const(i), reg + 1, {b_is_k = true})
                    end
                end
            elseif node.type == "Function" then
                local p = compile_func(node.body, node.params, current_scope)
                table.insert(proto.protos, p)
                emit(OP.CLOSURE, reg, #proto.protos - 1)
            end
        end

        function compile_stmt(node)
            if node.type == "LocalAssign" then
                for i, name in ipairs(node.names) do
                    local reg = proto.next_reg
                    current_scope.locals[name] = reg
                    proto.next_reg = proto.next_reg + 1
                    if node.values[i] then compile_exp(node.values[i], reg) else emit(OP.LOADNIL, reg) end
                end
            elseif node.type == "LocalFunction" then
                local reg = proto.next_reg
                current_scope.locals[node.name] = reg
                proto.next_reg = proto.next_reg + 1
                local p = compile_func(node.body, node.params, current_scope)
                table.insert(proto.protos, p)
                emit(OP.CLOSURE, reg, #proto.protos - 1)
            elseif node.type == "Assignment" then
                for i, target in ipairs(node.targets) do
                    local val_reg = proto.next_reg
                    compile_exp(node.values[i], val_reg)
                    if target.type == "Identifier" then
                        local vtype, vidx = resolve_var(target.name)
                        if vtype == "local" then emit(OP.MOVE, vidx, val_reg)
                        elseif vtype == "upval" then emit(OP.SETUP, vidx, val_reg)
                        else emit(OP.SETG, add_const(target.name), val_reg) end
                    elseif target.type == "Member" then
                        compile_exp(target.base, val_reg + 1)
                        emit(OP.SETTAB, val_reg + 1, add_const(target.property), val_reg, {b_is_k = true})
                    elseif target.type == "Index" then
                        compile_exp(target.base, val_reg + 1); compile_exp(target.index, val_reg + 2)
                        emit(OP.SETTAB, val_reg + 1, val_reg + 2, val_reg)
                    end
                end
            elseif node.type == "If" then
                compile_exp(node.condition, proto.next_reg)
                local jf_idx = emit(OP.JMPF, proto.next_reg, 0)
                compile_block(node.then_block)
                local end_jmp = emit(OP.JMP, 0)
                proto.code[jf_idx].b = #proto.code - jf_idx
                if node.else_block then compile_block(node.else_block) end
                proto.code[end_jmp].a = #proto.code - end_jmp
            elseif node.type == "While" then
                local start_pc = #proto.code
                compile_exp(node.condition, proto.next_reg)
                local jf_idx = emit(OP.JMPF, proto.next_reg, 0)
                compile_block(node.body)
                emit(OP.JMP, start_pc - #proto.code - 1)
                proto.code[jf_idx].b = #proto.code - jf_idx
            elseif node.type == "Return" then
                for i, v in ipairs(node.values) do compile_exp(v, i - 1) end
                emit(OP.RET, 0, #node.values + 1)
            elseif node.type == "ExpressionStatement" then
                compile_exp(node.expression, proto.next_reg)
            elseif node.type == "ForRange" then
                local old_next = proto.next_reg
                local r_idx = proto.next_reg
                current_scope.locals[node.name] = r_idx
                proto.next_reg = proto.next_reg + 1
                compile_exp(node.start, r_idx)
                local stop_reg = proto.next_reg
                proto.next_reg = proto.next_reg + 1
                compile_exp(node.stop, stop_reg)
                local step_reg = proto.next_reg
                proto.next_reg = proto.next_reg + 1
                if node.step then compile_exp(node.step, step_reg) else emit(OP.LOADK, step_reg, add_const(1)) end
                local start_pc = #proto.code
                local cond_reg = proto.next_reg
                emit(OP.LE, cond_reg, r_idx, stop_reg)
                local jf_idx = emit(OP.JMPF, cond_reg, 0)
                local saved_next = proto.next_reg
                proto.next_reg = proto.next_reg + 1
                compile_block(node.body)
                proto.next_reg = saved_next
                emit(OP.ADD, r_idx, r_idx, step_reg)
                emit(OP.JMP, start_pc - #proto.code - 1)
                proto.code[jf_idx].b = #proto.code - jf_idx
                proto.next_reg = old_next
            end
        end

        function compile_block(block)
            local old_next = proto.next_reg
            for _, stmt in ipairs(block) do compile_stmt(stmt) end
            proto.next_reg = old_next
        end

        compile_block(body)
        if #proto.code == 0 or proto.code[#proto.code].op ~= OP.RET then emit(OP.RET, 0, 1) end
        return proto
    end

    return compile_func(ast.body, {}, nil)
end

return compiler
