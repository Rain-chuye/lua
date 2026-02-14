import os

files = [
    "lapi.c", "lauxlib.c", "lbaselib.c", "lbitlib.c", "lcode.c", "lcorolib.c",
    "lctype.c", "ldblib.c", "ldebug.c", "ldo.c", "ldump.c", "lfunc.c", "lgc.c",
    "linit.c", "liolib.c", "llex.c", "lmathlib.c", "lmem.c", "lobject.c",
    "lopcodes.c", "loslib.c", "lparser.c", "lstate.c", "lstring.c", "lstrlib.c",
    "ltable.c", "ltablib.c", "ltm.c", "lundump.c", "lutf8lib.c", "lvm.c", "lzio.c"
]

mapping = {
    '"attempt to index a nil value"': '"尝试索引空值"',
    '"attempt to call a nil value"': '"尝试调用空值"',
    '"attempt to perform arithmetic on a %s value"': '"尝试对 %s 值进行算术运算"',
    '"attempt to perform arithmetic on"': '"尝试进行算术运算"',
    '"attempt to compare %s with %s"': '"尝试将 %s 与 %s 比较"',
    '"attempt to compare two %s values"': '"尝试比较两个 %s 值"',
    '"attempt to concatenate a %s value"': '"尝试拼接 %s 值"',
    '"attempt to %s a %s value%s"': '"尝试%s一个%s值%s"',
    '"table index is nil"': '"表索引为空"',
    '"table index is NaN"': '"表索引为 NaN"',
    '"stack overflow"': '"堆栈溢出"',
    '"not enough memory"': '"内存不足"',
    '"interrupted!"': '"被中断！"',
    '"\'for\' initial value must be a number"': '"\'for\' 初始值必须是数值"',
    '"\'for\' limit must be a number"': '"\'for\' 限制值必须是数值"',
    '"\'for\' step must be a number"': '"\'for\' 步长必须是数值"',
    '"\'__index\' chain too long; possible loop"': '"\'__index\' 链太长；可能存在循环"',
    '"\'__newindex\' chain too long; possible loop"': '"\'__newindex\' 链太长；可能存在循环"',
    '"bad argument #%d (%s)"': '"参数 #%d 错误 (%s)"',
    '"bad argument #%d to \'%s\' (%s)"': '"函数 \'%s\' 的参数 #%d 错误 (%s)"',
    '"%s expected, got %s"': '"应为 %s，得到 %s"',
    '"value expected"': '"缺少值"',
    '"assertion failed!"': '"断言失败！"',
    '"unexpected symbol"': '"未预期的符号"',
    '"index out of range"': '"索引超出范围"',
    '"attempt to divide by zero"': '"尝试除以零"',
    '"attempt to perform \'n%%0\'"': '"尝试执行 \'n%%0\'"',
    '"array key must be a integer"': '"数组键必须是整数"',
    '"const table cannot be set"': '"常量表不可设置"',
    '"too many upvalues"': '"upvalue 过多"',
    '"too many local variables"': '"局部变量过多"',
    '"too many nested functions"': '"嵌套函数过多"',
    '"too many parameters"': '"参数过多"',
    '"no loop to break"': '"没有可以 break 的循环"',
    '"unfinished string"': '"未完成的字符串"',
    '"lexical error"': '"词法错误"',
    '"syntax error"': '"语法错误"',
    '"main chunk"': '"主块"',
    '"cannot change a protected metatable"': '"无法更改受保护的元表"',
    '"reader function must return a string"': '"读取函数必须返回字符串"',
    '"base out of range"': '"进制超出范围"',
    '"nil or table expected"': '"应为 nil 或表"',
    '"table function or nil expected"': '"应为表、函数或 nil"',
    '"table or string expected"': '"应为表或字符串"',
    '"integer overflow"': '"整数溢出"',
    '"number has no integer representation"': '"数值无法表示为整数"',
    '"number%s has no integer representation"': '"数值%s无法表示为整数"',
    '"string length overflow"': '"字符串长度溢出"',
    '"too many arguments"': '"参数太多"',
    '"too many results to unpack"': '"unpack 结果太多"',
    '"invalid capture index"': '"无效的捕获索引"',
    '"invalid pattern capture"': '"无效的模式捕获"',
    '"unfinished capture"': '"未完成的捕获"',
    '"malformed pattern"': '"模式格式错误"',
    '"pattern too complex"': '"模式太复杂"',
    '"index"': '"索引"',
    '"call"': '"调用"',
    '"concatenate"': '"连接"',
    '"get length of"': '"获取长度"',
    '"perform arithmetic on"': '"进行算术运算"',
    '"compare"': '"比较"',
    '"bitwise operation on"': '"进行位运算"',
}

short_strings = ['"index"', '"call"', '"get length of"', '"concatenate"', '"compare"', '"perform arithmetic on"', '"bitwise operation on"']

for f in files:
    if not os.path.exists(f): continue
    with open(f, 'r') as fd:
        content = fd.read()
    changed = False
    for k, v in mapping.items():
        if k in short_strings and f not in ["lvm.c", "ldo.c", "ldebug.c"]:
            continue
        if k in content:
            content = content.replace(k, v)
            changed = True
    if changed:
        with open(f, 'w') as fd:
            fd.write(content)
        print(f"Updated {f}")
