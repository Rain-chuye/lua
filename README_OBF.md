# Lua 5.3.3 语法树级混淆工具 (AST Obfuscator)

本工具实现了以下功能：

1.  **标识符混淆**：对所有局部变量、函数参数进行哈希化处理。全局变量通过映射表处理，在保持功能的同时隐藏其名称。
2.  **控制流扁平化**：将顺序逻辑转换为基于 `while` 循环和 `if-elseif` 分发器的状态机模式。
3.  **假分支注入**：插入基于数学恒等式、表特性的不透明谓词（Opaque Predicates），增加静态分析难度。
4.  **指令虚拟化 (Virtualization)**：
    *   将经过 AST 混淆的代码编译为自定义的随机字节码。
    *   生成一个基于 Lua 实现的虚拟机（Interpreter）来执行这些字节码。
    *   虚拟机本身采用了随机指令映射、XOR 加密以及控制流扁平化保护。

## 使用方法

使用提供的 Lua 5.3.3 解释器运行该工具：

```bash
./lua obfuscator_tool.lua <输入文件.lua> [输出文件.lua]
```

例如：

```bash
./lua obfuscator_tool.lua my_script.lua protected_script.lua
```

## 支持特性

*   **完全兼容 Lua 5.3 语法**（位运算符、整除等）。
*   **支持协同例程 (Coroutines)**：混淆后的代码可以正常进行 `yield` 和 `resume`。
*   **支持元表 (Metatables)**：算术运算符等触发的元方法均可正常工作。
*   **支持变长参数 (...)**。
*   **支持多返回值**。
*   **支持递归调用（闭包上值处理）**。

## 目录说明

*   `obfuscator_tool.lua`: 独立的混淆工具（已打包所有模块）。
*   `lua533_src/`: 修复了编译问题的原版 Lua 5.3.3 源码。
