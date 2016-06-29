del ".\Demo1 - Interprete basico\*.dbg"
del ".\Demo1 - Interprete basico\*.exe"
del ".\Demo2 - IDE con compilador a ASM8086\*.dbg"
del ".\Demo2 - IDE con compilador a ASM8086\*.exe"
del ".\Demo3 - Compilador Metodika a ASM8086\*.dbg"
del ".\Demo3 - Compilador Metodika a ASM8086\*.exe"
del ".\Demo4 - Parser\*.dbg"
del ".\Demo4 - Parser\*.exe"

rd ".\Demo1 - Interprete basico\Source\lib" /s /Q
rd ".\Demo2 - IDE con compilador a ASM8086\lib" /s /Q
rd ".\Demo3 - Compilador Metodika a ASM8086\lib" /s /Q
rd ".\Demo4 - Parser\lib" /s /Q

pause