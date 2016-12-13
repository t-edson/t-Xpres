.COMMENT
 ****************************************************************************
 DLLTEST.ASM Example Win32 file using the example DLL TESTDLL.DLL
                (c) 1998,1999 by Dieter Pawelczak - Pass32 Version 3.0alpha
 ============================================================================
 ****************************************************************************


.MODEL WIN32 CONSOLE
.include console.inc
.mem 32

; a simple Win32 DLL is load and tested
.code
.include console.inc

proc main
  writeln('Test program load and executed')
.invoke .win32 'TESTDLL.Proc1'
.invoke .win32 'TESTDLL.Proc2'
.invoke .win32 'TESTDLL.Proc3'
  readln
.invoke .win32 'Kernel32.ExitProcess'
endp main

end main
