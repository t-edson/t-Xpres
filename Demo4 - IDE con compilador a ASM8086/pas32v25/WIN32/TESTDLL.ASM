.COMMENT
 ****************************************************************************
 TESTDLL.ASM Example Win32 DLL
                (c) 1998,1999 by Dieter Pawelczak - Pass32 Version 3.0alpha
 ============================================================================
 ****************************************************************************

    .MODEL WIN32 CONSOLE DLL
    .INTERFACE

; This is the Interface part of the DLL

    PROC    'Proc1'    OFFSET DLLProc1
    PROC    'Proc2'    OFFSET DLLProc2
    PROC    'Proc3'    OFFSET DLLProc3



    .CODE

; init code - Win32 calls this code, when the DLL is load.
; note, this must be the first line of the code segment here!
; do not include prior to the minimum DLL init code:
; >>>>>   mov eax,1    ; init successful
; >>>>>   ret 0ch      ; return
;
     call init_console
; the return value must be TRUE = 1, otherwise:
; Windows prints funny error messages.
     mov eax,1
; frees the parameters from the stack
     ret 0ch




    .include console.inc


    PROC DLLPROC1
      writeln('TESTDLL: procedure 1 is called');
      ret
    ENDP DLLPROC1

    PROC DLLPROC2
      writeln('TESTDLL: procedure 2 is called');
      ret
    ENDP DLLPROC2

    PROC DLLPROC3
      writeln('TESTDLL: procedure 3 is called');
      ret
    ENDP DLLPROC3
