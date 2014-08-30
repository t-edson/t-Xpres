.COMMENT
 ****************************************************************************
 TESTDLL.DLL    Example DLL - assemble with /DLL
                (c) 1996,1998 by Dieter Pawelczak - Pass32 Version 2.0
 ============================================================================
 ****************************************************************************
    .MODEL DLL
    .INTERFACE

    ; This is the Interface part of the DLL


    PROC    TestDLLMain    OFFSET DLLMain
    PROC    TestDLLProc1   OFFSET DLLProc1
    PROC    TestDLLProc2   OFFSET DLLProc2
    PROC    TestDLLProc3   OFFSET DLLProc3

    .DATA
       DLLMesg          db    'DLL is load! ',0
       Mesg1            db    'DLL Procedure 1 executed',0
       Mesg2            db    'DLL Procedure 2 executed',0
       Mesg3            db    'DLL Procedure 3 executed',0
    .CODE
    .FAR                ; treat all procedure calls as far

    PROC DLLMain FAR
      push ds
      mov ax,cs:[0]
      mov ds,ax
      mov edi,offset DLLMesg
      mov bh,14
      mov TextColor,bh
      call systemwriteLn
      pop ds
      ret
    ENDP DLLMain
    PROC DLLPROC1 FAR
      push ds
      mov ax,cs:[0]
      mov ds,ax
      mov edi,offset Mesg1
      call systemwriteLn
      pop ds
      ret
    ENDP DLLPROC1
    PROC DLLPROC2 FAR
      push ds
      mov ax,cs:[0]
      mov ds,ax
      mov edi,offset Mesg2
      call systemwriteLn
      pop ds
      ret
    ENDP DLLPROC2
    PROC DLLPROC3 FAR
      push ds
      mov ax,cs:[0]
      mov ds,ax
      mov edi,offset Mesg3
      call systemwriteLn
      pop ds
      ret
    ENDP DLLPROC3

    .INCLUDE SYSTEM.INC
    END
