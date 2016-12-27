.COMMENT
 ****************************************************************************
 TESTDLL.DLL    Example Overlay - assemble with /OVL
                (c) 1996,1998 by Dieter Pawelczak - Pass32 Version 2.0
 ============================================================================
 ****************************************************************************
    .MODEL OVL
    .INTERFACE

    ; This is the Interface part of the OVL

    .ORG 50000h   ; Entry Point for the OVL
    PROC    TestOVLMain    OFFSET OVLMain
    PROC    TestOVLProc1   OFFSET OVLProc1
    PROC    TestOVLProc2   OFFSET OVLProc2
    PROC    TestOVLProc3   OFFSET OVLProc3

    .DATA
       OVLMesg          db    'OVL is load! ',0
       Mesg1            db    'OVL Procedure 1 executed',0
       Mesg2            db    'OVL Procedure 2 executed',0
       Mesg3            db    'OVL Procedure 3 executed',0
    .CODE
    .FAR                ; treat all procedure calls as far

    PROC OVLMain FAR
      push ds
      mov ax,cs:[0]
      mov ds,ax
      mov edi,offset OVLMesg
      mov bh,14
      mov TextColor,bh
      call systemwriteLn
      pop ds
      ret
    ENDP OVLMain
    PROC OVLPROC1 FAR
      push ds
      mov ax,cs:[0]
      mov ds,ax
      mov edi,offset Mesg1
      call systemwriteLn
      pop ds
      ret
    ENDP OVLPROC1
    PROC OVLPROC2 FAR
      push ds
      mov ax,cs:[0]
      mov ds,ax
      mov edi,offset Mesg2
      call systemwriteLn
      pop ds
      ret
    ENDP OVLPROC2
    PROC OVLPROC3 FAR
      push ds
      mov ax,cs:[0]
      mov ds,ax
      mov edi,offset Mesg3
      call systemwriteLn
      pop ds
      ret
    ENDP OVLPROC3

    .INCLUDE SYSTEM.INC
    END
