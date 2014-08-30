.COMMENT
 ****************************************************************************
 HELLO2.ASM     Example file for FLAT Model
                (c) 1996,1998 by Dieter Pawelczak - Pass32 Version 2.0
 ============================================================================
 ****************************************************************************
.MODEL FLAT
.DATA
       HelloMesg        db      'Hello,World',0
.CODE
       mov edi,OFFSET HelloMesg ; offset of the text string
       call systemwriteLn       ; call a protected mode library function
       mov ah,4ch               ; DPMI terminate function
       int 21h                  ; call DPMI function
  .include system.inc           ; include SYSTEM.INC (contains systemwriteLn)
END
