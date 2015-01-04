.COMMENT
 ****************************************************************************
 OVLTEST.ASM    Overlay example
                needs TESTOVL.OVL!
                (c) 1996,1998 by Dieter Pawelczak - Pass32 Version 2.0
 ============================================================================
 ****************************************************************************
.MODEL FLAT
.UCU
.NM

.include TESTOVL.ASM

 OVL_ERROR      .EQU 0
.DATA
.ALIGN 4
   OVLname      db    'TESTOVL.OVL',0
   ErrorMesg    db    'ERROR: TESTOVL.OVL not found!',0
   Mesg1        db    'Press any key to go on!',0
   Mesg2        db    'End of Demo!',0

.CODE
   mov esi,offset TESTOVL       ; OFFSET TO DATA BUFFER TESTOVL
   mov edi,offset OVLname       ; OFFSET TO Filename
   call InitOVL
   call loadOVL
   jc OVLnotfound
     call TestOVLMain
     mov edi,offset Mesg1
     mov bh,13
     call systemwriteLn
     call systemgetkey
     call TestOVLProc1
     call TestOVLProc2
     mov edi,offset Mesg1
     mov bh,13
     call systemwriteLn
     call systemgetkey
     call TestOVLProc3
   mov edi,offset Mesg2
   mov bh,13
   call systemwriteLn
   mov ah,4ch
   int 21h

OVLNOTFOUND:                      ; OVL not found or Too less memory
   mov edi,offset ErrorMesg
   mov bh,15
   call SystemWriteLn             ; from system.inc
   mov ah,4ch                     ; terminate program
   int 21h

.include system.inc                ; include system.inc
.smart                             ; you should assemble with the optimize
                                  ; option, because only the print procedure
                                  ; is used!
END

