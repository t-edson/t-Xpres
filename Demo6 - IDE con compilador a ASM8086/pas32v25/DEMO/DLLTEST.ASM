.COMMENT
 ****************************************************************************
 DLLTEST.ASM    Example file for to demonstrate DLL loading
                Needs TESTDLL.DLL!
                (c) 1996,1998 by Dieter Pawelczak - Pass32 Version 2.0
 ============================================================================
 ****************************************************************************
.MODEL FLAT
.UCU
.NM


.include TESTDLL.ASM            ; includes the DLL Header

 DLL_ERROR      .EQU    0       ; display error automatically = 1

.DATA
.ALIGN 4
   DLLname      db    'TESTDLL.DLL',0
   ErrorMesg    db    'ERROR: TESTDLL.DLL not found!',0
   ErrorMesg2   db    'ERROR: Too less memory available!',0
   Mesg1        db    'Press any key to go on!',0
   Mesg2        db    'End of Demo!',0

.CODE
.PUBLIC START:
   mov esi,offset TESTDLL       ; OFFSET TO DATA BUFFER TESTDLL
   mov edi,offset DLLname       ; OFFSET TO Filename
   call InitDLL
   call loadDLL
   jc dllnotfound
     call TestDLLMain
     mov edi,offset Mesg1
     mov bh,13
     call systemwriteLn
     call systemgetkey
     call TestDLLProc1
     call TestDLLProc2
     mov edi,offset Mesg1
     mov bh,13
     call systemwriteLn
     call systemgetkey
     call TestDLLProc3
   mov edi,offset Mesg2
   mov bh,13
   call systemwriteLn
   mov esi,offset TESTDLL        ; OFFSET TO DATA BUFFER TESTDLL
   call FreeDLL                  ; dispose DLL!
   mov ah,4ch
   int 21h

DLLNOTFOUND:                      ; DLL not found or Too less memory
   mov edi,offset ErrorMesg
   cmp ax,1                       ; AX=1 Memory Error
   jne LoadError
   mov edi,offset ErrorMesg2
   LoadError:
   mov bh,15
   call SystemWriteLn             ; from system.inc
   mov ah,4ch                     ; terminate program
   int 21h

.include system.inc                ; include system.inc
.smart                             ; you should assemble with the optimize
                                  ; option, because only the print procedure
                                  ; is used!
END START
END
