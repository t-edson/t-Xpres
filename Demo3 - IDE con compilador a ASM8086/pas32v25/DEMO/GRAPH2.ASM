.COMMENT
 ****************************************************************************
 GRAPH2.ASM     Example file for module GRAPH.INC as DLL
                Provides the same functions as GRAPH.INC,
                but loads functions dynaically from the
                GRAPHDLL.
            *** This is a module - do not assemble !!!! ***
                (c) 1996,1998 by Dieter Pawelczak - Pass32 Version 2.0
 ============================================================================
 ****************************************************************************

.include GRAPHDLL.ASM

.DATA
.ALIGN 4
   GRDLLname      db    'GRAPHDLL.DLL',0
   GRErrorMesg    db    'ERROR: GRAPHDLL.DLL not found!',0
   GRErrorMesg2   db    'ERROR: Too less memory available!',0
.CODE

.COMMENT *** This PROCEDURE will initialize the DLL automatically ***

PROC GraphInit START            ; START PROCEDURE !
   mov esi,offset GRAPHDLL       ; OFFSET TO DATA BUFFER
   mov edi,offset GRDLLname       ; OFFSET TO Filename
   call InitDLL
   call loadDLL
   jc grdllnotfound
   ret
grdllnotfound:                      ; DLL not found or Too less memory
   mov edi,offset GrErrorMesg
   cmp ax,1                       ; AX=1 Memory Error
   jne LoadError
   mov edi,offset GrErrorMesg2
   LoadError:
   mov bh,15
   call SystemWriteLn             ; from system.inc
   mov ah,4ch                     ; terminate program
   int 21h
   ret
ENDP GraphInit


PROC ColoredLine            ; draws a colored horizontal line at DX: Y
  mov cx,319
Color@LOOP:
  mov bx,cx
  shr bx,3
  call putpixel
  loop Color@LOOP
  ret
ENDP  ColoredLine



.include system.inc                ; include system.inc
;END GRAPH2
