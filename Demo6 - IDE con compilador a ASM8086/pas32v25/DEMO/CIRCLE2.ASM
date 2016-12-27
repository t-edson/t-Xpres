.COMMENT
 ****************************************************************************
 CIRCLE.ASM     Example file for module GRAPHDLL
                (c) 1996,1998 by Dieter Pawelczak - Pass32 Version 2.0
 ============================================================================
 ****************************************************************************
.MODEL FLAT
.UCU
.NM


.include system.inc
.include graphdll


debugvideo .equ 1

.DATA
.ALIGN 4
   DLLname      db    'GRAPHDLL.DLL',0
   ErrorMesg    db    'ERROR: GRAPHDLL.DLL not found!',0
   ErrorMesg2   db    'ERROR: Too less memory available!',0
   mesg1        db    'Circle Demo with DLL by Dieter Pawelczak',0
   counter      dw   0
   counter2     dw   0

.CODE
.PUBLIC START:
   mov esi,offset GRAPHDLL      ; OFFSET TO DATA BUFFER
   mov edi,offset DLLname       ; OFFSET TO Filename
   call InitDLL
   call loadDLL
   jc dllnotfound

; *** Now DLL is load ***
   call initgraph
   xor cx,cx
   xor dx,dx
   mov si,319
   mov di,199
   mov bl,68
   call FillBlock        ; fill screen with dark blue
   mov dx,10
   call ColoredLine
   mov al,65
   mov cx,0
   mov dx,0
   mov bl,31
   mov edi,offset mesg1
   call outtextxy       ; display message
   xor bx,bx
DrawCircles:
   inc bx
   inc counter
   inc counter2
   cmp bx,112
   jb next2
   xor bx,bx
   dec counter2
next2:
   cmp counter,80
   jb next3
   mov counter,0
next3:
   cmp counter2,80
   jb next4
   mov counter2,0
next4:
   mov cx,160
   mov dx,100
   mov si,counter
   mov di,counter2
   call circle          ; Draw circle randomly
   call waitforverticalretrace
   call systemkeypressed
   je drawcircles       ; repeat until keypressed
   call SetTextMode
   mov ah,4ch
   int 21h              ; return to dos


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

PROC ColoredLine            ; draws a colored horizontal line at DX: Y
  mov cx,319
Color@LOOP:
  mov bx,cx
  shr bx,3
  call putpixel
  loop Color@LOOP
  ret
ENDP  ColoredLine



END START
END