.COMMENT
 ****************************************************************************
 CIRCLE.ASM     Example file for module GRAPH.INC
                (c) 1996,1998 by Dieter Pawelczak - Pass32 Version 2.0
 ============================================================================
 ****************************************************************************
.MODEL FLAT
.UCU
.NM

.INCLUDE graph.inc

.DATA
  mesg1         db   'Circle Demo (c) 1997 by Dieter Pawelczak',0

.CODE
START:
  call initgraph
  call initrandom       ; prepare the getrandom function
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
DrawCircles:
  mov eax,300          ; X-Position
  call getrandom
  add ax,20
  mov cx,ax
  mov eax,180          ; Y-Position
  call getrandom
  add ax,20
  mov dx,ax
  movzx eax,bx
  shr eax,1
  add ax,2
  call getrandom       ; Radius 1
  inc ax
  mov si,ax
  movzx eax,bx
  shr eax,1
  add ax,2
  call getrandom       ; Radius 1
  add ax,2
  mov di,ax
  inc bx               ; All Colors
  cmp bx,112
  jb next2
  xor bx,bx
next2:
  call circle          ; Draw circle randomly
  call waitforverticalretrace
  call keypressed
  je drawcircles       ; repeat until keypressed
  call SetTextMode
  mov ah,4ch
  int 21h              ; return to dos

PROC KeyPressed    NEAR     ; Z Flag if No Keypressed
     mov ax,[4]         ; Zero Selector
     mov gs,ax
     mov ax,gs:[41Ah]
     cmp ax,gs:[41Ch]
     ret
ENDP KeyPressed

PROC ColoredLine            ; draws a colored horizontal line at DX: Y
  mov cx,319
Color@LOOP:
  mov bx,cx
  shr bx,3
  call putpixel
  loop Color@LOOP
  ret
ENDP  ColoredLine


InitRandom PROC
.DATA
  Rand1         dd      ?
  Rand2         dd      ?
  Rand3         dd      ?
  BIOSSEL       dw      ?
.CODE
  push gs
  mov bx,0040h
  mov ax,0002h
  int 31h                       ; Get Selector For BIOS DATA 0040:0000
  mov BiosSel,ax
  mov GS,AX
  mov EAX,GS:[6ch]              ; Get Bios Timer
  mov Rand1,EAX                 ; Init First Random Var
  or DX,01101101b
  mul dx
  mov Rand2,EAX                 ; Init Second Random Var
  rol EAX,11
  add dx,ax
  mov edx,RAND1
  xor EAX,EDX
  mov Rand3,EAX                 ; Init Third Random Var
  pop gs                        ; restore Zero Selector
  ret
ENDP InitRandom

GetRandom PROC                  ; AX = Random Area 0..AX
  push gs
  push bx
  push dx
  push cx
  push ax
  mov ax,BiosSel
  mov GS,AX
  mov EAX,GS:[6ch]              ; Get Bios Timer
  mov Cl,Byte PTR Rand1
  shl eax,Cl
  or al,cl
  or EAX,Rand1
  mov DX,Word PTR Rand2
  mul DX
  SHL Eax,8
  add EAX,Rand2
  XOR EAX,Rand3
  add Eax,Rand1
  push EAX
  mov EAX,Rand2
  mov Rand1,EAX
  mov Eax,Rand3
  mov Rand2,Eax
  pop Eax
  Xadd Rand3,Eax                ; EAX is New Random Var
  pop BX                        ; max size of Randomvar
  mov ECX,EAX                   ; store EAX in ECX
  mov eax,0ffffh
  mul bx                        ; get max Size for division in DX:AX
  shl edx,16
  add eax,edx                   ; EAX is max divident
PrepareForDivision:
  cmp ecx,eax
  jb OkForDivision
  shr ecx,1
  jmp PrepareForDivision

  OkForDivision:
  mov EDX,ECX
  shr EDX,16
  mov AX,CX
  cmp AX,0                     ; dividend zero
  je ZeroDivide
  cmp BX,0                     ; dividend zero
  je ZeroDivide
  div bx                                ; DX:AX  div  BX
ZeroDivide:
  movzx EAX,DX
  pop cx
  pop dx
  pop bx
  pop gs
  ret
ENDP GetRandom


END START
END
