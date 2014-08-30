.COMMENT
 ****************************************************************************
 DEMO.ASM       Graphic demo using different graphic dlls.
                For the VESA modes, it checks first, if a VESA V2.0 standard
                is supported.
 ============================================================================
 ****************************************************************************

.include graphic.inc
.include system.mac


.smart3
.smart2

Maxstars        .EQU        45
DLL_ERROR       .EQU         1


.data
  starsX    dd MaxStars dup (0)
  starsY    dd MaxStars dup (0)
  starsDX   dd MaxStars dup (0)
  starsDY   dd MaxStars dup (0)
  starsDX2  dd MaxStars dup (0)
  starsDY2  dd MaxStars dup (0)
  starsPt   db MaxStars dup (0)
  palname   db 'graph.pal'
.data?
  old1c     dd ?
  old1csel  dw ?
  minX      dd ?
  minY      dd ?
  maxX      dd ?
  maxY      dd ?
  imagebuffer dd 224*64 dup(?)
.code

; initializes star-position and delta X, delta Y

proc initstars
  mov ecx,0
L1:
; Init Start PosX
  call getmaxx
  shr eax,1
  mov [starsX+4*ecx],eax
; Init Start PosY
  call getmaxy
  shr eax,1
  mov [starsY+4*ecx],eax
  mov eax,13
  call getrandom
  sub eax,7
  cmp eax,0
  jne short L2
  dec eax
L2:
  mov [starsDX+4*ecx],eax
  mov eax,16
  call getrandom
  sub eax,8
  cmp eax,0
  jne short L3
  dec eax
L3:
  mov [starsDY+4*ecx],eax
  inc ecx
  cmp ecx,MaxStars
  jne L1
  ret
endp initstars

proc stars
  mov edi,0
l1:
  mov ecx,[starsX+4*edi]
  mov edx,[starsY+4*edi]
  mov bl,[starsPt+edi]
  call putpixel
  inc edi
  cmp edi,MaxStars
  jnz L1
  mov edi,0
l2:
  mov eax,[starsDX+4*edi]
  add [starsDX2+4*edi],eax
  mov eax,[starsDX2+4*edi]
  cdq
  mov ebx,7
  idiv ebx
  mov ecx,[starsX+4*edi]
  add ecx,eax
  mov [starsX+4*edi],ecx
  mov eax,[starsDY+4*edi]
  add [starsDY2+4*edi],eax
  mov eax,[starsDY2+4*edi]
  jmp short m4
m3:
  sub [starsDY2+4*edi],1
  add eax,[starsDY2+4*edi]
  mov [starsDY+4*edi],eax
m4:
  cdq
  mov ebx,7
  idiv ebx
  mov edx,[starsY+4*edi]
  add edx,eax
  mov [starsY+4*edi],edx
  inc edi
  cmp edi,MaxStars
  jnz L2
  mov edi,0
l3:
  mov ecx,[starsX+4*edi]
  mov edx,[starsY+4*edi]
  call getmaxx
  cmp ecx,eax
  jae l5
  call getmaxy
  cmp edx,eax
  jae l5
Cont:
  call getpixel
  mov [starsPt+edi],bl
  inc edi
  cmp edi,MaxStars
  jnz L3
  mov edi,0
l4:
  mov ecx,[starsX+4*edi]
  mov edx,[starsY+4*edi]
  mov bl,31
  call putpixel
  inc edi
  cmp edi,MaxStars
  jnz L4
  ret
L5:
  call getmaxx
  shr eax,1
  mov [starsX+4*edi],eax
; Init Start PosY
  call getmaxy
  shr eax,1
  mov [starsY+4*edi],eax
  mov eax,15
  call getrandom
  sub eax,7
  cmp eax,0
  jne short L6
  dec eax
L6:
  mov [starsDX+4*edi],eax
  mov eax,16
  call getrandom
  sub eax,8
  mov [starsDY+4*edi],eax
  xor eax,eax
  mov [starsDX2+4*edi],eax
  mov [starsDY2+4*edi],eax
  mov ecx,[starsX+4*edi]
  mov edx,[starsY+4*edi]
  jmp  cont
  ret
endp stars

proc demo_1     ; draws points in an rectangle
  call clearscreen
  drawtext(20,0,'Draw Pixels with direct video access',14);
  call getmaxx
  mov ebx,3
  xor edx,edx
  div ebx
  mov esi,eax
  mov MaxX,eax
  call getmaxy
  sub eax,8
  mov MaxY,eax
  mov edi,eax
  mov ecx,8
  mov minX,ecx
  mov edx,32
  mov minY,edx
  mov bl,14
  call rectangle
Loop1:
  mov eax,Maxx
  sub eax,MinX
  dec eax
  call getrandom
  add eax,Minx
  inc eax
  push eax
  mov eax,MaxY
  sub eax,MinY
  dec eax
  call getrandom
  add eax,MinY
  inc eax
  push eax
  mov eax,255
  call getrandom
  inc eax
  mov ebx,eax
  pop edx
  pop ecx
  call putpixel
  call systemkeypressed
  je loop1
  call systemgetkey
  ret
endp demo_1

proc demo_2     ; draws points in an rectangle with buffer
.code
  drawtext(20,0,'Draw Pixels with direct video access',7);
  call getmaxx
  mov ecx,eax
  sub ecx,26*8
  drawtext(ecx,16,'Draw Pixels with buffer',14);
  mov eax,1
  call setpage
  call clearscreen
  call getmaxx
  mov ebx,3
  xor edx,edx
  div ebx
  mov esi,eax
  call getmaxx
  sub eax,esi
  sub eax,8
  mov MinX,eax
  mov esi,eax
  call getmaxy
  sub eax,8
  mov MaxY,eax
  mov edi,eax
  call getmaxx
  mov ecx,eax
  sub ecx,8
  mov maxX,ecx
  mov edx,32
  mov minY,edx
  mov bl,14
  call rectangle
Loop0:
  mov ecx,2000
Loop1:
  push ecx
  mov eax,Maxx
  sub eax,MinX
  dec eax
  call getrandom
  add eax,Minx
  inc eax
  push eax
  mov eax,MaxY
  sub eax,MinY
  dec eax
  call getrandom
  add eax,MinY
  inc eax
  push eax
  mov eax,255
  call getrandom
  inc eax
  mov ebx,eax
  pop edx
  pop ecx
  call putpixel
  pop ecx
  loop Loop1
  mov ecx,MinX
  mov edx,MinY
  mov esi,MaxX
  mov edi,MaxY
  call flushwindow
  call systemkeypressed
  je loop0
  call systemgetkey
  call checkexit
  ret
endp demo_2

proc demo_3     ; draws lines in an rectangle
  mov eax,0
  call setpage
  call clearscreen
  drawtext(20,0,'Draw Lines with direct video access',14);
  call getmaxx
  mov ebx,3
  xor edx,edx
  div ebx
  mov esi,eax
  mov MaxX,eax
  call getmaxy
  sub eax,8
  mov MaxY,eax
  mov edi,eax
  mov ecx,8
  mov minX,ecx
  mov edx,32
  mov minY,edx
  mov bl,14
  call rectangle
Loop1:
  mov eax,Maxx
  sub eax,MinX
  dec eax
  call getrandom
  add eax,Minx
  inc eax
  push eax
  mov eax,MaxY
  sub eax,MinY
  dec eax
  call getrandom
  add eax,MinY
  inc eax
  push eax
  mov eax,Maxx
  sub eax,MinX
  dec eax
  call getrandom
  add eax,Minx
  inc eax
  push eax
  mov eax,MaxY
  sub eax,MinY
  dec eax
  call getrandom
  add eax,MinY
  inc eax
  push eax
  mov eax,255
  call getrandom
  inc eax
  mov ebx,eax
  pop edx
  pop ecx
  pop edi
  pop esi
  call line
  call systemkeypressed
  je loop1
  call systemgetkey
  ret
endp demo_3

proc demo_4     ; draws lines in an rectangle with buffer
.code
  drawtext(20,0,'Draw Lines with direct video access',7);
  call getmaxx
  mov ecx,eax
  sub ecx,26*8
  drawtext(ecx,16,'Draw lines with buffer',14);
  mov eax,1
  call setpage
  call clearscreen
  call getmaxx
  mov ebx,3
  xor edx,edx
  div ebx
  mov esi,eax
  call getmaxx
  sub eax,esi
  sub eax,8
  mov MinX,eax
  mov esi,eax
  call getmaxy
  sub eax,8
  mov MaxY,eax
  mov edi,eax
  call getmaxx
  mov ecx,eax
  sub ecx,8
  mov maxX,ecx
  mov edx,32
  mov minY,edx
  mov bl,14
  call rectangle
Loop0:
  mov ecx,200
Loop1:
  push ecx
  mov eax,Maxx
  sub eax,MinX
  dec eax
  call getrandom
  add eax,Minx
  inc eax
  push eax
  mov eax,MaxY
  sub eax,MinY
  dec eax
  call getrandom
  add eax,MinY
  inc eax
  push eax
  mov eax,Maxx
  sub eax,MinX
  dec eax
  call getrandom
  add eax,Minx
  inc eax
  push eax
  mov eax,MaxY
  sub eax,MinY
  dec eax
  call getrandom
  add eax,MinY
  inc eax
  push eax
  mov eax,255
  call getrandom
  inc eax
  mov ebx,eax
  pop edx
  pop ecx
  pop edi
  pop esi
  call line
  pop ecx
  dec ecx
  jnz Loop1
  mov ecx,MinX
  mov edx,MinY
  mov esi,MaxX
  mov edi,MaxY
  call flushwindow
  call systemkeypressed
  je loop0
  call systemgetkey
  call checkexit
  ret
endp demo_4

proc demo_5     ; draws rectangles in an rectangle
  mov eax,0
  call setpage
  call clearscreen
  drawtext(0,0,'Draw Rectangles with direct video access',14);
  call getmaxx
  mov ebx,3
  xor edx,edx
  div ebx
  mov esi,eax
  mov MaxX,eax
  call getmaxy
  sub eax,8
  mov MaxY,eax
  mov edi,eax
  mov ecx,8
  mov minX,ecx
  mov edx,32
  mov minY,edx
  mov bl,14
  call rectangle
Loop1:
  mov eax,Maxx
  sub eax,MinX
  dec eax
  call getrandom
  add eax,Minx
  inc eax
  push eax
  mov eax,MaxY
  sub eax,MinY
  dec eax
  call getrandom
  add eax,MinY
  inc eax
  push eax
  mov eax,Maxx
  sub eax,MinX
  dec eax
  call getrandom
  add eax,Minx
  inc eax
  push eax
  mov eax,MaxY
  sub eax,MinY
  dec eax
  call getrandom
  add eax,MinY
  inc eax
  push eax
  mov eax,255
  call getrandom
  inc eax
  mov ebx,eax
  pop edx
  pop ecx
  pop edi
  pop esi
  call rectangle
  call systemkeypressed
  je loop1
  call systemgetkey
  ret
endp demo_5

proc demo_6     ; draws lines in an rectangle with buffer
.code
  drawtext(0,0,'Draw Rectangles with direct video access',7);
  call getmaxx
  mov ecx,eax
  sub ecx,32*8
  drawtext(ecx,16,'Draw Rectangles with buffer',14);
  mov eax,1
  call setpage
  call clearscreen
  call getmaxx
  mov ebx,3
  xor edx,edx
  div ebx
  mov esi,eax
  call getmaxx
  sub eax,esi
  sub eax,8
  mov MinX,eax
  mov esi,eax
  call getmaxy
  sub eax,8
  mov MaxY,eax
  mov edi,eax
  call getmaxx
  mov ecx,eax
  sub ecx,8
  mov maxX,ecx
  mov edx,32
  mov minY,edx
  mov bl,14
  call rectangle
Loop0:
  mov ecx,200
Loop1:
  push ecx
  mov eax,Maxx
  sub eax,MinX
  dec eax
  call getrandom
  add eax,Minx
  inc eax
  push eax
  mov eax,MaxY
  sub eax,MinY
  dec eax
  call getrandom
  add eax,MinY
  inc eax
  push eax
  mov eax,Maxx
  sub eax,MinX
  dec eax
  call getrandom
  add eax,Minx
  inc eax
  push eax
  mov eax,MaxY
  sub eax,MinY
  dec eax
  call getrandom
  add eax,MinY
  inc eax
  push eax
  mov eax,255
  call getrandom
  inc eax
  mov ebx,eax
  pop edx
  pop ecx
  pop edi
  pop esi
  call rectangle
  pop ecx
  dec ecx
  jne Loop1
  mov ecx,MinX
  mov edx,MinY
  mov esi,MaxX
  mov edi,MaxY
  call flushwindow
  call systemkeypressed
  je loop0
  call systemgetkey
  call checkexit
  ret
endp demo_6

proc draw_win
  drawicon(minx,miny,icon+1*256);
  mov ecx,minx
  add ecx,16
L1:
  drawicon(ecx,miny,icon+2*256);
  add ecx,16
  cmp ecx,maxx
  jb L1
  drawicon(ecx,miny,icon+3*256);

  mov edx,miny
  add edx,16
  drawicon(minx,edx,icon+4*256);
  mov ecx,minx
  add ecx,16
L2:
  drawicon(ecx,edx,icon+5*256);
  add ecx,16
  cmp ecx,maxx
  jb L2
  drawicon(ecx,edx,icon+6*256);
  mov edx,miny
  add edx,32
  drawicon(minx,edx,icon+4*256);
  mov ecx,minx
  add ecx,16
L3:
  drawicon(ecx,edx,icon+5*256);
  add ecx,16
  cmp ecx,maxx
  jb L3
  drawicon(ecx,edx,icon+6*256);
  mov edx,miny
  add edx,48
  drawicon(minx,edx,icon+7*256);
  mov ecx,minx
  add ecx,16
L4:
  drawicon(ecx,edx,icon+8*256);
  add ecx,16
  cmp ecx,maxx
  jb L4
  drawicon(ecx,edx,icon+9*256);
  mov ecx,minx
  add ecx,16
  mov edx,miny
  add edx,16
  drawicon(ecx,edx,icon+10*256)
  add edx,16
  drawicon(ecx,edx,icon+12*256)
  add ecx,16
  sub edx,16
  drawicon(ecx,edx,icon+11*256)
  add edx,16
  drawicon(ecx,edx,icon+13*256)
  ret
endp draw_win


proc demo_7
  mov eax,0
  call setpage
  call clearscreen
  mov edi,offset palname
  mov eax,0
  call loadpalette
  mov minx,32
  mov maxx,272
  mov miny,64
  call draw_win
  drawtext(100,70,'Draw Icons',12);
  drawtext(90,90,'Directly Written',12);
  call systemgetkey
  call checkexit
  ret
endp demo_7

proc demo_8
  mov eax,1
  call setpage
  mov eax,4
  mov minx,eax
  mov eax,228
  mov maxx,eax
  mov eax,4
  mov miny,eax
  call clearscreen
  call flushbuffer
L0:
  call clearscreen
  call draw_win
  mov ecx,minx
  add ecx,64
  mov edx,miny
  add edx,6
  drawtext(ecx,edx,'Draw Icons',12);
  mov ecx,minx
  add ecx,54
  mov edx,miny
  add edx,26
  drawtext(ecx,edx,'Buffered Written',12);
  call waitforverticalretrace
  mov ecx,minx
  sub ecx,4
  mov edx,miny
  sub edx,4
  mov esi,maxx
  add esi,16
  mov edi,edx
  add edi,68

  call flushwindow
  call getmaxy
  sub eax,68
  cmp miny,eax
  jae short L1
  add miny,4
L1:
  call getmaxx
  sub eax,16*16
  cmp minx,eax
  jae short L2
  add minx,4
  add maxx,4
L2:
  call systemkeypressed
  je L0
l6:
  call systemgetkey
  call checkexit
  ret
endp demo_8

proc demo_9     ; draws lines in an rectangle
  mov eax,0
  call setpage
  call clearscreen
  mov ecx,0
  mov minx,ecx
  mov miny,ecx
  mov ecx,224-16
  mov maxx,ecx
  call draw_win
  drawtext(20,6,'Copy Image',14);
  drawtext(50,26,'Copy Direct',14);
  mov ecx,0
  mov edx,0
  mov edi,64
  mov ebx,224
  mov esi,offset imagebuffer
  call getimage
  call getmaxx
  sub eax,224
  mov MaxX,eax
  call getmaxy
  sub eax,64
  mov MaxY,eax
  mov edi,eax
  mov ecx,8
Loop1:
  mov eax,Maxx
  call getrandom
  push eax
  mov eax,MaxY
  call getrandom
  push eax
  pop edx
  pop ecx
  mov ebx,224
  mov edi,64
  mov esi,offset imagebuffer
  call putimage
  call systemkeypressed
  je loop1
  call systemgetkey
  ret
endp demo_9

proc run_demo
  call demo_1
  call demo_2
  call demo_3
  call demo_4
  call demo_5
  call demo_6
  call demo_7
  call demo_8
  call demo_9
  ret
endp run_demo

proc checkexit
  cmp al,27
  jne short L1
  call settextmode
  mov eax,4c00h
  int 21h
L1:
  ret
endp checkexit

proc Newint1c
  push ds
  push gs
  pushad
  mov ds,word ptr cs:[0]
  mov gs,word ptr cs:[4]
  call getactpage
  push eax
  cmp al,0
  je short L1
  mov eax,0
  call setpage
L1:
  call stars
  pop eax
  cmp eax,0
  je short L2
  call setpage
l2:
  popad
  pop gs
  pop ds
  iret
endp Newint1c





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
  push ebx
  push edx
  push ecx
  push eax
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
  pop eBX                        ; max size of Randomvar
  mov ECX,EAX                   ; store EAX in ECX
  mov eax,0ffffh
  mul ebx                        ; get max Size for division in DX:AX
  shl edx,16
  add eax,edx                   ; EAX is max divident
PrepareForDivision:
  cmp ecx,eax
  jb short OkForDivision
  shr ecx,1
  jmp PrepareForDivision

  OkForDivision:
  mov EDX,ECX
  shr EDX,16
  mov AX,CX
  cmp AX,0                     ; dividend zero
  je short ZeroDivide
  cmp BX,0                     ; dividend zero
  je short ZeroDivide
  div bx                                ; DX:AX  div  BX
ZeroDivide:
  movzx EAX,DX
  pop ecx
  pop edx
  pop ebx
  pop gs
  ret
ENDP GetRandom

proc settimer
  mov edx,offset NewInt1c
  mov cx,cs
  mov ax,0205h
  mov bx,1ch
  int 31h
  ret
endp settimer

proc disabletimer
  mov edx,old1c
  mov cx,old1csel
  mov ax,0205h
  mov bx,1ch
  int 31h
  ret
endp disabletimer


begin:
  call initrandom
  mov ax,0204h
  mov bx,1ch
  int 31h
  mov old1c,edx
  mov old1csel,cx
L0:
  mov eax,0
  call initgraph
  jc error
  mov edi,offset palname
  mov eax,0
  call loadpalette
  call initstars
  call settimer
  drawtext(110,20,'Graphic Demo',14);
  drawtext(40,40,'(c) 1998 by Dieter Pawelczak',14);
  drawline(20,50,310,50,14);
  drawtext(80,70,'Choose Graphic Mode',12);
  drawtext(50,90,'1 - VGA 256 Colors 320x200',12);
  drawtext(50,102,'2 - XVGA 256 Colors 320x240',12);
  drawtext(50,114,'3 - VESA 256 Colors 640x400',12);
  drawtext(50,126,'4 - VESA 256 Colors 640x480',12);
  drawtext(50,138,'5 - VESA 256 Colors 800x600',12);
Loop1:
  call systemgetkey
  cmp al,27
  je exit
  cmp al,49
  je short demo1
  cmp al,50
  je short demo2
  cmp al,51
  je short demo3
  cmp al,52
  je short demo4
  cmp al,53
  je short demo5
  jmp short Loop1
demo1:
  mov eax,0
  jmp demox
demo2:
  mov eax,1
  jmp short demox
demo3:
  call disabletimer
  call settextmode
  mov eax,5       ; check vbe 2.0
  call initgraph
  jnc demox3
  call settextmode
  mov eax,2
    jmp short demox2
demo4:
  call disabletimer
  call settextmode
  mov eax,6       ; check vbe 2.0
  call initgraph
  jnc demox3
  call settextmode
  mov eax,3
  jmp short demox2

demo5:
  call disabletimer
  call settextmode
  mov eax,7       ; check vbe 2.0
  call initgraph
  jnc demox3
  call settextmode
  mov eax,4
  jmp short demox2

demox:
  push eax
  call disabletimer
  call settextmode
  pop eax
demox2:
  call initgraph
  jc error
demox3:
  call run_demo
exit:
  call settextmode
endx:
  mov ax,4c00h
  int 21h
error:
  writeln('Mode not supported!')
  exit(3)
.PUBLIC Icon:
.incbin demo.chr
end begin
