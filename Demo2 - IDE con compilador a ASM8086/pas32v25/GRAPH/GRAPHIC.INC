.COMMENT
 ****************************************************************************
 GRAPHIC.INC    Main Graphic Driver V0.7
                This module provides a common interface to several different
                graphic DLLs.
 ============================================================================

 PUTPIXEL    : ECX = X-Axis, EDX = Y-Axis, EBX = color
 GETPIXEL    : ECX = X-Axis, EDX = Y-Axis, EBX = color
 LINE        : ECX = X1, EDX = Y1, ESI = X2, EDI = Y2, EBX = color
 RECTANGLE   : ECX = X1, EDX = Y1, ESI = X2, EDI = Y2, EBX = color
 FILL        : ECX = X1, EDX = Y1, ESI = X2, EDI = Y2, EBX = color
 CIRLCE      : ECX = X, EDX =Y, ESI = r1, EDI = r2, EBX = color
 OUTTEXTXY   : ECX = X-Axis, EDX = Y-Axis, EBX = color, EDI = offset to textstr.
 PUTSPRITE   : ECX = X, EDX = Y, EBX = Size X, EDI = Size Y, ESI = offset to sprite
 PutSpriteCombined : ECX = X, EDX = Y,  ESI = offset to sprite 16x16
                     copies sprite from ESI with background Buffer2 to Buffer1
 FLUSHBUFFER : copys activ buffer to screen
 CLEARSCREEN

 The following procedures assume DWORD alignment for X and the size X

 FASTFILL    : ECX = X1, EDX = Y1, ESI = X2, EDI = Y2, EBX = color
 PUTIMAGE    : ECX = X, EDX = Y, EBX = Size X, EDI = Size Y, ESI = offset to sprite
 GETIMAGE    : ECX = X, EDX = Y, EBX = Size X, EDI = Size Y, ESI = offset to sprite
 PUT16x16    : ECX = X, EDX = Y, EBX = Size X, EDI = Size Y, ESI = offset to sprite
 FLUSHWINDOW : ECX = X1, EDX = Y1, ESI = X2, EDI = Y2

 Driver Management procedures

 INITGRAPH   : AL = video mode
             : AL = FF, EDI = offset to driver name
 SETTEXTMODE : return to co80 mode and frees driver
 GETMAXX     : returns max value for X
 GETMAXY     : returns max value for Y
 SETPAGE     : EAX = page (0=screen, 1..n = BUFFER)
 GETACTPAGE  : returns in EAX active page
 LOADPALETTE : loads palette EAX = first color to read (typically 0)
 WAITFORVERTICALRETRACE : EAX and EDX destroyed

 Macros

 DrawText(X,Y,String,Color)
 DrawLine(X1,Y1,X2,Y2,Color)
 DrawRectangle(X1,Y1,X2,Y2,Color)
 DrawFill(X1,Y1,X2,Y2,Color)
 DrawImage(X1,Y1,X2,Y2,offs)
 DrawIcon(X1,Y1,offs)

 ****************************************************************************

; INCLUDE COMMON DRIVER INTERFACE *******************************************

.include vga256.inc

.include dpmi.inc
.include io.inc

.PUBLIC .DATA
; Definition of the user interface procedures
  PutPixel     df ?  ; x: ECX, y: EDX, Color: EBX
  GetPixel     df ?  ; x: ECX, y: EDX  ->  Color: EBX
  PutImage     df ?
  GetImage     df ?
  PutSprite    df ?
  Put16x16     df ?
  Fill         df ?
  FastFill     df ?

.DATA?
  Palette DB 4095 DUP(?)

.ALIGN 8

.Data
  Driver1      db   'VGA256.DLL',0
               db   'XVGA256.DLL',0
               db   'VESA.DLL',0
               db   'VESA1.DLL',0
               db   'VESA2.DLL',0
               db   'VBE.DLL',0,0,0
               db   'VBE1.DLL',0,0,0
               db   'VBE2.DLL',0,0,0
               db   'VESA3.DLL',0
               db   'VBE3.DLL',0,0,0
  UserDriver   db   64  dup(0)
.PUBLIC  RomFont      DD   0
  ActPage      db   0

.NOALIGN

.CODE

.COMMENT
==========================================================
Initgraph: Parameter:  EAX = DRIVER TO LOAD:
                          - 0  = VGA    320x200x256
                          - 1  = XVGA   320x240x256
                          - 2  = VESA   640x400x256
                          - 3  = VESA   640x480x256
                          - 4  = VESA   800x600x256
                          - 5  = VESA   640x400x256 linear V2.0
                          - 6  = VESA   640x480x256 linear V2.0
                          - 7  = VESA   800x600x256 linear V2.0
                          - FF = UserDefined:
                                 offset to driver in EDI
           Returns:    Carry Flag Clear on Success
==========================================================




PROC InitGraph
  push edi
  push eax
  mov esi,Offset Vga256
  call InitDLL
  pop eax
  pop edi
  cmp al,0ffh
  je short U2
U1:
  lea edi,[eax*2]
  lea edi,[edi*8+offset Driver1]
U2:
  mov esi,offset Vga256
  call loaddll
  jc X
  call InitVideoMode
  jc X
  mov ax,ds
  mov es,ax
  mov edi,offset intedi         ; make sure ES:EDI points to int-structure
  mov inteax,1130h              ; Get Offset of BIOS CHAR ROM
  mov intebx,300h
  mov ax,300h                   ; DPMI Function 0300h: Call Real Mode Int
  xor cx,cx                     ; No parameters on the PM Stack
  mov bx,10h
  int 31h                       ; call real mode int 10h Function 1130h
  xor eax,eax
  xor ebx,ebx
  mov ax,intes                  ; ax = real mode ES
  mov bx,intbp                  ; bx = real mode BP
  shl eax,4
  add eax,ebx                   ; calculate linear address (Segment+Offset)
  mov RomFont,eax               ; save address of ROM character set
  mov eax,0
  call setpage
  ret
X:
  stc
  ret
ENDP Initgraph

PROC SetTextmode
   call freeDriver
   mov esi,offset Vga256
   call freedll
   mov ax,3
   int 10h
   ret
ENDP SetTextMode

PROC SetPage
; sets virtual procedures according to page and mode
  mov actpage,al
  cmp al,0
  jne L1
; page 0 means direct output to screen
  mov eax,dword ptr PutImageDirect
  mov dword ptr PutImage,eax
  mov eax,dword ptr GetImageDirect
  mov dword ptr GetImage,eax
  mov ax,word ptr PutImageDirect+4
  mov word ptr PutImage+4,ax
  mov ax,word ptr GetImageDirect+4
  mov word ptr GetImage+4,ax
  mov eax,dword ptr PutPixelDirect
  mov dword ptr PutPixel,eax
  mov eax,dword ptr GetPixelDirect
  mov dword ptr GetPixel,eax
  mov ax,word ptr PutPixelDirect+4
  mov word ptr PutPixel+4,ax
  mov ax,word ptr GetPixelDirect+4
  mov word ptr GetPixel+4,ax
  mov eax,dword ptr PutSpriteDirect
  mov dword ptr PutSprite,eax
  mov ax,word ptr PutSpriteDirect+4
  mov word ptr PutSprite+4,ax
  mov eax,dword ptr FillDirect
  mov dword ptr Fill,eax
  mov ax,word ptr FillDirect+4
  mov word ptr Fill+4,ax
  mov eax,dword ptr FastFillDirect
  mov dword ptr FastFill,eax
  mov ax,word ptr FastFillDirect+4
  mov word ptr FastFill+4,ax
  mov eax,dword ptr Put16x16Direct
  mov dword ptr Put16x16,eax
  mov ax,word ptr Put16x16Direct+4
  mov word ptr Put16x16+4,ax
  ret
L1:
; page 1.. means output to buffer
  call setbuffer
  jc L2
  mov eax,dword ptr PutImageBuffer
  mov dword ptr PutImage,eax
  mov eax,dword ptr GetImageBuffer
  mov dword ptr GetImage,eax
  mov ax,word ptr PutImageBuffer+4
  mov word ptr PutImage+4,ax
  mov ax,word ptr GetImageBuffer+4
  mov word ptr GetImage+4,ax
  mov eax,dword ptr PutPixelBuffer
  mov dword ptr PutPixel,eax
  mov eax,dword ptr GetPixelBuffer
  mov dword ptr GetPixel,eax
  mov ax,word ptr PutPixelBuffer+4
  mov word ptr PutPixel+4,ax
  mov ax,word ptr GetPixelBuffer+4
  mov word ptr GetPixel+4,ax
  mov eax,dword ptr PutSpriteBuffer
  mov dword ptr PutSprite,eax
  mov ax,word ptr PutSpriteBuffer+4
  mov word ptr PutSprite+4,ax
  mov eax,dword ptr FillBuffer
  mov dword ptr Fill,eax
  mov ax,word ptr FillBuffer+4
  mov word ptr Fill+4,ax
  mov eax,dword ptr FastFillBuffer
  mov dword ptr FastFill,eax
  mov ax,word ptr FastFillBuffer+4
  mov word ptr FastFill+4,ax
  mov eax,dword ptr Put16x16Buffer
  mov dword ptr Put16x16,eax
  mov ax,word ptr Put16x16Buffer+4
  mov word ptr Put16x16+4,ax
L2:
  ret
ENDP SetPage

PROC GetActPage
  Movzx eax,ActPage
  ret
ENDP GetActPage



PROC Line               ;ECX:X1, EDX:Y1, ESI:X2, EDI:Y2   Bl:color;
; Bresenham-Algorithmus

.DATA
  Linedx            dd   0
  Linedy            dd   0
  LinedAB           dd   0
  LineIncA          dd   0
  LineIncB          dd   0
  LineX             dd   0
  LineY             dd   0
  Lineh1            dd   0
  Lineh2            dd   0
  LineX1            dd   0
  LineX2            dd   0
  LineY1            dd   0
  LineY2            dd   0
.CODE
  cmp ecx,esi
  je short L1
  jmp short L2
L1:
  cmp edx,edi
  jne short L2
  call putpixel
  ret
L2:
  mov LineX1,ecx
  mov LineX2,esi
  mov LineY1,edx
  mov LineY2,edi
  cmp ecx,esi              ;             IF x1 > x2 THEN
  jle short DrawLineArg1
  mov LineX1,esi
  mov LineX2,ecx          ;  x1 <-> x2
  mov LineY1,edi
  mov LineY2,edx          ;  y1 <-> y2
DrawLineArg1:
  mov eax,LineX2
  sub eax,LineX1
  mov LineDx,eax         ;      dx := x2-x1;
  mov LineH1,eax;        ;      h1:=dx
  mov eax,LineY2
  sub eax,LineY1
  mov LineDy,eax         ;      dy := y2-y1;
  mov Lineh2,eax         ;      h2 := dy;
  cmp eax,0
  jge short DrawLineNext1 ;      IF (dx < -dy) AND (dy < 0)
  neg eax
  cmp Linedx,eax
  jge short  DrawLineNext1     ;      THEN
  neg LineY1             ;    y1:=-y1;
  neg LineY2             ;    y2:=-y2;
  mov eax,LineX1
  mov ecx,LineY1
  mov LineX1,ecx         ;     x1 <> y1;
  mov LineY1,eax
  mov eax,LineX2
  mov ecx,LineY2
  mov LineX2,ecx         ;     x2 <> y2;
  mov LineY2,eax
DrawLineNext1:
  mov eax,LineDY
  cmp eax,0
  jge short DrawLineNext2 ;     IF (dx >= -dy) AND (dy < 0)
  neg eax
  cmp LineDx,eax
  jl short DrawLineNext2     ;     THEN
  neg LineY1             ;    y1:=-y1;
  neg LineY2             ;    y2:=-y2;
DrawLineNext2:
  mov eax,LineDy
  cmp eax,0
  jle short DrawLineNext3 ;      IF (dx <= dy) AND (dy > 0)
  cmp LineDx,eax
  jnle short DrawLineNext3      ;      THEN
  mov eax,LineX1
  mov ecx,LineY1
  mov LineX1,ecx
  Mov LineY1,eax         ;       x1 <> y1
  mov eax,LineX2
  mov ecx,LineY2
  mov LineX2,ecx
  Mov LineY2,eax         ;       x2 <> y2
DrawLineNext3:
  mov eax,LineX2
  sub eax,LineX1
  mov LineDx,eax         ;      dx := x2-x1;
  mov eax,LineY2
  sub eax,LineY1
  mov LineDy,eax         ;      dy := y2-y1;
  shl eax,1
  mov LineIncB,eax       ;      IncB := 2*dy;
  sub eax,Linedx
  mov LinedAB,eax        ;      dAB := 2*dy-dx;
  mov eax,LineDy
  sub eax,LineDx
  shl eax,1
  mov LineIncA,eax      ;       IncA := 2*(dy-dx);
  mov eax,LineX1
  mov LineX,eax          ;      x := x1;
  mov eax,LineY1
  mov LineY,eax          ;      y := y1;
  mov eax,LineH2
  cmp eax,0
  jge short DrawPixel1   ;     IF (h1 < -h2) AND (h2 < 0)
  neg eax
  cmp LineH1,eAx
  jge short DrawPixel1        ;   THEN
  mov ecx,LINEY
  mov edx,LINEX
  neg edx                ;        Putpixel(y, -x, bl);
  call putpixel
  jmp Drawpixel4
DrawPixel1:
  mov eax, LineH2
  cmp eax,0
  jge short DrawPixel2   ;    IF (h1 >= -h2) AND (h2 < 0)
  neg eax
  cmp LineH1,eax
  jl short DrawPixel2         ;  THEN
  mov eCX,LineX
  mov edx,LineY
  neg edx
  call putpixel         ;        putpixel (x, -y, bl);
  jmp short Drawpixel4
DrawPixel2:
  mov eax,LineH2
  cmp eax,0
  jl short  Drawpixel3        ; IF (h1 > h2) AND (h2 >= 0)
  cmp LineH1,eax
  jle DrawPixel3         ; THEN
  mov ecx,LineX
  mov edx,LineY          ;
  call putpixel         ;        putpixel (x, y, bl);
  jmp short Drawpixel4
Drawpixel3:
  mov eax,LineH2
  cmp eax,0
  jle short DrawPixel4        ;  IF (h1 <= h2) AND (h2 >= 0)
  cmp LineH1,eax
  jnle short DrawPixel4         ; THEN
  mov ecx,LineY
  mov edx,LineX
  call putpixel
DrawPixel4:
  inc LineX             ;       FOR x := x1+1 TO x2 DO
  mov eax,LinedaB
  cmp eax,0              ; IF dAB < 0 THEN
  jge short DABAboveZero
  mov eax,LinedAB        ; Inc(dAB, IncB)
  add eax,LineIncB
  mov LinedAB,eAX
  jmp short  DABENDIF
DABAboveZero:
  mov eax,LinedAB        ; Inc(dAB, IncA)
  add eax,LineIncA
  mov LinedAB,eAX
  inc LineY
DABENDIF:
  mov eax,LineH2
  cmp eax,0
  jge short @DrawPixel1   ;     IF (h1 < -h2) AND (h2 < 0)
  neg eax
  cmp LineH1,eAx
  jge short @DrawPixel1   ;   THEN
  mov ecx,LINEY
  mov edx,LINEX
  neg edx                 ;        Putpixel(y, -x, bl);
  call putpixel
  jmp @Drawpixel4
@DrawPixel1:
  mov eax, LineH2
  cmp eax,0
  jge short @DrawPixel2   ;    IF (h1 >= -h2) AND (h2 < 0)
  neg eax
  cmp LineH1,eax
  jl short @DrawPixel2    ;  THEN
  mov eCX,LineX
  mov edx,LineY
  neg edx
  call putpixel         ;        putpixel (x, -y, bl);
  jmp short @Drawpixel4
@DrawPixel2:
  mov eax,LineH2
  cmp eax,0
  jl short @Drawpixel3        ; IF (h1 > h2) AND (h2 >= 0)
  cmp LineH1,eax
  jle short @DrawPixel3         ; THEN
  mov ecx,LineX
  mov edx,LineY          ;
  call putpixel         ;        putpixel (x, y, bl);
  jmp short @Drawpixel4
@Drawpixel3:
  mov eax,LineH2
  cmp eax,0
  jle short @DrawPixel4        ;  IF (h1 <= h2) AND (h2 > 0)
  cmp LineH1,eax
  jg short @DrawPixel4         ; THEN
  mov ecx,LineY
  mov edx,LineX
  call putpixel         ;        putpixel (y, x, bl);
@DrawPixel4:
  mov eax,LineX
  cmp eax,LineX2
  jl  DrawPixel4
ret
ENDP Line

PROC Rectangle
.DATA
    rectx1 DD 0
    rectx2 DD 0
    recty1 DD 0
    recty2 DD 0
.CODE
  mov rectx1,ecx
  mov rectx2,esi
  mov recty1,edx
  mov recty2,edi
  mov edi,edx
  call line
  mov ecx,rectx1
  mov esi,rectx2
  mov edx,recty2
  mov edi,edx
  call line
  mov ecx,rectx1
  mov esi,ecx
  mov edx,recty1
  mov edi,recty2
  call line
  mov ecx,rectx2
  mov esi,ecx
  mov edx,recty1
  mov edi,recty2
  call line
  ret
ENDP Rectangle

PROC Circle;   x: ECX, y: EDX, ESI: x-Radius, EDI: y-Radius, EBX: Color
.DATA
  h1 DD ?
  h2 DD ?
  h3 DD ?
  ix DD ?
  iy DD ?
.CODE
  cmp esi,0
  jle X
  cmp edi,0
  jle X
  mov ix,0      ;     ix := 0;
  cmp esi,edi   ;     IF xRadius > yRadius THEN
  jle short L1  ;

  mov iy,esi    ;     iy := xRadius;
  mov h1,esi    ;     h1 := xRadius;
  mov h2,edi
  fild h2       ;     f := yRadius / xRadius
  fild h1       ;     ST(0) = xRadius , ST(1) = yRadius
  fdivr ST,ST(1)
  jmp short L2
L1:             ;     ELSE
  mov iy,edi    ;     iy := xRadius;
  mov h1,edi    ;     h1 := xRadius;
  mov h2,esi
  fild h2       ;     f := xRadius / yRadius
  fild h1       ;     ST(0) = yRadius , ST(1) = xRadius
  fdivr ST,ST(1) ;     ST(0) = f
L2:
  mov eax,iy    ;     WHILE (ix<=iy)
  cmp ix,eax
  jg XX
  cmp h1,0      ;     IF h1 < 0 THEN
  jge L3        ;
  dec iy        ;     DEC iy
  mov eax,iy
  add h1,eax    ;     INC(h1, 2*iy);
  add h1,eax    ;     INC(h1, 2*iy);
  jmp short L2
L3:             ;     ELSE
  fild ix       ;     ST(0) = ix, ST(1) =f
  fmul st,st(1) ;
  fistp h2      ;     h2 := Round(ix*f);
  fild iy
  fmul st,st(1) ;     h3 := Round(iy*f);
  fistp h3
  cmp esi,edi   ;     IF xRadius > yRadius THEN
  jle L4  ;
  push ecx
  push edx
  sub ecx,ix
  sub edx,h3
  call putpixel ;     putpixel(x-ix, y-h3, color);
  add edx,h3
  add edx,h3
  call putpixel ;     putpixel(x-ix, y+h3, color);
  add ecx,ix
  add ecx,ix
  call putpixel ;     putpixel(x+ix, y+h3, color);
  sub edx,h3
  sub edx,h3
  call putpixel ;     putpixel(x+ix, y-h3, color);
  pop edx
  pop ecx
  push ecx
  push edx
  sub ecx,iy
  sub edx,h2
  call putpixel ;      putpixel(x-iy, y-h2, color);
  add edx,h2
  add edx,h2
  call putpixel ;      putpixel(x-iy, y+h2, color);
  add ecx,iy
  add ecx,iy
  call putpixel ;      putpixel(x+iy, y+h2, color);
  sub edx,h2
  sub edx,h2
  call putpixel ;      putpixel(x+iy, y-h2, color);
  pop edx
  pop ecx
  jmp L5
L4:
  push ecx
  push edx
  sub ecx,h2
  sub edx,iy
  call putpixel ;     putpixel(x-h2, y-iy, color);
  add edx,iy
  add edx,iy
  call putpixel ;     putpixel(x-h2, y+iy, color);
  add ecx,h2
  add ecx,h2
  call putpixel ;     putpixel(x+h2, y+iy, color);
  sub edx,iy
  sub edx,iy
  call putpixel ;     putpixel(x+h2, y-iy, color);
  pop edx
  pop ecx
  push ecx
  push edx
  sub ecx,h3
  sub edx,ix
  call putpixel ;     putpixel(x-h3, y-ix, color);
  add edx,ix
  add edx,ix
  call putpixel ;     putpixel(x-h3, y+ix, color);
  add ecx,h3
  add ecx,h3
  call putpixel ;     putpixel(x+h3, y+ix, color);
  sub edx,ix
  sub edx,ix
  call putpixel ;     putpixel(x+h3, y-ix, color);
  pop edx
  pop ecx
L5:
  mov eax,ix
  sub h1,eax
  sub h1,eax    ;     Dec(hilf1, 2*ix-1);
  inc ix        ;     Inc(ix);
  dec h1
  jmp L2
XX:
  fcompp
  X:
    ret
ENDP Circle

PROC outchar                    ; ECX: X EDX: Y , BL:Color  AL:Char
  push edx
  push edi
  push esi
  movzx esi,al
  shl esi,3                      ; ax * 8
  add esi,romfont
  push 8
L1:
  mov edi,7
L2:
  mov al,gS:[esi]               ; gs: Zero Selector
  bt eax,edi
  jnc short L3
  call putpixel
L3:
  inc ecx
  dec edi
  jns L2
  sub ecx,8
  inc edx
  inc esi
  dec dword ptr [esp]
  jnz L1
  pop eax
  pop esi
  pop edi
  pop edx
  ret
ENDP outchar

PROC OuttextXY                  ; ECX: X EDX: Y , BL:Color  EDI: Offset String
  call getmaxx
  sub eax,7
  push eax
L0:
  mov al,[edi]
  cmp al,0
  je short L1
  call outchar
  inc edi
  add ecx,8
  cmp [esp],ecx
  ja L0
L1:
  pop eax
  ret
ENDP OuttextXY

proc clearscreen
  call getmaxx
  dec eax
  mov esi,eax
  call getmaxy
  dec eax
  mov edi,eax
  xor ecx,ecx
  xor edx,edx
  xor ebx,ebx
  call fastfill
  ret
endp clearscreen

proc LoadPalette     ; EDI: Filename of palette file
                     ; eax first color to read
  push eax
  mov esi,offset palette
  call loadfile
  jc short X
  pop ebx
  mov intebx,ebx
  mov ebx,3
  xor edx,edx
  div ebx
  mov intecx,eax                ; CX = colors total
  mov ax,ds
  mov es,ax
  mov edi,offset intedi
  mov inteax,1012h              ; BIOS function AX=1012h
  mov intedx,0
  mov ax,[8]
  mov intes,ax
  xor cx,cx
  mov ax,300h
  mov bx,10h
  int 31h
  ret
X:
  pop eax
  ret
ENDP LoadPalette

proc FadePalette     ; in ECX number of colors
  mov intecx,ecx                ; CX = colors total
  mov esi,offset palette
  mov es,word ptr cs:[6]
  xor edi,edi
  mov ecx,1024
L1:
  mov eax,[esi]
  mov es:[edi],eax
  lea esi,[esi+4]
  lea edi,[edi+4]
  loop L1
  mov edx,63
L2:
  xor edi,edi
  mov ecx,4096
L3:
  mov al,es:[edi]
  cmp al,0
  je short L4
  dec byte ptr es:[edi]
L4:
  inc edi
  loop L3
  push es
  pusha
  mov ax,ds
  mov es,ax
  mov edi,offset intedi
  mov inteax,1012h              ; BIOS function AX=1012h
  mov intebx,0
  mov intedx,0
  mov ax,[8]
  mov intes,ax
  xor cx,cx
  mov ax,300h
  mov bx,10h
  int 31h
  popa
  pop es
  dec edx
  jnz L2
  ret
ENDP FadePalette

proc FadeInPalette     ; in ECX number of colors
  mov intecx,ecx                ; CX = colors total
  mov esi,offset palette
  mov es,word ptr cs:[6]
  xor edi,edi
  mov ecx,1024
  xor eax,eax
L1:
  mov es:[edi],eax
  lea edi,[edi+4]
  loop L1
  mov edx,63
L2:
  xor edi,edi
  mov esi,offset palette
  mov ecx,4096
L3:
  mov al,es:[edi]
  cmp al,[esi]
  je short L4
  inc byte ptr es:[edi]
L4:
  inc edi
  inc esi
  loop L3
  push es
  pusha
  mov ax,ds
  mov es,ax
  mov edi,offset intedi
  mov inteax,1012h              ; BIOS function AX=1012h
  mov intebx,0
  mov intedx,0
  mov ax,[8]
  mov intes,ax
  xor cx,cx
  mov ax,300h
  mov bx,10h
  int 31h
  popa
  pop es
  dec edx
  jnz L2
  ret
ENDP FadeInPalette

proc ClearPalette     ; in ECX number of colors
  mov intecx,ecx                ; CX = colors total
  mov esi,offset palette
  mov es,word ptr cs:[6]
  xor edi,edi
  mov ecx,1024
  xor eax,eax
L1:
  mov es:[edi],eax
  lea edi,[edi+4]
  loop L1
  mov ax,ds
  mov es,ax
  mov edi,offset intedi
  mov inteax,1012h              ; BIOS function AX=1012h
  mov intebx,0
  mov intedx,0
  mov ax,[8]
  mov intes,ax
  xor cx,cx
  mov ax,300h
  mov bx,10h
  int 31h
  ret
ENDP ClearPalette

PROC WaitForVerticalRetrace
      mov dx,3dah
    @vert1:
      in al,dx
      test al,8
      jz @vert1
    @vert2:
      in al,dx
      test al,8
      jnz @vert2
      ret
ENDP WaitForVerticalRetrace

.MACRO DrawText(X,Y,String,Color)
   .IFS String
       .DATA
          .LOCAL mesg DB String,0
       .CODE
        mov edi,OFFSET .LOCAL mesg
   .ELSE
       .IFR String
           mov edi,String
       .ELSE
           mov edi,offset String
       .ENDIF
   .ENDIF
      mov ecx,x
      mov edx,y
      mov ebx,color
      call OutTextXy
ENDMACRO

.MACRO DrawLine(X1,Y1,X2,Y2,Color)
      mov ecx,x1
      mov edx,y1
      mov esi,x2
      mov edi,y2
      mov ebx,color
      call Line
ENDMACRO

.MACRO DrawRectangle(X1,Y1,X2,Y2,Color)
      mov ecx,x1
      mov edx,y1
      mov esi,x2
      mov edi,y2
      mov ebx,color
      call Rectangle
ENDMACRO

.MACRO DrawFill(X1,Y1,X2,Y2,Color)
      mov ecx,x1
      mov edx,y1
      mov esi,x2
      mov edi,y2
      mov ebx,color
      call fill
ENDMACRO

.MACRO DrawImage(X1,Y1,X2,Y2,offs)
      mov ecx,x1
      mov edx,y1
      mov ebx,x2
      mov edi,y2
      .ifr offs
      mov esi,offs
      .else
      mov esi,offset offs
      .endif
      mov ebx,color
      call putimage
ENDMACRO

.MACRO DrawSprite(X1,Y1,X2,Y2,offs)
      mov ecx,x1
      mov edx,y1
      mov ebx,x2
      mov edi,y2
      mov esi,offs
      call putsprite
ENDMACRO

.MACRO DrawIcon(X1,Y1,offs)
      mov ecx,x1
      mov edx,y1
      .ifr offs
      mov esi,offs
      .else
      mov esi,offset offs
      .endif
      call put16x16
ENDMACRO

PROC GINIT START
  pushad
  mov esi,Offset Vga256
  call InitDLL
  popad
  ret
ENDP GINIT

.include system.inc

