.COMMENT
 ****************************************************************************
 GRAPHDLL.ASM   Example DLL for graphic routines - assemble with /DLL
                (c) 1996,1998 by Dieter Pawelczak - Pass32 Version 2.0
 ============================================================================
 ****************************************************************************
.MODEL DLL

; enable video mode switching for debugging

debugvideo      .equ 1


.INTERFACE

     PROC InitGraph               OFFSET Initgraph
     PROC SetTextMode             OFFSET SetTextmode
     PROC PutPixel                OFFSET PutPixel
     PROC GetPixel                OFFSET GetPixel
     PROC OutChar                 OFFSET OutChar
     PROC OutTextXY               OFFSET OutTextXY
     PROC Circle                  OFFSET Circle
     PROC Line                    OFFSET Line
     PROC Rectangle               OFFSET Rectangle
     PROC FillBlock               OFFSET FillBlock
     PROC WaitForVerticalRetrace  OFFSET WaitForVerticalRetrace
     PROC PutObject               OFFSET PutObject
     PROC PutImage                OFFSET PutImage
     PROC GetImage                OFFSET GetImage
     PROC ColorOuttextXY          OFFSET ColorOuttextXY
     PROC ColorOutchar            OFFSET ColorOutchar
.DATA
.ALIGN
     GSEL     DW  ?              ; graphic selector
     ROMFONT  DD  ?              ; ROM Character address

.INCLUDE DPMI.INC
.CODE


Initgraph PROC FAR
   push ds
   mov ax,cs:[0]
   mov ds,ax

                               ; Copy Colorpallete Into DOS Memory
   mov ax,[6]                  ; Real Mode File Buffer Selector
   mov es,ax
   mov edi,offset colorpalette ; access the colorpalette
   mov ecx,84                  ; number of entries / 4
INIT@PALLOOP:
   mov eax,[edi+ecx*4]         ; make full use of 32 bit register and memory
   mov es:[ecx*4],eax          ; copy to real
   loop INIT@PALLOOP
   mov eax,[edi]               ; copy the first 4 bytes as well
   xor edi,edi
   mov es:[edi],eax

   mov ax,13h
   int 10h                       ; init 320x200x256 Color Mode

   mov ax,ds                     ;SetPalette
   mov es,ax                     ; ES = DS !!!
   mov edi,offset intedi         ; EDI = OFFSET of Data Field
   mov inteax,1012h              ; BIOS function AX=1012h
   mov intebx,0                  ; BX = first palette register
   mov intecx,112                ; CX = 112 colors total
   mov intedx,0                  ; DX = Offset of the palette
   mov ax,[8]                    ; Real Mode Segment To File Buffer
   mov intes,ax                  ; ES = Real Mode Segment of the palette
   mov ax,300h                   ; DPMI Function 0300h: Call Real Mode Int
   xor cx,cx                     ; No parameters on the PM Stack
   mov bx,10h                    ; Interrupt Number, BH must be 0
   int 31h                       ; call DPMI function

   mov ax,ds
   mov es,ax
   mov edi,offset intedi         ; make sure ES:EDI points to our structure
   mov inteax,1130h                 ; Get Offset of BIOS CHAR ROM
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

   mov ax,2                      ; create real mode selector
   mov bx,0a000h                 ; for graphic screen
   int 31h
   mov GSEL,AX
   pop ds

   ret

ENDP InitGraph

.ALIGN
SetTextMode PROC FAR
   mov ax,3
   int 10h
   ret
ENDP SetTextMode

.ALIGN
PROC PutPixel FAR                 ; CX: X DX: Y  BL : Color
  push ds
  mov ax,cs:[0]
  mov ds,ax
  push dx
  push di
  mov ax,gsel
  mov es,ax
  mov ax,320
  mul dx
  add ax,cx
  mov di,ax
  mov es:[di],bl
  pop di
  pop dx
  pop ds
  ret
ENDP Putpixel


.ALIGN
PROC GetPixel FAR                   ; CX: X DX: Y  === > BL : Color
  push ds
  mov ax,cs:[0]
  mov ds,ax
  push dx
  push di
  mov ax,gsel
  mov es,ax
  mov ax,320
  mul dx
  add ax,cx
  mov di,ax
  mov bl,es:[di]
  pop di
  pop dx
  pop ds
  ret
ENDP Getpixel


.ALIGN
PROC outchar  FAR                   ; CX: X DX: Y , BL:Color  AL:Char
.DATA
CharacterTable   db 128,64,32,16,8,4,2,1
Characterloop    db  0
.CODE
  push ds
  mov si,cs:[0]
  mov ds,si
  xor ah,ah
  mov characterloop,ah
  xor si,si
  shl ax,3                      ; ax * 8
  movzx edi,ax
  add edi,romfont
@CharNextLine:
  xor si,si
@CharLine:
  mov al,FS:[edi]               ; fs: Zero Selector
  and al,[CharacterTable+SI]
  cmp al,[CharacterTable+SI]
  jne short  @NoPixel
  call putpixel
  @NoPixel:
  inc cx
  inc si
  cmp si,8
  jne @CharLine
  sub cx,8
  inc dx
  inc Characterloop
  inc edi
  cmp Characterloop,8
  jne @CharNextLine
  pop ds
  ret
ENDP outchar

.ALIGN
PROC OuttextXY  FAR             ; CX: X DX: Y , BL:Color  EDI: Offset String
  mov al,[edi]
  cmp al,0
  je short OuttextXYEND
  push edi
  push dx
  call outchar
  pop dx
  pop edi
  inc edi
  add cx,8
  cmp cx,320
  jb Outtextxy
OuttextXYEND:
  ret
ENDP OuttextXY


.ALIGN
PROC FillBlock FAR                 ; CX:X1 DX:Y1 SI:X2 DI:Y2   BL:Color
.DATA
  FillBlockX1  dw 0
  FillBlockX2  dw 0
  FillblockY2  dw 0
.CODE
  push ds
  mov ax,cs:[0]
  mov ds,ax
  mov FillblockX1,cx
  mov Fillblockx2,si
  mov FillblockY2,di
FillBlock1:
  mov cx,FillBlockX1
FillBlock2:
  call putpixel
  inc cx
  cmp cx,FillblockX2
  jne FillBlock2
  inc dx
  cmp dx,FillblockY2
  jne FillBlock1
  pop ds
  ret
ENDP FillBlock


.ALIGN
PROC Circle   FAR              ; CX:X DX:Y SI:Radius1 DI:Radius2   BL:Color
.DATA
  CircleR1       dw   0
  CircleR2       dw   0
  CircleMX       dw   0
  CircleMY       dw   0
  CircleX        dw   0
  CircleY        dw   0
  CircleStart    re   0
  CircleSolution re   0.01
.CODE
  push ds
  mov ax,cs:[0]
  mov ds,ax
  mov CircleR1,si
  mov CircleR2,di
  mov CircleMX,cx
  mov CircleMY,dx
  fldz
  fstp CircleStart
  fld Circlesolution
  mov cx,628                     ; 2*Pi/0.01
CircleLoop:
  push cx
  fild CircleR2                  ; ST(2)
  fild CircleMY                  ; ST(1)
  fld CircleStart                ; ST(0)
  fcos
  fmul st,st(2)                  ;
  fadd st,st(1)                  ;
  fistp CircleY
  fcompp
  mov dx,CircleY
  fild CircleR1                  ; ST(2)
  fild CircleMX                  ; ST(1)
  fld CircleStart                ; ST(0)
  fsin
  fmul st,st(2)
  fadd st,st(1)
  fistp CircleX
  fcompp
  mov cx,CircleX
  fld CircleStart
  fadd st,st(1)                  ; + CircleSolution
  fstp CircleStart
  call putpixel
  pop cx
  loop CircleLoop
  fcompp
  pop ds
  ret
  ENDP Circle


.ALIGN
PROC Line  FAR             ;CX:X1, DX:Y1, SI:X2, DI:Y2   Bl:color;
; Bresenham-Algorithmus

.DATA
  Linedx            dw   0
  Linedy            dw   0
  LinedAB           dw   0
  LineIncA          dw   0
  LineIncB          dw   0
  LineX             dw   0
  LineY             dw   0
  Lineh1            dw   0
  Lineh2            dw   0
  LineX1            dw   0
  LineX2            dw   0
  LineY1            dw   0
  LineY2            dw   0
.CODE
  cmp cx,si
  je Linedot1
  jmp short DrawLine
Linedot1:
  cmp dx,di
  jne short DrawLine
  call putpixel
  ret
DrawLine:
  push ds
  mov ax,cs:[0]
  mov ds,ax
  mov LineX1,cx
  mov LineX2,si
  mov LineY1,dx
  mov LineY2,di
  cmp cx,si              ;             IF x1 > x2 THEN
  jbe short DrawLineArg1
  mov LineX1,si
  mov LineX2,cx          ;  x1 <> x2
  mov LineY1,di
  mov LineY2,dx          ;  y1 <> y2
DrawLineArg1:
  mov ax,LineX2
  sub ax,LineX1
  mov LineDx,ax         ;      dx := x2-x1;
  mov LineH1,ax;        ;      h1:=dx
  mov ax,LineY2
  sub ax,LineY1
  mov LineDy,ax         ;      dy := y2-y1;
  mov Lineh2,ax         ;      h2 := dy;
  cmp ax,0
  je short DrawLineNext1
  jns DrawLineNext1     ;      IF (dx < -dy) AND (dy < 0)
  neg ax
  cmp Linedx,ax
  jae short  DrawLineNext1     ;      THEN
  neg LineY1             ;    y1:=-y1;
  neg LineY2             ;    y2:=-y2;
  mov ax,LineX1
  mov cx,LineY1
  mov LineX1,cx         ;     x1 <> y1;
  mov LineY1,ax
  mov ax,LineX2
  mov cx,LineY2
  mov LineX2,cx         ;     x2 <> y2;
  mov LineY2,ax
DrawLineNext1:
  mov ax,LineDY
  cmp ax,0
  je short DrawLineNext2
  jns short DrawLineNext2     ;     IF (dx >= -dy) AND (dy < 0)
  neg ax
  cmp LineDx,ax
  jb short DrawLineNext2     ;     THEN
  neg LineY1             ;    y1:=-y1;
  neg LineY2             ;    y2:=-y2;
DrawLineNext2:
  mov ax,LineDy
  cmp ax,0
  je short DrawLineNext3
  js short DrawLineNext3      ;      IF (dx <= dy) AND (dy > 0)
  cmp LineDx,ax
  ja short DrawLineNext3      ;      THEN
  mov ax,LineX1
  mov cx,LineY1
  mov LineX1,cx
  Mov LineY1,ax         ;       x1 <> y1
  mov ax,LineX2
  mov cx,LineY2
  mov LineX2,cx
  Mov LineY2,ax         ;       x2 <> y2
DrawLineNext3:
  mov ax,LineX2
  sub ax,LineX1
  mov LineDx,ax         ;      dx := x2-x1;
  mov ax,LineY2
  sub ax,LineY1
  mov LineDy,ax         ;      dy := y2-y1;
  shl ax,1
  mov LineIncB,ax       ;      IncB := 2*dy;
  sub ax,Linedx
  mov LinedAB,ax        ;      dAB := 2*dy-dx;
  mov ax,LineDy
  sub ax,LineDx
  shl ax,1
  mov LineIncA,ax      ;       IncA := 2*(dy-dx);
  mov ax,LineX1
  mov LineX,ax          ;      x := x1;
  mov ax,LineY1
  mov LineY,ax          ;      y := y1;
  mov ax,LineH2
  cmp ax,0
  je short DrawPixel1
  jns short DrawPixel1        ;     IF (h1 < -h2) AND (h2 < 0)
  neg ax
  cmp LineH1,Ax
  jae short DrawPixel1        ;   THEN
  mov cx,LINEY
  mov dx,LINEX
  neg dx                ;        Putpixel(y, -x, bl);
  call putpixel
  jmp Drawpixel4
DrawPixel1:
  mov ax, LineH2
  cmp ax,0
  je short DrawPixel2
  jns short  DrawPixel2         ;    IF (h1 >= -h2) AND (h2 < 0)
  neg ax
  cmp LineH1,ax
  jb short DrawPixel2         ;  THEN
  mov CX,LineX
  mov dx,LineY
  neg dx
  call putpixel         ;        putpixel (x, -y, bl);
  jmp short Drawpixel4
DrawPixel2:
  mov ax,LineH2
  cmp ax,0
  js short  Drawpixel3        ; IF (h1 > h2) AND (h2 >= 0)
  cmp LineH1,ax
  jbe DrawPixel3         ; THEN
  mov cx,LineX
  mov dx,LineY          ;
  call putpixel         ;        putpixel (x, y, bl);
  jmp short Drawpixel4
Drawpixel3:
  mov ax,LineH2
  cmp ax,0
  js short DrawPixel4        ;  IF (h1 <= h2) AND (h2 >= 0)
  cmp LineH1,ax
  ja short DrawPixel4         ; THEN
  mov cx,LineY
  mov dx,LineX
  call putpixel
DrawPixel4:
  inc LineX             ;       FOR x := x1+1 TO x2 DO
  mov ax,LinedaB
  cmp ax,0              ; IF dAB < 0 THEN
  je short DABAboveZero
  jns short DABAboveZero
  mov ax,LinedAB        ; Inc(dAB, IncB)
  add ax,LineIncB
  mov LinedAB,AX
  jmp short  DABENDIF
DABAboveZero:
  mov ax,LinedAB        ; Inc(dAB, IncA)
  add ax,LineIncA
  mov LinedAB,AX
  inc LineY
DABENDIF:
  mov ax,LineH2
  cmp ax,0
  je short @DrawPixel1
  jns short @DrawPixel1        ;     IF (h1 < -h2) AND (h2 < 0)
  neg ax
  cmp LineH1,Ax
  jae short @DrawPixel1        ;   THEN
  mov cx,LINEY
  mov dx,LINEX
  neg dx                ;        Putpixel(y, -x, bl);
  call putpixel
  jmp @Drawpixel4
@DrawPixel1:
  mov ax, LineH2
  cmp ax,0
  je short @DrawPixel2
  jns short @DrawPixel2         ;    IF (h1 >= -h2) AND (h2 < 0)
  neg ax
  cmp LineH1,ax
  jb short @DrawPixel2         ;  THEN
  mov CX,LineX
  mov dx,LineY
  neg dx
  call putpixel         ;        putpixel (x, -y, bl);
  jmp short @Drawpixel4
@DrawPixel2:
  mov ax,LineH2
  cmp ax,0
  js short @Drawpixel3        ; IF (h1 > h2) AND (h2 >= 0)
  cmp LineH1,ax
  jbe short @DrawPixel3         ; THEN
  mov cx,LineX
  mov dx,LineY          ;
  call putpixel         ;        putpixel (x, y, bl);
  jmp short @Drawpixel4
@Drawpixel3:
  mov ax,LineH2
  cmp ax,0
  js short @DrawPixel4        ;  IF (h1 <= h2) AND (h2 > 0)
  cmp LineH1,ax
  ja short @DrawPixel4         ; THEN
  mov cx,LineY
  mov dx,LineX
  call putpixel         ;        putpixel (y, x, bl);
@DrawPixel4:
  mov ax,LineX
  cmp ax,LineX2
  jb  DrawPixel4
  pop ds
  ret
ENDP Line

.ALIGN
PROC Rectangle FAR         ;CX:X1, DX:Y1, SI:X2, DI:Y2   Bl:color;
.DATA
  RectX1        dw      0
  RectX2        dw      0
  RectY1        dw      0
  RectY2        dw      0
 .CODE
  push ds
  mov ax,cs:[0]
  mov ds,ax
  mov rectx1,cx
  mov rectx2,si
  mov recty1,dx
  mov recty2,di
  mov di,dx
  call line
  mov cx,rectx1
  mov si,rectx2
  mov dx,recty2
  mov di,dx
  call line
  mov cx,rectx1
  mov si,cx
  mov dx,recty1
  mov di,recty2
  call line
  mov cx,rectx2
  mov si,cx
  mov dx,recty1
  mov di,recty2
  call line
  pop ds
  ret
 ENDP Rectangle

.ALIGN
PROC WaitForVerticalRetrace FAR
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

.ALIGN
PROC PutObject FAR                       ; Displays a sprite object
                                        ; EDI : Offset to OBJect
                                        ; CX : X  ; SI : Size X
                                        ; DX : Y  ; BX : Size Y
.DATA
  ObjectSizeX   DW ?
  ObjectSizeY   DW ?
.CODE
  push gs
  mov ax,cs:[0]
  mov gs,ax
  mov gs:ObjectSizeX,Si
  mov gs:ObjectsizeY,BX
  mov ax,gs:gsel
  mov es,ax
  mov ax,320
  mul dx
  add ax,cx
  mov si,ax                             ; Offset in Graphic Area
  mov CX,gs:ObjectSizeY
  OBJLineLoop:
  mov DX,gs:ObjectSizeX
OBJColumnLoop:
  mov al,[edi]
  cmp al,0
  je @NoPut
  mov es:[si],al
@NoPut:
  inc si
  inc edi
  dec dx
  jnz OBJColumnLoop
  add si,320
  sub si,gs:ObjectSizeX
  dec cx
  jnz OBJLineLoop
  pop gs
  ret
ENDP PutObject

.DATA                                   ; Data for get- and putimage
  ImageSizeX   DW ?
  ImageSizeY   DW ?
.CODE

.ALIGN
PROC PutImage FAR                          ; Displays an image
                                        ; EDI : Offset to Image
                                        ; CX : X  ; SI : Size X
                                        ; DX : Y  ; BX : Size Y
  push gs
  mov ax,cs:[0]
  mov gs,ax
  mov gs:ImageSizeX,Si
  mov gs:ImageSizeY,BX
  mov ax,gs:gsel
  mov es,ax
  mov ax,320
  mul dx
  add ax,cx
  mov si,ax                             ; Offset in Graphic Area
  mov CX,gs:ImageSizeY
  IMGLineLoop:
  mov DX,gs:ImageSizeX
IMGColumnLoop:
  mov al,[edi]
  mov es:[si],al
  inc si
  inc edi
  dec dx
  jnz IMGColumnLoop
  add si,320
  sub si,gs:ImageSizeX
  dec cx
  jnz IMGLineLoop
  pop gs
  ret
ENDP PutImage

.ALIGN
PROC GetImage  FAR                      ; Gets an image from Screen
                                        ; EDI : Offset to Image Buffer
                                        ; CX : X  ; SI : Size X
                                        ; DX : Y  ; BX : Size Y
  push gs
  mov ax,cs:[0]
  mov gs,ax
  mov gs:ImageSizeX,Si
  mov gs:ImageSizeY,BX
  mov ax,gs:gsel
  mov es,ax
  mov ax,320
  mul dx
  add ax,cx
  mov si,ax                             ; Offset in Graphic Area
  mov CX,gs:ImageSizeY
  IMGLineLoop2:
  mov DX,gs:ImageSizeX
IMGColumnLoop2:
  mov al,es:[si]
  mov [edi],al
  inc si
  inc edi
  dec dx
  jnz IMGColumnLoop2
  add si,320
  sub si,gs:ImageSizeX
  dec cx
  jnz IMGLineLoop2
  pop gs
  ret
ENDP GetImage

.ALIGN
PROC ColorOutchar FAR                   ; CX: X DX: Y , BL:Color  AL:Char
.DATA
CharacterTable2   db 128,64,32,16,8,4,2,1
Characterloop2    db  0
.CODE
  push ds
  mov si,cs:[0]
  mov ds,si
  xor ah,ah
  mov characterloop2,ah
  xor si,si
  shl ax,3                      ; ax * 8
  movzx edi,ax
  add edi,romfont
@CharNextLine2:
  xor si,si
@CharLine2:
  mov al,FS:[edi]               ; fs: Zero Selector
  and al,[CharacterTable2+SI]
  cmp al,[CharacterTable2+SI]
  jne short  @NoPixel2
  call putpixel
  @NoPixel2:
  inc cx
  inc si
  cmp si,8
  jne @CharLine2
  sub cx,8
  inc dx
  inc Characterloop2
  inc edi
  inc bx
  cmp Characterloop2,8
  jne @CharNextLine2
  pop ds
  ret
ENDP coloroutchar

.ALIGN
PROC ColorOuttextXY FAR         ; CX: X DX: Y , BL:Color  EDI: Offset String
  mov al,[edi]
  cmp al,0
  je short ColorOuttextXYEND
  push edi
  push dx
  push bx
  call coloroutchar
  pop bx
  pop dx
  pop edi
  inc edi
  add cx,8
  cmp cx,320-7
  jb ColorOuttextxy
ColorOuttextXYEND:
  ret
ENDP ColorOuttextXY

.PUBLIC colorpalette:
.loadbin graph.pal             ; load VGA Palette into program file

END
