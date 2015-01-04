.COMMENT
 ****************************************************************************
 VBE3.INC       Vesa Graphic Driver for 1024x768 mode with linear frame buffer
                needs VESA V2.0!
 Attention:     The Graphic DLLs assume GS = Zero Selector!
                You must preserve this Selector!
 ****************************************************************************
.MODEL DLL
.INTERFACE

; do not alter ==============================================================

 PROC PutPixelDirect  OFFSET PutPixelDirect   ; put pixel on screen
 PROC PutPixelBuffer  OFFSET PutPixelBuffer   ; put pixel on buffer
 PROC GetPixelDirect  OFFSET GetPixelDirect
 PROC GetPixelBuffer  OFFSET GetPixelBuffer
 PROC InitvideoMode   OFFSET InitVideomode    ; video mode init
 PROC GetMaxX         OFFSET GetMaxX
 PROC GetMaxY         OFFSET GetMaxY
 PROC GetPageCount    OFFSET GetBufferCount   ; number of buffers available
 PROC PutImageDirect  OFFSET PutImageDirect
 PROC PutImageBuffer  OFFSET PutImageBuffer
 PROC GetImageDirect  OFFSET GetImageDirect
 PROC GetImageBuffer  OFFSET GetImageBuffer
 PROC PutSpriteDirect OFFSET PutSpriteDirect
 PROC PutSpriteBuffer OFFSET PutSpriteBuffer
 PROC FillDirect      OFFSET FillDirect
 PROC FillBuffer      OFFSET FillBuffer
 PROC FastFillDirect  OFFSET FastFillDirect   ; fills an dword aligned area
 PROC FastFillBuffer  OFFSET FastFillBuffer   ; fills an dword aligned area
 PROC Put16x16Direct  OFFSET Put16x16Direct
 PROC Put16x16Buffer  OFFSET Put16x16Buffer
 PROC SetBuffer       OFFSET SetBuffer        ; choose buffer 1..
 PROC FlushBuffer     OFFSET FlushSBuffer     ; flush whole buffer to Screen
 PROC FlushWindow     OFFSET FlushWindow      ; flush only parts of the buffer
 PROC GetBufferAddr   OFFSET GetBufferAddr    ; returns linear basis address
 PROC FreeDriver      OFFSET FreeDriver       ; frees memory and descriptors

; do not alter ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.SMART

.DATA?
   NumberOfBuffers .EQU 1
   SizeX           .EQU 1024
   SizeY           .EQU 768

ScreenBuffer    db    SizeX*SizeY dup (?)
.DATA
BufferAddr      dd              ?
FrameBufferBase dd              ?

;Real Mode Calling "Structure"

.include dpmi.inc

.CODE
.FAR

;Code from P„r Spjut`s PROTON ------------------------------------------------

PROC InitVideoMode
  push ds
  push fs
  mov ds,word ptr cs:[0]
  mov fs,word ptr cs:[6]
  mov eax,0
  mov fs:[40],eax
  mov edi,offset Screenbuffer
  mov ecx,SizeX*SizeY/4-1
L1:
  mov [edi+ecx*4],eax
  loopd L1
  mov [edi],eax
  Mov   InteAX,4F01h
  Mov   InteCX,105h
  Mov   AX,CS:[8]
  Mov   IntES,AX
  Mov   InteDI,0
  Mov   AX,DS
  Mov   ES,AX
  Mov   AX,300h
  Mov   BX,10h
  Mov   CX,0
  Mov   eDI,Offset InteDI
  Int   31h
  Mov   cX,FS:[40]
  Mov   BX,FS:[42]
  cmp dword ptr fs:[40],0
  je XX
  test byte ptr fs:[0],128
  je XX
  mov   SI, SIZEX*SIZEY/65536
  mov   DI, SIZEX*SIZEY%65536
  mov   ax,0800h  ; get linear address
  int 31h
  mov   word ptr FrameBufferBase,CX
  mov   word ptr FrameBufferBase+2,BX
  Mov   AX,4F02h
  Mov   BX,4105h
  Int   10h
  push eax
  mov bx,cs
  mov ax,06h
  int 31h
  mov  word ptr Bufferaddr,dx
  mov  word ptr Bufferaddr+2,cx
  add Bufferaddr, OFFSET Screenbuffer
  mov eax,BufferAddr
  mov ds:BufferAddr2,eax
  mov ds:BufferAddr1,eax
  mov ds:BufferAddr3,eax
  mov ds:BufferAddr4,eax
  mov ds:BufferAddr5,eax
  mov ds:BufferAddr6,eax
  mov ds:BufferAddr7,eax
  mov ds:BufferAddr8,eax
  mov ds:BufferAddr10,eax
  mov ds:BufferAddr11,eax
  mov eax,FrameBufferBase
  mov ds:FrameBufferAddr2,eax
  mov ds:FrameBufferAddr1,eax
  mov ds:FrameBufferAddr3,eax
  mov ds:FrameBufferAddr4,eax
  mov ds:FrameBufferAddr5,eax
  mov ds:FrameBufferAddr6,eax
  mov ds:FrameBufferAddr7,eax
  mov ds:FrameBufferAddr8,eax
  mov ds:FrameBufferAddr10,eax
  mov ds:FrameBufferAddr11,eax
  pop eax
XX:
  pop fs
  pop ds
  cmp al,4fh
  jne short X
  cmp ah,0
  jne short X
  clc
  ret
X:
  stc
  ret
ENDP InitVideoMode

;----------------------------------------------------------------------------


PROC PutPixelBuffer
  mov eax,edx
  shl eax,10
.CONST
  db 65h,88h,9ch,08h
  BufferAddr1 DD ?
.CODE
  ret
ENDP PutPixelBuffer



PROC GetPixelBuffer
  mov eax,edx
  shl eax,10
.CONST
  db 65h,8ah,9ch,08h
  BufferAddr2 DD ?
.CODE
  ret
ENDP GetPixelBuffer

PROC PutPixelDirect
  mov eax,edx
  shl eax,10
.CONST
  db 65h,88h,9ch,08h
  FrameBufferAddr1 DD ?
.CODE

  ret
ENDP PutPixelDirect

PROC GetPixelDirect
  mov eax,edx
  shl eax,10
.CONST
  db 65h,8ah,9ch,08h
  FrameBufferAddr2 DD ?
.CODE
  ret
ENDP GetPixelDirect

PROC GetMaxX
  mov eax,SizeX
  ret
ENDP GetMaxX

PROC GetMaxY
  mov eax,SizeY
  ret
ENDP GetMaxY

PROC GetBufferCount
  mov eax,NumberOfBuffers
  ret
ENDP GetBufferCount

PROC PutImageDirect     ; EBX = SIZE X, EDI = SIZE Y, ESI = OFFSET TO BUFFER
  push es
  push ecx
  push edx
  mov eax,edx
  mov es,word ptr cs:[4]
  add ebx,3
  shl eax,10
  shr ebx,2
.CONST
  db 8dh,84h,08h
FrameBufferAddr3 dd ?
.CODE
  cld
  mov edx,edi
Loop:
  mov ecx,ebx
  mov edi,eax
  rep movsd
  lea eax,[eax+SIZEX]
  dec edx
  cmp edx,0
  ja Loop
  pop edx
  pop ecx
  pop es
  ret
ENDP PutImageDirect

PROC GetImageDirect     ; EBX = SIZE X, EDI = SIZE Y, ESI = OFFSET TO BUFFER
  push es
  push ds
  push ecx
  push edx
  mov ax,ds
  mov es,ax
  mov eax,edx
  mov ds,word ptr cs:[4]
  add ebx,3
  shl eax,10
  shr ebx,2
.CONST
  db 8dh,84h,08h
FrameBufferAddr4 dd ?
.CODE
  cld
  mov edx,edi
  mov edi,esi
Loop:
  mov ecx,ebx
  mov esi,eax
  rep movsd
  lea eax,[eax+SIZEX]
  dec edx
  cmp edx,0
  ja Loop
  pop edx
  pop ecx
  pop ds
  pop es
  ret
  ret
ENDP GetImageDirect

PROC PutImageBuffer
  push es
  push ecx
  push edx
  mov eax,edx   ; always assume DWORD Alignement
  mov es,word ptr cs:[4]
  add ebx,3
  shl eax,10
  shr ebx,2
.CONST
  db 8dh,84h,08h
BufferAddr3 dd ?
.CODE
  cld
  mov edx,edi
Loop:
  mov ecx,ebx
  mov edi,eax
  rep movsd
  lea eax,[eax+SIZEX]
  dec edx
  cmp edx,0
  ja Loop
  pop edx
  pop ecx
  pop es
  ret
ENDP PutImageBuffer

PROC GetImageBuffer     ; EBX = SIZE X, EDI = SIZE Y, ESI = OFFSET TO BUFFER
  push es
  push ds
  push ecx
  push edx
  mov ax,ds
  mov es,ax
  mov ds,word ptr cs:[4]
  mov eax,edx
  add ebx,3
  shr ebx,2
  shl eax,10
.CONST
  db 8dh,84h,08h
BufferAddr4 dd ?
.CODE
  cld
  mov edx,edi
  mov edi,esi
Loop:
  mov ecx,ebx
  mov esi,eax
  rep movsd
  lea eax,[eax+SIZEX]
  dec edx
  cmp edx,0
  ja Loop
  pop edx
  pop ecx
  pop ds
  pop es
  ret
ENDP GetImageBuffer

PROC PutSpriteDirect
  mov eax,edx
  push ecx
  shl eax,10
  push edx
.CONST
  db 8dh,84h,08h
FrameBufferAddr7 dd ?
.CODE
  mov edx,edi
  push ebx
Loop:
  mov ecx,[esp]
  mov edi,eax
loop2:
  mov bl,[esi]
  cmp bl,0
  je short next
  mov gs:[edi],bl
next:
  inc edi
  inc esi
  dec ecx
  jne loop2
  lea eax,[eax+SIZEX]
  dec edx
  jne Loop
  pop ebx
  pop edx
  pop ecx
  ret
ENDP PutSpriteDirect

PROC PutSpriteBuffer
  mov eax,edx
  push ecx
  shl eax,10
  push edx
.CONST
  db 8dh,84h,08h
BufferAddr7 dd ?
.CODE
  mov edx,edi
  push ebx
Loop:
  mov ecx,[esp]
  mov edi,eax
loop2:
  mov bl,[esi]
  cmp bl,0
  je short next
  mov gs:[edi],bl
next:
  inc edi
  inc esi
  dec ecx
  jne loop2
  lea eax,[eax+SIZEX]
  dec edx
  jne Loop
  pop ebx
  pop edx
  pop ecx
  ret
ENDP PutSpriteBuffer

PROC Put16x16Direct
  mov eax,edx
  shl eax,10
.CONST
  db 8dh,84h,08h
FrameBufferAddr6 dd ?
.CODE
  mov edi,10h
loop:
  mov ebx,[esi]
  mov gs:[eax],ebx
  mov ebx,[esi+4]
  mov gs:[eax+4],ebx
  mov ebx,[esi+8]
  mov gs:[eax+8],ebx
  mov ebx,[esi+12]
  mov gs:[eax+12],ebx
  lea eax,[eax+SIZEX]
  lea esi,[esi+16]
  dec edi
  jnz loop
  ret
ENDP Put16x16Direct

PROC Put16x16Buffer
  mov eax,edx
  shl eax,10
.CONST
  db 8dh,84h,08h
BufferAddr6 dd ?
.CODE
  mov edi,10h
loop:
  mov ebx,[esi]
  mov gs:[eax],ebx
  mov ebx,[esi+4]
  mov gs:[eax+4],ebx
  mov ebx,[esi+8]
  mov gs:[eax+8],ebx
  mov ebx,[esi+12]
  mov gs:[eax+12],ebx
  lea eax,[eax+SIZEX]
  lea esi,[esi+16]
  dec edi
  jnz loop
  ret
ENDP Put16x16Buffer

PROC SetBuffer
  movzx eax,al
  cmp al,NumberOfBuffers
  jbe short L1
  stc
  ret
L1:
  clc
  ret
ENDP SetBuffer

PROC FillDirect
  push es
  push ecx
  push edx
  mov al,bl
  mov es,word ptr cs:[4]
  mov ebx,edx
  sub esi,ecx
  sub edi,edx
  shl ebx,10
  mov edx,edi
.CONST
  db 8dh,9ch,0bh
FrameBufferAddr8 dd ?
.CODE
  cld
Loop:
  mov ecx,esi
  mov edi,ebx
  rep stosb
  lea ebx,[ebx+SIZEX]
  dec edx
  jns Loop
  pop edx
  pop ecx
  pop es
  ret
ENDP FillDirect

PROC FillBuffer
  push es
  push ecx
  push edx
  mov al,bl
  mov es,word ptr cs:[4]
  mov ebx,edx
  sub esi,ecx
  sub edi,edx
  shl ebx,10
  mov edx,edi
.CONST
  db 8dh,9ch,0bh
BufferAddr8 dd ?
.CODE
  cld
Loop:
  mov ecx,esi
  mov edi,ebx
  rep stosb
  lea ebx,[ebx+SIZEX]
  dec edx
  jns Loop
  pop edx
  pop ecx
  pop es
  ret
ENDP FillBuffer

PROC FastFillDirect
  mov bh,bl
  push es
  mov eax,ebx
  push ecx
  shl eax,16
  push edx
  mov ax,bx
  mov es,word ptr cs:[4]
  sub esi,ecx
  mov ebx,edx
  add esi,3
  sub edi,edx
  shl ebx,10
  mov edx,edi
  shr esi,2
.CONST
  db 8dh,9ch,0bh
FrameBufferAddr5 dd ?
.CODE
  cld
Loop:
  mov ecx,esi
  mov edi,ebx
  rep stosd
  lea ebx,[ebx+SIZEX]
  dec edx
  jns Loop
  pop edx
  pop ecx
  pop es
  ret
ENDP FastFillDirect

PROC FastFillBuffer
  mov bh,bl
  push es
  mov eax,ebx
  push ecx
  shl eax,16
  push edx
  mov ax,bx
  mov es,word ptr cs:[4]
  sub esi,ecx
  mov ebx,edx
  add esi,3
  sub edi,edx
  shl ebx,10
  mov edx,edi
  shr esi,2
.CONST
  db 8dh,9ch,0bh
BufferAddr5 dd ?
.CODE
  cld
Loop:
  mov ecx,esi
  mov edi,ebx
  rep stosd
  lea ebx,[ebx+SIZEX]
  dec edx
  jns Loop
  pop edx
  pop ecx
  pop es
  ret
ENDP FastFillBuffer


PROC FlushWindow
  mov eax,edx
  shl edi,10
  shl eax,10
  mov ebp, ecx
L1:
  mov ecx, ebp
L2:
.CONST
  db 65h,8bh,9Ch,08H
BufferAddr10 dd ?         ; mov ebx,[eax+ecx+Buffer]
  db 65h,89h,9Ch,08H
FrameBufferAddr10 dd ?    ; mov [eax+ecxFrameBuffer,ebx]
.CODE
  lea ecx,[ecx+4]
  cmp ecx,esi
  jb L2
  lea eax,[eax+SizeX]
  cmp eax,edi
  jbe L1
  ret
ENDP FlushWindow      ; flush only parts of the buffer

PROC GetBufferAddr
  push ds
  mov ds,word ptr cs:[0]
  mov eax,bufferaddr
  pop ds
  ret
ENDP GetBufferAddr    ; returns linear basis address


PROC FreeDriver
  ret
ENDP FreeDriver


FlushSBuffer proc
  push ds
  push es
  mov ax,gs
  mov ds,ax
  mov es,ax
  mov ecx,SIZEX*SIZEY/4
.CONST
  DB 0BEh
  BufferAddr11 DD ?; MOV ESI,BUFFERADDR
  DB 0BFh
  FrameBufferAddr11 DD ?; MOV EDI,FrameBUFFERADDR
.CODE
  rep movsd			;flush screen buffer
  pop es
  pop ds
  ret
ENDP FlushSBuffer

