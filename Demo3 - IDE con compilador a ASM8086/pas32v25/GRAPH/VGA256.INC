.COMMENT
 ****************************************************************************
 VGA256.INC     Basic Graphic Driver for 320x200 mode
                Included by GRAPHIC.INC
                Note, this is the default interface for any graphics
                Driver supported by GRAPHIC.INC
                Don't alter the interface - copy this file and then
                alter the code!
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
 PROC FlushBuffer     OFFSET FlushBuffer      ; flush whole buffer to Screen
 PROC FlushWindow     OFFSET FlushWindow      ; flush only parts of the buffer
 PROC GetBufferAddr   OFFSET GetBufferAddr    ; returns linear basis address
 PROC FreeDriver      OFFSET FreeDriver       ; frees memory and descriptors

; do not alter ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.SMART

.DATA?
   NumberOfBuffers .EQU 2
   SizeX           .EQU 320
   SizeY           .EQU 200

   BufferOffset    DD ?
   BufferAddr      DD ?
   ScreenBuffer    DB SizeX*SizeY*NumberOfBuffers DUP(?)
   Reserve         DB 16*SizeX*DUP(?)

.CODE

PROC InitvideoMode
  push ds
  mov ax,13h
  int 10h
.const
  db 66h;
.code
  mov ds,word ptr cs:[0]
  xor eax,eax
  mov esi,offset ScreenBuffer
  mov ecx,NumberOfBuffers*SizeX*SizeY/4-1
L1:
  mov [esi+4*ecx],eax
  loop L1
  mov [esi],eax
  mov bx,cs
  mov ax,06h
  int 31h
  mov  word ptr Bufferaddr,dx
  mov  word ptr Bufferaddr+2,cx
  add Bufferaddr, OFFSET Screenbuffer
  mov eax,BufferAddr
  mov ds:BufferAddr1,eax
  mov ds:BufferAddr2,eax
  mov ds:BufferAddr3,eax
  mov ds:BufferAddr4,eax
  mov ds:BufferAddr5,eax
  mov ds:BufferAddr6,eax
  mov ds:BufferAddr7,eax
  mov BufferOffset, OFFSET Screenbuffer
  pop ds
  ret
ENDP InitvideoMode

.COMMENT *** PutPixel/GetPixel reserve all registers but not eax, color in bl ***


PROC PutPixelDirect             ; ECX: X EDX: Y  BL : Color
  lea eax,[edx*4+edx]
  shl eax,6
  mov gs:[eax+ecx+0A0000h],bl
  ret
ENDP PutpixelDirect

PROC GetPixelDirect             ; ECX: X EDX: Y  BL : Color
  lea eax,[edx*4+edx]
  shl eax,6
  mov bl,gs:[eax+ecx+0A0000h]
  ret
ENDP GetpixelDirect

PROC PutPixelBuffer             ; ECX: X EDX: Y  BL : Color
  lea eax,[edx*4+edx]
  shl eax,6
.CONST
  db 65h,88h,9ch,08h
BufferAddr1  dd ?
.CODE
  ret
ENDP PutpixelBuffer

PROC GetPixelBuffer             ; ECX: X EDX: Y  BL : Color
  lea eax,[edx*4+edx]
  shl eax,6
.CONST
  db 65h,8Ah,9ch,08h
BufferAddr2  dd ?
.CODE
  ret
ENDP GetpixelBuffer


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
  mov es,word ptr cs:[4]
  lea eax,[edx*4+edx]   ; always assume DWORD Alignement
  add ebx,3
  shl eax,6
  shr ebx,2
  lea eax,[eax+ecx+0a0000h]
  cld
  mov edx,edi
Loop:
  mov ecx,ebx
  mov edi,eax
  rep movsd
  lea eax,[eax+320]
  dec edx
  cmp edx,0
  ja Loop
  pop edx
  pop ecx
  pop es
  ret
ENDP PutImageDirect

PROC GetImageDirect     ; EBX = SIZE X, EDI = SIZE Y, ESI = OFFSET TO BUFFER
.CODE
  push es
  push ds
  push ecx
  push edx
  mov ax,ds
  mov es,ax
  mov ds,word ptr cs:[4]
  lea eax,[edx*4+edx]   ; always assume DWORD Alignement
  add ebx,3
  shl eax,6
  shr ebx,2
  lea eax,[eax+ecx+0a0000h]
  cld
  mov edx,edi
  mov edi,esi
Loop:
  mov ecx,ebx
  mov esi,eax
  rep movsd
  lea eax,[eax+320]
  dec edx
  cmp edx,0
  ja Loop
  pop edx
  pop ecx
  pop ds
  pop es
  ret
ENDP GetImageDirect

PROC PutImageBuffer
  push es
  push ecx
  push edx
  mov es,word ptr cs:[4]
  lea eax,[edx*4+edx]   ; always assume DWORD Alignement
  add ebx,3
  shl eax,6
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
  lea eax,[eax+320]
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
  lea eax,[edx*4+edx]   ; always assume DWORD Alignement
  add ebx,3
  shl eax,6
  shr ebx,2
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
  lea eax,[eax+320]
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
  push ecx
  push edx
  lea eax,[edx*4+edx]   ; always assume DWORD Alignement
  shl eax,6
  lea eax,[eax+ecx+0a0000h]
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
  lea eax,[eax+320]
  dec edx
  jne Loop
  pop ebx
  pop edx
  pop ecx
  ret
ENDP PutSpriteDirect

PROC PutSpriteBuffer
  push ecx
  push edx
  lea eax,[edx*4+edx]   ; always assume DWORD Alignement
  shl eax,6
.CONST
  db 8dh,84h,08h
BufferAddr5 dd ?
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
  lea eax,[eax+320]
  dec edx
  jne Loop
  pop ebx
  pop edx
  pop ecx
  ret
ENDP PutSpriteBuffer

PROC Put16x16Direct
  lea eax,[edx*4+edx]
  shl eax,6
  lea eax,[eax+ecx+0A0000h]
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
  lea eax,[eax+320]
  lea esi,[esi+16]
  dec edi
  jnz loop
  ret
ENDP Put16x16Direct

PROC Put16x16Buffer
  lea eax,[edx*4+edx]
  shl eax,6
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
  lea eax,[eax+320]
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
  dec eax
  push ds
  mov ebx,SizeX*SizeY
  mov ds,word ptr cs:[0]
  mul ebx
  mov BufferOffset,eax
  add BufferOffset, OFFSET Screenbuffer
  add eax,BufferAddr
  mov ds:BufferAddr2,eax
  mov ds:BufferAddr1,eax
  mov ds:BufferAddr3,eax
  mov ds:BufferAddr4,eax
  mov ds:BufferAddr5,eax
  mov ds:BufferAddr6,eax
  mov ds:BufferAddr7,eax
  pop ds
  ret
ENDP SetBuffer

PROC FillDirect
  push ds
  mov ds,word ptr cs:[0]
  sub edi,edx         ; Size Y
  sub esi,ecx         ; Size X
  lea eax,[edx+4*edx]
  shl eax,6
  push esi
L1:
  xor edx,edx
  lea esi,[ecx+eax+0a0000h]
L2:
  mov gs:[esi+edx],bl
  inc edx
  cmp edx,[ESP]
  jbe L2
  lea eax,[eax+320]
  dec edi
  jns L1
  pop ebx
  pop ds
  ret

ENDP FillDirect

PROC FillBuffer
  push es
  push ecx
  push edx
  mov al,bl
  mov es,word ptr cs:[4]
  sub esi,ecx
  lea ebx,[edx*4+edx]   ; always assume DWORD Alignement
  sub edi,edx
  shl ebx,6
  mov edx,edi
.CONST
  db 8dh,9ch,0bh
BufferAddr7 dd ?
.CODE
  cld
Loop:
  mov ecx,esi
  mov edi,ebx
  rep stosb
  lea ebx,[ebx+320]
  dec edx
  jns Loop
  pop edx
  pop ecx
  pop es
  ret
ENDP FillBuffer

PROC FlushBuffer
  push ds
  push es
  mov ax,gs
  mov ds,word ptr cs:[0]
  mov edi,0a0000h
  mov es,ax
  mov esi,BufferOffset
  mov ecx,SizeX*SizeY/4
  cld
  rep movsd
  pop es
  pop ds
  ret
ENDP FlushBuffer      ; flush whole buffer to Screen

PROC FlushWindow      ; ECX, EDX, ESI, EDI: Window Positions
  mov ebx,edi
  lea eax,[edx+4*edx]
  push ds
  sub ebx,edx         ; Size Y
  push ebx
  shl eax,6
  mov ebx,esi
  sub ebx,ecx         ; Size X
  mov ds,word ptr cs:[0]
  push ebx
L1:
  lea esi,[ecx+eax+offset Screenbuffer]
  xor edx,edx
  lea edi,[ecx+eax+0a0000h]
L2:
  mov ebx,[esi+edx]
  mov gs:[edi+edx],ebx
  add edx,4
  cmp edx,[ESP]
  jb L2
  lea eax,[eax+320]
  dec dword ptr [esp+4]
  jns L1
  pop ebx
  pop eax
  pop ds
  ret
ENDP FlushWindow      ; flush only parts of the buffer

PROC FastFillBuffer   ; ECX, EDX, ESI, EDI: Window BL: Color
  mov bh,bl
  push ds
  mov eax,ebx
  mov ds,word ptr cs:[0]
  shl eax,16
  mov ax,bx
  sub edi,edx         ; Size Y
  mov ebx,eax
  sub esi,ecx         ; Size X
  lea eax,[edx+4*edx]
  shl eax,6
  push esi
L1:
  xor edx,edx
  lea esi,[ecx+eax+offset Screenbuffer]
L2:
  mov [esi+edx],ebx
  add edx,4
  cmp edx,[ESP]
  jb L2
  lea eax,[eax+320]
  dec edi
  jns L1
  pop ebx
  pop ds
  ret
ENDP FastFillBuffer      ; flush only parts of the buffer

PROC FastFillDirect   ; ECX, EDX, ESI, EDI: Window BL: Color
  mov bh,bl
  push ds
  mov eax,ebx
  mov ds,word ptr cs:[0]
  shl eax,16
  mov ax,bx
  sub edi,edx         ; Size Y
  mov ebx,eax
  sub esi,ecx         ; Size X
  lea eax,[edx+4*edx]
  shl eax,6
  push esi
L1:
  xor edx,edx
  lea esi,[ecx+eax+0a0000h]
L2:
  mov gs:[esi+edx],ebx
  add edx,4
  cmp edx,[ESP]
  jb L2
  lea eax,[eax+320]
  dec edi
  jns L1
  pop ebx
  pop ds
  ret
ENDP FastFillDirect      ; flush only parts of the buffer

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
