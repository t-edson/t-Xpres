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
   SizeY           .EQU 240

   BufferOffset    DD ?
   BufferAddr      DD ?
   ScreenBuffer    DB SizeX*SizeY*NumberOfBuffers DUP(?)
   Reserve         DB 16*SizeX*DUP(?)

.CODE
.FAR

PROC InitvideoMode
  push ds
  mov ax,13h                    ; init 320x200
  int 10h
  mov dx,3d4h                   ; set X-Mode
  mov al,11h
  out dx,al
  inc dx
  in al,dx
  and al,7fh
  dec dx
  push eax
  mov al,11h
  out dx,al
  inc dx
  pop eax
  out dx,al
  mov al,0e3h
  mov dx,03c2h
  out dx,al
  mov dx,03d4h
  mov al,6
  out dx,al
  inc dx
  mov al,0dh
  out dx,al
  dec dx
  mov al,7
  out dx,al
  inc dx
  mov al,3eh
  out dx,al
  dec dx
  mov al,9
  out dx,al
  inc dx
  mov al,41h
  out dx,al
  dec dx
  mov al,10h
  out dx,al
  inc dx
  mov al,0eah
  out dx,al
  dec dx
  mov al,11h
  out dx,al
  inc dx
  mov al,0ach
  out dx,al
  dec dx
  mov al,12h
  out dx,al
  inc dx
  mov al,0dfh
  out dx,al
  dec dx
  mov al,15h
  out dx,al
  inc dx
  mov al,0e7h
  out dx,al
  dec dx
  mov al,16h
  out dx,al
  inc dx
  mov al,6
  out dx,al
  mov al,04                     ; enable TWEAK-Mode
  mov dx,3c4h
  out dx,al
  inc dx
  in al,dx
  and al,0f7h
  dec dx
  mov bl,al
  mov al,4
  out dx,al
  inc dx
  mov al,bl
  out dx,al
  mov dx,03d4h
  mov al,14h
  out dx,al
  inc dx
  in al,dx
  and al,0bfh
  mov bl,al
  mov al,14h
  dec dx
  out dx,al
  inc dx
  mov al,bl
  out dx,al
  mov al,17h
  dec dx
  out dx,al
  inc dx
  in al,dx
  or al,040h
  mov bl,al
  dec dx
  mov al,17h
  out dx,al
  inc dx
  mov al,bl
  out dx,al
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
  call FlushBuffer
  pop ds
  ret
ENDP InitvideoMode

.COMMENT *** PutPixel/GetPixel reserve all registers but not eax, color in bl ***


PROC PutPixelDirect             ; ECX: X EDX: Y  BL : Color

  lea eax,[edx*4+edx]
  push ecx
  shl eax,6
  push edx
  lea eax,[eax+ecx]
  mov bh,1
  shr eax,2
  and cl,3
  push eax
  shl bh,cl
  mov al,2
  mov dx,3c4h
  out dx,al
  inc dx
  mov al,bh
  out dx,al
  pop eax
  pop edx
  mov gs:[eax+0a0000h],bl
  pop ecx
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

PROC PutImageDirect     ; EBX = SIZE X2, EDI = SIZE Y2, ESI = OFFSET TO BUFFER
  push edx
  lea eax,[edx+4*edx]
  push ecx
  push esi
  add ebx,3
  shl eax,6
  push edi
  shr ebx,2
  push ebx
  lea ebx,[eax+ecx]
  mov al,2
  mov edx,3c4h
  shr ebx,2
  out edx,al
  mov al,1
  inc edx
  push ebx
  out edx,al
  xor ecx,ecx
L1:
  xor edx,edx
L2:
  mov al,[esi+edx*4]
  mov gs:[ebx+edx+0a0000h],al
  inc edx
  cmp edx,[ESP+4]
  jb L2
  inc ecx
  lea ebx,[ebx+320/4]
  lea esi,[esi+edx*4]
  cmp ecx,[esp+8]
  jb L1
  mov esi,[ESP+12]
  mov edx,3c4h
  mov ebx,[ESP]
  mov al,2
  out edx,al
  inc edx
  mov al,2
  out edx,al
  xor ecx,ecx
L3:
  xor edx,edx
L4:
  mov al,[esi+edx*4+1]
  mov gs:[ebx+edx+0a0000h],al
  inc edx
  cmp edx,[ESP+4]
  jb L4
  inc ecx
  lea ebx,[ebx+320/4]
  lea esi,[esi+edx*4]
  cmp ecx,[esp+8]
  jb L3
  mov esi,[ESP+12]
  mov edx,3c4h
  mov ebx,[ESP]
  mov al,2
  out edx,al
  inc edx
  mov al,4
  out edx,al
  xor ecx,ecx
L5:
  xor edx,edx
L6:
  mov al,[esi+edx*4+2]
  mov gs:[ebx+edx+0a0000h],al
  inc edx
  cmp edx,[ESP+4]
  jb L6
  inc ecx
  lea ebx,[ebx+320/4]
  lea esi,[esi+edx*4]
  cmp ecx,[esp+8]
  jb L5
  mov esi,[ESP+12]
  mov edx,3c4h
  mov ebx,[esp]
  mov al,2
  out edx,al
  inc edx
  mov al,8
  out edx,al
  xor ecx,ecx
L7:
  xor edx,edx
L8:
  mov al,[esi+edx*4+3]
  mov gs:[ebx+edx+0a0000h],al
  inc edx
  cmp edx,[ESP+4]
  jb L8
  inc ecx
  lea ebx,[ebx+320/4]
  lea esi,[esi+edx*4]
  cmp ecx,[esp+8]
  jb L7
  pop ebx
  pop eax
  pop ebx
  pop esi
  pop ecx
  pop edx
  ret
ENDP PutImageDirect

PROC GetImageDirect     ; EBX = SIZE X, EDI = SIZE Y, ESI = OFFSET TO BUFFER
  push edx
  lea eax,[edx+4*edx]
  push ecx
  push esi
  add ebx,3
  shl eax,6
  push edi
  shr ebx,2
  push ebx
  lea ebx,[eax+ecx]
  mov al,4
  mov edx,3ceh
  shr ebx,2
  out edx,al
  mov al,0
  inc edx
  push ebx
  out edx,al
  xor ecx,ecx
L1:
  xor edx,edx
L2:
  mov al,gs:[ebx+edx+0a0000h]
  mov [esi+edx*4],al
  inc edx
  cmp edx,[ESP+4]
  jb L2
  inc ecx
  lea ebx,[ebx+320/4]
  lea esi,[esi+edx*4]
  cmp ecx,[esp+8]
  jb L1
  mov esi,[ESP+12]
  mov edx,3ceh
  mov ebx,[ESP]
  mov al,4
  out edx,al
  inc edx
  mov al,1
  out edx,al
  xor ecx,ecx
L3:
  xor edx,edx
L4:
  mov al, gs:[ebx+edx+0a0000h]
  mov [esi+edx*4+1],al
  inc edx
  cmp edx,[ESP+4]
  jb L4
  inc ecx
  lea ebx,[ebx+320/4]
  lea esi,[esi+edx*4]
  cmp ecx,[esp+8]
  jb L3
  mov esi,[ESP+12]
  mov edx,3ceh
  mov ebx,[ESP]
  mov al,4
  out edx,al
  inc edx
  mov al,2
  out edx,al
  xor ecx,ecx
L5:
  xor edx,edx
L6:
  mov al,gs:[ebx+edx+0a0000h]
  mov [esi+edx*4+2],al
  inc edx
  cmp edx,[ESP+4]
  jb L6
  inc ecx
  lea ebx,[ebx+320/4]
  lea esi,[esi+edx*4]
  cmp ecx,[esp+8]
  jb L5
  mov esi,[ESP+12]
  mov edx,3ceh
  mov ebx,[esp]
  mov al,4
  out edx,al
  inc edx
  mov al,3
  out edx,al
  xor ecx,ecx
L7:
  xor edx,edx
L8:
  mov al,gs:[ebx+edx+0a0000h]
  mov [esi+edx*4+3],al
  inc edx
  cmp edx,[ESP+4]
  jb L8
  inc ecx
  lea ebx,[ebx+320/4]
  lea esi,[esi+edx*4]
  cmp ecx,[esp+8]
  jb L7
  pop ebx
  pop eax
  pop ebx
  pop esi
  pop ecx
  pop edx
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
  push edx
  lea eax,[edx+4*edx]
  push ecx
  push esi
  add ebx,3
  shl eax,6
  push edi
  shr ebx,2
  push ebx
  lea ebx,[eax+ecx]
  mov al,2
  mov edx,3c4h
  shr ebx,2
  out edx,al
  mov al,1
  and cl,3
  shl al,cl
  inc edx
  push ebx
  out edx,al
  xor ecx,ecx
L1:
  xor edx,edx
L2:
  mov al,[esi+edx*4]
  cmp al,0
  je short next
  mov gs:[ebx+edx+0a0000h],al
  next:
  inc edx
  cmp edx,[ESP+4]
  jb L2
  inc ecx
  lea ebx,[ebx+320/4]
  lea esi,[esi+edx*4]
  cmp ecx,[esp+8]
  jb L1
  mov esi,[ESP+12]
  mov edx,3c4h
  mov ebx,[ESP]
  mov al,2
  out edx,al
  inc edx
  mov cl,[esp+16]
  mov al,1
  inc cl
  and cl,3
  shl al,cl
  cmp cl,0
  jne short X1
  inc ebx
  inc dword ptr [ESP]
X1:
  out edx,al
  xor ecx,ecx
L3:
  xor edx,edx
L4:
  mov al,[esi+edx*4+1]
  cmp al,0
  je short next2
  mov gs:[ebx+edx+0a0000h],al
  next2:
  inc edx
  cmp edx,[ESP+4]
  jb L4
  inc ecx
  lea ebx,[ebx+320/4]
  lea esi,[esi+edx*4]
  cmp ecx,[esp+8]
  jb L3
  mov esi,[ESP+12]
  mov edx,3c4h
  mov ebx,[ESP]
  mov al,2
  out edx,al
  inc edx
  mov cl,[esp+16]
  mov al,1
  inc cl
  inc cl
  and cl,3
  shl al,cl
  cmp cl,0
  jne short X2
  inc ebx
  inc dword ptr [ESP]
X2:
  out edx,al
  xor ecx,ecx
L5:
  xor edx,edx
L6:
  mov al,[esi+edx*4+2]
  cmp al,0
  je short next3
  mov gs:[ebx+edx+0a0000h],al
  next3:
  inc edx
  cmp edx,[ESP+4]
  jb L6
  inc ecx
  lea ebx,[ebx+320/4]
  lea esi,[esi+edx*4]
  cmp ecx,[esp+8]
  jb L5
  mov esi,[ESP+12]
  mov edx,3c4h
  mov ebx,[esp]
  mov al,2
  out edx,al
  inc edx
  mov cl,[esp+16]
  mov al,1
  add cl,3
  and cl,3
  shl al,cl
  cmp cl,0
  jne short X3
  inc ebx
  inc dword ptr [ESP]
X3:
  out edx,al
  xor ecx,ecx
L7:
  xor edx,edx
L8:
  mov al,[esi+edx*4+3]
  cmp al,0
  je short next4
  mov gs:[ebx+edx+0a0000h],al
next4:
  inc edx
  cmp edx,[ESP+4]
  jb L8
  inc ecx
  lea ebx,[ebx+320/4]
  lea esi,[esi+edx*4]
  cmp ecx,[esp+8]
  jb L7
  pop ebx
  pop eax
  pop ebx
  pop esi
  pop ecx
  pop edx
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
  push edx
  lea eax,[edx+4*edx]
  push ecx
  push esi
  shl eax,6
  shr ebx,2
  lea ebx,[eax+ecx]
  mov al,2
  mov edx,3c4h
  shr ebx,2
  out edx,al
  mov al,1
  inc edx
  push ebx
  out edx,al
  xor ecx,ecx
L1:
  xor edx,edx
L2:
  mov al,[esi+edx*4]
  mov gs:[ebx+edx+0a0000h],al
  inc edx
  cmp edx,4
  jb L2
  inc ecx
  lea ebx,[ebx+320/4]
  lea esi,[esi+edx*4]
  cmp ecx,16
  jb L1
  mov esi,[ESP+4]
  mov edx,3c4h
  mov ebx,[ESP]
  mov al,2
  out edx,al
  inc edx
  mov al,2
  out edx,al
  xor ecx,ecx
L3:
  xor edx,edx
L4:
  mov al,[esi+edx*4+1]
  mov gs:[ebx+edx+0a0000h],al
  inc edx
  cmp edx,4
  jb L4
  inc ecx
  lea ebx,[ebx+320/4]
  lea esi,[esi+edx*4]
  cmp ecx,16
  jb L3
  mov esi,[ESP+4]
  mov edx,3c4h
  mov ebx,[ESP]
  mov al,2
  out edx,al
  inc edx
  mov al,4
  out edx,al
  xor ecx,ecx
L5:
  xor edx,edx
L6:
  mov al,[esi+edx*4+2]
  mov gs:[ebx+edx+0a0000h],al
  inc edx
  cmp edx,4
  jb L6
  inc ecx
  lea ebx,[ebx+320/4]
  lea esi,[esi+edx*4]
  cmp ecx,16
  jb L5
  mov esi,[ESP+4]
  mov edx,3c4h
  mov ebx,[esp]
  mov al,2
  out edx,al
  inc edx
  mov al,8
  out edx,al
  xor ecx,ecx
L7:
  xor edx,edx
L8:
  mov al,[esi+edx*4+3]
  mov gs:[ebx+edx+0a0000h],al
  inc edx
  cmp edx,4
  jb L8
  inc ecx
  lea ebx,[ebx+320/4]
  lea esi,[esi+edx*4]
  cmp ecx,16
  jb L7
  pop ebx
  pop esi
  pop ecx
  pop edx
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
  push edx
  sub edi,edx         ; Size Y
  push ecx
  inc edi
  push edi
  sub esi,ecx         ; Size X
  lea edi,[edx+4*edx]
  shl edi,6
  mov edx,3c4h
  lea edi,[edi+ecx]
  push esi
  mov al,2
  out edx,al
  shr edi,2
  inc edx
  and cl,3
  mov al,1
  shl al,cl
  out edx,al
  push edi
  xor ecx,ecx
L1:
  xor esi,esi
  xor edx,edx
L2:
  mov gs:[edi+esi+0a0000h],bl
  add edx,4
  inc esi
  cmp edx,[ESP+4]
  jb L2
  inc ecx
  lea edi,[edi+320/4]
  cmp ecx,[esp+8]
  jb L1
  mov edx,3c4h
  mov edi,[ESP]
  mov al,2
  out edx,al
  mov cl,[ESP+12]
  mov al,1
  inc cl
  and cl,3
  cmp cl,0
  jne short X1
  inc edi
  inc dword ptr [ESP]
  sub dword ptr [ESP+4],4
X1:
  shl al,cl
  inc edx
  out edx,al
  xor ecx,ecx
L3:
  xor esi,esi
  xor edx,edx
L4:
  mov gs:[edi+esi+0a0000h],bl
  add edx,4
  inc esi
  cmp edx,[ESP+4]
  jb L4
  inc ecx
  lea edi,[edi+320/4]
  cmp ecx,[esp+8]
  jb L3
  mov edx,3c4h
  mov edi,[ESP]
  mov al,2
  out edx,al
  mov cl,[ESP+12]
  mov al,1
  inc cl
  inc cl
  and cl,3
  cmp cl,0
  jne short X2
  inc edi
  inc dword ptr [ESP]
  sub dword ptr [ESP+4],4
X2:
  shl al,cl
  inc edx
  out edx,al
  xor ecx,ecx
L5:
  xor edx,edx
  xor esi,esi
L6:
  mov gs:[edi+esi+0a0000h],bl
  add edx,4
  inc esi
  cmp edx,[ESP+4]
  jb L6
  inc ecx
  lea edi,[edi+320/4]
  cmp ecx,[esp+8]
  jb L5
  mov edx,3c4h
  mov edi,[esp]
  mov al,2
  out edx,al
  mov cl,[ESP+12]
  mov al,1
  add cl,3
  and cl,3
  cmp cl,0
  jne short X3
  inc edi
  inc dword ptr [ESP]
  sub dword ptr [ESP+4],4
X3:
  shl al,cl
  inc edx
  out edx,al
  xor ecx,ecx
L7:
  xor edx,edx
  xor esi,esi
L8:
  mov gs:[edi+esi+0a0000h],bl
  add edx,4
  inc esi
  cmp edx,[ESP+4]
  jb L8
  inc ecx
  lea edi,[edi+320/4]
  cmp ecx,[esp+8]
  jb L7
  pop ebx
  pop eax
  pop ebx
  pop ecx
  pop edx
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
  ret
ENDP FillBuffer

PROC FlushBuffer
  push ds
  mov edx,3c4h
  mov al,2
  mov ds,word ptr cs:[0]
  out edx,al
  inc edx
  mov al,1
  out edx,al
  mov edi,0a0000h
  mov esi,BufferOffset
  mov ecx,SizeX*SizeY/4
L1:
  mov bl,[esi+4*ecx]
  mov gs:[edi+ecx],bl
  loop L1
  mov bl,[esi]
  dec edx
  mov al,2
  mov gs:[edi],bl
  out edx,al
  inc edx
  out edx,al
  mov ecx,SizeX*SizeY/4-4
L2:
  mov bl,[esi+4*ecx+1]
  mov gs:[edi+ecx],bl
  loop L2
  mov bl,[esi]
  dec edx
  mov al,2
  mov gs:[edi],bl
  out edx,al
  inc edx
  mov al,4
  out edx,al
  mov ecx,SizeX*SizeY/4-4
L3:
  mov bl,[esi+4*ecx+2]
  mov gs:[edi+ecx],bl
  loop L3
  mov bl,[esi]
  dec edx
  mov al,2
  mov gs:[edi],bl
  out edx,al
  inc edx
  mov al,8
  out edx,al
  mov ecx,SizeX*SizeY/4-4
L4:
  mov bl,[esi+4*ecx+3]
  mov gs:[edi+ecx],bl
  loop L4
  mov bl,[esi]
  pop ds
  mov gs:[edi],bl
  ret
ENDP FlushBuffer      ; flush whole buffer to Screen

PROC FlushWindow      ; ECX, EDX, ESI, EDI: Window Positions
  push ds
  lea ebx,[edx+4*edx]
  push edx
  sub edi,edx         ; Size Y
  push ecx
  inc edi
  shl ebx,6
  push edi
  sub esi,ecx         ; Size X
  mov ds,word ptr cs:[0]
  add esi,7
  mov edx,3c4h
  shr esi,2
  lea ebx,[ebx+ecx]
  push esi
  mov al,2
  out edx,al
  shr ebx,2
  inc edx
  mov al,1
  out edx,al
  push ebx
  xor ecx,ecx
L1:
  lea esi,[ebx*4]
  xor edx,edx
  add esi,BufferOffset
  lea edi,[ebx+0a0000h]
L2:
  mov al,[esi+4*edx]
  mov gs:[edi+edx],al
  inc edx
  cmp edx,[ESP+4]
  jb L2
  inc ecx
  lea ebx,[ebx+320/4]
  cmp ecx,[esp+8]
  jb L1
  mov edx,3c4h
  mov ebx,[ESP]
  mov al,2
  out edx,al
  inc edx
  mov al,2
  out edx,al
  xor ecx,ecx
L3:
  lea esi,[ebx*4+1]
  xor edx,edx
  add esi,BufferOffset
  lea edi,[ebx+0a0000h]
L4:
  mov al,[esi+4*edx]
  mov gs:[edi+edx],al
  inc edx
  cmp edx,[ESP+4]
  jb L4
  inc ecx
  lea ebx,[ebx+320/4]
  cmp ecx,[esp+8]
  jb L3
  mov edx,3c4h
  mov ebx,[ESP]
  mov al,2
  out edx,al
  inc edx
  mov al,4
  out edx,al
  xor ecx,ecx
L5:
  lea esi,[ebx*4+2]
  xor edx,edx
  add esi,BufferOffset
  lea edi,[ebx+0a0000h]
L6:
  mov al,[esi+4*edx]
  mov gs:[edi+edx],al
  inc edx
  cmp edx,[ESP+4]
  jb L6
  inc ecx
  lea ebx,[ebx+320/4]
  cmp ecx,[esp+8]
  jb L5
  mov edx,3c4h
  mov ebx,[esp]
  mov al,2
  out edx,al
  inc edx
  mov al,8
  out edx,al
  xor ecx,ecx
L7:
  lea esi,[ebx*4+3]
  xor edx,edx
  add esi,BufferOffset
  lea edi,[ebx+0a0000h]
L8:
  mov al,[esi+4*edx]
  mov gs:[edi+edx],al
  inc edx
  cmp edx,[ESP+4]
  jb L8
  inc ecx
  lea ebx,[ebx+320/4]
  cmp ecx,[esp+8]
  jb L7
  pop ebx
  pop eax
  pop ebx
  pop ecx
  pop edx
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
  push edx
  sub edi,edx         ; Size Y
  push ecx
  inc edi
  push edi
  sub esi,ecx         ; Size X
  lea edi,[edx+4*edx]
  add esi,3
  shl edi,6
  mov edx,3c4h
  shr esi,2
  lea edi,[edi+ecx]
  push esi
  mov al,2
  out edx,al
  shr edi,2
  inc edx
  mov al,1
  out edx,al
  push edi
  xor ecx,ecx
L1:
  xor edx,edx
L2:
  mov gs:[edi+edx+0a0000h],bl
  inc edx
  cmp edx,[ESP+4]
  jb L2
  inc ecx
  lea edi,[edi+320/4]
  cmp ecx,[esp+8]
  jb L1
  mov edx,3c4h
  mov edi,[ESP]
  mov al,2
  out edx,al
  inc edx
  mov al,2
  out edx,al
  xor ecx,ecx
L3:
  xor edx,edx
L4:
  mov gs:[edi+edx+0a0000h],bl
  inc edx
  cmp edx,[ESP+4]
  jb L4
  inc ecx
  lea edi,[edi+320/4]
  cmp ecx,[esp+8]
  jb L3
  mov edx,3c4h
  mov edi,[ESP]
  mov al,2
  out edx,al
  inc edx
  mov al,4
  out edx,al
  xor ecx,ecx
L5:
  xor edx,edx
L6:
  mov gs:[edi+edx+0a0000h],bl
  inc edx
  cmp edx,[ESP+4]
  jb L6
  inc ecx
  lea edi,[edi+320/4]
  cmp ecx,[esp+8]
  jb L5
  mov edx,3c4h
  mov edi,[esp]
  mov al,2
  out edx,al
  inc edx
  mov al,8
  out edx,al
  xor ecx,ecx
L7:
  xor edx,edx
L8:
  mov gs:[edi+edx+0a0000h],bl
  inc edx
  cmp edx,[ESP+4]
  jb L8
  inc ecx
  lea edi,[edi+320/4]
  cmp ecx,[esp+8]
  jb L7
  pop ebx
  pop eax
  pop ebx
  pop ecx
  pop edx
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
