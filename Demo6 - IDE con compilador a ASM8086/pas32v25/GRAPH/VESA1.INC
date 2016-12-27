.COMMENT
 ****************************************************************************
 VESA1.INC      Vesa Graphic Driver for 640x480 mode
                Code adapted in large parts from VIDEO.INC by Chris Hazard.
                Pass32 adaptions and some additions by Dieter Pawelczak.
                Note, this driver can be load by GRAPHIC.INC
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
   SizeX           .EQU 640
   SizeY           .EQU 480

ScreenBuffer    db    640*480 dup (?) ; 640*480 MODE
EndOfBuffer     db    ?
Reserve         DB 16*SizeX*DUP(?)

.DATA
NoVesa		db		'VESA not detected', 0
NotDetected	db		'Video Mode Not Detected', 0
BankError	db		'An error occured in bank switching',0
BankShift	db		?
VideoSelector	dw		?
ModeToSet	dd		?
CurrentBank     db              0
BufferAddr      dd              ?

.CODE
.FAR

PROC InitVideoMode
  push ds
  mov ds,word ptr cs:[0]
  mov eax,0
  mov edi,offset Screenbuffer
  mov ecx,640*480/4-1
L1:
  mov [edi+ecx*4],eax
  loopd L1
  mov [edi],eax
  mov ModeToSet,101h            ; VESA-MODE
  call Initialize
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
  pop ds
  ret
ENDP InitVideoMode

PROC PutPixelBuffer
  lea eax,[edx+4*edx]
  shl eax,7
.CONST
  db 65h,88h,9ch,08h
  BufferAddr1 DD ?
.CODE
  ret
ENDP PutPixelBuffer



PROC GetPixelBuffer
  lea eax,[edx+4*edx]
  shl eax,7
.CONST
  db 65h,8ah,9ch,08h
  BufferAddr2 DD ?
.CODE
  ret
ENDP GetPixelBuffer

PROC PutPixelDirect
  lea eax,[edx+4*edx]
  shl eax,7
  add eax,ecx
  push eax
  mov al,[esp+2]
  cmp al,cs:CurrentBank
  je short L1
  push ds
  mov ds,word ptr cs:[0]
  mov CurrentBank,al
  push edx
  push ebx
  movzx dx,al
  push ecx
  mov cl,BankShift
  shl edx,cl		;calculate bank shift
  xor ebx,ebx
  mov ax,4f05h
  int 10h
  pop ecx
  pop ebx
  pop edx
  pop ds
L1:
  pop eax
  and eax,0ffffh
  mov gs:[eax+0a0000h],bl
  ret
ENDP PutPixelDirect

PROC GetPixelDirect
  lea eax,[edx+4*edx]
  shl eax,7
  add eax,ecx
  push eax
  mov al,[esp+2]
  cmp al,cs:CurrentBank
  je short L1
  push ds
  mov ds,word ptr cs:[0]
  mov CurrentBank,al
  push edx
  movzx dx,al
  push ecx
  mov cl,BankShift
  shl edx,cl		;calculate bank shift
  xor ebx,ebx
  mov ax,4f05h
  int 10h
  pop ecx
  pop edx
  pop ds
L1:
  pop eax
  and eax,0ffffh
  mov bl,gs:[eax+0a0000h]
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

PROC PutImageDirect
  push es
  lea eax,[edx+4*edx]
  push ecx
  mov es,word ptr cs:[0]
  push edx
  shl eax,7
  push edi            ; Size Y
  lea eax,[eax+ecx]
  push ebx            ; Size X
  xor edx,edx
  push eax
  mov dl,[ESP+2]
  mov cl,es:[BankShift]
  cmp dl,es:Currentbank
  je short L0
  mov es:CurrentBank,dl
  mov ax,4f05h
  shl edx,cl		;calculate bank shift
  xor ebx,ebx
  int 10h
L0:
  pop eax
  mov ebx,eax
  add ebx,65536
  xor bx,bx             ; in ebx: value for next bank switch
L1:
  mov edx,eax
  add edx,[ESP]
  cmp edx,ebx
  jae  L4
  mov edi,eax
  xor edx,edx
  and edi,0ffffh
L2:
  mov ecx,[esi]
  mov gs:[edi+edx+0a0000h],ecx
  add edx,4
  add esi,4
  cmp edx,[ESP]
  jb L2
Cont:
  lea eax,[eax+640]
  cmp eax,ebx
  jae short L3
  dec dword ptr [esp+4]
  jnz L1
  pop ebx
  pop eax
  pop edx
  pop ecx
  pop es
  ret
L3:
  add ebx,65536
  inc es:currentbank
  xor edx,edx
  mov cl,es:bankshift
  mov dl,es:currentbank
  push ebx
  shl edx,cl
  push eax
  xor ebx,ebx
  mov ax,4f05h
  push esi
  int 10h
  pop esi
  pop eax
  pop ebx
  dec dword ptr [esp+4]
  jnz L1
  pop ebx
  pop eax
  pop edx
  pop ecx
  pop es
  ret
L4:
  mov edi,eax
  xor edx,edx
  and edi,0ffffh
L5:
  mov ecx,[esi]
  mov gs:[edi+edx+0a0000h],ecx
  add edx,4
  add esi,4
  lea ecx,[eax+edx]
  cmp ecx,ebx
  jb short L6
  push edx
  add ebx,65536
  xor edx,edx
  inc es:currentbank
  mov dl,es:currentbank
  mov cl,es:bankshift
  push ebx
  shl edx,cl
  push eax
  xor ebx,ebx
  mov ax,4f05h
  int 10h
  pop eax
  pop ebx
  pop edx
  sub edi,65536
  cmp edx,[ESP]
  jb L2
L6:
  cmp edx,[ESP]
  jb L5
  jmp Cont
  ret
ENDP PutImageDirect


PROC GetImageDirect
  push es
  lea eax,[edx+4*edx]
  push ecx
  shl eax,7
  push edx
  mov es,word ptr cs:[0]
  push edi            ; Size Y
  lea eax,[eax+ecx]
  push ebx            ; Size X
  xor edx,edx
  push eax
  mov dl,[ESP+2]
  mov cl,es:[BankShift]
  cmp dl,es:Currentbank
  je short L0
  mov es:CurrentBank,dl
  mov ax,4f05h
  shl edx,cl		;calculate bank shift
  xor ebx,ebx
  int 10h
L0:
  pop eax
  mov ebx,eax
  add ebx,65536
  xor bx,bx             ; in ebx: value for next bank switch
L1:
  mov edx,eax
  add edx,[ESP]
  cmp edx,ebx
  ja L4
  mov edi,eax
  xor edx,edx
  and edi,0ffffh
L2:
  mov ecx,gs:[edi+edx+0a0000h]
  mov [esi],ecx
  add esi,4
  add edx,4
  cmp edx,[ESP]
  jb L2
Cont:
  lea eax,[eax+640]
  cmp eax,ebx
  jae short L3
  dec dword ptr [esp+4]
  jne L1
  pop ebx
  pop eax
  pop edx
  pop ecx
  pop es
  ret
L3:
  add ebx,65536
  inc es:currentbank
  xor edx,edx
  mov cl,es:bankshift
  mov dl,es:currentbank
  push ebx
  shl edx,cl
  push eax
  xor ebx,ebx
  mov ax,4f05h
  int 10h
  pop eax
  pop ebx
  dec dword ptr [esp+4]
  jne L1
  pop ebx
  pop eax
  pop edx
  pop ecx
  pop es
  ret
L4:
  mov edi,eax
  xor edx,edx
  and edi,0ffffh
L5:
  mov ecx,gs:[edi+edx+0a0000h]
  mov [esi],ecx
  add edx,4
  add esi,4
  lea ecx,[eax+edx]
  cmp ecx,ebx
  jb short L6
  push edx
  add ebx,65536
  xor edx,edx
  inc es:currentbank
  mov cl,es:bankshift
  mov dl,es:currentbank
  push ebx
  shl edx,cl
  push eax
  xor ebx,ebx
  mov ax,4f05h
  int 10h
  pop eax
  pop ebx
  pop edx
  sub edi,65536
  cmp edx,[ESP]
  jb L2
L6:
  cmp edx,[ESP]
  jb L5
  jmp Cont
  ret

ENDP GetImageDirect

PROC PutImageBuffer
  push es
  push ecx
  push edx
  mov es,word ptr cs:[4]
  lea eax,[edx*4+edx]   ; always assume DWORD Alignement
  add ebx,3
  shl eax,7
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
  lea eax,[eax+640]
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
  shl eax,7
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
  lea eax,[eax+640]
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
  push es
  lea eax,[edx+4*edx]
  push ecx
  shl eax,7
  push edx
  mov es,word ptr cs:[0]
  push edi            ; Size Y
  lea eax,[eax+ecx]
  push ebx            ; Size X
  xor edx,edx
  push eax
  mov dl,[ESP+2]
  mov cl,es:[BankShift]
  cmp dl,es:Currentbank
  je short L0
  mov es:CurrentBank,dl
  mov ax,4f05h
  shl edx,cl		;calculate bank shift
  xor ebx,ebx
  int 10h
L0:
  pop eax
  mov ebx,eax
  add ebx,65536
  xor bx,bx             ; in ebx: value for next bank switch
L1:
  mov edx,eax
  add edx,[ESP]
  cmp edx,ebx
  ja L4
  mov edi,eax
  xor edx,edx
  and edi,0ffffh
L2:
  mov cl,[esi]
  cmp cl,0
  je short LX
  mov gs:[edi+edx+0a0000h],cl
LX:
  inc esi
  inc edx
  cmp edx,[ESP]
  jb L2
Cont:
  lea eax,[eax+640]
  cmp eax,ebx
  jae short L3
  dec dword ptr [esp+4]
  jne L1
  pop ebx
  pop eax
  pop edx
  pop ecx
  pop es
  ret
L3:
  add ebx,65536
  inc es:currentbank
  xor edx,edx
  mov cl,es:bankshift
  mov dl,es:currentbank
  push ebx
  shl edx,cl
  push eax
  xor ebx,ebx
  mov ax,4f05h
  int 10h
  pop eax
  pop ebx
  dec dword ptr [esp+4]
  jne L1
  pop ebx
  pop eax
  pop edx
  pop ecx
  pop es
  ret
L4:
  mov edi,eax
  xor edx,edx
  and edi,0ffffh
L5:
  mov cl,[esi]
  cmp cl,0
  je short LX2
  mov gs:[edi+edx+0a0000h],cl
LX2:
  inc edx
  inc esi
  lea ecx,[eax+edx]
  cmp ecx,ebx
  jb short L6
  push edx
  add ebx,65536
  xor edx,edx
  inc es:currentbank
  mov cl,es:bankshift
  mov dl,es:currentbank
  push ebx
  shl edx,cl
  push eax
  xor ebx,ebx
  mov ax,4f05h
  int 10h
  pop eax
  pop ebx
  pop edx
  sub edi,65536
  cmp edx,[ESP]
  jb L2
L6:
  cmp edx,[ESP]
  jb L5
  jmp Cont
  ret
  ret

ENDP PutSpriteDirect

PROC PutSpriteBuffer
  push ecx
  push edx
  lea eax,[edx*4+edx]   ; always assume DWORD Alignement
  shl eax,7
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
  lea eax,[eax+640]
  dec edx
  jne Loop
  pop ebx
  pop edx
  pop ecx
  ret

ENDP PutSpriteBuffer

PROC Put16x16Direct
  push es
  lea eax,[edx+4*edx]
  push ecx
  mov es,word ptr cs:[0]
  push edx
  shl eax,7
  push 16            ; Size Y
  lea eax,[eax+ecx]
  xor edx,edx
  push eax
  mov dl,[ESP+2]
  mov cl,es:[BankShift]
  cmp dl,es:Currentbank
  je short L0
  mov es:CurrentBank,dl
  mov ax,4f05h
  shl edx,cl		;calculate bank shift
  xor ebx,ebx
  int 10h
L0:
  pop eax
  mov ebx,eax
  add ebx,65536
  xor bx,bx             ; in ebx: value for next bank switch
L1:
  lea edx,[eax+16]
  cmp edx,ebx
  jae L4
  mov edi,eax
  xor edx,edx
  and edi,0ffffh
L2:
  mov ecx,[esi]
  mov gs:[edi+edx+0a0000h],ecx
  add edx,4
  add esi,4
  cmp edx,16
  jb L2
Cont:
  lea eax,[eax+640]
  cmp eax,ebx
  jae short L3
  dec dword ptr [esp]
  jnz L1
  pop eax
  pop edx
  pop ecx
  pop es
  ret
L3:
  add ebx,65536
  inc es:currentbank
  xor edx,edx
  mov cl,es:bankshift
  mov dl,es:currentbank
  push ebx
  shl edx,cl
  push eax
  xor ebx,ebx
  mov ax,4f05h
  int 10h
  pop eax
  pop ebx
  dec dword ptr [esp]
  jnz L1
  pop eax
  pop edx
  pop ecx
  pop es
  ret
L4:
  mov edi,eax
  xor edx,edx
  and edi,0ffffh
L5:
  mov ecx,[esi]
  mov gs:[edi+edx+0a0000h],ecx
  add edx,4
  add esi,4
  lea ecx,[eax+edx]
  cmp ecx,ebx
  jb short L6
  push edx
  add ebx,65536
  xor edx,edx
  inc es:currentbank
  mov dl,es:currentbank
  mov cl,es:bankshift
  push ebx
  shl edx,cl
  push eax
  xor ebx,ebx
  mov ax,4f05h
  int 10h
  pop eax
  pop ebx
  pop edx
  sub edi,65536
  cmp edx,16
  jb L2
L6:
  cmp edx,16
  jb L5
  jmp Cont
  ret

ENDP Put16x16Direct

PROC Put16x16Buffer
  lea eax,[edx*4+edx]
  shl eax,7
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
  lea eax,[eax+640]
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
  push ds
  mov ds,word ptr cs:[0]
  lea eax,[edx+4*edx]
  sub edi,edx         ; Size Y
  shl eax,7
  push edi
  sub esi,ecx         ; Size X
  push esi
  lea eax,[eax+ecx]
  xor edx,edx
  push ebx
  push eax
  mov cl,[BankShift]
  mov dl,[ESP+2]
  mov CurrentBank,dl
  mov ax,4f05h
  shl edx,cl		;calculate bank shift
  xor ebx,ebx
  int 10h
  pop eax
  pop esi               ;
  mov ebx,eax
  add ebx,65536
  xor bx,bx             ; in ebx: value for next bank switch
L1:
  mov edx,eax
  add edx,[ESP]
  cmp edx,ebx
  jae L4
  mov edi,eax
  xor edx,edx
  mov cx,si
  and edi,0ffffh
L2:
  mov gs:[edi+edx+0a0000h],cl
  inc edx
  cmp edx,[ESP]
  jbe L2
Cont:
  lea eax,[eax+640]
  cmp eax,ebx
  jae short L3
  dec dword ptr [esp+4]
  jns L1
  pop ebx
  pop eax
  pop ds
  ret
L3:
  add ebx,65536
  inc currentbank
  xor edx,edx
  mov cl,bankshift
  mov dl,currentbank
  push ebx
  shl edx,cl
  push eax
  xor ebx,ebx
  mov ax,4f05h
  push esi
  int 10h
  pop esi
  pop eax
  pop ebx
  dec dword ptr [esp+4]
  jnz L1
  pop ebx
  pop eax
  pop ds
  ret
L4:
  mov edi,eax
  xor edx,edx
  and edi,0ffffh
L5:
  mov cx,si
  mov gs:[edi+edx+0a0000h],cl
  inc edx
  lea ecx,[eax+edx]
  cmp ecx,ebx
  jb short L6
  push edx
  add ebx,65536
  xor edx,edx
  inc currentbank
  mov dl,currentbank
  mov cl,bankshift
  push ebx
  shl edx,cl
  push eax
  xor ebx,ebx
  mov ax,4f05h
  int 10h
  pop eax
  pop ebx
  pop edx
  sub edi,65536
  cmp edx,[ESP]
  jbe L2
L6:
  cmp edx,[ESP]
  jbe L5
  jmp Cont
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
  shl ebx,7
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
  lea ebx,[ebx+640]
  dec edx
  jns Loop
  pop edx
  pop ecx
  pop es
  ret
ENDP FillBuffer

PROC FastFillDirect
  mov bh,bl
  push ds
  mov eax,ebx
  mov ds,word ptr cs:[0]
  shl eax,16
  mov ax,bx
  mov ebx,eax
  lea eax,[edx+4*edx]
  sub edi,edx         ; Size Y
  shl eax,7
  push edi
  sub esi,ecx         ; Size X
  push esi
  lea eax,[eax+ecx]
  xor edx,edx
  push ebx
  push eax
  mov cl,[BankShift]
  mov dl,[ESP+2]
  mov CurrentBank,dl
  mov ax,4f05h
  shl edx,cl		;calculate bank shift
  xor ebx,ebx
  int 10h
  pop eax
  pop esi               ;
  mov ebx,eax
  add ebx,65536
  xor bx,bx             ; in ebx: value for next bank switch
L1:
  mov edx,eax
  add edx,[ESP]
  cmp edx,ebx
  jae L4
  mov edi,eax
  xor edx,edx
  and edi,0ffffh
L2:
  mov gs:[edi+edx+0a0000h],esi
  add edx,4
  cmp edx,[ESP]
  jb L2
Cont:
  lea eax,[eax+640]
  cmp eax,ebx
  jae short L3
  dec dword ptr [esp+4]
  jns L1
  pop ebx
  pop eax
  pop ds
  ret
L3:
  add ebx,65536
  inc currentbank
  xor edx,edx
  mov cl,bankshift
  mov dl,currentbank
  push ebx
  shl edx,cl
  push eax
  xor ebx,ebx
  mov ax,4f05h
  push esi
  int 10h
  pop esi
  pop eax
  pop ebx
  dec dword ptr [esp+4]
  jns L1
  pop ebx
  pop eax
  pop ds
  ret
L4:
  mov edi,eax
  xor edx,edx
  and edi,0ffffh
L5:
  mov gs:[edi+edx+0a0000h],esi
  add edx,4
  lea ecx,[eax+edx]
  cmp ecx,ebx
  jb short L6
  push edx
  add ebx,65536
  xor edx,edx
  inc currentbank
  mov dl,currentbank
  mov cl,bankshift
  push ebx
  shl edx,cl
  push eax
  xor ebx,ebx
  mov ax,4f05h
  int 10h
  pop eax
  pop ebx
  pop edx
  sub edi,65536
  cmp edx,[ESP]
  jb L2
L6:
  cmp edx,[ESP]
  jb L5
  jmp Cont
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
  lea ebx,[edx*4+edx]   ; always assume DWORD Alignement
  add esi,3
  sub edi,edx
  shl ebx,7
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
  lea ebx,[ebx+640]
  dec edx
  jns Loop
  pop edx
  pop ecx
  pop es
  ret
ENDP FastFillBuffer

PROC FlushBuffer
  ret
ENDP FlushBuffer      ; flush whole buffer to Screen

PROC FlushWindow      ; ECX, EDX, ESI, EDI: Window Positions
  mov ebx,edi
  lea eax,[edx+4*edx]
  push ds
  sub ebx,edx         ; Size Y
  push ebx
  shl eax,7
  mov ebx,esi
  sub ebx,ecx         ; Size X
  mov ds,word ptr cs:[0]
  push ebx
  lea eax,[eax+ecx]
  xor edx,edx
  push eax
  mov cl,[BankShift]
  mov dl,[ESP+2]
  mov CurrentBank,dl
  mov ax,4f05h
  shl edx,cl		;calculate bank shift
  xor ebx,ebx
  int 10h
  pop eax
  mov ebx,eax
  add ebx,65536
  xor bx,bx             ; in ebx: value for next bank switch
L1:
  mov edx,eax
  add edx,[ESP]
  cmp edx,ebx
  jae L4
  mov edi,eax
  xor edx,edx
  and edi,0ffffh
L2:
  mov esi,[eax+edx+offset Screenbuffer]
  mov gs:[edi+edx+0a0000h],esi
  add edx,4
  cmp edx,[ESP]
  jb L2
Cont:
  lea eax,[eax+640]
  cmp eax,ebx
  jae short L3
  dec dword ptr [esp+4]
  jns L1
  pop ebx
  pop eax
  pop ds
  ret
L3:
  add ebx,65536
  inc currentbank
  xor edx,edx
  mov cl,bankshift
  mov dl,currentbank
  push ebx
  shl edx,cl
  push eax
  xor ebx,ebx
  mov ax,4f05h
  int 10h
  pop eax
  pop ebx
  dec dword ptr [esp+4]
  jns L1
  pop ebx
  pop eax
  pop ds
  ret
L4:
  lea esi,[eax+offset Screenbuffer]
  mov edi,eax
  xor edx,edx
  and edi,0ffffh
L5:
  mov esi,[eax+edx+offset Screenbuffer]
  mov gs:[edi+edx+0a0000h],esi
  add edx,4
  lea esi,[eax+edx]
  cmp esi,ebx
  jb short L6
  push edx
  add ebx,65536
  xor edx,edx
  inc currentbank
  mov dl,currentbank
  mov cl,bankshift
  push ebx
  shl edx,cl
  push eax
  xor ebx,ebx
  mov ax,4f05h
  int 10h
  pop eax
  pop ebx
  pop edx
  sub edi,65536
  cmp edx,[ESP]
  jb L2
L6:
  cmp edx,[ESP]
  jb L5
  jmp Cont
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


;Code by Chris Hazard -------------------------------------------------------

;Real Mode Calling "Structure"

.INCLUDE DPMI.INC

;----------------------------------------------------------------------------

.CODE

;****************************************************************************
Initialize proc
;	        push esi
		call GetVesaInfo		;see if vesa is there
		call GetVesaModeInfo		;get info
		call SetVesaMode		;set mode
;		pop esi				;save esi for palette
;		call SetPal			;set palette
                ret
ENDP Initialize

;****************************************************************************
GetVesaInfo proc NEAR
		mov Inteax,4f00h
		mov Intecx,0
		mov Intedx,0
		mov Intedi,0
		mov ax,word ptr cs:[8]
		mov Intes,ax
		mov edi,0
		push es
		push ds
		pop es
		mov eax,300h
		mov ebx,10h
		xor ecx,ecx
		mov edi,offset Intedi
		int 31h
		mov eax,[Inteax]
		pop es
		cmp ax,4fh
		jz AllSet
		mov edi,offset NoVesa
		xor ebx,ebx
		xor ecx,ecx
		xor edx,edx
		mov ah,1
		call PrintError
		mov ax,4c00h
		int 21h
	AllSet:	ret
ENDP GetVesaInfo



;****************************************************************************
GetVesaModeInfo proc
		mov eax,[ModeToSet]
		mov Intecx,eax
		mov ax,word ptr cs:[8]
		mov Inteax,4f01h
		mov Intes,ax
		mov Intedi,0
		mov Intedx,0
		mov ax,300h
		mov bx,10h
		xor ecx,ecx
		push es
		push ds
		pop es
		mov edi,offset Intedi
		int 31h

		mov cx,cs:[6]
		mov es,cx
		mov ax,es:[0]
		and ax,1
		cmp ax,1
		je AllGood

		;show error in video mode
		pop es
		mov edi,offset NotDetected
		mov ah,1
		xor ecx,ecx
		xor ebx,ebx
		xor edx,edx
		call PrintError
		mov ax,4c00h
		int 21h
	AllGood:

		mov bx,es:[4]		;load Window Granularity
		mov ax,es:[6]		;load Window Size
		xor dx,dx
		div bx			;WinSize / WinGran

		cmp ax,1
		jne NB1
		mov [BankShift],0
		jmp NBF
	NB1:	cmp ax,2
		jne NB2
		mov [BankShift],1
		jmp NBF
	NB2:	cmp ax,4
		jne NB3
		mov [BankShift],2
	NB3:	cmp ax,8
		jne NB4
		mov [BankShift],3
		jmp NBF
	NB4:	cmp ax,16
		jne NB5
		mov [BankShift],4
		jmp NBF
	NB5:	cmp ax,32
		jne NB6
		mov [BankShift],5
		jmp NBF
	NB6:	cmp ax,64
		jne NBE
		mov [BankShift],6
		jmp NBF
	NBE:	;print stuff about error
		pop es
		mov edi,offset BankError
		mov ah,1
		xor ebx,ebx
		xor eax,eax
		xor edx,edx
		call PrintError
		mov ax,4c00h
		int 21h
	NBF:

	;Now save selector for video
		pop es
		mov BX,0A000h		;Video Graphic Memory
		mov AX,0002h
		int 31h
		mov [VideoSelector],ax
		ret
ENDP GetVesaModeInfo

;****************************************************************************
SetVesaMode proc
		xor eax,eax
		xor ebx,ebx
		xor edx,edx
		mov ax,4f02h
		mov ebx,[ModeToSet]
		xor ecx,ecx
		int 10h
		ret
ENDP SetVesaMode


;****************************************************************************
FlushSBuffer proc
                push ds
                mov ds,word ptr cs:[0]
		mov ax,[VideoSelector]
		mov es,ax		;load video selector

		mov esi,offset ScreenBuffer
		xor ebx,ebx
       		xor edx,edx		;clear bankswitch
		mov ax,4f05h
		int 10h			;call vesa bank switch
                cld

		xor edi,edi		;start at beginning of screen
		mov ecx,4000h		;loop this many times (DW = 4 B)
	rep	movsd			;flush screen buffer
                inc edx
                mov cl,cs:[BankShift]
		push edx
		shl edx,cl		;calculate bank shift
		mov ax,4f05h
		int 10h			;call vesa bank switch
                pop edx

		xor edi,edi		;start at beginning of screen
		mov ecx,4000h		;loop this many times (DW = 4 B)
	rep	movsd			;flush screen buffer
                inc edx
                mov cl,cs:[BankShift]
		push edx
		shl edx,cl		;calculate bank shift
		mov ax,4f05h
		int 10h			;call vesa bank switch
                pop edx

		xor edi,edi		;start at beginning of screen
		mov ecx,4000h		;loop this many times (DW = 4 B)
	rep	movsd			;flush screen buffer
                inc edx
                mov cl,cs:[BankShift]
		push edx
		shl edx,cl		;calculate bank shift
		mov ax,4f05h
		int 10h			;call vesa bank switch
                pop edx

		xor edi,edi		;start at beginning of screen
		mov ecx,4000h		;loop this many times (DW = 4 B)
	rep	movsd			;flush screen buffer
                inc edx
                mov cl,cs:[BankShift]
		push edx
		shl edx,cl		;calculate bank shift
		mov ax,4f05h
		int 10h			;call vesa bank switch
                pop edx

		xor edi,edi		;start at beginning of screen
		mov ecx,11264		;loop this many times (DW = 4 B)
	rep	movsd			;flush screen buffer
                mov currentBank,dl
		pop ds
		ret
ENDP FlushSBuffer

;****************************************************************************

PrintError proc
  ; ah : Color, cx : X, dx : Y, EDI: OFFSET Message
	mov bh,ah
	mov ax,cs:[2]
	mov es,ax
	mov ax,160
	mul dx
	shl cx,1
	add ax,cx
	mov si,ax        ; Screen Offset
printeloop:
	mov bl,[edi]
	cmp bl,0
	je printeende
	mov es:[si],bx
	add si,2
	inc edi
	jne printeloop
printeende:
	inc edi
	ret
ENDP printerror
;****************************************************************************
