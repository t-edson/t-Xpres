.COMMENT
 **************************************************************************
  DISS32.ASM - 32 Bit Disassembler - Main Modul
  Pass32 Version 2.3.1 - (c) 1996-1998 by Dieter Pawelczak
  ========================================================================
  MAIN MODUL: reads and checks parameters, invokes disassembler

  Assembler: FIRST: >>>Pass32 DIS32 -f<<< to generate ASM Table DIS32.BIN
             THEN   >>>Pass32 DISS32<<< to generate DISS32.EXE
  (c) 1998 by Dieter Pawelczak


 **************************************************************************

.MODEL FLAT
.UCU
.NM
.MEM 64                         ; needs only 64K Memory
.STACK 16                       ; needs only 16 KBytes of stack

.COMMENT *** INCLUDE FILES ***

.INCLUDE DOSX.INC               ; Extended Dos Functions
.INCLUDE SYSTEM.MAC             ; Macro Library
.INCLUDE STRING.INC             ; String Functions
.INCLUDE DISS32.INC             ; Disassembler module
.INCLUDE IO.INC                 ; File functions

.DATA
  SourceFile            DB  64 DUP(0)
  IsExt                 DB  '.',0
  EXEExt                DB  '.EXE',0
  TosExt                DB  '.3OS',0
  DLLExt                DB  '.DLL',0
  OVLExt                DB  '.OVL',0
  HeaderSize            DD 0
  SegmentLimit          DD ?
  FL                    DD ?          ; File Length
  FSEL                  DW ?          ; Selector for XMS Memory
  CurrentOffset         DD 256        ; Offset to load File
  RealSize              DD ?          ; actual binary size
  DString1              DB 256 DUP(0) ; String will contain hex values
  DString2              DB 256 DUP(0) ; String will contain instruction
  Spaces                DB '                              ',0
.DATA?
  Pro32Header           DB 16384 DUP(?)

.CODE


PROC getXMSmem                  ; allocates XMS Memory and creates a selector
.DATA
  xmemsize DD ?
  sel     DW ?
.CODE
  mov xmemsize,eax
  mov ax,0
  mov cx,1
  int 31h
  jc SystemError
  mov sel,ax
  mov eax,xmemSize
  mov ebx,eax
  shr ebx,16
  mov ecx,eax
  mov ax,0501h
  int 31h                            ; get memory
  jc MemError
  mov dx,cx
  mov cx,bx
  mov bx,sel
  mov ax,7
  int 31h                            ; set base
  jc SystemError
  mov bx,Sel
  mov ecx,XMemSize
  mov edx,XMemSize
  shr ecx,16
  or dx,0fffh
  mov ax,8
  int 31h                            ; set limit
  xor eax,eax
  mov ax,Sel
  ret
SystemError:
  color(15,0)
  writeln('Error: Invalid DPMI call.')
  jmp ErrorExit
MemError:
  color(15,0)
  writeln('Error: Too less XMS Memory available.')
  jmp ErrorExit
  ret
ENDP GetXmsMem

PROC loadfile2; EDX:Filename BX:Selector ESI:offset

.DATA
    FileCount  DD ?
    Fileload   DD ?
    Filehandle DW ?
    readnumber DW ?
.CODE
   mov fileload,esi
   mov fs,bx
   mov FileCount,0
   push gs
   push es
   mov ax,3d00h
   int 21h
   jc FileNotFound
   mov filehandle,ax           ;Filehandle
   mov edi,fileload
   mov ax,[6]
   mov gs,ax
@loadloop2:
   mov FileLoad,edi
   mov edi,offset intedi
   mov ax,cs:[8]
   mov intds,ax
   mov intedx,0
   mov inteax,3f00h
   mov bx,filehandle
   mov intebx,ebx
   mov intecx,16384
   mov bx,21h
   mov ax,0300h
   xor cx,cx
   int 31h
   mov ecx,inteax
   mov ReadNumber,cx
   add FileCount,ecx
   inc cx
   inc cx
   inc cx
   shr cx,2
   mov edi,fileload
   mov esi,0
@transferloop2:
   mov eax,gs:[si]
   mov fs:[edi],eax
   add si,4
   add edi,4
   loop @transferloop2
   mov cx,readnumber
   cmp cx,16384
   je  @loadloop2
   mov bx,filehandle
   mov ah,3eh
   int 21h
   pop es
   pop gs
   clc
   mov eax,FileCount
   ret
ENDP LoadFile2

PROC ReArrange                  ; removes Header Information
  mov edi,headersize
  add edi,100h
  mov esi,100h
  mov ecx,segmentlimit
  sub ecx,edi
  mov ax,fsel
  mov fs,ax
L0:
  mov al,FS:[edi]
  mov FS:[esi],al
  inc edi
  inc esi
  loopd L0
  ret
ENDP ReArrange

PROC Hexout                      ; prints Hexvalue in eax
  mov ebx,eax
  mov cl,28
L0:
  mov eax,ebx
  shr eax,cl
  and ax,0fh
  add ax,48
  cmp ax,58
  jb L1
  add ax,7
L1:
  mov dl,al
  mov ah,2
  int 21h
  sub cl,4
  cmp cl,0
  jne L0
  mov eax,ebx
  and ax,0fh
  add ax,48
  cmp ax,58
  jb L2
  add ax,7
L2:
  mov dl,al
  mov ah,2
  int 21h
  ret
ENDP HEXOUT

PROC DisAssembler
L0:
  mov eax, Currentoffset
  call Hexout
  mov dl,':'
  mov ah,2
  int 21h
  mov edi, Offset DString1
  mov ecx,256/4
  xor eax,eax
L1:
  mov [edi+4*ecx],eax
  loop L1
  mov [edi],eax
  mov edi, CurrentOffset
  mov edx, Offset DString1
  mov esi, Offset DString2
  mov ax,FSEL
  mov FS,ax
  mov bl,1                      ; hexadecimal output
  call Disassemble
  mov CurrentOffset,eax
  mov edi, offset DString1
  mov esi, offset Spaces
  call strcat
  mov byte ptr [edi+31],0
  mov esi, Offset DString2
  call strcat
  call strlength
  mov dword ptr [edi+eax],13+10*256+'$'*65536  ; append CR/LF and '$'
  mov edx,edi
  mov ah,9h
  int 21h
  mov eax, RealSize
  add eax, 256
  cmp CurrentOffset,eax
  jb L0
  ret
ENDP DisAssembler



.COMMENT ***  PROGRAM MAIN  ***

MAIN PROC NEAR
  color(11,0)
  Writeln('')
  Writeln('Diss32-Disassembler - Version 2.4 (c) 1996-1998 by Dieter Pawelczak')
  mov al,1
  call getparamstr
  jnc L1
  Writeln('===================================================================')
  writeln('')
  color(15,0)
  writeln('Syntax: Diss32 Filename[.EXT]                                      ')
  writeln('')
  color(14,0)
  writeln('Valid extensions are:')
  writeln('')
  writeln('- .BIN   for 32 bit binaries')
  writeln('- .DLL   for Pass32 32 bit DLL')
  writeln('- .EXE   for Pro32 Programs')
  writeln('- .3OS   for 3OS Programs')
  color(7,0)
  write(' ')
  exit(0);
L1:
  mov esi,edi                   ; get SourceFilename from ParameterLine
  mov edi, offset SourceFile
  call strcopy
  color(15,0)
  writeln('')
  write('Disassembling: ')
  mov edi, offset SourceFile
  call strupcase                ; upcase filename
  mov esi,edi
  mov edi, offset IsExt
  call strpos                   ; search for '.' in filename
  cmp al,0
  jne short L2
  mov edi, offset SourceFile    ; add '.EXE' extension
  mov esi, offset EXEExt
  call strcat
L2:
  mov edi, offset SourceFile
  call systemwriteln            ; print filename
  color(14,0)
  writeln('')
  mov esi, offset SourceFile
  mov edi, offset TosExt
  call strpos
  jnc short Tos
  mov esi, offset SourceFile
  mov edi, offset EXEExt
  call strpos
  jc L6                         ; file has not .EXE extension
Tos:
  mov edx,offset SourceFile
  xor ecx,ecx
  mov ax,3d00h
  int 21h
  jc FileNotFound               ; file not found
  push eax
  mov bx,ax
  mov edx,offset Pro32Header    ; read Pro32 Header
  mov ecx,16384
  mov ax,3f00h
  int 21h
  pop ebx
  mov ah,3eh
  int 21h
  mov edi,offset Pro32Header
  xor ecx,ecx
  L4:
  mov eax,[edi+ecx]
  cmp eax,3a4d454dh             ; 'MEM' - Mark
  je short L5
  inc cx
  cmp cx,16384
  jb L4
  color(15,0)
  writeLn('Error: This is not a valid Pro32 File...');
  jmp ErrorExit
L5:
  mov ax,[edi+ecx+8]
  mov word ptr HeaderSize,ax    ; get Pro32 Header Size
L6:
  mov edi,offset SourceFile
  call FileLength
  jc Filenotfound
  mov FL,eax
  add eax,32768
  mov Segmentlimit,eax          ; allocate XMS memory for file space
  call getXMSmem
  mov FSEL,ax
  mov esi, offset SourceFile
  mov edi, offset OVLExt
  call strpos
  jc short L7
  mov currentoffset,0         ; if OVL file, cut 256 bytes interface
L7:
  mov esi, offset SourceFile
  mov edi, offset DLLExt
  call strpos
  jc short L8
  mov currentoffset,0         ; if DLL file, cut 256 bytes interface
L8:
  mov bx,FSEL
  mov edx,Offset SourceFile
  mov esi, CurrentOffset
  call loadfile2
  mov currentoffset,256
  cmp headerSize,0
  je short L9
  call ReArrange              ; if Pro32 file then cut Pro32 Header
L9:
  mov eax,FL
  sub eax,HeaderSize
  mov RealSize,eax            ; actual file size without header
  call disassembler
  color(7,0)
  write(' ')
  Exit(0);
.PUBLIC Filenotfound:
  color(15,0)
  writeln('Error: File not found.')
.PUBLIC ErrorExit:
  color(7,0)
  write(' ')
  Exit(3)
  ret
ENDP MAIN
END MAIN
END
