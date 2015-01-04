.COMMENT
 ****************************************************************************
 DLOADS.INC     Pass32 SYSTEM File
                invoked by internal debugger to display source code
                (c) 1996,1998 by Dieter Pawelczak - Pass32 Version 2.0
 ============================================================================
 Attention:     don't alter this file!
 DLOADS.INC - SYSTEM FILE USED BY THE PASS32 ASSEMBLER
 When assembling with the -MM option and debugging this file is appended
 to the source file. This file can load and display the debug file created
 by Pass32 ...
 ****************************************************************************

.INCLUDE DPMI.INC

.IFPM

.DATA
      DebugFileLoad     DD 0
      DebugFileError    db '00000000: Debug File .DMP not Found - assemble with PASS32 -mm',13,10
      DebugFileName     DB DEBUGFILEINFO,0
.DATA?
      DebugFILEBUFFER   DB 256*1024 DUP(?)      ; FILE BUFFSER SIZE
      DebugScreenBuffer DB 160*22 DUP(?)
      DebugHexBuffer    DB 8 DUP(?)
      DebugAddrBuffer   DD ?
      DebugAddr         DD ?
.CODE

.NODEBUG

PROC DISPLAYDEBUGSOURCE
   mov Debugaddr,edi
   cmp debugfileload,0
   jnz L3
   mov edi,offset DebugFilename
   mov esi,offset DebugFileBuffer
   call loadfile
   jnc L2
   mov ecx,20
   mov esi,offset DebugFileError-4
   mov edi,offset DebugFileBuffer-4
L1:
   mov eax,[esi+ecx*4]
   mov [edi+ecx*4],eax
   loop L1
   mov eax,40
L2:
   add eax,offset debugfilebuffer
   mov debugfileload,eax
L3:
; find the address in the debug file
   mov ax,cs:[2]
   mov es,ax
   mov edi,offset DEBUGFILEBUFFER
L4:
   mov DEBUGADDRBUFFER,EDI
   mov esi,offset DEBUGHEXBUFFER
   xor edx,edx
L5:
   mov al,[edi]
   cmp al,10
   je L7
   cmp al,13
   je L6
   cmp edx,8
   jae L6
   mov [esi+edx],al
L6:
   inc edi
   inc edx
   cmp debugfileload,edi
   ja L5
L7:
   mov eax,debugfileload
   cmp edi,eax
   jae L10
   inc edi
   mov esi,offset debughexbuffer
   mov ecx,28
   xor ebx,ebx
L8:
   xor eax,eax
   mov al,[esi]
   sub al,48   ; ascii to dec
   cmp al,11
   jb L9
   sub al,7
L9:
   shl eax,cl
   or ebx,eax
   inc esi
   sub cl,4
   jns L8
   cmp ebx,debugaddr
   jne L4
   je L11
L10:
   mov edi, offset DEBUGFILEBUFFER
   mov DEBUGADDRBUFFER,edi
L11:
   mov edi,DEBUGADDRBUFFER
   xor ecx,ecx
L12:
   xor eax,eax
   mov ax,160
   mul cx
   mov esi,eax
   xor edx,edx
L13:
   mov al,[edi]
   cmp al,10
   je L15
   cmp al,13
   je L14
   cmp edx,80
   jae L14
   mov es:[esi],al
   add esi,2
L14:
   inc edi
   inc edx
   jmp L13
L15:
   mov eax,debugfileload
   cmp edi,eax
   jae L16
   inc edi
   inc cx
   cmp cx,22
   jb L12
L16:
   ret
ENDP DISPLAYDEBUGSOURCE

PROC DISPLAYSCREENSTORE
   mov ecx,879
   xor esi,esi
   mov edi,offset DebugScreenbuffer
   mov ebx,0e200e20h
L1:
   mov eax,es:[esi+4*ecx]
   mov es:[esi+4*ecx],ebx
   mov [edi+4*ecx],eax
   loop L1
   mov eax,es:[esi]
   mov es:[esi],ebx
   mov [edi],eax
   ret
ENDP DISPLAYSCREENSTORE

PROC DISPLAYSCREENRESTORE
   mov ecx,879
   xor esi,esi
   mov edi,offset DebugScreenbuffer
L1:
   mov eax,[edi+4*ecx]
   mov es:[esi+4*ecx],eax
   loop L1
   mov eax,[edi]
   mov es:[esi],eax
   ret
ENDP DISPLAYSCREENRESTORE

.IFM NOT IO.INC
.DATA
      FILEHANDLE DD     0
      ReadNumber DW     0
      FileLoad   DD     0
      FileCount  DD     0
.CODE

COPYTOREAL2 PROC         ;EDI : OFFSET;   Zero For End
   mov ax,[6]      ; Real Mode File Buffer Selector
   mov es,ax
   mov esi,0
L1:
   mov al,[edi]
   mov es:[si],al
   inc si
   inc edi
   cmp al,0
   jne L1
   ret
ENDP COPYTOREAL2

LoadFile        PROC    ;  EDI : OFFSET FileName, ESI : Destination
                        ;  RETURN:   EAX: Length
   push gs
   push es
   mov FileCount,0
   mov fileload,esi
   call CopyToReal2   ;FileName To Real
   mov inteax,3d00h
   mov intedx,0
   mov ax,[8]
   mov intds,ax
   mov edi,offset intedi
   mov ax,ds
   mov es,ax
   mov ax,300h
   xor cx,cx
   mov bx,21h
   int 31h
   mov ax,intflg
   test ax,1
   jne L3
   mov ebx,inteax
   mov filehandle,ebx           ;Filehandle
   mov edi,fileload
   mov ax,[6]
   mov gs,ax
L1:
   mov FileLoad,edi
   mov edi,offset intedi
   mov ax,[8]
   mov intds,ax
   mov intedx,0
   mov inteax,3f00h
   mov ebx,filehandle
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
L2:
   mov eax,gs:[si]
   mov [edi],eax
   add si,4
   add edi,4
   loop L2
   mov cx,readnumber
   cmp cx,16384
   je  L1
   mov ebx,filehandle
   mov ah,3eh
   int 21h
   pop es
   pop gs
   clc
   mov eax,FileCount
   ret
L3:
   pop es
   pop gs
   stc
   ret
ENDP LoadFile
.ENDIF
.ELSE
.OUT No Source Code Displaying for the real mode debugger!
.ENDIF

