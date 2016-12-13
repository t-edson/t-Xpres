.COMMENT
 ****************************************************************************
 IO.INC         Some useful PM I/O functions
                (c) 1997,1998 by Dieter Pawelczak - Pass32 Version 2.5
 ============================================================================
 INTERFACE

 Function FileLength: EDI offset to filename, length in EAX
 Function LoadFile: EDI offset to filename, ESI destination, result:size in EAX
 Function SaveFile: EDI offset to filename, ESI Source, ECX: size
 Function OpenFileToRead: EDI offset to filename, returns handle in ebx
 Function OpenFileToWrite: EDI offset to filename, returns handle in ebx
 Function CloseFile: EBX handle
 Function BlockRead: EBX handle, ECX size, EDI memory dest., returns bytes read in EAX
 Function BlockWrite: EBX handle, ECX size, EDI mem. source, returns bytes written in EAX
 Function GetDir: copies directory name of DRIVE in DL to STRING in EDI
 Function ChDir: changes to the directory in String EDI

 Failure: Carry Flag set

 ****************************************************************************


.include dpmi.inc


.DATA
      FILEHANDLE   DD     0
      ReadNumber   DW     0
      FileLoad     DD     0
      FileCount    DD     0
      FileCounter  DD     0
.CODE

.COMMENT *** Function FileLength: EDI Offset to filename, length in EAX ***

FileLength PROC
   call openFileToRead
   jc short L1
   push ebx           ;save filehandle
   xor ecx,ecx
   xor edx,edx
   mov ax,4202h
   int 21h
   movzx ebx,ax
   movzx eax,dx
   shl eax,10h
   add eax,ebx
   pop ebx    ;filehandle
   push eax   ;filesize
   mov ax,3e00h
   int 21h
   pop eax
   clc
   ret
L1:
   xor eax,eax
   stc
   ret
ENDP FILELENGTH

.COMMENT *** Function LoadFile:  EDI offset to filename, ESI destination ***

LoadFile        PROC    ;  RETURN:   EAX: Length, Carry on Error
   push gs
   push es
   mov FileCount,0
   mov fileload,esi
   call OpenFileToRead
   jc L3
   mov filehandle,ebx           ;Filehandle
.IFPM
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
   mov ax,3e00h
   int 21h
   pop es
   pop gs
   clc
   mov eax,FileCount
   ret
.ELSE
; Real Mode Load File Procedure
   mov ax,word ptr fileload
   mov dx,ax
   mov cx,65535
   mov bx, word ptr filehandle
   mov ax,3f00h
   int 21h
   jc L3
   push ax
   mov ax,3e00h
   mov bx, word ptr filehandle
   int 21h
   pop ax
   pop es
   pop gs
   clc
   ret
.ENDIF
L3:
   pop es
   pop gs
   stc
   ret
ENDP LoadFile

.COMMENT *** Function SaveFile: EDI offset to filename, ESI Source, ECX: size ***

SaveFile        PROC    ;  RETURN:    Carry on Error
   push gs
   push es
   mov FileCount,ecx
   mov fileload,esi
   call OpenFileToWrite
   jc L5
   mov filehandle,ebx           ;Filehandle
.IFPM
   mov ax,[6]
   mov gs,ax
L1:
   mov edi,fileload
   xor esi,esi
L2:
   mov eax,[edi]
   mov gs:[esi],eax
   add esi,4
   add edi,4
   sub FileCount,4
   jc short L3
   cmp FileCount,0
   je short L3
   cmp si,16384
   jb L2
   mov eax,esi
   jmp short L4
L3:
   mov eax,Filecount
   add eax, esi
L4:
   mov ebx,[edi]
   mov gs:[esi],ebx
   mov fileload,edi
   mov intecx,eax               ; size of block
   mov ax,[8]
   mov intds,ax
   mov intedx,0
   mov inteax,4000h
   mov ebx,filehandle
   mov intebx,ebx
   mov ax,ds
   mov es,ax
   mov edi,offset intedi
   mov bx,21h
   mov ax,0300h
   xor cx,cx
   int 31h
   jc short L5
   cmp FileCount,0
   jg L1
   mov ebx,filehandle
   mov ax,3e00h
   int 21h
   pop es
   pop gs
   clc
   ret
.ELSE
   mov cx, word ptr filecount
   mov dx, word ptr fileload
   mov ax,4000h
   mov bx,word ptr filehandle
   int 21h
   jc L5
   mov bx,word ptr filehandle
   mov ax,3e00h
   int 21h
   pop es
   pop gs
   clc
   ret
.ENDIF
L5:
   pop es
   pop gs
   stc
   ret
ENDP SaveFile

.COMMENT *** Function BlockRead: EBX handle, ECX size, EDI dest., returns bytes read in EAX ***

BlockRead       PROC          ;
.IFPM
   push gs
   push es
   mov FileCount,EcX
   mov FileCounter,0
   mov fileload,EDI
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
   mov ecx,16384
   cmp FileCount,16384
   ja L2
   mov ecx,FileCount
L2:
   mov intecx,ecx
   sub FileCount,ecx

   mov bx,21h
   mov ax,0300h
   xor cx,cx
   int 31h
   mov ecx,inteax
   mov ReadNumber,cx
   add FileCounter,ecx
   mov edi,fileload
   push ds
   mov ax,gs
   mov ds,ax
   xor esi,esi
   rep movsb
   pop ds
   mov ecx,FileCounter
   cmp FileCount,0
   ja L1
   mov eax,FileCounter
   mov ebx,Filehandle
   pop es
   pop gs
   ret
.ELSE
   mov dx,di
   mov ax,3f00h
   int 21h
   ret
.ENDIF
ENDP BlockRead

.COMMENT *** Function BlockWrite: EBX handle, ECX size, EDI dest., returns bytes written in EAX ***

BlockWrite        PROC    ;  RETURN:    Carry on Error
.IFPM
   push gs
   push es
   mov FileCount,ecx
   mov fileload,esi
   mov filehandle,ebx           ;Filehandle
   mov FileCounter,0
   mov ax,[6]
   mov gs,ax
L1:
   mov edi,fileload
   xor esi,esi
L2:
   mov eax,[edi]
   mov gs:[esi],eax
   add esi,4
   add edi,4
   sub FileCount,4
   jc short L3
   cmp FileCount,0
   je short L3
   cmp si,16384
   jb L2
   mov eax,esi
   jmp short L4
L3:
   mov eax,Filecount
   add eax, esi
L4:
   mov ebx,[edi]
   mov gs:[esi],ebx
   mov fileload,edi
   mov intecx,eax               ; size of block
   mov ax,[8]
   mov intds,ax
   mov intedx,0
   mov inteax,4000h
   mov ebx,filehandle
   mov intebx,ebx
   mov ax,ds
   mov es,ax
   mov edi,offset intedi
   mov bx,21h
   mov ax,0300h
   xor cx,cx
   int 31h
   xor eax,eax
   mov ax,word ptr inteax
   add FileCounter,eax
   cmp FileCount,0
   jg L1
   mov ebx,filehandle
   mov eax,FileCounter
   pop es
   pop gs
   clc
   ret
.ELSE
   mov dx,di
   mov ax,4000h
   int 21h
   ret
.ENDIF
ENDP BlockWrite

.COMMENT *** Function OpenFileToRead: EDI offset to filename, returns handle in ebx ***

OpenFileToRead PROC
.IFPM
   mov inteax,3d00h             ; DOS Function for Open file to read
   call @OpenFile
.ELSE
   mov ax,3d00h
   xor cx,cx
   mov dx,di
   int 21h
   mov bx,ax
.ENDIF
   ret
ENDP OpenFileToRead

.COMMENT *** Function OpenFileToWrite: EDI offset to filename, returns handle in ebx ***

OpenFileToWrite PROC
.IFPM
   mov inteax,3c00h             ; DOS Function for Open file to write
   call @OpenFile
.ELSE
   mov ax,3c00h
   xor cx,cx
   mov dx,di
   int 21h
   mov bx,ax
.ENDIF
   ret
ENDP OpenFileToWrite

.COMMENT ***  Function CloseFile: EBX handle ***

PROC CloseFile
   mov ax,3e00h
   int 21h
   ret
ENDP CloseFile

.COMMENT *** Function GetDir: copies directory name of DRIVE in DL to STRING in EDI ***

PROC GetDir ; Copies current directory of Drive in DL to String in EDI
.IFPM
  push ebp
  push edi
  push edx
  mov word ptr [edi],0
  mov intedx,edx
  xor esi,esi
  mov intesi,esi
  mov ax,cs:[8]
  mov intds,ax
  mov inteax,4700h
  mov ax,ds
  mov es,ax
  mov edi,offset intedi
  mov eax,300h
  mov ebx,21h
  xor ecx,ecx
  int 31h
  pop edx
  pop edi
  mov ax,intflg
  test ax,1
  jne short L4
  cmp dl,0
  jnz short L1
  mov ah,19h
  int 21h
  mov dl,al
  inc dl
L1:
  add dl,64
  mov dh,58
  mov [edi],dx
  inc edi
  inc edi
  mov dl,92
  mov [edi],dl
  inc edi
  xor esi,esi
  mov ax,cs:[6]
  mov fs,ax
L2:
  mov al,fs:[esi]
  cmp al,0
  je short L3
  mov [edi],al
  inc edi
  inc esi
  jmp L2
L3:
  pop ebp
  ret
L4:
  stc
  pop ebp
.ELSE
  mov ax,4700h
  mov si,di
  int 21h
.ENDIF
  ret
ENDP GetDir

.COMMENT *** Function ChDir: changes to the directory in String EDI ***

PROC ChDir; changes to directory in String EDI
.IFPM
   push ebp
   call @COPYTOREAL2
   mov Inteax,3b00h
   mov Intedx,0
   mov ax,cs:[8]
   mov intds,ax
   mov ax,ds
   mov es,ax
   mov edi,offset intedi
   mov eax,300h
   mov ebx,21h
   xor ecx,ecx
   int 31h
   pop ebp
   mov ax,intflg
   test ax,1
   jne short L1
   ret
L1:
   stc
.ELSE
  mov ax,3b00h
  mov dx,di
  int 21h
.ENDIF
   ret
ENDP ChDir

.COMMENT *** Subfunctions ***

@OpenFile PROC
   call @copytoreal2
   mov intedx,0
   mov intecx,0
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
   jne short L1
   mov ebx,inteax     ;Filehandle
   ret
L1:
   stc
   ret
ENDP @OpenFile

@COPYTOREAL2 PROC         ;EDI : OFFSET;   Zero For End
   mov ax,[6]            ; Real Mode File Buffer Selector
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
ENDP @COPYTOREAL2


;.IFPM
;.ELSE
;.OUT Target not for real mode
;.ERR
;.ENDIF


;END LIB



