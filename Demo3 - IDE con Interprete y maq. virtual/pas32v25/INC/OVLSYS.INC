.COMMENT
 ****************************************************************************
 OVLSYS.INC     SYSTEM FILE USED BY THE PASS32 ASSEMBLER TO LOAD OVL FILES
                (c) 1996,1997 by Dieter Pawelczak - Pass32 Version 2.3
 ============================================================================

 This file will be automatically included, when an OVL modul is included.
 Attention - you should not alter this file -
             unless you know what you're doing!
 ****************************************************************************


.include dpmi.inc

.data
      ovlfileload    dd 0
      ovlfilehandle  dd 0
      ovlreadnumber  dw 0
      OVL_0          dd 0
      OVLPATH        db 'PATH',0
.Code

OVLCOPYTOREAL2 PROC         ;EDI : OFFSET;   Zero For End
   mov ax,[6]      ; Real Mode File Buffer Selector
   mov es,ax
   mov esi,0
OVLCOPYTOREAL2LOOP:
   mov al,[edi]
   mov es:[si],al
   inc si
   inc edi
   cmp al,0
   jne OVLCOPYTOREAL2LOOP
   ret
ENDP OVLCOPYTOREAL2

LoadOVL        PROC    ;  EDI : OFFSET FileName (' .OVL')
                       ;  ESI : OFFSET OVLNAME
                       ;  RETURN CF on ERROR
   push gs
   push es
   mov ovlfileload,esi
   push edi
   mov esi,offset OVL_0 ; search in the current directory first
   call systemfindfile
   pop edi
   jnc F1
   push edi
   mov edi,offset OVLPATH ; search the path next
   call getenvstr
   mov esi,eax
   pop edi
   push edi
   call systemfindfile
   pop edi
   jnc F1
   call OVLCopyToReal2   ;try nevertheless...
F1:
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
   jne LoadOVLError
   mov ebx,inteax
   mov OVLfilehandle,ebx           ;Filehandle
   mov ax,[6]
   mov gs,ax
   mov edi,offset intedi
   mov ax,[8]
   mov intds,ax
   mov intedx,0
   mov inteax,3f00h
   mov ebx,ovlfilehandle
   mov intebx,ebx
   mov intecx,256
   mov bx,21h
   mov ax,0300h
   xor cx,cx
   int 31h
   mov ecx,inteax
   mov edi,ovlfileload
   mov esi,0
   mov ecx,64
ovltransferheader:
   mov eax,gs:[si]
   mov [edi],eax
   add si,4
   add edi,4
   loop ovltransferheader
   mov edi,ovlfileload
   mov ax,cs            ; 48 bit pointer needs CS selector
   add edi,36
   mov cx,36
ovlheadercs:
   mov [edi],ax
   add edi,6
   loop ovlheadercs
   mov edi,ovlfileload             ; First dword of header contains load offset
   mov edi,[edi]
ovlloadloop:
   mov ovlFileLoad,edi
   mov edi,offset intedi
   mov ax,[8]
   mov intds,ax
   mov intedx,0
   mov inteax,3f00h
   mov ebx,ovlfilehandle
   mov intebx,ebx
   mov intecx,16384
   mov bx,21h
   mov ax,0300h
   xor cx,cx
   int 31h
   mov ecx,inteax
   mov OvlReadNumber,cx
   inc cx
   inc cx
   inc cx
   shr cx,2
   mov edi,ovlfileload
   mov esi,0
ovltransferloop:
   mov eax,gs:[si]
   mov [edi],eax
   add si,4
   add edi,4
   loop ovltransferloop
   mov cx,Ovlreadnumber
   cmp cx,16384
   je  Ovlloadloop
   mov ebx,ovlfilehandle
   mov ah,3eh
   int 21h
   pop es
   pop gs
   clc
   ret
LoadOVLError:
   pop es
   pop gs
.IF NOT OVL_ERROR
   stc
   ret
ENDP LoadOVL
.ELSE
   .DATA
       OVLerror     db     'ERROR loading Overlay - abort ',0
   .CODE
      mov edi,offset OVLerror
   .IFM SYSTEM.INC
      call SystemWriteLn
   .ELSE
      call OVLPrint
      mov dx,1800h
      xor bx,bx
      mov ah,2
      int 10h
   .ENDIF
      mov ah,4ch
      int 21h
      ret
ENDP LoadOVL

.IFM NOT SYSTEM.INC
OVLPrint        PROC       ; Print Message with offset in edi
     mov esi,24*160
     mov ah,14
OVLPrintLoop:
     mov al,[edi]
     cmp al,0
     jne OVLPrintNext
     mov di,0
     mov ecx,25*160/4
OVLPrintNewLine:
     mov eax,es:[di+160]
     mov es:[di],eax
     add di,4
     loop OVLPrintNewLine
     ret
OVLPrintNext:
     mov es:[esi],ax
     inc edi
     add esi,2
     jmp OVLPrintLoop
     ret
ENDP OVLPrint
.ENDIF
.ENDIF

InitOVL        PROC     ; ESI : OFFSET OVLNAME
   mov ax,cs            ; 48 bit pointer needs CS selector
   push esi
   push edi
   mov edi,offset InitOVLAddress
   add esi,36
   mov cx,36
Initovlheadercs:
   mov [esi],ax
   mov [esi-4],edi
   add esi,6
   loop Initovlheadercs
   pop edi
   pop esi
   ret
InitOVLAddress db 0cbh
ENDP InitOvl

.IFM NOT SYSTEM.INC

; IF SYSTEM MODULE NOT INCLUDED ...

PROC SystemFindFile ; Filename in EDI, Directories in ESI
.DATA?
  FindFiles DB 64 DUP(?)
.CODE
  push fs
  push es
  mov fs,word ptr cs:[6]
  mov es,word ptr cs:[0]
  mov dword ptr fs:[0],0
; Copy Directory ...
L0:
  mov edx,0
  mov ecx,OFFSET Findfiles
L1:
  mov bl,[ESI]
  cmp bl,0
  je short L2
  cmp bl,';'
  je short L2
  mov fs:[EDX],bl
  mov [ecx],bl
  inc esi
  inc edx
  inc ecx
  jmp L1
L2:
; ADD Filename
  xor eax,eax
  mov bl,fs:[edx]
  cmp bl,0
  je short L3
  cmp bl,'\'
  je short L3
  mov byte ptr fs:[edx],'\'
  mov byte ptr [ecx],'\'
  inc edx
  inc ecx
L3:
  mov bl,[edi+eax]
  mov fs:[edx],bl
  mov [ecx],bl
  inc edx
  inc ecx
  inc eax
  cmp bl,0
  je short L4
  jmp L3
L4:
  push esi
  push edi
  mov intecx,0
  mov intedx,0
  mov ax,cs:[8]
  mov intds,ax
  mov inteax,4e00h
  mov edi,offset intedi
  mov es,word ptr cs:[0]
  mov ax,300h
  mov bx,21h
  xor ecx,ecx
  int 31h
  mov ax,intflg
  test ax,1
  jne short L5
  mov eax,offset FindFiles
  pop edi
  pop esi
  pop es
  pop fs
  clc
  ret
L5:
  pop edi
  pop esi
  mov bl,[esi]
  cmp bl,0
  je short L6
  inc esi
  jmp L0
L6:
  pop es
  pop fs
  mov eax,offset FindFiles
  mov dword ptr [eax],0
  stc
  ret
ENDP SystemFindfile

PROC GetEnvStr  ; returns string in eax
.DATA?
  GetEnv DB 64 DUP(?)
.CODE
  push es
  mov esi,offset getenv
  mov dword ptr [esi],0
  mov ax,cs:[2ch]
  mov es,ax
  xor eax,eax
L0:
  xor edx,edx
Lp:
  mov bl,es:[eax]
  mov cl,[edi+edx]
  cmp cl,97
  jb L1
  and cl,223
L1:
  cmp cl,0
  je short L2
  cmp bl,61
  je short L3
  cmp bl,cl
  jne short L3
  inc eax
  inc edx
  jmp Lp
L2:
  inc eax
  mov edi,offset getenv
L6:
  mov bl,es:[eax]
  mov [edi],bl
  inc edi
  inc eax
  cmp bl,0
  jne short L6
  pop es
  mov eax,offset getenv
  ret
L3:
  mov bl,es:[eax]
  inc eax
  cmp bl,0
  jne L3
  mov bl,es:[eax]
  cmp bl,0
  je short L5
  jmp L0
L5:
  pop es
  mov eax,offset getenv
  ret
ENDP GetEnvStr
.ENDIF

