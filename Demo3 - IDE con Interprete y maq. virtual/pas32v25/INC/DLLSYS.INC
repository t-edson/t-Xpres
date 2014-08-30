.COMMENT
 ****************************************************************************
 DLLSYS.INC     SYSTEM FILE USED BY THE PASS32 ASSEMBLER TO LOAD DLL FILES
                (c) 1996,1997 by Dieter Pawelczak - Pass32 Version 2.3
 ============================================================================

 This file will be automatically included, when a DLL modul is included.
 Attention - you should not alter this file -
             unless you know what you're doing!
 ****************************************************************************

.include dpmi.inc

.data
      DLLfileload    dd 0
      DLLfilehandle  dd 0
      DLLreadnumber  dw 0
      DLLInterface   dd 0
      DLLDescriptor  dd 0
                     dd 0
      DLLDSSel       dw 0
      DLL_0          dd 0
      DLLPATH        db 'PATH',0

.Code
DLLCOPYTOREAL2 PROC         ;EDI : OFFSET;   Zero For End
   mov ax,CS:[6]      ; Real Mode File Buffer Selector
   mov es,ax
   mov esi,0
DLLCOPYTOREAL2LOOP:
   mov al,[edi]
   mov es:[si],al
   inc si
   inc edi
   cmp al,0
   jne DLLCOPYTOREAL2LOOP
   ret
ENDP DLLCOPYTOREAL2

LoadDLL        PROC    ;  EDI : OFFSET FileName (' .DLL')
                       ;  ESI : OFFSET DLLNAME
                       ;  RETURN CFLAG ON ERROR AX=0 File not found
   push gs
   push es
   mov DLLInterface,esi
   mov DLLfileload,esi
   push edi
   mov esi,offset DLL_0 ; search in the current directory first
   call systemfindfile
   pop edi
   jnc F1
   push edi
   mov edi,offset DLLPATH ; search the path next
   call getenvstr
   mov esi,eax
   pop edi
   push edi
   call systemfindfile
   pop edi
   jnc F1
   call DLLCopyToReal2   ; try nevertheless ...
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
   jne X
   mov ebx,inteax
   mov DLLfilehandle,ebx           ;Filehandle
   mov ax,[6]
   mov gs,ax
   mov edi,offset intedi
   mov ax,[8]
   mov intds,ax
   mov intedx,0
   mov inteax,3f00h
   mov ebx,DLLfilehandle
   mov intebx,ebx
   mov intecx,256
   mov bx,21h
   mov ax,0300h
   xor cx,cx
   int 31h
   mov ecx,inteax
   mov edi,DLLfileload
   xor si,si
   mov cx,64
L1:              ; Copy Header to DLL Interface
   mov eax,gs:[si]
   mov [edi],eax
   add si,4
   add edi,4
   loop L1
   mov edi,DLLInterface         ; Init DLL Interface Area
   mov AX,cs:[2]
   mov [edi+2],AX               ; Video Selector
   mov AX,cs:[4]
   mov [edi+4],AX               ; Zero Sel
   mov ax,cs:[6]
   mov [edi+6],AX               ; Real Mode Sel
   mov ax,cs:[8]
   mov [edi+8],AX               ; Real Mode Segment
   mov ax,100h
   mov ebx,0ffffh
   int 31h                      ; Get Free DOS Memory
   shl ebx,4                    ; in blocks
   mov edi,DLLInterface         ; Init DLL Interface Area
   cmp ebx,[edi+0Ah]
   jb NotRM
   mov ebx,[edi+0ah]
   shr ebx,4                   ; Memory Size in Bytes
   inc ebx
   mov ax,100h
   int 31h
   jc short NotRM
   xor ebx,ebx
   mov bx,ax
   shl ax,4
   mov [edi+14h],eax            ; EAX linear Address off DLL (ZeroSel Operation)
   mov bx,dx                    ; dx : descriptor
   mov ax,0ah
   int 31h                      ; Get Alias Descriptor
   jc XX
   mov edi,DLLInterface         ; Init DLL Interface Area
   mov dword ptr [edi+0eh],0    ; No Memory Handle For Free DLL Function
   mov [edi],AX                 ; DS Selector for DLL
   mov BX,AX                    ; DS Sel in BX
   jmp L2
NotRM:
   mov BX,[edi+0Ch]
   mov CX,[edi+0Ah]             ; Get requested Memory from Header
   mov AX,0501h
   int 31h                      ; Get Memory
   jc X
   push di
   mov edi,DLLInterface         ; Init DLL Interface Area
   mov [edi+0eh],SI             ; SI
   pop si
   mov [edi+10h],SI             ; DI Memory Handle For Free DLL Function
   mov [edi+14h],CX             ; CX
   mov [edi+16h],BX             ; BX linear Address off DLL (ZeroSel Operation)
   mov SI,[edi+0ch]
   mov DI,[edi+0ah]
   mov ax,0600h                 ; lock linear memory region
   int 31h
   xor AX,AX
   mov cx,1
   int 31h                      ; Get Sel
   jc XX
   mov edi,DLLInterface         ; Init DLL Interface Area
   mov [edi],AX                 ; DS Selector for DLL
   mov DLLDSSel,AX              ; save DLL DS Sel
   mov DX,CX
   mov CX,BX                    ; Linear Address in CX:DX
   mov BX,AX                    ; DS Sel in BX
   and dx,0fffch
   mov AX,0007h
   int 31h                      ; Set Basis Address of Selector
   mov bx,DLLDSSEL
   mov DX,0fffh
   or DX,[edi+0Ah]
   mov cx,[edi+0Ch]             ; Memory Size in CX:DX
   mov AX,0008h
   int 31h                      ; Set Limit of Selector
L2:
   mov DLLDSSel,BX              ; save DLL DS Sel
   mov edi,DLLInterface         ; Init DLL Interface Area
   mov ax,000Ah
   int 31h                      ; Get Alias Descriptor
   jc XX
   mov [edi+12h],AX             ; CS Sel in INTERFACE
   mov BX,AX
   mov ax,DS
   mov [edi+18h],AX             ; Main DS Sel in INTERFACE
   mov ES,AX
   mov edi,offset DLLDescriptor
   mov AX,000bh
   int 31h                      ; Get CS Descriptor
   bts word ptr [DLLDescriptor+5],3
   bts word ptr [DLLDescriptor+5],14
   mov edi,offset DLLDescriptor
   mov ax,000Ch
   int 31h                      ; Set Descriptor
   mov edi,DLLInterface
   mov ax,[edi+12h]             ; 48 bit pointer needs CS selector
   add edi,36
   mov ecx,36
L3:
   mov [edi],ax
   add edi,6
   loop L3
   mov edi,DLLInterface                 ; Copy DLL Interface into Highmemory
   xor esi,esi
   mov ecx,3Fh
   mov ax,DLLDSSel
   mov es,ax
L4:
   mov eax,[edi+ecx*4]
   mov es:[esi+ecx*4],eax
   loop L4
   mov eax,[edi]
   mov es:[esi],eax
   mov ax,cs:[2ch]
   mov es:[2ch],ax              ; selector for DOS environment in DLL PSP
   mov edi,0100h                ; DLL Entry OFFSET is 0000100h
L5:
   mov DLLFileLoad,edi
   mov edi,offset intedi
   mov ax,ds
   mov es,ax                    ;es:edi points to intedi
   mov ax,[8]
   mov intds,ax
   mov intedx,0
   mov inteax,3f00h
   mov ebx,DLLfilehandle
   mov intebx,ebx
   mov intecx,16384
   mov bx,21h
   mov ax,0300h
   xor cx,cx
   int 31h
   mov ecx,inteax
   mov DLLReadNumber,cx
   inc cx
   inc cx
   inc cx
   shr cx,2
   mov edi,DLLfileload
   xor si,si
   mov ax,DLLDSSel
   mov es,ax
L6:
   mov eax,gs:[si]
   mov es:[edi],eax                     ; copy DLL into HighMemory
   add si,4
   add edi,4
   loop L6
   mov cx,DLLreadnumber
   cmp cx,16384
   je  L5
   mov ebx,DLLfilehandle
   mov ah,3eh
   int 21h                              ; close File
   pop es
   pop gs
   clc
   ret
XX:                         ; AX = 1 : Too less memory available
   mov ax,1
   pop es
   pop gs
.IF NOT DLL_ERROR
   stc
   ret
.ELSE
  .DATA
       DLLerror2     db     'ERROR allocating memory for DLL - abort ',0
  .CODE
      mov edi,offset DLLerror2
   .IFM SYSTEM.INC
      call SYSTEMWRITELN
   .ELSE
      call DLLPrint
      mov dx,1800h
      xor bx,bx
      mov ah,2
      int 10h
   .ENDIF
      mov ah,4ch
      int 21h
.ENDIF

X:
   xor ax,ax                                    ;AX=0 : Load Error
   pop es
   pop gs
.IF NOT DLL_ERROR
   stc
   ret
ENDP LoadDLL
.ELSE
   .DATA
       DLLerror     db     'ERROR loading DLL - abort ',0
   .CODE
      mov edi,offset DLLerror
   .IFM SYSTEM.INC
      call SYSTEMWRITELN
   .ELSE
      call DLLPrint
      mov dx,1800h
      xor bx,bx
      mov ah,2
      int 10h
   .ENDIF
      mov ah,4ch
      int 21h
      ret
ENDP LoadDLL

.IFM NOT SYSTEM.INC
DLLPrint        PROC       ; Print Message with offset in edi
     mov esi,24*160
     mov ah,14
L1:
     mov al,[edi]
     cmp al,0
     jne L3
     mov di,0
     mov ecx,25*160/4
L2:
     mov eax,es:[di+160]
     mov es:[di],eax
     add di,4
     loop L2
     ret
L3:
     mov es:[esi],ax
     inc edi
     add esi,2
     jmp L1
     ret
ENDP DLLPrint
.ENDIF ;  MODULE SYSTEM
.ENDIF ;  DLL ERROR
.NODEBUG
InitDLL        PROC     ; ESI : OFFSET OVLNAME
   mov ax,cs            ; 48 bit pointer needs CS selector
   push esi
   push edi
   mov edi,offset InitDLLAddress
   add esi,32
   mov cx,36
InitDLLheadercs:
   mov [esi+4],ax
   mov [esi],edi
   add esi,6
   loop InitDLLheadercs
   pop edi
   pop esi
   ret
InitDLLAddress db 0cbh
ENDP InitDLL

FreeDLL PROC           ;  ESI : OFFSET DLLNAME
   call InitDLL
   mov bx,[ESI+12h]    ; Free DLL - CS Descriptor
   mov ax,1
   int 31h
   cmp dword ptr [ESI+0Eh],0
   je FreeDOSDLL
   mov bx,[ESI]        ; Free DLL - DS Descriptor
   mov ax,1
   int 31h
   mov cx,[esi+14h]
   mov bx,[esi+16h]             ; BX:CX linear Address off DLL (ZeroSel Operation)
   mov SI,[esi+0ch]
   mov DI,[esi+0ah]
   mov ax,601h
   int 31h
   mov ax,0600h                 ; lock linear memory region
   int 31h
   mov bx,[ESI+0Eh]
   mov cx,[ESI+10h]
   mov ax,0502h
   int 31h             ; Free memory
   ret
FreeDOSDLL:
   mov ax,0101h
   mov dx,[ESI]        ; Free DLL - DS Descriptor and DOS Memory
   int 31h
   ret
ENDP FreeDLL

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

