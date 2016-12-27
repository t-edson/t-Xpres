.COMMENT
 ****************************************************************************
 DOSX.INC       Emulation of extended DOS functions
                (c) 1996,1997,1998 by Dieter Pawelczak - Pass32 Version 2.0
 ============================================================================

                        =========================
                            DOSX Version 1.5
                        =========================

  To enable extended DOS functions you should include this module at the
  beginning of your source code:

                           .INCLUDE DOSX.INC

  Use with care in DLL / OVL modules: Basically extended DOS functions
  should be enabled by the main program!

 ****************************************************************************

.INCLUDE DPMI.INC                        ; DPMI STRUC
.DATA
.BLOCK
    INTOLD21                DD    ?      ; OFFSET OF DOS INTERRUPT
                            DW    ?      ; SELECTOR OF DOS INTERRUPT
    DOSXFunctionList        DB    9      ; FUNCTIONS WITH $ TERMINATED BLOCK
                                         ; DS : EDX
                            DB    3ch,5ah; FUNCTIONS WITH 0 TERMINATED BLOCK
                            DB    5bh,3dh; DS : EDX
                            DB    41h,43h,39h,3ah,3bh,4eh
                            DB    56h    ; FUNCTIONS WITH 0 TERMINATED BLOCK
                                         ; DS : EDX  AND ES : EDI
                            DB    40h    ; FUNCTION WITH SPECIFIC BLOCK SIZE
                                         ; IN CX - BLOCK IN: DS : EDX
                            DB    3fh    ; FUNCTION WITH SPECIFIC BLOCK SIZE
                                         ; IN AX - BLOCK OUT: DS : EDX
                            DB    47h    ; FUNCTION WITH OUTPUT THROUGH
                                         ; DS : ESI
                            DB    4bh    ; FUNCTIONS WITH 0 TERMINATED BLOCK
                                         ; DS : DX and BLOCK AT ES:BX
                            DB    2fh,34h; FUNCTION WITH OUTPUT
                                         ; ES : EBX
                            DB    48h    ; FUNCTION THROUGH INT 31h
                            DB    49h    ; FUNCTION THROUGH INT 31h
                            DB    1bh    ; FUNCTION WITH OUTPUT
                                  1ch,1fh,32h; DS : EBX
                            DB    25h    ; FUNCTION THROUGH INT 31h
                            DB    35h    ; FUNCTION THROUGH INT 31h
                            DB    1ah    ; FORCE ERROR
                            DB    00h    ; END OF LIST

    ; all other functions will be done by a simple DOS call ...

    DOSXInstalled           DB    0

    ; all DOS FUNCTIONS EXPECT DS TO POINT AT INITIAL DS ...
.NOBLOCK
.CODE
.IFPM
.NODEBUG
PROC New21Handler          ; DOS INTERRUPT HANDLER FOR EXTENDED DOS FUNCTIONS
   push esi
   push es
   push edi
   push ebx
   mov edi,0
L0:
   mov bh,cs:[edi + OFFSET DOSXFunctionList]
   cmp ah,bh
   je SHORT ExtendedDOS
   inc edi
   cmp bh,0
   jnz L0
   pop ebx
   pop edi
   pop es
   pop esi
.CONST
   DB 0EAh   ; JUMP
   XDOSJMPOFFS DD ?
   XDOSJMPSEL  DW ?
.CODE
ExtendedDOS:
   cmp edi,0
   jz XDOS0
   cmp edi,10
   jbe XDOS1
   cmp edi,11
   jbe XDOS2
   cmp edi,12
   jbe XDOS3
   cmp edi,13
   jbe XDOS4
   cmp edi,14
   jbe XDOS5
   cmp edi,15
   jbe XDOS6
   cmp edi,17
   jbe XDOS7
   cmp edi,18
   jbe XDOS8
   cmp edi,19
   jbe XDOS9
   cmp edi,23
   jbe XDOS10
   cmp edi,24
   jbe XDOS11
   cmp edi,25
   jbe XDOS12
   pop edi
   pop ebx
   pop es
   pop esi
   bts word ptr [esp+8],0
   iret
.NODEBUG ; You can only DEBUG FROM THIS POINT
XDOS0:   ; Function 09h = $ TERMINATED BLOCK IN DS : EDX
   push edx
   mov bx,cs:[6]           ; DOS FILE BUFFER
   mov es,bx
L1:
   mov bl,[edx]            ; Get Char ...
   mov es:[di],bl          ; Transfer to DOS MEM
   inc edx
   inc edi
   cmp bl,'$'
   jne L1
   cmp byte ptr [edx],'$'
   je L1
L2:
   mov ebx,[ESP+4]         ; old EBX
   call XDOSCALL
   pop edx
L3:
   pop ebx
   pop edi
   pop es
   test si,1
   jne SHORT L4
   btr word ptr [esp+12],0
   pop esi
   iret
L4:
   bts word ptr [esp+12],0
   pop esi
   iret
XDOS1:   ; Functions = 0 TERMINATED BLOCK IN DS : EDX
   push edx
   call XDOSCOPY
   jmp SHORT L2

XDOS2:   ; Functions = 0 TERMINATED BLOCK IN DS : EDX   and ES : EDI
   push edx
   push fs
   mov bx,es
   mov fs,bx
   call XDOSCOPY
   mov edi,[esp+12]   ; OLD EDI
   call XDOSCOPY2
   pop fs
   jmp  L2
XDOS3:   ; Functions with SPECIFIC BLOCKSIZE IN CX - BLOCK DS : EDX
   push edx
   push ecx
   movzx ecx,cx
   add cx,3
   shr ecx,2
   xor edi,edi
   mov bx,cs:[6]           ; DOS FILE BUFFER
   mov es,bx
L5:
   mov ebx,[edx+ecx*4]
   mov es:[edi+ecx*4],ebx
   cmp cx,0
   je short L6
   loop L5
L6:
   mov ebx,[edx]
   mov es:[edi],ebx
   pop ecx
   jmp L2
XDOS4:   ; Functions with SPECIFIC BLOCKSIZE RETURN IN AX - BLOCK DS : EDX
   mov ebx,[esp]
   push ecx
   push edx
   call XDOSCALL
   pop edx
   mov bx,cs:[6]           ; DOS FILE BUFFER
   mov es,bx
   push ax
   movzx ecx,ax
   shr ecx,2
   xor edi,edi
   test si,1
   jne SHORT L10
   cmp cx,0
   je short L8
L7:
   mov ebx,es:[edi]
   mov [edx],ebx
   add edi,4
   add edx,4
   loop L7
L8:
   pop cx
   and cx,3
   cmp cx,0
   je short L10
L9:
   mov bl,es:[edi]
   mov [edx],bl
   inc edi
   inc edx
   loop L9
L10:
   pop ecx
   jmp L3
XDOS5:   ; Functions with OUTPUT THROUGH DS : ESI
   push esi
   call XDOSCALL2
   pop esi
   mov bx,cs:[6]           ; DOS FILE BUFFER
   mov es,bx
   xor edi,edi
L11:
   mov bl,es:[edi]
   mov [esi+edi],bl
   inc edi
   cmp bl,0
   jnz L11
   jmp L3
XDOS6:   ; Functions = 0 TERMINATED BLOCK IN DS : EDX   and  BLOCK in ES : EBX
   jmp XDOS14
XDOS7:   ; Return Value in ES:BX
   call XDOSCALL
   movzx eax,es:intes   ; ES VALUE in EAX
   shl eax,4
   movzx ebx,word ptr es:intebx
   add ebx,eax
   pop edi
   pop eax
   pop es
   mov ax,cs:[4]
   mov es,ax         ; ES : Zero Flag
   pop esi
   btr word ptr [esp+8],0
   iret
XDOS8:
   mov ax,0100h
   int 31h
   jnc SHORT L14
L12:
   pop edi
   pop eax
   pop es
   pop esi
   bts word ptr [esp+8],0
   iret
L14:
   pop edi
   pop eax
   pop es
   pop esi
   btr word ptr [esp+8],0
   iret
XDOS9:
   mov ax,0101h
   mov dx,es
   int 31h
   jnc L14
   jmp SHORT L12
XDOS10:   ; Return Value in DS:BX
   call XDOSCALL
   movzx edi,es:intds   ; ES VALUE in EAX
   shl edi,4
   mov edx,es:intedx
   pop ebx
   movzx ebx,es:word ptr intebx
   add ebx,edi
   pop edi
   pop es
   mov si,cs:[4]
   mov es,si         ; ES : Zero Flag
   pop esi
   iret
XDOS11:
   movzx bx,al
   mov cx,ds
   mov ax,205h
   int 31h
   jmp L14
XDOS12:
   movzx bx,al
   mov ax,204h
   int 31h
   pop edi
   pop es
   mov es,cx
   pop esi
   iret
XDOS14:
   push fs
   xor ecx,ecx
   mov ax,cs:[6]
   mov fs,ax
L15:
   mov al,[edx+ecx]
   cmp al,0
   je short L16
   mov fs:[ecx+1],al
   inc ecx
   jmp L15
L16:
   mov byte ptr fs:[ecx+1],32
   inc ecx
   pop eax
   pop ebx
   pop edi
   pop es
L17:
   mov dl,es:[ebx]
   cmp dl,0
   je short L18
   mov fs:[ecx+1],dl
   inc ecx
   inc ebx
   jmp L17
L18:
   mov byte ptr fs:[ecx+1],13
   mov fs:[0],cl
   push eax
   push es
   mov ax,[8]
   mov intds,ax
   mov intes,ax
   mov intesi,0
   mov intflg,0
   mov ax,0300h
   xor cx,cx
   push ds
   pop es
   mov edi,offset intedi
   mov bx,2eh
   int 31h
   pop es
   pop fs
   pop esi
   iret
ENDP New21Handler
.NODEBUG
PROC XDOSCALL    ;          runs DOS CALL via function 300h
                 ;          DOS DX : OFFSET = 0
                 ;          DOS DI : OFFSET = 0100h
                 ;          DOS DS,ES : FILE BUFFER SEGMENT
   mov dx,cs:[0]              ; DS
   mov es,dx
   mov es:intebx,ebx
   mov es:intedx,0            ; STRING OFFSET   DX
   mov es:intedi,0100h        ; STRING OFFSET   DI
   mov es:intecx,ecx
   mov es:inteax,eax          ; FUNCTION
   mov bx,cs:[8]              ;
   mov es:intds,bx            ; DOS DS
   mov es:intes,bx            ; DOS ES
   mov edi,offset intedi
   xor cx,cx
   mov bx,21h
   mov ax,300h
   int 31h
   mov eax,es:inteax          ; get ERROR MESSAGE
   mov ecx,es:intecx
   mov si,es:intflg
   ret
ENDP XDOSCALL
PROC XDOSCALL2   ;          runs DOS CALL via function 300h
                 ;          DOS SI : OFFSET = 0000h
                 ;          DOS DI : OFFSET = 0100h
                 ;          DOS DS,ES : FILE BUFFER SEGMENT
   mov esi,edx
   mov dx,cs:[0]              ; DS
   mov es,dx
   mov es:intebx,ebx
   mov es:intedx,esi
   mov es:intedi,0100h        ; STRING OFFSET   DI
   mov es:intesi,0h           ; STRING OFFSET   SI
   mov es:intecx,ecx
   mov es:inteax,eax          ; FUNCTION
   mov bx,cs:[8]              ;
   mov es:intds,bx            ; DOS DS
   mov es:intes,bx            ; DOS ES
   mov edi,offset intedi
   xor cx,cx
   mov bx,21h
   mov ax,300h
   int 31h
   mov eax,es:inteax          ; get ERROR MESSAGE
   mov ecx,es:intecx
   mov si,es:intflg
   ret
ENDP XDOSCALL2

PROC XDOSCOPY    ; copy BLOCK with 0 Terminated String FROM DS:EDX
   mov bx,cs:[6]           ; DOS FILE BUFFER
   mov es,bx
   xor edi,edi
L1:
   mov bl,[edx]            ; Get Char ...
   mov es:[di],bl          ; Transfer to DOS MEM
   inc edx
   inc edi
   cmp bl,0
   jne L1
   ret
ENDP XDOSCOPY
PROC XDOSCOPY2    ; copy BLOCK with 0 Terminated String FROM ES:EDI
   mov edx,0100h  ; we assume ES already set to FILE BUFFER
                  ; FS with original ES value ...
L1:
   mov bl,fs:[edi]             ; Get Char ...  FS : OLD ES
   mov es:[edx],bl             ; Transfer to DOS MEM
   inc edx
   inc edi
   cmp bl,0
   jne L1
   ret
ENDP XDOSCOPY2

.NODEBUG         ; DO NOT DEBUG BEYOND THIS POINT !!!
                 ; RECURSIVE DEBUGGING WILL CAUSE AN STACK EXCEPTION
.ENDIF
PROC INITDOSX START
.IFPM
    cmp DOSXInstalled,0
    je short L1
    ret
L1:
    mov DOSXInstalled,1
    mov ax,0204h           ; get pm int vector
    mov bx,021h            ; int 21h
    int 31h
    mov word ptr INTOLD21 + 4,CX ; selector
    mov DS:XDOSJMPSEL,CX
    mov INTOLD21,EDX             ; offset
    mov DS:XDOSJMPOFFS,EDX
    mov cx,cs
    mov edx,OFFSET New21Handler
    mov ax,0205h           ; set pm int
    mov bx,021h            ; int 21h
    int 31h                ; set new interrupt
.ELSE
    .OUT DOSX will be ignored...
.ENDIF
    ret
ENDP INITDOSX
