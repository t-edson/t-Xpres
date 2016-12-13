.COMMENT
 ****************************************************************************
 WINSYS.INC     Some useful functions for Win32 applications
                (c) 1999 by Dieter Pawelczak - Pass32 Version 3.0
 ============================================================================
 ****************************************************************************

GENERIC_READ   .EQU 80000000h
GENERIC_WRITE  .EQU 40000000h

FILE_SHARE_READ   .EQU   1
FILE_SHARE_WRITE  .EQU   2
FILE_SHARE_DELETE .EQU   4

CREATE_NEW        .EQU   1
CREATE_ALWAYS     .EQU   2
OPEN_EXISTING     .EQU   3
OPEN_ALWAYS       .EQU   4



.DATA?
   HeapOrg   DD 0
   HeapEnd   DD 0
   hInstance DD 0
   CMDLine   DD 0
   CMDLineLength DD 0



.CODE

.COMMENT *** Function GetParamStr Returns offset to parameter AL in EDI ***

GetParamStr PROC ;  RETURNS  EDI: String Offset to paramstr(AL)
                 ;                terminated with zero character
                 ;                CFLAG if No Paramstr (AL)
                 ;                AL = Max Number
    push ebp
    inc al
    mov bl,0
    mov esi,CMDLINE
    mov edi,esi
    mov ebp,esi
    add ebp,CMDLINELength
L1:
    mov cl,[esi]
    cmp cl,32 ; Ok First <Space>
    jne short L2
    inc esi
    inc edi
    jmp L1
L2:
    cmp cl,9 ; <TAB>
    jne short L3
    inc esi
    inc edi
    jmp L1
L3:
    mov cl,[esi]
    cmp cl,0
    je L4
    cmp cl,32
    je L4
    cmp cl,9
    jne L8
L4:
    xor cl,cl
    mov [esi],cl
    inc bl
    cmp al,bl
    je L9
    mov edi,esi
    inc edi               ; EDI Points to next character
L5:
    mov cl,[edi]
    cmp cl,32             ; next character Space as well ?
    jne L6
    inc esi
    inc edi
    jmp L5
L6:
    cmp cl,0
    jne L7
    inc esi
    inc edi
    jmp L5
L7:
    cmp cl,9
    jne short  L8
    inc esi
    inc edi
    jmp L5
L8:
    cmp esi,ebp
    jae Error
    inc esi
    jmp L3
L9:          ; Yes ParamStr detected and END marked with zero
    clc
    pop ebp
    ret
Error:          ; Carriage Return Detected!
    mov al,bl
    dec al
    pop ebp
    stc
    ret
ENDP GetParamstr

PROC SYSTEMMAIN START  ; This procedure will initialize the system unit
     pushad                          ; save all registers ...
     mov ebx,lastdata                ; install HEAP
     mov HeapOrg,ebx
     mov ecx,memsize
     sub ecx,4                       ; number of bytes on the heap
     mov HeapEnd,ecx
     sub ecx,ebx                     ; First Heap Entry:
     and ecx,0fffffffch              ; mark this pointer as free
     mov dword ptr [ebx],ecx         ; push value on the heap
     mov eax,ecx
     push 0
        .invoke .win32 'KERNEL32.GetModuleHandleA'
     mov hInstance,eax
        .invoke .win32 'KERNEL32.GetCommandLineA'
     mov CMDLine,eax
     xor ebx,ebx
L0:
     mov cl,[eax+ebx]
     cmp cl,0
     je short L1
     inc ebx
     jmp L0
L1:
     mov CMDLineLength,ebx
     popad                           ; restore all registers ...
     ret
ENDP SYSTEMMAIN
