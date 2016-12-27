.COMMENT
 ****************************************************************************
 MSDEMO.INC     Example file for protected mode mouse support
            *** This is a module - do not assemble !!!! ***
                (c) 1996,1998 by Dieter Pawelczak - Pass32 Version 2.0
 ============================================================================
 Function InitMouse:   Initializes pm mouse driver for COM1 or COM2
 Variable MSX:         current mouse X position
 Variable MSY:         current mouse Y position
 Variable MSLEFT:      status of left mouse button
 Variable MSRIGHT:     status of right mouse button
 Pointer  MSDRAW:      far pointer for mouse draw procedure;
                       called with every mouse movement
 ****************************************************************************
debugvideo      .equ 1
.PUBLIC .DATA
       MSX     DW    100    ; mouse x position
       MSY     DW    100    ; mouse y position
       MSMAXX  DW    640  ; max mouse x position
       MSMAXY  DW    400  ; max mouse y position
       MSLEFT  DB    0    ; left mouse button
       MSRIGHT DB    0    ; right mouse button
       MSDRAW  DF    ?    ; mouse draw procedure
.DATA
       M1      DB    65
       M2      DB    0
       M3      DB    0
       SYNC    DB    0
.CODE

PROC MSDrawDummy FAR
     ret
ENDP MSDrawDummy

PROC Com1Mouse FAR
    push eax
    push ebx
    push ecx
    push edx
    push esi
    push edi
    push ds
    mov ax,cs:[0]
    mov ds,ax
    inc sync
    mov dx,3f8h
    in al,dx
    test al,64
    je  short Com1NoSync
    mov sync,0
Com1NoSync:
    cmp sync,0
    jne short  Com1Sync1
    mov m1,al
Com1Sync1:
    cmp sync,1
    jne short  Com1Sync2
    mov m2,al
Com1Sync2:
    cmp sync,2
    jne Com1Sync3
    mov m3,al
    mov sync,0
    test m1,16
    je  short Com1MSright
    mov MSRight,1
    jmp short Com1Next2
Com1MSright:
    mov MSRight,0
Com1Next2:
    test m1,32
    je  short Com1MSLeft
    mov MSLeft,1
    jmp short Com1Next3
Com1MsLeft:
    mov MSLeft,0
Com1Next3:
    mov al,m1
    shl al,6
    add al,m2
    movsx ax,al
    add msx,ax
    mov al,m1
    and al,4+8
    shl al,4
    add al,m3
    movsx ax,al
    add msy,ax
    cmp m2,0
    jne Com1MSDraw
    cmp m3,0
    jne Com1MSDraw
    jmp short Com1Sync3
Com1MSDraw:
    call MSDRAW
Com1Sync3:
    pop ds
    pop edi
    pop esi
    pop edx
    pop ecx
    pop ebx
    mov al,20h
    out 20h,al
    pop eax
    iret
ENDP Com1Mouse

PROC Com2Mouse Far
    push eax
    push ebx
    push ecx
    push edx
    push esi
    push edi
    push ds
    mov ax,cs:[0]
    mov ds,ax
    inc sync
    mov dx,2f8h
    in al,dx
    test al,64
    je  short Com2NoSync
    mov sync,0
Com2NoSync:
    cmp sync,0
    jne short  Com2Sync1
    mov m1,al
Com2Sync1:
    cmp sync,1
    jne short  Com2Sync2
    mov m2,al
Com2Sync2:
    cmp sync,2
    jne Com2Sync3
    mov m3,al
    mov sync,0
    test m1,16
    je  short Com2MSright
    mov MSRight,1
    jmp short Com2Next2
Com2MSright:
    mov MSRight,0
Com2Next2:
    test m1,32
    je  short Com2MSLeft
    mov MSLeft,1
    jmp short Com2Next3
Com2MsLeft:
    mov MSLeft,0
Com2Next3:
    test m1,2
    je  short Com2AddXpos
    mov al,64
    sub al,m2
    movzx ax,al
    sub msx,ax
    jnc short  Com2XposEnd
    mov msx,0
    jmp short  Com2XposEnd
Com2AddXpos:
    movzx ax,m2
    add ax,msx
    mov msx,ax
    mov ax,msmaxx
    cmp msx,ax
    jb  short Com2XPosEND
    mov msx,ax
Com2XposEND:
    test m1,4
    je  short Com2AddYpos
    mov al,64
    sub al,m3
    movzx ax,al
    sub msy,ax
    jnc short  Com2YposEnd
    mov msy,0
    jmp short  Com2YposEnd
Com2AddYpos:
    movzx ax,m3
    add ax,msy
    mov msy,ax
    mov ax,msmaxy
    cmp msy,ax
    jb Com2yPosEND
    mov msy,ax
Com2YposEND:
    cmp m2,0
    jne Com2MSDraw
    cmp m3,0
    jne Com2MSDraw
    jmp short Com2Sync3
Com2MSDraw:
    call MSDRAW
Com2Sync3:
    pop ds
    pop edi
    pop esi
    pop edx
    pop ecx
    pop ebx
    mov al,20h
    out 20h,al
    pop eax
    iret
ENDP Com2Mouse


PROC InitMouseCom1 NEAR
    mov cx,cs
    mov edx,OFFSET Com1Mouse
    mov ax,0205h          ; set pm int
    mov bx,0ch            ; int 0ch
    int 31h               ; set new interrupt
    ret
ENDP InitMouseCom1

PROC InitMouseCom2 NEAR
    mov cx,cs
    mov edx,OFFSET Com2Mouse
    mov ax,0205h          ; set pm int
    mov bx,0bh            ; int 0bh
    int 31h               ; set new interrupt
    ret
ENDP InitMouseCom2

PROC InitMouse NEAR       ; Initializes Mouse,
                          ; when fails : Carry Flag set
    mov edx,OFFSET MSDrawDummy
    mov dword ptr msdraw,edx   ; install dummy proc for displaying pointer
    mov cx,cs
    mov word ptr  msdraw+4,cx  ; install dummy proc for displaying pointer
    mov ax,24h
    int 33h               ; DOS Mouse interrupt
    cmp cl,3              ; com2 ?
    jne NotCom2
    call InitMouseCom2
    clc
    ret
NotCom2:
    cmp cl,4
    jne NotCom1
    call InitMouseCom1
    clc
    ret
NotCom1:
    stc
    ret
ENDP InitMouse
