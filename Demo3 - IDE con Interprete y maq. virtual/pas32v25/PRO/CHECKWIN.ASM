;Program   CheckWin Demo Program which tests if Windows is active
;Pass32 and Pro32 (c) 1996,1997 by Dieter Pawelczak
;Pass32 Assembler Source

.MODEL FLAT  ; Can only run in 32-bit Version

.DATA
        Mesg0   db   'Win Check - (c) 1997 by Dieter Pawelczak',0
        Mesg1   db   'Windows is activ.',0
        Mesg2   db   'No Windows present.',0

.TYPE (FALSE, TRUE)

.NM

.CODE
; --------------------- MAIN ---------------------
MAIN:
    call clearscreen
    mov dx,0
    mov cx,0
    lea edi,Mesg0
    call print
    lea edi,Mesg1
    cmp byte ptr cs:[0eh], TRUE
    je short NoWin
    lea edi,Mesg2
NoWin:
    mov dx,2
    mov cx,0
    call print
MAINEND:
    mov ax,0200h                ; Set Cursor To line 4
    mov dx,400h
    mov bx,0
    int 10h
    mov ax,4c00h                ; Free memory and Terminate
    int 21h


;------------------ Subroutines ------------------


Print           PROC  ;  cx : X, dx : Y, EDI: OFFSET Message
       mov ax,160
       mul dx
       shl cx,1
       add ax,cx
       mov si,ax        ; Screen Offset
       mov bh,14
printloop:
       mov bl,[edi]
       cmp bl,0
       je printende
       mov es:[si],bx
       add si,2
       inc edi
       jne printloop
printende:
       inc edi
       ret
ENDP print


Clearscreen PROC        ; ah: color
       mov ax,0720h     ; Space with attribute 7
       xor ebx,ebx
       mov bx,ax
       shl eax,10h      ; shift 16 bits
       add eax,ebx
       mov ecx,1000     ; screen size div 4
       mov si,0
@Clearscreenloop:
       mov es:[si],eax  ; es video selector
       add si,4
       loop @Clearscreenloop
       ret
ENDP ClearScreen
END MAIN                ; Entry Point
END


