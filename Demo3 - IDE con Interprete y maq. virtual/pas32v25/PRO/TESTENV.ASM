.MODEL FLAT
.CODE
       mov ax,[2ch]              ; Descriptor for the environment
       mov ds,ax                 ; in DS
       mov ah,01eh
       call clearscreen          ; clear screen
       mov edi,0                 ; Offset to environment
       mov dx,0
@loop:
       push dx
       xor cx,cx
       mov ah,01eh
       call print
       pop dx
       inc dx
       mov ah,[edi]
       cmp ah, 0
       jne @loop
       mov ax,0200h                ; Set Cursor To last line of environment
       shl dx,8
       mov bx,0
       int 10h
       mov ax,4c00h                ; Free memory and Terminate
       int 21h

; Subroutines

Clearscreen PROC        ; ah: color
       mov al,32        ; Space
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

Print           PROC
   ; ah : Color, cx : X, dx : Y, EDI: OFFSET Message
       mov bh,ah
       mov ax,cs:[2]
       mov es,ax
       mov ax,160
       mul dx
       shl cx,1
       add ax,cx
       mov si,ax        ; Screen Offset
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

END