;Program   Hello - Demo Program To Display Hello Message
;Pass32 and Pro32 (c) 1996,1997 by Dieter Pawelczak
;Pass32 Assembler Source

.MODEL FLAT
.DATA

hallomsg       db   '  ษออออออออออออออออออออออออออออออออออป  ',0
               db   '  บ     H e l l o   W o r l d        บ  ',0
               db   '  ศออออออออออออออออออออออออออออออออออผ  ',0
protmsg        db   '  ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป  ',0
               db   '  บ           The Processor Is Now In Protected Mode ...           บ  ',0
               db   '  ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ  ',0
generalmsg     db   '  ษออออออออออออออออออออออออออออออออออป  ',0
               db   '  บ     General Information          บ  ',0
               db   '  บ ================================ บ  ',0
               db   '  บ Processor Type: 80x86            บ  ',0
               db   '  บ Free Memory   :                  บ  ',0
               db   '  บ Basis Address :                  บ  ',0
               db   '  ศออออออออออออออออออออออออออออออออออผ  ',0
keymsg         db   'PRESS [ESC] to return to real mode .... ',0
Freememstr     db   '              ',0
Basisaddress   db   '              ',0
mathoutbcd     DT       0
mathoutint     DD       0
mathoutnull    DB       0
FreeMemory     DB 30h DUP(0)

color          .EQU   17h      ; dark blue and grey
colorhallomsg  .EQU   2fh      ; dark green and white
protcolor      .EQU   3fh      ; dark cyan and white
generalcolor   .EQU   5fh      ; dark purple and white

.CODE

START:
       mov ah,color
       CALL clearscreen
       mov ah,colorhallomsg
       mov cx,20
       mov dx,2
       mov edi,offset hallomsg
       call print
       mov ah,colorhallomsg
       mov cx,20
       mov dx,3
       call print
       mov ah,colorhallomsg
       mov cx,20
       mov dx,4
       call print
       mov ah,protcolor
       mov cx,5
       mov dx,6
       mov edi,offset protmsg
       call print
       mov ah,protcolor
       mov cx,5
       mov dx,7
       call print
       mov ah,protcolor
       mov cx,5
       mov dx,8
       call print
       mov edi,offset generalmsg
       mov dx,11
generalmsgloop:
       mov ah,generalcolor
       mov cx,20
       push dx
       call print
       pop dx
       inc dx
       cmp dx,18
       jne generalmsgloop
       mov edi,offset freememory
       mov ax,ds
       mov es,ax
       mov ax,0500h                     ; Get free memory
       int 31h
       mov eax,dword ptr freememory
       mov edi,offset freememstr
       call mathtostring
       mov edi,offset freememstr
       mov dx,15
       mov cx,40
       mov ah,generalcolor
       call print
       mov ax,0400h                      ; Get version
       int 31h
       add cl,48                         ; Processor type in cl to ascii
       mov ax,cs:[2]                     ; load PSP[2] = video selector
       mov es,ax
       mov es:[14*160+84],cl
       mov bx,cs
       mov ax,0006h                       ; get Basis Address
       int 31h
       movzx eax,cx
       shl eax,16
       or ax,dx
       mov edi,offset basisaddress
       call mathtostring
       mov edi,offset basisaddress
       mov dx,16
       mov cx,40
       mov ah,generalcolor
       call print
       mov ah,color
       mov dx,24
       mov cx,0
       mov edi,offset keymsg
       call print
@keyloop:
       call getkey
       cmp al,27
       jne @keyloop
       mov ah,4ch
       int 21h                            ; return to DOS


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

Print           PROC    ; ah : Color, cx : X, dx : Y, EDI: OFFSET Message
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

MathToString PROC       ;  32 bit (EAX) to BCD String OFFSET EDI
       cmp eax,0
       jne MathToStr@NoNil
       mov dx,48
       mov [edi],dx
       ret
MathToStr@NoNil:
        mov mathoutint,eax
        fild mathoutint         ; load integer in FPU
        fbstp mathoutbcd        ; store BCD in memory
        mov cx,9
        mov mathoutnull,0
        mov al,byte ptr mathoutbcd+9
        and al,128
        cmp al,0
        je MathtoString@nosign
        mov dx,'-'
        mov [edi],dx
        inc edi
MathtoString@nosign:
        mov cx,8
        xor dx,dx
        mov esi,offset mathoutbcd+7
MathToStrConvert:
        mov al,[esi]
        shr ax,4
        and al,0fh
        cmp al,0
        je MathToStr@next
        mov mathoutnull,1
MathToStr@next:
        cmp mathoutnull,0
        je MathToStr@next2
        mov dl,al
        add dl,48
        mov [edi],dx
        inc edi
MathToStr@next2:
        mov al,[esi]
        and al,0fh
        cmp al,0
        je MathToStr@next3
        mov mathoutnull,1
MathToStr@next3:
        cmp mathoutnull,0
        je MathToStr@next4
        cmp cx,2
        jne MathToStr@NoT2
        mov dl,','
        mov [edi],dx
        inc edi
MathToStr@NoT2:
        mov dl,al
        add dl,48
        mov [edi],dx
        inc edi
        cmp cx,4
        jne MathToStr@NoT1
        mov dl,','
        mov [edi],dx
        inc edi
MathToStr@NoT1:
MathToStr@next4:
        dec esi
        dec cx
        cmp cx,0
        jne MathToStrConvert
        ret
ENDP MathToString

KeyPressed   PROC               ; Z Flag if No Keypressed
        mov ax,gs:[41Ah]        ; GS: ZERO Selector
        cmp ax,gs:[41Ch]
        ret
ENDP KeyPressed

GetKey      PROC                ; Key in AL
getkey@Loop:
        call keypressed
        je getkey@loop
        mov di,gs:[41Ah]        ; GS: ZERO Selector
        mov gs:[41Ch],di
        add di,400h
        mov al,gs:[di]
        ret
ENDP getkey

END START                       ; Program Entry
END
