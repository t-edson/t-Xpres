;Program   Check DPMI - Demo Program To Get DPMI Information
;Pass32 and Pro32 (c) 1996,1997 by Dieter Pawelczak
;Pass32 Assembler Source

.MODEL FLAT  ; Can only run in 32-bit Version

.DATA
        Mesg0   db   'CHECK DPMI VERSION written by Dieter Pawelczak',0
        Mesg1   db   'DPMI is 32 bit host',0
        Mesg2   db   'DPMI supports virtual memory',0
        Mesg3   db   'DPMI Version          : '
        High    db  '0'
                db  '.',0
        Mesg4   db   'Processor Type is     : '
                db   '80'
        CPUTYP  db   '3'
                db   '86',0
        mesg5   db   'Available Free Memory :',0
        mesg6   db   'Total Installed Memory:',0
        mesg7   db   'Free DOS Memory       :',0
        Pro32   db   'DPMI handled by Pro32 host',0

        error   db   'ERROR 80386 Processor detected - need 80486 to continue'
                db    0     ; Because Of FPU instructions in MathToString...
        mtstr   db   '                ',0

; Variables used by MathToString ...
        mathoutbcd     DT       0
        mathoutint     DD       0
        mathoutnull    DB       0
; Field Used for Memory
        MemAvail       DD       0
        MemGetNoLock   DD       0
        MemGetLcok     DD       0
        MemSizeInPages DD       0
        MemLockedPages DD       0
        MemFreePages   DD       0
        MemAllPages    DD       0
        MemLinPages    DD       0
        VMemFileSize   DD       0



.CODE
; --------------------- MAIN ---------------------
MAIN:
    call clearscreen
    mov dx,0
    mov cx,0            ; will be replaced by xor reg,reg if you use the -o
                        ; parameter !
    lea edi,Mesg0
    call print
    mov ax,0400h        ; get DPMI Version
    int 31h
    add cl,48
    mov CPUTYP,cl       ; alter Proctyp
    mov dx,1
    mov cx,0
    lea edi,Mesg4
    call print
    cmp CPUTYP,51
    ja CPU_TYPE_OK
    lea edi,Error
    mov dx,2
    mov cx,0
    call print
    jmp MAINEND
CPU_TYPE_OK:
    mov ax,0400h        ; get DPMI Version
    int 31h
    push ax
    push bx
    test bx,1
    je NO_32_BIT
    mov dx,2
    mov cx,0
    lea edi,Mesg1
    call print
NO_32_BIT:
    pop bx
    test bx,4
    je NO_Virtual_Memory
    mov dx,3
    mov cx,0
    lea edi,Mesg2
    call print
NO_Virtual_Memory:
    cmp byte ptr cs:[0fh],0   ; test if Pro32 DPMI is active: =0 Yes, =1,NO
    jne short NoPro32DPMI
    mov dx,3
    mov cx,0
    lea edi,Pro32
    call print

NoPro32DPMI:
    pop ax
    add ah,48
    mov High,ah
    mov ah,0
    push ax
    mov dx,4
    mov cx,0
    lea edi,Mesg3
    call print
    pop ax
    movzx eax,ax
    mov edi,offset mtstr
    call MathToString
    mov edi,offset mtstr
    mov cx,26
    mov dx,4
    call print            ; print MtStr ( LOW DPMI Version Number)
    mov ax,0500h
    push es               ; save ES
    push ds
    pop es
    mov edi,offset MemAvail
    int 31h
    pop es
    mov eax,MemAvail
    mov edi,offset mtstr
    call MathToString
    mov dx,5
    mov cx,0
    lea edi,Mesg5
    call print
    mov edi,offset mtstr
    mov cx,24
    mov dx,5
    call print            ; print MtStr ( Memavail )
    mov eax,MemAllPages
    shl eax,12
    mov edi,offset mtstr
    call MathToString
    mov dx,6
    mov cx,0
    lea edi,Mesg6
    call print
    mov edi,offset mtstr
    mov cx,24
    mov dx,6
    call print            ; print MtStr ( Memavail )
    mov dx,7
    mov cx,0
    lea edi,Mesg7
    call print
    mov ax,0100h
    mov bx,0ffffh         ; get free DOS Memory
    int 31h
    xor eax,eax
    mov ax,bx
    shl eax,4
    mov edi,offset mtstr
    call MathToString
    mov edi,offset mtstr
    mov cx,24
    mov dx,7
    call print            ; print MtStr ( Memavail )



MAINEND:
    mov ax,0200h                ; Set Cursor To line 8
    mov dx,800h
    mov bx,0
    int 10h
    mov ax,4c00h                ; Free memory and Terminate
    int 21h


;------------------ Subroutines ------------------


MathToString PROC       ;  32 bit (EAX) to BCD String OFFSET EDI
    cmp eax,0
    jne MathToStr@NoNil
    mov dx,48
    mov [edi],dx
    ret
MathToStr@NoNil:
    mov mathoutint,eax
    fild mathoutint
    fbstp mathoutbcd
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


