.COMMENT
 ****************************************************************************
 EVENT2.ASM     Example file for a mouse event handler
                (c) 1996,1998 by Dieter Pawelczak - Pass32 Version 2.3
 ============================================================================
 ****************************************************************************

.include system.mac
.include dpmi.inc

.mem 1024
.nb
.data
    programname db  'c:\command.com',0
    parameter   db  0
    linestr     db  '            ',0
    backstr     db  '      :     ',0
    xvalue      dd  0
    yvalue      dd  0
    CBSeg       dw 0
    CBOffs      dw 0

.code

PROC MouseEventHandler

;  Parameters: ES : EDI = real mode register field
;              DS : ESI = real mode stack
;
;  a typical mouse event handler:
;
;  AX = ES:INTEAX = mouse event, which activated the handler
;  BX = ES:INTEBX = mouse button, which is actually pressed   (bit 0=left, bit 1=right, bit 2=middle)
;  CX = ES:INTECX = mouse position X
;  DX = ES:INTEDX = mouse position Y
;  DI = ES:INTEDI = rel mouse movement X
;  SI = ES:INTESI = rel mouse movement Y
;
;  note, read return address from stack (DS:ESI) and write address to INTCS
;  and INTIP!
;  note, ES:EDI must point to a valid register transfer buffer when exit
;  last note: you should use a different real mode transfer buffer for
;             event handler and main program (not like here the public
;             dpmi.inc)!

       bt dword ptr  es:[inteax],1   ; event left key pressed
       jc short leftkeypressed
       bt dword ptr es:[inteax],2    ; event left key not pressed
       jc short leftkeynotpressed
       bt dword ptr  es:[inteax],3
       jc short rightkeypressed
       bt dword ptr es:[inteax],4
       jc rightkeynotpressed
EventExit:
       cld
       lodsw                                    ; Get real mode ret IP
       mov     es:[intip], ax
       lodsw                                    ; Get real mode ret CS
       mov     es:[intcs], ax
       add    word ptr  es:[intsp], 4           ; correct real mode stack
       iret
leftkeypressed:
       mov ax,440
       call SystemSound
       jmp EventExit
leftkeynotpressed:
       call SystemNoSound
       jmp EventExit
rightkeypressed:
rightkeynotpressed:
       push ds                                  ; save stack and transfer
       push es                                  ; buffer pointer!
       push esi
       push edi

       mov ax,cs:[0]                            ; load ds
       mov ds,ax
       xor eax,eax
       mov ax,word ptr intecx
       mov xvalue,eax
       mov ax,word ptr intedx
       mov yvalue,eax
       mov edi,offset linestr
       call mathtostring
       mov edi,offset backStr
       mov cx,60
       mov dx,0
       call systemprint
       mov edi,offset linestr
       mov cx,68
       mov dx,0
       call systemprint
       mov eax,xvalue
       mov edi,offset linestr
       call mathtostring
       mov edi,offset linestr
       mov cx,62
       mov dx,0
       call systemprint

       pop edi                                  ; restore stack and
       pop esi                                  ; transfer buffer pointer!
       pop es
       pop ds
       jmp EventExit
ENDP MouseEventHandler




START:
    mov ax,0
    int 33h
    cmp ax,0
    je MouseError
    push ds
    mov ax,ds
    mov es,ax
    mov ax,cs
    mov ds,ax
    mov esi,offset MouseEventHandler
    mov edi,offset intedi
    mov ax,303h
    int 31h
    pop ds
    jc DPMIError
    mov CBSeg,CX
    mov CBOffs,DX
    mov ax,ds
    mov es,ax
    mov intes,cx
    mov intedx,edx
    mov inteax,14h
    mov intecx,2+4+8+16
    mov ax,300h
    mov bx,33h
    mov cx,0
    mov edi,offset intedi
    int 31h
    call systemclrscr
    color(14,0)
    writeln('Event Handler Installed - press mouse button!')
    write('Type EXIT to terminate demo!')
    mov ax,1
    int 33h                             ; show mouse
    color(7,0)
    writeln(' ')
    mov edi,offset programname
    mov esi,offset parameter
    call SystemExec
; OK - now disable event handler!
    mov ax,2
    int 33h                             ; hide mouse
    mov ax,0
    int 33h                             ; reset mouse driver
; OK - now free call back
    mov cx,CBSeg
    mov dx,CBOffs                       ; CX : DX Call Back Function
    mov ax,0304h                        ; DPMI Function 0304h
    int 31h                             ; Free Real Mode Call Back
    writeln('Exit demo!')
    exit(0);
DPMIError:
    writeln('Error installing the Event Handler!')
    exit(3);
MouseError:
    writeln('Mouse driver not present!')
    exit(3);
END START
END