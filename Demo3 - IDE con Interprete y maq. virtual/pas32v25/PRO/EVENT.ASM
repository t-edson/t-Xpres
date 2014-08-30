.data

      mesg1  db    'Ok, real mode call back installed!',0
      mesg2  db    'press left mouse button to test handler.',0
      mesg3  db    'press any key to return to real mode.',0
      mesg4  db    'Error installing the mouse handler!',0

      ; REAL MODE TRANSFER REGISTER BUFFER

.BLOCK

      intedi dd 0
      intesi dd 0
      intbp  dw 0
      intbph dw 0
      intddw dd 0
      intebx dd 0
      intedx dd 0
      intecx dd 0
      inteax dd 0
      intflg dw 0
      intes  dw 0
      intds  dw 0
      intfs  dw 0
      intgs  dw 0
      intip  dw 0
      intcs  dw 0
      intsp  dw 0
      intss  dw 0
      intqq  dw 0

.NOBLOCK

      ; Call Back Data

      CBSeg  DW 0
      CBOffs DW 0

.code


PROC MouseEventHandler              ; a real mode call back function

;  Parameters: ES : EDI = real mode register field
;              DS : ESI = real mode stack
;
;  a typical mouse event handler:
;
;  AX = ES:INTEAX = mouse event, which activated the handler
;  BX = ES:INTEBX = mouse button, which is actually pressed   (bit 0=left, bit 1=right, bit 2=middle)
;  CX = ES:INTECX = mouse position X
;  DX = ES:INTEDX = mouse position Y
;  DI = ES:INTEDI = rel mouse mouvement X
;  SI = ES:INTESI = rel mouse mouvement Y
;
;  note, read return address from stack (DS:ESI) and write address to INTCS
;  and INTIP!

       bt dword ptr  es:[inteax],1   ; event left key pressed
       jc short leftkeypressed
       bt dword ptr es:[inteax],2    ; event left key not pressed
       jc short leftkeynotpressed
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
       call Sound
       jmp EventExit
leftkeynotpressed:
       call NoSound
       jmp EventExit
ENDP MouseEventHandler


START:
    call clearscreen
; Check if mouse driver found
    mov ax,0
    int 33h
    cmp ax,0
    je MouseError
; OK - mouse driver present!
; Allocate call back function:
    push ds
    mov ax,ds
    mov es,ax
    mov ax,cs
    mov ds,ax
    mov esi,offset MouseEventHandler   ; DS : ESI = sel:offs to our handler
    mov edi,offset intedi              ; ES : EDI = sel:offs to real mode regs
    mov ax,303h                        ; DPMI Function 0303h
    int 31h                            ; get real mode call back
    pop ds
    jc DPMIError
    mov CBSeg,CX
    mov CBOffs,DX
; OK - CX:DX points to a real mode function, which will call our handler
    mov ax,ds
    mov es,ax
    mov intes,cx                       ; CX:DX is our real mode call back
    mov intedx,edx
    mov inteax,0ch                     ; install mouse event handler
    mov intecx,2+4                     ; events: left mouse button
    mov ax,300h                        ; DPMI Function 0300h
    mov bx,33h                         ; mouse interrupt
    mov cx,0
    mov edi,offset intedi
    int 31h                            ; set event handler
; OK - mouse event handler installed!
    mov edi, offset mesg1
    mov dx,0
    mov cx,0
    call print                         ; print message mesg1 - mesg3
    mov dx,1
    mov cx,0
    call print                         ; print message mesg1 - mesg3
    mov dx,2
    mov cx,0
    call print                         ; print message mesg1 - mesg3
    call getkey                        ; loop until keypressed!
; OK - now disable event handler!
    mov ax,0
    int 33h                            ; reset mouse driver
; OK - now free call back
    mov cx,CBSeg
    mov dx,CBOffs                       ; CX : DX Call Back Function
    mov ax,0304h                        ; DPMI Function 0304h
    int 31h                             ; Free Real Mode Call Back
MainEND:
    mov ax,0200h                        ; Set Cursor To line 4
    mov dx,400h
    mov bx,0
    int 10h
    mov ax,4c00h                        ; Free memory and Terminate
    int 21h

MouseError:
DPMIError:
    mov edi,offset mesg4
    mov dx,0
    mov cx,0
    call print
    jmp MAINEND



;------------------ Subroutines ------------------


Clearscreen PROC        ; ah: color
       mov ax,0720h     ; Space with attribute 7
       xor ebx,ebx
       mov bx,ax
       shl eax,10h      ; shift 16 bits
       add eax,ebx
       mov ecx,999     ; screen size div 4-1
@Clearscreenloop:
       mov es:[ecx*4],eax  ; es video selector
       add si,4
       loop @Clearscreenloop
       mov es:[ecx],eax
       ret
ENDP ClearScreen

Print           PROC  ;  cx : X, dx : Y, EDI: OFFSET Message
       push es
       mov ax,cs:[2]
       mov es,ax
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
       pop es
       ret
ENDP print

KeyPressed   PROC               ; Z Flag if No Keypressed
        mov ax,gs:[41Ah]        ; GS: ZERO Selector
        cmp ax,gs:[41Ch]
        ret
ENDP KeyPressed

GetKey      PROC                ; Key in AL
        call keypressed
        je getkey
        mov di,gs:[41Ah]        ; GS: ZERO Selector
        mov gs:[41Ch],di
        add di,400h
        mov al,gs:[di]
        ret
ENDP getkey

PROC Sound                      ; AX = frequency  -  turn speaker on
  mov    bx,ax
  mov    ax,34DDh
  mov    dx,12h
  cmp    dx,bx
  jnb short L2
  cmp bx,0
  je short L2
  div    bx
  mov    bx,ax
  in     al,61h
  test   al,3h
  jne short  L1
  or     al,3h
  out    61h,al
  mov    al,0B6h
  out    43h,al
L1:
  mov    al,bl
  out    42h,al
  mov    al,bh
  out    42h,al
L2:
  ret
ENDP Sound


PROC NoSound                    ; turn speaker off
  in     al,61h
  and    al,0FCh
  out    61h,al
  ret
ENDP NoSound


END START
END