;DEBUG.INC - SYSTEM FILE USED BY THE PASS32 ASSEMBLER
;Attention - you should not alter this file -
;            unless you know what you're doing!
;You can use an individual debug file with the .DEBUGFILE directive
;With the .DEBUG option the procedure DEBUGPROC is called before
;each instruction.
;(c) 1996,1997 by Dieter Pawelczak - Pass32 Version 1.8

.DATA

.BLOCK
 debugregs         db  'EAX: 00000000 EBX: 00000000 ECX: 00000000 EDX: 00000000 EDI: 00000000  SP: 0000 '
 debugregs2        db  'ESI: 00000000 CS: 0000 DS: 0000 ES: 0000 FS: 0000 GS: 0000 F:0000 EBP: 0000 '
 debuginfo         db  '             : line :      :                                                        ',0;
 debughex          db  '0123456789ABCDEF'
 debugdump         db  '         :                                                 :                    '
.NOBLOCK
 debugcheckkey     db  0
 debugvideoseg     dw  0
 debughold         db  0
 debuginputstring  db  '                        ',0
 debuglineinfo0    db  '     0:                 ',0
 debugbreakpt      dd  0
 debugdisplay      db  0


.DATA?
 DebugVideoBuffer    db 160*4 DUP(?)
 regeax            dd  ?        ; register block
 regebx            dd  ?
 regecx            dd  ?
 regedx            dd  ?
 regedi            dd  ?
 regesi            dd  ?
 regebp            dd  ?
 regesp            dd  ?
 reges             dw  ?
 regds             dw  ?
 debuginfoOffset   dd  ?        ; entry point
 debugflags        dd  ?
 debugOffset       dd  ?
 debugdumpaddr     dd  ?
 debugcursor       dw  ?

.CODE
.NODEBUG

DEBUGPROC PROC
; ==============================================
; Initialize DS and save all registers and flags
; ==============================================

   push ds                      ; save old DS
   push ax
   mov ax,CS:[0]                ; load AX with CS:[0] = PSP DS Selector
   mov ds,ax                    ;                     = DLL DS Selector
   pop ax
   mov regeax,eax               ; save all registers
   mov regedi,edi
   mov regesi,esi
   mov regebx,ebx
   mov regecx,ecx
   mov regedx,edx
   mov regebp,ebp
   mov ax,es
   mov reges,ax
   pop es                       ; old DS
   mov ax,es
   mov regds,ax
   pop ebx                      ; entry address
   pop eax                      ; offset of debug information
   mov regesp,esp
   mov debuginfooffset,eax
   push eax
   mov debugoffset,ebx
   push ebx
   pushf                        ; save flags
   pop eax                      ;
   mov debugflags,eax
   push eax
   sub debugoffset,0ah          ; correct the push and call debugproc instr.
debugbegin:

; ===========================================================
; check break point, next instruction for RET/JMP instruction
; ===========================================================
   mov debugdisplay,0
   mov edi,debugoffset
   cmp edi,debugbreakpt
   jne debugnobreakpt
   jmp short debugrundebug
debugnobreakpt:
   cmp debugcheckkey,3
   je debugnof5;       SKIP DEBUG
   mov al,[edi+0ah]
   cmp debugcheckkey,2
   je short debugnextjmp
   cmp al,0c2h        ; RET / RETF INSTRUCTION
   je short debugrundebug
   cmp al,0c3h
   je short debugrundebug
   cmp al,0cbh
   je short debugrundebug
   cmp al,0cah
   je short debugrundebug
   cmp debugcheckkey,1
   je debugnof5;       SKIP DEBUG
debugnextjmp:
   cmp al,0e8h        ; CALL / JMP INSTRUCTION
   je short debugrundebug
   cmp al,0ffh
   je short debugrundebug
   cmp al,0ebh
   je short debugrundebug
   cmp al,0e9h
   je short debugrundebug
   cmp debugcheckkey,0
   jne debugnof5;       SKIP DEBUG
debugrundebug:

; =====================================================
; save video mode information if debugvideo is enabled
; =====================================================

.IF debugvideo                  ; VIDEO DISPLAY SWAPPING
   cmp debugvideoseg,0
   jne debugvideoinstalled
   mov ax,100h
   mov bx,4096                  ; get a 64 KBytes real mode buffer...
   int 31h
   jc debugvideoswapend
   mov debugvideoseg,ax
debugvideoinstalled:
   ; STORE VIDEO ...
   mov inteax,1c01h
   mov intecx,7
   xor ebx,ebx
   mov intebx,ebx
   mov intedx,ebx
   mov ax,debugvideoseg
   mov intes,ax
   mov ax,cs:[0]
   mov es,ax
   mov edi,offset intedi
   mov ax,300h
   xor cx,cx
   mov bx,10h
   int 31h                      ; store video info in buffer
   mov ah,0fh
   int 10h
   cmp al,3
   je debugvideoswapend
   mov ax,3
   int 10h
debugvideoswapend:
.ENDIF
; =====================================================
; store screen information and display string debugregs
; =====================================================
   mov ax,CS:[2]                ; promode psp : VideoSelector
   mov es,ax
   mov ecx,81
   mov edx,offset debugvideobuffer-2
   mov esi,3360-2
debugfillbuffer:
   mov ax,es:[2*ecx+esi]
   mov [edx+2*ecx],ax
   loop debugfillbuffer
   mov si,3520
   mov ecx,240
   mov edi,offset debugregs     ; display debug message
   mov edx,offset debugvideobuffer+160
debugprintloop:
   mov ax,es:[si]
   mov [edx],ax
   mov al,[edi]
   mov ah,1eh
   mov es:[si],ax
   inc edi
   add si,2
   add edx,2
   loop debugprintloop
; ============================================
; display all registers and selector registers
; ============================================
   mov eax,regeax               ; display old EAX register
   mov si,3530                  ; screen offset = line * 160 + 2 * column
   call debughexout             ; display hex number in EAX
   mov eax,regebx               ; display old EBX register
   add si,14
   call debughexout
   mov eax,regecx               ; display old ECX register
   add si,14
   call debughexout
   mov eax,regedx               ; display old EDX register
   add si,14
   call debughexout
   mov eax,regedi               ; display old EDI register
   add si,14
   call debughexout
   mov eax,regesp               ; display old ESP register
   add si,14
   call debughexout4
   mov si,3690
   mov eax,regesi               ; display old ESI register
   call debughexout
   add si,12
   mov ax,cs
   call debughexout4            ; display CS Selector
   add si,12
   mov ax,regds                 ; display DS Selector
   call debughexout4
   add si,12
   mov ax,reges                 ; display ES Selector
   call debughexout4
   add si,12
   mov ax,fs                    ; display FS Selector
   call debughexout4
   add si,12
   mov ax,gs                    ; display GS Selector
   call debughexout4
   mov eax,debugflags           ; display Flags
   add si,8
   call debughexout4
   add si,14
   mov eax,regebp                  ; display (E)SP
   call debughexout
; ============================================
;   display EIP and Source Code Information
; ============================================
   mov si,3840                  ; screen offset
   mov eax,debugoffset          ; old EIP
   call debughexout             ; display current EIP
   mov si,3870                  ; screen offset
   mov edi,debuginfooffset      ; display Source Code information
debugprintloop2:
   mov al,[edi]
   cmp al,0
   je debugprintloopende
   mov es:[si],al
   inc edi
   add si,2
   cmp si,4000
   jne debugprintloop2
debugprintloopende:
; ==================
; check break point
; ==================
   mov edi,debugoffset
   cmp edi,debugbreakpt
   jne debugnobreakpt2
   mov al,'*'
   mov es:[3858],al
debugnobreakpt2:

; ==============================================
; check dump hold mode - if enabled display dump
; ==============================================
debugholddump:
  ; ===========================================================
  ; display source code
  ; ===========================================================

.IFM DLOADS.INC
.IFMM
   call DISPLAYSCREENSTORE
   mov edi,debugoffset
   call DISPLAYDEBUGSOURCE
   mov debugdisplay,1
.ENDIF
.ENDIF

   cmp debughold,0
   jz debugkey
   mov edi,debugdumpaddr
   jmp debugdisplaydump
; ==============================================
; get key and react according to the key pressed
; ==============================================
debugkey:
   mov debugcheckkey,0
   mov ah,7                     ; Wait for a keystroke
   int 21h
   cmp al,0
   je debugkey
   cmp al,2    ; [CRTL]+[B]
   jne debugnostrb
   mov edi,debugoffset
   cmp debugbreakpt,edi
   je debugdelbreakpt
   mov debugbreakpt,edi
   mov al,'*'
   mov es:[3858],al
   jmp debugkey
debugdelbreakpt:
   mov debugbreakpt,0
   mov al,' '
   mov es:[3858],al
   jmp debugkey
debugnostrb:
   cmp al,8
   jne debugnobckspc
   mov debugcheckkey,3

debugnobckspc:
   cmp al,'a'
   jne short debugnotakey
   mov edi,regeax
   jmp debugdisplaydump
debugnotakey:
   cmp al,'b'
   jne short debugnotbkey
   mov edi,regebx
   jmp debugdisplaydump
debugnotbkey:
   cmp al,'c'
   jne short debugnotckey
   mov edi,regecx
   jmp debugdisplaydump
debugnotckey:
   cmp al,'d'
   jne short debugnotdkey
   mov edi,regedx
   jmp debugdisplaydump
debugnotdkey:
   cmp al,'e'
   jne short debugnotekey
   mov edi,regedi
   jmp debugdisplaydump
debugnotekey:
   cmp al,'f'
   jne short debugnotfkey
   mov edi,regesi
   jmp debugdisplaydump
debugnotfkey:
   cmp al,'h'
   jne short debugnothkey
   mov edi,debugdumpaddr
   mov debughold,1
   jmp debugdisplaydump
debugnothkey:
   cmp al,72
   jne short debugnocursorup
   mov edi,debugdumpaddr
   sub edi,10h
   jmp debugdisplaydump
debugnocursorup:
   cmp al,80
   jne short debugnocursordown
   mov edi,debugdumpaddr
   add edi,10h
   jmp debugdisplaydump
debugnocursordown:
   cmp al,73
   jne short debugnocursorpgup
   mov edi,debugdumpaddr
   sub edi,100h
   jmp debugdisplaydump
debugnocursorpgup:
   cmp al,81
   jne short debugnocursorpgdown
   mov edi,debugdumpaddr
   add edi,100h
   jmp debugdisplaydump
debugnocursorpgdown:
   cmp al,'A'
   jne short debugnotSAkey
   mov si,3530                  ; current register value
   mov edi,offset regeax        ; register to alter
   mov dx,1605h                 ; cursor position
   jmp debugalterreg
debugnotSAkey:
   cmp al,'B'
   jne short debugnotSBkey
   mov si,3558                  ; current register value
   mov edi,offset regebx        ; register to alter
   mov dx,1613h                 ; cursor position
   jmp debugalterreg
debugnotSBkey:
   cmp al,'C'
   jne short debugnotSCkey
   mov si,3586                  ; current register value
   mov edi,offset regecx        ; register to alter
   mov dx,1621h                 ; cursor position
   jmp debugalterreg
debugnotSCkey:
   cmp al,'D'
   jne short debugnotSDkey
   mov si,3614                  ; current register value
   mov edi,offset regedx        ; register to alter
   mov dx,162Fh                 ; cursor position
   jmp debugalterreg
debugnotSDkey:
   cmp al,'E'
   jne short debugnotSEkey
   mov si,3642                  ; current register value
   mov edi,offset regedi        ; register to alter
   mov dx,163dh                 ; cursor position
   jmp debugalterreg
debugnotSEkey:
   cmp al,'F'
   jne short debugnotSFkey
   mov si,3690                  ; current register value
   mov edi,offset regesi        ; register to alter
   mov dx,1705h                 ; cursor position
   jmp debugalterreg
debugnotSFkey:
   cmp al,8
   je debugnokey
   cmp al,32
   je debugnokey
   cmp al,3
   je debugnokey
   cmp al,63
   je debugnokey
   cmp al,27
   je debugnokey
   cmp al,13
   je debugnokey
   jmp debugkey
debugnokey:
   cmp al,27                    ; [ESC]  RUN UNTIL RET/RETF INSTRUCTION
   jne short  debugnoesc
; ==============================================
;   enable run until ret/retf instruction mode
; ==============================================
   mov debugcheckkey,1
debugnoesc:
   cmp al,13                    ; [RETURN] RUN UNTIL NEXT CALL/JMP
   jne short debugnoret               ;          INSTRUCTION
; ==============================================
;   enable run until call/jmp instruction mode
; ==============================================
   mov debugcheckkey,2
debugnoret:
  push ax
; ==============================
;  restore old screen contents
; ==============================
   mov esi,3360-2
   mov ecx,321
   mov edx,offset debugvideobuffer-2
debugprintloop3:
   mov ax,[edx+2*ecx]
   mov es:[esi+2*ecx],ax
   loop debugprintloop3
; ===================================================
; restore screen - if debug source had been displayed
; ===================================================

.IFM DLOADS.INC
.IFMM
   cmp debugdisplay,0
   je debugnorestorescreen
   call DISPLAYSCREENRESTORE
debugnorestorescreen:
.ENDIF
.ENDIF


; ===============================================
; restore old video mode - if debug video enabled
; ===============================================
.IF DEBUGVIDEO
   cmp debugvideoseg,0
   je debugvideonotinstalled
   ; VIDEO Back
   mov inteax,1c02h
   mov intecx,7
   xor ebx,ebx
   mov intebx,ebx
   mov intedx,ebx
   mov ax,debugvideoseg
   mov intes,ax
   mov ax,cs:[0]
   mov es,ax
   mov edi,offset intedi
   mov ax,300h
   xor cx,cx
   mov bx,10h
   int 31h
debugvideonotinstalled:
.ENDIF
; =======================================
;  check key for [CRTL]+[C], [f1], [f5]
; =======================================
   pop ax
   cmp al,3                     ; [CRTL]+[C]
   je debugabort
   cmp al,63 ; [F5]
   jne short debugnof5
   mov ah,7
   int 21h
   jmp debugbegin
debugnof5:
; ================================================
; in case of debug mode 1,2,3 check keyboard
; ================================================
   cmp debugcheckkey,0
   je debugrestoreall
   mov ax,cs:[4]                ; Load Zero Selector ...
   mov es,ax
   mov ax,es:[41Ah]             ; ES: ZERO Selector
   cmp ax,es:[41Ch]
   je debugrestoreall
   mov debugcheckkey,0
   mov ah,7
   int 21h
; =======================================
;    restore all registers and flags
; =======================================
debugrestoreall:
   mov ax,reges                 ; restore all registers
   mov es,ax
   mov edi,regedi
   mov esi,regesi
   mov eax,regeax
   mov ebx,regebx
   mov ecx,regecx
   mov edx,regedx
   mov edi,regedi
   mov esi,regesi
   mov ebp,regebp
   push ax
   mov ax,regds
   mov ds,ax
   pop ax
   popf
; =======================================
;  return and execute next instruction
; =======================================
   ret 4        ;
; ===================
;   abort debugging
; ===================
debugabort:     ; [CRTL]+[C] pressed - abort debugging
   mov ah,4ch
   int 21h
; =======================================
;  display hex dump at the offset of EDI
; =======================================
debugdisplaydump: ; displays HEX DUMP at offset edi
   mov debugdumpaddr,edi
   mov esi,3360-2
   mov edx,offset debugdump-2
   mov ecx,80
   mov ah,11
debugclearlineloop:
   mov al,[edx+ecx]
   mov es:[esi+2*ecx],ax
   loop debugclearlineloop
   mov si,3360
   mov eax,edi
   call debughexout
   mov edx,ebx
   xor ecx,ecx
   mov si,3384
   mov di,3484
debugdumploop:
   mov ebx,edx
   add ebx,ecx
   cmp MEMSIZE,ebx
   mov al,0ffh
   jb debugdumpok
   mov al,[edx+ecx]
debugdumpok:
   mov ah,'.'
   cmp al,31
   jb debugdisplaydumpnext
   mov ah,al
debugdisplaydumpnext:
   mov es:[di],ah
   mov ah,al
   shr al,4
   xor ebx,ebx
   mov bl,al
   mov al,[OFFSET DEBUGHEX+EBX]
   mov es:[si],al
   mov al,ah
   and al,0fh
   mov bl,al
   mov al,[OFFSET DEBUGHEX+EBX]
   mov es:[si+2],al
   add si,6
   add di,2
   inc cx
   cmp cx,16
   jne debugdumploop
   jmp debugkey
; ==========================================================
;  alter hex value to [EDI], SI : Screen Offset of old value
; ==========================================================
debugalterreg:  ; to alter Registers
   push si
   push edi
   push edi     ; offset of register address
   push dx      ; save cursor position
   mov ecx,8
   mov edi,offset debuginputstring
debuggetoldvalue:
   mov al,es:[si]
   mov [edi],al
   inc edi
   add si,2
   loop debuggetoldvalue
   mov ah,3
   xor ebx,ebx
   int 10h      ; get old cursor position
   mov debugcursor,dx
   pop dx
   mov ah,2
   int 10h      ; set cursor for input
   mov edi,offset debuginputstring
   xor ecx,ecx
debuginput:
   mov ah,7
   int 21h
   cmp al,0
   jne debuginput2
   mov ah,7
   int 21h
   jmp debuginput
debuginput2:
   cmp al,13
   je debuginputend2
   cmp al,8
   jne debuginput3
   cmp cx,0
   je debuginput
   mov dl,8
   mov ah,2
   int 21h
   mov dl,48
   dec cx
   mov [edi+ecx],dl
   mov ah,2
   int 21h
   mov dl,8
   mov ah,2
   int 21h
   jmp debuginput
debuginput3:
   cmp al,27
   jne debuginput4
   pop edi
   jmp short debuginputabort
debuginput4:
   cmp al,48
   jb debuginput
   cmp al,58
   ja debuginputnext3
   jmp debuginputend
debuginputnext3:
   and al,223   ; eliminate small letters
   cmp al,'A'
   jb debuginput
   cmp al,'F'
   ja debuginput
debuginputend:
   mov [edi+ecx],al
   mov dl,al
   mov ah,2
   int 21h
   inc cx
   cmp cx,8
   jb debuginput
debuginputend2:
; ==================================
;  convert hex string to hex value
; ==================================
   mov edi,offset debuginputstring
   mov ecx,28
   xor ebx,ebx
debugconvertloop:
   xor eax,eax
   mov al,[edi]
   sub al,48   ; ascii to dec
   cmp al,11
   jb debugconvert
   sub al,7
debugconvert:
   shl eax,cl
   or ebx,eax
   inc edi
   sub cl,4
   jns debugconvertloop
   pop edi
   mov [edi],ebx
debuginputabort:
   pop edi
   pop si
   mov eax,[edi]
   call debughexout
   mov dx,debugcursor
   mov ah,2
   xor ebx,ebx
   int 10h     ; restore old cursor
   jmp debugkey
ENDP DEBUGPROC

DEBUGHEXOUT PROC                ; Hexadecimal output of EAX at ES:[SI]
   mov ebx,eax
   mov cl,28
DEBUGHEXOUTLOOP:
   mov eax,ebx
   shr eax,cl
   and eax,0fh
   mov edi,offset DEBUGHEX
   add edi,eax
   mov al,[edi]
   mov es:[si],al
   add si,2
   sub cl,4
   cmp cl,0
   jne DEBUGHEXOUTLOOP
   mov eax,ebx
   and eax,0fh
   mov edi,offset DEBUGHEX
   add edi,eax
   mov al,[edi]
   mov es:[si],al
   ret
ENDP DEBUGHEXOUT

DEBUGHEXOUT4 PROC               ; Hexadecimal output of AX at ES:[SI]
   mov bx,ax
   mov cl,12
DEBUGHEXOUTLOOP4:
   mov ax,bx
   shr ax,cl
   and eax,0fh
   mov edi,offset DEBUGHEX
   add edi,eax
   mov al,[edi]
   mov es:[si],al
   add si,2
   sub cl,4
   cmp cl,0
   jne DEBUGHEXOUTLOOP4
   mov ax,bx
   and eax,0fh
   mov edi,offset DEBUGHEX
   add edi,eax
   mov al,[edi]
   mov es:[si],al
   ret
ENDP DEBUGHEXOUT4
;END                             ;debug.inc ends
.INCLUDE DPMI.INC

