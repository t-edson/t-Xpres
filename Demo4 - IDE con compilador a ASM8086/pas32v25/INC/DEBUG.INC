.COMMENT
 ****************************************************************************
 DEBUG32.INC    Pass32 SYSTEM File
                this file implements the internal 16/32 bit debugger
                (c) 1996,1998 by Dieter Pawelczak - Pass32 Version 2.5
                (c) 1999 supports real and pmode debugging
 ============================================================================
 DEBUG32.INC - SYSTEM FILE USED BY THE PASS32 ASSEMBLER
 Attention   - don't alter this file !

; You can use an individual debug file with the .DEBUGFILE directive
; With the .DEBUG option the procedure DEBUGPROC is called before
; each instruction.

 ****************************************************************************

.DATA

.BLOCK
 debugregs         db  'EAX: 00000000 EBX: 00000000 ECX: 00000000 EDX: 00000000 EDI: 00000000  SP: 0000 '
 debugregs2        db  'ESI: 00000000 CS: 0000 DS: 0000 ES: 0000 FS: 0000 GS: 0000 F:0000 EBP: 0000 '
.IFPM
 debuginfo         db  '             : line                                                                 ',0;
.ELSE
 debuginfo         db  '      : line                                                                        ',0;
.ENDIF

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

.INCLUDE DPMI.INC

.CODE
.NODEBUG

DEBUGPROC PROC
; ==============================================
; Initialize DS and save all registers and flags
; ==============================================

   push ds                      ; save old DS
   push ax
.IFPM
   mov ax,CS:[0]                ; load AX with CS:[0] = PSP DS Selector
.ELSE
   mov ax,cs
.ENDIF
   mov ds,ax                    ;                     = DLL DS Selector  / Segment
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
.IFPM
   pop ebx                      ; entry address
   pop eax                      ; offset of debug information
.ELSE
   xor ebx,ebx
   xor eax,eax
   pop bx
   pop ax
.ENDIF
   mov regesp,esp
   mov debuginfooffset,eax
   mov debugoffset,ebx
.IFPM
   push eax
   push ebx
.ELSE
   push ax
   push bx
.ENDIF

   pushfd                        ; save flags
   pop eax                      ;
   mov debugflags,eax
   btr eax,9
   push eax
   popfd
   push ax                      ; store flags
   sub debugoffset,0ah          ; correct the push and call debugproc instr.
L1:

; ===========================================================
; check break point, next instruction for RET/JMP instruction
; ===========================================================
   mov debugdisplay,0
   mov edi,debugoffset
   cmp edi,debugbreakpt
   jne L2
   jmp short L4
L2:
   cmp debugcheckkey,3
   je L40;       SKIP DEBUG
   mov al,[edi+0ah]
   cmp debugcheckkey,2
   je short L3
   cmp al,0c2h        ; RET / RETF INSTRUCTION
   je short L4
   cmp al,0c3h
   je short L4
   cmp al,0cbh
   je short L4
   cmp al,0cah
   je short L4
   cmp debugcheckkey,1
   je L40;       SKIP DEBUG
L3:
   cmp al,0e8h        ; CALL / JMP INSTRUCTION
   je short L4
   cmp al,0ffh
   je short L4
   cmp al,0ebh
   je short L4
   cmp al,0e9h
   je short L4
   cmp debugcheckkey,0
   jne L40;       SKIP DEBUG
L4:

; =====================================================
; save video mode information if debugvideo is enabled
; =====================================================

.IF debugvideo                  ; VIDEO DISPLAY SWAPPING
   cmp debugvideoseg,0
   jne L5
   mov ax,100h
   mov bx,4096                  ; get a 64 KBytes real mode buffer...
.IFPM
   int 31h
   jc L6
.ELSE
   mov ax,4800h
   int 21h
   jc L6
.ENDIF
   mov debugvideoseg,ax
L5:
   ; STORE VIDEO ...
.IFPM
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
.ELSE
   mov ax,1c01h
   mov cx,7
   xor bx,bx
   xor dx,dx
   mov ax,debugvideoseg
   mov es,ax
   int 10h
.ENDIF
   mov ah,0fh
   int 10h
   cmp al,3
   je L6
   mov ax,3
   int 10h
L6:
.ENDIF

; =====================================================
; store screen information and display string debugregs
; =====================================================
.IFPM
   mov ax,CS:[2]                ; promode psp : VideoSelector
.ELSE
   mov ax,0b800h
.ENDIF
   mov es,ax
   mov ecx,81
   mov edx,offset debugvideobuffer-2
   mov esi,3360-2
L7:
   mov ax,es:[2*ecx+esi]
   mov [edx+2*ecx],ax
   loop L7
   mov si,3520
   mov ecx,240
   mov edi,offset debugregs     ; display debug message
   mov edx,offset debugvideobuffer+160
L8:
   mov ax,es:[si]
   mov [edx],ax
   mov al,[edi]
   mov ah,1eh
   mov es:[si],ax
   inc edi
   add si,2
   add edx,2
   loop L8
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
.IFPM
   call debughexout             ; display current EIP
   mov si,3854            ; screen offset
.ELSE
   call debughexout4             ; display current EIP
   mov si,3846            ; screen offset
.ENDIF
   mov edi,debuginfooffset      ; display Source Code information
L9:
   mov al,[edi]
   cmp al,0
   je L10
   mov es:[si],al
   inc edi
   add si,2
   cmp si,4000
   jne L9
L10:
; ==================
; check break point
; ==================
   mov edi,debugoffset
   cmp edi,debugbreakpt
   jne L11
   mov al,'*'
   mov es:[3858],al
L11:

; =========================================================================
; display source code - only available for PMODE due to memory restrictions
; =========================================================================
.IFPM
.IFM DLOADS.INC
.IFMM
   call DISPLAYSCREENSTORE
   mov edi,debugoffset
   call DISPLAYDEBUGSOURCE
   mov debugdisplay,1
.ENDIF
.ENDIF
.ENDIF
   cmp debughold,0
   jz L12
   mov edi,debugdumpaddr
   jmp debugdisplaydump
; ==============================================
; get key and react according to the key pressed
; ==============================================
L12:
   mov debugcheckkey,0
   mov ah,7                     ; Wait for a keystroke
   int 21h
   cmp al,0
   je L12
   cmp al,2    ; [CRTL]+[B]
   jne L15
   mov edi,debugoffset
   cmp debugbreakpt,edi
   je L14
   mov debugbreakpt,edi
   mov al,'*'
   mov es:[3858],al
   jmp L12
L14:
   mov debugbreakpt,0
   mov al,' '
   mov es:[3858],al
   jmp L12
L15:
   cmp al,8
   jne L16
   mov debugcheckkey,3

L16:
   cmp al,'a'
   jne short L17
   mov edi,regeax
   jmp debugdisplaydump
L17:
   cmp al,'b'
   jne short L18
   mov edi,regebx
   jmp debugdisplaydump
L18:
   cmp al,'c'
   jne short L19
   mov edi,regecx
   jmp debugdisplaydump
L19:
   cmp al,'d'
   jne short L20
   mov edi,regedx
   jmp debugdisplaydump
L20:
   cmp al,'e'
   jne short L21
   mov edi,regedi
   jmp debugdisplaydump
L21:
   cmp al,'f'
   jne short L22
   mov edi,regesi
   jmp debugdisplaydump
L22:
   cmp al,'h'
   jne short L23
   mov edi,debugdumpaddr
   mov debughold,1
   jmp debugdisplaydump
L23:
   cmp al,72
   jne short L24
   mov edi,debugdumpaddr
   sub edi,10h
   jmp debugdisplaydump
L24:
   cmp al,80
   jne short L25
   mov edi,debugdumpaddr
   add edi,10h
   jmp debugdisplaydump
L25:
   cmp al,73
   jne short L26
   mov edi,debugdumpaddr
   sub edi,100h
   jmp debugdisplaydump
L26:
   cmp al,81
   jne short L27
   mov edi,debugdumpaddr
   add edi,100h
   jmp debugdisplaydump
L27:
   cmp al,'A'
   jne short L28
   mov si,3530                  ; current register value
   mov edi,offset regeax        ; register to alter
   mov dx,1605h                 ; cursor position
   jmp debugalterreg
L28:
   cmp al,'B'
   jne short L29
   mov si,3558                  ; current register value
   mov edi,offset regebx        ; register to alter
   mov dx,1613h                 ; cursor position
   jmp debugalterreg
L29:
   cmp al,'C'
   jne short L30
   mov si,3586                  ; current register value
   mov edi,offset regecx        ; register to alter
   mov dx,1621h                 ; cursor position
   jmp debugalterreg
L30:
   cmp al,'D'
   jne short L31
   mov si,3614                  ; current register value
   mov edi,offset regedx        ; register to alter
   mov dx,162Fh                 ; cursor position
   jmp debugalterreg
L31:
   cmp al,'E'
   jne short L32
   mov si,3642                  ; current register value
   mov edi,offset regedi        ; register to alter
   mov dx,163dh                 ; cursor position
   jmp debugalterreg
L32:
   cmp al,'F'
   jne short L33
   mov si,3690                  ; current register value
   mov edi,offset regesi        ; register to alter
   mov dx,1705h                 ; cursor position
   jmp debugalterreg
L33:
   cmp al,8
   je short L34
   cmp al,32
   je short L34
   cmp al,3
   je short L34
   cmp al,63
   je short L34
   cmp al,27
   je short L34
   cmp al,13
   je short L34
   jmp L12
L34:
   cmp al,27                    ; [ESC]  RUN UNTIL RET/RETF INSTRUCTION
   jne short  L35
; ==============================================
;   enable run until ret/retf instruction mode
; ==============================================
   mov debugcheckkey,1
L35:
   cmp al,13                    ; [RETURN] RUN UNTIL NEXT CALL/JMP
   jne short L36               ;          INSTRUCTION
; ==============================================
;   enable run until call/jmp instruction mode
; ==============================================
   mov debugcheckkey,2
L36:
  push ax
; ==============================
;  restore old screen contents
; ==============================
   mov esi,3360-2
   mov ecx,321
   mov edx,offset debugvideobuffer-2
L37:
   mov ax,[edx+2*ecx]
   mov es:[esi+2*ecx],ax
   loop L37
; ===================================================
; restore screen - if debug source had been displayed
; ===================================================
.IFPM
.IFM DLOADS.INC
.IFMM
   cmp debugdisplay,0
   je short L38
   call DISPLAYSCREENRESTORE
L38:
.ENDIF
.ENDIF
.ENDIF

; ===============================================
; restore old video mode - if debug video enabled
; ===============================================
.IF DEBUGVIDEO
   cmp debugvideoseg,0
   je L39
   ; VIDEO Back
.IFPM
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
.ELSE
   mov ax,1c02h
   mov cx,7
   xor bx,bx
   xor dx,dx
   mov ax,debugvideoseg
   mov es,ax
   int 10h
.ENDIF
L39:
.ENDIF
; =======================================
;  check key for [CRTL]+[C], [f1], [f5]
; =======================================
   pop ax
   cmp al,3                     ; [CRTL]+[C]
   je L42
   cmp al,63 ; [F5]
   jne short L40
   mov ah,7
   int 21h
   jmp L1
L40:
; ================================================
; in case of debug mode 1,2,3 check keyboard
; ================================================
   cmp debugcheckkey,0
   je L41
.IFPM
   mov ax,cs:[4]                ; Load Zero Selector ...
.ELSE
   mov ax,0
.ENDIF
   mov es,ax
   mov ax,es:[41Ah]             ; ES: ZERO Selector
   cmp ax,es:[41Ch]
   je L41
   mov debugcheckkey,0
   mov ah,7
   int 21h
; =======================================
;    restore all registers and flags
; =======================================
L41:
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
.IFPM
   ret 4        ;
.ELSE
   ret 2
.ENDIF
; ===================
;   abort debugging
; ===================
L42:     ; [CRTL]+[C] pressed - abort debugging
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
L43:
   mov al,[edx+ecx]
   mov es:[esi+2*ecx],ax
   loop L43
   mov si,3360
   mov eax,edi
   call debughexout
   mov edx,ebx
   xor ecx,ecx
   mov si,3384
   mov di,3484
L44:
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
   jne L44
   jmp L12
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
L45:
   mov al,es:[si]
   mov [edi],al
   inc edi
   add si,2
   loop L45
   mov ah,3
   xor ebx,ebx
   int 10h      ; get old cursor position
   mov debugcursor,dx
   pop dx
   mov ah,2
   int 10h      ; set cursor for input
   mov edi,offset debuginputstring
   xor ecx,ecx
L46:
   mov ah,7
   int 21h
   cmp al,0
   jne L47
   mov ah,7
   int 21h
   jmp L46
L47:
   cmp al,13
   je L52
   cmp al,8
   jne L48
   cmp cx,0
   je L46
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
   jmp L46
L48:
   cmp al,27
   jne L49
   pop edi
   jmp short L55
L49:
   cmp al,48
   jb L46
   cmp al,58
   ja L50
   jmp L51
L50:
   and al,223   ; eliminate small letters
   cmp al,'A'
   jb L46
   cmp al,'F'
   ja L46
L51:
   mov [edi+ecx],al
   mov dl,al
   mov ah,2
   int 21h
   inc cx
   cmp cx,8
   jb L46
L52:
; ==================================
;  convert hex string to hex value
; ==================================
   mov edi,offset debuginputstring
   mov ecx,28
   xor ebx,ebx
L53:
   xor eax,eax
   mov al,[edi]
   sub al,48   ; ascii to dec
   cmp al,11
   jb L54
   sub al,7
L54:
   shl eax,cl
   or ebx,eax
   inc edi
   sub cl,4
   jns L53
   pop edi
   mov [edi],ebx
L55:
   pop edi
   pop si
   mov eax,[edi]
   call debughexout
   mov dx,debugcursor
   mov ah,2
   xor ebx,ebx
   int 10h     ; restore old cursor
   jmp L12
   ret
ENDP DEBUGPROC

DEBUGHEXOUT PROC                ; Hexadecimal output of EAX at ES:[SI]
   mov ebx,eax
   mov cl,28
L1:
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
   jne L1
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
L1:
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
   jne L1
   mov ax,bx
   and eax,0fh
   mov edi,offset DEBUGHEX
   add edi,eax
   mov al,[edi]
   mov es:[si],al
   ret
ENDP DEBUGHEXOUT4
;END                             ;debug.inc ends
