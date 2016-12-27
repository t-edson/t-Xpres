.COMMENT
 ****************************************************************************
 TESTPARA.ASM   Example file for the parameter line
                (c) 1996,1998 by Dieter Pawelczak - Pass32 Version 2.1
 ============================================================================
 ****************************************************************************
.MODEL FLAT
.UCU
.NM

.DATA
     mesg1       db   'Number of Parameters: '
     paramstr    db   '            ',0
     mesg2       db   'Parameters          :',0
     mesg4       db   'No Parameters!',0
     paramnumber db   0

     TextColor        .EQU 1eh

.CODE

START:
     mov eax,17201720h        ; Attribute = 17 (blue/gray), Character = 20 (space)
     mov ecx,(80*25*2)/4-1    ; ecx = (size of screen memory) / 4
   clearscreenloop:
     mov ES:[ecx*4],eax       ; write 4 bytes into screen at once
     loop clearscreenloop
     xor eax,eax
     mov al,255               ; maximum of parameters
     call getparamstr
     mov paramnumber,al       ; now in AL (EAX) the number of parameters
     cmp al,0
     je NoParameters
     mov edi,Offset paramstr
     call mathtostring        ; The integer Number in EAX will be converted
                              ; to a zero terminated string.
     mov edi,Offset mesg1     ; 'Number of Parameters: '
     mov bh,TextColor
     xor dx,dx                ; DX = Y - Pos = 0 = line 1
     xor cx,cx                ; CX = X - Pos = 0 = column 1
     call systemprint
     mov edi,Offset mesg2     ;  'Parameters          :',0
     mov dx,1                 ; in the next line
     mov cx,0
     call systemprint
     cmp ParamNumber,24          ; can display only 24 parameters
     jb LessThan24Lines
     mov ParamNumber,24          ; if more than reduce parameters to 24
LessThan24Lines:
     xor dl,dl                   ; DL = X - Pos = 0 = column 1
     mov dh,ParamNumber          ; DH = Y - Pos = ParamNumber = line
     xor bh,bh
     mov ah,02
     int 10h                     ; bios set cursor function
PrintParameters:
     mov al,Paramnumber
     movzx dx,al                 ; Y-pos in DX
     call getparamstr
     mov cx,22
     mov bh,TextColor
     call systemprint
     dec Paramnumber
     cmp Paramnumber,0
     jne PrintParameters         ; repeat until ParamNumber = 0
     mov ax,4c00h
     int 21h

NoParameters:
     mov edi,OFFSET mesg4
     mov bh,TextColor
     xor dx,dx                ; DX = Y - Pos = 0 = line 1
     xor cx,cx                ; CX = X - Pos = 0 = column 1
     call systemprint
     mov dx,1
     xor cx,cx
     xor bh,bh
     mov ah,02h
     int 10h                     ; bios set cursor function
     mov ax,4c00h
     int 21h
.INCLUDE SYSTEM.INC
END START
END