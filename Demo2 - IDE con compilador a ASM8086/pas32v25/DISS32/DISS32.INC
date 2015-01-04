.COMMENT
 **************************************************************************
  DISS32.INC - 32 Bit Disassembler Module
  Pass32 Version 2.1 - (c) 1996-1998 by Dieter Pawelczak
  ========================================================================
  used by DISS32 and PRODB32
  (c) 1997 by Dieter Pawelczak
 **************************************************************************
; ****
.DATA
   dhexs        db  '0123456789ABCDEF'
   dr8          db  'AL  ','CL  ','DL  ','BL  ','AH  ','CH  ','DH  ','BH  '
   dr16         db  'AX  ','CX  ','DX  ','BX  ','SP  ','BP  ','SI  ','DI  '
   dr32         db  'EAX ','ECX ','EDX ','EBX ','ESP ','EBP ','ESI ','EDI '
   dsreg        db  'ES: ','CS: ','SS: ','DS: ','FS: ','GS: '
   dispr16      db  '[BX+SI  ','[BX+DI  ','[BP+SI  ','[BP+DI  ','[SI     '
                db  '[DI     ','[       ','[BX     '
   dcreg0       db  'ST'
   dcreg        db  'ST(0)   ','ST(1)   ','ST(2)   ','ST(3)   ','ST(4)   '
                db  'ST(5)   ','ST(6)   ','ST(7)   '
   scale2       db  '+2* '
   scale4       db  '+4* '
   scale8       db  '+8* '
   drep         db  'REP '
   drepne       db  'REPNE '
   dlock        db  'LOCK '
   doperror      db  '???',0
   OPCODECOUNT DW 1252          ; opcode number * 2


   DCOFFS       DD      ?
   DCOFFS1      DD      ?
   DCORRECT     DB      ?
   DOP          DB      ?
   DOP1         DB      ?
   DOP2         DB      ?
   DOTHEROP     DB      ?
   DPRE         DB      ?
   DSEGPREFIX   DB      ?
   DADDRPREFIX  DB      ?
   DREGPREFIX   DB      ?
   DREPPREFIX   DB      ?
   DREPNEPREFIX DB      ?
   DLOCKPREFIX  DB      ?
   DREG         DB      ?
   DCRC         DD      ?
   DMODBYTE     DB      ?
   DMODB        DB      ?
   DMODB2       DB      ?
   DRM          DB      ?
   DSIB         DB      ?
   DSIBBYTE     DB      ?
.PUBLIC   DSTROFFS     DD      ?
.PUBLIC   DSTROFFS2    DD      ?
.PUBLIC  DDECFLAG     DB      ?               ; Decimal Flag 0 = Decimal

.CODE

PROC DISASSEMBLE NEAR          ; EDI : OFFSET to next instruction
                               ; ESI : OFFSET to Instruction STRING
                               ; EDX : OFFSET to Byte contents STRING
                               ; BL  : HEX/DEC Flag BL = 0  >> DEC
                               ;                    BL <> 0 >> Hex
                               ; RETURNS EAX = OFFSET to next instruction
   mov DCOFFS1,EDI
   mov DCOFFS,EDI
   mov DSTROFFS,ESI
   mov DSTROFFS2,EDX
   mov ddecflag,bl

   xor al,al
   mov DCORRECT,al
   mov DPRE,al
   mov DSEGPREFIX,al
   mov DADDRPREFIX,al
   mov DREGPREFIX,al
   mov DREPPREFIX,al
   mov DREPNEPREFIX,al
   mov DLOCKPREFIX,al
   mov DSIB,al
   mov DMODB,al
   mov DSIBBYTE,al
; ==============================================
;              Read all Prefixes
; ==============================================
DGetNext:
   call dreadal
   mov DOP,al
   inc edi

   cmp al,26h
   je short is_segprefix
   cmp al,2Eh
   je short is_segprefix
   cmp al,64h
   je short is_segprefix
   cmp al,65h
   je short is_segprefix
   cmp al,3eh
   je short is_segprefix
   cmp al,36h
   je short is_segprefix
   cmp al,66h
   je short is_regprefix
   cmp al,67h
   je short is_addrprefix
   cmp al,0f0h
   je short is_lockprefix
   cmp al,0f2h
   je short is_repneprefix
   cmp al,0f3h
   je short is_repprefix
   cmp al,0fh
   je short is_pre
   jmp short OK_got_opcode
is_segprefix:
   mov dsegprefix,al
   jmp DGetNext
is_regprefix:
   mov dregprefix,al
   jmp DGetNext
is_addrprefix:
   mov daddrprefix,al
   jmp DGetNext
is_lockprefix:
   mov dlockprefix,al
   jmp DGetNext
is_repprefix:
   mov drepprefix,al
   jmp DGetNext
is_repneprefix:
   mov drepneprefix,al
   jmp DGetNext
is_pre:
   mov dpre,al
   jmp DGetNext

; ==============================================
; No Prefix then OpCode
; ==============================================
OK_got_opcode:
; ===================================================
;           Rep / RepNe / Lock prefix ?
; ===================================================
   cmp drepprefix,0
   jz short norepprefix
   mov esi,dstroffs
   mov eax,dword ptr drep
   mov [esi],eax
   add dstroffs,4
norepprefix:
   cmp drepneprefix,0
   jz short norepneprefix
   mov esi,dstroffs
   mov eax,dword ptr drepne
   mov [esi],eax
   mov ax,word ptr drepne+4
   mov [esi+4],ax
   add dstroffs,6
norepneprefix:
   cmp dlockprefix,0
   jz short nolockprefix
   mov esi,dstroffs
   mov eax,dword ptr dlock
   mov [esi],eax
   mov ax,word ptr dlock+4
   mov [esi+4],ax
   add dstroffs,5
nolockprefix:

; ======================================================================
; Register additive Instruction? Correct Opcode and select register
; ======================================================================
   cmp dpre,0
   jnz NoRegisterAddInstr
   cmp dop,040h
   jb NoRegisterAddInstr
   cmp dop,05fh
   ja short RegisterAddNext
   mov al,dop
   and al,7
   add al,16
   mov dreg,al
   mov al,dop
   and al,0f8h
   mov dop,al
   jmp short NoRegisterAddInstr
RegisterAddNext:
   cmp dop,090h
   jb short NoRegisterAddInstr
   cmp dop,097h
   ja short RegisterAddNext2
   mov al,dop
   and al,7
   add al,16
   mov dreg,al
   mov al,dop
   and al,0f8h
   mov dop,al
   jmp short NoRegisterAddInstr
RegisterAddNext2:
   cmp dop,0b0h
   jb short NoRegisterAddInstr
   cmp dop,0bfh
   ja short NoRegisterAddInstr
   mov al,dop
   and al,7
   add al,16
   mov dreg,al
   mov al,dop
   and al,0f8h
   mov dop,al
   jmp short NoRegisterAddInstr
NoRegisterAddInstr:
; ==============================================
; Compare with asscode table and select entry
; ==============================================
   mov ecx,0
   mov edi,offset opcodetable
CheckAsscodeLoop:
   inc ecx
   inc ecx
   cmp cx,opcodecount
   ja NoOpCodeFound
   mov al,[edi+8*ecx]
   cmp dop,al
   jne CheckAsscodeLoop
   mov al,[edi+8*ecx+1]
   cmp dpre,15
   jne short Ok_Got_Instruction_if_not_pre
   cmp al,15
   jne CheckAsscodeLoop
   jmp short Ok_Got_Instruction
Ok_Got_Instruction_if_not_pre:
   cmp al,0
   jne CheckAsscodeLoop
Ok_Got_Instruction:
   mov DCRC,ecx
; =======================
;  Check if MOD Byte ?
; =======================
   mov edi,offset opcodetable
   mov al,[edi+8*ecx+3]                ; MODB
   cmp al,0
   jz NOMODBYTE
   call dreadal
   mov dmodbyte,al
   mov DMODB,1
; =======================
;  Check, if SIBByte  ?
; =======================
   cmp daddrprefix,0
   jnz short  NoSibByte
   and al,011000111b
   cmp al,4
   je short Sibbyte
   cmp al,44h
   je short Sibbyte
   cmp al,84h
   je short Sibbyte
   jmp short NoSibbyte
Sibbyte:
   call dreadal
   mov DSIB,1
   mov DSIBBYTE,al
NoSibByte:
; =============================================
;  Check if extended opcode in ModR/M Byte
; =============================================

   mov edi,offset opcodetable
   mov al,[edi+8*ecx+2]                ; OPX
   and al,16
   jz short NoOPXCode
   mov al,dmodbyte
   and al,63
   shr al,3
   cmp dop,69h
   je NoOPXCode
   cmp dop,6bh
   je NoOPXCode
TestOPX:
   mov bl,[edi+8*ecx]                  ; OP
   cmp dop,bl
   jne short OPXNext                   ; Opcodes don't fit!
   mov bl,[edi+8*ecx+2]                ; OPX
   and bl,15
   cmp al,bl
   je short NoOPXCode
OPXNext:
   inc ecx
   inc ecx
   cmp Opcodecount,cx
   ja TestOpX
   jmp noopcodefound

NoOPXCode:
NoMODByte:
   push ecx
; ===============================
;  Check if Copro Instruction
; ===============================
   mov edi,offset opcodetable
   cmp byte ptr [edi+ecx*8+5],1
   jne short  @NoCoproInstruction
   xor ecx,ecx
   mov al,dmodbyte   ; here mod byte is part of OPcode
   cmp al,0
   je short  @NoCoproInstruction
   mov bl,dop
TestCopro:
   cmp [edi+ecx*8],al
   jne short TestCoproNext
   cmp [edi+ecx*8+1],bl
   jne short TestCoproNext
   jmp short OkIsCopro
TestCoproNext:
   inc ecx
   inc ecx
   cmp Opcodecount,cx
   ja TestCopro
   jmp short @NoCoproInstruction
OkIsCopro:
   pop eax
   push ecx
@NoCoproInstruction:
   pop ecx

; ===========================
;   Copy Instruction Name
; ===========================

   mov DCRC,ecx
   mov edi,offset opcodetable+8

   mov al,[edi+ecx*8-2]
   cmp al,0
   jne NoOps
   cmp dregprefix,066h
   jne NoOps
   mov al,66h
@Correct16bit:
   cmp [edi+ecx*8-7],al
   je NoOps
   dec ecx
   jne @Correct16bit
NoOps:

   mov esi,DSTROFFS
   mov eax,[edi+ecx*8]
   mov [esi],eax
   mov eax,[edi+ecx*8+4]
   mov [esi+4],eax
   add DSTROFFS,8
; =======================
;     Operand 1
; =======================
   mov al,[edi+ecx*8-1]                 ; Operand 2
   mov dop2,al
   mov dotherop,al
   mov al,[edi+ecx*8-2]                 ; Operand 1
   mov dop1,al
   cmp al,0
   je Nooperands
   cmp dop,0c2h
   je short @retimm
   cmp dop,0cah
   je short @retimm
   cmp  dpre,0
   jne short @noretimm
   jmp short @noretimm
@retimm:
   call dreadax
   call dhexout2
   jmp NoOperands
@noretimm:
; =======================
;    Op1 Immediate ?
; =======================
   cmp al,4
   ja short Op1@NoImm
   cmp al,2     ; Imm8
   ja short op1@NoImm8
   call dreadal
   call dhexout2
   jmp Operand2
op1@NoImm8:
   cmp al,3     ; Imm16
   ja short op1@Imm32
   cmp dregprefix,0
   je short op1@Imm32
   call dreadax
   call dhexout4
   jmp Operand2
op1@Imm32:     ; Imm32
   call dreadeax
   call dhexout
   jmp Operand2
Op1@NoImm:
; =======================
;    Op1 Reg ?
; =======================
  cmp al,33  { DX }
  jne Short Op1@NODX
   mov edi,offset dr16+8
   mov esi,dstroffs
   mov eax,[edi]
   mov [esi],eax
   inc dstroffs
   inc dstroffs
   jmp Operand2
Op1@NODX:
   cmp al,7
   ja Op1@NoReg
   cmp DMODB,0
   je short Op1@GotReg
   mov bl,dmodbyte
   shr bl,3
   and bl,7
   mov dreg,bl
Op1@GotReg:
   mov bl,dreg
   and bl,7
   cmp al,5
   jne short Op1@NoReg8
   movzx edx,bl
   mov edi,offset dr8
   mov esi,dstroffs
   mov eax,[edi+edx*4]
   mov [esi],eax
   inc dstroffs
   inc dstroffs
   jmp Operand2
Op1@NoReg8:
   cmp al,6
   jne short Op1@Reg32
   cmp dregprefix,0
   je short op1@Reg32
   movzx edx,bl
   mov edi,offset dr16
   mov esi,dstroffs
   mov eax,[edi+edx*4]
   mov [esi],eax
   inc dstroffs
   inc dstroffs
   jmp Operand2
op1@Reg32:
   movzx edx,bl
   mov edi,offset dr32
   mov esi,dstroffs
   mov eax,[edi+edx*4]
   mov [esi],eax
   inc dstroffs
   inc dstroffs
   inc dstroffs
   jmp Operand2
Op1@NoReg:
; =======================
;    Op1  RM?
; =======================
   cmp al,10
   ja SHORT OP1@NORM
   call getdisplacement
   jmp Operand2
Op1@NoRM:

; =======================
;    Op1  Akku ?
; =======================
   cmp al,13
   ja Op1@NoAReg
   cmp al,11
   jne short Op1@NoAReg8
   mov edi,offset dr8
   mov esi,dstroffs
   mov eax,[edi]
   mov [esi],eax
   inc dstroffs
   inc dstroffs
   jmp Operand2
Op1@NoAReg8:
   cmp al,12
   jne short Op1@AReg32
   cmp dregprefix,0
   je short op1@AReg32
   mov edi,offset dr16
   mov esi,dstroffs
   mov eax,[edi]
   mov [esi],eax
   inc dstroffs
   inc dstroffs
   jmp Operand2
op1@AReg32:
   mov edi,offset dr32
   mov esi,dstroffs
   mov eax,[edi]
   mov [esi],eax
   inc dstroffs
   inc dstroffs
   inc dstroffs
   jmp Operand2
op1@NoAreg:
; =======================
;    Op1 Sreg ?
; =======================
   cmp al,14
   jne short op1@NoSreg
   mov bl,dreg
   cmp DMODB,0
   je short Op1@GotReg2
   mov bl,dmodbyte
   shr bl,3
   and bl,7
   mov dreg,bl
Op1@GotReg2:
   mov edi,offset dsreg
   movzx edx,dreg
   mov esi,dstroffs
   mov eax,[edi+4*edx]
   mov [esi],eax
   inc dstroffs
   inc dstroffs
   jmp Operand2
op1@NoSreg:
   cmp al,15
   jne short op1@NoCS
   mov edi,offset dsreg
   mov esi,dstroffs
   mov eax,[edi+4]
   mov [esi],eax
   inc dstroffs
   inc dstroffs
   jmp Operand2
op1@NoCS:
   cmp al,16
   jne short op1@NoDS
   mov edi,offset dsreg
   mov esi,dstroffs
   mov eax,[edi+12]
   mov [esi],eax
   inc dstroffs
   inc dstroffs
   jmp Operand2
op1@NoDS:
   cmp al,17
   jne short op1@NoES
   mov edi,offset dsreg
   mov esi,dstroffs
   mov eax,[edi]
   mov [esi],eax
   inc dstroffs
   inc dstroffs
   jmp Operand2
op1@NoES:
   cmp al,18
   jne short op1@NoSS
   mov edi,offset dsreg
   mov esi,dstroffs
   mov eax,[edi+8]
   mov [esi],eax
   inc dstroffs
   inc dstroffs
   jmp Operand2
op1@NoSS:
   cmp al,19
   jne short op1@NoFS
   mov edi,offset dsreg
   mov esi,dstroffs
   mov eax,[edi+16]
   mov [esi],eax
   inc dstroffs
   inc dstroffs
   jmp Operand2
op1@NoFS:
   cmp al,20
   jne short op1@NoGS
   mov edi,offset dsreg
   mov esi,dstroffs
   mov eax,[edi+20]
   mov [esi],eax
   inc dstroffs
   inc dstroffs
   jmp Operand2
op1@NogS:
; =======================
;    Op1 ST ?
; =======================
   cmp al,21
   jne SHORT OP1@CREG0
   mov esi,dstroffs
   mov ax,word ptr dcreg0
   mov [esi],ax
   inc dstroffs
   inc dstroffs
   jmp Operand2
op1@Creg0:
   cmp al,22
jne SHORT OP1@CREG
   mov bl,dmodbyte
   and bl,7
   movzx ebx,bl
   mov esi,dstroffs
   mov edi,offset dcreg
   mov eax,[edi+8*ebx]
   mov [esi],eax
   add dstroffs,4
   mov al,[edi+8*ebx+4]
   add esi,4
   mov [esi],al
   inc dstroffs
   jmp Operand2
op1@Creg:

; =======================
;    Op1 rel ?
; =======================
   cmp al,25
   ja op1@norel
   cmp al,23
   jne SHORT OP1@NOREL8
   call dreadal
   mov ebx,dcoffs
   movsx eax,al
   add eax,ebx
;   cmp bl,127
;   ja op1@rel8A
   call dhexout
   jmp Operand2
;op1@rel8A:
;   add eax,251
;   add eax,ebx
;   call dhexout
;   jmp Operand2
op1@norel8:
   cmp al,24
   jne SHORT OP1@REL32
   cmp daddrprefix,0
   je short op1@Rel32A
   xor eax,eax
   call dreadax
   movsx eax,ax
   mov ebx,dcoffs
   add eax,ebx
   call dhexout
   jmp Operand2
op1@Rel32:
   cmp al,25
   jne SHORT OP1@NOREL
op1@Rel32A:
   call dreadeax
   mov ebx,dcoffs
   add eax,ebx
   call dhexout
   jmp Operand2

op1@norel:

; =======================
;    Op1 MEM ?
; =======================
   cmp al,27
   jb Op1@NoMEM
   call checksegprefix
   cmp al,30            ; call 48 bit pointer
   je short Op1@Mem48
   mov bl,'['
   mov esi,dstroffs
   mov [esi],bl
   inc dstroffs
   cmp daddrprefix,0
   je short op1@Mem32
   call dreadax
   call dhexout4
   mov al,']'
   mov esi,dstroffs
   mov [esi],al
   inc dstroffs
jmp SHORT OPERAND2
op1@Mem32:     ; Imm32
   call dreadeax
   call dhexout
   mov al,']'
   mov esi,dstroffs
   mov [esi],al
   inc dstroffs
   jmp SHORT OPERAND2

Op1@Mem48:
   call getdisplacement
Op1@NoMem:

Operand2:
   mov edi,offset opcodetable+8
   mov al,dop1                 ; Operand 1
   mov dotherop,al
   mov al,dop2                 ; Operand 2

   cmp al,0
   je NoOperands
   mov bl,','
   mov esi,DSTROFFS
   mov [esi],bl
   inc DSTROFFS
; =======================
;    Op2 Immediate ?
; =======================
   cmp al,4
   ja short Op2@NoImm
   cmp al,2     ; Imm8
   ja short op2@NoImm8
   call dreadal
   call dhexout2
   jmp NoOperands
op2@NoImm8:
   cmp al,3     ; Imm16
   ja short op2@Imm32
   cmp dregprefix,0
   je short op2@Imm32
op2@Imm16:
   call dreadax
   call dhexout4
   jmp NoOperands
op2@Imm32:     ; Imm32
   call dreadeax
   call dhexout
   jmp NoOperands
Op2@NoImm:
  cmp al,33  { DX }
  jne Short Op2@NODX
   mov edi,offset dr16+8
   mov esi,dstroffs
   mov eax,[edi]
   mov [esi],eax
   inc dstroffs
   inc dstroffs
   jmp NoOperands
Op2@NODX:
  cmp al,34  { CL }
  jne Short Op2@NOCL
   mov edi,offset dr8+4
   mov esi,dstroffs
   mov eax,[edi]
   mov [esi],eax
   inc dstroffs
   inc dstroffs
   jmp NoOperands
Op2@NOCL:
; =======================
;    Op2 Register ?
; =======================
   cmp al,7
   ja Op2@NoReg
   cmp DMODB,0
   je short Op2@GotReg
   mov bl,dmodbyte
   shr bl,3
   and bl,7
   mov dreg,bl
Op2@GotReg:
   mov bl,dreg
   and bl,7
   cmp al,5
   jne short Op2@NoReg8
   movzx edx,bl
   mov edi,offset dr8
   mov esi,dstroffs
   mov eax,[edi+edx*4]
   mov [esi],eax
   inc dstroffs
   inc dstroffs
   jmp NoOperands
Op2@NoReg8:
   cmp al,6
   jne short Op2@Reg32
   cmp dregprefix,0
   je short op2@Reg32
   movzx edx,bl
   mov edi,offset dr16
   mov esi,dstroffs
   mov eax,[edi+edx*4]
   mov [esi],eax
   inc dstroffs
   inc dstroffs
   jmp NoOperands
op2@Reg32:
   movzx edx,bl
   mov edi,offset dr32
   mov esi,dstroffs
   mov eax,[edi+edx*4]
   mov [esi],eax
   inc dstroffs
   inc dstroffs
   inc dstroffs
   jmp NoOperands

Op2@NoReg:
; =======================
;    Op2  RM?
; =======================
   cmp al,10
   ja SHORT OP2@NORM
   call getdisplacement
   jmp NoOperands
Op2@NoRM:
; =======================
;    Op2 Akku ?
; =======================
   cmp al,13
   ja Op2@NoAReg
   cmp al,11
   jne short Op2@NoAReg8
   mov edi,offset dr8
   mov esi,dstroffs
   mov eax,[edi]
   mov [esi],eax
   inc dstroffs
   inc dstroffs
   jmp NoOperands
Op2@NoAReg8:
   cmp al,12
   jne short Op2@AReg32
   cmp dregprefix,0
   je short op2@AReg32
   mov edi,offset dr16
   mov esi,dstroffs
   mov eax,[edi]
   mov [esi],eax
   inc dstroffs
   inc dstroffs
   jmp NoOperands
op2@AReg32:
   mov edi,offset dr32
   mov esi,dstroffs
   mov eax,[edi]
   mov [esi],eax
   inc dstroffs
   inc dstroffs
   inc dstroffs
   jmp NoOperands
op2@NoAReg:
; =======================
;    Op2 Sreg ?
; =======================
   cmp al,14
   jne short op2@NoSreg
   mov bl,dreg
   cmp DMODB,0
   je short Op2@GotReg2
   mov bl,dmodbyte
   shr bl,3
   and bl,7
Op2@GotReg2:
   mov edi,offset dsreg
   movzx edx,bl
   mov esi,dstroffs
   mov eax,[edi+4*edx]
   mov [esi],eax
   inc dstroffs
   inc dstroffs
   jmp NoOperands
op2@NoSreg:
; =======================
;    Op1 ST ?
; =======================
   cmp al,21
   jne SHORT OP2@CREG0
   mov esi,dstroffs
   mov ax,word ptr dcreg0
   mov [esi],ax
   inc dstroffs
   inc dstroffs
   jmp NoOperands
op2@Creg0:
   cmp al,22
   jne short op2@Creg
   mov bl,dmodbyte
   and bl,7
   movzx ebx,bl
   mov esi,dstroffs
   mov edi,offset dcreg
   mov eax,[edi+8*ebx]
   mov [esi],eax
   add dstroffs,4
   mov al,[edi+8*ebx+4]
   add esi,4
   mov [esi],al
   inc dstroffs
   jmp NoOperands
op2@Creg:

; =======================
;    Op2 MEM ?
; =======================
   cmp al,27
   jb short Op2@NoMEM
   call checksegprefix
   mov bl,'['
   mov esi,dstroffs
   mov [esi],bl
   inc dstroffs
   cmp daddrprefix,0
   je short op2@Mem32
   call dreadax
   call dhexout4
   mov al,']'
   mov esi,dstroffs
   mov [esi],al
   inc dstroffs
jmp SHORT NOOPERANDS
op2@Mem32:     ; Imm32
   call dreadeax
   call dhexout
   mov al,']'
   mov esi,dstroffs
   mov [esi],al
   inc dstroffs
   jmp SHORT NOOPERANDS
Op2@NoMem:

NoOperands:
   mov esi,dstroffs
   xor al,al
   mov [esi],al                 ; Zero Terminated string ...
NoOpCode:
   mov eax,dcoffs
   ret
NoOpCodeFound:
   mov eax,dword ptr doperror
   mov esi,dstroffs
   mov [esi],eax
   mov eax,dcoffs
   ret
ENDP DISASSEMBLE

PROC GetDisplacement NEAR
   push ecx
   mov al,dmodbyte
   and al,7+64+128
   mov dmodb2,al
   and al,7
   mov drm,al
   mov al,dmodb2
   and al,128+64
   cmp al,128+64
   jnz DISP16NoRegister
   mov al,dop
   and al,1
   jnz SHORT DISPREG16
   movzx ebx,drm
   cmp dotherop,14              ; if other operand is 16 bit SReg, then use 16 bit!
   je short DISPReg162
   mov edi,offset dr8
   mov eax,[edi+4*ebx]
   mov esi,dstroffs
   mov [esi],eax
   inc dstroffs
   inc dstroffs
   pop ecx
   ret
DISPReg16:
   cmp dregprefix,0
   jz SHORT DISPREG32
   movzx ebx,drm
DISPReg162:
   mov edi,offset dr16
   mov eax,[edi+4*ebx]
   mov esi,dstroffs
   mov [esi],eax
   inc dstroffs
   inc dstroffs
   pop ecx
   ret
DISPReg32:
   movzx ebx,drm
   mov edi,offset dr32
   mov eax,[edi+4*ebx]
   mov esi,dstroffs
   mov [esi],eax
   inc dstroffs
   inc dstroffs
   inc dstroffs
   pop ecx
   ret
DISP16NoRegister:
   cmp daddrprefix,0
   je D32BitAddressing
   call checksegprefix
   movzx ebx,drm
   mov edi,offset dispr16
   mov esi,dstroffs
   mov eax,[edi+8*ebx]
   mov [esi],eax
   mov eax,[edi+8*ebx+4]
   mov [esi+4],eax
   inc dstroffs
   cmp drm,6
   je SHORT DISP16ADDDISP
   inc dstroffs
   inc dstroffs
   cmp drm,3
   ja SHORT DISP16ADDDISP
   add dstroffs,3
DISP16AddDisp:
   cmp dmodb2,6
   jne SHORT DISP16ADDDISP2
   call dreadax
   call dhexout4
   jmp DISP16ADDEND
DISP16AddDisp2:
   mov al,dmodb2
   and al,128+64
   mov dmodb2,al
   cmp dmodb2,64
   jnz SHORT DISP16ADDDISP3
   mov dl,'+'
   push edx
   call dreadal
   cmp al,127
   jb @invert
   neg al
   pop edx
   mov dl,'-'
   push edx
@invert:
   pop edx
   mov esi,dstroffs
   mov [esi],dl
   inc dstroffs
   call dhexout2
jmp SHORT DISP16ADDEND
DISP16AddDisp3:
   cmp dmodb2,128
   jnz SHORT DISP16ADDEND
   mov al,'+'
   mov esi,dstroffs
   mov [esi],al
   inc dstroffs
   call dreadax
   call dhexout4
   jmp SHORT DISP16ADDEND
DISP16ADDEND:
   mov al,']'
   mov esi,dstroffs
   mov [esi],al
   inc dstroffs
   pop ecx
   ret
D32BitAddressing:
   call checksegprefix
   mov al,'['
   mov esi,dstroffs
   mov [esi],al
   inc dstroffs
   cmp drm,4
   jnz DISP32NoSib
   mov bl,dsibbyte
   and bl,7
   movzx ebx,bl
   cmp bl,5
   jne short SIB@NOBP
   mov al,dmodbyte
   and al,192
   cmp al,0
   jnz short SIB@NOBP
   call dreadeax
   call dhexout
   movzx ebx,dsibbyte
   shr ebx,3
   and ebx,7
   cmp bl,4
   je short Sib@Noscale
   jmp short ScaleFactor


SIB@NOSCALE:
   mov bl,dSibbyte
   and bl,7
   jmp  DISP32ADD
SIB@NOBP:
   mov edi,offset dr32
   mov esi,dstroffs
   mov eax,[edi+4*ebx]
   mov [esi],eax
   add dstroffs,3
   cmp bl,4
   je DISP32ADD
ScaleFactor:
   mov bl,dsibbyte
   and bl,192
   cmp bl,0
   jnz short SIBNExt
   mov al,'+'
   mov esi,dstroffs
   mov [esi],al
   inc dstroffs
SIBNExt:
   cmp bl,64
   jnz short SIBNExt2
   mov eax, DWORD PTR Scale2
   mov esi,dstroffs
   mov [esi],eax
   add dstroffs,3
SIBNExt2:
   cmp bl,128
   jnz short SIBNExt3
   mov eax, DWORD PTR Scale4
   mov esi,dstroffs
   mov [esi],eax
   add dstroffs,3
SIBNExt3:
   cmp bl,192
   jnz short SIBNExt4
   mov eax, DWORD PTR Scale8
   mov esi,dstroffs
   mov [esi],eax
   add dstroffs,3
SIBNExt4:
   mov bl,dsibbyte
   shr bl,3
   and bl,7
   movzx ebx,bl
   mov edi,offset dr32
   mov esi,dstroffs
   mov eax,[edi+4*ebx]
   mov [esi],eax
   add dstroffs,3
jmp SHORT DISP32ADD
DISP32NoSib:
   cmp dmodb2,5
   je SHORT DISP32ADD
   movzx ebx,drm
   mov edi,offset dr32
   mov esi,dstroffs
   mov eax,[edi+4*ebx]
   mov [esi],eax
   add dstroffs,3
DISP32ADD:
   cmp dmodb2,5
   jne SHORT DISP32ADDDISP2
   call dreadeax
   call dhexout
   jmp DISP16ADDEND
DISP32AddDisp2:
   mov al,dmodb2
   and al,128+64
   mov dmodb2,al
   cmp dmodb2,64
   jnz SHORT DISP32ADDDISP3
   mov dl,'+'
   push edx
   call dreadal
   cmp al,127
   jb @@invert
   neg al
   pop edx
   mov dl,'-'
   push edx
@@invert:
   pop edx
   mov esi,dstroffs
   mov [esi],dl
   inc dstroffs
   call dhexout2
   jmp DISP16ADDEND
DISP32AddDisp3:
   cmp dmodb2,128
   jnz DISP16AddEND
   mov al,'+'
   mov esi,dstroffs
   mov [esi],al
   inc dstroffs
   call dreadeax
   call dhexout
   jmp DISP16ADDEND
   ret
.NODEBUG
ENDP GetDisplacement

PROC DREADAL NEAR                 ; reads 1 Byte
   push edi
   push esi
   mov esi,dstroffs
   push esi
   mov esi,dstroffs2
   mov dstroffs,esi
   mov edi,dcoffs
   mov al,FS:[edi]
   call Dhexout2
   mov esi,dstroffs
   mov al,' '
   mov [esi],al
   inc esi
   mov dstroffs2,esi
   pop esi
   mov dstroffs,esi
   mov edi,dcoffs
   mov al,FS:[edi]
   inc dcoffs
   pop esi
   pop edi
   ret
ENDP DREADAL

PROC DREADAX NEAR               ; reads 2 Bytes
   push edi
   push esi
   mov edi,dcoffs
   push edi
   mov esi,dstroffs
   push esi
   mov esi,dstroffs2
   mov dstroffs,esi
   mov edi,dcoffs
   mov al,FS:[edi]
   call Dhexout2
   mov esi,dstroffs
   mov al,' '
   mov [esi],al
   inc dstroffs
   inc dcoffs
   mov edi,dcoffs
   mov al,FS:[edi]
   call Dhexout2
   mov esi,dstroffs
   mov al,' '
   mov [esi],al
   inc esi
   mov dstroffs2,esi
   pop esi
   mov dstroffs,esi
   pop edi
   mov ax,FS:[edi]
   inc dcoffs
   pop esi
   pop edi
   ret
ENDP DREADAX

PROC DREADEAX NEAR               ; reads 4 Bytes
   push edi
   push esi
   mov edi,dcoffs
   push edi
   mov esi,dstroffs
   push esi
   mov esi,dstroffs2
   mov dstroffs,esi
   mov edi,dcoffs
   mov al,FS:[edi]
   call Dhexout2
   mov esi,dstroffs
   mov al,' '
   mov [esi],al
   inc dstroffs
   inc dcoffs
   mov edi,dcoffs
   mov al,FS:[edi]
   call Dhexout2
   mov esi,dstroffs
   mov al,' '
   mov [esi],al
   inc dstroffs
   inc dcoffs
   mov edi,dcoffs
   mov al,FS:[edi]
   call Dhexout2
   mov esi,dstroffs
   mov al,' '
   mov [esi],al
   inc dstroffs
   inc dcoffs
   mov edi,dcoffs
   mov al,FS:[edi]
   call Dhexout2
   mov esi,dstroffs
   mov al,' '
   mov [esi],al
   inc esi
   mov dstroffs2,esi
   pop esi
   mov dstroffs,esi
   pop edi
   mov eax,FS:[edi]
   inc dcoffs
   pop esi
   pop edi
   ret
ENDP DREADEAX

PROC checksegprefix NEAR ; Adds the segment prefix
  push eax
  push esi
  push edi
  cmp dsegprefix,0
  jz checksegprefix@end
  cmp dsegprefix,2eh
  jne short checksegprefix@NoCS
   mov edi,offset dsreg+4
   mov esi,DSTROFFS
   mov eax,[edi]
   mov [esi],eax
   add DSTROFFS,3
   jmp checksegprefix@end
checksegprefix@NoCS:
  cmp dsegprefix,3eh
  jne short checksegprefix@NoDS
   mov edi,offset dsreg+12
   mov esi,DSTROFFS
   mov eax,[edi]
   mov [esi],eax
   add DSTROFFS,3
   jmp checksegprefix@end
checksegprefix@NoDS:
  cmp dsegprefix,26h
  jne short checksegprefix@NoES
   mov edi,offset dsreg
   mov esi,DSTROFFS
   mov eax,[edi]
   mov [esi],eax
   add DSTROFFS,3
   jmp checksegprefix@end
checksegprefix@NoES:
  cmp dsegprefix,64h
  jne short checksegprefix@NoFS
   mov edi,offset dsreg+16
   mov esi,DSTROFFS
   mov eax,[edi]
   mov [esi],eax
   add DSTROFFS,3
jmp SHORT CHECKSEGPREFIX@END
checksegprefix@NoFS:
  cmp dsegprefix,65h
  jne short checksegprefix@NoGS
   mov edi,offset dsreg+20
   mov esi,DSTROFFS
   mov eax,[edi]
   mov [esi],eax
   add DSTROFFS,3
   jmp SHORT CHECKSEGPREFIX@END
checksegprefix@NoGS:
   cmp dsegprefix,36h
   jne short checksegprefix@NoSS
   mov edi,offset dsreg+8
   mov esi,DSTROFFS
   mov eax,[edi]
   mov [esi],eax
   add DSTROFFS,3
checksegprefix@NoSS:
checksegprefix@end:
  pop edi
  pop esi
  pop eax
  ret
ENDP checksegprefix

.NODEBUG
PROC DHEXOUT2 NEAR                   ; 1 Byte hex OutPut in String DSTROFFS
                                ; if decimalflag = 0, then decimal output
   movzx ebx,al
   cmp ddecflag,0
   jz EBXtoString
   shr ebx,4
   mov bl,[EBX+OFFSET DHEXS]
   mov esi,DSTROFFS
   mov [esi],bl
   mov bl,al
   and bl,15
   movzx ebx,bl
   mov bh,0
   mov bl,[EBX+OFFSET DHEXS]
   mov [esi+1],bx
   add DSTROFFS,2
   ret
ENDP DHEXOUT2

DHEXOUT PROC  NEAR               ; Hexadecimal output of EAX to STRING DSTROFFS
   mov ebx,eax
   cmp ddecflag,0
   jz EBXtoString
   push ecx
   mov cl,28
DHEXOUTLOOP:
   mov esi,DSTROFFS
   mov eax,ebx
   shr eax,cl
   and eax,0fh
   mov edi,offset DHEXS
   add edi,eax
   mov al,[edi]
   mov [esi],al
   inc DSTROFFS
   add si,2
   sub cl,4
   cmp cl,0
   jne DHEXOUTLOOP
   mov eax,ebx
   and eax,0fh
   mov edi,offset DHEXS
   add edi,eax
   mov al,[edi]
   mov ah,0
   mov esi,DSTROFFS
   mov [esi],ax
   inc DSTROFFS
   pop ecx
   ret
ENDP DHEXOUT

DHEXOUT4 PROC  NEAR             ; Hexadecimal output of AX at DSTROFFS
   movzx ebx,ax
   cmp ddecflag,0
   jz short EBXtoString
   push ecx
   mov cl,12
DHEXOUTLOOP4:
   mov esi,DSTROFFS
   mov ax,bx
   shr ax,cl
   and eax,0fh
   mov edi,offset DHEXS
   add edi,eax
   mov al,[edi]
   mov [esi],al
   inc DSTROFFS
   add si,2
   sub cl,4
   cmp cl,0
   jne DHEXOUTLOOP4
   mov ax,bx
   and eax,0fh
   mov edi,offset DHEXS
   add edi,eax
   mov al,[edi]
   mov ah,0
   mov esi,DSTROFFS
   mov [esi],ax
   inc DSTROFFS
   pop ecx
   ret
.PUBLIC EBXtoString:        ; converts numeric value in EBX to String
   mov EDI,Dstroffs
   mov eax,ebx
   push ecx
   call MathtoString
EBXtoStringFindZeroLoop:
   mov EDI,Dstroffs
   mov al,[edi]
   cmp al,0
   je SHORT EBXTOSTRINGEND
   inc dstroffs
   jmp EBXtoStringFindZeroLoop
EBXtoStringEnd:
   pop ecx
   ret
ENDP DHEXOUT4

.IFM SYSTEM.INC
.ELSE
MathToString PROC  NEAR      ;  32 bit (EAX) to BCD String OFFSET EDI
.DATA
      @mathoutbcd     DT       0
      @mathoutint     DD       0
      @mathoutnull    DB       0
.CODE
    cmp eax,0
    jne SHORT @MATHTOSTR@NONIL
    mov dx,48
    mov [edi],dx
    ret
@MathToStr@NoNil:
    mov @mathoutint,eax
    fild @mathoutint
    fbstp @mathoutbcd
    mov ecx,9
    mov @mathoutnull,0
    fwait
    mov al,byte ptr @mathoutbcd+9
    and al,128
    cmp al,0
    je SHORT @MATHTOSTRING@NOSIGN
    mov dx,'-'
    mov [edi],dx
    inc edi
@MathtoString@nosign:
    xor dx,dx
    mov esi,offset @mathoutbcd+8
@MathToStrConvert:
    mov al,[esi]
    shr ax,4
    and al,0fh
    cmp al,0
    je SHORT @MATHTOSTR@NEXT
    mov @mathoutnull,1
@MathToStr@next:
    cmp @mathoutnull,0
    je SHORT @MATHTOSTR@NEXT2
    mov dl,al
    add dl,48
    mov [edi],dx
    inc edi
@MathToStr@next2:
    mov al,[esi]
    and al,0fh
    cmp al,0
    je SHORT @MATHTOSTR@NEXT3
    mov @mathoutnull,1
@MathToStr@next3:
    cmp @mathoutnull,0
    je SHORT @MATHTOSTR@NEXT4
    mov dl,al
    add dl,48
    mov [edi],dx
    inc edi
@MathToStr@next4:
    dec esi
    loop @MathToStrConvert
    ret
ENDP MathToString
.ENDIF


.PUBLIC OPCODETABLE:
.LOADBIN DIS32.BIN
