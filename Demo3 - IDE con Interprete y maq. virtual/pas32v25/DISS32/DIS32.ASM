.PUBLIC .DATA
OPCODETABLE DB 16 DUP(0)
; OP, PRE, OPX, MOD, XREG, COPRO, OP1, OP2, Instruction
DB 140,  0, 0,1,0,0, 9,14,'MOV     '; MOV     r/m16, sreg 
DB 142,  0, 0,1,0,0,14, 9,'MOV     '; MOV     sreg, r/m16 
DB 176,  0, 0,0,1,0, 5, 2,'MOV     '; MOV     reg8, imm8 
DB 184,  0, 0,0,1,0, 6, 3,'MOV     '; MOV     reg16, imm16 
DB 184,  0, 0,0,1,0, 7, 4,'MOV     '; MOV     reg32, imm32 
DB 162,  0, 0,0,1,0,27,11,'MOV     '; MOV     mem8, AL 
DB 163,  0, 0,0,1,0,28,12,'MOV     '; MOV     mem16, AX 
DB 163,  0, 0,0,1,0,29,13,'MOV     '; MOV     mem32, EAX 
DB 160,  0, 0,0,1,0,11,27,'MOV     '; MOV     AL, mem8 
DB 161,  0, 0,0,1,0,12,28,'MOV     '; MOV     AX, mem16 
DB 161,  0, 0,0,1,0,13,29,'MOV     '; MOV     EAX, mem32 
DB 136,  0, 0,1,1,0, 8, 5,'MOV     '; MOV     r/m8, reg8 
DB 137,  0, 0,1,1,0, 9, 6,'MOV     '; MOV     r/m16, reg16 
DB 137,  0, 0,1,1,0,10, 7,'MOV     '; MOV     r/m32, reg32 
DB 138,  0, 0,1,1,0, 5, 8,'MOV     '; MOV     reg8, r/m8 
DB 139,  0, 0,1,1,0, 6, 9,'MOV     '; MOV     reg16, r/m16 
DB 139,  0, 0,1,1,0, 7,10,'MOV     '; MOV     reg32, r/m32 
DB 198,  0, 0,1,1,0, 8, 2,'MOV     '; MOV     r/m8, imm8 
DB 199,  0, 0,1,1,0, 9, 3,'MOV     '; MOV     r/m16, imm16 
DB 199,  0, 0,1,1,0,10, 4,'MOV     '; MOV     r/m32, imm32 
DB  48,  0, 0,1,1,0, 8, 5,'XOR     '; XOR     r/m8, reg8 
DB  49,  0, 0,1,1,0, 9, 6,'XOR     '; XOR     r/m16, reg16 
DB  49,  0, 0,1,1,0,10, 7,'XOR     '; XOR     r/m32, reg32 
DB  50,  0, 0,1,1,0, 5, 8,'XOR     '; XOR     reg8, r/m8 
DB  51,  0, 0,1,1,0, 6, 9,'XOR     '; XOR     reg16, r/m16 
DB  51,  0, 0,1,1,0, 7,10,'XOR     '; XOR     reg32, r/m32 
DB  52,  0, 0,0,1,0,11, 2,'XOR     '; XOR     AL, imm8 
DB  53,  0, 0,0,1,0,12, 3,'XOR     '; XOR     AX, imm16 
DB  53,  0, 0,0,1,0,13, 4,'XOR     '; XOR     EAX, imm32 
DB 128,  0,22,1,1,0, 8, 2,'XOR     '; XOR     r/m8, imm8 
DB 129,  0,22,1,1,0, 9, 3,'XOR     '; XOR     r/m16, imm16 
DB 129,  0,22,1,1,0,10, 4,'XOR     '; XOR     r/m32, imm32 
DB 131,  0,22,1,1,0, 9, 2,'XOR     '; XOR     r/m16, imm8 
DB 131,  0,22,1,1,0,10, 2,'XOR     '; XOR     r/m32, imm8 
DB 232,  0, 0,0,1,0,24, 0,'CALL    '; CALL    rel16
DB 232,  0, 0,0,1,0,25, 0,'CALL    '; CALL    rel32
DB 255,  0,18,1,1,0,10, 0,'CALL    '; CALL    r/m32
DB 255,  0,19,1,1,0,30, 0,'CALL    '; CALL    mem48
DB 195,  0, 0,0,1,0, 0, 0,'RET     '; RET     
DB 194,  0, 0,0,0,0, 4, 0,'RET     '; RET     
DB 203,  0, 0,0,1,0, 0, 0,'RETF    '; RETF    
DB 202,  0, 0,0,0,0, 4, 0,'RETF    '; RETF    
DB  80,  0, 0,0,1,0, 6, 0,'PUSH    '; PUSH    reg16
DB  80,  0, 0,0,1,0, 7, 0,'PUSH    '; PUSH    reg32
DB  14,  0, 0,0,1,0,15, 0,'PUSH    '; PUSH    CS
DB  22,  0, 0,0,1,0,18, 0,'PUSH    '; PUSH    SS
DB  30,  0, 0,0,1,0,16, 0,'PUSH    '; PUSH    DS
DB   6,  0, 0,0,1,0,17, 0,'PUSH    '; PUSH    ES
DB 160, 15, 0,0,1,0,19, 0,'PUSH    '; PUSH    FS
DB 168, 15, 0,0,1,0,20, 0,'PUSH    '; PUSH    GS
DB 104,  0, 0,0,1,0, 3, 0,'PUSH    '; PUSH    
DB 104,  0, 0,0,1,0, 4, 0,'PUSH    '; PUSH    
DB 255,  0,22,1,1,0, 9, 0,'PUSH    '; PUSH    rm16
DB 255,  0,22,1,1,0,10, 0,'PUSH    '; PUSH    rm32
DB 143,  0,16,1,1,0, 9, 0,'POP     '; POP     rm16
DB 143,  0,16,1,1,0,10, 0,'POP     '; POP     rm32
DB  23,  0, 0,0,1,0,18, 0,'POP     '; POP     SS
DB  31,  0, 0,0,1,0,16, 0,'POP     '; POP     DS
DB   7,  0, 0,0,1,0,17, 0,'POP     '; POP     ES
DB 161, 15, 0,0,1,0,19, 0,'POP     '; POP     FS
DB 169, 15, 0,0,1,0,20, 0,'POP     '; POP     GS
DB  88,  0, 0,0,1,0, 6, 0,'POP     '; POP     reg16
DB  88,  0, 0,0,1,0, 7, 0,'POP     '; POP     reg32
DB   0,  0, 0,1,1,0, 8, 5,'ADD     '; ADD     r/m8, reg8
DB   1,  0, 0,1,1,0, 9, 6,'ADD     '; ADD     r/m16, reg16
DB   1,  0, 0,1,1,0,10, 7,'ADD     '; ADD     r/m32, reg32
DB   2,  0, 0,1,1,0, 5, 8,'ADD     '; ADD     reg8, r/m8
DB   3,  0, 0,1,1,0, 6, 9,'ADD     '; ADD     reg16, r/m16
DB   3,  0, 0,1,1,0, 7,10,'ADD     '; ADD     reg32, r/m32
DB   4,  0, 0,0,1,0,11, 2,'ADD     '; ADD     AL, imm8 
DB   5,  0, 0,0,1,0,12, 3,'ADD     '; ADD     AX, imm16 
DB   5,  0, 0,0,1,0,13, 4,'ADD     '; ADD     EAX, imm32 
DB 128,  0,16,1,1,0, 8, 2,'ADD     '; ADD     r/m8, imm8 
DB 131,  0,16,1,1,0, 9, 2,'ADD     '; ADD     r/m16, imm8 
DB 131,  0,16,1,1,0,10, 2,'ADD     '; ADD     r/m32, imm8 
DB 128,  0,16,1,1,0, 8, 2,'ADD     '; ADD     r/m8, imm8 
DB 129,  0,16,1,1,0, 9, 3,'ADD     '; ADD     r/m16, imm16 
DB 129,  0,16,1,1,0,10, 4,'ADD     '; ADD     r/m32, imm32 
DB  32,  0, 0,1,1,0, 8, 5,'AND     '; AND     r/m8, reg8 
DB  33,  0, 0,1,1,0, 9, 6,'AND     '; AND     r/m16, reg16 
DB  33,  0, 0,1,1,0,10, 7,'AND     '; AND     r/m32, reg32 
DB  34,  0, 0,1,1,0, 5, 8,'AND     '; AND     reg8, r/m8 
DB  35,  0, 0,1,1,0, 6, 9,'AND     '; AND     reg16, r/m16 
DB  35,  0, 0,1,1,0, 7,10,'AND     '; AND     reg32, r/m32 
DB  36,  0, 0,0,1,0,11, 2,'AND     '; AND     AL, imm8 
DB  37,  0, 0,0,1,0,12, 3,'AND     '; AND     AX, imm16 
DB  37,  0, 0,0,1,0,13, 4,'AND     '; AND     EAX, imm32 
DB 128,  0,20,1,1,0, 8, 2,'AND     '; AND     r/m8, imm8 
DB 129,  0,20,1,1,0, 9, 3,'AND     '; AND     r/m16, imm16 
DB 129,  0,20,1,1,0,10, 4,'AND     '; AND     r/m32, imm32 
DB 131,  0,20,1,1,0, 9, 2,'AND     '; AND     r/m16, imm8 
DB 131,  0,20,1,1,0,10, 2,'AND     '; AND     r/m32, imm8 
DB  56,  0, 0,1,1,0, 8, 5,'CMP     '; CMP     r/m8, reg8 
DB  57,  0, 0,1,1,0, 9, 6,'CMP     '; CMP     r/m16, reg16 
DB  57,  0, 0,1,1,0,10, 7,'CMP     '; CMP     r/m32, reg32 
DB  58,  0, 0,1,1,0, 5, 8,'CMP     '; CMP     reg8, r/m8 
DB  59,  0, 0,1,1,0, 6, 9,'CMP     '; CMP     reg16, r/m16 
DB  59,  0, 0,1,1,0, 7,10,'CMP     '; CMP     reg32, r/m32 
DB  60,  0, 0,0,1,0,11, 2,'CMP     '; CMP     AL, imm8 
DB  61,  0, 0,0,1,0,12, 3,'CMP     '; CMP     AX, imm16 
DB  61,  0, 0,0,1,0,13, 4,'CMP     '; CMP     EAX, imm32 
DB 128,  0,23,1,1,0, 8, 2,'CMP     '; CMP     r/m8, imm8 
DB 131,  0,23,1,1,0, 9, 2,'CMP     '; CMP     r/m16, imm8 
DB 131,  0,23,1,1,0,10, 2,'CMP     '; CMP     r/m32, imm8 
DB 129,  0,23,1,1,0, 9, 3,'CMP     '; CMP     r/m16, imm16 
DB 129,  0,23,1,1,0,10, 4,'CMP     '; CMP     r/m32, imm32 
DB  72,  0, 0,0,1,0, 6, 0,'DEC     '; DEC     reg16
DB  72,  0, 0,0,1,0, 7, 0,'DEC     '; DEC     reg32
DB 254,  0,17,1,1,0, 8, 0,'DEC     '; DEC     r/m8
DB 255,  0,17,1,1,0, 9, 0,'DEC     '; DEC     r/m16
DB 255,  0,17,1,1,0,10, 0,'DEC     '; DEC     r/m32
DB 246,  0,22,1,1,0, 8, 0,'DIV     '; DIV     r/m8
DB 247,  0,22,1,1,0, 9, 0,'DIV     '; DIV     r/m16
DB 247,  0,22,1,1,0,10, 0,'DIV     '; DIV     r/m32
DB 246,  0,23,1,1,0, 8, 0,'IDIV    '; IDIV    r/m8
DB 247,  0,23,1,1,0, 9, 0,'IDIV    '; IDIV    r/m16
DB 247,  0,23,1,1,0,10, 0,'IDIV    '; IDIV    r/m32
DB 246,  0,21,1,1,0, 8, 0,'IMUL    '; IMUL    r/m8
DB 247,  0,21,1,1,0, 9, 0,'IMUL    '; IMUL    r/m16
DB 247,  0,21,1,1,0,10, 0,'IMUL    '; IMUL    r/m32
DB 175, 15, 0,1,1,0, 6, 9,'IMUL    '; IMUL    reg16, r/m16
DB 175, 15, 0,1,1,0, 7, 9,'IMUL    '; IMUL    reg32, r/m16
DB 107,  0,0,1,1,0, 9, 2,'IMUL    '; IMUL    r/m16, imm8
DB 107,  0,0,1,1,0,10, 2,'IMUL    '; IMUL    r/m32, imm8
DB 105,  0,0,1,1,0, 9, 3,'IMUL    '; IMUL    r/m16, imm16
DB 105,  0,0,1,1,0,10, 4,'IMUL    '; IMUL    r/m32, imm32
DB 228,  0, 0,0,0,0,11, 2,'IN      '; IN      AL, imm8 
DB 229,  0, 0,0,1,0,12, 2,'IN      '; IN      AX, imm8 
DB 229,  0, 0,0,1,0,13, 2,'IN      '; IN      EAX, imm8 
DB 236,  0, 0,0,0,0,11,33,'IN      '; IN      AL, DX
DB 237,  0, 0,0,1,0,12,33,'IN      '; IN      AX, DX
DB 237,  0, 0,0,1,0,13,33,'IN      '; IN      EAX, DX
DB  64,  0, 0,0,1,0, 6, 0,'INC     '; INC     reg16
DB  64,  0, 0,0,1,0, 7, 0,'INC     '; INC     reg32
DB 254,  0,16,1,1,0, 8, 0,'INC     '; INC     r/m8
DB 255,  0,16,1,1,0, 9, 0,'INC     '; INC     r/m16
DB 255,  0,16,1,1,0,10, 0,'INC     '; INC     r/m32
DB 246,  0,20,1,1,0, 8, 0,'MUL     '; MUL     r/m8
DB 247,  0,20,1,1,0, 9, 0,'MUL     '; MUL     r/m16
DB 247,  0,20,1,1,0,10, 0,'MUL     '; MUL     r/m32
DB 246,  0,19,1,1,0, 8, 0,'NEG     '; NEG     r/m8
DB 247,  0,19,1,1,0, 9, 0,'NEG     '; NEG     r/m16
DB 247,  0,19,1,1,0,10, 0,'NEG     '; NEG     r/m32
DB   0, 15,20,1,0,0, 9, 0,'VERR    '; VERR    r/m16
DB   0, 15,21,1,0,0, 9, 0,'VERW    '; VERW    r/m16
DB 246,  0,18,1,1,0, 8, 0,'NOT     '; NOT     r/m8
DB 247,  0,18,1,1,0, 9, 0,'NOT     '; NOT     r/m16
DB 247,  0,18,1,1,0,10, 0,'NOT     '; NOT     r/m32
DB   8,  0, 0,1,1,0, 8, 5,'OR      '; OR      r/m8, reg8 
DB   9,  0, 0,1,1,0, 9, 6,'OR      '; OR      r/m16, reg16 
DB   9,  0, 0,1,1,0,10, 7,'OR      '; OR      r/m32, reg32 
DB  10,  0, 0,1,1,0, 5, 8,'OR      '; OR      reg8, r/m8 
DB  11,  0, 0,1,1,0, 6, 9,'OR      '; OR      reg16, r/m16 
DB  11,  0, 0,1,1,0, 7,10,'OR      '; OR      reg32, r/m32 
DB  12,  0, 0,0,1,0,11, 2,'OR      '; OR      AL, imm8 
DB  13,  0, 0,0,1,0,12, 3,'OR      '; OR      AX, imm16 
DB  13,  0, 0,0,1,0,13, 4,'OR      '; OR      EAX, imm32 
DB 128,  0,17,1,1,0, 8, 2,'OR      '; OR      r/m8, imm8 
DB 129,  0,17,1,1,0, 9, 3,'OR      '; OR      r/m16, imm16 
DB 129,  0,17,1,1,0,10, 4,'OR      '; OR      r/m32, imm32 
DB 131,  0,17,1,1,0, 9, 2,'OR      '; OR      r/m16, imm8 
DB 131,  0,17,1,1,0,10, 2,'OR      '; OR      r/m32, imm8 
DB 230,  0, 0,0,0,0, 2,11,'OUT     '; OUT     , AL 
DB 231,  0, 0,0,1,0, 2,12,'OUT     '; OUT     , AX 
DB 231,  0, 0,0,1,0, 2,13,'OUT     '; OUT     , EAX 
DB 238,  0, 0,0,0,0,33,11,'OUT     '; OUT     DX, AL 
DB 239,  0, 0,0,1,0,33,12,'OUT     '; OUT     DX, AX 
DB 239,  0, 0,0,1,0,33,13,'OUT     '; OUT     DX, EAX 
DB  24,  0, 0,1,1,0, 8, 5,'SBB     '; SBB     r/m8, reg8 
DB  25,  0, 0,1,1,0, 9, 6,'SBB     '; SBB     r/m16, reg16 
DB  25,  0, 0,1,1,0,10, 7,'SBB     '; SBB     r/m32, reg32 
DB  26,  0, 0,1,1,0, 5, 8,'SBB     '; SBB     reg8, r/m8 
DB  27,  0, 0,1,1,0, 6, 9,'SBB     '; SBB     reg16, r/m16 
DB  27,  0, 0,1,1,0, 7,10,'SBB     '; SBB     reg32, r/m32 
DB  28,  0, 0,0,1,0,11, 2,'SBB     '; SBB     AL, imm8 
DB  29,  0, 0,0,1,0,12, 3,'SBB     '; SBB     AX, imm16 
DB  29,  0, 0,0,1,0,13, 4,'SBB     '; SBB     EAX, imm32 
DB 128,  0,19,1,1,0, 8, 2,'SBB     '; SBB     r/m8, imm8 
DB 131,  0,19,1,1,0, 9, 2,'SBB     '; SBB     r/m16, imm8 
DB 131,  0,19,1,1,0,10, 2,'SBB     '; SBB     r/m32, imm8 
DB 129,  0,19,1,1,0, 9, 3,'SBB     '; SBB     r/m16, imm16 
DB 129,  0,19,1,1,0,10, 4,'SBB     '; SBB     r/m32, imm32 
DB  40,  0, 0,1,1,0, 8, 5,'SUB     '; SUB     r/m8, reg8 
DB  41,  0, 0,1,1,0, 9, 6,'SUB     '; SUB     r/m16, reg16 
DB  41,  0, 0,1,1,0,10, 7,'SUB     '; SUB     r/m32, reg32 
DB  42,  0, 0,1,1,0, 5, 8,'SUB     '; SUB     reg8, r/m8 
DB  43,  0, 0,1,1,0, 6, 9,'SUB     '; SUB     reg16, r/m16 
DB  43,  0, 0,1,1,0, 7,10,'SUB     '; SUB     reg32, r/m32 
DB  44,  0, 0,0,1,0,11, 2,'SUB     '; SUB     AL, imm8 
DB  45,  0, 0,0,1,0,12, 3,'SUB     '; SUB     AX, imm16 
DB  45,  0, 0,0,1,0,13, 4,'SUB     '; SUB     EAX, imm32 
DB 128,  0,21,1,1,0, 8, 2,'SUB     '; SUB     r/m8, imm8 
DB 131,  0,21,1,1,0, 9, 2,'SUB     '; SUB     r/m16, imm8 
DB 131,  0,21,1,1,0,10, 2,'SUB     '; SUB     r/m32, imm8 
DB 129,  0,21,1,1,0, 9, 3,'SUB     '; SUB     r/m16, imm16 
DB 129,  0,21,1,1,0,10, 4,'SUB     '; SUB     r/m32, imm32 
DB  16,  0, 0,1,1,0, 8, 5,'ADC     '; ADC     r/m8, reg8 
DB  17,  0, 0,1,1,0, 9, 6,'ADC     '; ADC     r/m16, reg16 
DB  17,  0, 0,1,1,0,10, 7,'ADC     '; ADC     r/m32, reg32 
DB  18,  0, 0,1,1,0, 5, 8,'ADC     '; ADC     reg8, r/m8 
DB  19,  0, 0,1,1,0, 6, 9,'ADC     '; ADC     reg16, r/m16 
DB  19,  0, 0,1,1,0, 7,10,'ADC     '; ADC     reg32, r/m32 
DB  20,  0, 0,0,1,0,11, 2,'ADC     '; ADC     AL, imm8 
DB  21,  0, 0,0,1,0,12, 3,'ADC     '; ADC     AX, imm16 
DB  21,  0, 0,0,1,0,13, 4,'ADC     '; ADC     EAX, imm32 
DB 128,  0,18,1,1,0, 8, 2,'ADC     '; ADC     r/m8, imm8 
DB 131,  0,18,1,1,0, 9, 2,'ADC     '; ADC     r/m16, imm8 
DB 131,  0,18,1,1,0,10, 2,'ADC     '; ADC     r/m32, imm8 
DB 129,  0,18,1,1,0, 9, 3,'ADC     '; ADC     r/m16, imm16 
DB 129,  0,18,1,1,0,10, 4,'ADC     '; ADC     r/m32, imm32 
DB 144,  0, 0,0,1,0, 0, 0,'NOP     '; NOP     
DB  99,  0, 0,1,1,0, 9, 0,'ARPL    '; ARPL    r/m16
DB  98,  0, 0,1,1,0, 9, 0,'BOUND   '; BOUND   r/m16
DB  98,  0, 0,1,1,0,10, 0,'BOUND   '; BOUND   r/m32
DB 188, 15, 0,1,1,0, 6, 9,'BSF     '; BSF     reg16, r/m16 
DB 188, 15, 0,1,1,0, 7,10,'BSF     '; BSF     reg32, r/m32 
DB 189, 15, 0,1,1,0, 6, 9,'BSR     '; BSR     reg16, r/m16 
DB 189, 15, 0,1,1,0, 7,10,'BSR     '; BSR     reg32, r/m32 
DB 163, 15, 0,1,1,0, 9, 6,'BT      '; BT      r/m16, reg16 
DB 163, 15, 0,1,1,0,10, 7,'BT      '; BT      r/m32, reg32 
DB 186, 15,20,1,1,0, 9, 2,'BT      '; BT      r/m16, imm8 
DB 186, 15,20,1,1,0,10, 2,'BT      '; BT      r/m32, imm8 
DB 187, 15, 0,1,1,0, 9, 6,'BTC     '; BTC     r/m16, reg16 
DB 187, 15, 0,1,1,0,10, 7,'BTC     '; BTC     r/m32, reg32 
DB 186, 15,23,1,1,0, 9, 2,'BTC     '; BTC     r/m16, imm8 
DB 186, 15,23,1,1,0,10, 2,'BTC     '; BTC     r/m32, imm8 
DB 179, 15, 0,1,1,0, 9, 6,'BTR     '; BTR     r/m16, reg16 
DB 179, 15, 0,1,1,0,10, 7,'BTR     '; BTR     r/m32, reg32 
DB 186, 15,22,1,1,0, 9, 2,'BTR     '; BTR     r/m16, imm8 
DB 186, 15,22,1,1,0,10, 2,'BTR     '; BTR     r/m32, imm8 
DB 171, 15, 0,1,1,0, 9, 6,'BTS     '; BTS     r/m16, reg16 
DB 171, 15, 0,1,1,0,10, 7,'BTS     '; BTS     r/m32, reg32 
DB 186, 15,21,1,1,0, 9, 2,'BTS     '; BTS     r/m16, imm8 
DB 186, 15,21,1,1,0,10, 2,'BTS     '; BTS     r/m32, imm8 
DB   2, 15, 0,1,0,0, 6, 9,'LAR     '; LAR     reg16, r/m16 
DB 141,  0, 0,1,1,0, 6, 9,'LEA     '; LEA     reg16, rm16
DB 141,  0, 0,1,1,0, 7,10,'LEA     '; LEA     reg32, rm32
DB 166,  0, 0,0,1,0, 0, 0,'CMPSB   '; CMPSB   
DB 167,102, 0,0,1,0, 0, 0,'CMPSW   '; CMPSW   
DB 167,  0, 0,0,1,0, 0, 0,'CMPSD   '; CMPSD   
DB 108,  0, 0,0,1,0, 0, 0,'INSB    '; INSB    
DB 215,  0, 0,0,1,0, 0, 0,'XLATB   '; XLATB   
DB 109,102, 0,0,1,0, 0, 0,'INSW    '; INSW    
DB 109,  0, 0,0,1,0, 0, 0,'INSD    '; INSD    
DB 230,  0, 0,0,1,0, 0, 0,'OUTSB   '; OUTSB   
DB 231,102, 0,0,1,0, 0, 0,'OUTSW   '; OUTSW   
DB 231,  0, 0,0,1,0, 0, 0,'OUTSD   '; OUTSD   
DB 174,  0, 0,0,1,0, 0, 0,'SCASB   '; SCASB   
DB 175,102, 0,0,1,0, 0, 0,'SCASW   '; SCASW   
DB 175,  0, 0,0,1,0, 0, 0,'SCASD   '; SCASD   
DB 172,  0, 0,0,1,0, 0, 0,'LODSB   '; LODSB   
DB 173,102, 0,0,1,0, 0, 0,'LODSW   '; LODSW   
DB 173,  0, 0,0,1,0, 0, 0,'LODSD   '; LODSD   
DB 164,  0, 0,0,1,0, 0, 0,'MOVSB   '; MOVSB   
DB 165,102, 0,0,1,0, 0, 0,'MOVSW   '; MOVSW   
DB 165,  0, 0,0,1,0, 0, 0,'MOVSD   '; MOVSD   
DB 170,  0, 0,0,1,0, 0, 0,'STOSB   '; STOSB   
DB 171,102, 0,0,1,0, 0, 0,'STOSW   '; STOSW   
DB 171,  0, 0,0,1,0, 0, 0,'STOSD   '; STOSD   
DB 0b0h,15, 0,1,1,0, 8, 5,'CMPXCHG '; CMPXCHG r/m8, reg8
DB 0b1h,15, 0,1,1,0, 9, 6,'CMPXCHG '; CMPXCHG r/m16, reg16
DB 0b1h,15, 0,1,1,0,10, 7,'CMPXCHG '; CMPXCHG r/m32, reg32
DB 200, 15, 0,0,0,0, 7, 0,'BSWAP   '; BSWAP   reg32
DB 200,  0, 0,0,0,0, 3, 2,'ENTER   '; ENTER   , imm8 
DB 144,  0, 0,0,1,0,12, 6,'XCHG    '; XCHG    AX, reg16 
DB 144,  0, 0,0,1,0,13, 7,'XCHG    '; XCHG    EAX, reg32 
DB 134,  0, 0,1,1,0, 8, 5,'XCHG    '; XCHG    r/m8, reg8 
DB 135,  0, 0,1,1,0, 9, 6,'XCHG    '; XCHG    r/m16, reg16 
DB 135,  0, 0,1,1,0,10, 7,'XCHG    '; XCHG    r/m32, reg32 
DB 134,  0, 0,1,1,0, 5, 8,'XCHG    '; XCHG    reg8, r/m8 
DB 135,  0, 0,1,1,0, 6, 9,'XCHG    '; XCHG    reg16, r/m16 
DB 135,  0, 0,1,1,0, 7,10,'XCHG    '; XCHG    reg32, r/m32
DB 205,  0, 0,0,1,0, 2, 0,'INT     '; INT     
DB  55,  0, 0,0,1,0, 0, 0,'AAA     '; AAA     
DB  10,213, 0,0,1,0, 0, 0,'AAD     '; AAD     
DB  10,212, 0,0,1,0, 0, 0,'AAM     '; AAM     
DB  63,  0, 0,0,1,0, 0, 0,'AAS     '; AAS     
DB 152,  0, 0,0,1,0, 0, 0,'CBW     '; CBW     
DB 153,  0, 0,0,1,0, 0, 0,'CDQ     '; CDQ     
DB 248,  0, 0,0,1,0, 0, 0,'CLC     '; CLC     
DB 252,  0, 0,0,1,0, 0, 0,'CLD     '; CLD     
DB 250,  0, 0,0,1,0, 0, 0,'CLI     '; CLI     
DB 245,  0, 0,0,1,0, 0, 0,'CMC     '; CMC     
DB   6, 15, 0,0,1,0, 0, 0,'CLTS    '; CLTS    
DB 153,102, 0,0,1,0, 0, 0,'CWD     '; CWD     
DB 152,102, 0,0,1,0, 0, 0,'CWDE    '; CWDE    
DB  39,  0, 0,0,1,0, 0, 0,'DAA     '; DAA     
DB  47,  0, 0,0,1,0, 0, 0,'DAS     '; DAS     
DB 244,  0, 0,0,1,0, 0, 0,'HLT     '; HLT     
DB   1, 15,23,0,1,0, 0, 0,'INVLPG  '; INVLPG  
DB 207,  0, 0,0,1,0, 0, 0,'IRET    '; IRET    
DB 207,102, 0,0,1,0, 0, 0,'IRETD   '; IRETD
DB   8, 15, 0,0,1,0, 0, 0,'INVD    '; INVD    
DB 204,  0, 0,0,1,0, 0, 0,'INT3    '; INT3    
DB 206,  0, 0,0,1,0, 0, 0,'INTO    '; INTO    
DB 159,  0, 0,0,1,0, 0, 0,'LAHF    '; LAHF    
DB 201,  0, 0,0,1,0, 0, 0,'LEAVE   '; LEAVE   
DB 240,  0, 0,0,1,0, 0, 0,'LOCK    '; LOCK    
DB  97,  0, 0,0,1,0, 0, 0,'POPA    '; POPA    
DB  97,102, 0,0,1,0, 0, 0,'POPAD   '; POPAD   
DB 157,  0, 0,0,1,0, 0, 0,'POPF    '; POPF    
DB 157,102, 0,0,1,0, 0, 0,'POPFD   '; POPFD   
DB  96,  0, 0,0,1,0, 0, 0,'PUSHA   '; PUSHA   
DB  96,102, 0,0,1,0, 0, 0,'PUSHAD  '; PUSHAD  
DB 156,  0, 0,0,1,0, 0, 0,'PUSHF   '; PUSHF   
DB 156,102, 0,0,1,0, 0, 0,'PUSHFD  '; PUSHFD  
DB 158,  0, 0,0,1,0, 0, 0,'SAHF    '; SAHF    
DB 159,  0, 0,0,1,0, 0, 0,'LAHF    '; LAHF    
DB 249,  0, 0,0,1,0, 0, 0,'STC     '; STC     
DB 253,  0, 0,0,1,0, 0, 0,'STD     '; STD     
DB 251,  0, 0,0,1,0, 0, 0,'STI     '; STI     
DB   9, 15, 0,0,1,0, 0, 0,'WBINVD  '; WBINVD
DB 117,  0, 0,0,0,0,23, 0,'JNZ     '; JNZ     rel8
DB 133, 15, 0,0,0,0,24, 0,'JNZ     '; JNZ     rel16
DB 116,  0, 0,0,0,0,23, 0,'JZ      '; JZ      rel8
DB 132, 15, 0,0,0,0,24, 0,'JZ      '; JZ      rel16
DB 116,  0, 0,0,0,0,23, 0,'JE      '; JE      rel8
DB 132, 15, 0,0,0,0,24, 0,'JE      '; JE      rel16
DB 117,  0, 0,0,0,0,23, 0,'JNE     '; JNE     rel8
DB 133, 15, 0,0,0,0,24, 0,'JNE     '; JNE     rel16
DB 121,  0, 0,0,0,0,23, 0,'JNS     '; JNS     rel8
DB 137, 15, 0,0,0,0,24, 0,'JNS     '; JNS     rel16
DB 120,  0, 0,0,0,0,23, 0,'JS      '; JS      rel8
DB 136, 15, 0,0,0,0,24, 0,'JS      '; JS      rel16
DB 117,  0, 0,0,0,0,23, 0,'JNA     '; JNA     rel8
DB 134, 15, 0,0,0,0,24, 0,'JNA     '; JNA     rel16
DB 119,  0, 0,0,0,0,23, 0,'JA      '; JA      rel8
DB 135, 15, 0,0,0,0,24, 0,'JA      '; JA      rel16
DB 115,  0, 0,0,0,0,23, 0,'JAE     '; JAE     rel8
DB 131, 15, 0,0,0,0,24, 0,'JAE     '; JAE     rel16
DB 114,  0, 0,0,0,0,23, 0,'JNAE    '; JNAE    rel8
DB 130, 15, 0,0,0,0,24, 0,'JNAE    '; JNAE    rel16
DB 114,  0, 0,0,0,0,23, 0,'JB      '; JB      rel8
DB 130, 15, 0,0,0,0,24, 0,'JB      '; JB      rel16
DB 115,  0, 0,0,0,0,23, 0,'JNB     '; JNB     rel8
DB 131, 15, 0,0,0,0,24, 0,'JNB     '; JNB     rel16
DB 118,  0, 0,0,0,0,23, 0,'JBE     '; JBE     rel8
DB 134, 15, 0,0,0,0,24, 0,'JBE     '; JBE     rel16
DB 119,  0, 0,0,0,0,23, 0,'JNBE    '; JNBE    rel8
DB 135, 15, 0,0,0,0,24, 0,'JNBE    '; JNBE    rel16
DB 112,  0, 0,0,0,0,23, 0,'JO      '; JO      rel8
DB 128, 15, 0,0,0,0,24, 0,'JO      '; JO      rel16
DB 113,  0, 0,0,0,0,23, 0,'JNO     '; JNO     rel8
DB 129, 15, 0,0,0,0,24, 0,'JNO     '; JNO     rel16
DB 114,  0, 0,0,0,0,23, 0,'JC      '; JC      rel8
DB 130, 15, 0,0,0,0,24, 0,'JC      '; JC      rel16
DB 115,  0, 0,0,0,0,23, 0,'JNC     '; JNC     rel8
DB 131, 15, 0,0,0,0,24, 0,'JNC     '; JNC     rel16
DB 124,  0, 0,0,0,0,23, 0,'JL      '; JL      rel8
DB 140, 15, 0,0,0,0,24, 0,'JL      '; JL      rel16
DB 125,  0, 0,0,0,0,23, 0,'JNL     '; JNL     rel8
DB 237, 15, 0,0,0,0,24, 0,'JNL     '; JNL     rel16
DB 126,  0, 0,0,0,0,23, 0,'JLE     '; JLE     rel8
DB 142, 15, 0,0,0,0,24, 0,'JLE     '; JLE     rel16
DB 127,  0, 0,0,0,0,23, 0,'JNLE    '; JNLE    rel8
DB 143, 15, 0,0,0,0,24, 0,'JNLE    '; JNLE    rel16
DB 122,  0, 0,0,0,0,23, 0,'JP      '; JP      rel8
DB 138, 15, 0,0,0,0,24, 0,'JP      '; JP      rel16
DB 123,  0, 0,0,0,0,23, 0,'JPO     '; JPO     rel8
DB 171, 15, 0,0,0,0,24, 0,'JPO     '; JPO     rel16
DB 123,  0, 0,0,0,0,23, 0,'JNP     '; JNP     rel8
DB 139, 15, 0,0,0,0,24, 0,'JNP     '; JNP     rel16
DB 127,  0, 0,0,0,0,23, 0,'JG      '; JG      rel8
DB 143, 15, 0,0,0,0,24, 0,'JG      '; JG      rel16
DB 125,  0, 0,0,0,0,23, 0,'JGE     '; JGE     rel8
DB 141, 15, 0,0,0,0,24, 0,'JGE     '; JGE     rel16
DB 126,  0, 0,0,0,0,23, 0,'JNG     '; JNG     rel8
DB 142, 15, 0,0,0,0,24, 0,'JNG     '; JNG     rel16
DB 235,  0, 0,0,0,0,23, 0,'JMP     '; JMP     rel8
DB 233,  0, 0,0,0,0,24, 0,'JMP     '; JMP     rel16
DB 233, 15, 0,0,0,0,25, 0,'JMP     '; JMP     rel32
DB 255,  0, 16+5,0,0,0,10, 0,'JMP     '; JMP     rm32
DB 226,  0, 0,0,0,0,23, 0,'LOOP    '; LOOP    rel8
DB 225,  0, 0,0,0,0,23, 0,'LOOPE   '; LOOPE   rel8
DB 225,  0, 0,0,0,0,23, 0,'LOOPZ   '; LOOPZ   rel8
DB 224,  0, 0,0,0,0,23, 0,'LOOPNE  '; LOOPNE  rel8
DB 224,  0, 0,0,0,0,23, 0,'LOOPNZ  '; LOOPNZ  rel8
DB 226,102, 0,0,0,0,23, 0,'LOOPD   '; LOOPD   rel8
DB 225,102, 0,0,0,0,23, 0,'LOOPDE  '; LOOPDE  rel8
DB 225,102, 0,0,0,0,23, 0,'LOOPDZ  '; LOOPDZ  rel8
DB 224,102, 0,0,0,0,23, 0,'LOOPDNE '; LOOPDNE rel8
DB 224,102, 0,0,0,0,23, 0,'LOOPDNZ '; LOOPDNZ rel8
DB 172, 15, 0,1,1,0, 9, 2,'SHRD    '; SHRD    r/m16, imm8 
DB 172, 15, 0,1,1,0,10, 2,'SHRD    '; SHRD    r/m32, imm8 
DB 173, 15, 0,1,1,0, 9, 6,'SHRD    '; SHRD    r/m16, reg16 
DB 173, 15, 0,1,1,0,10, 7,'SHRD    '; SHRD    r/m32, reg32 
DB 164, 15, 0,1,1,0, 9, 2,'SHLD    '; SHLD    r/m16, imm8 
DB 164, 15, 0,1,1,0,10, 2,'SHLD    '; SHLD    r/m32, imm8 
DB 165, 15, 0,1,1,0, 9, 6,'SHLD    '; SHLD    r/m16, reg16 
DB 165, 15, 0,1,1,0,10, 7,'SHLD    '; SHLD    r/m32, reg32 
DB 192,  0,20,1,1,0, 8, 2,'SHL     '; SHL     r/m8, imm8 
DB 211,  0,20,1,1,0, 9,34,'SHL     '; SHL     r/m16, CL
DB 193,  0,20,1,1,0, 9, 2,'SHL     '; SHL     r/m16, imm8 
DB 211,  0,20,1,1,0,10,34,'SHL     '; SHL     r/m32, CL
DB 193,  0,20,1,1,0,10, 2,'SHL     '; SHL     r/m32, imm8 
DB 192,  0,20,1,1,0, 8, 2,'SAL     '; SAL     r/m8, imm8 
DB 211,  0,20,1,1,0, 9,34,'SAL     '; SAL     r/m16, CL
DB 193,  0,20,1,1,0, 9, 2,'SAL     '; SAL     r/m16, imm8 
DB 211,  0,20,1,1,0,10,34,'SAL     '; SAL     r/m32, CL
DB 193,  0,20,1,1,0,10, 2,'SAL     '; SAL     r/m32, imm8 
DB 192,  0,21,1,1,0, 8, 2,'SHR     '; SHR     r/m8, imm8 
DB 211,  0,21,1,1,0, 9,34,'SHR     '; SHR     r/m16, CL
DB 193,  0,21,1,1,0, 9, 2,'SHR     '; SHR     r/m16, imm8 
DB 211,  0,21,1,1,0,10,34,'SHR     '; SHR     r/m32, CL
DB 193,  0,21,1,1,0,10, 2,'SHR     '; SHR     r/m32, imm8 
DB 192,  0,23,1,1,0, 8, 2,'SAR     '; SAR     r/m8, imm8 
DB 211,  0,23,1,1,0, 9,34,'SAR     '; SAR     r/m16, CL
DB 193,  0,23,1,1,0, 9, 2,'SAR     '; SAR     r/m16, imm8 
DB 211,  0,23,1,1,0,10,34,'SAR     '; SAR     r/m32, CL
DB 193,  0,23,1,1,0,10, 2,'SAR     '; SAR     r/m32, imm8 
DB 192,  0,18,1,1,0, 8, 2,'RCL     '; RCL     r/m8, imm8 
DB 211,  0,18,1,1,0, 9,34,'RCL     '; RCL     r/m16, CL
DB 193,  0,18,1,1,0, 9, 2,'RCL     '; RCL     r/m16, imm8 
DB 211,  0,18,1,1,0,10,34,'RCL     '; RCL     r/m32, CL
DB 193,  0,18,1,1,0,10, 2,'RCL     '; RCL     r/m32, imm8 
DB 192,  0,16,1,1,0, 8, 2,'ROL     '; ROL     r/m8, imm8 
DB 211,  0,16,1,1,0, 9,34,'ROL     '; ROL     r/m16, CL
DB 193,  0,16,1,1,0, 9, 2,'ROL     '; ROL     r/m16, imm8 
DB 211,  0,16,1,1,0,10,34,'ROL     '; ROL     r/m32, CL
DB 193,  0,16,1,1,0,10, 2,'ROL     '; ROL     r/m32, imm8 
DB 192,  0,19,1,1,0, 8, 2,'RCR     '; RCR     r/m8, imm8 
DB 211,  0,19,1,1,0, 9,34,'RCR     '; RCR     r/m16, CL
DB 193,  0,19,1,1,0, 9, 2,'RCR     '; RCR     r/m16, imm8 
DB 211,  0,19,1,1,0,10,34,'RCR     '; RCR     r/m32, CL
DB 193,  0,19,1,1,0,10, 2,'RCR     '; RCR     r/m32, imm8 
DB 192,  0,17,1,1,0, 8, 2,'ROR     '; ROR     r/m8, imm8 
DB 211,  0,17,1,1,0, 9,34,'ROR     '; ROR     r/m16, CL
DB 193,  0,17,1,1,0, 9, 2,'ROR     '; ROR     r/m16, imm8 
DB 211,  0,17,1,1,0,10,34,'ROR     '; ROR     r/m32, CL
DB 193,  0,17,1,1,0,10, 2,'ROR     '; ROR     r/m32, imm8 
DB 132,  0, 0,1,1,0, 8, 5,'TEST    '; TEST    r/m8, reg8 
DB 133,  0, 0,1,1,0, 9, 6,'TEST    '; TEST    r/m16, reg16 
DB 133,  0, 0,1,1,0,10, 7,'TEST    '; TEST    r/m32, reg32 
DB 168,  0, 0,0,1,0,11, 2,'TEST    '; TEST    AL, imm8 
DB 169,  0, 0,0,1,0,12, 3,'TEST    '; TEST    AX, imm16 
DB 169,  0, 0,0,1,0,13, 4,'TEST    '; TEST    EAX, imm32 
DB 246,  0,16,1,1,0, 8, 2,'TEST    '; TEST    r/m8, imm8 
DB 247,  0,16,1,1,0, 9, 3,'TEST    '; TEST    r/m16, imm16 
DB 247,  0,16,1,1,0,10, 4,'TEST    '; TEST    r/m32, imm32 
DB 182, 15, 0,1,1,0, 6, 8,'MOVZX   '; MOVZX   reg16, r/m8 
DB 182, 15, 0,1,1,0, 7, 8,'MOVZX   '; MOVZX   reg32, r/m8 
DB 183, 15, 0,1,1,0, 7, 9,'MOVZX   '; MOVZX   reg32, r/m16 
DB 190, 15, 0,1,1,0, 6, 8,'MOVSX   '; MOVSX   reg16, r/m8 
DB 190, 15, 0,1,1,0, 7, 8,'MOVSX   '; MOVSX   reg32, r/m8 
DB 191, 15, 0,1,1,0, 7, 9,'MOVSX   '; MOVSX   reg32, r/m16 
DB 240,217, 0,0,0,1, 0, 0,'F2XM1   '; F2XM1   
DB 225,217, 0,0,0,1, 0, 0,'FABS    '; FABS
DB 222,  0,20,1,0,1, 9, 0,'FISUB   '; FISUB   mem16
DB 218,  0,20,1,0,1,10, 0,'FISUB   '; FISUB   mem32
DB 222,  0,21,1,0,1, 9, 0,'FISUBR  '; FISUBR  mem16
DB 218,  0,21,1,0,1,10, 0,'FISUBR  '; FISUBR  mem32
DB 223,  0,18,1,0,1, 9, 0,'FIST    '; FIST    mem16
DB 219,  0,18,1,0,1,10, 0,'FIST    '; FIST    mem32
DB 223,  0,19,1,0,1, 9, 0,'FISTP   '; FISTP   mem16
DB 219,  0,19,1,0,1,10, 0,'FISTP   '; FISTP   mem32
DB 222,  0,17,1,0,1, 9, 0,'FIMUL   '; FIMUL   mem16
DB 218,  0,17,1,0,1,10, 0,'FIMUL   '; FIMUL   mem32
DB 222,  0,19,1,0,1, 9, 0,'FICOMP  '; FICOMP  mem16
DB 218,  0,19,1,0,1,10, 0,'FICOMP  '; FICOMP  mem32
DB 222,  0,23,1,0,1, 9, 0,'FIDIVR  '; FIDIVR  mem16
DB 218,  0,23,1,0,1,10, 0,'FIDIVR  '; FIDIVR  mem32
DB 222,  0,22,1,0,1, 9, 0,'FIDIV   '; FIDIV   mem16
DB 218,  0,22,1,0,1,10, 0,'FIDIV   '; FIDIV   mem32
DB 222,  0,18,1,0,1, 9, 0,'FICOM   '; FICOM   mem16
DB 218,  0,18,1,0,1,10, 0,'FICOM   '; FICOM   mem32
DB 222,  0, 0,1,0,1, 9, 0,'FIADD   '; FIADD   mem16
DB 218,  0, 0,1,0,1,10, 0,'FIADD   '; FIADD   mem32
DB 217,  0, 0,1,0,1,22, 0,'FLD     '; FLD     st(i)
DB 217,  0, 0,1,0,1,29, 0,'FLD     '; FLD     mem32
DB 221,  0, 0,1,0,1,31, 0,'FLD     '; FLD     mem64
DB 219,  0,21,1,0,1,32, 0,'FLD     '; FLD     mem80
DB 223,  0, 0,1,0,1,28, 0,'FILD    '; FILD    mem16
DB 219,  0, 0,1,0,1,29, 0,'FILD    '; FILD    mem32
DB 223,  0,21,1,0,1,31, 0,'FILD    '; FILD    mem64
DB 216,  0,18,1,0,1,22, 0,'FCOM    '; FCOM    st(i)
DB 220,  0,18,1,0,1,31, 0,'FCOM    '; FCOM    mem64
DB 216,  0,18,1,0,1,29, 0,'FCOM    '; FCOM    mem32
DB 216,  0,19,1,0,1,22, 0,'FCOMP   '; FCOMP   st(i)
DB 220,  0,19,1,0,1,31, 0,'FCOMP   '; FCOMP   mem64
DB 216,  0,19,1,0,1,29, 0,'FCOMP   '; FCOMP   mem32
DB 221,  0,18,1,0,1,22, 0,'FST     '; FST     st(i)
DB 217,  0,18,1,0,1,29, 0,'FST     '; FST     mem32
DB 221,  0,18,1,0,1,31, 0,'FST     '; FST     mem64
DB 221,  0,19,1,0,1,22, 0,'FSTP    '; FSTP    st(i)
DB 217,  0,19,1,0,1,29, 0,'FSTP    '; FSTP    mem32
DB 221,  0,19,1,0,1,31, 0,'FSTP    '; FSTP    mem64
DB 219,  0,23,1,0,1,32, 0,'FSTP    '; FSTP    mem80
DB 223,  0,20,1,0,1,32, 0,'FBLD    '; FBLD    mem80
DB 217,  0,21,1,0,1,28, 0,'FLDCW   '; FLDCW   mem16
DB 217,  0,23,1,0,1,28, 0,'FSTCW   '; FSTCW   mem16
DB 223,  0,20,1,0,1,12, 0,'FSTSW   '; FSTSW   AX
DB 221,  0,23,1,0,1, 9, 0,'FSTSW   '; FSTSW   r/m16
DB 217,  0,20,1,0,1, 9, 0,'FLDENV  '; FLDENV  r/m16
DB 221,  0,20,1,0,1, 9, 0,'FRSTOR  '; FRSTOR  r/m16
DB 221,  0,22,1,0,1, 9, 0,'FSAVE   '; FSAVE   r/m16
DB 221,  0,20,1,0,1,22, 0,'FUCOM   '; FUCOM   st(i)
DB 221,  0,21,1,0,1,22, 0,'FUCOMP  '; FUCOMP  st(i)
DB 217,  0,17,1,0,1,22, 0,'FXCH    '; FXCH    st(i)
DB 233,218, 0,0,0,1, 0, 0,'FUCOMPP '; FUCOMPP 
DB 216,  0,22,1,0,1,21,22,'FDIV    '; FDIV    st, st(i) 
DB 220,  0,23,1,0,1,22,21,'FDIV    '; FDIV    st(i), st
DB 216,  0,22,1,0,1,29, 0,'FDIV    '; FDIV    mem32
DB 220,  0,22,1,0,1,31, 0,'FDIV    '; FDIV    mem64
DB 222,  0,22,1,0,1,22,21,'FDIVP   '; FDIVP   st(i), st 
DB 216,  0,23,1,0,1,21,22,'FDIVR   '; FDIVR   st, st(i) 
DB 220,  0,22,1,0,1,22,21,'FDIVR   '; FDIVR   st(i), st 
DB 216,  0,23,1,0,1,29, 0,'FDIVR   '; FDIVR   mem32
DB 220,  0,23,1,0,1,31, 0,'FDIVR   '; FDIVR   mem64
DB 222,  0,22,1,0,1,22,21,'FDIVRP  '; FDIVRP  st(i), st 
DB 216,  0, 0,1,0,1,21,22,'FADD    '; FADD    st, st(i) 
DB 220,  0, 0,1,0,1,22,21,'FADD    '; FADD    st(i), st 
DB 216,  0, 0,1,0,1,29, 0,'FADD    '; FADD    mem32
DB 220,  0, 0,1,0,1,31, 0,'FADD    '; FADD    mem64
DB 222,  0, 0,1,0,1,22,21,'FADDP   '; FADDP   st(i), st 
DB 216,  0,17,1,0,1,21,22,'FMUL    '; FMUL    st, st(i) 
DB 220,  0,17,1,0,1,22,21,'FMUL    '; FMUL    st(i), st 
DB 216,  0,17,1,0,1,29, 0,'FMUL    '; FMUL    mem32
DB 220,  0,17,1,0,1,31, 0,'FMUL    '; FMUL    mem64
DB 222,  0,17,1,0,1,22,21,'FMULP   '; FMULP   st(i), st 
DB 216,  0,20,1,0,1,21,22,'FSUB    '; FSUB    st, st(i)
DB 220,  0,21,1,0,1,22,21,'FSUB    '; FSUB    st(i), st 
DB 216,  0,20,1,0,1,29, 0,'FSUB    '; FSUB    mem32
DB 220,  0,20,1,0,1,31, 0,'FSUB    '; FSUB    mem64
DB 222,  0,21,1,0,1,22,21,'FSUBP   '; FSUBP   st(i), st 
DB 216,  0,21,1,0,1,21,22,'FSUBR   '; FSUBR   st, st(i) 
DB 220,  0,20,1,0,1,22,21,'FSUBR   '; FSUBR   st(i), st 
DB 216,  0,21,1,0,1,29, 0,'FSUBR   '; FSUBR   mem32
DB 220,  0,21,1,0,1,31, 0,'FSUBR   '; FSUBR   mem64
DB 222,  0,20,1,0,1,22,21,'FSUBRP  '; FSUBRP  st(i), st 
DB 217,  0,22,1,0,1, 9, 0,'FSTENV  '; FSTENV  r/m16
DB 217,  0,22,1,0,1, 9, 0,'FNSTENV '; FNSTENV r/m16
DB 224,217, 0,0,0,1, 0, 0,'FCHS    '; FCHS    
DB 226,219, 0,0,0,1, 0, 0,'FCLEX   '; FCLEX   
DB 217,222, 0,0,0,1, 0, 0,'FCOMPP  '; FCOMPP  
DB 255,217, 0,0,0,1, 0, 0,'FCOS    '; FCOS    
DB 246,217, 0,0,0,1, 0, 0,'FDECSTP '; FDECSTP 
DB 247,217, 0,0,0,1, 0, 0,'FINCSTP '; FINCSTP 
DB 227,219, 0,0,0,1, 0, 0,'FINIT   '; FINIT   
DB 227,219, 0,0,0,1, 0, 0,'FNINIT  '; FNINIT
DB 236,217, 0,0,0,1, 0, 0,'FLDLG2  '; FLDLG2  
DB 237,217, 0,0,0,1, 0, 0,'FLDLN2  '; FLDLN2  
DB 234,217, 0,0,0,1, 0, 0,'FLDL2E  '; FLDL2E  
DB 233,217, 0,0,0,1, 0, 0,'FLDL2T  '; FLDL2T  
DB 235,217, 0,0,0,1, 0, 0,'FLDPI   '; FLDPI   
DB 238,217, 0,0,0,1, 0, 0,'FLDZ    '; FLDZ    
DB 232,217, 0,0,0,1, 0, 0,'FLD1    '; FLD1    
DB 208,217, 0,0,0,1, 0, 0,'FNOP    '; FNOP    
DB 243,217, 0,0,0,1, 0, 0,'FPATAN  '; FPATAN  
DB 248,217, 0,0,0,1, 0, 0,'FPREM   '; FPREM   
DB 245,217, 0,0,0,1, 0, 0,'FPREM1  '; FPREM1  
DB 242,217, 0,0,0,1, 0, 0,'FPTAN   '; FPTAN   
DB 252,217, 0,0,0,1, 0, 0,'FRNDINT '; FRNDINT 
DB 253,217, 0,0,0,1, 0, 0,'FSCALE  '; FSCALE  
DB 228,219, 0,0,0,1, 0, 0,'FSETPM  '; FSETPM  
DB 254,217, 0,0,0,1, 0, 0,'FSIN    '; FSIN    
DB 251,217, 0,0,0,1, 0, 0,'FSINCOS '; FSINCOS 
DB 250,217, 0,0,0,1, 0, 0,'FSQRT   '; FSQRT   
DB 228,217, 0,0,0,1, 0, 0,'FTST    '; FTST    
DB 155,  0, 0,0,0,1, 0, 0,'FWAIT   '; FWAIT   
DB 155,  0, 0,0,0,1, 0, 0,'WAIT    '; WAIT    
DB 229,217, 0,0,0,1, 0, 0,'FXAM    '; FXAM    
DB 245,217, 0,0,0,1, 0, 0,'FXTRACT '; FXTRACT 
DB 241,217, 0,0,0,1, 0, 0,'FYL2X   '; FYL2X   
DB 249,217, 0,0,0,1, 0, 0,'FYL2XP1 '; FYL2XP1 
DB 240,217, 0,0,0,1, 0, 0,'F2XM1   '; F2XM1   
DB 223,  0,22,1,0,1,32, 0,'FBSTP   '; FBSTP   mem80
DB 221,  0, 0,1,0,1,22, 0,'FFREE   '; FFREE   st(i)
DB 223,  0, 0,1,0,1,22, 0,'FFREEP  '; FFREEP  st(i)
DB 192, 15, 0,1,1,0, 8, 5,'XADD    '; XADD    r/m8, reg8
DB 193, 15, 0,1,1,0, 9, 6,'XADD    '; XADD    r/m16, reg16
DB 193, 15, 0,1,1,0,10, 7,'XADD    '; XADD    r/m32, reg32
DB   1, 15,18,1,0,0,28, 0,'LGDT    '; LGDT    mem16
DB   1, 15,18,1,0,0,29, 0,'LGDT    '; LGDT    mem32
DB   1, 15,19,1,0,0,29, 0,'LIDT    '; LIDT    mem32
DB   1, 15,19,1,0,0,28, 0,'LIDT    '; LIDT    mem16
DB   1, 15,16,1,0,0,29, 0,'SGDT    '; SGDT    mem32
DB   1, 15,16,1,0,0,28, 0,'SGDT    '; SGDT    mem16
DB   1, 15,17,1,0,0,28, 0,'SIDT    '; SIDT    mem16
DB   1, 15,17,1,0,0,29, 0,'SIDT    '; SIDT    mem32
DB   0, 15,18,1,0,0,28, 0,'LLDT    '; LLDT    mem16
DB   1, 15,22,1,0,0, 9, 0,'LMSW    '; LMSW    r/m16
DB   1, 15,20,1,0,0, 9, 0,'SMSW    '; SMSW    r/m16
DB   0, 15,19,1,0,0, 9, 0,'LTR     '; LTR     r/m16
DB   0, 15,17,1,0,0, 9, 0,'STR     '; STR     r/m16
DB   3, 15, 0,1,0,0, 6, 9,'LSL     '; LSL     reg16, r/m16 
DB   3, 15, 0,1,0,0, 7,10,'LSL     '; LSL     reg32, r/m32 
DB 181, 15, 0,1,1,0, 6,29,'LGS     '; LGS     reg16, mem32
DB 181, 15, 0,1,1,0, 7,30,'LGS     '; LGS     reg32, mem48
DB 180, 15, 0,1,1,0, 6,29,'LFS     '; LFS     reg16, mem32
DB 180, 15, 0,1,1,0, 7,30,'LFS     '; LFS     reg32, mem48
DB 197,  0, 0,1,1,0, 6,29,'LDS     '; LDS     reg16, mem32
DB 197,  0, 0,1,1,0, 7,30,'LDS     '; LDS     reg32, mem48
DB 196,  0, 0,1,1,0, 6,29,'LES     '; LES     reg16, mem32
DB 196,  0, 0,1,1,0, 7,30,'LES     '; LES     reg32, mem48
DB 178, 15, 0,1,1,0, 6,29,'LSS     '; LSS     reg16, mem32
DB 178, 15, 0,1,1,0, 7,30,'LSS     '; LSS     reg32, mem48
DB 151, 15, 0,1,0,0, 8, 0,'SETA    '; SETA    r/m8
DB 147, 15, 0,1,0,0, 8, 0,'SETAE   '; SETAE   r/m8
DB 146, 15, 0,1,0,0, 8, 0,'SETB    '; SETB    r/m8
DB 150, 15, 0,1,0,0, 8, 0,'SETBE   '; SETBE   r/m8
DB 146, 15, 0,1,0,0, 8, 0,'SETC    '; SETC    r/m8
DB 148, 15, 0,1,0,0, 8, 0,'SETE    '; SETE    r/m8
DB 159, 15, 0,1,0,0, 8, 0,'SETG    '; SETG    r/m8
DB 157, 15, 0,1,0,0, 8, 0,'SETGE   '; SETGE   r/m8
DB 156, 15, 0,1,0,0, 8, 0,'SETL    '; SETL    r/m8
DB 158, 15, 0,1,0,0, 8, 0,'SETLE   '; SETLE   r/m8
DB 150, 15, 0,1,0,0, 8, 0,'SETNA   '; SETNA   r/m8
DB 146, 15, 0,1,0,0, 8, 0,'SETNAE  '; SETNAE  r/m8
DB 147, 15, 0,1,0,0, 8, 0,'SETNB   '; SETNB   r/m8
DB 151, 15, 0,1,0,0, 8, 0,'SETNBE  '; SETNBE  r/m8
DB 147, 15, 0,1,0,0, 8, 0,'SETNC   '; SETNC   r/m8
DB 149, 15, 0,1,0,0, 8, 0,'SETNE   '; SETNE   r/m8
DB 158, 15, 0,1,0,0, 8, 0,'SETNG   '; SETNG   r/m8
DB 156, 15, 0,1,0,0, 8, 0,'SETNGE  '; SETNGE  r/m8
DB 157, 15, 0,1,0,0, 8, 0,'SETNL   '; SETNL   r/m8
DB 159, 15, 0,1,0,0, 8, 0,'SETNLE  '; SETNLE  r/m8
DB 145, 15, 0,1,0,0, 8, 0,'SETNO   '; SETNO   r/m8
DB 155, 15, 0,1,0,0, 8, 0,'SETNP   '; SETNP   r/m8
DB 153, 15, 0,1,0,0, 8, 0,'SETNS   '; SETNS   r/m8
DB 149, 15, 0,1,0,0, 8, 0,'SETNZ   '; SETNZ   r/m8
DB 144, 15, 0,1,0,0, 8, 0,'SETO    '; SETO    r/m8
DB 154, 15, 0,1,0,0, 8, 0,'SETP    '; SETP    r/m8
DB 154, 15, 0,1,0,0, 8, 0,'SETPE   '; SETPE   r/m8
DB 155, 15, 0,1,0,0, 8, 0,'SETPO   '; SETPO   r/m8
DB 152, 15, 0,1,0,0, 8, 0,'SETS    '; SETS    r/m8
DB 148, 15, 0,1,0,0, 8, 0,'SETZ    '; SETZ    r/m8
OPCODECOUNT DW 1252
