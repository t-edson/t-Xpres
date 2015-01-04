.COMMENT
 ****************************************************************************
 HELLO3.ASM     Example file for DOSX - extended DOS Support
                (c) 1996,1998 by Dieter Pawelczak - Pass32 Version 2.0
 ============================================================================
 ****************************************************************************
.MODEL FLAT
.UCU

.INCLUDE DOSX.INC         ; include extended DOS library ...

.DATA
     mesg      db         'Hello, World - with extended DOS!',13,10,'$'
.CODE
START:
     mov edx,OFFSET mesg  ; offset to text string
     mov ah,9h            ; extended dos function string to standard output
     int 21h              ; dos call
     mov ax,4c00h
     int 21h              ; terminate
     nop
     nop
     nop
END START
END
