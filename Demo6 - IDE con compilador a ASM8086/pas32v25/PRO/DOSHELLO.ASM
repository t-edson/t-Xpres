; No Model - as this file is able to run in both modes ...
.UCU
.NM

.DATA
     mesg      db         'Hello, World - with extended DOS!',13,10,'$'
.CODE
START:
     call INITDOSX        ; initialize extended DOS functions...
     mov edx,OFFSET mesg  ; offset to text string
     mov ah,9h            ; extended dos function string to standard output
     int 21h              ; dos call
     mov ax,4c00h
     int 21h              ; terminate

.INCLUDE DOSX.INC         ; include extended DOS library ...

END START
END


; assemble with PASS32 DOSHELLO -t to create a DOS .COM executable !
; assemble with PASS32 DOSHELLO  to create a Pro32 executable !