.COMMENT
 ****************************************************************************
 EXEC.ASM       Example file for DOSX.INC
                (c) 1996,1998 by Dieter Pawelczak - Pass32 Version 2.0
 ============================================================================

 This file demonstrates the new DOSX.INC function 4BH = execute file
 The calling parameters are (note, this is not dos conform)

 Filename and Path (zero terminated ascci string) in DS : EDX
 Parameter Line (zero terminated ascci string) in ES : EBX

 ****************************************************************************

.include dosx.inc

.ucu
.nm
.mem 64
.data
    programname db  'Pass32',0
    parameter   db  'EXEC.ASM',0
    mesg        db  10,13,' ** Exit demo ** ',10,13,'$'

.CODE
START:
   mov edx,offset programname
   mov ebx,offset parameter
   push ds
   pop es
   mov ax,4b00h                  ; Execute Program with Parameter...
   int 21h
   mov edx,offset mesg
   mov ah,9h
   int 21h
   mov ax,4c00h
   int 21h
END START
END
