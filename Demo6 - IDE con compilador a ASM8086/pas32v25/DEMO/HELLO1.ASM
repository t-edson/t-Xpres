.COMMENT
 ****************************************************************************
 HELLO1.ASM     Example file for TINY Model - Assemble with /t
                (c) 1996,1998 by Dieter Pawelczak - Pass32 Version 2.0
 ============================================================================
 ****************************************************************************
.MODEL TINY
.DATA
       HelloMesg        db      'Hello,World',10,13,'$'
.CODE
START:
       mov dx,OFFSET HelloMesg  ; offset of the text string
       mov ah,9                 ; print string function number
       int 21h                  ; dos call
       mov ah,4ch               ; terminate function number
       int 21h                  ; dos call
END START                     ; marks the entry procedure of the program
END
