.COMMENT
 ****************************************************************************
 TYPES.ASM      Example for the .TYPE directive
                (c) 1996,1997 by Dieter Pawelczak - Pass32 Version 2.1
 ============================================================================
 ****************************************************************************
.MODEL FLAT
.NM
.UCU

; Definition of the VGA Colors

.TYPE (Black, Blue , Green, Cyan, Red, Magenta, Brown, Gray=8, LightBlue
       LightGreen, LightCyan, LightMagenta=13, Yellow, White=0fh*5/5)

; Definition of Exit Code

.TYPE FineExit=0, ErrorExit = 3

; Definition of the DOS functions...

.TYPE DosCall=21h, DosExit=4ch

.Data
    Mesg DB  'Example for the .TYPE directive',0

.INCLUDE System.inc

.CODE
.PUBLIC START:
   mov edi,offset Mesg
   mov TextColor, Gray
   call systemwriteln
   mov edi,offset Mesg
   mov TextColor, Green
   call systemwriteln
   mov edi,offset Mesg
   mov TextColor, LightGreen
   call systemwriteln
   mov ah, DosExit
   mov al, FineExit
   int DosCall
END START
END