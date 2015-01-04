.COMMENT
 ****************************************************************************
 MACRO.ASM      Example file for to demonstrate MACROS
                (c) 1996,1998 by Dieter Pawelczak - Pass32 Version 2.0
 ============================================================================
 ****************************************************************************
.MODEL FLAT
.UCU
.NM


.DATA
.ALIGN 4

.COMMENT  *** Definitions of the Macros ***

.MACRO WRITE(stringoffs)
       mov edi,stringoffs
.LOCAL @start:
       mov dl,[edi]
       cmp dl,0
       je short .LOCAL @end
       mov ah,2
       int 21h                  ; display string char by char
       inc edi
       jmp .LOCAL  @start
.LOCAL @end:
ENDMACRO

.MACRO WRITELN(stringoffs)
  WRITE(stringoffs)
  mov dl,10
  mov ah,2
  int 21h
  mov dl,13                     ; do Carriage Return
  mov ah,2
  int 21h
ENDMACRO

.MACRO Create_Message(name,string)
      .DATA
       name db string,0         ; create string message
       .CODE
ENDMACRO

.MACRO Exit(ExitCode)
       mov ah,4ch
       mov al,ExitCode
       int 21h
ENDMACRO

.MACRO PrintError(Message)
       Create_Message(.LOCAL ERROR,'ERROR: ')
       Write(offset .LOCAL ERROR)
       Create_Message(.LOCAL MESG,Message)
       Writeln(offset .LOCAL MESG)
       Exit(3)
ENDMACRO

.MACRO Print(Message)
       Create_Message(.LOCAL MESG,Message)
       Writeln(offset .LOCAL MESG)
ENDMACRO

.COMMENT =============================== MAIN ===============================

.CODE

START:
      Print('********** HELLO! *************')
      print('This is a simple macro example!')
      print('*******************************')
      PrintError('EOF Demo reached.')

END START
END

