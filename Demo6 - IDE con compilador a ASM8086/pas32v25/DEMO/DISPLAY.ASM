.COMMENT
 ****************************************************************************
 DISPLAY.ASM    Example file for module SYSTEM.INC, IO.INC
                (c) 1996,1998 by Dieter Pawelczak - Pass32 Version 2.0
 ============================================================================
 ****************************************************************************
.MODEL FLAT
.UCU
.NM


.MODEL FLAT
.MEM 2048  ; 2 MByte Memory to load large ascii files!
.NM        ; No further messages
.DATA
     mesg       db     'Syntax: DISPLAY Filename.Ext',0
     mesg1      db     'Error: File not found!',0
     ErrorColor .EQU  14
.CODE
MAIN:
     mov al,1
     call getparamstr                   ; check the first parameter
     jc NoParameters                    ; no parameter found
     mov esi,LASTDATA                   ; First Available Free Heap
     call Loadfile                      ; load ParamStr(1)
     jc FileNotFound                    ; file not found
     mov ecx,eax                        ; Number of Bytes
     mov edi,LastData
DisplayLoop:
     mov dl,[edi]
     mov ah,2
     int 21h                            ; dos display function
     inc edi
     loopd DisplayLoop                  ; note, we must use ECX to display
                                        ; files longer than 64K!!!
MainEnd:
     mov ah,4ch
     int 21h
NoParameters:
     mov edi,offset mesg
     mov bh,ErrorColor
     call systemwriteLn
     jmp MainEnd
FileNotFound:
     mov edi,offset mesg1
     mov bh,ErrorColor
     call systemwriteLn
     jmp MainEnd

.INCLUDE SYSTEM.INC
.INCLUDE IO.INC
END MAIN
END