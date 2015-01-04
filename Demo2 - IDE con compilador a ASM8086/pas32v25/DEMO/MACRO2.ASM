.COMMENT
 ****************************************************************************
 MACRO2.ASM     Example file to demonstrate the macro lib SYSTEM.MAC
                (c) 1996,1998 by Dieter Pawelczak - Pass32 Version 2.0
 ============================================================================
 ****************************************************************************
.MODEL FLAT
.UCU
.NM

.include system.mac

.CODE
.PUBLIC start:
  call systemclrscr
  print(15,6,'******************** MACRO 2 ***********************')
  print(15,7,'*                                                  *')
  print(15,8,'*                                                  *')
  print(15,9,'*                                                  *')
  print(15,10,'******************** MACRO 2 ***********************')
  color(14,0)
  print(31,8,'Simple Macro Demo')
  color(14,7)
  print(0,0,' Macro2.ASM                                                                    ');
  print(0,24,' Press any key to continue                                                     ');
  call systemgetkey
  color(7,0)
  print(0,24,'                                                                               ');
  gotoxy(1,12);
  exit(0)
END start
END
