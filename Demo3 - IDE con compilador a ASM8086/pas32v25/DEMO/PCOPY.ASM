.COMMENT
 ****************************************************************************
 PCOPY.ASM      Example file for IO.INC
                (c) 1996,1998 by Dieter Pawelczak - Pass32 Version 2.0
 ============================================================================

 A typical false friend:

 This copy program first loads the source file in to the heap memory,
 and then saves the destination file from heap memory...
 But, as DOS is 16 bit and not 32 bit, every read or write function
 uses the 32KByte real mode buffer...

 ****************************************************************************

.include system.inc
.include system.mac
.include io.inc

.ucu
.nm
.mem 8192

.CODE
START:
  mov al,2
  call getparamstr
  jc Error
  mov al,1
  call getparamstr
  mov esi,LASTDATA      ; load paramstr(1) into heap
  call LoadFile
  jc Error2             ; file not found
  push eax              ; save file length
  Write('Copy File:')   ; display copy message
  mov al,1
  call getparamstr
  call systemwrite
  Write(' To: ')
  mov al,2
  call getparamstr
  call systemwriteLn
  mov al,2
  call getparamstr
  pop ecx               ; restore file length
  mov esi,LASTDATA      ; save heap contents as paramstr(2)
  call SaveFile

  Exit(0)
Error:
  Writeln('Syntax: PCOPY Filename1.ext Filename2.ext');
  exit(3);
Error2:
  Writeln('Error: Source File not found');
  exit(3);
END START
END
