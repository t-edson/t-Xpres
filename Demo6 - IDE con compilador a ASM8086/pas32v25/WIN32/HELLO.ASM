.COMMENT
 ****************************************************************************
 HELLO.ASM   Example Win32 file using CONSOLE.INC
                (c) 1998,1999 by Dieter Pawelczak - Pass32 Version 3.0alpha
 ============================================================================
 ****************************************************************************


.MODEL WIN32 CONSOLE; - all you have to do ...

.MEM 32      ; - just like in old times
.STACK 32

.INCLUDE CONSOLE.INC
.INCLUDE WINSYS.INC


START:
  color(15,0);
  WriteLN('Hello World with Pass32 and a console application!')
  color(7,0);
  WriteLn('');
  Write('Number of Parameters:');
  mov eax,255
  call getparamstr
  and eax,0ffh
  push eax
  writeln(EAX)
  write('ParamStr(0): ')
  mov al,0
  call getparamstr
  call WriteConsole
  writeln('');
  pop eax
_L1:
  cmp eax,0
  je _L2
  push eax
  push eax
  push eax
  write('ParamStr(');
  pop eax
  write(EAX)
  write('): ');
  pop eax
  call getparamstr
  call WriteConsole
  writeln('');
  pop eax
  dec eax
  jmp _L1
_L2:
  writeln('')
  write('Press ENTER to continue');
  readln
  .invoke .win32 'Kernel32.ExitProcess'
END START