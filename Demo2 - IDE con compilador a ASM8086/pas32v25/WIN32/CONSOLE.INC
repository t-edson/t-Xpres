.COMMENT
 ****************************************************************************
 CONSOLE.INC    Useful Win32 functions/macros for console in-/output
                (c) 1999 by Dieter Pawelczak - Pass32 Version 3.0
 ============================================================================
 INTERFACE
 ****************************************************************************

.public .data

 STDOUT      DD         0
 STDIN       DD         0
 STDERR      DD         0
 OutPutLen   DD         0
 InPutLen    DD         0

.data
 .public __buffer     db 128 dup(0)
 .public __LF db 13,10,0
 .public __color dw 7
 ConsoleBufferSIze DD 80*25

.include string.inc


.COMMENT *** Write
             OutPuts a String or Register Value:
             Write('Hello') --> HELLO
             Write(eax)     --> 12345 ***

.macro Write(Msg)
  mov outputlen,0
  push 0
  push offset OutPutLen
.ifs MSG
  mov eax,COFFS+8
  jmp short .LOCAL next
.const
  db MSG
.code
.local next:
  mov ebx,COFFS
  sub ebx,eax
  push ebx
  push eax
.ELSE
  mov eax, msg
  mov edi,offset __buffer
  call mathtostring
  mov edi,offset __buffer
  call strlength
  push eax
  push edi
.ENDIF
  push stdout
  .invoke .win32 'Kernel32.WriteFile'
endm

.COMMENT *** WriteLn
             OutPuts a String or Register Value with new line:
             WriteLN('Hello') --> HELLO
             WriteLN(eax)     --> 12345 ***

.macro WriteLn(Msg)
  mov outputlen,0
  push 0
  push offset OutPutLen
.ifs MSG
  mov eax,COFFS+8
  jmp short .LOCAL next
.const
  db MSG,13,10
.code
.local next:
  mov ebx,COFFS
  sub ebx,eax
  push ebx
  push eax
.ELSE
  mov eax, msg
  mov edi,offset __buffer
  call mathtostring
  mov edi,offset __buffer
  mov esi,offset __LF
  call strcat
  push eax
  push edi
.ENDIF
  push stdout
  .invoke .win32 'Kernel32.WriteFile'
endm


.COMMENT *** ReadLn
             Reads a String  ***

.macro Readln
  mov inputlen,0
  push 0
  push offset InPutLen
  push 256
  push offset __buffer
  push STDIN
  .invoke .win32 'Kernel32.ReadFile'
  ret
endm

.COMMENT *** Color
             defines fore- and background color for console output ***

.macro Color(Fore,Back)
  mov __color, fore+back*16
  push fore+back*16
  push stdout
  .invoke .win32 'Kernel32.SetConsoleTextAttribute'
endm



proc writeConsole   ; EDI = OFFSET TO Zero Terminated String
  mov outputlen,0
  push 0
  push offset OutPutLen
  call strlength
  push eax
  push edi
  push STDOUT
  .invoke .win32 'Kernel32.WriteFile'
  ret
endp writeConsole

proc readConsole   ; EDI = OFFSET TO Input Buffer
  mov inputlen,0
  push 0
  push offset InPutLen
  push 256
  push edi
  push STDIN
  .invoke .win32 'Kernel32.ReadFile'
  ret
endp readConsole

.COMMENT **  Function ConsoleGotoXY:     set cursor at CX, DX **

proc ConsoleGotoXY
  mov word ptr __BUFFER,cx
  mov word ptr __BUFFER+2,dx
  push DWORD PTR __BUFFER
  push STDOUT
  .invoke .win32 'Kernel32.SetConsoleCursorPosition'
  ret
endp ConsoleGotoXY


proc ConsoleKeypressed
  push offset __buffer
  push stdin
    .invoke .win32 'Kernel32.GetNumberOfConsoleInputEvents'
  cmp __Buffer,0
  ret
endp ConsoleKeypressed


proc ConsoleClrScr
  push offset outputlen
  push 0 ; x,y = 0
  push ConsoleBufferSize
  push 32
  push stdout
    .invoke .win32 'Kernel32.FillConsoleOutputCharacterA'
  push offset outputlen
  push 0 ; x,y = 0
  push ConsoleBufferSize
  push __color
  push stdout
    .invoke .win32 'Kernel32.FillConsoleOutputAttribute'
  ret

endp ConsoleClrScr





proc init_console START
  pushad
  .invoke .win32 'KERNEL32.AllocConsole'
  push -10
  .invoke .win32 'KERNEL32.GetStdHandle'
  mov stdin,eax
  push -11
  .invoke .win32 'KERNEL32.GetStdHandle'
  mov stdout,eax
  push -12
  .invoke .win32 'KERNEL32.GetStdHandle'
  mov stderr,eax
  popad
  ret
endp init_console

MathToString PROC       ;
.DATA
      @mathoutbcd     DT       0
      @mathoutint     DD       0
      @mathoutnull    DB       0
.CODE
    cmp eax,0
    jne short L1
    mov dx,48                       ; Number zero, create '0'+\0
    mov [edi],dx
    ret
L1:                                 ; Number not zero
    mov @mathoutint,eax
    fild @mathoutint
    fbstp @mathoutbcd
    mov ecx,9
    mov @mathoutnull,0
    mov al,byte ptr @mathoutbcd+9
    and al,128
    cmp al,0
    je short L2
    mov dx,'-'                      ; Number is negative
    mov [edi],dx
    inc edi
L2:                                 ; Number has no sign
    xor dx,dx
    mov esi,offset @mathoutbcd+8
L3:
    mov al,[esi]
    shr ax,4
    and al,0fh
    cmp al,0
    je short L4
    mov @mathoutnull,1
L4:
    cmp @mathoutnull,0
    je short L5
    mov dl,al
    add dl,48
    mov [edi],dx
    inc edi
L5:
    mov al,[esi]
    and al,0fh
    cmp al,0
    je short L6
    mov @mathoutnull,1
L6:
    cmp @mathoutnull,0
    je short L7
    mov dl,al
    add dl,48
    mov [edi],dx
    inc edi
L7:
    dec esi
    dec cx
    cmp cx,0
    jne L3
    ret
ENDP MathToString

