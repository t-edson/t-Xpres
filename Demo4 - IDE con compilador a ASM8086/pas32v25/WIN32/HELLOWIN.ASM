.COMMENT
 ****************************************************************************
 HELLOWIN.ASM   Example Win32 file
                (c) 1998,1999 by Dieter Pawelczak - Pass32 Version 3.0alpha
 ============================================================================
 ****************************************************************************


.MODEL WIN32 ; - all you have to do ...

.MEM 32      ; - just like in old times
.STACK 32



    .data
        szDisplayName db 'Hello World Demo',0
        szMessage     db 'Hello, World with Pass32',0
        hwin dd 0
    .code

; WIN 32 Prototypes :

proc MessageBox
.win32 'USER32.MessageBoxA'
endp MessageBox


Start:
        mov ecx,0
loop:
        push ecx
        push ecx                      ; Style
        push OFFSET szDisplayName   ; Message-Window-Title
        push OFFSET szMessage       ; Message
        push 0                      ; hwin
        call MessageBox             ; call via Prototype
        pop ecx
        cmp eax,4                   ; = REPEAT
        je short L1
        inc ecx
        L1:
        cmp ecx,5
        jb loop
        push 0
        .invoke .win32 'KERNEL32.ExitProcess'; call direct

end start


end


