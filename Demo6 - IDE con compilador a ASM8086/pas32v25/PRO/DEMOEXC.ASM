; Pass32 Example file how to build an exception handler ....

.UCU
.DATA

; Exception Output
  DiverrorStr  db 'Division by Zero Error!',0
  CallerEip    db '-Caller EIP',0
  CallerCS     db '-Caller CS',0
  ErrorCode    db '-Error Code',0
  ErrorEIP     db '-Error EIP',0
  ErrorCS      db '-Error CS',0
  ErrorEFLAGS  db '-Error EFLAGS',0
  ErrorESP     db '-Error ESP',0
  ErrorSS      db '-Error SS',0

.CODE

START:
   mov bx,0
   mov ax,0203h
   mov cx,cs
   mov edx,offset DivError
   int 31h                      ; init Exception Handler
   mov bx,0
   div bx                       ; Force Exception
   mov ah,4ch
   int 21h

; Exception Handler for division by Zero

PROC DIVERROR
   mov ax,cs:[0]
   mov ds,ax
   mov edi,offset DiverrorStr
   xor cx,cx
   call print                    ; print exception message
   pop eax                       ; caller EIP
   call hexout
   mov edi,offset CallerEIP      ; ret address of the caller DPMI host
   mov cx,9
   call print
   pop eax                       ; caller CS
   call hexout                   ; ret selector of the caller DPMI host
   mov edi,offset CallerCS
   mov cx,9
   call print
   pop eax                       ; Error Code
   call hexout
   mov edi,offset ErrorCode
   mov cx,9
   call print
   pop eax                       ; EIP at which the error occured
   call hexout
   mov edi,offset ErrorEIP
   mov cx,9
   call print
   pop eax                       ; CS at which the error occured
   call hexout
   mov edi,offset ErrorCS
   mov cx,9
   call print
   pop eax                       ; EFlags at the moment of the exception
   call hexout
   mov edi,offset ErrorEflags
   mov cx,9
   call print
   pop eax           ; ESP       ; Stackpointer at the moment of the exception
   call hexout
   mov edi,offset ErrorESP
   mov cx,9
   call print
   pop eax           ; SS        ; Stack selector at the moment of the exception
   call hexout
   mov edi,offset ErrorSS
   mov cx,9
   call print

   mov ah,4ch                    ; terminate to real mode ...
   int 21h
ENDP DIVERROR


Print           PROC  ;  EDI: OFFSET Message
                      ;  CX : Column
                      ; prints in last line of screen and scrolls
       mov si,24*160
       shl cx,1
       add si,cx
       mov bh,14
printloop:
       mov bl,[edi]
       cmp bl,0
       je printende
       mov es:[si],bx
       add si,2
       inc edi
       jne printloop
printende:
       inc edi
       mov ax,0601h
       mov bh,14
       mov cx,0
       mov dx,1850h
       int 10h        ; scroll screen
       mov ah,2
       xor bx,bx
       mov dx,1800h
       int 10h
       ret
ENDP print

HEXOUT PROC          ;  HEXVALUE in EAX to STDO
      mov ebx,eax
      mov cl,28
@HEXOUTloop:
      mov eax,ebx
      shr eax,cl
      and ax,0fh
      add ax,48
      cmp ax,58
      jb SHORT @HEXOUTNext
      add ax,7
@HEXOUTNext:
      mov dl,al
      mov ah,2
      int 21h
      sub cl,4
      cmp cl,0
      jne @HEXOUTLoop
      mov eax,ebx
      and ax,0fh
      add ax,48
      cmp ax,58
      jb SHORT @HEXOUTNext2
      add ax,7
@HEXOUTNext2:
      mov dl,al
      mov ah,2
      int 21h
      ret
ENDP HEXOUT

END START
END