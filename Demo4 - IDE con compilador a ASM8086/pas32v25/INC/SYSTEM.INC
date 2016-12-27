.COMMENT
 ****************************************************************************
 SYSTEM.INC     Some useful PM functions
                (c) 1996-1998 by Dieter Pawelczak - Pass32 Version 2.2
 ============================================================================
 INTERFACE

 Function SystemKeyPressed: returns Zero Flag, if no key pressed
 Function SystemGetKey:     waits for key stroke, returns key in al
 Function SystemGetMem:     allocates eax bytes of memory on the heap
 Function SystemFreeMem:    frees eax bytes of memory on the heap
 Function SystemMaxAvail:   returns largest memory block available on the heap
 Function SystemMemAvail:   returns amount of memory available on the heap
 Function MathToString:     32 bit (EAX) number to string at Offset EDI
 Function DoubleToStr;      converts double in ST(0) to string at Offset EDI
 Function SystemPrint:      prints at screen CX=X, DX=Y, EDI=string
 Function SystemWrite:      prints string EDI at cursor
 Function SystemWriteLn:    prints string EDI at cursor with LineFeed
 Function SystemNewLine:    creates line feed and scrolling at cursor
 Function SystemClrSrc:     clears the text screen
 Function SystemReadLn:     reads line from keyboard to string in EDI
 Function SystemGotoXY:     set cursor at CX, DX
 Function SystemSound:      turn speaker on with frequency in AX
 Function SystemNoSound:    turn speaker of
 Function SystemDelay:      delays executions for ms in AX
 Function SystemExec:       executes a program prgname EDI, parameter line ESI
 Function GetParamStr:      gets paramstr in EDI for parameter(AL)
 Function GetEnvStr:        returns Environment String matching string in EDI
 Function GetPrgDir:        Returns Directory of the program path in EAX
 Function SystemFindFile:   Searches a filename in EDI, in the directories given in ESI

 Variable TextColor:  DB    foreground color
 Variable Background: DB    background color
 Macro    CheckCPU(CPUType) macro to check the CPU Type

 ****************************************************************************

.include dpmi.inc

.DATA
  .PUBLIC   HeapOrg    DD     0
  .PUBLIC   HeapPtr    DD     0
  .PUBLIC   HeapEnd    DD     0
  .PUBLIC   TextColor  DB     7
  .PUBLIC   BackGround DB     0
  rand1 dd 1987557217
  rand2 dd 185355771
  rand3 dd 1378597112
.CODE
.NODEBUG

; KEYBOARD ******************************************************************

.COMMENT *** Function SystemKeyPressed: returns Zero Flag, if no key pressed ***

SystemKeyPressed  PROC
        mov ax,gs:[41Ah]        ; assume GS: ZERO Selector
        cmp ax,gs:[41Ch]
        ret
ENDP SystemKeyPressed

.COMMENT *** Function SystemGetKey: waits for key stroke, key in al ***

SystemGetKey      PROC NEAR              ; Key in AL
.DATA
     @oldkey      db                 0
.CODE
.IFPM
     mov ax,901h
     int 31h                         ; enable interrupts
.ELSE
     sti
.ENDIF
     cmp byte ptr @oldkey,0
     je short L1
     mov al, @oldkey
     mov byte ptr @oldkey,0
     ret
L1:
     mov di,gs:[41ah]
     cmp gs:[41ch],di
     je L1
     mov di,gs:[41Ah]
     mov gs:[41Ch],di
     add di,0400h
     mov ax,gs:[di]
     cmp al,0
     je short L2
     cmp al,0e0h
     je short L2
     ret
L2:
     mov @oldkey,ah
     xor eax,eax
     ret
ENDP SystemGetkey

; HEAP MANAGEMENT ***********************************************************

.COMMENT *** Function SystemGetMem: allocates eax bytes of memory on the heap ***

SystemGetMem      PROC  NEAR         ; returns pointer in EAX / EDX
     add eax,3
     and eax,0fffffffch              ; dword alignment
     mov ebx,lastdata                ; get first free pointer
L2:                                  ; check next ptr
     mov ecx,[ebx]                   ; get size of pointer
     cmp eax,ecx
     ja short L3  ; block too small?
     bt ecx,0
     jc short L3  ; block not free?
                                     ; OK, allocate block
     bts eax,0                       ; reserve block
     mov [ebx],eax                   ; store size
     btr eax,0
     sub ecx,eax
     cmp ecx,0
     je short L5                     ; allocate exact same size again ..
     sub ecx,4
     mov [ebx+eax+4],ecx             ; store next heap ptr
L5:
     mov eax,ebx
     add eax,4
     mov edx,eax
     clc
     ret
L3:
     btr ecx,0
     add ebx,ecx
     add ebx,4
     cmp HeapEnd,ebx
     jb short L4
     jmp L2
L4:
     mov eax,memsize
     mov edx,eax
     stc
     ret
ENDP SystemGetMem

.COMMENT *** Function SystemFreeMem: frees eax bytes of memory pointer edx ***

SystemFreeMem     PROC  NEAR         ; Pointer in EDX
     mov ebx,edx
     sub ebx,4                       ; get size of pointer
     mov ecx,[ebx]
     btr ecx,0                       ; block had been reserved?
     jnc short L2
     cmp eax,ecx                     ; compare size with actual size
     jne short L2
     mov [ebx],ecx                   ; mark pointer as free
     mov eax,[ebx+ecx+4]             ; get next pointer
     btr eax,0                       ; check if free?
     jc short L1
     add ecx,eax                     ; add the next block to the
     add ecx,4                       ; current block
     mov [ebx],ecx                   ; mark them as free ...
L1:
     clc
     xor eax,eax
     ret
L2:
     xor edx,edx
     stc
     ret
ENDP SystemFreeMem

.COMMENT *** Function SystemMaxAvail: returns largest memory block available on the heap ***

SystemMaxAvail      PROC  NEAR       ; returns value  EAX
     mov ebx,lastdata                ; get first free pointer
     xor eax,eax
L2:                                  ; check next ptr
     mov ecx,[ebx]                   ; get size of pointer
     bt ecx,0
     jc short L3                     ; block not free?
     cmp eax,ecx
     ja short L3                     ; block small than other?
     mov eax,ecx                     ; No? remember this size
L3:
     btr ecx,0
     add ebx,ecx
     add ebx,4
     cmp HeapEnd,ebx
     jb short L4
     jmp L2
L4:
     ret
ENDP SystemMaxAvail

.COMMENT *** Function SystemMemAvail: returns amount of memory available on the heap ***

SystemMemAvail      PROC  NEAR       ; returns value  EAX
     xor edx,edx
     mov ebx,lastdata                ; get first free pointer
L2:                                  ; check next ptr
     mov ecx,[ebx]                   ; get size of pointer
     bt ecx,0
     jc short L3                     ; block not free?
     add edx,ecx                     ; add size to edx
L3:
     btr ecx,0
     add ebx,ecx
     add ebx,4
     cmp HeapEnd,ebx
     jb short L4
     jmp L2
L4:
     mov eax,edx
     ret
ENDP SystemMemAvail

.COMMENT ***
Function which is called by the Pro32 debugger for heap memory controll
Pro32SystemMaxAvail: returns largest memory block available on the heap
***

Pro32SystemMaxAvail      PROC  FAR   ; returns value  EAX
     cmp HeapOrg,0
     je X
     cmp HeapEnd,0
     je X
     mov ebx,lastdata                ; get first free pointer
     xor eax,eax
L2:                                  ; check next ptr
     mov ecx,[ebx]                   ; get size of pointer
     bt ecx,0
     jc short L3                     ; block not free?
     cmp eax,ecx
     ja short L3                     ; block small than other?
     mov eax,ecx                     ; No? remember this size
L3:
     btr ecx,0
     add ebx,ecx
     add ebx,4
     cmp HeapEnd,ebx
     jb short L4
     jmp L2
L4:
     ret
X:
     mov eax,-1
     ret
ENDP Pro32SystemMaxAvail

.COMMENT ***
Function which is called by the Pro32 debugger for heap memory controll
Pro32SystemMemAvail: returns amount of free memory
***


Pro32SystemMemAvail      PROC  FAR       ; returns value  EAX
     cmp HeapOrg,0
     je X
     cmp HeapEnd,0
     je X
     xor edx,edx
     mov ebx,lastdata                ; get first free pointer
L2:                                  ; check next ptr
     mov ecx,[ebx]                   ; get size of pointer
     bt ecx,0
     jc short L3                     ; block not free?
     add edx,ecx                     ; add size to edx
L3:
     btr ecx,0
     add ebx,ecx
     add ebx,4
     cmp HeapEnd,ebx
     jb short L4
     jmp L2
L4:
     mov eax,edx
     ret
x:
     mov eax,-1
     ret
ENDP Pro32SystemMemAvail

; NUMBER CONVERSION *********************************************************

.COMMENT *** Function MathToString: 32 bit (EAX) number to string at Offset EDI ***

MathToString PROC  NEAR     ;
.DATA
      @mathoutbcd     DT       0
      @mathoutint     DD       0
      @mathoutnull    DB       0
.CODE
    cmp eax,0
    jne short L1
    mov dx,48                       ; Number zero, create '0'+\0
    mov [edi],dx
    inc edi
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

.COMMENT *** Function GetParam0Str Returns parameter (0) in EDI ***

PROC getparam0str NEAR
.DATA?
  getparam0 DB 64 DUP(?)
.CODE
  push fs
  mov ax,cs:[2ch]
  mov fs,ax
  mov si,0
L0:
  mov ax,fs:[si]
  cmp ax,0
  je short L1
  inc si
  jmp L0
L1:
  mov edi,offset getparam0
  add si,3
L2:
  inc si
  mov al,fs:[si]
  cmp al,0
  je short L3
  mov ah,0
  mov [edi],ax
  inc edi
  jmp L2
L3:
  pop fs
  ret
ENDP getparam0str



.COMMENT *** Function GetParamStr Returns offset to parameter AL in EDI ***

GetParamStr PROC NEAR ;  RETURNS  EDI: String Offset to paramstr(AL)
                      ;                terminated with zero character
                      ;                CFLAG if No Paramstr (AL)
                      ;                AL = Max Number
    cmp al,0
    jne short L0
    call getparam0str
    ret
L0:
    mov bl,0
    mov esi,81h
    mov edi,esi
L1:
    mov cl,[si]
    cmp cl,32 ; Ok First <Space>
    jne short L2
    inc esi
    inc edi
    jmp L1
L2:
    cmp cl,9 ; <TAB>
    jne short L3
    inc esi
    inc edi
    jmp L1
L3:
    mov cl,[si]
    cmp cl,0
    je L4
    cmp cl,32
    je L4
    cmp cl,9
    jne L8
L4:
    xor cl,cl
    mov [si],cl
    inc bl
    cmp al,bl
    je L9
    mov edi,esi
    inc edi               ; EDI Points to next character
L5:
    mov cl,[edi]
    cmp cl,32             ; next character Space as well ?
    jne L6
    inc si
    inc edi
    jmp L5
L6:
    cmp cl,0
    jne L7
    inc si
    inc edi
    jmp L5
L7:
    cmp cl,9
    jne short  L8
    inc si
    inc edi
    jmp L5
L8:
    cmp cl,13
    je @GetParamstrerror
    inc si
    jmp L3
L9:          ; Yes ParamStr detected and END marked with zero
    clc
    ret
@GetParamstrerror:          ; Carriage Return Detected!
    cmp esi,edi
    je short L10
    mov cl,[edi]
    cmp cl,13
    je short L10
    mov cl,0
    mov [si],cl
    mov cl,13
    mov [si+1],cl
    inc bl
    cmp al,bl
    je L9
L10:
    mov al,bl
    stc
    ret
ENDP GetParamstr

.COMMENT *** Function GetEnv Returns Environment String matching string in EDI ***

PROC GetEnvStr NEAR  ; returns string in eax
.DATA?
  GetEnv DB 64 DUP(?)
.CODE
  push es
  mov esi,offset getenv
  mov dword ptr [esi],0
  mov ax,cs:[2ch]
  mov es,ax
  xor eax,eax
L0:
  xor edx,edx
Lp:
  mov bl,es:[eax]
  mov cl,[edi+edx]
  cmp cl,97
  jb L1
  and cl,223
L1:
  cmp cl,0
  je short L2
  cmp bl,61
  je short L3
  cmp bl,cl
  jne short L3
  inc eax
  inc edx
  jmp Lp
L2:
  inc eax
  mov edi,offset getenv
L6:
  mov bl,es:[eax]
  mov [edi],bl
  inc edi
  inc eax
  cmp bl,0
  jne short L6
  pop es
  mov eax,offset getenv
  ret
L3:
  mov bl,es:[eax]
  inc eax
  cmp bl,0
  jne L3
  mov bl,es:[eax]
  cmp bl,0
  je short L5
  jmp L0
L5:
  pop es
  mov eax,offset getenv
  ret
ENDP GetEnvStr

.COMMENT *** Function GetPrgDir Returns Directory of the program path in EAX ***

PROC GetPrgDir NEAR
  call getparam0str
  mov edi,eax
  xor eax,eax
L0:
  mov bl,[edi+eax]
  cmp bl,0
  je short L1
  inc eax
  jmp L0
L1:
  dec eax
  cmp eax,0
  je short L2
  mov bl,[edi+eax]
  cmp bl,92
  jne L1
  mov byte ptr [edi+eax],0
L2:
  mov eax,edi
  ret
ENDP GetPrgDir


.COMMENT *** Function SystemPrint: prints at screen CX=X, DX=Y, EDI=string ***

SystemPrint           PROC  NEAR;  cx : X, dx : Y, EDI: OFFSET Message
       push es
       mov ax,cs:[2]
       mov es,ax
       mov ax,160
       mul dx
       shl cx,1
       add ax,cx
       mov si,ax        ; Screen Offset
       mov bh,Background
       shl bh,4
       or bh,TextColor
L1:
       mov bl,[edi]
       cmp bl,0
       je short L2
       mov es:[si],bx
       add si,2
       inc edi
       jne L1
L2:
       inc edi
       pop es
       ret
ENDP Systemprint

.COMMENT *** Function SystemWrite: prints string EDI at cursor ***

SystemWrite PROC
   push es
   push ebp
   mov ax,[2]
   mov es,ax
   push esi
   push eax
   push ebx
   push ecx
   push edx
   push edi
   push ebx
   mov ax,300h                    ; adjust cursor for output
   xor bx,bx
   int 10h                        ; get cursor
   xor cx,cx
   mov cl,dl
   mov dl,dh
   xor dh,dh
   pop ebx
   pop edi
   push edx                        ; save line
   push ecx                        ; save col
   mov ax,160
   mul dx
   shl cx,1
   mov si,cx
   add si,ax

   mov al,32
   mov bx,ax
   shl eax,16
   mov ax,bx
   xor ebx,ebx
   pop ecx
   mov ah,Background
   shl ah,4
   or ah,TextColor
L1:
   mov al,[edi]
   cmp al,0
   je short L5
   mov es:[si],ax
   inc si
   inc si
   inc edi
   inc cl
   cmp cl,80
   jae short L2
   jmp  L1
L2:
   pop edx
   inc edx
   push edx
   xor cx,cx
   cmp si,4000
   jb L1
   xor esi,esi
   xor ecx,ecx
   push eax
L3:
   mov eax,es:[esi+160+ecx*4]
   mov es:[esi+ecx*4],eax
   inc ecx
   cmp cx,960
   jb L3
   pop eax
   mov al,32
   mov ecx,40
   mov esi,3840
L4:
   mov es:[esi+4*ecx],eax
   loop L4
   xor ecx,ecx
   mov si,3840
   pop edx
   dec edx
   push edx
   jmp L1
L5:
   pop edx
   xor bh,bh
   shl dx,8
   mov dl,cl
   mov ah,2
   int 10h
   pop edx
   pop ecx
   pop ebx
   pop eax
   pop esi
   pop ebp
   pop es
   ret
ENDP SystemWrite

.COMMENT *** Function SystemNewLine: creates line feed and scrolling at cursor ***

SystemNewLine PROC
    push es
    push ebp
    mov ax,cs:[2]
    mov es,ax
    mov ah,3
    xor bx,bx
    int 10h
    xor ch,ch
    mov cl,dl
    mov dl,dh
    xor dh,dh
    inc dl
    cmp dl,25
    jb short L3
    dec dl
    xor esi,esi
    mov cx,1000-40
L1:
    mov eax,es:[si+160]
    mov es:[si],eax
    add si,4
    loop L1
    mov cx,80
    mov si,3840
    mov ah,Background
    shl ah,4
    or ah,TextColor
    mov al,32
L2:
    mov es:[si],ax
    add si,2
    loop L2
L3:
     shl dx,8
     xor bx,bx
     mov ah,2
     int 10h
     pop ebp
     pop es
     ret
ENDP SystemNewLine

.COMMENT *** Function SystemWriteLn:prints string EDI at cursor with LineFeed ***

PROC SystemWriteln NEAR
    call systemWrite
    call SystemNewLine
    ret
ENDP SystemWriteln

.COMMENT *** Function SystemClrSrc:     clears the text screen ***

PROC SystemClrScr NEAR
    mov ax,cs:[2]
    mov es,ax
    mov ah,Background
    shl ah,4
    or ah,TextColor
    mov al,32
    mov ebx,eax
    shl eax,16
    mov ax,bx
    mov ecx,999
L1:
    mov es:[ecx*4],eax
    loop L1
    mov es:[0],eax
    xor cx,cx
    xor dx,dx
    call systemGotoXY
    ret
ENDP SystemClrScr

.COMMENT *** Function SystemReadLn: reads line from keyboard to string in EDI ***

PROC SystemReadln NEAR
    mov esi,edi
L1:
    push edi
    call systemGetKey
    pop edi
    cmp al,0
    je L4
    cmp al,8
    je short L3
    cmp al,13
    je short L2
    cmp al,32
    jb L1
L2:
    mov [edi],al
    mov dl,al
    mov ah,2
    int 21h
    cmp dl,13
    je L5
    inc edi
    jmp L1
L3:
    cmp edi,esi
    je L1
    dec edi
    mov dl,8
    mov ah,2
    int 21h
    mov dl,32
    mov ah,2
    int 21h
    mov dl,8
    mov ah,2
    int 21h
    jmp L1
L4:
    push edi
    call systemGetKey
    pop edi
    jmp L1
L5:
    mov byte ptr [edi],0
    mov dl,10
    mov ah,2
    int 21h
    mov edi,esi
    ret
ENDP SystemReadLn

.COMMENT *** Function SystemGotoXY: set cursor at CX, DX ***

PROC  SystemGotoXY NEAR
   mov dh,dl
   mov dl,cl
   mov bh,0
   mov ah,02
   int 10h
   ret
ENDP  SystemGotoXY

.COMMENT *** Function SystemSound: turn speaker on with frequency in AX  ***

PROC SystemSound NEAR
  mov    bx,ax
  mov    ax,34DDh
  mov    dx,12h
  cmp    dx,bx
  jnb short L2
  cmp bx,0
  je short L2
  div    bx
  mov    bx,ax
  in     al,61h
  test   al,3h
  jne short  L1
  or     al,3h
  out    61h,al
  mov    al,0B6h
  out    43h,al
L1:
  mov    al,bl
  out    42h,al
  mov    al,bh
  out    42h,al
L2:
  ret
ENDP SystemSound

.COMMENT *** Function SystemNoSound: turn speaker off ***

PROC SystemNoSound NEAR
  in     al,61h
  and    al,0FCh
  out    61h,al
  ret
ENDP SystemNoSound

.COMMENT *** Function SystemDelay: Delays for ms in AX ***

PROC SystemDelay NEAR
  mov BX, 1000
  mul BX
  mov CX, DX
  mov DX, AX
  mov AH, 86h
  int 15h
  ret
ENDP SystemDelay


.COMMENT ** Function SystemExec: executes a program prgname EDI, parameter line ESI **


PROC SYSTEMEXEC NEAR
  push es
  mov es,word ptr cs:[6]
  mov eax,0100h
L1:
  mov bl,[EDI]
  mov es:[eax],bl
  inc edi
  inc eax
  cmp bl,0
  jne short L1
  mov eax,038h
L2:
  inc eax
  mov bl,[ESI]
  mov es:[eax],bl
  inc esi
  cmp bl,0
  jne short L2
  mov byte ptr es:[eax],13
  sub eax,39h
  mov es:[38h],al
  mov ax,cs:[8]
  mov intes,ax
  mov intds,ax
  mov es:[4],ax
  mov es:[8],ax
  mov es:[12],ax
  mov ax,38h
  mov es:[2],ax
  mov ax,0eh
  mov es:[6],ax
  mov ax,1eh
  mov es:[0ah],ax
  mov eax,0202020h
  mov es:[0eh],eax
  mov es:[1eh],eax
  mov eax,20202020h
  mov es:[12h],eax
  mov es:[16h],eax
  mov es:[22h],eax
  mov es:[26h],eax
  xor eax,eax
  mov es:[1ah],eax
  mov es:[2ah],eax
  mov ax,0006h
  mov bx,cs:[2ch]
  int 31h
  shl ecx,16
  mov cx,dx
  shr ecx,4
  mov es:[0],cx
  mov intebx,0
  mov intedx,0100h
  mov inteax,04b00h
  mov edi,offset intedi
  xor ecx,ecx
  mov bx,21h
  mov es,word ptr cs:[0]
  mov ax,0300h
  int 31h
  pop es
  ret
ENDP SYSTEMEXEC


.COMMENT *** Function FindFile Searches a filename in EDI, in the directories given in ESI ***

PROC SystemFindFile NEAR; Filename in EDI, Directories in ESI
.DATA?
  FindFiles DB 64 DUP(?)
.CODE
  push fs
  push es
  mov fs,word ptr cs:[6]
  mov es,word ptr cs:[0]
  mov dword ptr fs:[0],0
; Copy Directory ...
L0:
  mov edx,0
  mov ecx,OFFSET Findfiles
L1:
  mov bl,[ESI]
  cmp bl,0
  je short L2
  cmp bl,';'
  je short L2
  mov fs:[EDX],bl
  mov [ecx],bl
  inc esi
  inc edx
  inc ecx
  jmp L1
L2:
; ADD Filename
  xor eax,eax
  mov bl,fs:[edx]
  cmp bl,0
  je short L3
  cmp bl,'\'
  je short L3
  mov byte ptr fs:[edx],'\'
  mov byte ptr [ecx],'\'
  inc edx
  inc ecx
L3:
  mov bl,[edi+eax]
  mov fs:[edx],bl
  mov [ecx],bl
  inc edx
  inc ecx
  inc eax
  cmp bl,0
  je short L4
  jmp L3
L4:
  push esi
  push edi
  mov intecx,0
  mov intedx,0
  mov ax,cs:[8]
  mov intds,ax
  mov inteax,4e00h
  mov edi,offset intedi
  mov es,word ptr cs:[0]
  mov ax,300h
  mov bx,21h
  xor ecx,ecx
  int 31h
  mov ax,intflg
  test ax,1
  jne short L5
  mov eax,offset FindFiles
  pop edi
  pop esi
  pop es
  pop fs
  clc
  ret
L5:
  pop edi
  pop esi
  mov bl,[esi]
  cmp bl,0
  je short L6
  inc esi
  jmp L0
L6:
  pop es
  pop fs
  mov eax,offset FindFiles
  mov dword ptr [eax],0
  stc
  ret
ENDP SystemFindfile

proc Systemgetrandom
  pushad
  add eax,eax
  push eax
  in al,40h
  mov cl,al
  mov eax,rand1
  xadd rand2,eax
  in al,40h
  add eax,rand3
  mov ah,cl
  mov rand1,eax
  xor edx,edx
  pop ebx
  cmp eax,0
  je short X
  div ebx
X:
  mov eax,rand2
  mov rand3,eax
  mov eax,rand1
  mov rand2,eax
  mov eax,edx
  shr eax,1
  push eax
  pop eax
  popad
  mov eax,[esp-8*4-4]
 ret
endp Systemgetrandom

.COMMENT *** Function DoubleToStr converts a double value in ST(0) to a string in EDI ***


PROC DoubleToStr   ; string in EDI
.data

  const1 re 1000000000000000000
  const2 re 10

.code
  push ebp
  mov ebp,esp
  sub esp,24
  push edi
  fstcw word ptr [ebp-18]
  push dword ptr [ebp-18]
  bts dword ptr [ebp-18],10
  bts dword ptr [ebp-18],11
  fldcw word ptr [ebp-18]
  fclex
  ftst
  fstsw AX
  sahf
  je Null
  fxam
  fstsw AX
  bt ax,14
  jnc valid
invalid:
  bt ax,8
  jc empty
  bt ax,9
  jnc nan
  jmp inv
valid:
  fstp tbyte ptr [ebp-10]
  mov dword ptr [ebp-14],0
  bt ax,8
  jc null
  fldlg2
  fld tbyte ptr [ebp-10]
  fyl2x
  fistp dword ptr [ebp-14]
  jmp NotNull
Null:
  jc invalid
  mov dword ptr [edi],'0.'
  jmp X
NotNull:
  mov dword ptr [ebp-24],0      ;   Exponent
  mov eax,dword ptr [ebp-14]
  cmp eax,4933                  ; Error
  jg INV
  cmp eax,-4933
  jl INV
  cmp eax,8
  jg extp
  cmp eax,-8
  jl expn
L0:
  fld tbyte ptr [ebp-10]
  fist dword ptr [ebp-14]
  mov eax, [ebp-14]
  call mathtostring
  mov eax,'.000'
  mov [edi],eax
  fild dword ptr [ebp-14]
  fsubp st(1),st
  fld const1
  fmulp st(1),st
  fbstp tbyte ptr [ebp-10]
  mov ecx,8
  inc edi
l1:
  mov al,ss:[ebp-10+ecx]
  shr al,4
  and al,0fh
  add al,48
  mov [edi],al
  inc edi
  mov al,ss:[ebp-10+ecx]
  and al,0fh
  add al,48
  mov [edi],al
  inc edi
  dec ecx
  jns l1
l2:
  mov byte ptr [edi],0
  dec edi
  mov al,[edi]
  cmp al,48
  je L2
  cmp dword ptr [ebp-24],0
  je L4
; need to add exponent ...
  mov word ptr [edi],' E'
  inc edi
  inc edi
  cmp dword ptr [ebp-24],0
  jl L3
  mov byte ptr [edi],'+'
  inc edi
L3:
  mov eax,dword ptr [ebp-24]
  call mathtostring
L4:
  jmp X

extp:  ; positive exponent is necessary!
  mov dword ptr [ebp-24],eax
  inc dword ptr [ebp-24]
  fld  const2
  dec eax
Lp:

  fld  const2
  fmulp st(1), st
  dec eax
  cmp eax,0
  jne Lp
;  fild dword ptr [ebp-14]
;  fldl2t
;  fmulp st(1),st
;  f2xm1
;  fld1
;  faddp st(1),st


  fld tbyte ptr [ebp-10]
  fdivrp st(1), st
  fstp tbyte ptr [ebp-10]
  jmp L0
expn:  ; negative exponent is necessary1
  mov dword ptr [ebp-24],eax
  dec dword ptr [ebp-24]
  neg eax
  fld  const2
Ln:
  fld  const2
  fmulp st(1), st
  dec eax
  cmp eax,0
  jne Ln
;  fild dword ptr [ebp-14]
;  fldl2t
;  fmulp st(1),st
;  f2xm1
;  fld1
;  faddp st(1),st

  fld tbyte ptr [ebp-10]
  fmulp st(1), st
  fstp tbyte ptr [ebp-10]
  jmp L0
NAN:
  mov eax,'NAN'
  mov [edi],eax
  xor eax,eax
  mov [edi+4],eax
  jmp X
INV:
  mov eax,'INV'
  mov [edi],eax
  xor eax,eax
  mov [edi+4],eax
  jmp X
Empty:
  mov eax,'EMPT'
  mov [edi],eax
  mov eax,'Y'
  mov [edi+4],eax
  jmp X

X:
  pop dword ptr [ebp-18]
  fldcw word ptr [ebp-18]
  pop edi
  mov esp,ebp
  pop ebp
  ret

ENDP DoubleToStr


.COMMENT *** Macro CheckCPU(CPUType) macro to check the CPU Type ***

.MACRO CheckCPU(CPUType)
.DATA
.LOCAL mesg db 'ERROR: need '
.LOCAL CPU  db '386 processor',0
.CODE
  mov ax,400h
  int 31h
  cmp cl,CPUType
  jae .local @next
  mov cl,CPUType
  add cl,48
  mov .local CPU,cl
  mov edi,offset .LOCAL mesg
  call systemwriteln
  mov ax,4c03h
  int 21h
.LOCAL @next:
ENDMACRO CheckCPU

;.IFPM
;.ELSE
;.OUT Target not for real mode
;.ERR
;.ENDIF

PROC SYSTEMMAIN START NEAR ; This procedure will initialize the system unit
     pushad                          ; save all registers ...
     mov ebx,lastdata                ; install HEAP
     mov HeapOrg,ebx
.IFPM
     mov ecx,memsize
.ELSE
     mov ecx,65536                   ; max real mode heap
.ENDIF
     sub ecx,4                       ; number of bytes on the heap
     mov HeapEnd,ecx
     sub ecx,ebx                     ; First Heap Entry:
     and ecx,0fffffffch              ; mark this pointer as free
     mov dword ptr [ebx],ecx         ; push value on the heap
     mov eax,ecx
.IFPM
.ELSE
     mov ax,cs                       ; installs Pro32 like PSP for RM code
     mov cs:[0],ax
     mov ax,0b800h
     mov cs:[2],ax
     mov ax,00000h
     mov cs:[4],ax
     mov fs,ax
     mov gs,ax
.ENDIF
     popad                           ; restore all registers ...
     ret
ENDP SYSTEMMAIN


;END SYSTEM.INC
