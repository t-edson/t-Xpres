.COMMENT
 ****************************************************************************
 MSDEMO2.ASM    Example file for to deomstrate mouse support
                (c) 1996,1998 by Dieter Pawelczak - Pass32 Version 2.0
 ============================================================================
 ****************************************************************************
.MODEL FLAT
.UCU
.NM
.DATA
.ALIGN
     mesg       db      'PM Mouse Demo',0
     mesg1      db      '(c) 1997 by Dieter Pawelczak',0
     mesg2      db      'Created with GraphDLL',0
     ErrorMesg  db      'ERROR: Supports only Serial Mouse on Port COM1 or COM2!',0
     mesgr      db      'Right Button pressed!',0
     mesgl      db      'Left Button pressed!',0
     .BLOCK
     mesg3      db      'X pos:'
     xposstr    db      '              Y pos: '
     yposstr    db      '                ',0
     .NOBLOCK
     oldx       dw      100
     oldy       dw      100
     msobj      db      31,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                db      31,31,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                db      31,31,31,0,0,0,0,0,0,0,0,0,0,0,0,0
                db      31,31,31,31,0,0,0,0,0,0,0,0,0,0,0,0
                db      31,31,31,31,31,0,0,0,0,0,0,0,0,0,0,0
                db      31,31,31,31,31,31,0,0,0,0,0,0,0,0,0,0
                db      31,31,31,31,31,31,31,0,0,0,0,0,0,0,0,0
                db      31,31,31,31,31,31,31,31,0,0,0,0,0,0,0,0
                db      31,31,31,31,31,31,31,31,31,0,0,0,0,0,0,0
                db      31,31,0,31,31,0,0,0,0,0,0,0,0,0,0,0
                db      31,0,0,0,31,31,0,0,0,0,0,0,0,0,0,0
                db      0,0,0,0,31,31,0,0,0,0,0,0,0,0,0,0
                db      0,0,0,0,0,31,31,0,0,0,0,0,0,0,0,0
                db      0,0,0,0,0,31,31,0,0,0,0,0,0,0,0,0
                db      0,0,0,0,0,0,31,31,0,0,0,0,0,0,0,0
                db      0,0,0,0,0,0,31,31,0,0,0,0,0,0,0,0
      msobj2    db      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                db      0,31,31,31,31,31,31,31,31,31,31,31,31,31,31,0
                db      0,31,31,0,0,0,0,0,0,0,0,0,0,31,31,0
                db      0,0,31,31,0,0,0,0,0,0,0,0,31,31,0,0
                db      0,0,0,31,31,0,0,0,0,0,0,31,31,0,0,0
                db      0,0,0,0,31,31,31,31,31,31,31,31,0,0,0,0
                db      0,0,0,0,0,31,31,31,31,31,31,0,0,0,0,0
                db      0,0,0,0,0,0,31,31,31,31,0,0,0,0,0,0
                db      0,0,0,0,0,31,31,0,0,31,31,0,0,0,0,0
                db      0,0,0,0,31,31,0,31,0,0,31,31,0,0,0,0
                db      0,0,0,31,31,0,0,0,0,0,0,31,31,0,0,0
                db      0,0,31,31,0,0,0,31,0,0,0,0,31,31,0,0
                db      0,31,31,0,0,0,31,31,31,31,0,0,0,31,31,0
                db      0,31,31,31,31,31,31,31,31,31,31,31,31,31,31,0
                db      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                db      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
     msimg      db 256 dup(0)
     msobject   dd ?

.INCLUDE msdemo.inc
.INCLUDE graph2.asm

.CODE
START:
  call InitGraph
  call InitMouse
  jc MouseInitError
  mov msobject,offset msobj
  mov msmaxx,310
  mov msmaxy,170
  mov edx,offset DrawMouse
  mov dword ptr MSDRAW,edx
  mov cx,cs
  mov word ptr MSDRAW+4,cx
  mov edi,offset mesg
  mov cx,100
  mov dx,4
  mov bl,70
  call colorouttextxy
  mov edi,offset mesg1
  mov cx,40
  mov dx,20
  mov bl,72
  call colorouttextxy
  mov edi,offset mesg2
  mov cx,70
  mov dx,40
  mov bl,7
  call colorouttextxy
  mov bl,73
  mov cx,0
  mov dx,0
  mov si,319
  mov di,176
  call rectangle
  .FAR
  call drawmouse
  .NOFAR


@loop:
  mov bl,0
  cmp byte ptr msright,1
  jne short Next
  mov msobject, offset msobj
  mov bl,9
Next:
  mov edi,offset mesgr
  mov dx,180
  mov cx,160
  call outtextxy
  mov bl,0
  cmp byte ptr msleft,1
  jne Next2
  mov bl,55
  mov msobject, offset msobj2
Next2:
  mov dx,180
  mov cx,0
  mov edi,offset mesgl
  call outtextxy
  mov edi,offset xposstr
  movzx eax,msx
  call mathtostring
  mov edi,offset yposstr
  movzx eax,msy
  call mathtostring
  mov edi,offset mesg3
  mov ecx,20
TestStr:
  cmp byte ptr [edi+ecx],0
  jne @NoStrNil
  mov byte ptr [edi+ecx],32
@NoStrNil:
  loop TestStr
  call waitforverticalretrace
  xor cx,cx
  mov dx,188
  mov si,300
  mov di,188+8
  mov bl,0
  call FillBlock
  mov bl,46
  mov dx,188
  mov cx,0
  mov edi,offset mesg3
  call outtextxy
  call systemkeypressed
  je @loop
  call systemgetkey
  call settextmode
  mov ax,4c00h
  int 21h

MouseInitError:
  call settextmode
  mov edi, Offset ErrorMesg
  call systemwriteln
  mov ax,4c01h
  int 21h


.ALIGN
PROC DrawMouse FAR
  push es
  mov cx,oldx
  mov dx,oldy
  mov edi,offset msimg
  mov si,16
  mov bx,si
  call putimage
  mov cx,msx
  mov oldx,cx
  mov dx,msy
  mov oldy,dx
  mov si,16
  mov bx,si
  mov edi,offset msimg
  call getimage
  mov cx,msx
  mov dx,msy
  mov si,16
  mov bx,16
  mov edi,msobject
  call putobject
  pop es
  ret
ENDP DrawMouse
.INCLUDE SYSTEM.INC
END START
END
