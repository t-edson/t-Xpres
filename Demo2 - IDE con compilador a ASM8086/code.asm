
.MODEL TINY
.DATA
  _tmpStr0 DB 255 DUP('#'),0
  x DW 0
.CODE
  ;expres
  mov al, '5'
  push ax
  pop dx
  mov ah, 02h
  int 21h
  ;fin expres
END
