.COMMENT
 ****************************************************************************
 GAME.INC     Some useful PM functions to access the Joystick
              Pass32 Version 2.0
 ============================================================================
 INTERFACE

 Function CheckforJoystick : Returns in EAX=0 if no joystick
 Function GetXAxis    : Returns value in EAX proportional to the X-Value
 Function GetYAxis    : Returns value in EAX proportional to the Y-Value
 Function CheckFire   : Zero-Flag set if Fire is pressed
 Function CheckButton : Zero-Flag set if Button is pressed


 ****************************************************************************
.CODE

PROC Checkforjoystick ; Returns in EAX=0 if no joystick
  mov dx,201h
  mov al,98
  out dx,al
  mov ah,1
  mov esi,10000h
  @next:
  in al,dx
  test al,ah
  je @finished
  dec esi
  cmp esi,0
  jnz @next
  xor eax,eax
  ret
  @Finished:
  mov eax,1
  ret
ENDP Checkforjoystick ;

PROC GetXAxis  ; Returns value in EAX proportional to the X-Value
  mov dx,201h
  mov al,98
  out dx,al
  mov ah,1
  mov esi,0
  @next:
  in al,dx
  test al,ah
  je @finished
  inc esi
  jmp @next
  @Finished:
  mov eax,esi
  ret
ENDP GetXAxis

PROC GetYAxis  ; Returns value in EAX proportional to the Y-Value
  mov dx,201h
  mov al,98
  out dx,al
  mov ah,2
  mov esi,0
  @next:
  in al,dx
  test al,ah
  je @finished
  inc esi
  jmp @next
  @Finished:
  mov eax,esi
  ret
ENDP GetYAxis

PROC CheckFire  ; Zero-Flag set if Fire is pressed
  mov dx,201h
  in al,dx
  test al,16
  ret
ENDP CheckFire

PROC CheckButton  ; Zero-Flag set if Button is pressed
  mov dx,201h
  in al,dx
  test al,32
  ret
ENDP CheckButton

