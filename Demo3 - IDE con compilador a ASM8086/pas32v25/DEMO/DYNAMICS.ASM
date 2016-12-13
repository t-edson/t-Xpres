.COMMENT
 ****************************************************************************
 DYNAMICS.ASM   Example file for dynamic memory allocation with SYSTEM.INC
                (c) 1996,1998 by Dieter Pawelczak - Pass32 Version 2.0
 ============================================================================
 ****************************************************************************
.MODEL FLAT
.UCU
.NM

.include system.inc             ; for dynamic memory functions...
.include system.mac             ; for macros

.DATA
     number   db '          ',0 ; String for number conversion
     pointers dd 20 dup(0)      ; Field that stores the allocated pointers...

.CODE
.PUBLIC START:
  mov edx,0

loop:   ; FIRST: Allocate 102400 byte memory blocks, as long as available

  push edx
  call systemmemavail
  mov edi,offset Number
  call mathtostring
  mov edi,offset Number
  call systemwrite
  Writeln(' Bytes available!')
  Writeln(' Allocate 102400 Bytes?')
  mov eax,102400
  call systemgetmem
  jc @next
  pop edx
  mov [offset pointers+4*edx],eax
  inc edx
  jmp loop
@next:
  pop edx
  dec edx
  push edx
  call systemmemavail
  mov edi,offset Number
  call mathtostring
  mov edi,offset Number
  call systemwrite
  Writeln(' Bytes available!')
  Writeln(' Free 102400 Bytes?')
  pop edx
loop2:  ; Second: Free all 102400 byte memory blocks...
  push edx
  mov edx,[pointers+4*edx]
  mov eax,102400
  call systemfreemem
  jc @next2
  Writeln(' Free 102400 Bytes?')
  pop edx
  cmp edx,0
  je @next2
  dec edx
  jmp loop2
@next2:
  call systemmemavail
  mov edi,offset Number
  call mathtostring
  mov edi,offset Number
  call systemwrite
  Writeln(' Bytes available!')
  Exit(0)

END START
END

