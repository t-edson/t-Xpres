.COMMENT
 ****************************************************************************
 TESTPAL.ASM    Example file for module GRAPH.INC
                (c) 1996,1998 by Dieter Pawelczak - Pass32 Version 2.0
 ============================================================================
 ****************************************************************************
.MODEL FLAT
.UCU
.NM

.INCLUDE GRAPH.INC

.CODE
.PUBLIC START:
      call initgraph                    ; initialize graphics
      mov dx,0
  @Loop:
      mov cx,0
  @LineLoop:
      mov bx,dx
      shr bx,1
      add bx,cx
      shr bx,2
      call putpixel
      inc cx
      cmp cx,320
      jb @LineLoop
      inc dx
      cmp dx,200                        ; Fill whole Screen
      jb @Loop
      mov ax,4c00h                      ; terminate
      int 21h
  END START
  END




