.COMMENT
 ****************************************************************************
 DPMI.INC       DPMI Register Structure for real mode calls
                (c) 1996,1997 by Dieter Pawelczak - Pass32 Version 2.0
 ============================================================================
 ****************************************************************************

.PUBLIC .DATA   ; Export all variables ...

.BLOCK
      intedi dd 0
      intesi dd 0
      intbp  dw 0
      intbph dw 0
      intddw dd 0
      intebx dd 0
      intedx dd 0
      intecx dd 0
      inteax dd 0
      intflg dw 0
      intes  dw 0
      intds  dw 0
      intfs  dw 0
      intgs  dw 0
      intip  dw 0
      intcs  dw 0
      intsp  dw 0
      intss  dw 0
.NOBLOCK

;END DPMI.INC

