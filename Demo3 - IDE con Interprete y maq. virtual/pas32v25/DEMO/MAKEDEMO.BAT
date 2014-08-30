@echo  Assembling: CIRCLE.ASM
@pass32 CIRCLE.ASM /S
@echo  Assembling:  CIRCLE2.ASM
@pass32 CIRCLE2.ASM /S
@echo  Assembling:  DISPLAY.ASM
@pass32 DISPLAY.ASM /NM /UC /S
@echo  Assembling:  DLLTEST.ASM
@pass32 DLLTEST.ASM /NM /S
@echo  Assembling:  GRAPHDLL.ASM
@pass32 GRAPHDLL.ASM /DLL /S
@echo  Assembling:  HELLO1.ASM
@pass32 HELLO1.ASM /S
@echo  Assembling:  HELLO2.ASM
@pass32 HELLO2.ASM  /NM /UC /S
@echo  Assembling:  HELLO3.ASM
@pass32 HELLO3.ASM  /NM /UC /S
@echo  Assembling:  MSDEMO.ASM
@pass32 MSDEMO.ASM /S
@echo  Assembling:  MSDEMO2.ASM
@pass32 MSDEMO2.ASM /S
@echo  Assembling:  TESTDLL.ASM
@pass32 TESTDLL.ASM  /DLL /S
@echo  Assembling:  TESTPAL.ASM
@pass32 TESTPAL.ASM /NM /S
@echo  Assembling:  TESTPARA.ASM
@pass32 TESTPARA.ASM /NM /S
@echo  Assembling:  TESTOVL.ASM
@pass32 TESTOVL.ASM /NM /S /OVL
@echo  Assembling:  OVLTEST.ASM
@pass32 OVLTEST.ASM /NM /S /UC
@echo  Assembling:  DYNAMICS.ASM
@pass32 DYNAMICS.ASM /NM /S /UC
@echo  Assembling:  EXEC.ASM
@pass32 EXEC.ASM /NM /S
@echo  Assembling:  JOYSTICK.ASM
@pass32 JOYSTICK.ASM /NM /S /UC
@echo  Assembling:  MACRO.ASM
@pass32 MACRO.ASM /NM /S /UC
@echo  Assembling:  MACRO2.ASM
@pass32 MACRO2.ASM /NM /S /UC
@echo  Assembling:  PCOPY.ASM
@pass32 PCOPY.ASM /NM /S /UC
@echo  Assembling:  EVENT2.ASM
@pass32 EVENT2.ASM /NM /S /UC
@echo  Assembling:  TYPES.ASM
@pass32 TYPES.ASM /NM /S /UC
