; How a Computer Starts:
; --- BIOS is copied from ROM into RAM
; --- BIOS executes code, initializes hardware, and runs POST (power-on self test)
; --- BIOS searches for OS to start 
; --- BIOS loads OS, OS runs 

; * Legacy Booting:
; --- BIOS loads first sector of each bootable device into memory at location 0x7C00
; --- BIOS checks for 0xAA55 signature. If found, executes code.
; EFI:
; --- BIOS looks for special EFI partitions
; --- OS must be compiled as EFI program
