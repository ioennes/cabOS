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

; *** Directives ------------

org 0x7C00
bits 16

%define ENDL 0x0D, 0x0A

; *** Instructions ---------

start:

  jmp main

puts:

  ; Save registers we will modify

  push  si 
  push  ax

.loop:

  lodsb         ; loads next character in al
  or    al, al  ; check if next char is null
  jz    .done 

  ; Interrupts: a signal which makes the processor stop what it's doing
  ; --- can be triggered by: exception, hardware, or software (INT)
  ; --- INT 10h -> Video,       INT 11h -> Equipment Check 
  ; --- INT 12h -> Memory Size, INT 13h -> Disk I/O 
  ; --- INT 14h -> Serial Comm, INT 15h -> Casette
  ; --- INT 16h -> Keyboard IO

  mov   ah, 0x0E
  mov   bh, 0x0
  int   0x10
  jmp   .loop

.done:

  pop   ax
  pop   si 
  ret


main:

  ; CS          -> currently running code segment
  ; DS          -> data segment 
  ; SS          -> stack segment
  ; ES, FS, GS  -> extra data segments

  ; Setting up data segments

  mov   ax, 0
  mov   ds, ax
  mov   es, ax

  ; Setting up the stack 

  mov   ss, ax
  mov   sp, 0x7C00  ; stack grows downward from this memory location

  mov   si, msg
  call  puts

.halt:
  jmp   .halt

msg: db 'Hello', ENDL, 0

times   510-($-$$) db 0 ; $ -> position of line start, $$ -> position of current section start, $-$$ -> length of program in bytes
dw      0x0AA55
