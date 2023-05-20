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

; *** FAT12 Header ---------

jmp short jumper
nop

bdb_oem:                    db 'MSWIN4.1' ; 8 bytes
bdb_bytes_per_sector:       dw 512
bdb_sectors_per_cluster:    db 1
bdb_reserved_sectors:       dw 2
bdb_fat_count:              db 2
bdb_dir_entries_count:      dw 0x0E0
bdb_total_sectors:          dw 2880
bdb_media_descriptor_type:  db 0x0F0
bdb_sectors_per_fat:        dw 9
bdb_sectors_per_track:      dw 18
bdb_heads:                  dw 2 
bdb_hidden_sectors:         dd 0 
bdb_large_sector_count:     dd 0 

; extended boot record

ebr_drive_number:           db 0 
                            db 0 
ebr_signature:              db 0x29
ebr_volume_id:              db 0x12, 0x34, 0x56, 0x78
ebr_volume_label:           db 'COS      '
ebr_system_id:              db 'FAT12    '

jumper:
  
  jmp   start

; *** Instructions ---------

; LBA ==> CHS 
; sector    = (LBA % bdb_sectors_per_track) + 1 
; head      = (LBA / bdb_sectors_per_track) % bdb_heads
; cylinder  = (LBA / bdb_sectors_per_track) / bdb_heads

lba_to_chs:
  
  ; Parameter: ax -> LBA address
  ; Returns:
  ; --- cx <0...5>  -> sector number 
  ; --- cx <6...15> -> cylinder 
  ; --- dh          -> head 

  push  ax
  push  dx
  
  xor   dx, dx                          ;   => dx = 0 
  div   word [bdb_sectors_per_track]    ;   => ax = LBA / bdb_sectors_per_track
                                        ;   => dx = LBA % bdb_sectors_per_track
  inc   dx                              ;   => dx = sector 
  mov   cx, dx                          ;   => cx = dx

  xor   dx, dx
  div   word [bdb_heads]                ;   => ax = cylinder
                                        ;   => dx = head 
  mov   dh, dl
  mov   ch, al
  shl   ah, 6
  or    cl, ah

  pop   ax
  mov   dl, al                          ;   => restore dl
  pop   ax
  ret 

disk_read:

  ; Parameters
  ; --- ax    -> LBA address
  ; --- cl    -> number of sectors to read (up to 128)
  ; --- dl    -> drive number 
  ; --- es:bx -> memory address to store read data 

  push  ax
  push  bx
  push  cx
  push  dx
  push  di

  push  cx 
  call  lba_to_chs                      ;   => compute CHS
  pop   ax                              ;   => al = number of sectors to read 

  mov   ah, 0x02
  mov   di, 3 

.retry:

  pusha                                 ;   => save all registers
  stc                                   ;   => set carry flag
  int   0x13                            ;   => Disk I/O 
  jnc   .done 

  ; failed

  popa
  call  disk_reset
  dec   di 
  test  di, di 
  jnz   .retry 

.fail:

  jmp   floppy_error

.done:

  popa

  pop  di
  pop  dx
  pop  cx
  pop  bx
  pop  ax

disk_reset:
  
  pusha 
  mov   ah, 0 
  stc   
  int   0x13 
  jc    floppy_error
  popa 
  ret

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

start:

  ; CS          -> currently running code segment
  ; DS          -> data segment 
  ; SS          -> stack segment
  ; ES, FS, GS  -> extra data segments

  ; Setting up data segments

  mov   ax, 0   ; cant set es/ds directly
  mov   ds, ax
  mov   es, ax

  mov   si, msg 
  call  puts

  ; Setting up the stack 

  mov   ss, ax
  mov   sp, 0x7C00  ; stack grows downward from this memory location


  ; The below is recommended because some BIOSes start at 07C0:0000 instead of 0000:07C0 

  push  es 
  push  word .after
  retf

.after:

  ; Reading from disk 
  mov   [ebr_drive_number], dl    ; dl => ebr_drive_number


  ; Read drive parameters

  push  es 
  mov   ah, 0x08 
  jc    floppy_error
  pop   es 

  and   cl, 0x3F                    ; remove top 2 bits 
  xor   ch, ch 
  mov   [bdb_sectors_per_track], cx ; sector count 

  inc   dh 
  mov   [bdb_heads], dh             ; head count 

  ; Read FAT root directory
  mov   ax, [bdb_sectors_per_fat]   ; lba of root dir = reserved + fats * fat_size
  mov   bl, [bdb_fat_count]
  xor   bh, bh 
  mul   bx
  add   ax, [bdb_reserved_sectors]
  push  ax

  mov   ax, [bdb_sectors_per_fat]   ; root dir size = 32 * (number of entries / bdb_bytes_per_sector)
  shl   ax, 5 
  xor   dx, dx 
  div   word [bdb_bytes_per_sector]

  test  dx, dx                      ; if dx != 0, ++dx
  jz    .root_dir_after
  inc   ax

.root_dir_after:

  mov   cl, al                      ; cl = num of sectors to read = size of root dir
  pop   ax                          ; ax = LBA of root dir
  mov   dl, [ebr_drive_number]      ; dl = drive number
  mov   bx, buffer                  ; es:bx = buffer

  call  disk_read

  ; searching for kernel.bin 

  xor   bx, bx
  mov   di, buffer 

.search_kernel:

  mov   si, file_kernel_bin
  mov   cx, 11
  push  di 
  repe  cmpsb 
  pop   di 
  je    .found_kernel

  add   di, 32
  inc   bx
  cmp   bx, [bdb_dir_entries_count]
  jl    .search_kernel

  jmp   kernel_not_found_error

.found_kernel:

  ; di should have address to entry

  mov   ax, [di + 26]         ; first logical cluster field
  mov   [kernel_cluster], ax

  ; load FAT into memory 

  mov   ax, [bdb_reserved_sectors]
  mov   bx, buffer
  mov   cl, [bdb_sectors_per_fat]
  mov   dl, [ebr_drive_number]
  call  disk_read

  ; read kernel and process FAT chain 

  mov   bx, KERNEL_LOAD_SEGMENT
  mov   es, bx
  mov   bx, KERNEL_LOAD_OFFSET


.load_kernel_loop:
 
  ; Read next cluster 

  mov   ax, [kernel_cluster]
  add   ax, 31 

  mov   cl, 1 
  mov   dl, [ebr_drive_number]
  call  disk_read

  add   bx, [bdb_bytes_per_sector]

  ; compute location of next cluster 

  mov   ax, [kernel_cluster]
  mov   cx, 3 
  mul   cx 
  mov   cx, 2 
  div   cx

  mov   si, buffer 
  add   si, ax
  mov   ax, [ds:si]

  or    dx, dx
  jz    .even

.odd:

  shr   ax, 4 
  jmp   .next_cluster_after

.even:

  and   ax, 0x0FFF

.next_cluster_after:

  cmp   ax, 0x0FF8 
  jae   .read_finish

  mov   [kernel_cluster], ax
  jmp   .load_kernel_loop

.read_finish:

  ; Boot device in dl 

  mov   dl, [ebr_drive_number]

  mov   ax, KERNEL_LOAD_SEGMENT       ; set segment registers 
  mov   ds, ax
  mov   es, ax

  jmp   KERNEL_LOAD_SEGMENT:KERNEL_LOAD_OFFSET

  ; below should not happen!

  jmp   wait_key_and_reboot

  cli 
  hlt

floppy_error:

  mov   si, msg_read_fail
  call  puts 
  jmp   wait_key_and_reboot

kernel_not_found_error:

  mov   si, msg_kernel_not_found
  call  puts 
  jmp   wait_key_and_reboot

wait_key_and_reboot:
  
  mov   ah,  0 
  int   0x16        ; wait for keypress
  jmp   0xFFFF0000  ; jump to start of BIOS 

.halt:
  cli       ;   disable interrupts 
  hlt

msg:                      db 'Hello from kernel', ENDL, 0
msg_read_fail:            db 'Read failed', ENDL, 0
msg_kernel_not_found:     db 'KERNEL.BIN not found', ENDL, 0
file_kernel_bin:          db 'KERNEL  BIN'
kernel_cluster:           dw 0 

KERNEL_LOAD_SEGMENT:      equ 0x2000
KERNEL_LOAD_OFFSET:       equ 0

times   510-($-$$) db 0 ; $ -> position of line start, $$ -> position of current section start, $-$$ -> length of program in bytes
dw      0x0AA55

buffer:
