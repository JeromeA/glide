
BITS 64
DEFAULT REL

%define BASE_ADDR 0x400000

; =============================================================================
; ELF Header
; =============================================================================

    ; -- e_ident (16 bytes) --
    db 0x7F, "ELF"          ; ELF magic
    db 2                    ; EI_CLASS = 64-bit
    db 1                    ; EI_DATA  = LSB
    db 1                    ; EI_VERSION
    db 0                    ; EI_OSABI
    db 0                    ; EI_ABIVERSION
    times 7 db 0            ; EI_PAD
    dw 2                    ; e_type = ET_EXEC
    dw 0x3E                 ; e_machine = EM_X86_64
    dd 1                    ; e_version = EV_CURRENT
    dq BASE_ADDR + start    ; e_entry
    dq program_headers      ; e_phoff
    dq 0
    dd 0                    ; e_flags
    dw ElfHeader_end        ; e_ehsize
    dw 56                   ; e_phentsize for Elf64_Phdr
    dw 3                    ; e_phnum (PT_LOAD + PT_DYNAMIC + PT_INTERP)
    dw 0                    ; e_shentsize
    dw 0                    ; e_shnum
    dw 0                    ; e_shstrndx = index of .shstrtab

ElfHeader_end:

; =============================================================================
; Program Headers
; =============================================================================

program_headers:

ph_load:
    dd 1             ; p_type = PT_LOAD
    dd 7             ; p_flags = PF_R + PF_W + PF_X
    dq 0             ; p_offset
    dq BASE_ADDR     ; p_vaddr
    dq BASE_ADDR     ; p_paddr
    dq eof
    dq eof + bss_size
    dq 0x1000        ; p_align

ph_dynamic:
    dd 2             ; p_type = PT_DYNAMIC
    dd 6             ; p_flags = PF_R + PF_W
    dq dynamic_section
    dq BASE_ADDR + dynamic_section
    dq BASE_ADDR + dynamic_section
    dq (dynamic_section_end - dynamic_section)
    dq (dynamic_section_end - dynamic_section)
    dq 8

ph_interp:
    dd 3                 ; p_type = PT_INTERP
    dd 4                 ; p_flags = PF_R  (interp is read-only)
    dq interp_section    ; p_offset = file offset of the .interp bytes
    dq BASE_ADDR + interp_section  ; p_vaddr
    dq BASE_ADDR + interp_section  ; p_paddr
    dq (interp_section_end - interp_section)
    dq (interp_section_end - interp_section)
    dq 1                 ; p_align

program_headers_end:

; =============================================================================
; .interp Section
; =============================================================================

interp_section: db "/lib64/ld-linux-x86-64.so.2", 0
interp_section_end:

; =============================================================================
; .dynamic Section
; =============================================================================

align 8
dynamic_section:

; DT_SYMTAB
    dq 6, BASE_ADDR + dynsym

; DT_STRTAB
    dq 5, BASE_ADDR + dynstr

; DT_RELA
    dq 7, BASE_ADDR + rela_text

; DT_RELASZ
    dq 8, (rela_text_end - rela_text)

; DT_RELAENT
    dq 9, 24 ; sizeof(Elf64_Rela)=24

; DT_NEEDED
    dq 1, str_libc - dynstr
    dq 1, str_libgtk - dynstr
    dq 1, str_libgobject - dynstr
    dq 1, str_libgtksource - dynstr

; The remaining entries are supposed to be mandatory, but nothing happens when they're missing.
; DT_SYMENT
;    dq 11               ; d_tag = DT_SYMENT
;    dq 24               ; sizeof(Elf64_Sym)=24
; DT_STRSZ
;    dq 10               ; d_tag = DT_STRSZ
;    dq (dynstr_end - dynstr)
; DT_NULL
;    dq 0
;    dq 0

dynamic_section_end:

; =============================================================================
; .text Section
; =============================================================================

start:
    ; Extract argc (number of arguments)
    mov rdi, [rsp]     ; First item on the stack is argc

    ; Extract argv (pointer to the array of arguments)
    lea rsi, [rsp + 8] ; Next item on the stack is a pointer to argv[0]

    ; Call main(argc, argv)
    call main

