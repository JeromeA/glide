
    .code64
    .section .text
    .set BASE_ADDR, 0x400000
file_start:
ElfHeader:
    # =============================================================================
    # ELF Header
    # =============================================================================
    .byte 0x7F, 'E', 'L', 'F'      # ELF magic
    .byte 2                        # EI_CLASS = 64-bit
    .byte 1                        # EI_DATA = LSB
    .byte 1                        # EI_VERSION
    .byte 0                        # EI_OSABI
    .byte 0                        # EI_ABIVERSION
    .zero 7                        # EI_PAD
    .word 2                        # e_type = ET_EXEC
    .word 0x3E                     # e_machine = EM_X86_64
    .long 1                        # e_version = EV_CURRENT
    .quad BASE_ADDR + start-file_start # e_entry
    .quad program_headers-file_start # e_phoff
.ifdef GDB
    .quad section_headers-file_start # e_shoff
.else
    .quad 0                        # e_shoff
.endif
    .long 0                        # e_flags
    .word ElfHeader_end-ElfHeader  # e_ehsize
    .word 56                       # e_phentsize (size of Elf64_Phdr)
    .word 3                        # e_phnum (PT_LOAD + PT_DYNAMIC + PT_INTERP)
.ifdef GDB
    .word 64                       # e_shentsize
    .word 3                        # e_shnum
    .word 2                        # e_shstrndx = index of .shstrtab
.endif
ElfHeader_end:

    # =============================================================================
    # Program Headers
    # =============================================================================
program_headers:

ph_load:
    .long 1                      # p_type = PT_LOAD
    .long 7                      # p_flags = PF_R + PF_W + PF_X
    .quad 0                      # p_offset
    .quad BASE_ADDR              # p_vaddr
    .quad BASE_ADDR              # p_paddr
    .quad eof - file_start       # p_filesz
    .quad bss_size
    .quad 0x1000                 # p_align

ph_dynamic:
    .long 2                      # p_type = PT_DYNAMIC
    .long 6                      # p_flags = PF_R + PF_W
    .quad dynamic_section - file_start  # p_offset
    .quad BASE_ADDR + (dynamic_section - file_start)  # p_vaddr
    .quad BASE_ADDR + (dynamic_section - file_start)  # p_paddr
    .quad (dynamic_section_end - dynamic_section)
    .quad (dynamic_section_end - dynamic_section)
    .quad 8

ph_interp:
    .long 3                      # p_type = PT_INTERP
    .long 4                      # p_flags = PF_R (read-only)
    .quad interp_section         # p_offset = file offset of .interp section
    .quad BASE_ADDR + (interp_section - file_start) # p_vaddr
    .quad BASE_ADDR + (interp_section - file_start) # p_paddr
    .quad (interp_section_end - interp_section)
    .quad (interp_section_end - interp_section)
    .quad 1                      # p_align

program_headers_end:

    # =============================================================================
    # .interp Section
    # =============================================================================
interp_section:
    .asciz "/lib64/ld-linux-x86-64.so.2"
interp_section_end:

    # =============================================================================
    # .dynamic Section
    # =============================================================================
    .balign 8
dynamic_section:
    .quad 6, BASE_ADDR + dynsym         # DT_SYMTAB
    .quad 5, BASE_ADDR + dynstr         # DT_STRTAB
    .quad 7, BASE_ADDR + rela_text      # DT_RELA
    .quad 8, (rela_text_end - rela_text)  # DT_RELASZ
    .quad 9, 24                         # DT_RELAENT (sizeof(Elf64_Rela)=24)
# Here: DT_NEEDED

.ifdef GDB
    .quad 4, BASE_ADDR + hash           # DT_HASH
    .quad 10, dynstr_end - dynstr         # DT_STRSZ
.endif
    .quad 0                             # DT_NULL

dynamic_section_end:

.ifdef GDB
    # =============================================================================
    # HASH Section
    # =============================================================================
hash:

    # =============================================================================
    # Section Headers
    # =============================================================================
section_headers:
    .zero 64                  # null header

    .long str_text - shstrtab    # sh_name: offset of ".text" in shstrtab
    .long 1                      # sh_type: SHT_PROGBITS
    .quad 6                    # sh_flags: SHF_ALLOC | SHF_EXECINSTR = 6
    .quad BASE_ADDR + start      # sh_addr: virtual address of .text
    .quad start                  # sh_offset: file offset of .text
    .quad eof - start            # sh_size: size of .text section
    .long 0                      # sh_link
    .long 0                      # sh_info
    .quad 16                   # sh_addralign: 16-byte alignment
    .quad 0                    # sh_entsize

    .long str_shstrtab - shstrtab  # sh_name: offset of ".shstrtab" in shstrtab
    .long 3                      # sh_type: SHT_STRTAB
    .quad 0                    # sh_flags
    .quad 0                    # sh_addr
    .quad shstrtab             # sh_offset
    .quad shstrtab_end - shstrtab  # sh_size: size of .shstrtab
    .long 0                    # sh_link
    .long 0                    # sh_info
    .quad 1                    # sh_addralign
    .quad 0                    # sh_entsize

shstrtab:
    .byte 0                   # The first string must be null
str_text:
    .asciz ".text"
str_shstrtab:
    .asciz ".shstrtab"
shstrtab_end:
.endif

    # =============================================================================
    # .text Section
    # =============================================================================
    .globl start
start:
    # Extract argc (number of arguments)
    mov (%rsp), %rdi        # first item on the stack is argc

    # Extract argv (pointer to the array of arguments)
    lea 8(%rsp), %rsi       # next item on the stack is pointer to argv[0]

    # Call main(argc, argv)
    call main

