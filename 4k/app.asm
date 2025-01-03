
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
; Nothing seems to happen when the following fields are missing are overwritten.
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
    dq 0, 0

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

LC0:
    db "_Open", 0
LC1:
    db "_Cancel", 0
LC2:
    db "Open File", 0
LC3:
    db "r", 0
LC4:
    db "Failed to open file: %s\n", 0
on_open_file:
    endbr64
    push r15
    lea rcx, [LC1]
    xor edx, edx
    xor eax, eax
    push r14
    lea r9, [LC0]
    mov r8d, -6
    lea rdi, [LC2]
    push r13
    push r12
    push rbp
    mov rbp, rsi
    xor esi, esi
    push rbx
    sub rsp, 24
    push 0
    push -3
    call [gtk_file_chooser_dialog_new]
    mov rdi, rax
    mov rbx, rax
    call [gtk_dialog_run]
    pop rdx
    pop rcx
    cmp eax, -3
    je .L7
.L2:
    add rsp, 24
    mov rdi, rbx
    pop rbx
    pop rbp
    pop r12
    pop r13
    pop r14
    pop r15
    jmp [gtk_widget_destroy]
.L7:
    mov rdi, rbx
    call [gtk_file_chooser_get_filename]
    lea rsi, [LC3]
    mov rdi, rax
    mov r14, rax
    call [fopen]
    mov r12, rax
    test rax, rax
    je .L3
    mov edx, 2
    xor esi, esi
    mov rdi, rax
    call [fseek]
    mov rdi, r12
    call [ftell]
    xor edx, edx
    xor esi, esi
    mov rdi, r12
    mov r15, rax
    call [fseek]
    lea rsi, 1[r15]
    mov rdi, rsi
    mov QWORD [rsp + 8], rsi
    call [g_malloc]
    mov rsi, QWORD [rsp + 8]
    mov r8, r12
    mov rcx, r15
    mov edx, 1
    mov rdi, rax
    mov r13, rax
    call [__fread_chk]
    mov rdi, r12
    mov BYTE 0[r13+rax], 0
    call [fclose]
    mov rdi, rbp
    mov edx, -1
    mov rsi, r13
    call [gtk_text_buffer_set_text]
    mov rdi, r13
    call [g_free]
.L4:
    mov rdi, r14
    call [g_free]
    jmp .L2
.L3:
    mov rsi, r14
    lea rdi, [LC4]
    xor eax, eax
    call [g_printerr]
    jmp .L4
LC5:
    db "delete-event", 0
LC6:
    db "commonlisp", 0
LC7:
    db "File", 0
LC8:
    db "Open...", 0
LC9:
    db "Quit", 0
LC10:
    db "activate", 0
main:
    endbr64
    push r15
    push r14
    push r13
    push r12
    push rbp
    push rbx
    sub rsp, 40
    mov DWORD [rsp + 28], edi
    lea rdi, [rsp + 28]
    mov QWORD [rsp + 16], rsi
    lea rsi, [rsp + 16]
    call [gtk_init]
    xor edi, edi
    call [gtk_window_new]
    mov edx, 600
    mov esi, 800
    mov rdi, rax
    mov rbx, rax
    call [gtk_window_set_default_size]
    xor r9d, r9d
    xor r8d, r8d
    xor ecx, ecx
    mov rdx, QWORD [gtk_main_quit]
    lea rsi, [LC5]
    mov rdi, rbx
    call [g_signal_connect_data]
    xor esi, esi
    xor edi, edi
    call [gtk_scrolled_window_new]
    mov edx, 1
    mov esi, 1
    mov rdi, rax
    mov rbp, rax
    call [gtk_scrolled_window_set_policy]
    call [gtk_source_language_manager_get_default]
    lea rsi, [LC6]
    mov rdi, rax
    call [gtk_source_language_manager_get_language]
    mov rdi, rax
    call [gtk_source_buffer_new_with_language]
    mov rdi, rax
    mov QWORD [rsp + 8], rax
    call [gtk_source_view_new_with_buffer]
    mov esi, 1
    mov r12, rax
    mov rdi, rax
    call [gtk_source_view_set_show_line_numbers]
    mov rsi, r12
    mov rdi, rbp
    call [gtk_container_add]
    call [gtk_menu_bar_new]
    mov QWORD [rsp], rax
    call [gtk_menu_new]
    lea rdi, [LC7]
    mov r12, rax
    call [gtk_menu_item_new_with_label]
    lea rdi, [LC8]
    mov r15, rax
    call [gtk_menu_item_new_with_label]
    lea rdi, [LC9]
    mov r14, rax
    call [gtk_menu_item_new_with_label]
    mov rsi, r12
    mov rdi, r15
    mov r13, rax
    call [gtk_menu_item_set_submenu]
    mov rdi, r12
    mov rsi, r14
    call [gtk_menu_shell_append]
    mov rdi, r12
    mov rsi, r13
    lea r12, [LC10]
    call [gtk_menu_shell_append]
    mov rsi, r15
    mov r15, QWORD [rsp]
    mov rdi, r15
    call [gtk_menu_shell_append]
    mov rcx, QWORD [rsp + 8]
    xor r9d, r9d
    mov rsi, r12
    xor r8d, r8d
    lea rdx, [on_open_file]
    mov rdi, r14
    call [g_signal_connect_data]
    xor r9d, r9d
    mov rsi, r12
    xor r8d, r8d
    mov rdx, QWORD [gtk_main_quit]
    xor ecx, ecx
    mov rdi, r13
    call [g_signal_connect_data]
    xor esi, esi
    mov edi, 1
    call [gtk_box_new]
    xor r8d, r8d
    xor ecx, ecx
    xor edx, edx
    mov r12, rax
    mov rdi, rax
    mov rsi, r15
    call [gtk_box_pack_start]
    xor r8d, r8d
    mov ecx, 1
    mov rsi, rbp
    mov edx, 1
    mov rdi, r12
    call [gtk_box_pack_start]
    mov rsi, r12
    mov rdi, rbx
    call [gtk_container_add]
    mov rdi, rbx
    call [gtk_widget_show_all]
    call [gtk_main]
    xor edi, edi
    call [exit]

dynsym:
    dq 0, 0, 0
    dd str_g_free - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_source_language_manager_get_default - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_source_view_set_show_line_numbers - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_menu_item_new_with_label - dynstr, 0x12, 0, 0, 0, 0
    dd str_g_malloc - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_menu_shell_append - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_file_chooser_dialog_new - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_widget_destroy - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_scrolled_window_set_policy - dynstr, 0x12, 0, 0, 0, 0
    dd str_fseek - dynstr, 0x12, 0, 0, 0, 0
    dd str_fclose - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_dialog_run - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_main_quit - dynstr, 0x12, 0, 0, 0, 0
    dd str___fread_chk - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_menu_new - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_container_add - dynstr, 0x12, 0, 0, 0, 0
    dd str_ftell - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_window_set_default_size - dynstr, 0x12, 0, 0, 0, 0
    dd str_g_printerr - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_scrolled_window_new - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_text_buffer_set_text - dynstr, 0x12, 0, 0, 0, 0
    dd str_g_signal_connect_data - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_source_view_new_with_buffer - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_init - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_menu_bar_new - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_main - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_box_pack_start - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_box_new - dynstr, 0x12, 0, 0, 0, 0
    dd str_exit - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_source_buffer_new_with_language - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_source_language_manager_get_language - dynstr, 0x12, 0, 0, 0, 0
    dd str_fopen - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_menu_item_set_submenu - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_widget_show_all - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_file_chooser_get_filename - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_window_new - dynstr, 0x12, 0, 0, 0, 0

dynstr:
    db 0
str_g_free: db "g_free", 0
str_gtk_source_language_manager_get_default: db "gtk_source_language_manager_get_default", 0
str_gtk_source_view_set_show_line_numbers: db "gtk_source_view_set_show_line_numbers", 0
str_gtk_menu_item_new_with_label: db "gtk_menu_item_new_with_label", 0
str_g_malloc: db "g_malloc", 0
str_gtk_menu_shell_append: db "gtk_menu_shell_append", 0
str_gtk_file_chooser_dialog_new: db "gtk_file_chooser_dialog_new", 0
str_gtk_widget_destroy: db "gtk_widget_destroy", 0
str_gtk_scrolled_window_set_policy: db "gtk_scrolled_window_set_policy", 0
str_fseek: db "fseek", 0
str_fclose: db "fclose", 0
str_gtk_dialog_run: db "gtk_dialog_run", 0
str_gtk_main_quit: db "gtk_main_quit", 0
str___fread_chk: db "__fread_chk", 0
str_gtk_menu_new: db "gtk_menu_new", 0
str_gtk_container_add: db "gtk_container_add", 0
str_ftell: db "ftell", 0
str_gtk_window_set_default_size: db "gtk_window_set_default_size", 0
str_g_printerr: db "g_printerr", 0
str_gtk_scrolled_window_new: db "gtk_scrolled_window_new", 0
str_gtk_text_buffer_set_text: db "gtk_text_buffer_set_text", 0
str_g_signal_connect_data: db "g_signal_connect_data", 0
str_gtk_source_view_new_with_buffer: db "gtk_source_view_new_with_buffer", 0
str_gtk_init: db "gtk_init", 0
str_gtk_menu_bar_new: db "gtk_menu_bar_new", 0
str_gtk_main: db "gtk_main", 0
str_gtk_box_pack_start: db "gtk_box_pack_start", 0
str_gtk_box_new: db "gtk_box_new", 0
str_exit: db "exit", 0
str_gtk_source_buffer_new_with_language: db "gtk_source_buffer_new_with_language", 0
str_gtk_source_language_manager_get_language: db "gtk_source_language_manager_get_language", 0
str_fopen: db "fopen", 0
str_gtk_menu_item_set_submenu: db "gtk_menu_item_set_submenu", 0
str_gtk_widget_show_all: db "gtk_widget_show_all", 0
str_gtk_file_chooser_get_filename: db "gtk_file_chooser_get_filename", 0
str_gtk_window_new: db "gtk_window_new", 0
str_libc:  db "libc.so.6", 0
str_libgtk:  db "libgtk-3.so.0", 0
str_libgobject:  db "libgobject-2.0.so.0", 0
str_libgtksource: db "libgtksourceview-4.so.0", 0
dynstr_end:

rela_text:
    dq (BASE_ADDR + g_free), (1 << 32) | 1, 0
    dq (BASE_ADDR + gtk_source_language_manager_get_default), (2 << 32) | 1, 0
    dq (BASE_ADDR + gtk_source_view_set_show_line_numbers), (3 << 32) | 1, 0
    dq (BASE_ADDR + gtk_menu_item_new_with_label), (4 << 32) | 1, 0
    dq (BASE_ADDR + g_malloc), (5 << 32) | 1, 0
    dq (BASE_ADDR + gtk_menu_shell_append), (6 << 32) | 1, 0
    dq (BASE_ADDR + gtk_file_chooser_dialog_new), (7 << 32) | 1, 0
    dq (BASE_ADDR + gtk_widget_destroy), (8 << 32) | 1, 0
    dq (BASE_ADDR + gtk_scrolled_window_set_policy), (9 << 32) | 1, 0
    dq (BASE_ADDR + fseek), (10 << 32) | 1, 0
    dq (BASE_ADDR + fclose), (11 << 32) | 1, 0
    dq (BASE_ADDR + gtk_dialog_run), (12 << 32) | 1, 0
    dq (BASE_ADDR + gtk_main_quit), (13 << 32) | 1, 0
    dq (BASE_ADDR + __fread_chk), (14 << 32) | 1, 0
    dq (BASE_ADDR + gtk_menu_new), (15 << 32) | 1, 0
    dq (BASE_ADDR + gtk_container_add), (16 << 32) | 1, 0
    dq (BASE_ADDR + ftell), (17 << 32) | 1, 0
    dq (BASE_ADDR + gtk_window_set_default_size), (18 << 32) | 1, 0
    dq (BASE_ADDR + g_printerr), (19 << 32) | 1, 0
    dq (BASE_ADDR + gtk_scrolled_window_new), (20 << 32) | 1, 0
    dq (BASE_ADDR + gtk_text_buffer_set_text), (21 << 32) | 1, 0
    dq (BASE_ADDR + g_signal_connect_data), (22 << 32) | 1, 0
    dq (BASE_ADDR + gtk_source_view_new_with_buffer), (23 << 32) | 1, 0
    dq (BASE_ADDR + gtk_init), (24 << 32) | 1, 0
    dq (BASE_ADDR + gtk_menu_bar_new), (25 << 32) | 1, 0
    dq (BASE_ADDR + gtk_main), (26 << 32) | 1, 0
    dq (BASE_ADDR + gtk_box_pack_start), (27 << 32) | 1, 0
    dq (BASE_ADDR + gtk_box_new), (28 << 32) | 1, 0
    dq (BASE_ADDR + exit), (29 << 32) | 1, 0
    dq (BASE_ADDR + gtk_source_buffer_new_with_language), (30 << 32) | 1, 0
    dq (BASE_ADDR + gtk_source_language_manager_get_language), (31 << 32) | 1, 0
    dq (BASE_ADDR + fopen), (32 << 32) | 1, 0
    dq (BASE_ADDR + gtk_menu_item_set_submenu), (33 << 32) | 1, 0
    dq (BASE_ADDR + gtk_widget_show_all), (34 << 32) | 1, 0
    dq (BASE_ADDR + gtk_file_chooser_get_filename), (35 << 32) | 1, 0
    dq (BASE_ADDR + gtk_window_new), (36 << 32) | 1, 0
rela_text_end:

eof:
    section .bss
bss:
g_free: resq 1
gtk_source_language_manager_get_default: resq 1
gtk_source_view_set_show_line_numbers: resq 1
gtk_menu_item_new_with_label: resq 1
g_malloc: resq 1
gtk_menu_shell_append: resq 1
gtk_file_chooser_dialog_new: resq 1
gtk_widget_destroy: resq 1
gtk_scrolled_window_set_policy: resq 1
fseek: resq 1
fclose: resq 1
gtk_dialog_run: resq 1
gtk_main_quit: resq 1
__fread_chk: resq 1
gtk_menu_new: resq 1
gtk_container_add: resq 1
ftell: resq 1
gtk_window_set_default_size: resq 1
g_printerr: resq 1
gtk_scrolled_window_new: resq 1
gtk_text_buffer_set_text: resq 1
g_signal_connect_data: resq 1
gtk_source_view_new_with_buffer: resq 1
gtk_init: resq 1
gtk_menu_bar_new: resq 1
gtk_main: resq 1
gtk_box_pack_start: resq 1
gtk_box_new: resq 1
exit: resq 1
gtk_source_buffer_new_with_language: resq 1
gtk_source_language_manager_get_language: resq 1
fopen: resq 1
gtk_menu_item_set_submenu: resq 1
gtk_widget_show_all: resq 1
gtk_file_chooser_get_filename: resq 1
gtk_window_new: resq 1
bss_end:
bss_size equ bss_end - bss
