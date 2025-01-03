	.file	"app.c"
	.intel_syntax noprefix
	.text
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC0:
	.string	"_Open"
.LC1:
	.string	"_Cancel"
.LC2:
	.string	"Open File"
.LC3:
	.string	"r"
.LC4:
	.string	"Failed to open file: %s\n"
	.text
	.p2align 4
	.globl	on_open_file
	.type	on_open_file, @function
on_open_file:
.LFB2621:
	.cfi_startproc
	endbr64
	push	r15
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	lea	rcx, .LC1[rip]
	xor	edx, edx
	xor	eax, eax
	push	r14
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	lea	r9, .LC0[rip]
	mov	r8d, -6
	lea	rdi, .LC2[rip]
	push	r13
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	push	r12
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	push	rbp
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	mov	rbp, rsi
	xor	esi, esi
	push	rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	sub	rsp, 24
	.cfi_def_cfa_offset 80
	push	0
	.cfi_def_cfa_offset 88
	push	-3
	.cfi_def_cfa_offset 96
	call	gtk_file_chooser_dialog_new@PLT
	mov	rdi, rax
	mov	rbx, rax
	call	gtk_dialog_run@PLT
	pop	rdx
	.cfi_def_cfa_offset 88
	pop	rcx
	.cfi_def_cfa_offset 80
	cmp	eax, -3
	je	.L7
.L2:
	add	rsp, 24
	.cfi_remember_state
	.cfi_def_cfa_offset 56
	mov	rdi, rbx
	pop	rbx
	.cfi_def_cfa_offset 48
	pop	rbp
	.cfi_def_cfa_offset 40
	pop	r12
	.cfi_def_cfa_offset 32
	pop	r13
	.cfi_def_cfa_offset 24
	pop	r14
	.cfi_def_cfa_offset 16
	pop	r15
	.cfi_def_cfa_offset 8
	jmp	gtk_widget_destroy@PLT
	.p2align 4,,10
	.p2align 3
.L7:
	.cfi_restore_state
	mov	rdi, rbx
	call	gtk_file_chooser_get_filename@PLT
	lea	rsi, .LC3[rip]
	mov	rdi, rax
	mov	r14, rax
	call	fopen@PLT
	mov	r12, rax
	test	rax, rax
	je	.L3
	mov	edx, 2
	xor	esi, esi
	mov	rdi, rax
	call	fseek@PLT
	mov	rdi, r12
	call	ftell@PLT
	xor	edx, edx
	xor	esi, esi
	mov	rdi, r12
	mov	r15, rax
	call	fseek@PLT
	lea	rsi, 1[r15]
	mov	rdi, rsi
	mov	QWORD PTR 8[rsp], rsi
	call	g_malloc@PLT
	mov	rsi, QWORD PTR 8[rsp]
	mov	r8, r12
	mov	rcx, r15
	mov	edx, 1
	mov	rdi, rax
	mov	r13, rax
	call	__fread_chk@PLT
	mov	rdi, r12
	mov	BYTE PTR 0[r13+rax], 0
	call	fclose@PLT
	mov	rdi, rbp
	mov	edx, -1
	mov	rsi, r13
	call	gtk_text_buffer_set_text@PLT
	mov	rdi, r13
	call	g_free@PLT
.L4:
	mov	rdi, r14
	call	g_free@PLT
	jmp	.L2
.L3:
	mov	rsi, r14
	lea	rdi, .LC4[rip]
	xor	eax, eax
	call	g_printerr@PLT
	jmp	.L4
	.cfi_endproc
.LFE2621:
	.size	on_open_file, .-on_open_file
	.section	.rodata.str1.1
.LC5:
	.string	"delete-event"
.LC6:
	.string	"commonlisp"
.LC7:
	.string	"File"
.LC8:
	.string	"Open..."
.LC9:
	.string	"Quit"
.LC10:
	.string	"activate"
	.section	.text.startup,"ax",@progbits
	.p2align 4
	.globl	main
	.type	main, @function
main:
.LFB2622:
	.cfi_startproc
	endbr64
	push	r15
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	push	r14
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	push	r13
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	push	r12
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	push	rbp
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	push	rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	sub	rsp, 40
	.cfi_def_cfa_offset 96
	mov	DWORD PTR 28[rsp], edi
	lea	rdi, 28[rsp]
	mov	QWORD PTR 16[rsp], rsi
	lea	rsi, 16[rsp]
	call	gtk_init@PLT
	xor	edi, edi
	call	gtk_window_new@PLT
	mov	edx, 600
	mov	esi, 800
	mov	rdi, rax
	mov	rbx, rax
	call	gtk_window_set_default_size@PLT
	xor	r9d, r9d
	xor	r8d, r8d
	xor	ecx, ecx
	mov	rdx, QWORD PTR gtk_main_quit@GOTPCREL[rip]
	lea	rsi, .LC5[rip]
	mov	rdi, rbx
	call	g_signal_connect_data@PLT
	xor	esi, esi
	xor	edi, edi
	call	gtk_scrolled_window_new@PLT
	mov	edx, 1
	mov	esi, 1
	mov	rdi, rax
	mov	rbp, rax
	call	gtk_scrolled_window_set_policy@PLT
	call	gtk_source_language_manager_get_default@PLT
	lea	rsi, .LC6[rip]
	mov	rdi, rax
	call	gtk_source_language_manager_get_language@PLT
	mov	rdi, rax
	call	gtk_source_buffer_new_with_language@PLT
	mov	rdi, rax
	mov	QWORD PTR 8[rsp], rax
	call	gtk_source_view_new_with_buffer@PLT
	mov	esi, 1
	mov	r12, rax
	mov	rdi, rax
	call	gtk_source_view_set_show_line_numbers@PLT
	mov	rsi, r12
	mov	rdi, rbp
	call	gtk_container_add@PLT
	call	gtk_menu_bar_new@PLT
	mov	QWORD PTR [rsp], rax
	call	gtk_menu_new@PLT
	lea	rdi, .LC7[rip]
	mov	r12, rax
	call	gtk_menu_item_new_with_label@PLT
	lea	rdi, .LC8[rip]
	mov	r15, rax
	call	gtk_menu_item_new_with_label@PLT
	lea	rdi, .LC9[rip]
	mov	r14, rax
	call	gtk_menu_item_new_with_label@PLT
	mov	rsi, r12
	mov	rdi, r15
	mov	r13, rax
	call	gtk_menu_item_set_submenu@PLT
	mov	rdi, r12
	mov	rsi, r14
	call	gtk_menu_shell_append@PLT
	mov	rdi, r12
	mov	rsi, r13
	lea	r12, .LC10[rip]
	call	gtk_menu_shell_append@PLT
	mov	rsi, r15
	mov	r15, QWORD PTR [rsp]
	mov	rdi, r15
	call	gtk_menu_shell_append@PLT
	mov	rcx, QWORD PTR 8[rsp]
	xor	r9d, r9d
	mov	rsi, r12
	xor	r8d, r8d
	lea	rdx, on_open_file[rip]
	mov	rdi, r14
	call	g_signal_connect_data@PLT
	xor	r9d, r9d
	mov	rsi, r12
	xor	r8d, r8d
	mov	rdx, QWORD PTR gtk_main_quit@GOTPCREL[rip]
	xor	ecx, ecx
	mov	rdi, r13
	call	g_signal_connect_data@PLT
	xor	esi, esi
	mov	edi, 1
	call	gtk_box_new@PLT
	xor	r8d, r8d
	xor	ecx, ecx
	xor	edx, edx
	mov	r12, rax
	mov	rdi, rax
	mov	rsi, r15
	call	gtk_box_pack_start@PLT
	xor	r8d, r8d
	mov	ecx, 1
	mov	rsi, rbp
	mov	edx, 1
	mov	rdi, r12
	call	gtk_box_pack_start@PLT
	mov	rsi, r12
	mov	rdi, rbx
	call	gtk_container_add@PLT
	mov	rdi, rbx
	call	gtk_widget_show_all@PLT
	call	gtk_main@PLT
	xor	edi, edi
	call	exit@PLT
	.cfi_endproc
.LFE2622:
	.size	main, .-main
	.ident	"GCC: (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0"
	.section	.note.GNU-stack,"",@progbits
	.section	.note.gnu.property,"a"
	.align 8
	.long	1f - 0f
	.long	4f - 1f
	.long	5
0:
	.string	"GNU"
1:
	.align 8
	.long	0xc0000002
	.long	3f - 2f
2:
	.long	0x3
3:
	.align 8
4:
