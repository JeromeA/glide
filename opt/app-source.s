	.file	"main.c"
	.intel_syntax noprefix
	.text
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC0:
	.string	"InteractionsView.class_init"
	.text
	.p2align 4
	.type	interactions_view_class_intern_init, @function
interactions_view_class_intern_init:
.LFB2752:
	.cfi_startproc
	endbr64
	push	rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	xor	eax, eax
	mov	rbx, rdi
	call	[QWORD PTR reloc_functions[rip+432]]
	mov	QWORD PTR interactions_view_parent_class[rip], rax
	mov	eax, DWORD PTR InteractionsView_private_offset[rip]
	test	eax, eax
	je	.L2
	lea	rsi, InteractionsView_private_offset[rip]
	mov	rdi, rbx
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+456]]
.L2:
	xor	eax, eax
	lea	rdx, .LC0[rip]
	mov	esi, 128
	xor	edi, edi
	call	g_log@PLT
	lea	rax, interactions_view_finalize[rip]
	mov	QWORD PTR 48[rbx], rax
	pop	rbx
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE2752:
	.size	interactions_view_class_intern_init, .-interactions_view_class_intern_init
	.section	.rodata.str1.1
.LC1:
	.string	"quit_delete_event_handler"
.LC2:
	.string	"app_on_quit_global"
.LC3:
	.string	"app_quit_global"
	.text
	.p2align 4
	.type	quit_delete_event_handler, @function
quit_delete_event_handler:
.LFB2822:
	.cfi_startproc
	endbr64
	sub	rsp, 8
	.cfi_def_cfa_offset 16
	lea	rdx, .LC1[rip]
	xor	edi, edi
	xor	eax, eax
	mov	esi, 128
	call	g_log@PLT
	lea	rdx, .LC2[rip]
	xor	edi, edi
	xor	eax, eax
	mov	esi, 128
	call	g_log@PLT
	lea	rdx, .LC3[rip]
	xor	edi, edi
	xor	eax, eax
	mov	esi, 128
	call	g_log@PLT
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+224]]
	mov	eax, 1
	add	rsp, 8
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE2822:
	.size	quit_delete_event_handler, .-quit_delete_event_handler
	.p2align 4
	.type	interaction_row_free, @function
interaction_row_free:
.LFB2756:
	.cfi_startproc
	endbr64
	push	rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	mov	rbx, rdi
	test	rdi, rdi
	je	.L8
	mov	rdi, QWORD PTR [rdi]
	test	rdi, rdi
	je	.L8
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+368]]
.L8:
	mov	rdi, rbx
	pop	rbx
	.cfi_def_cfa_offset 8
	jmp	g_free@PLT
	.cfi_endproc
.LFE2756:
	.size	interaction_row_free, .-interaction_row_free
	.p2align 4
	.type	static_unescape_string, @function
static_unescape_string:
.LFB2807:
	.cfi_startproc
	push	r12
	.cfi_def_cfa_offset 16
	.cfi_offset 12, -16
	mov	r12, rdi
	push	rbp
	.cfi_def_cfa_offset 24
	.cfi_offset 6, -24
	push	rbx
	.cfi_def_cfa_offset 32
	.cfi_offset 3, -32
	cmp	BYTE PTR [rdi], 34
	je	.L72
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+448]]
	pop	rbx
	.cfi_remember_state
	.cfi_def_cfa_offset 24
	mov	rdi, r12
	pop	rbp
	.cfi_def_cfa_offset 16
	pop	r12
	.cfi_def_cfa_offset 8
	jmp	g_strdup@PLT
	.p2align 4,,10
	.p2align 3
.L72:
	.cfi_restore_state
	xor	edi, edi
	xor	eax, eax
	lea	rbp, 1[r12]
	call	[QWORD PTR reloc_functions[rip+584]]
	movsx	edx, BYTE PTR 1[r12]
	mov	rbx, rax
	xor	eax, eax
	test	dl, dl
	je	.L35
	.p2align 4,,10
	.p2align 3
.L18:
	cmp	dl, 34
	je	.L73
	test	eax, eax
	je	.L19
	cmp	dl, 110
	je	.L20
	jle	.L74
	cmp	dl, 114
	je	.L24
	cmp	dl, 116
	jne	.L71
	test	rbx, rbx
	je	.L28
	mov	rax, QWORD PTR 8[rbx]
	lea	rdx, 1[rax]
	cmp	rdx, QWORD PTR 16[rbx]
	jnb	.L28
	mov	rcx, QWORD PTR [rbx]
	mov	QWORD PTR 8[rbx], rdx
	mov	BYTE PTR [rcx+rax], 9
	mov	rdx, QWORD PTR [rbx]
	mov	rax, QWORD PTR 8[rbx]
	mov	BYTE PTR [rdx+rax], 0
	.p2align 4,,10
	.p2align 3
.L27:
	xor	eax, eax
.L33:
	movsx	edx, BYTE PTR 1[rbp]
	add	rbp, 1
	test	dl, dl
	jne	.L18
.L35:
	mov	rdi, rbx
	xor	eax, eax
	pop	rbx
	.cfi_remember_state
	.cfi_def_cfa_offset 24
	pop	rbp
	.cfi_def_cfa_offset 16
	pop	r12
	.cfi_def_cfa_offset 8
	jmp	[QWORD PTR reloc_functions[rip+616]]
	.p2align 4,,10
	.p2align 3
.L28:
	.cfi_restore_state
	mov	edx, 9
	mov	rsi, -1
	mov	rdi, rbx
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+600]]
	jmp	.L27
	.p2align 4,,10
	.p2align 3
.L73:
	test	eax, eax
	je	.L35
	test	rbx, rbx
	je	.L31
	mov	rax, QWORD PTR 8[rbx]
	lea	rdx, 1[rax]
	cmp	rdx, QWORD PTR 16[rbx]
	jnb	.L31
	mov	rcx, QWORD PTR [rbx]
	mov	QWORD PTR 8[rbx], rdx
	mov	BYTE PTR [rcx+rax], 34
	mov	rdx, QWORD PTR [rbx]
	mov	rax, QWORD PTR 8[rbx]
	mov	BYTE PTR [rdx+rax], 0
	jmp	.L27
	.p2align 4,,10
	.p2align 3
.L31:
	mov	edx, 34
	mov	rsi, -1
	mov	rdi, rbx
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+600]]
	jmp	.L27
	.p2align 4,,10
	.p2align 3
.L19:
	mov	eax, 1
	cmp	dl, 92
	je	.L33
.L71:
	test	rbx, rbx
	je	.L34
	mov	rax, QWORD PTR 8[rbx]
	lea	rcx, 1[rax]
	cmp	rcx, QWORD PTR 16[rbx]
	jnb	.L34
	mov	rsi, QWORD PTR [rbx]
	mov	QWORD PTR 8[rbx], rcx
	mov	BYTE PTR [rsi+rax], dl
	mov	rdx, QWORD PTR [rbx]
	mov	rax, QWORD PTR 8[rbx]
	mov	BYTE PTR [rdx+rax], 0
	jmp	.L27
	.p2align 4,,10
	.p2align 3
.L34:
	mov	rsi, -1
	mov	rdi, rbx
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+600]]
	jmp	.L27
	.p2align 4,,10
	.p2align 3
.L20:
	test	rbx, rbx
	je	.L26
	mov	rax, QWORD PTR 8[rbx]
	lea	rdx, 1[rax]
	cmp	rdx, QWORD PTR 16[rbx]
	jnb	.L26
	mov	rcx, QWORD PTR [rbx]
	mov	QWORD PTR 8[rbx], rdx
	mov	BYTE PTR [rcx+rax], 10
	mov	rdx, QWORD PTR [rbx]
	mov	rax, QWORD PTR 8[rbx]
	mov	BYTE PTR [rdx+rax], 0
	jmp	.L27
	.p2align 4,,10
	.p2align 3
.L26:
	mov	edx, 10
	mov	rsi, -1
	mov	rdi, rbx
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+600]]
	jmp	.L27
	.p2align 4,,10
	.p2align 3
.L24:
	test	rbx, rbx
	je	.L29
	mov	rax, QWORD PTR 8[rbx]
	lea	rdx, 1[rax]
	cmp	rdx, QWORD PTR 16[rbx]
	jnb	.L29
	mov	rcx, QWORD PTR [rbx]
	mov	QWORD PTR 8[rbx], rdx
	mov	BYTE PTR [rcx+rax], 13
	mov	rdx, QWORD PTR [rbx]
	mov	rax, QWORD PTR 8[rbx]
	mov	BYTE PTR [rdx+rax], 0
	jmp	.L27
	.p2align 4,,10
	.p2align 3
.L29:
	mov	edx, 13
	mov	rsi, -1
	mov	rdi, rbx
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+600]]
	jmp	.L27
	.p2align 4,,10
	.p2align 3
.L74:
	cmp	dl, 92
	jne	.L71
	test	rbx, rbx
	je	.L30
	mov	rax, QWORD PTR 8[rbx]
	lea	rdx, 1[rax]
	cmp	rdx, QWORD PTR 16[rbx]
	jnb	.L30
	mov	rcx, QWORD PTR [rbx]
	mov	QWORD PTR 8[rbx], rdx
	mov	BYTE PTR [rcx+rax], 92
	mov	rdx, QWORD PTR [rbx]
	mov	rax, QWORD PTR 8[rbx]
	mov	BYTE PTR [rdx+rax], 0
	jmp	.L27
	.p2align 4,,10
	.p2align 3
.L30:
	mov	edx, 92
	mov	rsi, -1
	mov	rdi, rbx
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+600]]
	jmp	.L27
	.cfi_endproc
.LFE2807:
	.size	static_unescape_string, .-static_unescape_string
	.section	.rodata.str1.1
.LC4:
	.string	"LISP STDERR: %s\n"
	.text
	.p2align 4
	.type	on_lisp_stderr, @function
on_lisp_stderr:
.LFB2796:
	.cfi_startproc
	endbr64
	mov	rsi, QWORD PTR [rdi]
	xor	eax, eax
	lea	rdi, .LC4[rip]
	jmp	g_printerr@PLT
	.cfi_endproc
.LFE2796:
	.size	on_lisp_stderr, .-on_lisp_stderr
	.section	.rodata.str1.1
.LC5:
	.string	"InteractionsView"
	.text
	.p2align 4
	.type	interactions_view_get_type_once, @function
interactions_view_get_type_once:
.LFB2755:
	.cfi_startproc
	push	rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	lea	rdi, .LC5[rip]
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+24]]
	mov	rbx, rax
	call	gtk_box_get_type@PLT
	sub	rsp, 8
	.cfi_def_cfa_offset 24
	mov	edx, 1008
	mov	rsi, rbx
	push	0
	.cfi_def_cfa_offset 32
	mov	rdi, rax
	xor	eax, eax
	lea	rcx, interactions_view_class_intern_init[rip]
	lea	r9, interactions_view_init[rip]
	mov	r8d, 56
	call	[QWORD PTR reloc_functions[rip+440]]
	pop	rdx
	.cfi_def_cfa_offset 24
	pop	rcx
	.cfi_def_cfa_offset 16
	pop	rbx
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE2755:
	.size	interactions_view_get_type_once, .-interactions_view_get_type_once
	.section	.rodata.str1.1
.LC6:
	.string	"InteractionsView.init"
	.section	.rodata.str1.8,"aMS",@progbits,1
	.align 8
.LC7:
	.ascii	".interac"
	.string	"tion-output text { background-color: #f2f2f2; font-family: monospace; } .interaction-error text { background-color: #ffe5e5; font-family: monospace; color: #c00; } .interaction-result text { background-color: #e5ffe5; font-family: monospace; color: #060; }"
	.text
	.p2align 4
	.type	interactions_view_init, @function
interactions_view_init:
.LFB2761:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	lea	rdx, .LC6[rip]
	xor	eax, eax
	mov	esi, 128
	push	rbx
	.cfi_def_cfa_offset 24
	.cfi_offset 3, -24
	mov	rbx, rdi
	xor	edi, edi
	sub	rsp, 8
	.cfi_def_cfa_offset 32
	call	g_log@PLT
	mov	rdi, rbx
	mov	esi, 1
	call	gtk_orientable_set_orientation@PLT
	mov	rdi, rbx
	mov	esi, 1
	call	gtk_widget_set_vexpand@PLT
	call	gtk_css_provider_new@PLT
	xor	ecx, ecx
	mov	rdx, -1
	lea	rsi, .LC7[rip]
	mov	rdi, rax
	mov	rbp, rax
	call	gtk_css_provider_load_from_data@PLT
	mov	rdi, rbx
	call	gtk_widget_get_screen@PLT
	mov	rsi, rbp
	mov	edx, 600
	mov	rdi, rax
	call	gtk_style_context_add_provider_for_screen@PLT
	mov	rdi, rbp
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+648]]
	mov	rsi, QWORD PTR g_direct_equal@GOTPCREL[rip]
	mov	rdi, QWORD PTR g_direct_hash@GOTPCREL[rip]
	xor	edx, edx
	lea	rcx, interaction_row_free[rip]
	call	g_hash_table_new_full@PLT
	mov	QWORD PTR 48[rbx], rax
	add	rsp, 8
	.cfi_def_cfa_offset 24
	pop	rbx
	.cfi_def_cfa_offset 16
	pop	rbp
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE2761:
	.size	interactions_view_init, .-interactions_view_init
	.section	.rodata.str1.1
.LC8:
	.string	"InteractionsView.finalize"
	.text
	.p2align 4
	.type	interactions_view_finalize, @function
interactions_view_finalize:
.LFB2759:
	.cfi_startproc
	endbr64
	push	rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	lea	rdx, .LC8[rip]
	mov	rbx, rdi
	mov	esi, 128
	xor	edi, edi
	xor	eax, eax
	call	g_log@PLT
	mov	rdi, QWORD PTR 48[rbx]
	test	rdi, rdi
	je	.L81
	call	g_hash_table_destroy@PLT
	mov	QWORD PTR 48[rbx], 0
.L81:
	mov	rax, QWORD PTR interactions_view_parent_class[rip]
	mov	rdi, rbx
	pop	rbx
	.cfi_def_cfa_offset 8
	mov	rax, QWORD PTR 48[rax]
	jmp	rax
	.cfi_endproc
.LFE2759:
	.size	interactions_view_finalize, .-interactions_view_finalize
	.p2align 4
	.type	set_text_view, @function
set_text_view:
.LFB2757:
	.cfi_startproc
	push	r12
	.cfi_def_cfa_offset 16
	.cfi_offset 12, -16
	mov	r12, rdi
	push	rbp
	.cfi_def_cfa_offset 24
	.cfi_offset 6, -24
	mov	rbp, rsi
	push	rbx
	.cfi_def_cfa_offset 32
	.cfi_offset 3, -32
	sub	rsp, 16
	.cfi_def_cfa_offset 48
	mov	rdi, QWORD PTR [rsi]
	test	rdx, rdx
	je	.L112
	movzx	eax, BYTE PTR [rdx]
	mov	rbx, rdx
	test	al, al
	jne	.L110
	and	r8d, 1
	jne	.L112
	test	al, al
	jne	.L110
	test	rdi, rdi
	je	.L86
.L93:
	call	gtk_text_view_get_buffer@PLT
	add	rsp, 16
	.cfi_remember_state
	.cfi_def_cfa_offset 32
	mov	rsi, rbx
	mov	edx, -1
	pop	rbx
	.cfi_def_cfa_offset 24
	mov	rdi, rax
	pop	rbp
	.cfi_def_cfa_offset 16
	xor	eax, eax
	pop	r12
	.cfi_def_cfa_offset 8
	jmp	[QWORD PTR reloc_functions[rip+360]]
	.p2align 4,,10
	.p2align 3
.L110:
	.cfi_restore_state
	test	rdi, rdi
	jne	.L93
	mov	QWORD PTR 8[rsp], rcx
	call	gtk_text_view_new@PLT
	xor	esi, esi
	mov	QWORD PTR 0[rbp], rax
	mov	rdi, rax
	call	gtk_text_view_set_editable@PLT
	mov	rdi, QWORD PTR 0[rbp]
	mov	esi, 3
	call	gtk_text_view_set_wrap_mode@PLT
	mov	rcx, QWORD PTR 8[rsp]
	test	rcx, rcx
	je	.L94
	mov	rdi, QWORD PTR 0[rbp]
	call	gtk_widget_get_style_context@PLT
	mov	rsi, QWORD PTR 8[rsp]
	mov	rdi, rax
	call	gtk_style_context_add_class@PLT
.L94:
	mov	rsi, QWORD PTR 0[rbp]
	xor	r8d, r8d
	xor	ecx, ecx
	xor	edx, edx
	mov	rdi, r12
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+104]]
	mov	rdi, QWORD PTR 0[rbp]
	call	gtk_widget_show@PLT
	mov	rdi, QWORD PTR 0[rbp]
	test	rdi, rdi
	jne	.L93
.L86:
	add	rsp, 16
	.cfi_remember_state
	.cfi_def_cfa_offset 32
	pop	rbx
	.cfi_def_cfa_offset 24
	pop	rbp
	.cfi_def_cfa_offset 16
	pop	r12
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L112:
	.cfi_restore_state
	test	rdi, rdi
	je	.L86
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+368]]
	mov	QWORD PTR 0[rbp], 0
	add	rsp, 16
	.cfi_def_cfa_offset 32
	pop	rbx
	.cfi_def_cfa_offset 24
	pop	rbp
	.cfi_def_cfa_offset 16
	pop	r12
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE2757:
	.size	set_text_view, .-set_text_view
	.section	.rodata.str1.8
	.align 8
.LC9:
	.string	"InteractionsView.row_update for expr: %s"
	.section	.rodata.str1.1
.LC10:
	.string	"interaction-output"
.LC11:
	.string	"interaction-error"
.LC12:
	.string	"interaction-result"
	.text
	.p2align 4
	.type	interaction_row_update, @function
interaction_row_update:
.LFB2758:
	.cfi_startproc
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	xor	eax, eax
	mov	rbp, rsi
	lea	rdx, .LC9[rip]
	push	rbx
	.cfi_def_cfa_offset 24
	.cfi_offset 3, -24
	mov	rbx, rdi
	xor	edi, edi
	sub	rsp, 8
	.cfi_def_cfa_offset 32
	mov	rcx, QWORD PTR [rsi]
	mov	esi, 128
	call	g_log@PLT
	mov	rdx, QWORD PTR 0[rbp]
	mov	rdi, QWORD PTR 8[rbx]
	xor	r8d, r8d
	lea	rsi, 16[rbx]
	xor	ecx, ecx
	call	set_text_view
	mov	rdx, QWORD PTR 24[rbp]
	mov	rdi, QWORD PTR 8[rbx]
	lea	rsi, 24[rbx]
	mov	r8d, 1
	lea	rcx, .LC10[rip]
	call	set_text_view
	mov	rdx, QWORD PTR 32[rbp]
	mov	rdi, QWORD PTR 8[rbx]
	lea	rsi, 32[rbx]
	mov	r8d, 1
	lea	rcx, .LC11[rip]
	call	set_text_view
	mov	rdx, QWORD PTR 16[rbp]
	mov	rdi, QWORD PTR 8[rbx]
	lea	rsi, 40[rbx]
	mov	r8d, 1
	lea	rcx, .LC12[rip]
	call	set_text_view
	mov	rsi, QWORD PTR 40[rbx]
	test	rsi, rsi
	je	.L113
	mov	rdi, QWORD PTR 8[rbx]
	add	rsp, 8
	.cfi_remember_state
	.cfi_def_cfa_offset 24
	mov	edx, -1
	pop	rbx
	.cfi_def_cfa_offset 16
	pop	rbp
	.cfi_def_cfa_offset 8
	jmp	gtk_box_reorder_child@PLT
	.p2align 4,,10
	.p2align 3
.L113:
	.cfi_restore_state
	add	rsp, 8
	.cfi_def_cfa_offset 24
	pop	rbx
	.cfi_def_cfa_offset 16
	pop	rbp
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE2758:
	.size	interaction_row_update, .-interaction_row_update
	.section	.rodata.str1.8
	.align 8
.LC13:
	.string	"real_swank_process: read_until_from_lisp_output: waiting for '%s'"
	.align 8
.LC14:
	.string	"real_swank_process: read_until_from_lisp_output: found '%s'. Consumed up to %zu."
	.align 8
.LC15:
	.string	"real_swank_process: read_until_from_lisp_output: pattern not found, waiting on g_swank_out_cond."
	.align 8
.LC16:
	.string	"real_swank_process: read_until_from_lisp_output: woken up."
	.text
	.p2align 4
	.type	read_until_from_lisp_output, @function
read_until_from_lisp_output:
.LFB2794:
	.cfi_startproc
	push	r15
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	mov	rcx, rdi
	lea	rdx, .LC13[rip]
	xor	eax, eax
	push	r14
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	mov	esi, 128
	lea	r14, .LC15[rip]
	push	r13
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	lea	r13, g_swank_out_cond[rip]
	push	r12
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	lea	r12, .LC16[rip]
	push	rbp
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	lea	rbp, g_swank_out_mutex[rip]
	push	rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	mov	rbx, rdi
	xor	edi, edi
	sub	rsp, 8
	.cfi_def_cfa_offset 64
	call	g_log@PLT
	mov	rdi, rbx
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+448]]
	mov	rdi, rbp
	mov	r15, rax
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+680]]
	jmp	.L119
	.p2align 4,,10
	.p2align 3
.L117:
	mov	rdx, r14
	mov	esi, 128
	xor	edi, edi
	xor	eax, eax
	call	g_log@PLT
	mov	rsi, rbp
	mov	rdi, r13
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+720]]
	mov	rdx, r12
	mov	esi, 128
	xor	edi, edi
	xor	eax, eax
	call	g_log@PLT
.L119:
	mov	rax, QWORD PTR g_swank_out_buffer[rip]
	mov	rdi, QWORD PTR g_swank_out_consumed[rip]
	mov	rsi, rbx
	add	rdi, QWORD PTR [rax]
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+624]]
	test	rax, rax
	je	.L117
	mov	rdx, QWORD PTR g_swank_out_buffer[rip]
	xor	edi, edi
	mov	rcx, rbx
	mov	esi, 128
	sub	rax, QWORD PTR [rdx]
	lea	rdx, .LC14[rip]
	lea	r8, [rax+r15]
	xor	eax, eax
	mov	QWORD PTR g_swank_out_consumed[rip], r8
	call	g_log@PLT
	mov	rdi, QWORD PTR g_swank_out_buffer[rip]
	mov	rax, QWORD PTR g_swank_out_consumed[rip]
	cmp	QWORD PTR 8[rdi], rax
	je	.L121
.L118:
	add	rsp, 8
	.cfi_remember_state
	.cfi_def_cfa_offset 56
	mov	rdi, rbp
	xor	eax, eax
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
	jmp	[QWORD PTR reloc_functions[rip+688]]
	.p2align 4,,10
	.p2align 3
.L121:
	.cfi_restore_state
	xor	esi, esi
	call	g_string_set_size@PLT
	mov	QWORD PTR g_swank_out_consumed[rip], 0
	jmp	.L118
	.cfi_endproc
.LFE2794:
	.size	read_until_from_lisp_output, .-read_until_from_lisp_output
	.p2align 4
	.type	static_next_token, @function
static_next_token:
.LFB2806:
	.cfi_startproc
	mov	rcx, rdi
	mov	rdi, QWORD PTR [rdi]
.L127:
	movzx	edx, BYTE PTR [rdi]
	cmp	dl, 13
	jg	.L123
	cmp	dl, 8
	jg	.L124
	test	dl, dl
	je	.L128
.L152:
	mov	rax, rdi
.L142:
	cmp	dl, 13
	jg	.L143
	cmp	dl, 8
	jle	.L145
.L166:
	mov	rsi, rax
	sub	rsi, rdi
.L131:
	mov	QWORD PTR [rcx], rax
	jmp	g_strndup@PLT
	.p2align 4,,10
	.p2align 3
.L123:
	cmp	dl, 32
	je	.L124
	cmp	dl, 40
	jne	.L130
	movzx	edx, BYTE PTR 1[rdi]
	lea	rax, 1[rdi]
	test	dl, dl
	je	.L153
	xor	r8d, r8d
	xor	r9d, r9d
	mov	r10d, 1
	test	r8d, r8d
	jne	.L148
	.p2align 4,,10
	.p2align 3
.L170:
	cmp	dl, 92
	je	.L149
	cmp	dl, 34
	je	.L168
	mov	esi, 1
	test	r9d, r9d
	jne	.L132
	cmp	dl, 40
	je	.L169
	cmp	dl, 41
	jne	.L132
	sub	r10d, 1
	setne	sil
	.p2align 4,,10
	.p2align 3
.L132:
	movzx	edx, BYTE PTR 1[rax]
	add	rax, 1
	test	dl, dl
	je	.L166
	test	sil, sil
	je	.L166
	test	r8d, r8d
	je	.L170
.L148:
	mov	esi, 1
	xor	r8d, r8d
	jmp	.L132
	.p2align 4,,10
	.p2align 3
.L124:
	add	rdi, 1
	jmp	.L127
	.p2align 4,,10
	.p2align 3
.L143:
	cmp	dl, 32
	je	.L166
	sub	edx, 40
	cmp	dl, 1
	jbe	.L166
.L145:
	movzx	edx, BYTE PTR 1[rax]
	add	rax, 1
	test	dl, dl
	jne	.L142
	jmp	.L166
	.p2align 4,,10
	.p2align 3
.L149:
	mov	esi, 1
	mov	r8d, 1
	jmp	.L132
	.p2align 4,,10
	.p2align 3
.L168:
	xor	r9d, 1
	mov	esi, 1
	jmp	.L132
	.p2align 4,,10
	.p2align 3
.L130:
	cmp	dl, 34
	jne	.L152
	movzx	edx, BYTE PTR 1[rdi]
	lea	rax, 1[rdi]
	test	dl, dl
	jne	.L139
	jmp	.L153
	.p2align 4,,10
	.p2align 3
.L171:
	add	rax, 1
	cmp	dl, 34
	je	.L166
.L165:
	movzx	edx, BYTE PTR [rax]
	test	dl, dl
	je	.L166
.L139:
	cmp	dl, 92
	jne	.L171
	cmp	BYTE PTR 1[rax], 0
	lea	rdx, 1[rax]
	je	.L154
	add	rax, 2
	jmp	.L165
	.p2align 4,,10
	.p2align 3
.L128:
	xor	eax, eax
	ret
	.p2align 4,,10
	.p2align 3
.L169:
	add	r10d, 1
	jmp	.L132
	.p2align 4,,10
	.p2align 3
.L153:
	mov	esi, 1
	jmp	.L131
.L154:
	mov	rax, rdx
	jmp	.L166
	.cfi_endproc
.LFE2806:
	.size	static_next_token, .-static_next_token
	.p2align 4
	.type	interaction_free_members_static, @function
interaction_free_members_static:
.LFB2808:
	.cfi_startproc
	endbr64
	test	rdi, rdi
	je	.L172
	push	rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	mov	rbx, rdi
	mov	rdi, QWORD PTR [rdi]
	call	g_free@PLT
	mov	QWORD PTR [rbx], 0
	mov	rdi, QWORD PTR 24[rbx]
	call	g_free@PLT
	mov	QWORD PTR 24[rbx], 0
	mov	rdi, QWORD PTR 16[rbx]
	call	g_free@PLT
	mov	QWORD PTR 16[rbx], 0
	mov	rdi, QWORD PTR 32[rbx]
	call	g_free@PLT
	mov	QWORD PTR 32[rbx], 0
	mov	rdi, rbx
	pop	rbx
	.cfi_restore 3
	.cfi_def_cfa_offset 8
	jmp	g_free@PLT
	.p2align 4,,10
	.p2align 3
.L172:
	ret
	.cfi_endproc
.LFE2808:
	.size	interaction_free_members_static, .-interaction_free_members_static
	.section	.rodata.str1.8
	.align 8
.LC17:
	.string	"real_process_global: child_setup_global"
	.align 8
.LC18:
	.string	"prctl(PR_SET_PDEATHSIG, SIGKILL) failed"
	.section	.text.unlikely,"ax",@progbits
.LCOLDB19:
	.text
.LHOTB19:
	.p2align 4
	.type	child_setup_global, @function
child_setup_global:
.LFB2783:
	.cfi_startproc
	endbr64
	xor	edi, edi
	sub	rsp, 8
	.cfi_def_cfa_offset 16
	mov	esi, 128
	xor	eax, eax
	lea	rdx, .LC17[rip]
	call	g_log@PLT
	call	setsid@PLT
	xor	eax, eax
	mov	esi, 9
	mov	edi, 1
	call	prctl@PLT
	test	eax, eax
	js	.L179
	add	rsp, 8
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
	.section	.text.unlikely
	.cfi_startproc
	.type	child_setup_global.cold, @function
child_setup_global.cold:
.LFSB2783:
.L179:
	.cfi_def_cfa_offset 16
	lea	rdi, .LC18[rip]
	pop	rax
	.cfi_def_cfa_offset 8
	jmp	perror@PLT
	.cfi_endproc
.LFE2783:
	.text
	.size	child_setup_global, .-child_setup_global
	.section	.text.unlikely
	.size	child_setup_global.cold, .-child_setup_global.cold
.LCOLDE19:
	.text
.LHOTE19:
	.section	.rodata.str1.1
.LC20:
	.string	"quit_menu_item_handler"
	.text
	.p2align 4
	.type	quit_menu_item_handler, @function
quit_menu_item_handler:
.LFB2823:
	.cfi_startproc
	endbr64
	sub	rsp, 8
	.cfi_def_cfa_offset 16
	lea	rdx, .LC20[rip]
	xor	edi, edi
	xor	eax, eax
	mov	esi, 128
	call	g_log@PLT
	lea	rdx, .LC2[rip]
	xor	edi, edi
	xor	eax, eax
	mov	esi, 128
	call	g_log@PLT
	xor	eax, eax
	lea	rdx, .LC3[rip]
	xor	edi, edi
	mov	esi, 128
	call	g_log@PLT
	xor	eax, eax
	add	rsp, 8
	.cfi_def_cfa_offset 8
	jmp	[QWORD PTR reloc_functions[rip+224]]
	.cfi_endproc
.LFE2823:
	.size	quit_menu_item_handler, .-quit_menu_item_handler
	.section	.rodata.str1.8
	.align 8
.LC21:
	.string	"real_process_global: stdout_thread_global starting"
	.align 8
.LC22:
	.string	"real_process_global: stdout_thread_global exiting, n=%zd, errno=%d"
	.text
	.p2align 4
	.type	stdout_thread_global, @function
stdout_thread_global:
.LFB2781:
	.cfi_startproc
	endbr64
	push	r13
	.cfi_def_cfa_offset 16
	.cfi_offset 13, -16
	lea	rdx, .LC21[rip]
	mov	esi, 128
	xor	edi, edi
	push	r12
	.cfi_def_cfa_offset 24
	.cfi_offset 12, -24
	push	rbp
	.cfi_def_cfa_offset 32
	.cfi_offset 6, -32
	push	rbx
	.cfi_def_cfa_offset 40
	.cfi_offset 3, -40
	xor	ebx, ebx
	sub	rsp, 280
	.cfi_def_cfa_offset 320
	mov	rax, QWORD PTR fs:40
	mov	QWORD PTR 264[rsp], rax
	xor	eax, eax
	call	g_log@PLT
	mov	edx, DWORD PTR g_real_process_out_fd[rip]
	xor	r8d, r8d
	test	edx, edx
	js	.L185
	mov	r12, rsp
	mov	r13d, 256
	.p2align 4,,10
	.p2align 3
.L184:
	movsx	rax, edx
#APP
# 55 "../common/syscalls.h" 1
	mov rdi, rax
	mov rsi, r12
	mov rdx, r13
	mov rax, $0
	syscall
# 0 "" 2
#NO_APP
	mov	rbx, rax
	test	rax, rax
	js	.L196
	je	.L189
	cmp	QWORD PTR g_real_process_out_cb[rip], 0
	je	.L186
	mov	rsi, rbx
	mov	rdi, r12
	call	g_string_new_len@PLT
	mov	rsi, QWORD PTR g_real_process_out_user_data[rip]
	mov	rbp, rax
	mov	rdi, rax
	call	[QWORD PTR g_real_process_out_cb[rip]]
	mov	esi, 1
	mov	rdi, rbp
	call	g_string_free@PLT
.L186:
	mov	edx, DWORD PTR g_real_process_out_fd[rip]
	test	edx, edx
	jns	.L184
.L189:
	xor	r8d, r8d
.L185:
	xor	edi, edi
	xor	eax, eax
	mov	rcx, rbx
	mov	esi, 128
	lea	rdx, .LC22[rip]
	call	g_log@PLT
	mov	rax, QWORD PTR 264[rsp]
	sub	rax, QWORD PTR fs:40
	jne	.L197
	add	rsp, 280
	.cfi_remember_state
	.cfi_def_cfa_offset 40
	xor	eax, eax
	pop	rbx
	.cfi_def_cfa_offset 32
	pop	rbp
	.cfi_def_cfa_offset 24
	pop	r12
	.cfi_def_cfa_offset 16
	pop	r13
	.cfi_def_cfa_offset 8
	ret
.L196:
	.cfi_restore_state
	call	__errno_location@PLT
	mov	r8d, ebx
	mov	rbx, -1
	neg	r8d
	mov	DWORD PTR [rax], r8d
	jmp	.L185
.L197:
	call	__stack_chk_fail@PLT
	.cfi_endproc
.LFE2781:
	.size	stdout_thread_global, .-stdout_thread_global
	.section	.rodata.str1.8
	.align 8
.LC23:
	.string	"real_process_global: stderr_thread_global starting"
	.align 8
.LC24:
	.string	"real_process_global: stderr_thread_global exiting, n=%zd, errno=%d"
	.text
	.p2align 4
	.type	stderr_thread_global, @function
stderr_thread_global:
.LFB2782:
	.cfi_startproc
	endbr64
	push	r13
	.cfi_def_cfa_offset 16
	.cfi_offset 13, -16
	lea	rdx, .LC23[rip]
	mov	esi, 128
	xor	edi, edi
	push	r12
	.cfi_def_cfa_offset 24
	.cfi_offset 12, -24
	push	rbp
	.cfi_def_cfa_offset 32
	.cfi_offset 6, -32
	push	rbx
	.cfi_def_cfa_offset 40
	.cfi_offset 3, -40
	xor	ebx, ebx
	sub	rsp, 280
	.cfi_def_cfa_offset 320
	mov	rax, QWORD PTR fs:40
	mov	QWORD PTR 264[rsp], rax
	xor	eax, eax
	call	g_log@PLT
	mov	edx, DWORD PTR g_real_process_err_fd[rip]
	xor	r8d, r8d
	test	edx, edx
	js	.L200
	mov	r12, rsp
	mov	r13d, 256
	.p2align 4,,10
	.p2align 3
.L199:
	movsx	rax, edx
#APP
# 55 "../common/syscalls.h" 1
	mov rdi, rax
	mov rsi, r12
	mov rdx, r13
	mov rax, $0
	syscall
# 0 "" 2
#NO_APP
	mov	rbx, rax
	test	rax, rax
	js	.L211
	je	.L204
	cmp	QWORD PTR g_real_process_err_cb[rip], 0
	je	.L201
	mov	rsi, rbx
	mov	rdi, r12
	call	g_string_new_len@PLT
	mov	rsi, QWORD PTR g_real_process_err_user_data[rip]
	mov	rbp, rax
	mov	rdi, rax
	call	[QWORD PTR g_real_process_err_cb[rip]]
	mov	esi, 1
	mov	rdi, rbp
	call	g_string_free@PLT
.L201:
	mov	edx, DWORD PTR g_real_process_err_fd[rip]
	test	edx, edx
	jns	.L199
.L204:
	xor	r8d, r8d
.L200:
	xor	edi, edi
	xor	eax, eax
	mov	rcx, rbx
	mov	esi, 128
	lea	rdx, .LC24[rip]
	call	g_log@PLT
	mov	rax, QWORD PTR 264[rsp]
	sub	rax, QWORD PTR fs:40
	jne	.L212
	add	rsp, 280
	.cfi_remember_state
	.cfi_def_cfa_offset 40
	xor	eax, eax
	pop	rbx
	.cfi_def_cfa_offset 32
	pop	rbp
	.cfi_def_cfa_offset 24
	pop	r12
	.cfi_def_cfa_offset 16
	pop	r13
	.cfi_def_cfa_offset 8
	ret
.L211:
	.cfi_restore_state
	call	__errno_location@PLT
	mov	r8d, ebx
	mov	rbx, -1
	neg	r8d
	mov	DWORD PTR [rax], r8d
	jmp	.L200
.L212:
	call	__stack_chk_fail@PLT
	.cfi_endproc
.LFE2782:
	.size	stderr_thread_global, .-stderr_thread_global
	.section	.rodata.str1.8
	.align 8
.LC25:
	.string	"real_swank_session_on_message_internal: Received raw msg:"
	.section	.rodata.str1.1
.LC26:
	.string	"%s%.40s..."
.LC27:
	.string	"%s%s"
	.text
	.p2align 4
	.type	real_swank_session_on_message_internal, @function
real_swank_session_on_message_internal:
.LFB2811:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	xor	eax, eax
	push	rbx
	.cfi_def_cfa_offset 24
	.cfi_offset 3, -24
	mov	rbx, rdi
	sub	rsp, 8
	.cfi_def_cfa_offset 32
	mov	rbp, QWORD PTR [rdi]
	mov	rdi, rbp
	call	[QWORD PTR reloc_functions[rip+448]]
	mov	r8, rbp
	lea	rcx, .LC25[rip]
	cmp	rax, 40
	jbe	.L214
	lea	rdx, .LC26[rip]
	mov	esi, 128
	xor	edi, edi
	xor	eax, eax
	call	g_log@PLT
.L215:
	mov	edi, 8
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+72]]
	mov	rsi, QWORD PTR 8[rbx]
	mov	rdi, QWORD PTR [rbx]
	mov	rbp, rax
	call	g_string_new_len@PLT
	mov	rdx, rbp
	lea	rsi, real_swank_session_handle_message_on_main_thread[rip]
	xor	edi, edi
	mov	QWORD PTR 0[rbp], rax
	add	rsp, 8
	.cfi_remember_state
	.cfi_def_cfa_offset 24
	pop	rbx
	.cfi_def_cfa_offset 16
	pop	rbp
	.cfi_def_cfa_offset 8
	jmp	g_main_context_invoke@PLT
	.p2align 4,,10
	.p2align 3
.L214:
	.cfi_restore_state
	lea	rdx, .LC27[rip]
	mov	esi, 128
	xor	edi, edi
	xor	eax, eax
	call	g_log@PLT
	jmp	.L215
	.cfi_endproc
.LFE2811:
	.size	real_swank_session_on_message_internal, .-real_swank_session_on_message_internal
	.section	.rodata.str1.8
	.align 8
.LC28:
	.string	"real_swank_process: on_lisp_stdout received:"
	.text
	.p2align 4
	.type	on_lisp_stdout, @function
on_lisp_stdout:
.LFB2795:
	.cfi_startproc
	endbr64
	push	r14
	.cfi_def_cfa_offset 16
	.cfi_offset 14, -16
	xor	eax, eax
	push	r13
	.cfi_def_cfa_offset 24
	.cfi_offset 13, -24
	push	r12
	.cfi_def_cfa_offset 32
	.cfi_offset 12, -32
	push	rbp
	.cfi_def_cfa_offset 40
	.cfi_offset 6, -40
	mov	rbp, rdi
	push	rbx
	.cfi_def_cfa_offset 48
	.cfi_offset 3, -48
	mov	rbx, QWORD PTR [rdi]
	mov	rdi, rbx
	call	[QWORD PTR reloc_functions[rip+448]]
	mov	r8, rbx
	lea	rcx, .LC28[rip]
	cmp	rax, 40
	jbe	.L218
	lea	rdx, .LC26[rip]
	mov	esi, 128
	xor	edi, edi
	xor	eax, eax
	call	g_log@PLT
.L219:
	lea	r12, g_swank_out_mutex[rip]
	xor	eax, eax
	mov	rdi, r12
	call	[QWORD PTR reloc_functions[rip+680]]
	mov	rbx, QWORD PTR 8[rbp]
	mov	r13, QWORD PTR 0[rbp]
	mov	rbp, QWORD PTR g_swank_out_buffer[rip]
	mov	r14, rbx
	test	rbp, rbp
	je	.L232
	test	r13, r13
	je	.L222
	test	rbx, rbx
	js	.L233
	mov	rdi, QWORD PTR 8[rbp]
	lea	rax, [rdi+rbx]
	cmp	rax, QWORD PTR 16[rbp]
	jnb	.L234
.L225:
	add	rdi, QWORD PTR 0[rbp]
	lea	rax, 0[r13+rbx]
	mov	rdx, rbx
	mov	rsi, r13
	cmp	rdi, rax
	jnb	.L226
	lea	rax, [rdi+rbx]
	cmp	rax, r13
	jnb	.L235
.L226:
	call	memcpy@PLT
.L227:
	mov	rax, QWORD PTR 0[rbp]
	add	rbx, QWORD PTR 8[rbp]
	mov	QWORD PTR 8[rbp], rbx
	mov	BYTE PTR [rax+rbx], 0
	jmp	.L221
	.p2align 4,,10
	.p2align 3
.L234:
	mov	rcx, r14
	mov	rdx, r13
	mov	rsi, -1
	mov	rdi, rbp
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+552]]
.L221:
	lea	rdi, g_swank_out_cond[rip]
	call	g_cond_signal@PLT
	pop	rbx
	.cfi_remember_state
	.cfi_def_cfa_offset 40
	mov	rdi, r12
	pop	rbp
	.cfi_def_cfa_offset 32
	xor	eax, eax
	pop	r12
	.cfi_def_cfa_offset 24
	pop	r13
	.cfi_def_cfa_offset 16
	pop	r14
	.cfi_def_cfa_offset 8
	jmp	[QWORD PTR reloc_functions[rip+688]]
	.p2align 4,,10
	.p2align 3
.L233:
	.cfi_restore_state
	mov	rdi, r13
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+448]]
	mov	rdi, QWORD PTR 8[rbp]
	mov	rbx, rax
	lea	rax, [rdi+rbx]
	cmp	rax, QWORD PTR 16[rbp]
	jb	.L225
	jmp	.L234
	.p2align 4,,10
	.p2align 3
.L218:
	lea	rdx, .LC27[rip]
	mov	esi, 128
	xor	edi, edi
	xor	eax, eax
	call	g_log@PLT
	jmp	.L219
	.p2align 4,,10
	.p2align 3
.L235:
	call	memmove@PLT
	jmp	.L227
	.p2align 4,,10
	.p2align 3
.L222:
	test	rbx, rbx
	je	.L221
	mov	rdx, rbx
	xor	esi, esi
	mov	rdi, rbp
	call	g_string_append_len@PLT
	jmp	.L221
	.p2align 4,,10
	.p2align 3
.L232:
	mov	rdx, rbx
	mov	rsi, r13
	xor	edi, edi
	call	g_string_append_len@PLT
	jmp	.L221
	.cfi_endproc
.LFE2795:
	.size	on_lisp_stdout, .-on_lisp_stdout
	.section	.rodata.str1.8
	.align 8
.LC29:
	.string	"real_swank_process: swank_reader_thread_global starting for fd %d"
	.align 8
.LC30:
	.string	"real_swank_process: swank_reader_thread_global received data:"
	.align 8
.LC31:
	.string	"real_swank_process: swank_reader_thread_global: EOF on Swank FD %d"
	.align 8
.LC32:
	.string	"real_swank_process: swank_reader_thread_global read error on fd %d: %s (errno %d)\n"
	.align 8
.LC33:
	.string	"real_swank_process: swank_reader_thread_global exiting for fd %d"
	.text
	.p2align 4
	.type	swank_reader_thread_global, @function
swank_reader_thread_global:
.LFB2793:
	.cfi_startproc
	endbr64
	push	r15
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	lea	rdx, .LC29[rip]
	mov	esi, 128
	xor	edi, edi
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
	sub	rsp, 1064
	.cfi_def_cfa_offset 1120
	mov	ecx, DWORD PTR g_swank_fd[rip]
	mov	rax, QWORD PTR fs:40
	mov	QWORD PTR 1048[rsp], rax
	xor	eax, eax
	call	g_log@PLT
	mov	ecx, DWORD PTR g_swank_fd[rip]
	test	ecx, ecx
	js	.L237
	lea	rbp, 16[rsp]
	mov	r12d, 1024
.L257:
	movsx	rbx, ecx
#APP
# 55 "../common/syscalls.h" 1
	mov rdi, rbx
	mov rsi, rbp
	mov rdx, r12
	mov rax, $0
	syscall
# 0 "" 2
#NO_APP
	mov	rbx, rax
	test	rax, rax
	js	.L266
	jne	.L267
	mov	ecx, DWORD PTR g_swank_fd[rip]
	mov	esi, 128
	xor	edi, edi
	xor	eax, eax
	lea	rdx, .LC31[rip]
	call	g_log@PLT
	mov	ecx, DWORD PTR g_swank_fd[rip]
	.p2align 4,,10
	.p2align 3
.L237:
	xor	edi, edi
	xor	eax, eax
	lea	rdx, .LC33[rip]
	mov	esi, 128
	call	g_log@PLT
	mov	rax, QWORD PTR 1048[rsp]
	sub	rax, QWORD PTR fs:40
	jne	.L268
	add	rsp, 1064
	.cfi_remember_state
	.cfi_def_cfa_offset 56
	xor	eax, eax
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
	ret
.L266:
	.cfi_restore_state
	call	__errno_location@PLT
	neg	ebx
	mov	DWORD PTR [rax], ebx
	cmp	ebx, 11
	je	.L269
	mov	edi, ebx
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+776]]
	mov	esi, DWORD PTR g_swank_fd[rip]
	mov	ecx, ebx
	lea	rdi, .LC32[rip]
	mov	rdx, rax
	xor	eax, eax
	call	g_printerr@PLT
	mov	ecx, DWORD PTR g_swank_fd[rip]
	jmp	.L237
	.p2align 4,,10
	.p2align 3
.L267:
	mov	rsi, rax
	mov	rdi, rbp
	call	g_strndup@PLT
	mov	r13, rax
	mov	rdi, rax
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+448]]
	mov	r8, r13
	lea	rcx, .LC30[rip]
	cmp	rax, 40
	ja	.L270
	lea	rdx, .LC27[rip]
	mov	esi, 128
	xor	edi, edi
	xor	eax, eax
	call	g_log@PLT
.L243:
	mov	rdi, r13
	lea	r14, g_swank_incoming_mutex[rip]
	call	g_free@PLT
	mov	rdi, r14
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+680]]
	mov	r13, QWORD PTR g_swank_incoming_data_buffer[rip]
	test	r13, r13
	je	.L271
	mov	rdi, QWORD PTR 8[r13]
	lea	rax, [rbx+rdi]
	cmp	rax, QWORD PTR 16[r13]
	jb	.L246
	mov	rdi, r13
	mov	rcx, rbx
	mov	rdx, rbp
	mov	rsi, -1
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+552]]
	mov	r13, QWORD PTR g_swank_incoming_data_buffer[rip]
.L245:
	cmp	QWORD PTR g_swank_message_cb[rip], 0
	mov	rdx, QWORD PTR g_swank_incoming_consumed[rip]
	mov	rax, QWORD PTR 8[r13]
	jne	.L272
.L249:
	test	rdx, rdx
	je	.L254
	cmp	rdx, rax
	jb	.L273
.L254:
	cmp	rdx, rax
	je	.L274
.L255:
	mov	QWORD PTR g_swank_incoming_consumed[rip], 0
	mov	rdi, r14
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+688]]
.L256:
	mov	ecx, DWORD PTR g_swank_fd[rip]
	test	ecx, ecx
	jns	.L257
	jmp	.L237
	.p2align 4,,10
	.p2align 3
.L270:
	lea	rdx, .LC26[rip]
	mov	esi, 128
	xor	edi, edi
	xor	eax, eax
	call	g_log@PLT
	jmp	.L243
	.p2align 4,,10
	.p2align 3
.L272:
	mov	rcx, rax
	sub	rcx, rdx
	cmp	rcx, 5
	jbe	.L249
	lea	r15, 9[rsp]
.L253:
	add	rdx, QWORD PTR 0[r13]
	xor	esi, esi
	mov	rdi, r15
	mov	eax, DWORD PTR [rdx]
	mov	DWORD PTR [r15], eax
	movzx	eax, WORD PTR 4[rdx]
	mov	edx, 16
	mov	WORD PTR 4[r15], ax
	mov	BYTE PTR 15[rsp], 0
	call	g_ascii_strtoull@PLT
	mov	r13, QWORD PTR g_swank_incoming_data_buffer[rip]
	mov	rdx, QWORD PTR g_swank_incoming_consumed[rip]
	mov	rbx, rax
	mov	rax, QWORD PTR 8[r13]
	mov	rcx, rax
	sub	rcx, rdx
	sub	rcx, 6
	cmp	rcx, rbx
	jb	.L249
	mov	rax, QWORD PTR 0[r13]
	mov	rsi, rbx
	lea	rdi, 6[rax+rdx]
	call	g_string_new_len@PLT
	mov	rsi, QWORD PTR g_swank_message_cb_data[rip]
	mov	r13, rax
	mov	rax, QWORD PTR g_swank_incoming_consumed[rip]
	mov	rdi, r13
	lea	rax, 6[rbx+rax]
	mov	QWORD PTR g_swank_incoming_consumed[rip], rax
	call	[QWORD PTR g_swank_message_cb[rip]]
	mov	rdi, r13
	mov	esi, 1
	call	g_string_free@PLT
	mov	r13, QWORD PTR g_swank_incoming_data_buffer[rip]
	mov	rdx, QWORD PTR g_swank_incoming_consumed[rip]
	mov	rax, QWORD PTR 8[r13]
	cmp	rax, rdx
	je	.L251
	mov	rcx, rax
	sub	rcx, rdx
.L252:
	cmp	rcx, 5
	ja	.L253
	jmp	.L249
	.p2align 4,,10
	.p2align 3
.L246:
	add	rdi, QWORD PTR 0[r13]
	lea	rax, 0[rbp+rbx]
	mov	rdx, rbx
	mov	rsi, rbp
	cmp	rdi, rax
	jnb	.L247
	lea	rax, [rdi+rbx]
	cmp	rax, rbp
	jnb	.L275
.L247:
	call	memcpy@PLT
.L248:
	mov	rax, QWORD PTR 0[r13]
	add	rbx, QWORD PTR 8[r13]
	mov	QWORD PTR 8[r13], rbx
	mov	BYTE PTR [rax+rbx], 0
	jmp	.L245
	.p2align 4,,10
	.p2align 3
.L269:
	mov	edi, 10000
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+816]]
	jmp	.L256
	.p2align 4,,10
	.p2align 3
.L275:
	call	memmove@PLT
	jmp	.L248
	.p2align 4,,10
	.p2align 3
.L274:
	xor	esi, esi
	mov	rdi, r13
	call	g_string_set_size@PLT
	jmp	.L255
	.p2align 4,,10
	.p2align 3
.L273:
	xor	esi, esi
	mov	rdi, r13
	call	g_string_erase@PLT
	jmp	.L255
	.p2align 4,,10
	.p2align 3
.L271:
	mov	rdx, rbx
	mov	rsi, rbp
	xor	edi, edi
	call	g_string_append_len@PLT
	mov	r13, QWORD PTR g_swank_incoming_data_buffer[rip]
	jmp	.L245
.L251:
	mov	rdi, r13
	xor	esi, esi
	call	g_string_set_size@PLT
	mov	r13, QWORD PTR g_swank_incoming_data_buffer[rip]
	xor	edx, edx
	mov	QWORD PTR g_swank_incoming_consumed[rip], 0
	mov	rax, QWORD PTR 8[r13]
	mov	rcx, rax
	jmp	.L252
.L268:
	call	__stack_chk_fail@PLT
	.cfi_endproc
.LFE2793:
	.size	swank_reader_thread_global, .-swank_reader_thread_global
	.p2align 4
	.globl	interactions_view_get_type
	.type	interactions_view_get_type, @function
interactions_view_get_type:
.LFB2754:
	.cfi_startproc
	endbr64
	mov	rax, QWORD PTR static_g_define_type_id.2[rip]
	test	rax, rax
	je	.L287
	mov	rax, QWORD PTR static_g_define_type_id.2[rip]
	ret
	.p2align 4,,10
	.p2align 3
.L287:
	push	rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	lea	rbx, static_g_define_type_id.2[rip]
	mov	rdi, rbx
	call	g_once_init_enter_pointer@PLT
	test	eax, eax
	jne	.L288
	mov	rax, QWORD PTR static_g_define_type_id.2[rip]
	pop	rbx
	.cfi_remember_state
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L288:
	.cfi_restore_state
	call	interactions_view_get_type_once
	mov	rdi, rbx
	mov	rsi, rax
	call	g_once_init_leave_pointer@PLT
	mov	rax, QWORD PTR static_g_define_type_id.2[rip]
	pop	rbx
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE2754:
	.size	interactions_view_get_type, .-interactions_view_get_type
	.section	.rodata.str1.8
	.align 8
.LC34:
	.string	"InteractionsView.new (no-args)"
	.text
	.p2align 4
	.globl	interactions_view_new
	.type	interactions_view_new, @function
interactions_view_new:
.LFB2762:
	.cfi_startproc
	endbr64
	sub	rsp, 8
	.cfi_def_cfa_offset 16
	mov	esi, 128
	xor	edi, edi
	xor	eax, eax
	lea	rdx, .LC34[rip]
	call	g_log@PLT
	call	interactions_view_get_type
	xor	esi, esi
	add	rsp, 8
	.cfi_def_cfa_offset 8
	mov	rdi, rax
	xor	eax, eax
	jmp	g_object_new@PLT
	.cfi_endproc
.LFE2762:
	.size	interactions_view_new, .-interactions_view_new
	.section	.rodata.str1.1
.LC35:
	.string	"interaction != NULL"
	.section	.rodata.str1.8
	.align 8
.LC36:
	.string	"InteractionsView.add_interaction for expr: %s"
	.align 8
.LC37:
	.string	"GLIDE_IS_INTERACTIONS_VIEW(self)"
	.text
	.p2align 4
	.globl	interactions_view_add_interaction
	.type	interactions_view_add_interaction, @function
interactions_view_add_interaction:
.LFB2763:
	.cfi_startproc
	endbr64
	push	r12
	.cfi_def_cfa_offset 16
	.cfi_offset 12, -16
	mov	r12, rsi
	push	rbp
	.cfi_def_cfa_offset 24
	.cfi_offset 6, -24
	push	rbx
	.cfi_def_cfa_offset 32
	.cfi_offset 3, -32
	mov	rbx, rdi
	call	interactions_view_get_type
	test	rbx, rbx
	je	.L292
	mov	rsi, rax
	mov	rax, QWORD PTR [rbx]
	test	rax, rax
	je	.L293
	cmp	rsi, QWORD PTR [rax]
	je	.L296
.L293:
	xor	eax, eax
	mov	rdi, rbx
	call	[QWORD PTR reloc_functions[rip+672]]
	test	eax, eax
	je	.L292
.L296:
	test	r12, r12
	je	.L306
	mov	rcx, QWORD PTR [r12]
	lea	rdx, .LC36[rip]
	xor	edi, edi
	xor	eax, eax
	mov	esi, 128
	call	g_log@PLT
	mov	edi, 48
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+504]]
	xor	edi, edi
	mov	rbp, rax
	call	gtk_frame_new@PLT
	mov	esi, 5
	mov	QWORD PTR 0[rbp], rax
	mov	rdi, rax
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+392]]
	mov	rdi, QWORD PTR 0[rbp]
	mov	esi, 5
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+384]]
	mov	rdi, QWORD PTR 0[rbp]
	mov	esi, 5
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+400]]
	mov	rdi, QWORD PTR 0[rbp]
	mov	esi, 5
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+376]]
	mov	esi, 2
	mov	edi, 1
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+96]]
	mov	rdi, QWORD PTR 0[rbp]
	mov	QWORD PTR 8[rbp], rax
	mov	rsi, rax
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+136]]
	mov	rsi, r12
	mov	rdi, rbp
	call	interaction_row_update
	mov	rdi, QWORD PTR 48[rbx]
	mov	rdx, rbp
	mov	rsi, r12
	call	g_hash_table_insert@PLT
	mov	rdi, rbx
	mov	rsi, QWORD PTR 0[rbp]
	xor	r8d, r8d
	xor	ecx, ecx
	xor	edx, edx
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+104]]
	pop	rbx
	.cfi_remember_state
	.cfi_def_cfa_offset 24
	mov	rdi, QWORD PTR 0[rbp]
	xor	eax, eax
	pop	rbp
	.cfi_def_cfa_offset 16
	pop	r12
	.cfi_def_cfa_offset 8
	jmp	[QWORD PTR reloc_functions[rip+408]]
	.p2align 4,,10
	.p2align 3
.L306:
	.cfi_restore_state
	lea	rdx, .LC35[rip]
.L305:
	pop	rbx
	.cfi_remember_state
	.cfi_def_cfa_offset 24
	lea	rsi, __func__.1[rip]
	pop	rbp
	.cfi_def_cfa_offset 16
	xor	edi, edi
	xor	eax, eax
	pop	r12
	.cfi_def_cfa_offset 8
	jmp	[QWORD PTR reloc_functions[rip+528]]
	.p2align 4,,10
	.p2align 3
.L292:
	.cfi_restore_state
	lea	rdx, .LC37[rip]
	jmp	.L305
	.cfi_endproc
.LFE2763:
	.size	interactions_view_add_interaction, .-interactions_view_add_interaction
	.section	.rodata.str1.8
	.align 8
.LC38:
	.string	"InteractionsView.update_interaction for expr: %s"
	.align 8
.LC39:
	.string	"InteractionsView.update_interaction: row not found for interaction with expr: %s (tag: %u)"
	.text
	.p2align 4
	.globl	interactions_view_update_interaction
	.type	interactions_view_update_interaction, @function
interactions_view_update_interaction:
.LFB2764:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rsi
	push	rbx
	.cfi_def_cfa_offset 24
	.cfi_offset 3, -24
	mov	rbx, rdi
	sub	rsp, 8
	.cfi_def_cfa_offset 32
	call	interactions_view_get_type
	test	rbx, rbx
	je	.L308
	mov	rsi, rax
	mov	rax, QWORD PTR [rbx]
	test	rax, rax
	je	.L309
	cmp	rsi, QWORD PTR [rax]
	je	.L312
.L309:
	xor	eax, eax
	mov	rdi, rbx
	call	[QWORD PTR reloc_functions[rip+672]]
	test	eax, eax
	je	.L308
.L312:
	test	rbp, rbp
	je	.L323
	mov	rcx, QWORD PTR 0[rbp]
	lea	rdx, .LC38[rip]
	xor	edi, edi
	xor	eax, eax
	mov	esi, 128
	call	g_log@PLT
	mov	rdi, QWORD PTR 48[rbx]
	mov	rsi, rbp
	call	g_hash_table_lookup@PLT
	mov	rbx, rax
	test	rax, rax
	je	.L324
	mov	rdi, rax
	mov	rsi, rbp
	call	interaction_row_update
	mov	rdi, QWORD PTR [rbx]
	add	rsp, 8
	.cfi_remember_state
	.cfi_def_cfa_offset 24
	xor	eax, eax
	pop	rbx
	.cfi_def_cfa_offset 16
	pop	rbp
	.cfi_def_cfa_offset 8
	jmp	[QWORD PTR reloc_functions[rip+408]]
	.p2align 4,,10
	.p2align 3
.L323:
	.cfi_restore_state
	lea	rdx, .LC35[rip]
.L322:
	add	rsp, 8
	.cfi_remember_state
	.cfi_def_cfa_offset 24
	lea	rsi, __func__.0[rip]
	xor	edi, edi
	xor	eax, eax
	pop	rbx
	.cfi_def_cfa_offset 16
	pop	rbp
	.cfi_def_cfa_offset 8
	jmp	[QWORD PTR reloc_functions[rip+528]]
	.p2align 4,,10
	.p2align 3
.L308:
	.cfi_restore_state
	lea	rdx, .LC37[rip]
	jmp	.L322
	.p2align 4,,10
	.p2align 3
.L324:
	mov	rcx, QWORD PTR 0[rbp]
	mov	r8d, DWORD PTR 8[rbp]
	add	rsp, 8
	.cfi_def_cfa_offset 24
	xor	edi, edi
	pop	rbx
	.cfi_def_cfa_offset 16
	lea	rdx, .LC39[rip]
	mov	esi, 16
	xor	eax, eax
	pop	rbp
	.cfi_def_cfa_offset 8
	jmp	g_log@PLT
	.cfi_endproc
.LFE2764:
	.size	interactions_view_update_interaction, .-interactions_view_update_interaction
	.section	.rodata.str1.8
	.align 8
.LC40:
	.string	"real_swank_session_handle_message_on_main_thread: Processing msg:"
	.section	.rodata.str1.1
.LC41:
	.string	"(:return "
.LC42:
	.string	"(:new-features "
	.section	.rodata.str1.8
	.align 8
.LC43:
	.string	"real_swank_session: Could not parse :return message. Payload: %s"
	.align 8
.LC44:
	.string	"real_swank_session: Invalid tag_id in :return message: '%s'"
	.align 8
.LC45:
	.string	"real_swank_session: Received :return for unknown tag_id: %u"
	.section	.rodata.str1.1
.LC46:
	.string	"(:ok "
	.section	.rodata.str1.8
	.align 8
.LC47:
	.string	"Interaction %u OK: output='%s', result='%s'"
	.align 8
.LC48:
	.string	"real_swank_session: Failed to parse specific return type: %s"
	.align 8
.LC49:
	.string	"Failed to parse return from Swank"
	.align 8
.LC50:
	.string	"Interaction %u ABORT: reason='%s'"
	.align 8
.LC51:
	.string	"Interaction %u updated (status: %d)"
	.align 8
.LC52:
	.string	"parse_and_handle_return_message: interactions_view_global is NULL. Cannot update interaction in view."
	.section	.rodata.str1.1
.LC53:
	.string	"(:indentation-update "
.LC54:
	.string	"Received Swank :new-features:"
	.section	.rodata.str1.8
	.align 8
.LC55:
	.string	"Received Swank :indentation-update:"
	.align 8
.LC56:
	.string	"Received unknown Swank message type:"
	.text
	.p2align 4
	.type	real_swank_session_handle_message_on_main_thread, @function
real_swank_session_handle_message_on_main_thread:
.LFB2812:
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
	mov	rbx, rdi
	sub	rsp, 104
	.cfi_def_cfa_offset 160
	mov	rbp, QWORD PTR [rdi]
	mov	rax, QWORD PTR fs:40
	mov	QWORD PTR 88[rsp], rax
	xor	eax, eax
	mov	r12, QWORD PTR 0[rbp]
	mov	rdi, r12
	call	[QWORD PTR reloc_functions[rip+448]]
	mov	r8, r12
	lea	rcx, .LC40[rip]
	cmp	rax, 40
	jbe	.L326
	lea	rdx, .LC26[rip]
	mov	esi, 128
	xor	edi, edi
	xor	eax, eax
	call	g_log@PLT
	mov	r14, QWORD PTR 0[rbp]
	test	r14, r14
	je	.L390
.L328:
	xor	eax, eax
	mov	rdi, r14
	call	[QWORD PTR reloc_functions[rip+448]]
	cmp	rax, 8
	jbe	.L331
	movabs	rax, 7958552634295728680
	cmp	QWORD PTR [r14], rax
	je	.L391
.L331:
	xor	eax, eax
	mov	rdi, r14
	call	[QWORD PTR reloc_functions[rip+448]]
	cmp	rax, 14
	jbe	.L357
	movabs	rax, 7306577436281289256
	cmp	QWORD PTR [r14], rax
	je	.L392
.L357:
	xor	eax, eax
	mov	rdi, r14
	call	[QWORD PTR reloc_functions[rip+448]]
	cmp	rax, 20
	jbe	.L360
	movabs	rax, 8389754637861337640
	xor	rax, QWORD PTR [r14]
	movabs	rdx, 8103433056861910113
	xor	rdx, QWORD PTR 8[r14]
	or	rax, rdx
	je	.L393
.L360:
	mov	rdi, r14
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+448]]
	mov	r8, r14
	lea	rcx, .LC56[rip]
	cmp	rax, 40
	ja	.L387
.L367:
	lea	rdx, .LC27[rip]
	mov	esi, 128
	xor	edi, edi
	xor	eax, eax
	call	g_log@PLT
.L356:
	mov	rdi, rbp
	mov	esi, 1
	call	g_string_free@PLT
	mov	rdi, rbx
	call	g_free@PLT
	mov	rax, QWORD PTR 88[rsp]
	sub	rax, QWORD PTR fs:40
	jne	.L394
	add	rsp, 104
	.cfi_remember_state
	.cfi_def_cfa_offset 56
	xor	eax, eax
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
	ret
	.p2align 4,,10
	.p2align 3
.L326:
	.cfi_restore_state
	lea	rdx, .LC27[rip]
	mov	esi, 128
	xor	edi, edi
	xor	eax, eax
	call	g_log@PLT
	mov	r14, QWORD PTR 0[rbp]
	test	r14, r14
	jne	.L328
.L390:
	xor	edi, edi
	lea	rsi, .LC41[rip]
	call	g_str_has_prefix@PLT
	test	eax, eax
	jne	.L329
	xor	edi, edi
	lea	rsi, .LC42[rip]
	call	g_str_has_prefix@PLT
	test	eax, eax
	jne	.L334
	xor	edi, edi
	lea	rsi, .LC53[rip]
	call	g_str_has_prefix@PLT
	test	eax, eax
	je	.L360
.L366:
	xor	eax, eax
	mov	rdi, r14
	call	[QWORD PTR reloc_functions[rip+448]]
	cmp	rax, 40
	jbe	.L395
	mov	r8, r14
	lea	rcx, .LC55[rip]
	.p2align 4,,10
	.p2align 3
.L387:
	lea	rdx, .LC26[rip]
	mov	esi, 128
	xor	edi, edi
	xor	eax, eax
	call	g_log@PLT
	jmp	.L356
	.p2align 4,,10
	.p2align 3
.L391:
	cmp	BYTE PTR 8[r14], 32
	jne	.L331
.L329:
	lea	r12, 56[rsp]
	lea	rax, 9[r14]
	mov	rdi, r12
	mov	QWORD PTR 56[rsp], rax
	call	static_next_token
	mov	rdi, r12
	mov	r13, rax
	call	static_next_token
	mov	r12, rax
	test	r13, r13
	je	.L369
	test	rax, rax
	je	.L369
	lea	rsi, 64[rsp]
	mov	edx, 10
	mov	rdi, rax
	mov	QWORD PTR 64[rsp], 0
	call	g_ascii_strtoull@PLT
	mov	rsi, rax
	mov	rax, QWORD PTR 64[rsp]
	cmp	BYTE PTR [rax], 0
	jne	.L339
	lea	rax, -1[rsi]
	mov	edx, 4294967294
	cmp	rdx, rax
	jb	.L339
	mov	rdi, QWORD PTR g_swank_session_interactions_table[rip]
	mov	r15d, esi
	call	g_hash_table_lookup@PLT
	mov	r14, rax
	test	rax, rax
	je	.L396
	xor	eax, eax
	mov	rdi, r13
	call	[QWORD PTR reloc_functions[rip+448]]
	cmp	rax, 4
	jbe	.L343
	cmp	DWORD PTR 0[r13], 1802451496
	je	.L397
.L343:
	xor	eax, eax
	mov	rdi, r13
	call	[QWORD PTR reloc_functions[rip+448]]
	cmp	rax, 7
	jbe	.L353
	movabs	rax, 2338619929229605416
	cmp	QWORD PTR 0[r13], rax
	jne	.L353
	lea	rax, 8[r13]
	lea	rdi, 80[rsp]
	mov	QWORD PTR 80[rsp], rax
	call	static_next_token
	mov	rdi, rax
	test	rax, rax
	je	.L353
	mov	QWORD PTR 16[rsp], rax
	call	static_unescape_string
	mov	rdi, QWORD PTR 16[rsp]
	mov	QWORD PTR 8[rsp], rax
	call	g_free@PLT
	mov	r8, QWORD PTR 8[rsp]
	xor	eax, eax
	mov	ecx, r15d
	lea	rdx, .LC50[rip]
	mov	esi, 128
	xor	edi, edi
	call	g_log@PLT
	mov	rdi, QWORD PTR 32[r14]
	call	g_free@PLT
	mov	rax, QWORD PTR 8[rsp]
	mov	DWORD PTR 12[r14], 3
	mov	r8d, 3
	mov	QWORD PTR 32[r14], rax
.L351:
	xor	edi, edi
	mov	ecx, r15d
	lea	rdx, .LC51[rip]
	xor	eax, eax
	mov	esi, 128
	call	g_log@PLT
	mov	rdi, QWORD PTR interactions_view_global[rip]
	test	rdi, rdi
	je	.L354
	mov	rsi, r14
	call	interactions_view_update_interaction
	jmp	.L389
	.p2align 4,,10
	.p2align 3
.L392:
	movabs	rax, 2338324173806657893
	cmp	QWORD PTR 7[r14], rax
	jne	.L357
.L334:
	mov	rdi, r14
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+448]]
	mov	r8, r14
	lea	rcx, .LC54[rip]
	cmp	rax, 40
	jbe	.L367
	jmp	.L387
	.p2align 4,,10
	.p2align 3
.L393:
	movabs	rax, 2334399943507211565
	cmp	QWORD PTR 13[r14], rax
	jne	.L360
	jmp	.L366
	.p2align 4,,10
	.p2align 3
.L339:
	mov	rcx, r12
	lea	rdx, .LC44[rip]
.L388:
	mov	esi, 16
	xor	edi, edi
	xor	eax, eax
	call	g_log@PLT
.L389:
	mov	rdi, r13
	call	g_free@PLT
	mov	rdi, r12
	call	g_free@PLT
	jmp	.L356
	.p2align 4,,10
	.p2align 3
.L395:
	mov	r8, r14
	lea	rcx, .LC55[rip]
	jmp	.L367
	.p2align 4,,10
	.p2align 3
.L353:
	mov	rcx, r13
	lea	rdx, .LC48[rip]
	xor	eax, eax
	xor	edi, edi
	mov	esi, 16
	call	g_log@PLT
	mov	DWORD PTR 12[r14], 3
	mov	rdi, QWORD PTR 32[r14]
	call	g_free@PLT
	lea	rdi, .LC49[rip]
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+448]]
	lea	rdi, .LC49[rip]
	call	g_strdup@PLT
	mov	r8d, DWORD PTR 12[r14]
	mov	QWORD PTR 32[r14], rax
	jmp	.L351
	.p2align 4,,10
	.p2align 3
.L354:
	lea	rdx, .LC52[rip]
	mov	esi, 16
	xor	edi, edi
	xor	eax, eax
	call	g_log@PLT
	jmp	.L389
	.p2align 4,,10
	.p2align 3
.L397:
	cmp	BYTE PTR 4[r13], 32
	jne	.L343
	lea	rax, 5[r13]
	lea	rdi, 72[rsp]
	mov	QWORD PTR 72[rsp], rax
	call	static_next_token
	mov	QWORD PTR 24[rsp], rax
	test	rax, rax
	je	.L343
	cmp	BYTE PTR [rax], 40
	jne	.L348
	xor	eax, eax
	mov	rdi, QWORD PTR 24[rsp]
	call	[QWORD PTR reloc_functions[rip+448]]
	mov	rsi, QWORD PTR 24[rsp]
	cmp	BYTE PTR -1[rsi+rax], 41
	je	.L398
.L348:
	mov	rdi, QWORD PTR 24[rsp]
	call	g_free@PLT
	jmp	.L343
	.p2align 4,,10
	.p2align 3
.L396:
	mov	ecx, r15d
	lea	rdx, .LC45[rip]
	xor	edi, edi
	xor	eax, eax
	mov	esi, 16
	call	g_log@PLT
	mov	rdi, r13
	call	g_free@PLT
	mov	rdi, r12
	call	g_free@PLT
	jmp	.L356
.L398:
	mov	rax, rsi
	lea	rdi, 80[rsp]
	add	rax, 1
	mov	QWORD PTR 8[rsp], rdi
	mov	QWORD PTR 80[rsp], rax
	call	static_next_token
	mov	rdi, QWORD PTR 8[rsp]
	mov	QWORD PTR 32[rsp], rax
	call	static_next_token
	cmp	QWORD PTR 32[rsp], 0
	mov	QWORD PTR 40[rsp], rax
	je	.L370
	test	rax, rax
	je	.L370
	mov	rdi, QWORD PTR 32[rsp]
	call	static_unescape_string
	mov	rdi, QWORD PTR 40[rsp]
	mov	QWORD PTR 8[rsp], rax
	call	static_unescape_string
	mov	rdi, QWORD PTR 24[rsp]
	mov	QWORD PTR 16[rsp], rax
	call	g_free@PLT
	mov	rdi, QWORD PTR 32[rsp]
	call	g_free@PLT
	mov	rdi, QWORD PTR 40[rsp]
	call	g_free@PLT
	mov	r8, QWORD PTR 8[rsp]
	mov	ecx, r15d
	xor	eax, eax
	mov	r9, QWORD PTR 16[rsp]
	lea	rdx, .LC47[rip]
	mov	esi, 128
	xor	edi, edi
	call	g_log@PLT
	mov	rdi, QWORD PTR 24[r14]
	call	g_free@PLT
	mov	rax, QWORD PTR 8[rsp]
	mov	rdi, QWORD PTR 16[r14]
	mov	QWORD PTR 24[r14], rax
	call	g_free@PLT
	mov	rax, QWORD PTR 16[rsp]
	mov	DWORD PTR 12[r14], 2
	mov	r8d, 2
	mov	QWORD PTR 16[r14], rax
	jmp	.L351
.L394:
	call	__stack_chk_fail@PLT
.L369:
	mov	rcx, r14
	lea	rdx, .LC43[rip]
	jmp	.L388
.L370:
	mov	rdi, QWORD PTR 24[rsp]
	call	g_free@PLT
	mov	rdi, QWORD PTR 32[rsp]
	call	g_free@PLT
	mov	rdi, QWORD PTR 40[rsp]
	call	g_free@PLT
	jmp	.L343
	.cfi_endproc
.LFE2812:
	.size	real_swank_session_handle_message_on_main_thread, .-real_swank_session_handle_message_on_main_thread
	.section	.rodata.str1.1
.LC57:
	.string	"(null)"
	.section	.rodata.str1.8
	.align 8
.LC58:
	.string	"preferences_load_globals_internal: No preferences file found at '%s' or filename is NULL. Using defaults."
	.align 8
.LC59:
	.string	"preferences_load_globals_internal: Loading from '%s'"
	.section	.rodata.str1.1
.LC60:
	.string	"sdk"
.LC61:
	.string	"General"
	.section	.rodata.str1.8
	.align 8
.LC62:
	.string	"preferences_load_globals_internal: Loaded sdk='%s'"
	.align 8
.LC63:
	.string	"preferences_load_globals_internal: 'sdk' key not found or empty."
	.section	.rodata.str1.1
.LC64:
	.string	"swank_port"
	.section	.rodata.str1.8
	.align 8
.LC65:
	.string	"preferences_load_globals_internal: Loaded swank_port=%u"
	.align 8
.LC66:
	.string	"preferences_load_globals_internal: 'swank_port' key not found or invalid: %s. Using default %u."
	.align 8
.LC67:
	.string	"preferences_load_globals_internal: Failed to load preferences file '%s': %s. Using defaults."
	.align 8
.LC68:
	.string	"preferences_init_globals: loaded filename='%s', sdk='%s', port=%u"
	.align 8
.LC69:
	.string	"preferences_init_globals: config_dir=%s"
	.section	.rodata.str1.1
.LC70:
	.string	"preferences.ini"
.LC71:
	.string	"glide"
	.section	.rodata.str1.8
	.align 8
.LC72:
	.string	"preferences_init_globals: config_dir is NULL. Preferences might not load/save correctly."
	.section	.rodata.str1.1
.LC73:
	.string	"glide_preferences.ini"
	.text
	.p2align 4
	.globl	preferences_init_globals
	.type	preferences_init_globals, @function
preferences_init_globals:
.LFB2767:
	.cfi_startproc
	endbr64
	push	r12
	.cfi_def_cfa_offset 16
	.cfi_offset 12, -16
	push	rbp
	.cfi_def_cfa_offset 24
	.cfi_offset 6, -24
	push	rbx
	.cfi_def_cfa_offset 32
	.cfi_offset 3, -32
	sub	rsp, 32
	.cfi_def_cfa_offset 64
	mov	rax, QWORD PTR fs:40
	mov	QWORD PTR 24[rsp], rax
	xor	eax, eax
	test	rdi, rdi
	je	.L423
	mov	rcx, rdi
	lea	rdx, .LC69[rip]
	xor	eax, eax
	mov	rbx, rdi
	mov	esi, 128
	xor	edi, edi
	call	g_log@PLT
	mov	rdi, QWORD PTR g_pref_filename[rip]
	call	g_free@PLT
	xor	ecx, ecx
	lea	rdx, .LC70[rip]
	xor	eax, eax
	lea	rsi, .LC71[rip]
	mov	rdi, rbx
	call	g_build_filename@PLT
.L416:
	mov	rdi, QWORD PTR g_pref_sdk[rip]
	mov	QWORD PTR g_pref_filename[rip], rax
	call	g_free@PLT
	mov	rdi, QWORD PTR g_pref_filename[rip]
	mov	eax, 4005
	mov	QWORD PTR g_pref_sdk[rip], 0
	mov	WORD PTR g_pref_swank_port[rip], ax
	test	rdi, rdi
	je	.L405
	mov	esi, 16
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+640]]
	mov	rcx, QWORD PTR g_pref_filename[rip]
	test	eax, eax
	jne	.L404
	test	rcx, rcx
	je	.L405
.L403:
	lea	rdx, .LC58[rip]
	mov	esi, 128
	xor	edi, edi
	xor	eax, eax
	call	g_log@PLT
.L406:
	mov	r8, QWORD PTR g_pref_sdk[rip]
	mov	rcx, QWORD PTR g_pref_filename[rip]
	lea	rax, .LC57[rip]
	movzx	r9d, WORD PTR g_pref_swank_port[rip]
	test	r8, r8
	cmove	r8, rax
	test	rcx, rcx
	cmove	rcx, rax
	mov	rax, QWORD PTR 24[rsp]
	sub	rax, QWORD PTR fs:40
	jne	.L424
	add	rsp, 32
	.cfi_remember_state
	.cfi_def_cfa_offset 32
	lea	rdx, .LC68[rip]
	xor	edi, edi
	xor	eax, eax
	pop	rbx
	.cfi_def_cfa_offset 24
	mov	esi, 128
	pop	rbp
	.cfi_def_cfa_offset 16
	pop	r12
	.cfi_def_cfa_offset 8
	jmp	g_log@PLT
	.p2align 4,,10
	.p2align 3
.L404:
	.cfi_restore_state
	xor	edi, edi
	lea	rdx, .LC59[rip]
	mov	esi, 128
	xor	eax, eax
	call	g_log@PLT
	lea	rbp, 8[rsp]
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+56]]
	xor	edx, edx
	mov	rsi, QWORD PTR g_pref_filename[rip]
	mov	rcx, rbp
	mov	QWORD PTR 8[rsp], 0
	mov	rbx, rax
	mov	rdi, rax
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+48]]
	test	eax, eax
	je	.L407
	lea	r12, .LC61[rip]
	xor	ecx, ecx
	mov	rdi, rbx
	xor	eax, eax
	lea	rdx, .LC60[rip]
	mov	rsi, r12
	call	[QWORD PTR reloc_functions[rip+40]]
	mov	rbp, rax
	test	rax, rax
	jne	.L425
	lea	rdx, .LC63[rip]
	mov	esi, 128
	xor	edi, edi
	xor	eax, eax
	call	g_log@PLT
.L409:
	lea	rbp, 16[rsp]
	mov	rsi, r12
	mov	rdi, rbx
	xor	eax, eax
	lea	rdx, .LC64[rip]
	mov	rcx, rbp
	mov	QWORD PTR 16[rsp], 0
	call	[QWORD PTR reloc_functions[rip+744]]
	mov	rdx, QWORD PTR 16[rsp]
	test	rdx, rdx
	je	.L426
	mov	rcx, QWORD PTR 8[rdx]
	movzx	r8d, WORD PTR g_pref_swank_port[rip]
	xor	edi, edi
	xor	eax, eax
	lea	rdx, .LC66[rip]
	mov	esi, 128
	call	g_log@PLT
	mov	rdi, rbp
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+8]]
.L412:
	mov	rdi, rbx
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+32]]
	jmp	.L406
	.p2align 4,,10
	.p2align 3
.L423:
	lea	rcx, .LC57[rip]
	xor	eax, eax
	mov	esi, 128
	xor	edi, edi
	lea	rdx, .LC69[rip]
	lea	rbx, .LC73[rip]
	call	g_log@PLT
	mov	rdi, QWORD PTR g_pref_filename[rip]
	call	g_free@PLT
	lea	rdx, .LC72[rip]
	xor	edi, edi
	xor	eax, eax
	mov	esi, 16
	call	g_log@PLT
	mov	rdi, rbx
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+448]]
	mov	rdi, rbx
	call	g_strdup@PLT
	jmp	.L416
	.p2align 4,,10
	.p2align 3
.L425:
	mov	rdi, QWORD PTR g_pref_sdk[rip]
	call	g_free@PLT
	mov	rcx, rbp
	mov	esi, 128
	xor	edi, edi
	lea	rdx, .LC62[rip]
	xor	eax, eax
	mov	QWORD PTR g_pref_sdk[rip], rbp
	call	g_log@PLT
	jmp	.L409
	.p2align 4,,10
	.p2align 3
.L405:
	lea	rcx, .LC57[rip]
	jmp	.L403
	.p2align 4,,10
	.p2align 3
.L407:
	mov	rax, QWORD PTR 8[rsp]
	mov	rcx, QWORD PTR g_pref_filename[rip]
	xor	edi, edi
	lea	rdx, .LC67[rip]
	mov	esi, 16
	mov	r8, QWORD PTR 8[rax]
	xor	eax, eax
	call	g_log@PLT
	mov	rdi, rbp
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+8]]
	jmp	.L412
	.p2align 4,,10
	.p2align 3
.L426:
	movzx	ecx, ax
	mov	WORD PTR g_pref_swank_port[rip], ax
	lea	rdx, .LC65[rip]
	xor	edi, edi
	mov	esi, 128
	xor	eax, eax
	call	g_log@PLT
	jmp	.L412
.L424:
	call	__stack_chk_fail@PLT
	.cfi_endproc
.LFE2767:
	.size	preferences_init_globals, .-preferences_init_globals
	.section	.rodata.str1.8
	.align 8
.LC74:
	.string	"preferences_save_globals: g_pref_filename is NULL. Cannot save."
	.align 8
.LC75:
	.string	"preferences_save_globals: Saving to '%s'"
	.align 8
.LC76:
	.string	"Failed to create config directory '%s': %s\n"
	.align 8
.LC77:
	.string	"Failed to save preferences to '%s': %s\n"
	.align 8
.LC78:
	.string	"preferences_save_globals: Successfully saved to '%s'"
	.text
	.p2align 4
	.globl	preferences_save_globals
	.type	preferences_save_globals, @function
preferences_save_globals:
.LFB2769:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	push	rbx
	.cfi_def_cfa_offset 24
	.cfi_offset 3, -24
	sub	rsp, 24
	.cfi_def_cfa_offset 48
	mov	rcx, QWORD PTR g_pref_filename[rip]
	mov	rax, QWORD PTR fs:40
	mov	QWORD PTR 8[rsp], rax
	xor	eax, eax
	test	rcx, rcx
	je	.L445
	lea	rdx, .LC75[rip]
	mov	esi, 128
	xor	edi, edi
	xor	eax, eax
	call	g_log@PLT
	mov	rdi, QWORD PTR g_pref_filename[rip]
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+760]]
	mov	rbx, rax
	test	rax, rax
	jne	.L446
.L431:
	mov	rdi, rbx
	lea	rbp, .LC61[rip]
	call	g_free@PLT
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+56]]
	mov	rcx, QWORD PTR g_pref_sdk[rip]
	mov	QWORD PTR [rsp], 0
	mov	rbx, rax
	test	rcx, rcx
	je	.L433
	mov	rdi, rax
	lea	rdx, .LC60[rip]
	mov	rsi, rbp
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+64]]
.L433:
	mov	rsi, rbp
	mov	rbp, rsp
	lea	rdx, .LC64[rip]
	mov	rdi, rbx
	xor	eax, eax
	movzx	ecx, WORD PTR g_pref_swank_port[rip]
	call	[QWORD PTR reloc_functions[rip+808]]
	xor	eax, eax
	mov	rdx, rbp
	mov	rdi, rbx
	mov	rsi, QWORD PTR g_pref_filename[rip]
	call	[QWORD PTR reloc_functions[rip+464]]
	test	eax, eax
	je	.L447
	mov	rcx, QWORD PTR g_pref_filename[rip]
	mov	esi, 128
	xor	edi, edi
	xor	eax, eax
	lea	rdx, .LC78[rip]
	call	g_log@PLT
.L435:
	xor	eax, eax
	mov	rdi, rbx
	call	[QWORD PTR reloc_functions[rip+32]]
	mov	rax, QWORD PTR 8[rsp]
	sub	rax, QWORD PTR fs:40
	jne	.L444
	add	rsp, 24
	.cfi_remember_state
	.cfi_def_cfa_offset 24
	pop	rbx
	.cfi_def_cfa_offset 16
	pop	rbp
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L447:
	.cfi_restore_state
	mov	rax, QWORD PTR [rsp]
	mov	rsi, QWORD PTR g_pref_filename[rip]
	lea	rdi, .LC77[rip]
	mov	rdx, QWORD PTR 8[rax]
	xor	eax, eax
	call	g_printerr@PLT
	mov	rdi, rbp
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+8]]
	jmp	.L435
	.p2align 4,,10
	.p2align 3
.L446:
	mov	rdi, rax
	mov	esi, 448
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+792]]
	test	eax, eax
	je	.L431
	call	__errno_location@PLT
	mov	edi, DWORD PTR [rax]
	cmp	edi, 17
	je	.L431
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+776]]
	mov	rsi, rbx
	lea	rdi, .LC76[rip]
	mov	rdx, rax
	xor	eax, eax
	call	g_printerr@PLT
	jmp	.L431
	.p2align 4,,10
	.p2align 3
.L445:
	mov	rax, QWORD PTR 8[rsp]
	sub	rax, QWORD PTR fs:40
	jne	.L444
	add	rsp, 24
	.cfi_remember_state
	.cfi_def_cfa_offset 24
	lea	rdx, .LC74[rip]
	xor	edi, edi
	xor	eax, eax
	pop	rbx
	.cfi_def_cfa_offset 16
	mov	esi, 16
	pop	rbp
	.cfi_def_cfa_offset 8
	jmp	g_log@PLT
.L444:
	.cfi_restore_state
	call	__stack_chk_fail@PLT
	.cfi_endproc
.LFE2769:
	.size	preferences_save_globals, .-preferences_save_globals
	.p2align 4
	.globl	preferences_get_sdk_global
	.type	preferences_get_sdk_global, @function
preferences_get_sdk_global:
.LFB2770:
	.cfi_startproc
	endbr64
	mov	rax, QWORD PTR g_pref_sdk[rip]
	ret
	.cfi_endproc
.LFE2770:
	.size	preferences_get_sdk_global, .-preferences_get_sdk_global
	.section	.rodata.str1.8
	.align 8
.LC79:
	.string	"preferences_set_sdk_global: new_sdk='%s'"
	.text
	.p2align 4
	.globl	preferences_set_sdk_global
	.type	preferences_set_sdk_global, @function
preferences_set_sdk_global:
.LFB2771:
	.cfi_startproc
	endbr64
	test	rdi, rdi
	push	rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	lea	rcx, .LC57[rip]
	mov	rbx, rdi
	cmovne	rcx, rdi
	mov	esi, 128
	xor	edi, edi
	xor	eax, eax
	lea	rdx, .LC79[rip]
	call	g_log@PLT
	xor	eax, eax
	mov	rdi, QWORD PTR g_pref_sdk[rip]
	mov	rsi, rbx
	call	[QWORD PTR reloc_functions[rip+88]]
	test	eax, eax
	jne	.L454
	pop	rbx
	.cfi_remember_state
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L454:
	.cfi_restore_state
	mov	rdi, QWORD PTR g_pref_sdk[rip]
	call	g_free@PLT
	mov	rdi, rbx
	call	g_strdup@PLT
	pop	rbx
	.cfi_def_cfa_offset 8
	mov	QWORD PTR g_pref_sdk[rip], rax
	xor	eax, eax
	jmp	preferences_save_globals
	.cfi_endproc
.LFE2771:
	.size	preferences_set_sdk_global, .-preferences_set_sdk_global
	.p2align 4
	.globl	preferences_get_swank_port_global
	.type	preferences_get_swank_port_global, @function
preferences_get_swank_port_global:
.LFB2772:
	.cfi_startproc
	endbr64
	movzx	eax, WORD PTR g_pref_swank_port[rip]
	ret
	.cfi_endproc
.LFE2772:
	.size	preferences_get_swank_port_global, .-preferences_get_swank_port_global
	.section	.rodata.str1.8
	.align 8
.LC80:
	.string	"preferences_set_swank_port_global: new_port=%u"
	.text
	.p2align 4
	.globl	preferences_set_swank_port_global
	.type	preferences_set_swank_port_global, @function
preferences_set_swank_port_global:
.LFB2773:
	.cfi_startproc
	endbr64
	push	rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	movzx	ecx, di
	mov	ebx, edi
	xor	eax, eax
	xor	edi, edi
	lea	rdx, .LC80[rip]
	mov	esi, 128
	call	g_log@PLT
	cmp	WORD PTR g_pref_swank_port[rip], bx
	jne	.L459
	pop	rbx
	.cfi_remember_state
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L459:
	.cfi_restore_state
	mov	WORD PTR g_pref_swank_port[rip], bx
	xor	eax, eax
	pop	rbx
	.cfi_def_cfa_offset 8
	jmp	preferences_save_globals
	.cfi_endproc
.LFE2773:
	.size	preferences_set_swank_port_global, .-preferences_set_swank_port_global
	.section	.rodata.str1.8
	.align 8
.LC81:
	.string	"preferences_cleanup_globals: Freeing global preference data."
	.text
	.p2align 4
	.globl	preferences_cleanup_globals
	.type	preferences_cleanup_globals, @function
preferences_cleanup_globals:
.LFB2774:
	.cfi_startproc
	endbr64
	sub	rsp, 8
	.cfi_def_cfa_offset 16
	lea	rdx, .LC81[rip]
	xor	eax, eax
	xor	edi, edi
	mov	esi, 128
	call	g_log@PLT
	mov	rdi, QWORD PTR g_pref_filename[rip]
	call	g_free@PLT
	mov	rdi, QWORD PTR g_pref_sdk[rip]
	mov	QWORD PTR g_pref_filename[rip], 0
	call	g_free@PLT
	mov	QWORD PTR g_pref_sdk[rip], 0
	add	rsp, 8
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE2774:
	.size	preferences_cleanup_globals, .-preferences_cleanup_globals
	.section	.rodata.str1.1
.LC82:
	.string	"_Cancel"
.LC83:
	.string	"_OK"
.LC84:
	.string	"Preferences"
.LC85:
	.string	"Lisp binary:"
	.text
	.p2align 4
	.globl	preferences_dialog_new
	.type	preferences_dialog_new, @function
preferences_dialog_new:
.LFB2775:
	.cfi_startproc
	endbr64
	push	r15
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	xor	eax, eax
	push	r14
	.cfi_def_cfa_offset 24
	.cfi_offset 14, -24
	push	r13
	.cfi_def_cfa_offset 32
	.cfi_offset 13, -32
	lea	r13, LISP_NAMES[rip+16]
	push	r12
	.cfi_def_cfa_offset 40
	.cfi_offset 12, -40
	lea	r12, SEARCH_DIRS[rip]
	push	rbp
	.cfi_def_cfa_offset 48
	.cfi_offset 6, -48
	push	rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	mov	rbx, rdi
	mov	edi, 16
	sub	rsp, 24
	.cfi_def_cfa_offset 80
	call	[QWORD PTR reloc_functions[rip+504]]
	push	0
	.cfi_def_cfa_offset 88
	mov	edx, 1
	mov	rsi, rbx
	push	-6
	.cfi_def_cfa_offset 96
	lea	rcx, .LC83[rip]
	lea	r9, .LC82[rip]
	mov	r8d, -5
	mov	r15, rax
	lea	rdi, .LC84[rip]
	xor	eax, eax
	call	gtk_dialog_new_with_buttons@PLT
	mov	rdi, rax
	mov	QWORD PTR [r15], rax
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+144]]
	mov	rbx, rax
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+184]]
	mov	esi, 10
	mov	r14, rax
	mov	QWORD PTR 24[rsp], rax
	mov	rdi, rax
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+392]]
	mov	rdi, r14
	mov	esi, 10
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+384]]
	mov	rdi, r14
	mov	esi, 10
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+400]]
	mov	rdi, r14
	mov	esi, 10
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+376]]
	mov	rdi, r14
	mov	esi, 10
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+200]]
	mov	rdi, r14
	mov	esi, 10
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+192]]
	mov	rsi, r14
	mov	rdi, rbx
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+136]]
	lea	rdi, .LC85[rip]
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+216]]
	mov	QWORD PTR 16[rsp], rax
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+128]]
	mov	rdi, QWORD PTR g_free@GOTPCREL[rip]
	mov	QWORD PTR 8[r15], rax
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+488]]
	pop	rdx
	.cfi_def_cfa_offset 88
	pop	rcx
	.cfi_def_cfa_offset 80
	mov	r14, rax
	.p2align 4,,10
	.p2align 3
.L463:
	lea	rbp, LISP_NAMES[rip]
	jmp	.L466
	.p2align 4,,10
	.p2align 3
.L474:
	add	rbp, 8
	mov	rsi, rbx
	mov	rdi, r14
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+472]]
	cmp	rbp, r13
	je	.L473
.L466:
	mov	rsi, QWORD PTR 0[rbp]
	mov	rdi, QWORD PTR [r12]
	xor	edx, edx
	xor	eax, eax
	call	g_build_filename@PLT
	mov	esi, 16
	mov	rbx, rax
	mov	rdi, rax
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+640]]
	test	eax, eax
	je	.L464
	xor	eax, eax
	mov	esi, 8
	mov	rdi, rbx
	call	[QWORD PTR reloc_functions[rip+640]]
	test	eax, eax
	jne	.L474
.L464:
	mov	rdi, rbx
	add	rbp, 8
	call	g_free@PLT
	cmp	rbp, r13
	jne	.L466
.L473:
	add	r12, 8
	lea	rax, SEARCH_DIRS[rip+16]
	cmp	r12, rax
	jne	.L463
	mov	eax, DWORD PTR 8[r14]
	test	eax, eax
	je	.L468
	xor	ebx, ebx
	.p2align 4,,10
	.p2align 3
.L469:
	mov	rax, QWORD PTR [r14]
	mov	rdi, QWORD PTR 8[r15]
	mov	rsi, QWORD PTR [rax+rbx*8]
	xor	eax, eax
	add	rbx, 1
	mov	rdx, rsi
	call	[QWORD PTR reloc_functions[rip+120]]
	cmp	ebx, DWORD PTR 8[r14]
	jb	.L469
.L468:
	mov	rdi, r14
	mov	esi, 1
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+512]]
	mov	rbx, QWORD PTR 8[rsp]
	mov	rsi, QWORD PTR [rsp]
	xor	ecx, ecx
	mov	r9d, 1
	mov	r8d, 1
	xor	edx, edx
	xor	eax, eax
	mov	rdi, rbx
	call	[QWORD PTR reloc_functions[rip+176]]
	mov	rsi, QWORD PTR 8[r15]
	mov	rdi, rbx
	xor	ecx, ecx
	mov	r9d, 1
	mov	r8d, 1
	mov	edx, 1
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+176]]
	add	rsp, 24
	.cfi_def_cfa_offset 56
	mov	rax, r15
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
	ret
	.cfi_endproc
.LFE2775:
	.size	preferences_dialog_new, .-preferences_dialog_new
	.p2align 4
	.globl	preferences_dialog_free
	.type	preferences_dialog_free, @function
preferences_dialog_free:
.LFB2776:
	.cfi_startproc
	endbr64
	test	rdi, rdi
	je	.L475
	push	rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	mov	rbx, rdi
	xor	eax, eax
	mov	rdi, QWORD PTR [rdi]
	call	[QWORD PTR reloc_functions[rip+368]]
	mov	rdi, rbx
	pop	rbx
	.cfi_restore 3
	.cfi_def_cfa_offset 8
	jmp	g_free@PLT
	.p2align 4,,10
	.p2align 3
.L475:
	ret
	.cfi_endproc
.LFE2776:
	.size	preferences_dialog_free, .-preferences_dialog_free
	.p2align 4
	.globl	preferences_dialog_load
	.type	preferences_dialog_load, @function
preferences_dialog_load:
.LFB2777:
	.cfi_startproc
	endbr64
	mov	rdi, QWORD PTR 8[rdi]
	mov	rsi, QWORD PTR g_pref_sdk[rip]
	xor	eax, eax
	jmp	[QWORD PTR reloc_functions[rip+112]]
	.cfi_endproc
.LFE2777:
	.size	preferences_dialog_load, .-preferences_dialog_load
	.p2align 4
	.globl	preferences_dialog_save
	.type	preferences_dialog_save, @function
preferences_dialog_save:
.LFB2778:
	.cfi_startproc
	endbr64
	sub	rsp, 8
	.cfi_def_cfa_offset 16
	xor	eax, eax
	mov	rdi, QWORD PTR 8[rdi]
	call	[QWORD PTR reloc_functions[rip+496]]
	add	rsp, 8
	.cfi_def_cfa_offset 8
	mov	rdi, rax
	jmp	preferences_set_sdk_global
	.cfi_endproc
.LFE2778:
	.size	preferences_dialog_save, .-preferences_dialog_save
	.p2align 4
	.globl	preferences_dialog_run
	.type	preferences_dialog_run, @function
preferences_dialog_run:
.LFB2779:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	xor	eax, eax
	xor	ebp, ebp
	push	rbx
	.cfi_def_cfa_offset 24
	.cfi_offset 3, -24
	mov	rbx, rdi
	sub	rsp, 8
	.cfi_def_cfa_offset 32
	mov	rsi, QWORD PTR g_pref_sdk[rip]
	mov	rdi, QWORD PTR 8[rdi]
	call	[QWORD PTR reloc_functions[rip+112]]
	mov	rdi, QWORD PTR [rbx]
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+408]]
	xor	eax, eax
	mov	rdi, QWORD PTR [rbx]
	call	[QWORD PTR reloc_functions[rip+152]]
	cmp	eax, -5
	sete	bpl
	je	.L486
.L484:
	mov	rdi, QWORD PTR [rbx]
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+480]]
	add	rsp, 8
	.cfi_remember_state
	.cfi_def_cfa_offset 24
	mov	eax, ebp
	pop	rbx
	.cfi_def_cfa_offset 16
	pop	rbp
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L486:
	.cfi_restore_state
	mov	rdi, QWORD PTR 8[rbx]
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+496]]
	mov	rdi, rax
	call	preferences_set_sdk_global
	jmp	.L484
	.cfi_endproc
.LFE2779:
	.size	preferences_dialog_run, .-preferences_dialog_run
	.p2align 4
	.globl	on_preferences_global
	.type	on_preferences_global, @function
on_preferences_global:
.LFB2780:
	.cfi_startproc
	endbr64
	push	rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+768]]
	mov	rdi, rax
	call	preferences_dialog_new
	mov	rbx, rax
	mov	rdi, rax
	call	preferences_dialog_run
	test	rbx, rbx
	je	.L487
	mov	rdi, QWORD PTR [rbx]
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+368]]
	mov	rdi, rbx
	pop	rbx
	.cfi_remember_state
	.cfi_def_cfa_offset 8
	jmp	g_free@PLT
	.p2align 4,,10
	.p2align 3
.L487:
	.cfi_restore_state
	pop	rbx
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE2780:
	.size	on_preferences_global, .-on_preferences_global
	.section	.rodata.str1.8
	.align 8
.LC86:
	.string	"real_process_global_set_stdout_cb"
	.section	.rodata.str1.1
.LC87:
	.string	"process-stdout"
	.text
	.p2align 4
	.globl	real_process_global_set_stdout_cb
	.type	real_process_global_set_stdout_cb, @function
real_process_global_set_stdout_cb:
.LFB2786:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	lea	rdx, .LC86[rip]
	mov	rbp, rsi
	xor	eax, eax
	push	rbx
	.cfi_def_cfa_offset 24
	.cfi_offset 3, -24
	mov	esi, 128
	mov	rbx, rdi
	xor	edi, edi
	sub	rsp, 8
	.cfi_def_cfa_offset 32
	call	g_log@PLT
	mov	QWORD PTR g_real_process_out_cb[rip], rbx
	mov	QWORD PTR g_real_process_out_user_data[rip], rbp
	test	rbx, rbx
	je	.L490
	mov	edx, DWORD PTR g_real_process_started[rip]
	test	edx, edx
	je	.L490
	cmp	QWORD PTR g_real_process_out_thread[rip], 0
	je	.L496
.L490:
	add	rsp, 8
	.cfi_remember_state
	.cfi_def_cfa_offset 24
	pop	rbx
	.cfi_def_cfa_offset 16
	pop	rbp
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L496:
	.cfi_restore_state
	mov	eax, DWORD PTR g_real_process_out_fd[rip]
	test	eax, eax
	js	.L490
	xor	edx, edx
	lea	rsi, stdout_thread_global[rip]
	lea	rdi, .LC87[rip]
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+560]]
	mov	QWORD PTR g_real_process_out_thread[rip], rax
	jmp	.L490
	.cfi_endproc
.LFE2786:
	.size	real_process_global_set_stdout_cb, .-real_process_global_set_stdout_cb
	.section	.rodata.str1.8
	.align 8
.LC88:
	.string	"real_process_global_set_stderr_cb"
	.section	.rodata.str1.1
.LC89:
	.string	"process-stderr"
	.text
	.p2align 4
	.globl	real_process_global_set_stderr_cb
	.type	real_process_global_set_stderr_cb, @function
real_process_global_set_stderr_cb:
.LFB2787:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	lea	rdx, .LC88[rip]
	mov	rbp, rsi
	xor	eax, eax
	push	rbx
	.cfi_def_cfa_offset 24
	.cfi_offset 3, -24
	mov	esi, 128
	mov	rbx, rdi
	xor	edi, edi
	sub	rsp, 8
	.cfi_def_cfa_offset 32
	call	g_log@PLT
	mov	QWORD PTR g_real_process_err_cb[rip], rbx
	mov	QWORD PTR g_real_process_err_user_data[rip], rbp
	test	rbx, rbx
	je	.L497
	mov	edx, DWORD PTR g_real_process_started[rip]
	test	edx, edx
	je	.L497
	cmp	QWORD PTR g_real_process_err_thread[rip], 0
	je	.L503
.L497:
	add	rsp, 8
	.cfi_remember_state
	.cfi_def_cfa_offset 24
	pop	rbx
	.cfi_def_cfa_offset 16
	pop	rbp
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L503:
	.cfi_restore_state
	mov	eax, DWORD PTR g_real_process_err_fd[rip]
	test	eax, eax
	js	.L497
	xor	edx, edx
	lea	rsi, stderr_thread_global[rip]
	lea	rdi, .LC89[rip]
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+560]]
	mov	QWORD PTR g_real_process_err_thread[rip], rax
	jmp	.L497
	.cfi_endproc
.LFE2787:
	.size	real_process_global_set_stderr_cb, .-real_process_global_set_stderr_cb
	.section	.rodata.str1.1
.LC90:
	.string	"Unknown error"
.LC91:
	.string	"real_process_global_start"
	.section	.rodata.str1.8
	.align 8
.LC92:
	.string	"real_process_global_start: Process already started."
	.align 8
.LC93:
	.string	"real_process_global_start: No command (argv) to start.\n"
	.align 8
.LC94:
	.string	"real_process_global_start: g_spawn_async_with_pipes failed: %s\n"
	.align 8
.LC95:
	.string	"real_process_global_start: Spawned PID %d, in_fd=%d, out_fd=%d, err_fd=%d"
	.text
	.p2align 4
	.globl	real_process_global_start
	.type	real_process_global_start, @function
real_process_global_start:
.LFB2788:
	.cfi_startproc
	endbr64
	push	rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	xor	edi, edi
	lea	rdx, .LC91[rip]
	mov	esi, 128
	sub	rsp, 16
	.cfi_def_cfa_offset 32
	mov	rax, QWORD PTR fs:40
	mov	QWORD PTR 8[rsp], rax
	xor	eax, eax
	call	g_log@PLT
	mov	edi, DWORD PTR g_real_process_started[rip]
	test	edi, edi
	jne	.L526
	mov	rsi, QWORD PTR g_real_process_argv[rip]
	test	rsi, rsi
	je	.L507
	cmp	QWORD PTR [rsi], 0
	je	.L507
	mov	QWORD PTR [rsp], 0
	sub	rsp, 8
	.cfi_def_cfa_offset 40
	xor	edx, edx
	xor	edi, edi
	lea	rbx, 8[rsp]
	xor	r9d, r9d
	lea	rax, g_real_process_err_fd[rip]
	mov	ecx, 38
	push	rbx
	.cfi_def_cfa_offset 48
	lea	r8, child_setup_global[rip]
	push	rax
	.cfi_def_cfa_offset 56
	lea	rax, g_real_process_out_fd[rip]
	push	rax
	.cfi_def_cfa_offset 64
	lea	rax, g_real_process_in_fd[rip]
	push	rax
	.cfi_def_cfa_offset 72
	lea	rax, g_real_process_pid[rip]
	push	rax
	.cfi_def_cfa_offset 80
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+632]]
	mov	rsp, rbx
	.cfi_def_cfa_offset 32
	test	eax, eax
	jne	.L510
	mov	rax, QWORD PTR [rsp]
	lea	rsi, .LC90[rip]
	test	rax, rax
	je	.L511
	mov	rsi, QWORD PTR 8[rax]
.L511:
	lea	rdi, .LC94[rip]
	xor	eax, eax
	call	g_printerr@PLT
	mov	rdi, rbx
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+8]]
	mov	rdi, QWORD PTR g_real_process_argv[rip]
	call	g_strfreev@PLT
	mov	edi, DWORD PTR g_real_process_in_fd[rip]
	mov	QWORD PTR g_real_process_argv[rip], 0
	test	edi, edi
	jns	.L527
	mov	edi, DWORD PTR g_real_process_out_fd[rip]
	test	edi, edi
	jns	.L528
.L513:
	mov	edi, DWORD PTR g_real_process_err_fd[rip]
	test	edi, edi
	jns	.L529
	.p2align 4,,10
	.p2align 3
.L504:
	mov	rax, QWORD PTR 8[rsp]
	sub	rax, QWORD PTR fs:40
	jne	.L525
	add	rsp, 16
	.cfi_remember_state
	.cfi_def_cfa_offset 16
	pop	rbx
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L510:
	.cfi_restore_state
	mov	eax, DWORD PTR g_real_process_err_fd[rip]
	sub	rsp, 8
	.cfi_def_cfa_offset 40
	mov	esi, 128
	xor	edi, edi
	lea	rdx, .LC95[rip]
	push	rax
	.cfi_def_cfa_offset 48
	mov	ecx, DWORD PTR g_real_process_pid[rip]
	xor	eax, eax
	mov	r9d, DWORD PTR g_real_process_out_fd[rip]
	mov	r8d, DWORD PTR g_real_process_in_fd[rip]
	call	g_log@PLT
	cmp	QWORD PTR g_real_process_out_cb[rip], 0
	mov	DWORD PTR g_real_process_started[rip], 1
	pop	rcx
	.cfi_def_cfa_offset 40
	pop	rsi
	.cfi_def_cfa_offset 32
	je	.L516
	cmp	QWORD PTR g_real_process_out_thread[rip], 0
	je	.L530
.L516:
	cmp	QWORD PTR g_real_process_err_cb[rip], 0
	je	.L504
	cmp	QWORD PTR g_real_process_err_thread[rip], 0
	jne	.L504
	mov	eax, DWORD PTR g_real_process_err_fd[rip]
	test	eax, eax
	js	.L504
	xor	edx, edx
	lea	rsi, stderr_thread_global[rip]
	lea	rdi, .LC89[rip]
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+560]]
	mov	QWORD PTR g_real_process_err_thread[rip], rax
	jmp	.L504
	.p2align 4,,10
	.p2align 3
.L507:
	mov	rax, QWORD PTR 8[rsp]
	sub	rax, QWORD PTR fs:40
	jne	.L525
	add	rsp, 16
	.cfi_remember_state
	.cfi_def_cfa_offset 16
	lea	rdi, .LC93[rip]
	xor	eax, eax
	pop	rbx
	.cfi_def_cfa_offset 8
	jmp	g_printerr@PLT
	.p2align 4,,10
	.p2align 3
.L526:
	.cfi_restore_state
	mov	rax, QWORD PTR 8[rsp]
	sub	rax, QWORD PTR fs:40
	jne	.L525
	add	rsp, 16
	.cfi_remember_state
	.cfi_def_cfa_offset 16
	lea	rdx, .LC92[rip]
	xor	edi, edi
	xor	eax, eax
	mov	esi, 16
	pop	rbx
	.cfi_def_cfa_offset 8
	jmp	g_log@PLT
	.p2align 4,,10
	.p2align 3
.L529:
	.cfi_restore_state
	call	close@PLT
	mov	DWORD PTR g_real_process_err_fd[rip], -1
	jmp	.L504
	.p2align 4,,10
	.p2align 3
.L528:
	call	close@PLT
	mov	edi, DWORD PTR g_real_process_err_fd[rip]
	mov	DWORD PTR g_real_process_out_fd[rip], -1
	test	edi, edi
	js	.L504
	jmp	.L529
	.p2align 4,,10
	.p2align 3
.L527:
	call	close@PLT
	mov	edi, DWORD PTR g_real_process_out_fd[rip]
	mov	DWORD PTR g_real_process_in_fd[rip], -1
	test	edi, edi
	js	.L513
	jmp	.L528
	.p2align 4,,10
	.p2align 3
.L530:
	mov	edx, DWORD PTR g_real_process_out_fd[rip]
	test	edx, edx
	js	.L516
	xor	edx, edx
	lea	rsi, stdout_thread_global[rip]
	lea	rdi, .LC87[rip]
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+560]]
	mov	QWORD PTR g_real_process_out_thread[rip], rax
	jmp	.L516
.L525:
	call	__stack_chk_fail@PLT
	.cfi_endproc
.LFE2788:
	.size	real_process_global_start, .-real_process_global_start
	.section	.rodata.str1.8
	.align 8
.LC96:
	.string	"real_process_global_write: Process not started or input FD is invalid."
	.align 8
.LC97:
	.string	"real_process_global_write: Writing %zd bytes to process stdin (fd %d)"
	.align 8
.LC98:
	.string	"real_process_global_write: write error to fd %d: %s (errno %d)\n"
	.align 8
.LC99:
	.string	"real_process_global_write: partial write to fd %d. Wrote %zd of %zd bytes."
	.text
	.p2align 4
	.globl	real_process_global_write
	.type	real_process_global_write, @function
real_process_global_write:
.LFB2789:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	push	rbx
	.cfi_def_cfa_offset 24
	.cfi_offset 3, -24
	sub	rsp, 8
	.cfi_def_cfa_offset 32
	mov	eax, DWORD PTR g_real_process_started[rip]
	test	eax, eax
	je	.L532
	mov	r8d, DWORD PTR g_real_process_in_fd[rip]
	test	r8d, r8d
	js	.L532
	mov	rbp, rdi
	mov	rbx, rsi
	test	rsi, rsi
	js	.L541
.L535:
	xor	eax, eax
	mov	rcx, rbx
	lea	rdx, .LC97[rip]
	xor	edi, edi
	mov	esi, 128
	call	g_log@PLT
	movsx	rax, DWORD PTR g_real_process_in_fd[rip]
#APP
# 86 "../common/syscalls.h" 1
	mov rdi, rax
	mov rsi, rbp
	mov rdx, rbx
	mov rax, $1
	syscall
# 0 "" 2
#NO_APP
	mov	rbp, rax
	test	rax, rax
	js	.L536
	cmp	rbx, rax
	jg	.L542
.L538:
	xor	eax, eax
	cmp	rbx, rbp
	sete	al
	add	rsp, 8
	.cfi_remember_state
	.cfi_def_cfa_offset 24
	pop	rbx
	.cfi_def_cfa_offset 16
	pop	rbp
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L541:
	.cfi_restore_state
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+448]]
	mov	r8d, DWORD PTR g_real_process_in_fd[rip]
	mov	rbx, rax
	jmp	.L535
	.p2align 4,,10
	.p2align 3
.L532:
	xor	eax, eax
	lea	rdx, .LC96[rip]
	mov	esi, 16
	xor	edi, edi
	call	g_log@PLT
	add	rsp, 8
	.cfi_remember_state
	.cfi_def_cfa_offset 24
	xor	eax, eax
	pop	rbx
	.cfi_def_cfa_offset 16
	pop	rbp
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L542:
	.cfi_restore_state
	mov	ecx, DWORD PTR g_real_process_in_fd[rip]
	mov	r8, rax
	mov	r9, rbx
	xor	edi, edi
	lea	rdx, .LC99[rip]
	mov	esi, 16
	xor	eax, eax
	call	g_log@PLT
	jmp	.L538
.L536:
	call	__errno_location@PLT
	neg	ebp
	mov	DWORD PTR [rax], ebp
	mov	edi, ebp
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+776]]
	mov	esi, DWORD PTR g_real_process_in_fd[rip]
	mov	ecx, ebp
	lea	rdi, .LC98[rip]
	mov	rdx, rax
	xor	eax, eax
	mov	rbp, -1
	call	g_printerr@PLT
	jmp	.L538
	.cfi_endproc
.LFE2789:
	.size	real_process_global_write, .-real_process_global_write
	.section	.rodata.str1.8
	.align 8
.LC100:
	.string	"real_process_cleanup_globals: Cleaning up global process resources."
	.align 8
.LC101:
	.string	"real_process_cleanup_globals: Closing in_fd %d"
	.align 8
.LC102:
	.string	"real_process_cleanup_globals: Closing out_fd %d"
	.align 8
.LC103:
	.string	"real_process_cleanup_globals: Closing err_fd %d"
	.align 8
.LC104:
	.string	"real_process_cleanup_globals: Joining stdout_thread"
	.align 8
.LC105:
	.string	"real_process_cleanup_globals: Joining stderr_thread"
	.align 8
.LC106:
	.string	"real_process_cleanup_globals: Closing PID %d"
	.align 8
.LC107:
	.string	"real_process_cleanup_globals: Cleanup complete."
	.text
	.p2align 4
	.globl	real_process_cleanup_globals
	.type	real_process_cleanup_globals, @function
real_process_cleanup_globals:
.LFB2790:
	.cfi_startproc
	endbr64
	sub	rsp, 8
	.cfi_def_cfa_offset 16
	lea	rdx, .LC100[rip]
	xor	edi, edi
	xor	eax, eax
	mov	esi, 128
	call	g_log@PLT
	mov	ecx, DWORD PTR g_real_process_in_fd[rip]
	test	ecx, ecx
	js	.L544
	lea	rdx, .LC101[rip]
	mov	esi, 128
	xor	edi, edi
	xor	eax, eax
	call	g_log@PLT
	mov	edi, DWORD PTR g_real_process_in_fd[rip]
	call	close@PLT
	mov	DWORD PTR g_real_process_in_fd[rip], -1
.L544:
	mov	ecx, DWORD PTR g_real_process_out_fd[rip]
	test	ecx, ecx
	js	.L545
	lea	rdx, .LC102[rip]
	mov	esi, 128
	xor	edi, edi
	xor	eax, eax
	call	g_log@PLT
	mov	edi, DWORD PTR g_real_process_out_fd[rip]
	call	close@PLT
	mov	DWORD PTR g_real_process_out_fd[rip], -1
.L545:
	mov	ecx, DWORD PTR g_real_process_err_fd[rip]
	test	ecx, ecx
	js	.L546
	lea	rdx, .LC103[rip]
	mov	esi, 128
	xor	edi, edi
	xor	eax, eax
	call	g_log@PLT
	mov	edi, DWORD PTR g_real_process_err_fd[rip]
	call	close@PLT
	mov	DWORD PTR g_real_process_err_fd[rip], -1
.L546:
	cmp	QWORD PTR g_real_process_out_thread[rip], 0
	je	.L547
	lea	rdx, .LC104[rip]
	mov	esi, 128
	xor	edi, edi
	xor	eax, eax
	call	g_log@PLT
	mov	rdi, QWORD PTR g_real_process_out_thread[rip]
	call	g_thread_join@PLT
	mov	QWORD PTR g_real_process_out_thread[rip], 0
.L547:
	cmp	QWORD PTR g_real_process_err_thread[rip], 0
	je	.L548
	lea	rdx, .LC105[rip]
	mov	esi, 128
	xor	edi, edi
	xor	eax, eax
	call	g_log@PLT
	mov	rdi, QWORD PTR g_real_process_err_thread[rip]
	call	g_thread_join@PLT
	mov	QWORD PTR g_real_process_err_thread[rip], 0
.L548:
	mov	ecx, DWORD PTR g_real_process_pid[rip]
	test	ecx, ecx
	jle	.L549
	lea	rdx, .LC106[rip]
	mov	esi, 128
	xor	edi, edi
	xor	eax, eax
	call	g_log@PLT
	mov	edi, DWORD PTR g_real_process_pid[rip]
	call	g_spawn_close_pid@PLT
	mov	DWORD PTR g_real_process_pid[rip], 0
.L549:
	mov	rdi, QWORD PTR g_real_process_argv[rip]
	call	g_strfreev@PLT
	lea	rdx, .LC107[rip]
	xor	edi, edi
	xor	eax, eax
	mov	QWORD PTR g_real_process_argv[rip], 0
	mov	esi, 128
	mov	DWORD PTR g_real_process_started[rip], 0
	mov	QWORD PTR g_real_process_out_cb[rip], 0
	mov	QWORD PTR g_real_process_err_cb[rip], 0
	mov	QWORD PTR g_real_process_out_user_data[rip], 0
	mov	QWORD PTR g_real_process_err_user_data[rip], 0
	add	rsp, 8
	.cfi_def_cfa_offset 8
	jmp	g_log@PLT
	.cfi_endproc
.LFE2790:
	.size	real_process_cleanup_globals, .-real_process_cleanup_globals
	.section	.rodata.str1.8
	.align 8
.LC108:
	.string	"real_process_init_globals_from_argv: cmd=%s"
	.align 8
.LC109:
	.string	"real_process_init_globals_from_argv: Process already initialized or started. Cleaning up old one."
	.text
	.p2align 4
	.globl	real_process_init_globals_from_argv
	.type	real_process_init_globals_from_argv, @function
real_process_init_globals_from_argv:
.LFB2784:
	.cfi_startproc
	endbr64
	push	rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	lea	rcx, .LC57[rip]
	mov	rbx, rdi
	test	rdi, rdi
	je	.L552
	mov	rcx, QWORD PTR [rdi]
	lea	rax, .LC57[rip]
	test	rcx, rcx
	cmove	rcx, rax
.L552:
	xor	eax, eax
	lea	rdx, .LC108[rip]
	mov	esi, 128
	xor	edi, edi
	call	g_log@PLT
	mov	eax, DWORD PTR g_real_process_started[rip]
	test	eax, eax
	jne	.L558
.L553:
	mov	rdi, QWORD PTR g_real_process_argv[rip]
	call	g_strfreev@PLT
	mov	rdi, rbx
	call	g_strdupv@PLT
	pop	rbx
	.cfi_remember_state
	.cfi_def_cfa_offset 8
	mov	DWORD PTR g_real_process_pid[rip], 0
	mov	QWORD PTR g_real_process_argv[rip], rax
	mov	DWORD PTR g_real_process_in_fd[rip], -1
	mov	DWORD PTR g_real_process_out_fd[rip], -1
	mov	DWORD PTR g_real_process_err_fd[rip], -1
	mov	QWORD PTR g_real_process_out_cb[rip], 0
	mov	QWORD PTR g_real_process_out_user_data[rip], 0
	mov	QWORD PTR g_real_process_err_cb[rip], 0
	mov	QWORD PTR g_real_process_err_user_data[rip], 0
	mov	QWORD PTR g_real_process_out_thread[rip], 0
	mov	QWORD PTR g_real_process_err_thread[rip], 0
	mov	DWORD PTR g_real_process_started[rip], 0
	ret
	.p2align 4,,10
	.p2align 3
.L558:
	.cfi_restore_state
	xor	eax, eax
	lea	rdx, .LC109[rip]
	mov	esi, 16
	xor	edi, edi
	call	g_log@PLT
	xor	eax, eax
	call	real_process_cleanup_globals
	jmp	.L553
	.cfi_endproc
.LFE2784:
	.size	real_process_init_globals_from_argv, .-real_process_init_globals_from_argv
	.section	.rodata.str1.8
	.align 8
.LC110:
	.string	"real_process_init_globals: cmd=%s"
	.align 8
.LC111:
	.string	"real_process_init_globals: cmd is NULL."
	.text
	.p2align 4
	.globl	real_process_init_globals
	.type	real_process_init_globals, @function
real_process_init_globals:
.LFB2785:
	.cfi_startproc
	endbr64
	push	rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	sub	rsp, 32
	.cfi_def_cfa_offset 48
	mov	rax, QWORD PTR fs:40
	mov	QWORD PTR 24[rsp], rax
	xor	eax, eax
	test	rdi, rdi
	je	.L560
	mov	rbx, rdi
	mov	rcx, rdi
	mov	esi, 128
	xor	edi, edi
	lea	rdx, .LC110[rip]
	call	g_log@PLT
	mov	rdi, rsp
	mov	QWORD PTR [rsp], rbx
	mov	QWORD PTR 8[rsp], 0
	call	real_process_init_globals_from_argv
	mov	rax, QWORD PTR 24[rsp]
	sub	rax, QWORD PTR fs:40
	jne	.L564
	add	rsp, 32
	.cfi_remember_state
	.cfi_def_cfa_offset 16
	pop	rbx
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L560:
	.cfi_restore_state
	xor	edi, edi
	xor	eax, eax
	lea	rdx, .LC110[rip]
	mov	esi, 128
	lea	rcx, .LC57[rip]
	call	g_log@PLT
	xor	edi, edi
	xor	eax, eax
	lea	rdx, .LC111[rip]
	mov	esi, 16
	call	g_log@PLT
	mov	rax, QWORD PTR 24[rsp]
	sub	rax, QWORD PTR fs:40
	jne	.L564
	add	rsp, 32
	.cfi_remember_state
	.cfi_def_cfa_offset 16
	xor	edi, edi
	pop	rbx
	.cfi_def_cfa_offset 8
	jmp	real_process_init_globals_from_argv
.L564:
	.cfi_restore_state
	call	__stack_chk_fail@PLT
	.cfi_endproc
.LFE2785:
	.size	real_process_init_globals, .-real_process_init_globals
	.section	.rodata.str1.8
	.align 8
.LC112:
	.string	"real_swank_process_global_send: Swank process not started or FD invalid."
	.section	.rodata.str1.1
.LC113:
	.string	"%06zx"
	.section	.rodata.str1.8
	.align 8
.LC114:
	.string	"real_swank_process_global_send: Sending Swank message: Header='%s', Payload='%.*s'"
	.align 8
.LC115:
	.string	"real_swank_process_global_send: Failed to write Swank header (wrote %zd, errno %d)\n"
	.align 8
.LC116:
	.string	"real_swank_process_global_send: Failed to write Swank payload (wrote %zd of %zu, errno %d)\n"
	.align 8
.LC117:
	.string	"real_swank_process_global_send: Message sent successfully."
	.text
	.p2align 4
	.globl	real_swank_process_global_send
	.type	real_swank_process_global_send, @function
real_swank_process_global_send:
.LFB2800:
	.cfi_startproc
	endbr64
	push	r12
	.cfi_def_cfa_offset 16
	.cfi_offset 12, -16
	push	rbp
	.cfi_def_cfa_offset 24
	.cfi_offset 6, -24
	push	rbx
	.cfi_def_cfa_offset 32
	.cfi_offset 3, -32
	sub	rsp, 16
	.cfi_def_cfa_offset 48
	mov	edx, DWORD PTR g_swank_process_started[rip]
	mov	rax, QWORD PTR fs:40
	mov	QWORD PTR 8[rsp], rax
	xor	eax, eax
	test	edx, edx
	je	.L567
	mov	eax, DWORD PTR g_swank_fd[rip]
	test	eax, eax
	js	.L567
	mov	r12, QWORD PTR 8[rdi]
	lea	rbp, 1[rsp]
	mov	rbx, rdi
	xor	eax, eax
	mov	rdi, rbp
	lea	rdx, .LC113[rip]
	mov	esi, 7
	mov	rcx, r12
	call	g_snprintf@PLT
	mov	r9, QWORD PTR [rbx]
	mov	r8d, r12d
	xor	eax, eax
	mov	rcx, rbp
	lea	rdx, .LC114[rip]
	mov	esi, 128
	xor	edi, edi
	call	g_log@PLT
	movsx	rax, DWORD PTR g_swank_fd[rip]
	mov	r8d, 6
#APP
# 86 "../common/syscalls.h" 1
	mov rdi, rax
	mov rsi, rbp
	mov rdx, r8
	mov rax, $1
	syscall
# 0 "" 2
#NO_APP
	mov	rbp, rax
	test	rax, rax
	js	.L580
	cmp	rax, 6
	je	.L572
	call	__errno_location@PLT
	mov	edx, DWORD PTR [rax]
.L571:
	mov	rsi, rbp
	lea	rdi, .LC115[rip]
	xor	eax, eax
	call	g_printerr@PLT
.L566:
	mov	rax, QWORD PTR 8[rsp]
	sub	rax, QWORD PTR fs:40
	jne	.L579
	add	rsp, 16
	.cfi_remember_state
	.cfi_def_cfa_offset 32
	pop	rbx
	.cfi_def_cfa_offset 24
	pop	rbp
	.cfi_def_cfa_offset 16
	pop	r12
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L567:
	.cfi_restore_state
	mov	rax, QWORD PTR 8[rsp]
	sub	rax, QWORD PTR fs:40
	jne	.L579
	add	rsp, 16
	.cfi_remember_state
	.cfi_def_cfa_offset 32
	lea	rdx, .LC112[rip]
	xor	edi, edi
	xor	eax, eax
	pop	rbx
	.cfi_def_cfa_offset 24
	mov	esi, 16
	pop	rbp
	.cfi_def_cfa_offset 16
	pop	r12
	.cfi_def_cfa_offset 8
	jmp	g_log@PLT
	.p2align 4,,10
	.p2align 3
.L572:
	.cfi_restore_state
	movsx	rax, DWORD PTR g_swank_fd[rip]
	mov	r8, QWORD PTR [rbx]
#APP
# 86 "../common/syscalls.h" 1
	mov rdi, rax
	mov rsi, r8
	mov rdx, r12
	mov rax, $1
	syscall
# 0 "" 2
#NO_APP
	mov	rbx, rax
	test	rax, rax
	js	.L581
.L574:
	cmp	r12, rbx
	je	.L575
	call	__errno_location@PLT
	mov	rdx, r12
	mov	rsi, rbx
	lea	rdi, .LC116[rip]
	mov	ecx, DWORD PTR [rax]
	xor	eax, eax
	call	g_printerr@PLT
	jmp	.L566
	.p2align 4,,10
	.p2align 3
.L575:
	lea	rdx, .LC117[rip]
	mov	esi, 128
	xor	edi, edi
	xor	eax, eax
	call	g_log@PLT
	jmp	.L566
.L580:
	call	__errno_location@PLT
	mov	edx, ebp
	mov	rbp, -1
	neg	edx
	mov	DWORD PTR [rax], edx
	jmp	.L571
.L581:
	call	__errno_location@PLT
	neg	ebx
	mov	DWORD PTR [rax], ebx
	mov	rbx, -1
	jmp	.L574
.L579:
	call	__stack_chk_fail@PLT
	.cfi_endproc
.LFE2800:
	.size	real_swank_process_global_send, .-real_swank_process_global_send
	.section	.rodata.str1.8
	.align 8
.LC118:
	.string	"real_swank_process_global_set_message_cb"
	.text
	.p2align 4
	.globl	real_swank_process_global_set_message_cb
	.type	real_swank_process_global_set_message_cb, @function
real_swank_process_global_set_message_cb:
.LFB2801:
	.cfi_startproc
	endbr64
	push	r12
	.cfi_def_cfa_offset 16
	.cfi_offset 12, -16
	lea	rdx, .LC118[rip]
	lea	r12, g_swank_incoming_mutex[rip]
	xor	eax, eax
	push	rbp
	.cfi_def_cfa_offset 24
	.cfi_offset 6, -24
	mov	rbp, rdi
	xor	edi, edi
	push	rbx
	.cfi_def_cfa_offset 32
	.cfi_offset 3, -32
	mov	rbx, rsi
	mov	esi, 128
	call	g_log@PLT
	mov	rdi, r12
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+680]]
	mov	QWORD PTR g_swank_message_cb[rip], rbp
	mov	rdi, r12
	xor	eax, eax
	mov	QWORD PTR g_swank_message_cb_data[rip], rbx
	pop	rbx
	.cfi_def_cfa_offset 24
	pop	rbp
	.cfi_def_cfa_offset 16
	pop	r12
	.cfi_def_cfa_offset 8
	jmp	[QWORD PTR reloc_functions[rip+688]]
	.cfi_endproc
.LFE2801:
	.size	real_swank_process_global_set_message_cb, .-real_swank_process_global_set_message_cb
	.section	.rodata.str1.8
	.align 8
.LC119:
	.string	"real_swank_process_global_set_socket_fd: Setting Swank FD to %d"
	.align 8
.LC120:
	.string	"real_swank_process_global_set_socket_fd: Closing existing Swank FD %d"
	.align 8
.LC121:
	.string	"real_swank_process_global_set_socket_fd: Existing reader thread found. It might need manual restart."
	.text
	.p2align 4
	.globl	real_swank_process_global_set_socket_fd
	.type	real_swank_process_global_set_socket_fd, @function
real_swank_process_global_set_socket_fd:
.LFB2802:
	.cfi_startproc
	endbr64
	push	rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	mov	ecx, edi
	mov	ebx, edi
	lea	rdx, .LC119[rip]
	mov	esi, 128
	xor	edi, edi
	xor	eax, eax
	call	g_log@PLT
	mov	ecx, DWORD PTR g_swank_fd[rip]
	cmp	ecx, ebx
	je	.L587
	test	ecx, ecx
	jns	.L597
.L587:
	cmp	QWORD PTR g_swank_reader_thread[rip], 0
	mov	DWORD PTR g_swank_fd[rip], ebx
	je	.L584
.L598:
	lea	rdx, .LC121[rip]
	mov	esi, 128
	xor	edi, edi
	xor	eax, eax
	pop	rbx
	.cfi_remember_state
	.cfi_def_cfa_offset 8
	jmp	g_log@PLT
	.p2align 4,,10
	.p2align 3
.L597:
	.cfi_restore_state
	lea	rdx, .LC120[rip]
	mov	esi, 16
	xor	edi, edi
	xor	eax, eax
	call	g_log@PLT
	mov	edi, DWORD PTR g_swank_fd[rip]
	call	close@PLT
	mov	rdi, QWORD PTR g_swank_connection[rip]
	test	rdi, rdi
	je	.L587
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+648]]
	cmp	QWORD PTR g_swank_reader_thread[rip], 0
	mov	QWORD PTR g_swank_connection[rip], 0
	mov	DWORD PTR g_swank_fd[rip], ebx
	jne	.L598
.L584:
	pop	rbx
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE2802:
	.size	real_swank_process_global_set_socket_fd, .-real_swank_process_global_set_socket_fd
	.section	.rodata.str1.8
	.align 8
.LC122:
	.string	"real_swank_process_cleanup_globals: Starting cleanup."
	.align 8
.LC123:
	.string	"real_swank_process_cleanup_globals: Closing Swank FD %d."
	.align 8
.LC124:
	.string	"Error closing GSocketConnection: %s"
	.align 8
.LC125:
	.string	"real_swank_process_cleanup_globals: Joining Swank reader thread."
	.align 8
.LC126:
	.string	"real_swank_process_cleanup_globals: Cleanup complete."
	.text
	.p2align 4
	.globl	real_swank_process_cleanup_globals
	.type	real_swank_process_cleanup_globals, @function
real_swank_process_cleanup_globals:
.LFB2803:
	.cfi_startproc
	endbr64
	sub	rsp, 24
	.cfi_def_cfa_offset 32
	lea	rdx, .LC122[rip]
	mov	esi, 128
	xor	edi, edi
	mov	rax, QWORD PTR fs:40
	mov	QWORD PTR 8[rsp], rax
	xor	eax, eax
	call	g_log@PLT
	mov	ecx, DWORD PTR g_swank_fd[rip]
	test	ecx, ecx
	jns	.L611
.L600:
	cmp	QWORD PTR g_swank_reader_thread[rip], 0
	je	.L604
	lea	rdx, .LC125[rip]
	mov	esi, 128
	xor	edi, edi
	xor	eax, eax
	call	g_log@PLT
	mov	rdi, QWORD PTR g_swank_reader_thread[rip]
	call	g_thread_join@PLT
	mov	QWORD PTR g_swank_reader_thread[rip], 0
.L604:
	mov	rdi, QWORD PTR g_swank_out_buffer[rip]
	mov	esi, 1
	call	g_string_free@PLT
	mov	rdi, QWORD PTR g_swank_incoming_data_buffer[rip]
	mov	esi, 1
	mov	QWORD PTR g_swank_out_buffer[rip], 0
	call	g_string_free@PLT
	lea	rdi, g_swank_out_mutex[rip]
	xor	eax, eax
	mov	QWORD PTR g_swank_incoming_data_buffer[rip], 0
	call	[QWORD PTR reloc_functions[rip+704]]
	lea	rdi, g_swank_out_cond[rip]
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+712]]
	lea	rdi, g_swank_incoming_mutex[rip]
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+704]]
	mov	QWORD PTR g_swank_message_cb[rip], 0
	mov	QWORD PTR g_swank_message_cb_data[rip], 0
	mov	DWORD PTR g_swank_process_started[rip], 0
	mov	rax, QWORD PTR 8[rsp]
	sub	rax, QWORD PTR fs:40
	jne	.L612
	lea	rdx, .LC126[rip]
	mov	esi, 128
	xor	edi, edi
	xor	eax, eax
	add	rsp, 24
	.cfi_remember_state
	.cfi_def_cfa_offset 8
	jmp	g_log@PLT
	.p2align 4,,10
	.p2align 3
.L611:
	.cfi_restore_state
	xor	edi, edi
	lea	rdx, .LC123[rip]
	mov	esi, 128
	xor	eax, eax
	call	g_log@PLT
	mov	rdi, QWORD PTR g_swank_connection[rip]
	test	rdi, rdi
	je	.L601
	mov	rdx, rsp
	xor	esi, esi
	mov	QWORD PTR [rsp], 0
	call	g_io_stream_close@PLT
	mov	rax, QWORD PTR [rsp]
	test	rax, rax
	je	.L602
	mov	rcx, QWORD PTR 8[rax]
	xor	edi, edi
	lea	rdx, .LC124[rip]
	xor	eax, eax
	mov	esi, 16
	call	g_log@PLT
	mov	rdi, QWORD PTR [rsp]
	call	g_error_free@PLT
.L602:
	mov	rdi, QWORD PTR g_swank_connection[rip]
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+648]]
	mov	QWORD PTR g_swank_connection[rip], 0
.L603:
	mov	DWORD PTR g_swank_fd[rip], -1
	jmp	.L600
	.p2align 4,,10
	.p2align 3
.L601:
	mov	edi, DWORD PTR g_swank_fd[rip]
	call	close@PLT
	jmp	.L603
.L612:
	call	__stack_chk_fail@PLT
	.cfi_endproc
.LFE2803:
	.size	real_swank_process_cleanup_globals, .-real_swank_process_cleanup_globals
	.section	.rodata.str1.8
	.align 8
.LC127:
	.string	"real_swank_process_init_globals"
	.align 8
.LC128:
	.string	"real_swank_process_init_globals: Already initialized. Cleaning up old state."
	.align 8
.LC129:
	.string	"real_swank_process_init_globals: Invalid Swank port from preferences, using default %d"
	.align 8
.LC130:
	.string	"real_swank_process_init_globals: complete. Port: %d"
	.text
	.p2align 4
	.globl	real_swank_process_init_globals
	.type	real_swank_process_init_globals, @function
real_swank_process_init_globals:
.LFB2792:
	.cfi_startproc
	endbr64
	xor	eax, eax
	sub	rsp, 8
	.cfi_def_cfa_offset 16
	lea	rdx, .LC127[rip]
	xor	edi, edi
	mov	esi, 128
	call	g_log@PLT
	mov	eax, DWORD PTR g_swank_process_started[rip]
	test	eax, eax
	jne	.L618
.L614:
	lea	rdi, g_swank_out_mutex[rip]
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+696]]
	lea	rdi, g_swank_out_cond[rip]
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+728]]
	lea	rdi, g_swank_incoming_mutex[rip]
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+696]]
	xor	edi, edi
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+584]]
	xor	edi, edi
	mov	QWORD PTR g_swank_out_consumed[rip], 0
	mov	QWORD PTR g_swank_out_buffer[rip], rax
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+584]]
	mov	QWORD PTR g_swank_incoming_consumed[rip], 0
	mov	QWORD PTR g_swank_incoming_data_buffer[rip], rax
	movzx	eax, WORD PTR g_pref_swank_port[rip]
	mov	DWORD PTR g_swank_fd[rip], -1
	mov	QWORD PTR g_swank_connection[rip], 0
	mov	QWORD PTR g_swank_message_cb[rip], 0
	mov	QWORD PTR g_swank_message_cb_data[rip], 0
	mov	QWORD PTR g_swank_reader_thread[rip], 0
	test	eax, eax
	je	.L615
	mov	DWORD PTR g_swank_port_number[rip], eax
.L616:
	xor	esi, esi
	lea	rdi, on_lisp_stdout[rip]
	call	real_process_global_set_stdout_cb
	xor	esi, esi
	lea	rdi, on_lisp_stderr[rip]
	call	real_process_global_set_stderr_cb
	mov	ecx, DWORD PTR g_swank_port_number[rip]
	xor	edi, edi
	xor	eax, eax
	mov	DWORD PTR g_swank_process_started[rip], 0
	lea	rdx, .LC130[rip]
	mov	esi, 128
	add	rsp, 8
	.cfi_remember_state
	.cfi_def_cfa_offset 8
	jmp	g_log@PLT
	.p2align 4,,10
	.p2align 3
.L618:
	.cfi_restore_state
	xor	eax, eax
	lea	rdx, .LC128[rip]
	mov	esi, 16
	xor	edi, edi
	call	g_log@PLT
	xor	eax, eax
	call	real_swank_process_cleanup_globals
	jmp	.L614
	.p2align 4,,10
	.p2align 3
.L615:
	mov	ecx, 4005
	lea	rdx, .LC129[rip]
	xor	edi, edi
	xor	eax, eax
	mov	DWORD PTR g_swank_port_number[rip], 4005
	mov	esi, 16
	call	g_log@PLT
	jmp	.L616
	.cfi_endproc
.LFE2792:
	.size	real_swank_process_init_globals, .-real_swank_process_init_globals
	.section	.rodata.str1.8
	.align 8
.LC131:
	.string	"real_swank_process_global_start"
	.align 8
.LC132:
	.string	"real_swank_process_global_start: Swank process already started."
	.align 8
.LC133:
	.string	"real_swank_process: start_lisp_and_swank_server"
	.section	.rodata.str1.1
.LC134:
	.string	"* "
.LC135:
	.string	"(require :swank)\n"
	.section	.rodata.str1.8
	.align 8
.LC136:
	.string	"real_swank_process: Sending Lisp command: %s"
	.section	.rodata.str1.1
.LC137:
	.string	")"
	.section	.rodata.str1.8
	.align 8
.LC138:
	.string	"(swank:create-server :port %d :dont-close t)\n"
	.align 8
.LC139:
	.string	"real_swank_process: Swank server presumed started on Lisp side."
	.align 8
.LC140:
	.string	"real_swank_process: connect_to_swank_server trying port %d"
	.section	.rodata.str1.1
.LC141:
	.string	"127.0.0.1"
	.section	.rodata.str1.8
	.align 8
.LC142:
	.string	"real_swank_process: Connection attempt %d failed: %s. Retrying..."
	.align 8
.LC143:
	.string	"real_swank_process: Failed to connect to Swank server on port %d after multiple retries.\n"
	.align 8
.LC144:
	.string	"real_swank_process: Connected to Swank server. FD: %d"
	.section	.rodata.str1.1
.LC145:
	.string	"swank-reader"
	.section	.rodata.str1.8
	.align 8
.LC146:
	.string	"real_swank_process_global_start: Swank process started successfully."
	.align 8
.LC147:
	.string	"real_swank_process_global_start: Failed to start Swank process (connection failed).\n"
	.text
	.p2align 4
	.globl	real_swank_process_global_start
	.type	real_swank_process_global_start, @function
real_swank_process_global_start:
.LFB2799:
	.cfi_startproc
	endbr64
	push	r15
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	lea	rdx, .LC131[rip]
	mov	esi, 128
	xor	edi, edi
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
	sub	rsp, 168
	.cfi_def_cfa_offset 224
	mov	rax, QWORD PTR fs:40
	mov	QWORD PTR 152[rsp], rax
	xor	eax, eax
	call	g_log@PLT
	mov	ebx, DWORD PTR g_swank_process_started[rip]
	test	ebx, ebx
	jne	.L645
	lea	rdx, .LC133[rip]
	mov	esi, 128
	xor	edi, edi
	xor	eax, eax
	call	g_log@PLT
	lea	rbp, .LC134[rip]
	xor	eax, eax
	lea	r12, .LC135[rip]
	call	real_process_global_start
	lea	r13, .LC136[rip]
	mov	rdi, rbp
	lea	r15, .LC90[rip]
	call	read_until_from_lisp_output
	mov	rcx, r12
	mov	rdx, r13
	mov	esi, 128
	xor	edi, edi
	xor	eax, eax
	lea	r14, .LC142[rip]
	call	g_log@PLT
	mov	rsi, -1
	mov	rdi, r12
	lea	r12, 16[rsp]
	call	real_process_global_write
	lea	rdi, .LC137[rip]
	call	read_until_from_lisp_output
	mov	rdi, rbp
	call	read_until_from_lisp_output
	mov	ecx, DWORD PTR g_swank_port_number[rip]
	mov	rdi, r12
	xor	eax, eax
	lea	rdx, .LC138[rip]
	mov	esi, 128
	call	g_snprintf@PLT
	mov	rcx, r12
	mov	rdx, r13
	mov	esi, 128
	xor	edi, edi
	xor	eax, eax
	lea	r13, .LC141[rip]
	call	g_log@PLT
	mov	rsi, -1
	mov	rdi, r12
	call	real_process_global_write
	mov	rdi, rbp
	lea	rbp, 8[rsp]
	call	read_until_from_lisp_output
	lea	rdx, .LC139[rip]
	xor	edi, edi
	xor	eax, eax
	mov	esi, 128
	call	g_log@PLT
	mov	ecx, DWORD PTR g_swank_port_number[rip]
	xor	edi, edi
	xor	eax, eax
	lea	rdx, .LC140[rip]
	mov	esi, 128
	call	g_log@PLT
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+592]]
	mov	QWORD PTR 8[rsp], 0
	mov	r12, rax
	.p2align 4,,10
	.p2align 3
.L626:
	movzx	edx, WORD PTR g_swank_port_number[rip]
	mov	r8, rbp
	xor	ecx, ecx
	mov	rsi, r13
	mov	rdi, r12
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+544]]
	mov	QWORD PTR g_swank_connection[rip], rax
	test	rax, rax
	jne	.L627
	mov	rax, QWORD PTR 8[rsp]
	mov	r8, r15
	test	rax, rax
	je	.L625
	mov	r8, QWORD PTR 8[rax]
.L625:
	add	ebx, 1
	xor	edi, edi
	xor	eax, eax
	mov	rdx, r14
	mov	ecx, ebx
	mov	esi, 128
	call	g_log@PLT
	mov	rdi, rbp
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+8]]
	xor	eax, eax
	mov	edi, 500000
	call	[QWORD PTR reloc_functions[rip+816]]
	cmp	ebx, 10
	jne	.L626
.L627:
	mov	rdi, r12
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+648]]
	mov	rdi, QWORD PTR g_swank_connection[rip]
	test	rdi, rdi
	je	.L646
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+536]]
	mov	rdi, rax
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+576]]
	lea	rdx, .LC144[rip]
	mov	esi, 128
	xor	edi, edi
	mov	DWORD PTR g_swank_fd[rip], eax
	mov	ecx, eax
	xor	eax, eax
	call	g_log@PLT
	mov	eax, DWORD PTR g_swank_fd[rip]
	test	eax, eax
	js	.L631
	cmp	QWORD PTR g_swank_reader_thread[rip], 0
	je	.L647
.L633:
	mov	DWORD PTR g_swank_process_started[rip], 1
	mov	rax, QWORD PTR 152[rsp]
	sub	rax, QWORD PTR fs:40
	jne	.L644
	lea	rdx, .LC146[rip]
	mov	esi, 128
.L643:
	add	rsp, 168
	.cfi_remember_state
	.cfi_def_cfa_offset 56
	xor	edi, edi
	xor	eax, eax
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
	jmp	g_log@PLT
	.p2align 4,,10
	.p2align 3
.L647:
	.cfi_restore_state
	xor	edx, edx
	lea	rsi, swank_reader_thread_global[rip]
	lea	rdi, .LC145[rip]
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+560]]
	mov	QWORD PTR g_swank_reader_thread[rip], rax
.L642:
	mov	eax, DWORD PTR g_swank_fd[rip]
	test	eax, eax
	jns	.L633
.L631:
	xor	eax, eax
	lea	rdi, .LC147[rip]
	call	g_printerr@PLT
	mov	rax, QWORD PTR 152[rsp]
	sub	rax, QWORD PTR fs:40
	jne	.L644
	add	rsp, 168
	.cfi_remember_state
	.cfi_def_cfa_offset 56
	xor	eax, eax
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
	jmp	real_swank_process_cleanup_globals
	.p2align 4,,10
	.p2align 3
.L645:
	.cfi_restore_state
	mov	rax, QWORD PTR 152[rsp]
	sub	rax, QWORD PTR fs:40
	jne	.L644
	lea	rdx, .LC132[rip]
	mov	esi, 16
	jmp	.L643
	.p2align 4,,10
	.p2align 3
.L646:
	mov	esi, DWORD PTR g_swank_port_number[rip]
	xor	eax, eax
	lea	rdi, .LC143[rip]
	call	g_printerr@PLT
	cmp	QWORD PTR 8[rsp], 0
	je	.L642
	mov	rdi, rbp
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+8]]
	jmp	.L642
.L644:
	call	__stack_chk_fail@PLT
	.cfi_endproc
.LFE2799:
	.size	real_swank_process_global_start, .-real_swank_process_global_start
	.section	.rodata.str1.8
	.align 8
.LC148:
	.string	"real_swank_session_init_globals"
	.align 8
.LC149:
	.string	"real_swank_session_init_globals: Already initialized. Cleaning up old state."
	.align 8
.LC150:
	.string	"real_swank_session_cleanup_globals"
	.align 8
.LC151:
	.string	"real_swank_session_init_globals: Complete. Registered message callback."
	.text
	.p2align 4
	.globl	real_swank_session_init_globals
	.type	real_swank_session_init_globals, @function
real_swank_session_init_globals:
.LFB2809:
	.cfi_startproc
	endbr64
	push	rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	xor	edi, edi
	xor	eax, eax
	lea	rdx, .LC148[rip]
	mov	esi, 128
	call	g_log@PLT
	cmp	QWORD PTR g_swank_session_interactions_table[rip], 0
	je	.L650
	lea	rdx, .LC149[rip]
	mov	esi, 16
	xor	edi, edi
	xor	eax, eax
	call	g_log@PLT
	xor	edi, edi
	lea	rdx, .LC150[rip]
	xor	eax, eax
	mov	esi, 128
	call	g_log@PLT
	mov	rdi, QWORD PTR g_swank_session_interactions_table[rip]
	test	rdi, rdi
	je	.L650
	call	g_hash_table_destroy@PLT
	mov	QWORD PTR g_swank_session_interactions_table[rip], 0
.L650:
	mov	rsi, QWORD PTR g_direct_equal@GOTPCREL[rip]
	mov	rdi, QWORD PTR g_direct_hash@GOTPCREL[rip]
	lea	rcx, interaction_free_members_static[rip]
	xor	edx, edx
	mov	DWORD PTR g_swank_session_started[rip], 0
	lea	rbx, g_swank_incoming_mutex[rip]
	mov	DWORD PTR g_swank_session_next_tag[rip], 1
	call	g_hash_table_new_full@PLT
	lea	rdx, .LC118[rip]
	mov	esi, 128
	xor	edi, edi
	mov	QWORD PTR g_swank_session_interactions_table[rip], rax
	xor	eax, eax
	call	g_log@PLT
	mov	rdi, rbx
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+680]]
	lea	rax, real_swank_session_on_message_internal[rip]
	mov	rdi, rbx
	mov	QWORD PTR g_swank_message_cb_data[rip], 0
	mov	QWORD PTR g_swank_message_cb[rip], rax
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+688]]
	lea	rdx, .LC151[rip]
	xor	edi, edi
	xor	eax, eax
	mov	esi, 128
	pop	rbx
	.cfi_def_cfa_offset 8
	jmp	g_log@PLT
	.cfi_endproc
.LFE2809:
	.size	real_swank_session_init_globals, .-real_swank_session_init_globals
	.section	.rodata.str1.8
	.align 8
.LC152:
	.string	"real_swank_session_global_eval: NULL interaction or expression."
	.align 8
.LC153:
	.string	"real_swank_session_global_eval: expr='%s'"
	.align 8
.LC154:
	.string	"Interaction %u added (expression: %s)"
	.align 8
.LC155:
	.string	"real_swank_session_global_eval: interactions_view_global is NULL. Cannot add interaction to view."
	.section	.rodata.str1.1
.LC156:
	.string	"\\\\"
.LC157:
	.string	"\\\""
	.section	.rodata.str1.8
	.align 8
.LC158:
	.string	"(:emacs-rex (swank:eval-and-grab-output \"%s\") \"COMMON-LISP-USER\" t %u)"
	.text
	.p2align 4
	.globl	real_swank_session_global_eval
	.type	real_swank_session_global_eval, @function
real_swank_session_global_eval:
.LFB2810:
	.cfi_startproc
	endbr64
	test	rdi, rdi
	je	.L685
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
	mov	rbp, rdi
	push	rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	sub	rsp, 8
	.cfi_def_cfa_offset 64
	mov	rcx, QWORD PTR [rdi]
	test	rcx, rcx
	je	.L686
	mov	esi, 128
	lea	rdx, .LC153[rip]
	xor	edi, edi
	xor	eax, eax
	call	g_log@PLT
	mov	esi, DWORD PTR g_swank_session_started[rip]
	test	esi, esi
	je	.L687
.L659:
	mov	esi, DWORD PTR g_swank_session_next_tag[rip]
	mov	DWORD PTR 12[rbp], 1
	mov	rdx, rbp
	mov	rdi, QWORD PTR g_swank_session_interactions_table[rip]
	mov	DWORD PTR 8[rbp], esi
	lea	eax, 1[rsi]
	mov	DWORD PTR g_swank_session_next_tag[rip], eax
	call	g_hash_table_insert@PLT
	mov	ecx, DWORD PTR 8[rbp]
	mov	r8, QWORD PTR 0[rbp]
	xor	edi, edi
	lea	rdx, .LC154[rip]
	mov	esi, 128
	xor	eax, eax
	call	g_log@PLT
	mov	rdi, QWORD PTR interactions_view_global[rip]
	test	rdi, rdi
	je	.L660
	mov	rsi, rbp
	call	interactions_view_add_interaction
.L661:
	mov	r12, QWORD PTR 0[rbp]
	xor	edi, edi
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+584]]
	movsx	edx, BYTE PTR [r12]
	mov	rbx, rax
	test	dl, dl
	je	.L662
	lea	r15, .LC157[rip+2]
	lea	r14, -2[r15]
	jmp	.L675
	.p2align 4,,10
	.p2align 3
.L689:
	cmp	dl, 92
	jne	.L664
	test	rbx, rbx
	je	.L688
	mov	rax, QWORD PTR 8[rbx]
	lea	rdx, 2[rax]
	cmp	rdx, QWORD PTR 16[rbx]
	jb	.L667
	mov	ecx, 2
	lea	rdx, .LC156[rip]
	mov	rdi, rbx
	xor	eax, eax
	mov	rsi, -1
	call	[QWORD PTR reloc_functions[rip+552]]
	.p2align 4,,10
	.p2align 3
.L666:
	movsx	edx, BYTE PTR 1[r12]
	add	r12, 1
	test	dl, dl
	je	.L662
.L675:
	cmp	dl, 34
	jne	.L689
	test	rbx, rbx
	je	.L690
	mov	rax, QWORD PTR 8[rbx]
	lea	rdx, 2[rax]
	cmp	rdx, QWORD PTR 16[rbx]
	jb	.L671
	add	r12, 1
	mov	rdx, r14
	mov	ecx, 2
	mov	rdi, rbx
	mov	rsi, -1
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+552]]
	movsx	edx, BYTE PTR [r12]
	test	dl, dl
	jne	.L675
.L662:
	mov	rdi, rbx
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+616]]
	mov	edx, DWORD PTR 8[rbp]
	lea	rdi, .LC158[rip]
	mov	rsi, rax
	mov	rbx, rax
	xor	eax, eax
	call	g_strdup_printf@PLT
	mov	rdi, rbx
	mov	rbp, rax
	call	g_free@PLT
	mov	rdi, rbp
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+584]]
	mov	rdi, rbp
	mov	rbx, rax
	call	g_free@PLT
	mov	rdi, rbx
	call	real_swank_process_global_send
	add	rsp, 8
	.cfi_remember_state
	.cfi_def_cfa_offset 56
	mov	rdi, rbx
	mov	esi, 1
	pop	rbx
	.cfi_restore 3
	.cfi_def_cfa_offset 48
	pop	rbp
	.cfi_restore 6
	.cfi_def_cfa_offset 40
	pop	r12
	.cfi_restore 12
	.cfi_def_cfa_offset 32
	pop	r13
	.cfi_restore 13
	.cfi_def_cfa_offset 24
	pop	r14
	.cfi_restore 14
	.cfi_def_cfa_offset 16
	pop	r15
	.cfi_restore 15
	.cfi_def_cfa_offset 8
	jmp	g_string_free@PLT
	.p2align 4,,10
	.p2align 3
.L687:
	.cfi_restore_state
	xor	eax, eax
	call	real_swank_process_global_start
	mov	DWORD PTR g_swank_session_started[rip], 1
	jmp	.L659
	.p2align 4,,10
	.p2align 3
.L664:
	test	rbx, rbx
	je	.L674
	mov	rax, QWORD PTR 8[rbx]
	lea	rcx, 1[rax]
	cmp	rcx, QWORD PTR 16[rbx]
	jnb	.L674
	mov	rsi, QWORD PTR [rbx]
	mov	QWORD PTR 8[rbx], rcx
	mov	BYTE PTR [rsi+rax], dl
	mov	rdx, QWORD PTR [rbx]
	mov	rax, QWORD PTR 8[rbx]
	mov	BYTE PTR [rdx+rax], 0
	jmp	.L666
	.p2align 4,,10
	.p2align 3
.L674:
	mov	rsi, -1
	mov	rdi, rbx
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+600]]
	jmp	.L666
	.p2align 4,,10
	.p2align 3
.L667:
	add	rax, QWORD PTR [rbx]
	mov	ecx, 23644
	mov	WORD PTR [rax], cx
.L673:
	mov	rax, QWORD PTR 8[rbx]
	lea	rdx, 2[rax]
	mov	QWORD PTR 8[rbx], rdx
	mov	rdx, QWORD PTR [rbx]
	mov	BYTE PTR 2[rdx+rax], 0
	jmp	.L666
	.p2align 4,,10
	.p2align 3
.L671:
	add	rax, QWORD PTR [rbx]
	mov	edx, 8796
	mov	WORD PTR [rax], dx
	jmp	.L673
	.p2align 4,,10
	.p2align 3
.L686:
	xor	edi, edi
	lea	rdx, .LC152[rip]
	mov	esi, 16
	xor	eax, eax
	call	g_log@PLT
	add	rsp, 8
	.cfi_remember_state
	.cfi_def_cfa_offset 56
	mov	rdi, rbp
	pop	rbx
	.cfi_restore 3
	.cfi_def_cfa_offset 48
	pop	rbp
	.cfi_restore 6
	.cfi_def_cfa_offset 40
	pop	r12
	.cfi_restore 12
	.cfi_def_cfa_offset 32
	pop	r13
	.cfi_restore 13
	.cfi_def_cfa_offset 24
	pop	r14
	.cfi_restore 14
	.cfi_def_cfa_offset 16
	pop	r15
	.cfi_restore 15
	.cfi_def_cfa_offset 8
	jmp	g_free@PLT
	.p2align 4,,10
	.p2align 3
.L660:
	.cfi_restore_state
	lea	rdx, .LC155[rip]
	mov	esi, 16
	xor	edi, edi
	xor	eax, eax
	call	g_log@PLT
	jmp	.L661
	.p2align 4,,10
	.p2align 3
.L690:
	mov	edx, 2
	lea	rsi, .LC157[rip]
	xor	edi, edi
	call	g_string_append_len@PLT
	jmp	.L666
	.p2align 4,,10
	.p2align 3
.L688:
	mov	edx, 2
	lea	rsi, .LC156[rip]
	xor	edi, edi
	call	g_string_append_len@PLT
	jmp	.L666
	.p2align 4,,10
	.p2align 3
.L685:
	.cfi_def_cfa_offset 8
	.cfi_restore 3
	.cfi_restore 6
	.cfi_restore 12
	.cfi_restore 13
	.cfi_restore 14
	.cfi_restore 15
	lea	rdx, .LC152[rip]
	mov	esi, 16
	xor	edi, edi
	xor	eax, eax
	jmp	g_log@PLT
	.cfi_endproc
.LFE2810:
	.size	real_swank_session_global_eval, .-real_swank_session_global_eval
	.section	.rodata.str1.1
.LC159:
	.string	"Evaluate.on_evaluate_global"
	.section	.rodata.str1.8
	.align 8
.LC160:
	.string	"Evaluate.on_evaluate_global: buffer_global is NULL"
	.align 8
.LC161:
	.string	"Evaluate.on_evaluate_global: nothing to evaluate on the current line"
	.text
	.p2align 4
	.globl	on_evaluate_global
	.type	on_evaluate_global, @function
on_evaluate_global:
.LFB2739:
	.cfi_startproc
	endbr64
	push	r12
	.cfi_def_cfa_offset 16
	.cfi_offset 12, -16
	lea	rdx, .LC159[rip]
	mov	esi, 128
	xor	edi, edi
	push	rbp
	.cfi_def_cfa_offset 24
	.cfi_offset 6, -24
	push	rbx
	.cfi_def_cfa_offset 32
	.cfi_offset 3, -32
	sub	rsp, 256
	.cfi_def_cfa_offset 288
	mov	rax, QWORD PTR fs:40
	mov	QWORD PTR 248[rsp], rax
	xor	eax, eax
	call	g_log@PLT
	mov	rbx, QWORD PTR buffer_global[rip]
	test	rbx, rbx
	je	.L704
	mov	rdi, rbx
	xor	eax, eax
	lea	rbp, 80[rsp]
	call	[QWORD PTR reloc_functions[rip+664]]
	mov	rdi, rbx
	mov	rsi, rsp
	lea	r12, 160[rsp]
	mov	rdx, rax
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+608]]
	movdqa	xmm3, XMMWORD PTR 32[rsp]
	xor	esi, esi
	mov	rdi, rbp
	movdqa	xmm4, XMMWORD PTR 48[rsp]
	movdqa	xmm5, XMMWORD PTR 64[rsp]
	xor	eax, eax
	movdqa	xmm1, XMMWORD PTR [rsp]
	movdqa	xmm2, XMMWORD PTR 16[rsp]
	movaps	XMMWORD PTR 112[rsp], xmm3
	movaps	XMMWORD PTR 128[rsp], xmm4
	movaps	XMMWORD PTR 144[rsp], xmm5
	movaps	XMMWORD PTR 80[rsp], xmm1
	movaps	XMMWORD PTR 96[rsp], xmm2
	call	[QWORD PTR reloc_functions[rip+568]]
	movdqa	xmm6, XMMWORD PTR 80[rsp]
	mov	rdi, r12
	xor	eax, eax
	movdqa	xmm7, XMMWORD PTR 96[rsp]
	movdqa	xmm0, XMMWORD PTR 112[rsp]
	movdqa	xmm1, XMMWORD PTR 128[rsp]
	movaps	XMMWORD PTR 160[rsp], xmm6
	movdqa	xmm2, XMMWORD PTR 144[rsp]
	movaps	XMMWORD PTR 176[rsp], xmm7
	movaps	XMMWORD PTR 192[rsp], xmm0
	movaps	XMMWORD PTR 208[rsp], xmm1
	movaps	XMMWORD PTR 224[rsp], xmm2
	call	[QWORD PTR reloc_functions[rip+520]]
	mov	rdi, rbx
	xor	ecx, ecx
	mov	rdx, r12
	mov	rsi, rbp
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+352]]
	mov	rbx, rax
	test	rax, rax
	je	.L694
	cmp	BYTE PTR [rax], 0
	je	.L694
	mov	edi, 40
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+504]]
	mov	rdi, rbx
	mov	rbp, rax
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+448]]
	mov	rdi, rbx
	call	g_strdup@PLT
	mov	QWORD PTR 8[rbp], 0
	pxor	xmm0, xmm0
	mov	rdi, rbp
	mov	QWORD PTR 0[rbp], rax
	mov	QWORD PTR 32[rbp], 0
	movups	XMMWORD PTR 16[rbp], xmm0
	call	real_swank_session_global_eval
	mov	rdi, rbx
	call	g_free@PLT
.L691:
	mov	rax, QWORD PTR 248[rsp]
	sub	rax, QWORD PTR fs:40
	jne	.L703
	add	rsp, 256
	.cfi_remember_state
	.cfi_def_cfa_offset 32
	pop	rbx
	.cfi_def_cfa_offset 24
	pop	rbp
	.cfi_def_cfa_offset 16
	pop	r12
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L694:
	.cfi_restore_state
	xor	edi, edi
	lea	rdx, .LC161[rip]
	mov	esi, 128
	xor	eax, eax
	call	g_log@PLT
	mov	rdi, rbx
	call	g_free@PLT
	jmp	.L691
	.p2align 4,,10
	.p2align 3
.L704:
	mov	rax, QWORD PTR 248[rsp]
	sub	rax, QWORD PTR fs:40
	jne	.L703
	add	rsp, 256
	.cfi_remember_state
	.cfi_def_cfa_offset 32
	mov	esi, 16
	xor	edi, edi
	xor	eax, eax
	pop	rbx
	.cfi_def_cfa_offset 24
	lea	rdx, .LC160[rip]
	pop	rbp
	.cfi_def_cfa_offset 16
	pop	r12
	.cfi_def_cfa_offset 8
	jmp	g_log@PLT
.L703:
	.cfi_restore_state
	call	__stack_chk_fail@PLT
	.cfi_endproc
.LFE2739:
	.size	on_evaluate_global, .-on_evaluate_global
	.p2align 4
	.type	on_key_press_handler, @function
on_key_press_handler:
.LFB2824:
	.cfi_startproc
	endbr64
	movabs	rax, -4294967288
	and	rax, QWORD PTR 24[rsi]
	movabs	rcx, 280431299657736
	cmp	rax, rcx
	je	.L713
	xor	eax, eax
	ret
	.p2align 4,,10
	.p2align 3
.L713:
	sub	rsp, 8
	.cfi_def_cfa_offset 16
	xor	eax, eax
	call	on_evaluate_global
	mov	eax, 1
	add	rsp, 8
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE2824:
	.size	on_key_press_handler, .-on_key_press_handler
	.p2align 4
	.globl	real_swank_session_cleanup_globals
	.type	real_swank_session_cleanup_globals, @function
real_swank_session_cleanup_globals:
.LFB2816:
	.cfi_startproc
	endbr64
	xor	edi, edi
	sub	rsp, 8
	.cfi_def_cfa_offset 16
	lea	rdx, .LC150[rip]
	xor	eax, eax
	mov	esi, 128
	call	g_log@PLT
	mov	rdi, QWORD PTR g_swank_session_interactions_table[rip]
	test	rdi, rdi
	je	.L715
	call	g_hash_table_destroy@PLT
	mov	QWORD PTR g_swank_session_interactions_table[rip], 0
.L715:
	mov	DWORD PTR g_swank_session_started[rip], 0
	mov	DWORD PTR g_swank_session_next_tag[rip], 1
	add	rsp, 8
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE2816:
	.size	real_swank_session_cleanup_globals, .-real_swank_session_cleanup_globals
	.p2align 4
	.globl	main_get_filename
	.type	main_get_filename, @function
main_get_filename:
.LFB2817:
	.cfi_startproc
	endbr64
	mov	rax, QWORD PTR filename_global[rip]
	ret
	.cfi_endproc
.LFE2817:
	.size	main_get_filename, .-main_get_filename
	.section	.rodata.str1.1
.LC162:
	.string	"main_set_filename %s"
	.text
	.p2align 4
	.globl	main_set_filename
	.type	main_set_filename, @function
main_set_filename:
.LFB2818:
	.cfi_startproc
	endbr64
	push	rbx
	.cfi_def_cfa_offset 16
	.cfi_offset 3, -16
	mov	rbx, rdi
	test	rdi, rdi
	je	.L722
	mov	rcx, rdi
	lea	rdx, .LC162[rip]
	mov	esi, 128
	xor	edi, edi
	xor	eax, eax
	call	g_log@PLT
	mov	rdi, rbx
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+448]]
	mov	rdi, rbx
	call	g_strdup@PLT
	mov	rbx, rax
.L723:
	mov	rdi, QWORD PTR filename_global[rip]
	call	g_free@PLT
	mov	QWORD PTR filename_global[rip], rbx
	pop	rbx
	.cfi_remember_state
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L722:
	.cfi_restore_state
	lea	rcx, .LC57[rip]
	mov	esi, 128
	xor	edi, edi
	xor	eax, eax
	lea	rdx, .LC162[rip]
	call	g_log@PLT
	jmp	.L723
	.cfi_endproc
.LFE2818:
	.size	main_set_filename, .-main_set_filename
	.section	.rodata.str1.8
	.align 8
.LC163:
	.string	"simple_file_open_global: buffer_global is NULL. Cannot open file.\n"
	.section	.rodata.str1.1
.LC164:
	.string	"_Open"
.LC165:
	.string	"Open File"
.LC166:
	.string	"Failed to open %s (errno %d)\n"
.LC167:
	.string	"Failed to stat %s (errno %d)\n"
.LC168:
	.string	"Not a regular file: %s\n"
	.section	.rodata.str1.8
	.align 8
.LC169:
	.string	"Failed to allocate memory for file content.\n"
	.section	.rodata.str1.1
.LC170:
	.string	"Error reading %s (errno %d)\n"
	.text
	.p2align 4
	.globl	simple_file_open_global
	.type	simple_file_open_global, @function
simple_file_open_global:
.LFB2747:
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
	sub	rsp, 184
	.cfi_def_cfa_offset 240
	mov	rax, QWORD PTR fs:40
	mov	QWORD PTR 168[rsp], rax
	xor	eax, eax
	test	rdi, rdi
	je	.L726
	call	[QWORD PTR reloc_functions[rip+768]]
	mov	rdi, rax
.L726:
	cmp	QWORD PTR buffer_global[rip], 0
	je	.L761
	push	0
	.cfi_def_cfa_offset 248
	lea	rcx, .LC82[rip]
	xor	edx, edx
	mov	rsi, rdi
	push	-3
	.cfi_def_cfa_offset 256
	lea	r9, .LC164[rip]
	mov	r8d, -6
	xor	eax, eax
	lea	rdi, .LC165[rip]
	call	gtk_file_chooser_dialog_new@PLT
	mov	rdi, rax
	mov	rbx, rax
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+152]]
	pop	rdx
	.cfi_def_cfa_offset 248
	pop	rcx
	.cfi_def_cfa_offset 240
	cmp	eax, -3
	je	.L762
.L729:
	mov	rax, QWORD PTR 168[rsp]
	sub	rax, QWORD PTR fs:40
	jne	.L760
	add	rsp, 184
	.cfi_remember_state
	.cfi_def_cfa_offset 56
	mov	rdi, rbx
	xor	eax, eax
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
	jmp	[QWORD PTR reloc_functions[rip+368]]
	.p2align 4,,10
	.p2align 3
.L762:
	.cfi_restore_state
	mov	rdi, rbx
	xor	eax, eax
	xor	ebp, ebp
	call	[QWORD PTR reloc_functions[rip+160]]
	mov	rdi, rax
	mov	r12, rax
	call	main_set_filename
#APP
# 24 "../common/syscalls.h" 1
	mov rdi, r12
	mov rsi, rbp
	mov rdx, rbp
	mov rax, $2
	syscall
# 0 "" 2
#NO_APP
	mov	rbp, rax
	test	rax, rax
	js	.L763
	cmp	eax, -1
	je	.L764
	movsx	r13, eax
	lea	rbp, 16[rsp]
#APP
# 144 "../common/syscalls.h" 1
	mov rdi, r13
	mov rsi, rbp
	mov rax, $5
	syscall
# 0 "" 2
#NO_APP
	mov	rbp, rax
	test	rax, rax
	js	.L765
	mov	eax, DWORD PTR 40[rsp]
	and	eax, 61440
	cmp	eax, 32768
	je	.L757
	mov	rsi, r12
	lea	rdi, .LC168[rip]
	xor	eax, eax
	call	g_printerr@PLT
#APP
# 117 "../common/syscalls.h" 1
	mov rdi, r13
	mov rax, $3
	syscall
# 0 "" 2
#NO_APP
	mov	r13, rax
	test	rax, rax
	js	.L759
.L733:
	mov	rdi, r12
	call	g_free@PLT
	jmp	.L729
	.p2align 4,,10
	.p2align 3
.L764:
	call	__errno_location@PLT
	mov	edx, DWORD PTR [rax]
.L731:
	mov	rsi, r12
	lea	rdi, .LC166[rip]
	xor	eax, eax
	call	g_printerr@PLT
	jmp	.L733
	.p2align 4,,10
	.p2align 3
.L761:
	mov	rax, QWORD PTR 168[rsp]
	sub	rax, QWORD PTR fs:40
	jne	.L760
	add	rsp, 184
	.cfi_remember_state
	.cfi_def_cfa_offset 56
	lea	rdi, .LC163[rip]
	xor	eax, eax
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
	jmp	g_printerr@PLT
	.p2align 4,,10
	.p2align 3
.L757:
	.cfi_restore_state
	mov	r9, QWORD PTR 64[rsp]
	xor	eax, eax
	mov	QWORD PTR 8[rsp], r9
	lea	rdi, 1[r9]
	call	[QWORD PTR reloc_functions[rip+72]]
	mov	r14, rax
	test	rax, rax
	je	.L740
	mov	r9, QWORD PTR 8[rsp]
	xor	r8d, r8d
	mov	r15, rax
	test	r9, r9
	jg	.L741
	.p2align 4,,10
	.p2align 3
.L742:
	mov	BYTE PTR [r15], 0
#APP
# 117 "../common/syscalls.h" 1
	mov rdi, r13
	mov rax, $3
	syscall
# 0 "" 2
#NO_APP
	mov	r13, rax
	test	rax, rax
	js	.L766
.L745:
	mov	rdi, QWORD PTR buffer_global[rip]
	mov	edx, -1
	mov	rsi, r14
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+360]]
	mov	rdi, r14
	call	g_free@PLT
	jmp	.L733
	.p2align 4,,10
	.p2align 3
.L769:
	add	r8, rax
	cmp	r9, r8
	jle	.L767
.L741:
	mov	rbp, r9
	lea	r15, [r14+r8]
	sub	rbp, r8
#APP
# 55 "../common/syscalls.h" 1
	mov rdi, r13
	mov rsi, r15
	mov rdx, rbp
	mov rax, $0
	syscall
# 0 "" 2
#NO_APP
	mov	rbp, rax
	test	rax, rax
	js	.L768
	jne	.L769
	jmp	.L742
.L766:
	call	__errno_location@PLT
	mov	ebp, r13d
	neg	ebp
	mov	DWORD PTR [rax], ebp
	jmp	.L745
	.p2align 4,,10
	.p2align 3
.L765:
	call	__errno_location@PLT
	mov	edx, ebp
	mov	rsi, r12
	lea	rdi, .LC167[rip]
	neg	edx
	mov	r14, rax
	mov	DWORD PTR [rax], edx
	xor	eax, eax
	call	g_printerr@PLT
#APP
# 117 "../common/syscalls.h" 1
	mov rdi, r13
	mov rax, $3
	syscall
# 0 "" 2
#NO_APP
	test	rax, rax
	jns	.L733
	neg	eax
	mov	DWORD PTR [r14], eax
	jmp	.L733
	.p2align 4,,10
	.p2align 3
.L740:
	lea	rdi, .LC169[rip]
	xor	eax, eax
	call	g_printerr@PLT
#APP
# 117 "../common/syscalls.h" 1
	mov rdi, r13
	mov rax, $3
	syscall
# 0 "" 2
#NO_APP
	mov	r13, rax
	test	rax, rax
	jns	.L733
.L759:
	call	__errno_location@PLT
	mov	ebp, r13d
	neg	ebp
	mov	DWORD PTR [rax], ebp
	jmp	.L733
	.p2align 4,,10
	.p2align 3
.L767:
	lea	r15, [r14+r8]
	jmp	.L742
.L768:
	call	__errno_location@PLT
	mov	edx, ebp
	mov	rsi, r12
	lea	rdi, .LC170[rip]
	neg	edx
	mov	DWORD PTR [rax], edx
	xor	eax, eax
	call	g_printerr@PLT
	jmp	.L742
.L763:
	call	__errno_location@PLT
	mov	edx, ebp
	neg	edx
	mov	DWORD PTR [rax], edx
	jmp	.L731
.L760:
	call	__stack_chk_fail@PLT
	.cfi_endproc
.LFE2747:
	.size	simple_file_open_global, .-simple_file_open_global
	.section	.rodata.str1.8
	.align 8
.LC171:
	.string	"simple_file_save_global: buffer_global is NULL. Cannot save file.\n"
	.section	.rodata.str1.1
.LC172:
	.string	"_Save"
.LC173:
	.string	"Save File As"
	.section	.rodata.str1.8
	.align 8
.LC174:
	.string	"Failed to open file %s for writing (errno %d)\n"
	.align 8
.LC175:
	.string	"Error writing to %s (errno %d)\n"
	.section	.rodata.str1.1
.LC176:
	.string	"Error closing %s (errno %d)\n"
	.text
	.p2align 4
	.globl	simple_file_save_global
	.type	simple_file_save_global, @function
simple_file_save_global:
.LFB2748:
	.cfi_startproc
	endbr64
	push	r14
	.cfi_def_cfa_offset 16
	.cfi_offset 14, -16
	push	r13
	.cfi_def_cfa_offset 24
	.cfi_offset 13, -24
	push	r12
	.cfi_def_cfa_offset 32
	.cfi_offset 12, -32
	push	rbp
	.cfi_def_cfa_offset 40
	.cfi_offset 6, -40
	push	rbx
	.cfi_def_cfa_offset 48
	.cfi_offset 3, -48
	sub	rsp, 176
	.cfi_def_cfa_offset 224
	mov	rax, QWORD PTR fs:40
	mov	QWORD PTR 168[rsp], rax
	xor	eax, eax
	test	rdi, rdi
	je	.L771
	call	[QWORD PTR reloc_functions[rip+768]]
	mov	rdi, rax
.L771:
	cmp	QWORD PTR buffer_global[rip], 0
	je	.L804
	mov	rbx, QWORD PTR filename_global[rip]
	test	rbx, rbx
	je	.L805
	mov	rdi, rbx
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+448]]
	mov	rdi, rbx
	call	g_strdup@PLT
	mov	rbp, rax
.L778:
	lea	r12, 80[rsp]
	mov	rbx, rsp
	mov	rdi, QWORD PTR buffer_global[rip]
	xor	eax, eax
	mov	rsi, rbx
	call	[QWORD PTR reloc_functions[rip+344]]
	mov	rsi, r12
	mov	rdi, QWORD PTR buffer_global[rip]
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+336]]
	mov	rdx, r12
	xor	ecx, ecx
	mov	rsi, rbx
	mov	rdi, QWORD PTR buffer_global[rip]
	xor	eax, eax
	mov	r12d, 577
	call	[QWORD PTR reloc_functions[rip+352]]
	mov	r13, rax
	mov	eax, 438
#APP
# 24 "../common/syscalls.h" 1
	mov rdi, rbp
	mov rsi, r12
	mov rdx, rax
	mov rax, $2
	syscall
# 0 "" 2
#NO_APP
	mov	r12, rax
	test	rax, rax
	js	.L806
	cmp	eax, -1
	je	.L807
	mov	rdi, r13
	xor	eax, eax
	movsx	r12, r12d
	call	[QWORD PTR reloc_functions[rip+448]]
	mov	r9, rax
	test	rax, rax
	je	.L782
	xor	r8d, r8d
	xor	edx, edx
	.p2align 4,,10
	.p2align 3
.L786:
	mov	rax, r9
	lea	rbx, 0[r13+rdx]
	sub	rax, rdx
#APP
# 86 "../common/syscalls.h" 1
	mov rdi, r12
	mov rsi, rbx
	mov rdx, rax
	mov rax, $1
	syscall
# 0 "" 2
#NO_APP
	mov	rbx, rax
	test	rax, rax
	js	.L808
	add	r8, rax
	mov	rdx, r8
	cmp	r8, r9
	jb	.L786
.L782:
#APP
# 117 "../common/syscalls.h" 1
	mov rdi, r12
	mov rax, $3
	syscall
# 0 "" 2
#NO_APP
	mov	r12, rax
	test	rax, rax
	js	.L809
.L787:
	mov	rdi, r13
	call	g_free@PLT
	mov	rdi, rbp
	call	g_free@PLT
.L770:
	mov	rax, QWORD PTR 168[rsp]
	sub	rax, QWORD PTR fs:40
	jne	.L803
	add	rsp, 176
	.cfi_remember_state
	.cfi_def_cfa_offset 48
	pop	rbx
	.cfi_def_cfa_offset 40
	pop	rbp
	.cfi_def_cfa_offset 32
	pop	r12
	.cfi_def_cfa_offset 24
	pop	r13
	.cfi_def_cfa_offset 16
	pop	r14
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L805:
	.cfi_restore_state
	push	0
	.cfi_def_cfa_offset 232
	lea	rcx, .LC82[rip]
	mov	edx, 1
	xor	eax, eax
	push	-3
	.cfi_def_cfa_offset 240
	lea	r9, .LC172[rip]
	mov	r8d, -6
	mov	rsi, rdi
	lea	rdi, .LC173[rip]
	call	gtk_file_chooser_dialog_new@PLT
	mov	esi, 1
	mov	rbx, rax
	mov	rdi, rax
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+168]]
	mov	rdi, rbx
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+152]]
	pop	rdx
	.cfi_def_cfa_offset 232
	pop	rcx
	.cfi_def_cfa_offset 224
	cmp	eax, -3
	je	.L775
	mov	rax, QWORD PTR 168[rsp]
	sub	rax, QWORD PTR fs:40
	jne	.L803
	add	rsp, 176
	.cfi_remember_state
	.cfi_def_cfa_offset 48
	mov	rdi, rbx
	xor	eax, eax
	pop	rbx
	.cfi_def_cfa_offset 40
	pop	rbp
	.cfi_def_cfa_offset 32
	pop	r12
	.cfi_def_cfa_offset 24
	pop	r13
	.cfi_def_cfa_offset 16
	pop	r14
	.cfi_def_cfa_offset 8
	jmp	[QWORD PTR reloc_functions[rip+368]]
	.p2align 4,,10
	.p2align 3
.L804:
	.cfi_restore_state
	mov	rax, QWORD PTR 168[rsp]
	sub	rax, QWORD PTR fs:40
	jne	.L803
	add	rsp, 176
	.cfi_remember_state
	.cfi_def_cfa_offset 48
	lea	rdi, .LC171[rip]
	xor	eax, eax
	pop	rbx
	.cfi_def_cfa_offset 40
	pop	rbp
	.cfi_def_cfa_offset 32
	pop	r12
	.cfi_def_cfa_offset 24
	pop	r13
	.cfi_def_cfa_offset 16
	pop	r14
	.cfi_def_cfa_offset 8
	jmp	g_printerr@PLT
	.p2align 4,,10
	.p2align 3
.L807:
	.cfi_restore_state
	call	__errno_location@PLT
	mov	edx, DWORD PTR [rax]
.L780:
	mov	rsi, rbp
	lea	rdi, .LC174[rip]
.L801:
	xor	eax, eax
	call	g_printerr@PLT
	jmp	.L787
.L809:
	call	__errno_location@PLT
	mov	edx, r12d
	mov	rsi, rbp
	lea	rdi, .LC176[rip]
	neg	edx
	mov	DWORD PTR [rax], edx
	jmp	.L801
.L775:
	mov	rdi, rbx
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+160]]
	mov	rbp, rax
	mov	rdi, rax
	call	main_set_filename
	xor	eax, eax
	mov	rdi, rbx
	call	[QWORD PTR reloc_functions[rip+368]]
	test	rbp, rbp
	jne	.L778
	jmp	.L770
	.p2align 4,,10
	.p2align 3
.L808:
	call	__errno_location@PLT
	mov	edx, ebx
	mov	rsi, rbp
	lea	rdi, .LC175[rip]
	neg	edx
	mov	r14, rax
	mov	DWORD PTR [rax], edx
	xor	eax, eax
	call	g_printerr@PLT
#APP
# 117 "../common/syscalls.h" 1
	mov rdi, r12
	mov rax, $3
	syscall
# 0 "" 2
#NO_APP
	test	rax, rax
	jns	.L787
	neg	eax
	mov	DWORD PTR [r14], eax
	jmp	.L787
.L806:
	call	__errno_location@PLT
	mov	edx, r12d
	neg	edx
	mov	DWORD PTR [rax], edx
	jmp	.L780
.L803:
	call	__stack_chk_fail@PLT
	.cfi_endproc
.LFE2748:
	.size	simple_file_save_global, .-simple_file_save_global
	.p2align 4
	.globl	simple_file_saveas_global
	.type	simple_file_saveas_global, @function
simple_file_saveas_global:
.LFB2749:
	.cfi_startproc
	endbr64
	push	rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	mov	rbp, rdi
	push	rbx
	.cfi_def_cfa_offset 24
	.cfi_offset 3, -24
	sub	rsp, 8
	.cfi_def_cfa_offset 32
	mov	rbx, QWORD PTR filename_global[rip]
	test	rbx, rbx
	je	.L811
	mov	rdi, rbx
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+448]]
	mov	rdi, rbx
	call	g_strdup@PLT
	lea	rcx, .LC57[rip]
	lea	rdx, .LC162[rip]
	xor	edi, edi
	mov	esi, 128
	mov	rbx, rax
	xor	eax, eax
	call	g_log@PLT
	mov	rdi, QWORD PTR filename_global[rip]
	call	g_free@PLT
	mov	rdi, rbp
	mov	QWORD PTR filename_global[rip], 0
	call	simple_file_save_global
	cmp	QWORD PTR filename_global[rip], 0
	jne	.L813
	test	rbx, rbx
	jne	.L821
.L813:
	add	rsp, 8
	.cfi_remember_state
	.cfi_def_cfa_offset 24
	mov	rdi, rbx
	pop	rbx
	.cfi_def_cfa_offset 16
	pop	rbp
	.cfi_def_cfa_offset 8
	jmp	g_free@PLT
	.p2align 4,,10
	.p2align 3
.L811:
	.cfi_restore_state
	lea	rcx, .LC57[rip]
	mov	esi, 128
	xor	eax, eax
	xor	edi, edi
	lea	rdx, .LC162[rip]
	call	g_log@PLT
	mov	rdi, QWORD PTR filename_global[rip]
	call	g_free@PLT
	mov	rdi, rbp
	mov	QWORD PTR filename_global[rip], 0
	call	simple_file_save_global
	add	rsp, 8
	.cfi_remember_state
	.cfi_def_cfa_offset 24
	mov	rdi, rbx
	pop	rbx
	.cfi_def_cfa_offset 16
	pop	rbp
	.cfi_def_cfa_offset 8
	jmp	g_free@PLT
	.p2align 4,,10
	.p2align 3
.L821:
	.cfi_restore_state
	mov	rdi, rbx
	call	main_set_filename
	add	rsp, 8
	.cfi_def_cfa_offset 24
	mov	rdi, rbx
	pop	rbx
	.cfi_def_cfa_offset 16
	pop	rbp
	.cfi_def_cfa_offset 8
	jmp	g_free@PLT
	.cfi_endproc
.LFE2749:
	.size	simple_file_saveas_global, .-simple_file_saveas_global
	.p2align 4
	.globl	main_get_source_buffer
	.type	main_get_source_buffer, @function
main_get_source_buffer:
.LFB2819:
	.cfi_startproc
	endbr64
	mov	rax, QWORD PTR buffer_global[rip]
	ret
	.cfi_endproc
.LFE2819:
	.size	main_get_source_buffer, .-main_get_source_buffer
	.section	.rodata.str1.1
.LC177:
	.string	"Main.main"
.LC178:
	.string	"delete-event"
.LC179:
	.string	"commonlisp"
.LC180:
	.string	"key-press-event"
.LC181:
	.string	"File"
.LC182:
	.string	"Open\342\200\246"
.LC183:
	.string	"Save"
.LC184:
	.string	"Save as\342\200\246"
.LC185:
	.string	"Preferences\342\200\246"
.LC186:
	.string	"Quit"
.LC187:
	.string	"activate"
	.section	.text.startup,"ax",@progbits
	.p2align 4
	.globl	main
	.type	main, @function
main:
.LFB2825:
	.cfi_startproc
	endbr64
	push	r15
	.cfi_def_cfa_offset 16
	.cfi_offset 15, -16
	lea	rdx, .LC177[rip]
	xor	eax, eax
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
	lea	rbp, reloc_functions[rip]
	push	rbx
	.cfi_def_cfa_offset 56
	.cfi_offset 3, -56
	lea	r12, 824[rbp]
	lea	rbx, reloc_names[rip]
	sub	rsp, 56
	.cfi_def_cfa_offset 112
	mov	DWORD PTR 44[rsp], edi
	xor	edi, edi
	mov	QWORD PTR 32[rsp], rsi
	mov	esi, 128
	call	g_log@PLT
	.p2align 4,,10
	.p2align 3
.L824:
	lea	rsi, 1[rbx]
	xor	edi, edi
	add	rbp, 8
	call	dlsym@PLT
	mov	QWORD PTR -8[rbp], rax
	movsx	rax, BYTE PTR [rbx]
	add	rbx, rax
	cmp	rbp, r12
	jne	.L824
	lea	rsi, 32[rsp]
	lea	rdi, 44[rsp]
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+208]]
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+16]]
	mov	rdi, rax
	call	preferences_init_globals
	mov	rdi, QWORD PTR g_pref_sdk[rip]
	call	real_process_init_globals
	xor	eax, eax
	call	real_swank_process_init_globals
	xor	eax, eax
	call	real_swank_process_init_globals
	xor	eax, eax
	call	real_swank_session_init_globals
	xor	edi, edi
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+416]]
	mov	edx, 600
	mov	esi, 800
	mov	rbp, rax
	mov	rdi, rax
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+424]]
	xor	r9d, r9d
	xor	r8d, r8d
	xor	ecx, ecx
	lea	rdx, quit_delete_event_handler[rip]
	lea	rsi, .LC178[rip]
	mov	rdi, rbp
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+80]]
	xor	esi, esi
	xor	edi, edi
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+280]]
	mov	edx, 1
	mov	esi, 1
	mov	rdi, rax
	mov	r12, rax
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+288]]
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+304]]
	lea	rsi, .LC179[rip]
	mov	rdi, rax
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+312]]
	mov	rdi, rax
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+296]]
	mov	rdi, rax
	mov	QWORD PTR buffer_global[rip], rax
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+320]]
	mov	esi, 1
	mov	rbx, rax
	mov	rdi, rax
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+328]]
	mov	rsi, rbx
	mov	rdi, r12
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+136]]
	xor	r9d, r9d
	xor	r8d, r8d
	xor	ecx, ecx
	lea	rdx, on_key_press_handler[rip]
	lea	rsi, .LC180[rip]
	mov	rdi, rbx
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+80]]
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+240]]
	mov	QWORD PTR 24[rsp], rax
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+264]]
	lea	rdi, .LC181[rip]
	mov	rbx, rax
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+248]]
	lea	rdi, .LC182[rip]
	mov	QWORD PTR 16[rsp], rax
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+248]]
	lea	rdi, .LC183[rip]
	mov	r15, rax
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+248]]
	lea	rdi, .LC184[rip]
	mov	r14, rax
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+248]]
	lea	rdi, .LC185[rip]
	mov	r13, rax
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+248]]
	lea	rdi, .LC186[rip]
	mov	QWORD PTR 8[rsp], rax
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+248]]
	mov	rsi, rbx
	mov	rdi, QWORD PTR 16[rsp]
	mov	QWORD PTR [rsp], rax
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+256]]
	mov	rdi, rbx
	mov	rsi, r15
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+272]]
	mov	rdi, rbx
	mov	rsi, r14
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+272]]
	mov	rsi, r13
	mov	rdi, rbx
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+272]]
	mov	rdi, rbx
	mov	rsi, QWORD PTR 8[rsp]
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+272]]
	mov	rdi, rbx
	mov	rsi, QWORD PTR [rsp]
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+272]]
	lea	rbx, .LC187[rip]
	mov	rsi, QWORD PTR 16[rsp]
	xor	eax, eax
	mov	rdi, QWORD PTR 24[rsp]
	call	[QWORD PTR reloc_functions[rip+272]]
	xor	r9d, r9d
	xor	r8d, r8d
	mov	rsi, rbx
	mov	rcx, r15
	lea	rdx, simple_file_open_global[rip]
	mov	rdi, r15
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+80]]
	xor	r9d, r9d
	xor	r8d, r8d
	mov	rsi, rbx
	mov	rcx, r14
	lea	rdx, simple_file_save_global[rip]
	mov	rdi, r14
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+80]]
	xor	r9d, r9d
	xor	r8d, r8d
	mov	rcx, r13
	mov	rsi, rbx
	mov	rdi, r13
	lea	rdx, simple_file_saveas_global[rip]
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+80]]
	xor	r9d, r9d
	xor	r8d, r8d
	xor	ecx, ecx
	mov	rsi, rbx
	mov	rdi, QWORD PTR 8[rsp]
	lea	rdx, on_preferences_global[rip]
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+80]]
	xor	r9d, r9d
	xor	r8d, r8d
	mov	rsi, rbx
	xor	ecx, ecx
	lea	rdx, quit_menu_item_handler[rip]
	mov	rdi, QWORD PTR [rsp]
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+80]]
	xor	eax, eax
	call	interactions_view_new
	mov	edi, 1
	mov	QWORD PTR interactions_view_global[rip], rax
	call	gtk_paned_new@PLT
	mov	ecx, 1
	mov	edx, 1
	mov	rsi, r12
	mov	rdi, rax
	mov	r13, rax
	call	gtk_paned_pack1@PLT
	xor	edx, edx
	mov	ecx, 1
	mov	rdi, r13
	mov	rsi, QWORD PTR interactions_view_global[rip]
	call	gtk_paned_pack2@PLT
	xor	esi, esi
	mov	edi, 1
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+96]]
	xor	r8d, r8d
	xor	ecx, ecx
	xor	edx, edx
	mov	rbx, rax
	mov	rdi, rax
	mov	rsi, QWORD PTR 24[rsp]
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+104]]
	xor	r8d, r8d
	mov	ecx, 1
	mov	rsi, r13
	mov	edx, 1
	mov	rdi, rbx
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+104]]
	mov	rsi, rbx
	mov	rdi, rbp
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+136]]
	mov	rdi, rbp
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+408]]
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip+232]]
	mov	rdi, QWORD PTR filename_global[rip]
	call	g_free@PLT
	xor	eax, eax
	call	preferences_cleanup_globals
	xor	eax, eax
	call	real_swank_session_cleanup_globals
	xor	eax, eax
	call	real_process_cleanup_globals
	xor	eax, eax
	call	real_swank_process_cleanup_globals
	xor	edi, edi
	xor	eax, eax
	call	[QWORD PTR reloc_functions[rip]]
	.cfi_endproc
.LFE2825:
	.size	main, .-main
	.section	.rodata
	.align 32
	.type	__func__.0, @object
	.size	__func__.0, 37
__func__.0:
	.string	"interactions_view_update_interaction"
	.align 32
	.type	__func__.1, @object
	.size	__func__.1, 34
__func__.1:
	.string	"interactions_view_add_interaction"
	.local	static_g_define_type_id.2
	.comm	static_g_define_type_id.2,8,8
	.globl	interactions_view_global
	.bss
	.align 8
	.type	interactions_view_global, @object
	.size	interactions_view_global, 8
interactions_view_global:
	.zero	8
	.globl	filename_global
	.align 8
	.type	filename_global, @object
	.size	filename_global, 8
filename_global:
	.zero	8
	.globl	buffer_global
	.align 8
	.type	buffer_global, @object
	.size	buffer_global, 8
buffer_global:
	.zero	8
	.globl	reloc_functions
	.align 32
	.type	reloc_functions, @object
	.size	reloc_functions, 824
reloc_functions:
	.zero	824
	.globl	reloc_names
	.data
	.align 32
	.type	reloc_names, @object
	.size	reloc_names, 2322
reloc_names:
	.byte	6
	.string	"exit"
	.byte	15
	.string	"g_clear_error"
	.byte	23
	.string	"g_get_user_config_dir"
	.byte	24
	.string	"g_intern_static_string"
	.byte	17
	.string	"g_key_file_free"
	.byte	23
	.string	"g_key_file_get_string"
	.byte	27
	.string	"g_key_file_load_from_file"
	.byte	16
	.string	"g_key_file_new"
	.byte	23
	.string	"g_key_file_set_string"
	.byte	10
	.string	"g_malloc"
	.byte	23
	.string	"g_signal_connect_data"
	.byte	11
	.string	"g_strcmp0"
	.byte	13
	.string	"gtk_box_new"
	.byte	20
	.string	"gtk_box_pack_start"
	.byte	29
	.string	"gtk_combo_box_set_active_id"
	.byte	27
	.string	"gtk_combo_box_text_append"
	.byte	24
	.string	"gtk_combo_box_text_new"
	.byte	19
	.string	"gtk_container_add"
	.byte	29
	.string	"gtk_dialog_get_content_area"
	.byte	16
	.string	"gtk_dialog_run"
	.byte	31
	.string	"gtk_file_chooser_get_filename"
	.byte	48
	.string	"gtk_file_chooser_set_do_overwrite_confirmation"
	.byte	17
	.string	"gtk_grid_attach"
	.byte	14
	.string	"gtk_grid_new"
	.byte	29
	.string	"gtk_grid_set_column_spacing"
	.byte	26
	.string	"gtk_grid_set_row_spacing"
	.byte	10
	.string	"gtk_init"
	.byte	15
	.string	"gtk_label_new"
	.byte	15
	.string	"gtk_main_quit"
	.byte	10
	.string	"gtk_main"
	.byte	18
	.string	"gtk_menu_bar_new"
	.byte	30
	.string	"gtk_menu_item_new_with_label"
	.byte	27
	.string	"gtk_menu_item_set_submenu"
	.byte	14
	.string	"gtk_menu_new"
	.byte	23
	.string	"gtk_menu_shell_append"
	.byte	25
	.string	"gtk_scrolled_window_new"
	.byte	32
	.string	"gtk_scrolled_window_set_policy"
	.byte	37
	.string	"gtk_source_buffer_new_with_language"
	.byte	41
	.string	"gtk_source_language_manager_get_default"
	.byte	42
	.string	"gtk_source_language_manager_get_language"
	.byte	33
	.string	"gtk_source_view_new_with_buffer"
	.byte	39
	.string	"gtk_source_view_set_show_line_numbers"
	.byte	30
	.string	"gtk_text_buffer_get_end_iter"
	.byte	32
	.string	"gtk_text_buffer_get_start_iter"
	.byte	26
	.string	"gtk_text_buffer_get_text"
	.byte	26
	.string	"gtk_text_buffer_set_text"
	.byte	20
	.string	"gtk_widget_destroy"
	.byte	30
	.string	"gtk_widget_set_margin_bottom"
	.byte	27
	.string	"gtk_widget_set_margin_end"
	.byte	29
	.string	"gtk_widget_set_margin_start"
	.byte	27
	.string	"gtk_widget_set_margin_top"
	.byte	21
	.string	"gtk_widget_show_all"
	.byte	16
	.string	"gtk_window_new"
	.byte	29
	.string	"gtk_window_set_default_size"
	.byte	26
	.string	"g_type_class_peek_parent"
	.byte	31
	.string	"g_type_register_static_simple"
	.byte	8
	.string	"strlen"
	.byte	36
	.string	"g_type_class_adjust_private_offset"
	.byte	25
	.string	"g_key_file_save_to_file"
	.byte	17
	.string	"g_ptr_array_add"
	.byte	17
	.string	"gtk_widget_hide"
	.byte	32
	.string	"g_ptr_array_new_with_free_func"
	.byte	29
	.string	"gtk_combo_box_get_active_id"
	.byte	11
	.string	"g_malloc0"
	.byte	18
	.string	"g_ptr_array_free"
	.byte	35
	.string	"gtk_text_iter_forward_to_line_end"
	.byte	26
	.string	"g_return_if_fail_warning"
	.byte	32
	.string	"g_socket_connection_get_socket"
	.byte	33
	.string	"g_socket_client_connect_to_host"
	.byte	21
	.string	"g_string_insert_len"
	.byte	14
	.string	"g_thread_new"
	.byte	31
	.string	"gtk_text_iter_set_line_offset"
	.byte	17
	.string	"g_socket_get_fd"
	.byte	14
	.string	"g_string_new"
	.byte	21
	.string	"g_socket_client_new"
	.byte	19
	.string	"g_string_insert_c"
	.byte	34
	.string	"gtk_text_buffer_get_iter_at_mark"
	.byte	25
	.string	"g_string_free_and_steal"
	.byte	8
	.string	"strstr"
	.byte	26
	.string	"g_spawn_async_with_pipes"
	.byte	13
	.string	"g_file_test"
	.byte	16
	.string	"g_object_unref"
	.byte	19
	.string	"g_application_run"
	.byte	28
	.string	"gtk_text_buffer_get_insert"
	.byte	28
	.string	"g_type_check_instance_is_a"
	.byte	14
	.string	"g_mutex_lock"
	.byte	16
	.string	"g_mutex_unlock"
	.byte	14
	.string	"g_mutex_init"
	.byte	15
	.string	"g_mutex_clear"
	.byte	14
	.string	"g_cond_clear"
	.byte	13
	.string	"g_cond_wait"
	.byte	13
	.string	"g_cond_init"
	.byte	18
	.string	"g_cond_broadcast"
	.byte	24
	.string	"g_key_file_get_integer"
	.byte	20
	.string	"g_application_quit"
	.byte	20
	.string	"g_path_get_dirname"
	.byte	25
	.string	"gtk_widget_get_toplevel"
	.byte	12
	.string	"g_strerror"
	.byte	28
	.string	"gtk_application_window_new"
	.byte	22
	.string	"g_mkdir_with_parents"
	.byte	26
	.string	"gtk_application_get_type"
	.byte	24
	.string	"g_key_file_set_integer"
	.byte	10
	.string	"g_usleep"
	.local	g_swank_session_interactions_table
	.comm	g_swank_session_interactions_table,8,8
	.align 4
	.type	g_swank_session_next_tag, @object
	.size	g_swank_session_next_tag, 4
g_swank_session_next_tag:
	.long	1
	.local	g_swank_session_started
	.comm	g_swank_session_started,4,4
	.local	g_swank_process_started
	.comm	g_swank_process_started,4,4
	.local	g_swank_reader_thread
	.comm	g_swank_reader_thread,8,8
	.align 4
	.type	g_swank_port_number, @object
	.size	g_swank_port_number, 4
g_swank_port_number:
	.long	4005
	.local	g_swank_message_cb_data
	.comm	g_swank_message_cb_data,8,8
	.local	g_swank_message_cb
	.comm	g_swank_message_cb,8,8
	.local	g_swank_incoming_mutex
	.comm	g_swank_incoming_mutex,8,8
	.local	g_swank_incoming_consumed
	.comm	g_swank_incoming_consumed,8,8
	.local	g_swank_incoming_data_buffer
	.comm	g_swank_incoming_data_buffer,8,8
	.local	g_swank_out_cond
	.comm	g_swank_out_cond,16,16
	.local	g_swank_out_mutex
	.comm	g_swank_out_mutex,8,8
	.local	g_swank_out_consumed
	.comm	g_swank_out_consumed,8,8
	.local	g_swank_out_buffer
	.comm	g_swank_out_buffer,8,8
	.local	g_swank_connection
	.comm	g_swank_connection,8,8
	.align 4
	.type	g_swank_fd, @object
	.size	g_swank_fd, 4
g_swank_fd:
	.long	-1
	.local	g_real_process_started
	.comm	g_real_process_started,4,4
	.local	g_real_process_argv
	.comm	g_real_process_argv,8,8
	.local	g_real_process_err_thread
	.comm	g_real_process_err_thread,8,8
	.local	g_real_process_out_thread
	.comm	g_real_process_out_thread,8,8
	.local	g_real_process_err_user_data
	.comm	g_real_process_err_user_data,8,8
	.local	g_real_process_err_cb
	.comm	g_real_process_err_cb,8,8
	.local	g_real_process_out_user_data
	.comm	g_real_process_out_user_data,8,8
	.local	g_real_process_out_cb
	.comm	g_real_process_out_cb,8,8
	.align 4
	.type	g_real_process_err_fd, @object
	.size	g_real_process_err_fd, 4
g_real_process_err_fd:
	.long	-1
	.align 4
	.type	g_real_process_out_fd, @object
	.size	g_real_process_out_fd, 4
g_real_process_out_fd:
	.long	-1
	.align 4
	.type	g_real_process_in_fd, @object
	.size	g_real_process_in_fd, 4
g_real_process_in_fd:
	.long	-1
	.local	g_real_process_pid
	.comm	g_real_process_pid,4,4
	.align 2
	.type	g_pref_swank_port, @object
	.size	g_pref_swank_port, 2
g_pref_swank_port:
	.value	4005
	.local	g_pref_sdk
	.comm	g_pref_sdk,8,8
	.local	g_pref_filename
	.comm	g_pref_filename,8,8
	.local	InteractionsView_private_offset
	.comm	InteractionsView_private_offset,4,4
	.local	interactions_view_parent_class
	.comm	interactions_view_parent_class,8,8
	.section	.rodata.str1.1
.LC188:
	.string	"/usr/bin"
.LC189:
	.string	"/usr/local/bin"
	.section	.data.rel.local,"aw"
	.align 16
	.type	SEARCH_DIRS, @object
	.size	SEARCH_DIRS, 16
SEARCH_DIRS:
	.quad	.LC188
	.quad	.LC189
	.section	.rodata.str1.1
.LC190:
	.string	"sbcl"
.LC191:
	.string	"clisp"
	.section	.data.rel.local
	.align 16
	.type	LISP_NAMES, @object
	.size	LISP_NAMES, 16
LISP_NAMES:
	.quad	.LC190
	.quad	.LC191
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
