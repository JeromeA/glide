LC0:
    db `InteractionsView.class_init`, 0
interactions_view_class_intern_init:
    endbr64
    push rbx
    xor eax, eax
    mov rbx, rdi
    call QWORD [reloc_functions+432]
    mov QWORD [interactions_view_parent_class], rax
    mov eax, DWORD [InteractionsView_private_offset]
    test eax, eax
    je L2
    lea rsi, [InteractionsView_private_offset]
    mov rdi, rbx
    xor eax, eax
    call QWORD [reloc_functions+456]
L2:
    xor eax, eax
    lea rdx, [LC0]
    mov esi, 128
    xor edi, edi
    call [g_log]
    lea rax, [interactions_view_finalize]
    mov QWORD [rbx+48], rax
    pop rbx
    ret
interaction_row_free:
    endbr64
    push rbx
    mov rbx, rdi
    test rdi, rdi
    je L6
    mov rdi, QWORD [rdi]
    test rdi, rdi
    je L6
    xor eax, eax
    call QWORD [reloc_functions+368]
L6:
    mov rdi, rbx
    pop rbx
    jmp [g_free]
static_unescape_string:
    push r12
    mov r12, rdi
    push rbp
    push rbx
    cmp BYTE [rdi], 34
    je L70
    xor eax, eax
    call QWORD [reloc_functions+448]
    pop rbx
    mov rdi, r12
    pop rbp
    pop r12
    jmp [g_strdup]
L70:
    xor edi, edi
    xor eax, eax
    lea rbp, [r12+1]
    call QWORD [reloc_functions+584]
    movsx edx, BYTE [r12+1]
    mov rbx, rax
    xor eax, eax
    test dl, dl
    je L33
L16:
    cmp dl, 34
    je L71
    test eax, eax
    je L17
    cmp dl, 110
    je L18
    jle L72
    cmp dl, 114
    je L22
    cmp dl, 116
    jne L69
    test rbx, rbx
    je L26
    mov rax, QWORD [rbx+8]
    lea rdx, [rax+1]
    cmp rdx, QWORD [rbx+16]
    jnb L26
    mov rcx, QWORD [rbx]
    mov QWORD [rbx+8], rdx
    mov BYTE [rcx+rax], 9
    mov rdx, QWORD [rbx]
    mov rax, QWORD [rbx+8]
    mov BYTE [rdx+rax], 0
L25:
    xor eax, eax
L31:
    movsx edx, BYTE [rbp+1]
    add rbp, 1
    test dl, dl
    jne L16
L33:
    mov rdi, rbx
    xor eax, eax
    pop rbx
    pop rbp
    pop r12
    jmp QWORD [reloc_functions+616]
L26:
    mov edx, 9
    mov rsi, -1
    mov rdi, rbx
    xor eax, eax
    call QWORD [reloc_functions+600]
    jmp L25
L71:
    test eax, eax
    je L33
    test rbx, rbx
    je L29
    mov rax, QWORD [rbx+8]
    lea rdx, [rax+1]
    cmp rdx, QWORD [rbx+16]
    jnb L29
    mov rcx, QWORD [rbx]
    mov QWORD [rbx+8], rdx
    mov BYTE [rcx+rax], 34
    mov rdx, QWORD [rbx]
    mov rax, QWORD [rbx+8]
    mov BYTE [rdx+rax], 0
    jmp L25
L29:
    mov edx, 34
    mov rsi, -1
    mov rdi, rbx
    xor eax, eax
    call QWORD [reloc_functions+600]
    jmp L25
L17:
    mov eax, 1
    cmp dl, 92
    je L31
L69:
    test rbx, rbx
    je L32
    mov rax, QWORD [rbx+8]
    lea rcx, [rax+1]
    cmp rcx, QWORD [rbx+16]
    jnb L32
    mov rsi, QWORD [rbx]
    mov QWORD [rbx+8], rcx
    mov BYTE [rsi+rax], dl
    mov rdx, QWORD [rbx]
    mov rax, QWORD [rbx+8]
    mov BYTE [rdx+rax], 0
    jmp L25
L32:
    mov rsi, -1
    mov rdi, rbx
    xor eax, eax
    call QWORD [reloc_functions+600]
    jmp L25
L18:
    test rbx, rbx
    je L24
    mov rax, QWORD [rbx+8]
    lea rdx, [rax+1]
    cmp rdx, QWORD [rbx+16]
    jnb L24
    mov rcx, QWORD [rbx]
    mov QWORD [rbx+8], rdx
    mov BYTE [rcx+rax], 10
    mov rdx, QWORD [rbx]
    mov rax, QWORD [rbx+8]
    mov BYTE [rdx+rax], 0
    jmp L25
L24:
    mov edx, 10
    mov rsi, -1
    mov rdi, rbx
    xor eax, eax
    call QWORD [reloc_functions+600]
    jmp L25
L22:
    test rbx, rbx
    je L27
    mov rax, QWORD [rbx+8]
    lea rdx, [rax+1]
    cmp rdx, QWORD [rbx+16]
    jnb L27
    mov rcx, QWORD [rbx]
    mov QWORD [rbx+8], rdx
    mov BYTE [rcx+rax], 13
    mov rdx, QWORD [rbx]
    mov rax, QWORD [rbx+8]
    mov BYTE [rdx+rax], 0
    jmp L25
L27:
    mov edx, 13
    mov rsi, -1
    mov rdi, rbx
    xor eax, eax
    call QWORD [reloc_functions+600]
    jmp L25
L72:
    cmp dl, 92
    jne L69
    test rbx, rbx
    je L28
    mov rax, QWORD [rbx+8]
    lea rdx, [rax+1]
    cmp rdx, QWORD [rbx+16]
    jnb L28
    mov rcx, QWORD [rbx]
    mov QWORD [rbx+8], rdx
    mov BYTE [rcx+rax], 92
    mov rdx, QWORD [rbx]
    mov rax, QWORD [rbx+8]
    mov BYTE [rdx+rax], 0
    jmp L25
L28:
    mov edx, 92
    mov rsi, -1
    mov rdi, rbx
    xor eax, eax
    call QWORD [reloc_functions+600]
    jmp L25
LC1:
    db `LISP STDERR: %s\n`, 0
on_lisp_stderr:
    endbr64
    mov rsi, QWORD [rdi]
    xor eax, eax
    lea rdi, [LC1]
    jmp [g_printerr]
LC2:
    db `InteractionsView`, 0
interactions_view_get_type_once:
    push rbx
    lea rdi, [LC2]
    xor eax, eax
    call QWORD [reloc_functions+24]
    mov rbx, rax
    call [gtk_box_get_type]
    sub rsp, 8
    mov edx, 1008
    mov rsi, rbx
    push 0
    mov rdi, rax
    xor eax, eax
    lea rcx, [interactions_view_class_intern_init]
    lea r9, [interactions_view_init]
    mov r8d, 56
    call QWORD [reloc_functions+440]
    pop rdx
    pop rcx
    pop rbx
    ret
LC3:
    db `InteractionsView.init`, 0
    align 8
LC4:
    db `.interac`
    db `tion-output text { background-color: #f2f2f2; font-family: monospace; } .interaction-error text { background-color: #ffe5e5; font-family: monospace; color: #c00; } .interaction-result text { background-color: #e5ffe5; font-family: monospace; color: #060; }`, 0
interactions_view_init:
    endbr64
    push rbp
    lea rdx, [LC3]
    xor eax, eax
    mov esi, 128
    push rbx
    mov rbx, rdi
    xor edi, edi
    sub rsp, 8
    call [g_log]
    mov rdi, rbx
    mov esi, 1
    call [gtk_orientable_set_orientation]
    mov rdi, rbx
    mov esi, 1
    call [gtk_widget_set_vexpand]
    call [gtk_css_provider_new]
    xor ecx, ecx
    mov rdx, -1
    lea rsi, [LC4]
    mov rdi, rax
    mov rbp, rax
    call [gtk_css_provider_load_from_data]
    mov rdi, rbx
    call [gtk_widget_get_screen]
    mov rsi, rbp
    mov edx, 600
    mov rdi, rax
    call [gtk_style_context_add_provider_for_screen]
    mov rdi, rbp
    xor eax, eax
    call QWORD [reloc_functions+648]
    mov rsi, QWORD [g_direct_equal]
    mov rdi, QWORD [g_direct_hash]
    xor edx, edx
    lea rcx, [interaction_row_free]
    call [g_hash_table_new_full]
    mov QWORD [rbx+48], rax
    add rsp, 8
    pop rbx
    pop rbp
    ret
LC5:
    db `InteractionsView.finalize`, 0
interactions_view_finalize:
    endbr64
    push rbx
    lea rdx, [LC5]
    mov rbx, rdi
    mov esi, 128
    xor edi, edi
    xor eax, eax
    call [g_log]
    mov rdi, QWORD [rbx+48]
    test rdi, rdi
    je L79
    call [g_hash_table_destroy]
    mov QWORD [rbx+48], 0
L79:
    mov rax, QWORD [interactions_view_parent_class]
    mov rdi, rbx
    pop rbx
    mov rax, QWORD [rax+48]
    jmp rax
set_text_view:
    push r12
    mov r12, rdi
    push rbp
    mov rbp, rsi
    push rbx
    sub rsp, 16
    mov rdi, QWORD [rsi]
    test rdx, rdx
    je L110
    movzx eax, BYTE [rdx]
    mov rbx, rdx
    test al, al
    jne L108
    and r8d, 1
    jne L110
    test al, al
    jne L108
    test rdi, rdi
    je L84
L91:
    call [gtk_text_view_get_buffer]
    add rsp, 16
    mov rsi, rbx
    mov edx, -1
    pop rbx
    mov rdi, rax
    pop rbp
    xor eax, eax
    pop r12
    jmp QWORD [reloc_functions+360]
L108:
    test rdi, rdi
    jne L91
    mov QWORD [rsp+8], rcx
    call [gtk_text_view_new]
    xor esi, esi
    mov QWORD [rbp+0], rax
    mov rdi, rax
    call [gtk_text_view_set_editable]
    mov rdi, QWORD [rbp+0]
    mov esi, 3
    call [gtk_text_view_set_wrap_mode]
    mov rcx, QWORD [rsp+8]
    test rcx, rcx
    je L92
    mov rdi, QWORD [rbp+0]
    call [gtk_widget_get_style_context]
    mov rsi, QWORD [rsp+8]
    mov rdi, rax
    call [gtk_style_context_add_class]
L92:
    mov rsi, QWORD [rbp+0]
    xor r8d, r8d
    xor ecx, ecx
    xor edx, edx
    mov rdi, r12
    xor eax, eax
    call QWORD [reloc_functions+104]
    mov rdi, QWORD [rbp+0]
    call [gtk_widget_show]
    mov rdi, QWORD [rbp+0]
    test rdi, rdi
    jne L91
L84:
    add rsp, 16
    pop rbx
    pop rbp
    pop r12
    ret
L110:
    test rdi, rdi
    je L84
    xor eax, eax
    call QWORD [reloc_functions+368]
    mov QWORD [rbp+0], 0
    add rsp, 16
    pop rbx
    pop rbp
    pop r12
    ret
LC6:
    db `InteractionsView.row_update for expr: %s`, 0
LC7:
    db `interaction-output`, 0
LC8:
    db `interaction-error`, 0
LC9:
    db `interaction-result`, 0
interaction_row_update:
    push rbp
    xor eax, eax
    mov rbp, rsi
    lea rdx, [LC6]
    push rbx
    mov rbx, rdi
    xor edi, edi
    sub rsp, 8
    mov rcx, QWORD [rsi]
    mov esi, 128
    call [g_log]
    mov rdx, QWORD [rbp+0]
    mov rdi, QWORD [rbx+8]
    xor r8d, r8d
    lea rsi, [rbx+16]
    xor ecx, ecx
    call set_text_view
    mov rdx, QWORD [rbp+24]
    mov rdi, QWORD [rbx+8]
    lea rsi, [rbx+24]
    mov r8d, 1
    lea rcx, [LC7]
    call set_text_view
    mov rdx, QWORD [rbp+32]
    mov rdi, QWORD [rbx+8]
    lea rsi, [rbx+32]
    mov r8d, 1
    lea rcx, [LC8]
    call set_text_view
    mov rdx, QWORD [rbp+16]
    mov rdi, QWORD [rbx+8]
    lea rsi, [rbx+40]
    mov r8d, 1
    lea rcx, [LC9]
    call set_text_view
    mov rsi, QWORD [rbx+40]
    test rsi, rsi
    je L111
    mov rdi, QWORD [rbx+8]
    add rsp, 8
    mov edx, -1
    pop rbx
    pop rbp
    jmp [gtk_box_reorder_child]
L111:
    add rsp, 8
    pop rbx
    pop rbp
    ret
LC10:
    db `swank_process: read_until_from_lisp_output: waiting for '%s'`, 0
LC11:
    db `swank_process: read_until_from_lisp_output: found '%s'. Consumed up to %zu.`, 0
LC12:
    db `swank_process: read_until_from_lisp_output: pattern not found, waiting on g_swank_out_cond.`, 0
LC13:
    db `swank_process: read_until_from_lisp_output: woken up.`, 0
read_until_from_lisp_output:
    push r15
    mov rcx, rdi
    lea rdx, [LC10]
    xor eax, eax
    push r14
    mov esi, 128
    lea r14, [LC12]
    push r13
    lea r13, [g_swank_out_cond]
    push r12
    lea r12, [LC13]
    push rbp
    lea rbp, [g_swank_out_mutex]
    push rbx
    mov rbx, rdi
    xor edi, edi
    sub rsp, 8
    call [g_log]
    mov rdi, rbx
    xor eax, eax
    call QWORD [reloc_functions+448]
    mov rdi, rbp
    mov r15, rax
    xor eax, eax
    call QWORD [reloc_functions+680]
    jmp L117
L115:
    mov rdx, r14
    mov esi, 128
    xor edi, edi
    xor eax, eax
    call [g_log]
    mov rsi, rbp
    mov rdi, r13
    xor eax, eax
    call QWORD [reloc_functions+720]
    mov rdx, r12
    mov esi, 128
    xor edi, edi
    xor eax, eax
    call [g_log]
L117:
    mov rax, QWORD [g_swank_out_buffer]
    mov rdi, QWORD [g_swank_out_consumed]
    mov rsi, rbx
    add rdi, QWORD [rax]
    xor eax, eax
    call QWORD [reloc_functions+624]
    test rax, rax
    je L115
    mov rdx, QWORD [g_swank_out_buffer]
    xor edi, edi
    mov rcx, rbx
    mov esi, 128
    sub rax, QWORD [rdx]
    lea rdx, [LC11]
    lea r8, [rax+r15]
    xor eax, eax
    mov QWORD [g_swank_out_consumed], r8
    call [g_log]
    mov rdi, QWORD [g_swank_out_buffer]
    mov rax, QWORD [g_swank_out_consumed]
    cmp QWORD [rdi+8], rax
    je L119
L116:
    add rsp, 8
    mov rdi, rbp
    xor eax, eax
    pop rbx
    pop rbp
    pop r12
    pop r13
    pop r14
    pop r15
    jmp QWORD [reloc_functions+688]
L119:
    xor esi, esi
    call [g_string_set_size]
    mov QWORD [g_swank_out_consumed], 0
    jmp L116
static_next_token:
    mov rcx, rdi
    mov rdi, QWORD [rdi]
L125:
    movzx edx, BYTE [rdi]
    cmp dl, 13
    jg L121
    cmp dl, 8
    jg L122
    test dl, dl
    je L126
L150:
    mov rax, rdi
L140:
    cmp dl, 13
    jg L141
    cmp dl, 8
    jle L143
L164:
    mov rsi, rax
    sub rsi, rdi
L129:
    mov QWORD [rcx], rax
    jmp [g_strndup]
L121:
    cmp dl, 32
    je L122
    cmp dl, 40
    jne L128
    movzx edx, BYTE [rdi+1]
    lea rax, [rdi+1]
    test dl, dl
    je L151
    xor r8d, r8d
    xor r9d, r9d
    mov r10d, 1
    test r8d, r8d
    jne L146
L168:
    cmp dl, 92
    je L147
    cmp dl, 34
    je L166
    mov esi, 1
    test r9d, r9d
    jne L130
    cmp dl, 40
    je L167
    cmp dl, 41
    jne L130
    sub r10d, 1
    setne sil
L130:
    movzx edx, BYTE [rax+1]
    add rax, 1
    test dl, dl
    je L164
    test sil, sil
    je L164
    test r8d, r8d
    je L168
L146:
    mov esi, 1
    xor r8d, r8d
    jmp L130
L122:
    add rdi, 1
    jmp L125
L141:
    cmp dl, 32
    je L164
    sub edx, 40
    cmp dl, 1
    jbe L164
L143:
    movzx edx, BYTE [rax+1]
    add rax, 1
    test dl, dl
    jne L140
    jmp L164
L147:
    mov esi, 1
    mov r8d, 1
    jmp L130
L166:
    xor r9d, 1
    mov esi, 1
    jmp L130
L128:
    cmp dl, 34
    jne L150
    movzx edx, BYTE [rdi+1]
    lea rax, [rdi+1]
    test dl, dl
    jne L137
    jmp L151
L169:
    add rax, 1
    cmp dl, 34
    je L164
L163:
    movzx edx, BYTE [rax]
    test dl, dl
    je L164
L137:
    cmp dl, 92
    jne L169
    cmp BYTE [rax+1], 0
    lea rdx, [rax+1]
    je L152
    add rax, 2
    jmp L163
L126:
    xor eax, eax
    ret
L167:
    add r10d, 1
    jmp L130
L151:
    mov esi, 1
    jmp L129
L152:
    mov rax, rdx
    jmp L164
interaction_free_members_static:
    endbr64
    test rdi, rdi
    je L170
    push rbx
    mov rbx, rdi
    mov rdi, QWORD [rdi]
    call [g_free]
    mov QWORD [rbx], 0
    mov rdi, QWORD [rbx+24]
    call [g_free]
    mov QWORD [rbx+24], 0
    mov rdi, QWORD [rbx+16]
    call [g_free]
    mov QWORD [rbx+16], 0
    mov rdi, QWORD [rbx+32]
    call [g_free]
    mov QWORD [rbx+32], 0
    mov rdi, rbx
    pop rbx
    jmp [g_free]
L170:
    ret
LC14:
    db `process_global: child_setup_global`, 0
LC15:
    db `prctl(PR_SET_PDEATHSIG, SIGKILL) failed`, 0
.LCOLDB16:
.LHOTB16:
child_setup_global:
    endbr64
    xor edi, edi
    sub rsp, 8
    mov esi, 128
    xor eax, eax
    lea rdx, [LC14]
    call [g_log]
    call [setsid]
    xor eax, eax
    mov esi, 9
    mov edi, 1
    call [prctl]
    test eax, eax
    js L177
    add rsp, 8
    ret
child_setup_global.cold:
.LFSB2778:
L177:
    lea rdi, [LC15]
    pop rax
    jmp [perror]
.LCOLDE16:
.LHOTE16:
LC17:
    db `quit_menu_item_handler`, 0
LC18:
    db `app_on_quit_global`, 0
LC19:
    db `app_quit_global`, 0
quit_menu_item_handler:
    endbr64
    sub rsp, 8
    lea rdx, [LC17]
    xor edi, edi
    xor eax, eax
    mov esi, 128
    call [g_log]
    lea rdx, [LC18]
    xor edi, edi
    xor eax, eax
    mov esi, 128
    call [g_log]
    xor eax, eax
    lea rdx, [LC19]
    xor edi, edi
    mov esi, 128
    call [g_log]
    xor eax, eax
    add rsp, 8
    jmp QWORD [reloc_functions+224]
LC20:
    db `quit_delete_event_handler`, 0
quit_delete_event_handler:
    endbr64
    sub rsp, 8
    lea rdx, [LC20]
    xor edi, edi
    xor eax, eax
    mov esi, 128
    call [g_log]
    lea rdx, [LC18]
    xor edi, edi
    xor eax, eax
    mov esi, 128
    call [g_log]
    lea rdx, [LC19]
    xor edi, edi
    xor eax, eax
    mov esi, 128
    call [g_log]
    xor eax, eax
    call QWORD [reloc_functions+224]
    mov eax, 1
    add rsp, 8
    ret
LC21:
    db `process_global: stdout_thread_global starting`, 0
LC22:
    db `process_global: stdout_thread_global exiting, n=%zd, errno=%d`, 0
stdout_thread_global:
    endbr64
    push r13
    lea rdx, [LC21]
    mov esi, 128
    xor edi, edi
    push r12
    push rbp
    push rbx
    xor ebx, ebx
    sub rsp, 280
    mov rax, QWORD fs:40
    mov QWORD [rsp+264], rax
    xor eax, eax
    call [g_log]
    mov edx, DWORD [g_process_out_fd]
    xor r8d, r8d
    test edx, edx
    js L185
    mov r12, rsp
    mov r13d, 256
L184:
    movsx rax, edx
    mov rdi, rax
    mov rsi, r12
    mov rdx, r13
    mov rax, $0
    syscall
    mov rbx, rax
    test rax, rax
    js L196
    je L189
    cmp QWORD [g_process_out_cb], 0
    je L186
    mov rsi, rbx
    mov rdi, r12
    call [g_string_new_len]
    mov rsi, QWORD [g_process_out_user_data]
    mov rbp, rax
    mov rdi, rax
    call QWORD [g_process_out_cb]
    mov esi, 1
    mov rdi, rbp
    call [g_string_free]
L186:
    mov edx, DWORD [g_process_out_fd]
    test edx, edx
    jns L184
L189:
    xor r8d, r8d
L185:
    xor edi, edi
    xor eax, eax
    mov rcx, rbx
    mov esi, 128
    lea rdx, [LC22]
    call [g_log]
    mov rax, QWORD [rsp+264]
    sub rax, QWORD fs:40
    jne L197
    add rsp, 280
    xor eax, eax
    pop rbx
    pop rbp
    pop r12
    pop r13
    ret
L196:
    call [__errno_location]
    mov r8d, ebx
    mov rbx, -1
    neg r8d
    mov DWORD [rax], r8d
    jmp L185
L197:
    call [__stack_chk_fail]
LC23:
    db `process_global: stderr_thread_global starting`, 0
LC24:
    db `process_global: stderr_thread_global exiting, n=%zd, errno=%d`, 0
stderr_thread_global:
    endbr64
    push r13
    lea rdx, [LC23]
    mov esi, 128
    xor edi, edi
    push r12
    push rbp
    push rbx
    xor ebx, ebx
    sub rsp, 280
    mov rax, QWORD fs:40
    mov QWORD [rsp+264], rax
    xor eax, eax
    call [g_log]
    mov edx, DWORD [g_process_err_fd]
    xor r8d, r8d
    test edx, edx
    js L200
    mov r12, rsp
    mov r13d, 256
L199:
    movsx rax, edx
    mov rdi, rax
    mov rsi, r12
    mov rdx, r13
    mov rax, $0
    syscall
    mov rbx, rax
    test rax, rax
    js L211
    je L204
    cmp QWORD [g_process_err_cb], 0
    je L201
    mov rsi, rbx
    mov rdi, r12
    call [g_string_new_len]
    mov rsi, QWORD [g_process_err_user_data]
    mov rbp, rax
    mov rdi, rax
    call QWORD [g_process_err_cb]
    mov esi, 1
    mov rdi, rbp
    call [g_string_free]
L201:
    mov edx, DWORD [g_process_err_fd]
    test edx, edx
    jns L199
L204:
    xor r8d, r8d
L200:
    xor edi, edi
    xor eax, eax
    mov rcx, rbx
    mov esi, 128
    lea rdx, [LC24]
    call [g_log]
    mov rax, QWORD [rsp+264]
    sub rax, QWORD fs:40
    jne L212
    add rsp, 280
    xor eax, eax
    pop rbx
    pop rbp
    pop r12
    pop r13
    ret
L211:
    call [__errno_location]
    mov r8d, ebx
    mov rbx, -1
    neg r8d
    mov DWORD [rax], r8d
    jmp L200
L212:
    call [__stack_chk_fail]
LC25:
    db `swank_session_on_message_internal: Received raw msg:`, 0
LC26:
    db `%s%.40s...`, 0
LC27:
    db `%s%s`, 0
swank_session_on_message_internal:
    endbr64
    push rbp
    xor eax, eax
    push rbx
    mov rbx, rdi
    sub rsp, 8
    mov rbp, QWORD [rdi]
    mov rdi, rbp
    call QWORD [reloc_functions+448]
    mov r8, rbp
    lea rcx, [LC25]
    cmp rax, 40
    jbe L214
    lea rdx, [LC26]
    mov esi, 128
    xor edi, edi
    xor eax, eax
    call [g_log]
L215:
    mov edi, 8
    xor eax, eax
    call QWORD [reloc_functions+72]
    mov rsi, QWORD [rbx+8]
    mov rdi, QWORD [rbx]
    mov rbp, rax
    call [g_string_new_len]
    mov rdx, rbp
    lea rsi, [swank_session_handle_message_on_main_thread]
    xor edi, edi
    mov QWORD [rbp+0], rax
    add rsp, 8
    pop rbx
    pop rbp
    jmp [g_main_context_invoke]
L214:
    lea rdx, [LC27]
    mov esi, 128
    xor edi, edi
    xor eax, eax
    call [g_log]
    jmp L215
LC28:
    db `swank_process: on_lisp_stdout received:`, 0
on_lisp_stdout:
    endbr64
    push r14
    xor eax, eax
    push r13
    push r12
    push rbp
    mov rbp, rdi
    push rbx
    mov rbx, QWORD [rdi]
    mov rdi, rbx
    call QWORD [reloc_functions+448]
    mov r8, rbx
    lea rcx, [LC28]
    cmp rax, 40
    jbe L218
    lea rdx, [LC26]
    mov esi, 128
    xor edi, edi
    xor eax, eax
    call [g_log]
L219:
    lea r12, [g_swank_out_mutex]
    xor eax, eax
    mov rdi, r12
    call QWORD [reloc_functions+680]
    mov rbx, QWORD [rbp+8]
    mov r13, QWORD [rbp+0]
    mov rbp, QWORD [g_swank_out_buffer]
    mov r14, rbx
    test rbp, rbp
    je L232
    test r13, r13
    je L222
    test rbx, rbx
    js L233
    mov rdi, QWORD [rbp+8]
    lea rax, [rdi+rbx]
    cmp rax, QWORD [rbp+16]
    jnb L234
L225:
    add rdi, QWORD [rbp+0]
    lea rax, 0[r13+rbx]
    mov rdx, rbx
    mov rsi, r13
    cmp rdi, rax
    jnb L226
    lea rax, [rdi+rbx]
    cmp rax, r13
    jnb L235
L226:
    call [memcpy]
L227:
    mov rax, QWORD [rbp+0]
    add rbx, QWORD [rbp+8]
    mov QWORD [rbp+8], rbx
    mov BYTE [rax+rbx], 0
    jmp L221
L234:
    mov rcx, r14
    mov rdx, r13
    mov rsi, -1
    mov rdi, rbp
    xor eax, eax
    call QWORD [reloc_functions+552]
L221:
    lea rdi, [g_swank_out_cond]
    call [g_cond_signal]
    pop rbx
    mov rdi, r12
    pop rbp
    xor eax, eax
    pop r12
    pop r13
    pop r14
    jmp QWORD [reloc_functions+688]
L233:
    mov rdi, r13
    xor eax, eax
    call QWORD [reloc_functions+448]
    mov rdi, QWORD [rbp+8]
    mov rbx, rax
    lea rax, [rdi+rbx]
    cmp rax, QWORD [rbp+16]
    jb L225
    jmp L234
L218:
    lea rdx, [LC27]
    mov esi, 128
    xor edi, edi
    xor eax, eax
    call [g_log]
    jmp L219
L235:
    call [memmove]
    jmp L227
L222:
    test rbx, rbx
    je L221
    mov rdx, rbx
    xor esi, esi
    mov rdi, rbp
    call [g_string_append_len]
    jmp L221
L232:
    mov rdx, rbx
    mov rsi, r13
    xor edi, edi
    call [g_string_append_len]
    jmp L221
LC29:
    db `swank_process: swank_reader_thread_global starting for fd %d`, 0
LC30:
    db `swank_process: swank_reader_thread_global received data:`, 0
LC31:
    db `swank_process: swank_reader_thread_global: EOF on Swank FD %d`, 0
LC32:
    db `swank_process: swank_reader_thread_global read error on fd %d: %s (errno %d)\n`, 0
LC33:
    db `swank_process: swank_reader_thread_global exiting for fd %d`, 0
swank_reader_thread_global:
    endbr64
    push r15
    lea rdx, [LC29]
    mov esi, 128
    xor edi, edi
    push r14
    push r13
    push r12
    push rbp
    push rbx
    sub rsp, 1064
    mov ecx, DWORD [g_swank_fd]
    mov rax, QWORD fs:40
    mov QWORD [rsp+1048], rax
    xor eax, eax
    call [g_log]
    mov ecx, DWORD [g_swank_fd]
    test ecx, ecx
    js L237
    lea rbp, [rsp+16]
    mov r12d, 1024
L257:
    movsx rbx, ecx
    mov rdi, rbx
    mov rsi, rbp
    mov rdx, r12
    mov rax, $0
    syscall
    mov rbx, rax
    test rax, rax
    js L266
    jne L267
    mov ecx, DWORD [g_swank_fd]
    mov esi, 128
    xor edi, edi
    xor eax, eax
    lea rdx, [LC31]
    call [g_log]
    mov ecx, DWORD [g_swank_fd]
L237:
    xor edi, edi
    xor eax, eax
    lea rdx, [LC33]
    mov esi, 128
    call [g_log]
    mov rax, QWORD [rsp+1048]
    sub rax, QWORD fs:40
    jne L268
    add rsp, 1064
    xor eax, eax
    pop rbx
    pop rbp
    pop r12
    pop r13
    pop r14
    pop r15
    ret
L266:
    call [__errno_location]
    neg ebx
    mov DWORD [rax], ebx
    cmp ebx, 11
    je L269
    mov edi, ebx
    xor eax, eax
    call QWORD [reloc_functions+776]
    mov esi, DWORD [g_swank_fd]
    mov ecx, ebx
    lea rdi, [LC32]
    mov rdx, rax
    xor eax, eax
    call [g_printerr]
    mov ecx, DWORD [g_swank_fd]
    jmp L237
L267:
    mov rsi, rax
    mov rdi, rbp
    call [g_strndup]
    mov r13, rax
    mov rdi, rax
    xor eax, eax
    call QWORD [reloc_functions+448]
    mov r8, r13
    lea rcx, [LC30]
    cmp rax, 40
    ja L270
    lea rdx, [LC27]
    mov esi, 128
    xor edi, edi
    xor eax, eax
    call [g_log]
L243:
    mov rdi, r13
    lea r14, [g_swank_incoming_mutex]
    call [g_free]
    mov rdi, r14
    xor eax, eax
    call QWORD [reloc_functions+680]
    mov r13, QWORD [g_swank_incoming_data_buffer]
    test r13, r13
    je L271
    mov rdi, QWORD [r13+8]
    lea rax, [rbx+rdi]
    cmp rax, QWORD [r13+16]
    jb L246
    mov rdi, r13
    mov rcx, rbx
    mov rdx, rbp
    mov rsi, -1
    xor eax, eax
    call QWORD [reloc_functions+552]
    mov r13, QWORD [g_swank_incoming_data_buffer]
L245:
    cmp QWORD [g_swank_message_cb], 0
    mov rdx, QWORD [g_swank_incoming_consumed]
    mov rax, QWORD [r13+8]
    jne L272
L249:
    test rdx, rdx
    je L254
    cmp rdx, rax
    jb L273
L254:
    cmp rdx, rax
    je L274
L255:
    mov QWORD [g_swank_incoming_consumed], 0
    mov rdi, r14
    xor eax, eax
    call QWORD [reloc_functions+688]
L256:
    mov ecx, DWORD [g_swank_fd]
    test ecx, ecx
    jns L257
    jmp L237
L270:
    lea rdx, [LC26]
    mov esi, 128
    xor edi, edi
    xor eax, eax
    call [g_log]
    jmp L243
L272:
    mov rcx, rax
    sub rcx, rdx
    cmp rcx, 5
    jbe L249
    lea r15, [rsp+9]
L253:
    add rdx, QWORD [r13+0]
    xor esi, esi
    mov rdi, r15
    mov eax, DWORD [rdx]
    mov DWORD [r15], eax
    movzx eax, WORD [rdx+4]
    mov edx, 16
    mov WORD [r15+4], ax
    mov BYTE [rsp+15], 0
    call [g_ascii_strtoull]
    mov r13, QWORD [g_swank_incoming_data_buffer]
    mov rdx, QWORD [g_swank_incoming_consumed]
    mov rbx, rax
    mov rax, QWORD [r13+8]
    mov rcx, rax
    sub rcx, rdx
    sub rcx, 6
    cmp rcx, rbx
    jb L249
    mov rax, QWORD [r13+0]
    mov rsi, rbx
    lea rdi, 6[rax+rdx]
    call [g_string_new_len]
    mov rsi, QWORD [g_swank_message_cb_data]
    mov r13, rax
    mov rax, QWORD [g_swank_incoming_consumed]
    mov rdi, r13
    lea rax, 6[rbx+rax]
    mov QWORD [g_swank_incoming_consumed], rax
    call QWORD [g_swank_message_cb]
    mov rdi, r13
    mov esi, 1
    call [g_string_free]
    mov r13, QWORD [g_swank_incoming_data_buffer]
    mov rdx, QWORD [g_swank_incoming_consumed]
    mov rax, QWORD [r13+8]
    cmp rax, rdx
    je L251
    mov rcx, rax
    sub rcx, rdx
L252:
    cmp rcx, 5
    ja L253
    jmp L249
L246:
    add rdi, QWORD [r13+0]
    lea rax, 0[rbp+rbx]
    mov rdx, rbx
    mov rsi, rbp
    cmp rdi, rax
    jnb L247
    lea rax, [rdi+rbx]
    cmp rax, rbp
    jnb L275
L247:
    call [memcpy]
L248:
    mov rax, QWORD [r13+0]
    add rbx, QWORD [r13+8]
    mov QWORD [r13+8], rbx
    mov BYTE [rax+rbx], 0
    jmp L245
L269:
    mov edi, 10000
    xor eax, eax
    call QWORD [reloc_functions+816]
    jmp L256
L275:
    call [memmove]
    jmp L248
L274:
    xor esi, esi
    mov rdi, r13
    call [g_string_set_size]
    jmp L255
L273:
    xor esi, esi
    mov rdi, r13
    call [g_string_erase]
    jmp L255
L271:
    mov rdx, rbx
    mov rsi, rbp
    xor edi, edi
    call [g_string_append_len]
    mov r13, QWORD [g_swank_incoming_data_buffer]
    jmp L245
L251:
    mov rdi, r13
    xor esi, esi
    call [g_string_set_size]
    mov r13, QWORD [g_swank_incoming_data_buffer]
    xor edx, edx
    mov QWORD [g_swank_incoming_consumed], 0
    mov rax, QWORD [r13+8]
    mov rcx, rax
    jmp L252
L268:
    call [__stack_chk_fail]
interactions_view_get_type:
    endbr64
    mov rax, QWORD [static_g_define_type_id.2]
    test rax, rax
    je L287
    mov rax, QWORD [static_g_define_type_id.2]
    ret
L287:
    push rbx
    lea rbx, [static_g_define_type_id.2]
    mov rdi, rbx
    call [g_once_init_enter_pointer]
    test eax, eax
    jne L288
    mov rax, QWORD [static_g_define_type_id.2]
    pop rbx
    ret
L288:
    call interactions_view_get_type_once
    mov rdi, rbx
    mov rsi, rax
    call [g_once_init_leave_pointer]
    mov rax, QWORD [static_g_define_type_id.2]
    pop rbx
    ret
LC34:
    db `InteractionsView.new (no-args)`, 0
interactions_view_new:
    endbr64
    sub rsp, 8
    mov esi, 128
    xor edi, edi
    xor eax, eax
    lea rdx, [LC34]
    call [g_log]
    call interactions_view_get_type
    xor esi, esi
    add rsp, 8
    mov rdi, rax
    xor eax, eax
    jmp [g_object_new]
LC35:
    db `interaction != NULL`, 0
LC36:
    db `InteractionsView.add_interaction for expr: %s`, 0
LC37:
    db `GLIDE_IS_INTERACTIONS_VIEW(self)`, 0
interactions_view_add_interaction:
    endbr64
    push r12
    mov r12, rsi
    push rbp
    push rbx
    mov rbx, rdi
    call interactions_view_get_type
    test rbx, rbx
    je L292
    mov rsi, rax
    mov rax, QWORD [rbx]
    test rax, rax
    je L293
    cmp rsi, QWORD [rax]
    je L296
L293:
    xor eax, eax
    mov rdi, rbx
    call QWORD [reloc_functions+672]
    test eax, eax
    je L292
L296:
    test r12, r12
    je L306
    mov rcx, QWORD [r12]
    lea rdx, [LC36]
    xor edi, edi
    xor eax, eax
    mov esi, 128
    call [g_log]
    mov edi, 48
    xor eax, eax
    call QWORD [reloc_functions+504]
    xor edi, edi
    mov rbp, rax
    call [gtk_frame_new]
    mov esi, 5
    mov QWORD [rbp+0], rax
    mov rdi, rax
    xor eax, eax
    call QWORD [reloc_functions+392]
    mov rdi, QWORD [rbp+0]
    mov esi, 5
    xor eax, eax
    call QWORD [reloc_functions+384]
    mov rdi, QWORD [rbp+0]
    mov esi, 5
    xor eax, eax
    call QWORD [reloc_functions+400]
    mov rdi, QWORD [rbp+0]
    mov esi, 5
    xor eax, eax
    call QWORD [reloc_functions+376]
    mov esi, 2
    mov edi, 1
    xor eax, eax
    call QWORD [reloc_functions+96]
    mov rdi, QWORD [rbp+0]
    mov QWORD [rbp+8], rax
    mov rsi, rax
    xor eax, eax
    call QWORD [reloc_functions+136]
    mov rsi, r12
    mov rdi, rbp
    call interaction_row_update
    mov rdi, QWORD [rbx+48]
    mov rdx, rbp
    mov rsi, r12
    call [g_hash_table_insert]
    mov rdi, rbx
    mov rsi, QWORD [rbp+0]
    xor r8d, r8d
    xor ecx, ecx
    xor edx, edx
    xor eax, eax
    call QWORD [reloc_functions+104]
    pop rbx
    mov rdi, QWORD [rbp+0]
    xor eax, eax
    pop rbp
    pop r12
    jmp QWORD [reloc_functions+408]
L306:
    lea rdx, [LC35]
L305:
    pop rbx
    lea rsi, [__func__.1]
    pop rbp
    xor edi, edi
    xor eax, eax
    pop r12
    jmp QWORD [reloc_functions+528]
L292:
    lea rdx, [LC37]
    jmp L305
LC38:
    db `InteractionsView.update_interaction for expr: %s`, 0
LC39:
    db `InteractionsView.update_interaction: row not found for interaction with expr: %s (tag: %u)`, 0
interactions_view_update_interaction:
    endbr64
    push rbp
    mov rbp, rsi
    push rbx
    mov rbx, rdi
    sub rsp, 8
    call interactions_view_get_type
    test rbx, rbx
    je L308
    mov rsi, rax
    mov rax, QWORD [rbx]
    test rax, rax
    je L309
    cmp rsi, QWORD [rax]
    je L312
L309:
    xor eax, eax
    mov rdi, rbx
    call QWORD [reloc_functions+672]
    test eax, eax
    je L308
L312:
    test rbp, rbp
    je L323
    mov rcx, QWORD [rbp+0]
    lea rdx, [LC38]
    xor edi, edi
    xor eax, eax
    mov esi, 128
    call [g_log]
    mov rdi, QWORD [rbx+48]
    mov rsi, rbp
    call [g_hash_table_lookup]
    mov rbx, rax
    test rax, rax
    je L324
    mov rdi, rax
    mov rsi, rbp
    call interaction_row_update
    mov rdi, QWORD [rbx]
    add rsp, 8
    xor eax, eax
    pop rbx
    pop rbp
    jmp QWORD [reloc_functions+408]
L323:
    lea rdx, [LC35]
L322:
    add rsp, 8
    lea rsi, [__func__.0]
    xor edi, edi
    xor eax, eax
    pop rbx
    pop rbp
    jmp QWORD [reloc_functions+528]
L308:
    lea rdx, [LC37]
    jmp L322
L324:
    mov rcx, QWORD [rbp+0]
    mov r8d, DWORD [rbp+8]
    add rsp, 8
    xor edi, edi
    pop rbx
    lea rdx, [LC39]
    mov esi, 16
    xor eax, eax
    pop rbp
    jmp [g_log]
LC40:
    db `swank_session_handle_message_on_main_thread: Processing msg:`, 0
LC41:
    db `(:return `, 0
LC42:
    db `(:new-features `, 0
LC43:
    db `swank_session: Could not parse :return message. Payload: %s`, 0
LC44:
    db `swank_session: Invalid tag_id in :return message: '%s'`, 0
LC45:
    db `swank_session: Received :return for unknown tag_id: %u`, 0
LC46:
    db `(:ok `, 0
LC47:
    db `Interaction %u OK: output='%s', result='%s'`, 0
LC48:
    db `swank_session: Failed to parse specific return type: %s`, 0
LC49:
    db `Failed to parse return from Swank`, 0
LC50:
    db `Interaction %u ABORT: reason='%s'`, 0
LC51:
    db `Interaction %u updated (status: %d)`, 0
LC52:
    db `parse_and_handle_return_message: interactions_view_global is NULL. Cannot update interaction in view.`, 0
LC53:
    db `(:indentation-update `, 0
LC54:
    db `Received Swank :new-features:`, 0
LC55:
    db `Received Swank :indentation-update:`, 0
LC56:
    db `Received unknown Swank message type:`, 0
swank_session_handle_message_on_main_thread:
    endbr64
    push r15
    push r14
    push r13
    push r12
    push rbp
    push rbx
    mov rbx, rdi
    sub rsp, 104
    mov rbp, QWORD [rdi]
    mov rax, QWORD fs:40
    mov QWORD [rsp+88], rax
    xor eax, eax
    mov r12, QWORD [rbp+0]
    mov rdi, r12
    call QWORD [reloc_functions+448]
    mov r8, r12
    lea rcx, [LC40]
    cmp rax, 40
    jbe L326
    lea rdx, [LC26]
    mov esi, 128
    xor edi, edi
    xor eax, eax
    call [g_log]
    mov r14, QWORD [rbp+0]
    test r14, r14
    je L390
L328:
    xor eax, eax
    mov rdi, r14
    call QWORD [reloc_functions+448]
    cmp rax, 8
    jbe L331
    mov rax, 7958552634295728680
    cmp QWORD [r14], rax
    je L391
L331:
    xor eax, eax
    mov rdi, r14
    call QWORD [reloc_functions+448]
    cmp rax, 14
    jbe L357
    mov rax, 7306577436281289256
    cmp QWORD [r14], rax
    je L392
L357:
    xor eax, eax
    mov rdi, r14
    call QWORD [reloc_functions+448]
    cmp rax, 20
    jbe L360
    mov rax, 8389754637861337640
    xor rax, QWORD [r14]
    mov rdx, 8103433056861910113
    xor rdx, QWORD [r14+8]
    or rax, rdx
    je L393
L360:
    mov rdi, r14
    xor eax, eax
    call QWORD [reloc_functions+448]
    mov r8, r14
    lea rcx, [LC56]
    cmp rax, 40
    ja L387
L367:
    lea rdx, [LC27]
    mov esi, 128
    xor edi, edi
    xor eax, eax
    call [g_log]
L356:
    mov rdi, rbp
    mov esi, 1
    call [g_string_free]
    mov rdi, rbx
    call [g_free]
    mov rax, QWORD [rsp+88]
    sub rax, QWORD fs:40
    jne L394
    add rsp, 104
    xor eax, eax
    pop rbx
    pop rbp
    pop r12
    pop r13
    pop r14
    pop r15
    ret
L326:
    lea rdx, [LC27]
    mov esi, 128
    xor edi, edi
    xor eax, eax
    call [g_log]
    mov r14, QWORD [rbp+0]
    test r14, r14
    jne L328
L390:
    xor edi, edi
    lea rsi, [LC41]
    call [g_str_has_prefix]
    test eax, eax
    jne L329
    xor edi, edi
    lea rsi, [LC42]
    call [g_str_has_prefix]
    test eax, eax
    jne L334
    xor edi, edi
    lea rsi, [LC53]
    call [g_str_has_prefix]
    test eax, eax
    je L360
L366:
    xor eax, eax
    mov rdi, r14
    call QWORD [reloc_functions+448]
    cmp rax, 40
    jbe L395
    mov r8, r14
    lea rcx, [LC55]
L387:
    lea rdx, [LC26]
    mov esi, 128
    xor edi, edi
    xor eax, eax
    call [g_log]
    jmp L356
L391:
    cmp BYTE [r14+8], 32
    jne L331
L329:
    lea r12, [rsp+56]
    lea rax, [r14+9]
    mov rdi, r12
    mov QWORD [rsp+56], rax
    call static_next_token
    mov rdi, r12
    mov r13, rax
    call static_next_token
    mov r12, rax
    test r13, r13
    je L369
    test rax, rax
    je L369
    lea rsi, [rsp+64]
    mov edx, 10
    mov rdi, rax
    mov QWORD [rsp+64], 0
    call [g_ascii_strtoull]
    mov rsi, rax
    mov rax, QWORD [rsp+64]
    cmp BYTE [rax], 0
    jne L339
    lea rax, [rsi-1]
    mov edx, 4294967294
    cmp rdx, rax
    jb L339
    mov rdi, QWORD [g_swank_session_interactions_table]
    mov r15d, esi
    call [g_hash_table_lookup]
    mov r14, rax
    test rax, rax
    je L396
    xor eax, eax
    mov rdi, r13
    call QWORD [reloc_functions+448]
    cmp rax, 4
    jbe L343
    cmp DWORD [r13+0], 1802451496
    je L397
L343:
    xor eax, eax
    mov rdi, r13
    call QWORD [reloc_functions+448]
    cmp rax, 7
    jbe L353
    mov rax, 2338619929229605416
    cmp QWORD [r13+0], rax
    jne L353
    lea rax, [r13+8]
    lea rdi, [rsp+80]
    mov QWORD [rsp+80], rax
    call static_next_token
    mov rdi, rax
    test rax, rax
    je L353
    mov QWORD [rsp+16], rax
    call static_unescape_string
    mov rdi, QWORD [rsp+16]
    mov QWORD [rsp+8], rax
    call [g_free]
    mov r8, QWORD [rsp+8]
    xor eax, eax
    mov ecx, r15d
    lea rdx, [LC50]
    mov esi, 128
    xor edi, edi
    call [g_log]
    mov rdi, QWORD [r14+32]
    call [g_free]
    mov rax, QWORD [rsp+8]
    mov DWORD [r14+12], 3
    mov r8d, 3
    mov QWORD [r14+32], rax
L351:
    xor edi, edi
    mov ecx, r15d
    lea rdx, [LC51]
    xor eax, eax
    mov esi, 128
    call [g_log]
    mov rdi, QWORD [interactions_view_global]
    test rdi, rdi
    je L354
    mov rsi, r14
    call interactions_view_update_interaction
    jmp L389
L392:
    mov rax, 2338324173806657893
    cmp QWORD [r14+7], rax
    jne L357
L334:
    mov rdi, r14
    xor eax, eax
    call QWORD [reloc_functions+448]
    mov r8, r14
    lea rcx, [LC54]
    cmp rax, 40
    jbe L367
    jmp L387
L393:
    mov rax, 2334399943507211565
    cmp QWORD [r14+13], rax
    jne L360
    jmp L366
L339:
    mov rcx, r12
    lea rdx, [LC44]
L388:
    mov esi, 16
    xor edi, edi
    xor eax, eax
    call [g_log]
L389:
    mov rdi, r13
    call [g_free]
    mov rdi, r12
    call [g_free]
    jmp L356
L395:
    mov r8, r14
    lea rcx, [LC55]
    jmp L367
L353:
    mov rcx, r13
    lea rdx, [LC48]
    xor eax, eax
    xor edi, edi
    mov esi, 16
    call [g_log]
    mov DWORD [r14+12], 3
    mov rdi, QWORD [r14+32]
    call [g_free]
    lea rdi, [LC49]
    xor eax, eax
    call QWORD [reloc_functions+448]
    lea rdi, [LC49]
    call [g_strdup]
    mov r8d, DWORD [r14+12]
    mov QWORD [r14+32], rax
    jmp L351
L354:
    lea rdx, [LC52]
    mov esi, 16
    xor edi, edi
    xor eax, eax
    call [g_log]
    jmp L389
L397:
    cmp BYTE [r13+4], 32
    jne L343
    lea rax, [r13+5]
    lea rdi, [rsp+72]
    mov QWORD [rsp+72], rax
    call static_next_token
    mov QWORD [rsp+24], rax
    test rax, rax
    je L343
    cmp BYTE [rax], 40
    jne L348
    xor eax, eax
    mov rdi, QWORD [rsp+24]
    call QWORD [reloc_functions+448]
    mov rsi, QWORD [rsp+24]
    cmp BYTE -1[rsi+rax], 41
    je L398
L348:
    mov rdi, QWORD [rsp+24]
    call [g_free]
    jmp L343
L396:
    mov ecx, r15d
    lea rdx, [LC45]
    xor edi, edi
    xor eax, eax
    mov esi, 16
    call [g_log]
    mov rdi, r13
    call [g_free]
    mov rdi, r12
    call [g_free]
    jmp L356
L398:
    mov rax, rsi
    lea rdi, [rsp+80]
    add rax, 1
    mov QWORD [rsp+8], rdi
    mov QWORD [rsp+80], rax
    call static_next_token
    mov rdi, QWORD [rsp+8]
    mov QWORD [rsp+32], rax
    call static_next_token
    cmp QWORD [rsp+32], 0
    mov QWORD [rsp+40], rax
    je L370
    test rax, rax
    je L370
    mov rdi, QWORD [rsp+32]
    call static_unescape_string
    mov rdi, QWORD [rsp+40]
    mov QWORD [rsp+8], rax
    call static_unescape_string
    mov rdi, QWORD [rsp+24]
    mov QWORD [rsp+16], rax
    call [g_free]
    mov rdi, QWORD [rsp+32]
    call [g_free]
    mov rdi, QWORD [rsp+40]
    call [g_free]
    mov r8, QWORD [rsp+8]
    mov ecx, r15d
    xor eax, eax
    mov r9, QWORD [rsp+16]
    lea rdx, [LC47]
    mov esi, 128
    xor edi, edi
    call [g_log]
    mov rdi, QWORD [r14+24]
    call [g_free]
    mov rax, QWORD [rsp+8]
    mov rdi, QWORD [r14+16]
    mov QWORD [r14+24], rax
    call [g_free]
    mov rax, QWORD [rsp+16]
    mov DWORD [r14+12], 2
    mov r8d, 2
    mov QWORD [r14+16], rax
    jmp L351
L394:
    call [__stack_chk_fail]
L369:
    mov rcx, r14
    lea rdx, [LC43]
    jmp L388
L370:
    mov rdi, QWORD [rsp+24]
    call [g_free]
    mov rdi, QWORD [rsp+32]
    call [g_free]
    mov rdi, QWORD [rsp+40]
    call [g_free]
    jmp L343
LC57:
    db `process_global_set_stdout_cb`, 0
LC58:
    db `process-stdout`, 0
process_global_set_stdout_cb:
    endbr64
    push rbp
    lea rdx, [LC57]
    mov rbp, rsi
    xor eax, eax
    push rbx
    mov esi, 128
    mov rbx, rdi
    xor edi, edi
    sub rsp, 8
    call [g_log]
    mov QWORD [g_process_out_cb], rbx
    mov QWORD [g_process_out_user_data], rbp
    test rbx, rbx
    je L399
    mov edx, DWORD [g_process_started]
    test edx, edx
    je L399
    cmp QWORD [g_process_out_thread], 0
    je L405
L399:
    add rsp, 8
    pop rbx
    pop rbp
    ret
L405:
    mov eax, DWORD [g_process_out_fd]
    test eax, eax
    js L399
    xor edx, edx
    lea rsi, [stdout_thread_global]
    lea rdi, [LC58]
    xor eax, eax
    call QWORD [reloc_functions+560]
    mov QWORD [g_process_out_thread], rax
    jmp L399
LC59:
    db `process_global_set_stderr_cb`, 0
LC60:
    db `process-stderr`, 0
process_global_set_stderr_cb:
    endbr64
    push rbp
    lea rdx, [LC59]
    mov rbp, rsi
    xor eax, eax
    push rbx
    mov esi, 128
    mov rbx, rdi
    xor edi, edi
    sub rsp, 8
    call [g_log]
    mov QWORD [g_process_err_cb], rbx
    mov QWORD [g_process_err_user_data], rbp
    test rbx, rbx
    je L406
    mov edx, DWORD [g_process_started]
    test edx, edx
    je L406
    cmp QWORD [g_process_err_thread], 0
    je L412
L406:
    add rsp, 8
    pop rbx
    pop rbp
    ret
L412:
    mov eax, DWORD [g_process_err_fd]
    test eax, eax
    js L406
    xor edx, edx
    lea rsi, [stderr_thread_global]
    lea rdi, [LC60]
    xor eax, eax
    call QWORD [reloc_functions+560]
    mov QWORD [g_process_err_thread], rax
    jmp L406
LC61:
    db `Unknown error`, 0
LC62:
    db `process_global_start`, 0
LC63:
    db `process_global_start: Process already started.`, 0
LC64:
    db `process_global_start: No command (argv) to start.\n`, 0
LC65:
    db `process_global_start: g_spawn_async_with_pipes failed: %s\n`, 0
LC66:
    db `process_global_start: Spawned PID %d, in_fd=%d, out_fd=%d, err_fd=%d`, 0
process_global_start:
    endbr64
    push rbx
    xor edi, edi
    lea rdx, [LC62]
    mov esi, 128
    sub rsp, 16
    mov rax, QWORD fs:40
    mov QWORD [rsp+8], rax
    xor eax, eax
    call [g_log]
    mov edi, DWORD [g_process_started]
    test edi, edi
    jne L435
    mov rsi, QWORD [g_process_argv]
    test rsi, rsi
    je L416
    cmp QWORD [rsi], 0
    je L416
    mov QWORD [rsp], 0
    sub rsp, 8
    xor edx, edx
    xor edi, edi
    lea rbx, [rsp+8]
    xor r9d, r9d
    lea rax, [g_process_err_fd]
    mov ecx, 6
    push rbx
    lea r8, [child_setup_global]
    push rax
    lea rax, [g_process_out_fd]
    push rax
    lea rax, [g_process_in_fd]
    push rax
    lea rax, [g_process_pid]
    push rax
    xor eax, eax
    call QWORD [reloc_functions+632]
    mov rsp, rbx
    test eax, eax
    jne L419
    mov rax, QWORD [rsp]
    lea rsi, [LC61]
    test rax, rax
    je L420
    mov rsi, QWORD [rax+8]
L420:
    lea rdi, [LC65]
    xor eax, eax
    call [g_printerr]
    mov rdi, rbx
    xor eax, eax
    call QWORD [reloc_functions+8]
    mov rdi, QWORD [g_process_argv]
    call [g_strfreev]
    mov edi, DWORD [g_process_in_fd]
    mov QWORD [g_process_argv], 0
    test edi, edi
    jns L436
    mov edi, DWORD [g_process_out_fd]
    test edi, edi
    jns L437
L422:
    mov edi, DWORD [g_process_err_fd]
    test edi, edi
    jns L438
L413:
    mov rax, QWORD [rsp+8]
    sub rax, QWORD fs:40
    jne L434
    add rsp, 16
    pop rbx
    ret
L419:
    mov eax, DWORD [g_process_err_fd]
    sub rsp, 8
    mov esi, 128
    xor edi, edi
    lea rdx, [LC66]
    push rax
    mov ecx, DWORD [g_process_pid]
    xor eax, eax
    mov r9d, DWORD [g_process_out_fd]
    mov r8d, DWORD [g_process_in_fd]
    call [g_log]
    cmp QWORD [g_process_out_cb], 0
    mov DWORD [g_process_started], 1
    pop rcx
    pop rsi
    je L425
    cmp QWORD [g_process_out_thread], 0
    je L439
L425:
    cmp QWORD [g_process_err_cb], 0
    je L413
    cmp QWORD [g_process_err_thread], 0
    jne L413
    mov eax, DWORD [g_process_err_fd]
    test eax, eax
    js L413
    xor edx, edx
    lea rsi, [stderr_thread_global]
    lea rdi, [LC60]
    xor eax, eax
    call QWORD [reloc_functions+560]
    mov QWORD [g_process_err_thread], rax
    jmp L413
L416:
    mov rax, QWORD [rsp+8]
    sub rax, QWORD fs:40
    jne L434
    add rsp, 16
    lea rdi, [LC64]
    xor eax, eax
    pop rbx
    jmp [g_printerr]
L435:
    mov rax, QWORD [rsp+8]
    sub rax, QWORD fs:40
    jne L434
    add rsp, 16
    lea rdx, [LC63]
    xor edi, edi
    xor eax, eax
    mov esi, 16
    pop rbx
    jmp [g_log]
L438:
    call [close]
    mov DWORD [g_process_err_fd], -1
    jmp L413
L437:
    call [close]
    mov edi, DWORD [g_process_err_fd]
    mov DWORD [g_process_out_fd], -1
    test edi, edi
    js L413
    jmp L438
L436:
    call [close]
    mov edi, DWORD [g_process_out_fd]
    mov DWORD [g_process_in_fd], -1
    test edi, edi
    js L422
    jmp L437
L439:
    mov edx, DWORD [g_process_out_fd]
    test edx, edx
    js L425
    xor edx, edx
    lea rsi, [stdout_thread_global]
    lea rdi, [LC58]
    xor eax, eax
    call QWORD [reloc_functions+560]
    mov QWORD [g_process_out_thread], rax
    jmp L425
L434:
    call [__stack_chk_fail]
LC67:
    db `process_global_write: Process not started or input FD is invalid.`, 0
LC68:
    db `process_global_write: Writing %zd bytes to process stdin (fd %d)`, 0
LC69:
    db `process_global_write: write error to fd %d: %s (errno %d)\n`, 0
LC70:
    db `process_global_write: partial write to fd %d. Wrote %zd of %zd bytes.`, 0
process_global_write:
    endbr64
    push rbp
    push rbx
    sub rsp, 8
    mov eax, DWORD [g_process_started]
    test eax, eax
    je L441
    mov r8d, DWORD [g_process_in_fd]
    test r8d, r8d
    js L441
    mov rbp, rdi
    mov rbx, rsi
    test rsi, rsi
    js L450
L444:
    xor eax, eax
    mov rcx, rbx
    lea rdx, [LC68]
    xor edi, edi
    mov esi, 128
    call [g_log]
    movsx rax, DWORD [g_process_in_fd]
    mov rdi, rax
    mov rsi, rbp
    mov rdx, rbx
    mov rax, $1
    syscall
    mov rbp, rax
    test rax, rax
    js L445
    cmp rbx, rax
    jg L451
L447:
    xor eax, eax
    cmp rbx, rbp
    sete al
    add rsp, 8
    pop rbx
    pop rbp
    ret
L450:
    xor eax, eax
    call QWORD [reloc_functions+448]
    mov r8d, DWORD [g_process_in_fd]
    mov rbx, rax
    jmp L444
L441:
    xor eax, eax
    lea rdx, [LC67]
    mov esi, 16
    xor edi, edi
    call [g_log]
    add rsp, 8
    xor eax, eax
    pop rbx
    pop rbp
    ret
L451:
    mov ecx, DWORD [g_process_in_fd]
    mov r8, rax
    mov r9, rbx
    xor edi, edi
    lea rdx, [LC70]
    mov esi, 16
    xor eax, eax
    call [g_log]
    jmp L447
L445:
    call [__errno_location]
    neg ebp
    mov DWORD [rax], ebp
    mov edi, ebp
    xor eax, eax
    call QWORD [reloc_functions+776]
    mov esi, DWORD [g_process_in_fd]
    mov ecx, ebp
    lea rdi, [LC69]
    mov rdx, rax
    xor eax, eax
    mov rbp, -1
    call [g_printerr]
    jmp L447
LC71:
    db `process_cleanup_globals: Cleaning up global process resources.`, 0
LC72:
    db `process_cleanup_globals: Closing in_fd %d`, 0
LC73:
    db `process_cleanup_globals: Closing out_fd %d`, 0
LC74:
    db `process_cleanup_globals: Closing err_fd %d`, 0
LC75:
    db `process_cleanup_globals: Joining stdout_thread`, 0
LC76:
    db `process_cleanup_globals: Joining stderr_thread`, 0
LC77:
    db `process_cleanup_globals: Closing PID %d`, 0
LC78:
    db `process_cleanup_globals: Cleanup complete.`, 0
process_cleanup_globals:
    endbr64
    sub rsp, 8
    lea rdx, [LC71]
    xor edi, edi
    xor eax, eax
    mov esi, 128
    call [g_log]
    mov ecx, DWORD [g_process_in_fd]
    test ecx, ecx
    js L453
    lea rdx, [LC72]
    mov esi, 128
    xor edi, edi
    xor eax, eax
    call [g_log]
    mov edi, DWORD [g_process_in_fd]
    call [close]
    mov DWORD [g_process_in_fd], -1
L453:
    mov ecx, DWORD [g_process_out_fd]
    test ecx, ecx
    js L454
    lea rdx, [LC73]
    mov esi, 128
    xor edi, edi
    xor eax, eax
    call [g_log]
    mov edi, DWORD [g_process_out_fd]
    call [close]
    mov DWORD [g_process_out_fd], -1
L454:
    mov ecx, DWORD [g_process_err_fd]
    test ecx, ecx
    js L455
    lea rdx, [LC74]
    mov esi, 128
    xor edi, edi
    xor eax, eax
    call [g_log]
    mov edi, DWORD [g_process_err_fd]
    call [close]
    mov DWORD [g_process_err_fd], -1
L455:
    cmp QWORD [g_process_out_thread], 0
    je L456
    lea rdx, [LC75]
    mov esi, 128
    xor edi, edi
    xor eax, eax
    call [g_log]
    mov rdi, QWORD [g_process_out_thread]
    call [g_thread_join]
    mov QWORD [g_process_out_thread], 0
L456:
    cmp QWORD [g_process_err_thread], 0
    je L457
    lea rdx, [LC76]
    mov esi, 128
    xor edi, edi
    xor eax, eax
    call [g_log]
    mov rdi, QWORD [g_process_err_thread]
    call [g_thread_join]
    mov QWORD [g_process_err_thread], 0
L457:
    mov ecx, DWORD [g_process_pid]
    test ecx, ecx
    jle L458
    lea rdx, [LC77]
    mov esi, 128
    xor edi, edi
    xor eax, eax
    call [g_log]
    mov edi, DWORD [g_process_pid]
    call [g_spawn_close_pid]
    mov DWORD [g_process_pid], 0
L458:
    mov rdi, QWORD [g_process_argv]
    call [g_strfreev]
    lea rdx, [LC78]
    xor edi, edi
    xor eax, eax
    mov QWORD [g_process_argv], 0
    mov esi, 128
    mov DWORD [g_process_started], 0
    mov QWORD [g_process_out_cb], 0
    mov QWORD [g_process_err_cb], 0
    mov QWORD [g_process_out_user_data], 0
    mov QWORD [g_process_err_user_data], 0
    add rsp, 8
    jmp [g_log]
LC79:
    db `(null)`, 0
LC80:
    db `process_init_globals_from_argv: cmd=%s`, 0
LC81:
    db `process_init_globals_from_argv: Process already initialized or started. Cleaning up old one.`, 0
process_init_globals_from_argv:
    endbr64
    push rbx
    lea rcx, [LC79]
    mov rbx, rdi
    test rdi, rdi
    je L461
    mov rcx, QWORD [rdi]
    lea rax, [LC79]
    test rcx, rcx
    cmove rcx, rax
L461:
    xor eax, eax
    lea rdx, [LC80]
    mov esi, 128
    xor edi, edi
    call [g_log]
    mov eax, DWORD [g_process_started]
    test eax, eax
    jne L467
L462:
    mov rdi, QWORD [g_process_argv]
    call [g_strfreev]
    mov rdi, rbx
    call [g_strdupv]
    pop rbx
    mov DWORD [g_process_pid], 0
    mov QWORD [g_process_argv], rax
    mov DWORD [g_process_in_fd], -1
    mov DWORD [g_process_out_fd], -1
    mov DWORD [g_process_err_fd], -1
    mov QWORD [g_process_out_cb], 0
    mov QWORD [g_process_out_user_data], 0
    mov QWORD [g_process_err_cb], 0
    mov QWORD [g_process_err_user_data], 0
    mov QWORD [g_process_out_thread], 0
    mov QWORD [g_process_err_thread], 0
    mov DWORD [g_process_started], 0
    ret
L467:
    xor eax, eax
    lea rdx, [LC81]
    mov esi, 16
    xor edi, edi
    call [g_log]
    xor eax, eax
    call process_cleanup_globals
    jmp L462
LC82:
    db `process_init_globals: cmd=%s`, 0
LC83:
    db `process_init_globals: cmd is NULL.`, 0
process_init_globals:
    endbr64
    push rbx
    sub rsp, 32
    mov rax, QWORD fs:40
    mov QWORD [rsp+24], rax
    xor eax, eax
    test rdi, rdi
    je L469
    mov rbx, rdi
    mov rcx, rdi
    mov esi, 128
    xor edi, edi
    lea rdx, [LC82]
    call [g_log]
    mov rdi, rsp
    mov QWORD [rsp], rbx
    mov QWORD [rsp+8], 0
    call process_init_globals_from_argv
    mov rax, QWORD [rsp+24]
    sub rax, QWORD fs:40
    jne L473
    add rsp, 32
    pop rbx
    ret
L469:
    xor edi, edi
    xor eax, eax
    lea rdx, [LC82]
    mov esi, 128
    lea rcx, [LC79]
    call [g_log]
    xor edi, edi
    xor eax, eax
    lea rdx, [LC83]
    mov esi, 16
    call [g_log]
    mov rax, QWORD [rsp+24]
    sub rax, QWORD fs:40
    jne L473
    add rsp, 32
    xor edi, edi
    pop rbx
    jmp process_init_globals_from_argv
L473:
    call [__stack_chk_fail]
LC84:
    db `swank_process_global_send: Swank process not started or FD invalid.`, 0
LC85:
    db `%06zx`, 0
LC86:
    db `swank_process_global_send: Sending Swank message: Header='%s', Payload='%.*s'`, 0
LC87:
    db `swank_process_global_send: Failed to write Swank header (wrote %zd, errno %d)\n`, 0
LC88:
    db `swank_process_global_send: Failed to write Swank payload (wrote %zd of %zu, errno %d)\n`, 0
LC89:
    db `swank_process_global_send: Message sent successfully.`, 0
swank_process_global_send:
    endbr64
    push r12
    push rbp
    push rbx
    sub rsp, 16
    mov edx, DWORD [g_swank_process_started]
    mov rax, QWORD fs:40
    mov QWORD [rsp+8], rax
    xor eax, eax
    test edx, edx
    je L476
    mov eax, DWORD [g_swank_fd]
    test eax, eax
    js L476
    mov r12, QWORD [rdi+8]
    lea rbp, [rsp+1]
    mov rbx, rdi
    xor eax, eax
    mov rdi, rbp
    lea rdx, [LC85]
    mov esi, 7
    mov rcx, r12
    call [g_snprintf]
    mov r9, QWORD [rbx]
    mov r8d, r12d
    xor eax, eax
    mov rcx, rbp
    lea rdx, [LC86]
    mov esi, 128
    xor edi, edi
    call [g_log]
    movsx rax, DWORD [g_swank_fd]
    mov r8d, 6
    mov rdi, rax
    mov rsi, rbp
    mov rdx, r8
    mov rax, $1
    syscall
    mov rbp, rax
    test rax, rax
    js L489
    cmp rax, 6
    je L481
    call [__errno_location]
    mov edx, DWORD [rax]
L480:
    mov rsi, rbp
    lea rdi, [LC87]
    xor eax, eax
    call [g_printerr]
L475:
    mov rax, QWORD [rsp+8]
    sub rax, QWORD fs:40
    jne L488
    add rsp, 16
    pop rbx
    pop rbp
    pop r12
    ret
L476:
    mov rax, QWORD [rsp+8]
    sub rax, QWORD fs:40
    jne L488
    add rsp, 16
    lea rdx, [LC84]
    xor edi, edi
    xor eax, eax
    pop rbx
    mov esi, 16
    pop rbp
    pop r12
    jmp [g_log]
L481:
    movsx rax, DWORD [g_swank_fd]
    mov r8, QWORD [rbx]
    mov rdi, rax
    mov rsi, r8
    mov rdx, r12
    mov rax, $1
    syscall
    mov rbx, rax
    test rax, rax
    js L490
L483:
    cmp r12, rbx
    je L484
    call [__errno_location]
    mov rdx, r12
    mov rsi, rbx
    lea rdi, [LC88]
    mov ecx, DWORD [rax]
    xor eax, eax
    call [g_printerr]
    jmp L475
L484:
    lea rdx, [LC89]
    mov esi, 128
    xor edi, edi
    xor eax, eax
    call [g_log]
    jmp L475
L489:
    call [__errno_location]
    mov edx, ebp
    mov rbp, -1
    neg edx
    mov DWORD [rax], edx
    jmp L480
L490:
    call [__errno_location]
    neg ebx
    mov DWORD [rax], ebx
    mov rbx, -1
    jmp L483
L488:
    call [__stack_chk_fail]
LC90:
    db `swank_process_global_set_message_cb`, 0
swank_process_global_set_message_cb:
    endbr64
    push r12
    lea rdx, [LC90]
    lea r12, [g_swank_incoming_mutex]
    xor eax, eax
    push rbp
    mov rbp, rdi
    xor edi, edi
    push rbx
    mov rbx, rsi
    mov esi, 128
    call [g_log]
    mov rdi, r12
    xor eax, eax
    call QWORD [reloc_functions+680]
    mov QWORD [g_swank_message_cb], rbp
    mov rdi, r12
    xor eax, eax
    mov QWORD [g_swank_message_cb_data], rbx
    pop rbx
    pop rbp
    pop r12
    jmp QWORD [reloc_functions+688]
LC91:
    db `swank_process_global_set_socket_fd: Setting Swank FD to %d`, 0
LC92:
    db `swank_process_global_set_socket_fd: Closing existing Swank FD %d`, 0
LC93:
    db `swank_process_global_set_socket_fd: Existing reader thread found. It might need manual restart.`, 0
swank_process_global_set_socket_fd:
    endbr64
    push rbx
    mov ecx, edi
    mov ebx, edi
    lea rdx, [LC91]
    mov esi, 128
    xor edi, edi
    xor eax, eax
    call [g_log]
    mov ecx, DWORD [g_swank_fd]
    cmp ecx, ebx
    je L496
    test ecx, ecx
    jns L506
L496:
    cmp QWORD [g_swank_reader_thread], 0
    mov DWORD [g_swank_fd], ebx
    je L493
L507:
    lea rdx, [LC93]
    mov esi, 128
    xor edi, edi
    xor eax, eax
    pop rbx
    jmp [g_log]
L506:
    lea rdx, [LC92]
    mov esi, 16
    xor edi, edi
    xor eax, eax
    call [g_log]
    mov edi, DWORD [g_swank_fd]
    call [close]
    mov rdi, QWORD [g_swank_connection]
    test rdi, rdi
    je L496
    xor eax, eax
    call QWORD [reloc_functions+648]
    cmp QWORD [g_swank_reader_thread], 0
    mov QWORD [g_swank_connection], 0
    mov DWORD [g_swank_fd], ebx
    jne L507
L493:
    pop rbx
    ret
LC94:
    db `swank_process_cleanup_globals: Starting cleanup.`, 0
LC95:
    db `swank_process_cleanup_globals: Closing Swank FD %d.`, 0
LC96:
    db `Error closing GSocketConnection: %s`, 0
LC97:
    db `swank_process_cleanup_globals: Joining Swank reader thread.`, 0
LC98:
    db `swank_process_cleanup_globals: Cleanup complete.`, 0
swank_process_cleanup_globals:
    endbr64
    sub rsp, 24
    lea rdx, [LC94]
    mov esi, 128
    xor edi, edi
    mov rax, QWORD fs:40
    mov QWORD [rsp+8], rax
    xor eax, eax
    call [g_log]
    mov ecx, DWORD [g_swank_fd]
    test ecx, ecx
    jns L520
L509:
    cmp QWORD [g_swank_reader_thread], 0
    je L513
    lea rdx, [LC97]
    mov esi, 128
    xor edi, edi
    xor eax, eax
    call [g_log]
    mov rdi, QWORD [g_swank_reader_thread]
    call [g_thread_join]
    mov QWORD [g_swank_reader_thread], 0
L513:
    mov rdi, QWORD [g_swank_out_buffer]
    mov esi, 1
    call [g_string_free]
    mov rdi, QWORD [g_swank_incoming_data_buffer]
    mov esi, 1
    mov QWORD [g_swank_out_buffer], 0
    call [g_string_free]
    lea rdi, [g_swank_out_mutex]
    xor eax, eax
    mov QWORD [g_swank_incoming_data_buffer], 0
    call QWORD [reloc_functions+704]
    lea rdi, [g_swank_out_cond]
    xor eax, eax
    call QWORD [reloc_functions+712]
    lea rdi, [g_swank_incoming_mutex]
    xor eax, eax
    call QWORD [reloc_functions+704]
    mov QWORD [g_swank_message_cb], 0
    mov QWORD [g_swank_message_cb_data], 0
    mov DWORD [g_swank_process_started], 0
    mov rax, QWORD [rsp+8]
    sub rax, QWORD fs:40
    jne L521
    lea rdx, [LC98]
    mov esi, 128
    xor edi, edi
    xor eax, eax
    add rsp, 24
    jmp [g_log]
L520:
    xor edi, edi
    lea rdx, [LC95]
    mov esi, 128
    xor eax, eax
    call [g_log]
    mov rdi, QWORD [g_swank_connection]
    test rdi, rdi
    je L510
    mov rdx, rsp
    xor esi, esi
    mov QWORD [rsp], 0
    call [g_io_stream_close]
    mov rax, QWORD [rsp]
    test rax, rax
    je L511
    mov rcx, QWORD [rax+8]
    xor edi, edi
    lea rdx, [LC96]
    xor eax, eax
    mov esi, 16
    call [g_log]
    mov rdi, QWORD [rsp]
    call [g_error_free]
L511:
    mov rdi, QWORD [g_swank_connection]
    xor eax, eax
    call QWORD [reloc_functions+648]
    mov QWORD [g_swank_connection], 0
L512:
    mov DWORD [g_swank_fd], -1
    jmp L509
L510:
    mov edi, DWORD [g_swank_fd]
    call [close]
    jmp L512
L521:
    call [__stack_chk_fail]
LC99:
    db `swank_process_init_globals`, 0
LC100:
    db `swank_process_init_globals: Already initialized. Cleaning up old state.`, 0
LC101:
    db `swank_process_init_globals: complete. Port: %d`, 0
swank_process_init_globals:
    endbr64
    xor eax, eax
    sub rsp, 8
    lea rdx, [LC99]
    xor edi, edi
    mov esi, 128
    call [g_log]
    mov eax, DWORD [g_swank_process_started]
    test eax, eax
    jne L525
L523:
    lea rdi, [g_swank_out_mutex]
    xor eax, eax
    call QWORD [reloc_functions+696]
    lea rdi, [g_swank_out_cond]
    xor eax, eax
    call QWORD [reloc_functions+728]
    lea rdi, [g_swank_incoming_mutex]
    xor eax, eax
    call QWORD [reloc_functions+696]
    xor edi, edi
    xor eax, eax
    call QWORD [reloc_functions+584]
    xor edi, edi
    mov QWORD [g_swank_out_consumed], 0
    mov QWORD [g_swank_out_buffer], rax
    xor eax, eax
    call QWORD [reloc_functions+584]
    xor esi, esi
    lea rdi, [on_lisp_stdout]
    mov QWORD [g_swank_incoming_consumed], 0
    mov QWORD [g_swank_incoming_data_buffer], rax
    mov DWORD [g_swank_fd], -1
    mov QWORD [g_swank_connection], 0
    mov QWORD [g_swank_message_cb], 0
    mov QWORD [g_swank_message_cb_data], 0
    mov QWORD [g_swank_reader_thread], 0
    mov DWORD [g_swank_port_number], 4005
    call process_global_set_stdout_cb
    xor esi, esi
    lea rdi, [on_lisp_stderr]
    call process_global_set_stderr_cb
    mov ecx, DWORD [g_swank_port_number]
    xor edi, edi
    xor eax, eax
    mov DWORD [g_swank_process_started], 0
    lea rdx, [LC101]
    mov esi, 128
    add rsp, 8
    jmp [g_log]
L525:
    xor eax, eax
    lea rdx, [LC100]
    mov esi, 16
    xor edi, edi
    call [g_log]
    xor eax, eax
    call swank_process_cleanup_globals
    jmp L523
LC102:
    db `swank_process_global_start`, 0
LC103:
    db `swank_process_global_start: Swank process already started.`, 0
LC104:
    db `swank_process: start_lisp_and_swank_server`, 0
LC105:
    db `* `, 0
LC106:
    db `(require :swank)\n`, 0
LC107:
    db `swank_process: Sending Lisp command: %s`, 0
LC108:
    db `)`, 0
LC109:
    db `(swank:create-server :port %d :dont-close t)\n`, 0
LC110:
    db `swank_process: Swank server presumed started on Lisp side.`, 0
LC111:
    db `swank_process: connect_to_swank_server trying port %d`, 0
LC112:
    db `127.0.0.1`, 0
LC113:
    db `swank_process: Connection attempt %d failed: %s. Retrying...`, 0
LC114:
    db `swank_process: Failed to connect to Swank server on port %d after multiple retries.\n`, 0
LC115:
    db `swank_process: Connected to Swank server. FD: %d`, 0
LC116:
    db `swank-reader`, 0
LC117:
    db `swank_process_global_start: Swank process started successfully.`, 0
LC118:
    db `swank_process_global_start: Failed to start Swank process (connection failed).\n`, 0
swank_process_global_start:
    endbr64
    push r15
    lea rdx, [LC102]
    mov esi, 128
    xor edi, edi
    push r14
    push r13
    push r12
    push rbp
    push rbx
    sub rsp, 168
    mov rax, QWORD fs:40
    mov QWORD [rsp+152], rax
    xor eax, eax
    call [g_log]
    mov ebx, DWORD [g_swank_process_started]
    test ebx, ebx
    jne L552
    lea rdx, [LC104]
    mov esi, 128
    xor edi, edi
    xor eax, eax
    call [g_log]
    lea rbp, [LC105]
    xor eax, eax
    lea r12, [LC106]
    call process_global_start
    lea r13, [LC107]
    mov rdi, rbp
    lea r15, [LC61]
    call read_until_from_lisp_output
    mov rcx, r12
    mov rdx, r13
    mov esi, 128
    xor edi, edi
    xor eax, eax
    lea r14, [LC113]
    call [g_log]
    mov rsi, -1
    mov rdi, r12
    lea r12, [rsp+16]
    call process_global_write
    lea rdi, [LC108]
    call read_until_from_lisp_output
    mov rdi, rbp
    call read_until_from_lisp_output
    mov ecx, DWORD [g_swank_port_number]
    mov rdi, r12
    xor eax, eax
    lea rdx, [LC109]
    mov esi, 128
    call [g_snprintf]
    mov rcx, r12
    mov rdx, r13
    mov esi, 128
    xor edi, edi
    xor eax, eax
    lea r13, [LC112]
    call [g_log]
    mov rsi, -1
    mov rdi, r12
    call process_global_write
    mov rdi, rbp
    lea rbp, [rsp+8]
    call read_until_from_lisp_output
    lea rdx, [LC110]
    xor edi, edi
    xor eax, eax
    mov esi, 128
    call [g_log]
    mov ecx, DWORD [g_swank_port_number]
    xor edi, edi
    xor eax, eax
    lea rdx, [LC111]
    mov esi, 128
    call [g_log]
    xor eax, eax
    call QWORD [reloc_functions+592]
    mov QWORD [rsp+8], 0
    mov r12, rax
L533:
    movzx edx, WORD [g_swank_port_number]
    mov r8, rbp
    xor ecx, ecx
    mov rsi, r13
    mov rdi, r12
    xor eax, eax
    call QWORD [reloc_functions+544]
    mov QWORD [g_swank_connection], rax
    test rax, rax
    jne L534
    mov rax, QWORD [rsp+8]
    mov r8, r15
    test rax, rax
    je L532
    mov r8, QWORD [rax+8]
L532:
    add ebx, 1
    xor edi, edi
    xor eax, eax
    mov rdx, r14
    mov ecx, ebx
    mov esi, 128
    call [g_log]
    mov rdi, rbp
    xor eax, eax
    call QWORD [reloc_functions+8]
    xor eax, eax
    mov edi, 500000
    call QWORD [reloc_functions+816]
    cmp ebx, 10
    jne L533
L534:
    mov rdi, r12
    xor eax, eax
    call QWORD [reloc_functions+648]
    mov rdi, QWORD [g_swank_connection]
    test rdi, rdi
    je L553
    xor eax, eax
    call QWORD [reloc_functions+536]
    mov rdi, rax
    xor eax, eax
    call QWORD [reloc_functions+576]
    lea rdx, [LC115]
    mov esi, 128
    xor edi, edi
    mov DWORD [g_swank_fd], eax
    mov ecx, eax
    xor eax, eax
    call [g_log]
    mov eax, DWORD [g_swank_fd]
    test eax, eax
    js L538
    cmp QWORD [g_swank_reader_thread], 0
    je L554
L540:
    mov DWORD [g_swank_process_started], 1
    mov rax, QWORD [rsp+152]
    sub rax, QWORD fs:40
    jne L551
    lea rdx, [LC117]
    mov esi, 128
L550:
    add rsp, 168
    xor edi, edi
    xor eax, eax
    pop rbx
    pop rbp
    pop r12
    pop r13
    pop r14
    pop r15
    jmp [g_log]
L554:
    xor edx, edx
    lea rsi, [swank_reader_thread_global]
    lea rdi, [LC116]
    xor eax, eax
    call QWORD [reloc_functions+560]
    mov QWORD [g_swank_reader_thread], rax
L549:
    mov eax, DWORD [g_swank_fd]
    test eax, eax
    jns L540
L538:
    xor eax, eax
    lea rdi, [LC118]
    call [g_printerr]
    mov rax, QWORD [rsp+152]
    sub rax, QWORD fs:40
    jne L551
    add rsp, 168
    xor eax, eax
    pop rbx
    pop rbp
    pop r12
    pop r13
    pop r14
    pop r15
    jmp swank_process_cleanup_globals
L552:
    mov rax, QWORD [rsp+152]
    sub rax, QWORD fs:40
    jne L551
    lea rdx, [LC103]
    mov esi, 16
    jmp L550
L553:
    mov esi, DWORD [g_swank_port_number]
    xor eax, eax
    lea rdi, [LC114]
    call [g_printerr]
    cmp QWORD [rsp+8], 0
    je L549
    mov rdi, rbp
    xor eax, eax
    call QWORD [reloc_functions+8]
    jmp L549
L551:
    call [__stack_chk_fail]
LC119:
    db `swank_session_init_globals`, 0
LC120:
    db `swank_session_init_globals: Already initialized. Cleaning up old state.`, 0
LC121:
    db `swank_session_cleanup_globals`, 0
LC122:
    db `swank_session_init_globals: Complete. Registered message callback.`, 0
swank_session_init_globals:
    endbr64
    push rbx
    xor edi, edi
    xor eax, eax
    lea rdx, [LC119]
    mov esi, 128
    call [g_log]
    cmp QWORD [g_swank_session_interactions_table], 0
    je L557
    lea rdx, [LC120]
    mov esi, 16
    xor edi, edi
    xor eax, eax
    call [g_log]
    xor edi, edi
    lea rdx, [LC121]
    xor eax, eax
    mov esi, 128
    call [g_log]
    mov rdi, QWORD [g_swank_session_interactions_table]
    test rdi, rdi
    je L557
    call [g_hash_table_destroy]
    mov QWORD [g_swank_session_interactions_table], 0
L557:
    mov rsi, QWORD [g_direct_equal]
    mov rdi, QWORD [g_direct_hash]
    lea rcx, [interaction_free_members_static]
    xor edx, edx
    mov DWORD [g_swank_session_started], 0
    lea rbx, [g_swank_incoming_mutex]
    mov DWORD [g_swank_session_next_tag], 1
    call [g_hash_table_new_full]
    lea rdx, [LC90]
    mov esi, 128
    xor edi, edi
    mov QWORD [g_swank_session_interactions_table], rax
    xor eax, eax
    call [g_log]
    mov rdi, rbx
    xor eax, eax
    call QWORD [reloc_functions+680]
    lea rax, [swank_session_on_message_internal]
    mov rdi, rbx
    mov QWORD [g_swank_message_cb_data], 0
    mov QWORD [g_swank_message_cb], rax
    xor eax, eax
    call QWORD [reloc_functions+688]
    lea rdx, [LC122]
    xor edi, edi
    xor eax, eax
    mov esi, 128
    pop rbx
    jmp [g_log]
LC123:
    db `swank_session_global_eval: NULL interaction or expression.`, 0
LC124:
    db `swank_session_global_eval: expr='%s'`, 0
LC125:
    db `Interaction %u added (expression: %s)`, 0
LC126:
    db `swank_session_global_eval: interactions_view_global is NULL. Cannot add interaction to view.`, 0
LC127:
    db `\\\\`, 0
LC128:
    db `\\\"`, 0
LC129:
    db `(:emacs-rex (swank:eval-and-grab-output \"%s\") \"COMMON-LISP-USER\" t %u)`, 0
swank_session_global_eval:
    endbr64
    test rdi, rdi
    je L592
    push r15
    push r14
    push r13
    push r12
    push rbp
    mov rbp, rdi
    push rbx
    sub rsp, 8
    mov rcx, QWORD [rdi]
    test rcx, rcx
    je L593
    mov esi, 128
    lea rdx, [LC124]
    xor edi, edi
    xor eax, eax
    call [g_log]
    mov esi, DWORD [g_swank_session_started]
    test esi, esi
    je L594
L566:
    mov esi, DWORD [g_swank_session_next_tag]
    mov DWORD [rbp+12], 1
    mov rdx, rbp
    mov rdi, QWORD [g_swank_session_interactions_table]
    mov DWORD [rbp+8], esi
    lea eax, [rsi+1]
    mov DWORD [g_swank_session_next_tag], eax
    call [g_hash_table_insert]
    mov ecx, DWORD [rbp+8]
    mov r8, QWORD [rbp+0]
    xor edi, edi
    lea rdx, [LC125]
    mov esi, 128
    xor eax, eax
    call [g_log]
    mov rdi, QWORD [interactions_view_global]
    test rdi, rdi
    je L567
    mov rsi, rbp
    call interactions_view_add_interaction
L568:
    mov r12, QWORD [rbp+0]
    xor edi, edi
    xor eax, eax
    call QWORD [reloc_functions+584]
    movsx edx, BYTE [r12]
    mov rbx, rax
    test dl, dl
    je L569
    lea r15, [LC128+2]
    lea r14, [r15-2]
    jmp L582
L596:
    cmp dl, 92
    jne L571
    test rbx, rbx
    je L595
    mov rax, QWORD [rbx+8]
    lea rdx, [rax+2]
    cmp rdx, QWORD [rbx+16]
    jb L574
    mov ecx, 2
    lea rdx, [LC127]
    mov rdi, rbx
    xor eax, eax
    mov rsi, -1
    call QWORD [reloc_functions+552]
L573:
    movsx edx, BYTE [r12+1]
    add r12, 1
    test dl, dl
    je L569
L582:
    cmp dl, 34
    jne L596
    test rbx, rbx
    je L597
    mov rax, QWORD [rbx+8]
    lea rdx, [rax+2]
    cmp rdx, QWORD [rbx+16]
    jb L578
    add r12, 1
    mov rdx, r14
    mov ecx, 2
    mov rdi, rbx
    mov rsi, -1
    xor eax, eax
    call QWORD [reloc_functions+552]
    movsx edx, BYTE [r12]
    test dl, dl
    jne L582
L569:
    mov rdi, rbx
    xor eax, eax
    call QWORD [reloc_functions+616]
    mov edx, DWORD [rbp+8]
    lea rdi, [LC129]
    mov rsi, rax
    mov rbx, rax
    xor eax, eax
    call [g_strdup_printf]
    mov rdi, rbx
    mov rbp, rax
    call [g_free]
    mov rdi, rbp
    xor eax, eax
    call QWORD [reloc_functions+584]
    mov rdi, rbp
    mov rbx, rax
    call [g_free]
    mov rdi, rbx
    call swank_process_global_send
    add rsp, 8
    mov rdi, rbx
    mov esi, 1
    pop rbx
    pop rbp
    pop r12
    pop r13
    pop r14
    pop r15
    jmp [g_string_free]
L594:
    xor eax, eax
    call swank_process_global_start
    mov DWORD [g_swank_session_started], 1
    jmp L566
L571:
    test rbx, rbx
    je L581
    mov rax, QWORD [rbx+8]
    lea rcx, [rax+1]
    cmp rcx, QWORD [rbx+16]
    jnb L581
    mov rsi, QWORD [rbx]
    mov QWORD [rbx+8], rcx
    mov BYTE [rsi+rax], dl
    mov rdx, QWORD [rbx]
    mov rax, QWORD [rbx+8]
    mov BYTE [rdx+rax], 0
    jmp L573
L581:
    mov rsi, -1
    mov rdi, rbx
    xor eax, eax
    call QWORD [reloc_functions+600]
    jmp L573
L574:
    add rax, QWORD [rbx]
    mov ecx, 23644
    mov WORD [rax], cx
L580:
    mov rax, QWORD [rbx+8]
    lea rdx, [rax+2]
    mov QWORD [rbx+8], rdx
    mov rdx, QWORD [rbx]
    mov BYTE 2[rdx+rax], 0
    jmp L573
L578:
    add rax, QWORD [rbx]
    mov edx, 8796
    mov WORD [rax], dx
    jmp L580
L593:
    xor edi, edi
    lea rdx, [LC123]
    mov esi, 16
    xor eax, eax
    call [g_log]
    add rsp, 8
    mov rdi, rbp
    pop rbx
    pop rbp
    pop r12
    pop r13
    pop r14
    pop r15
    jmp [g_free]
L567:
    lea rdx, [LC126]
    mov esi, 16
    xor edi, edi
    xor eax, eax
    call [g_log]
    jmp L568
L597:
    mov edx, 2
    lea rsi, [LC128]
    xor edi, edi
    call [g_string_append_len]
    jmp L573
L595:
    mov edx, 2
    lea rsi, [LC127]
    xor edi, edi
    call [g_string_append_len]
    jmp L573
L592:
    lea rdx, [LC123]
    mov esi, 16
    xor edi, edi
    xor eax, eax
    jmp [g_log]
LC130:
    db `Evaluate.on_evaluate_global`, 0
LC131:
    db `Evaluate.on_evaluate_global: buffer_global is NULL`, 0
LC132:
    db `Evaluate.on_evaluate_global: nothing to evaluate on the current line`, 0
on_evaluate_global:
    endbr64
    push r12
    lea rdx, [LC130]
    mov esi, 128
    xor edi, edi
    push rbp
    push rbx
    sub rsp, 256
    mov rax, QWORD fs:40
    mov QWORD [rsp+248], rax
    xor eax, eax
    call [g_log]
    mov rbx, QWORD [buffer_global]
    test rbx, rbx
    je L611
    mov rdi, rbx
    xor eax, eax
    lea rbp, [rsp+80]
    call QWORD [reloc_functions+664]
    mov rdi, rbx
    mov rsi, rsp
    lea r12, [rsp+160]
    mov rdx, rax
    xor eax, eax
    call QWORD [reloc_functions+608]
    movdqa xmm3, OWORD [rsp+32]
    xor esi, esi
    mov rdi, rbp
    movdqa xmm4, OWORD [rsp+48]
    movdqa xmm5, OWORD [rsp+64]
    xor eax, eax
    movdqa xmm1, OWORD [rsp]
    movdqa xmm2, OWORD [rsp+16]
    movaps OWORD [rsp+112], xmm3
    movaps OWORD [rsp+128], xmm4
    movaps OWORD [rsp+144], xmm5
    movaps OWORD [rsp+80], xmm1
    movaps OWORD [rsp+96], xmm2
    call QWORD [reloc_functions+568]
    movdqa xmm6, OWORD [rsp+80]
    mov rdi, r12
    xor eax, eax
    movdqa xmm7, OWORD [rsp+96]
    movdqa xmm0, OWORD [rsp+112]
    movdqa xmm1, OWORD [rsp+128]
    movaps OWORD [rsp+160], xmm6
    movdqa xmm2, OWORD [rsp+144]
    movaps OWORD [rsp+176], xmm7
    movaps OWORD [rsp+192], xmm0
    movaps OWORD [rsp+208], xmm1
    movaps OWORD [rsp+224], xmm2
    call QWORD [reloc_functions+520]
    mov rdi, rbx
    xor ecx, ecx
    mov rdx, r12
    mov rsi, rbp
    xor eax, eax
    call QWORD [reloc_functions+352]
    mov rbx, rax
    test rax, rax
    je L601
    cmp BYTE [rax], 0
    je L601
    mov edi, 40
    xor eax, eax
    call QWORD [reloc_functions+504]
    mov rdi, rbx
    mov rbp, rax
    xor eax, eax
    call QWORD [reloc_functions+448]
    mov rdi, rbx
    call [g_strdup]
    mov QWORD [rbp+8], 0
    pxor xmm0, xmm0
    mov rdi, rbp
    mov QWORD [rbp+0], rax
    mov QWORD [rbp+32], 0
    movups OWORD [rbp+16], xmm0
    call swank_session_global_eval
    mov rdi, rbx
    call [g_free]
L598:
    mov rax, QWORD [rsp+248]
    sub rax, QWORD fs:40
    jne L610
    add rsp, 256
    pop rbx
    pop rbp
    pop r12
    ret
L601:
    xor edi, edi
    lea rdx, [LC132]
    mov esi, 128
    xor eax, eax
    call [g_log]
    mov rdi, rbx
    call [g_free]
    jmp L598
L611:
    mov rax, QWORD [rsp+248]
    sub rax, QWORD fs:40
    jne L610
    add rsp, 256
    mov esi, 16
    xor edi, edi
    xor eax, eax
    pop rbx
    lea rdx, [LC131]
    pop rbp
    pop r12
    jmp [g_log]
L610:
    call [__stack_chk_fail]
on_key_press_handler:
    endbr64
    mov rax, -4294967288
    and rax, QWORD [rsi+24]
    mov rcx, 280431299657736
    cmp rax, rcx
    je L620
    xor eax, eax
    ret
L620:
    sub rsp, 8
    xor eax, eax
    call on_evaluate_global
    mov eax, 1
    add rsp, 8
    ret
swank_session_cleanup_globals:
    endbr64
    xor edi, edi
    sub rsp, 8
    lea rdx, [LC121]
    xor eax, eax
    mov esi, 128
    call [g_log]
    mov rdi, QWORD [g_swank_session_interactions_table]
    test rdi, rdi
    je L622
    call [g_hash_table_destroy]
    mov QWORD [g_swank_session_interactions_table], 0
L622:
    mov DWORD [g_swank_session_started], 0
    mov DWORD [g_swank_session_next_tag], 1
    add rsp, 8
    ret
main_get_filename:
    endbr64
    mov rax, QWORD [filename_global]
    ret
LC133:
    db `main_set_filename %s`, 0
main_set_filename:
    endbr64
    push rbx
    mov rbx, rdi
    test rdi, rdi
    je L629
    mov rcx, rdi
    lea rdx, [LC133]
    mov esi, 128
    xor edi, edi
    xor eax, eax
    call [g_log]
    mov rdi, rbx
    xor eax, eax
    call QWORD [reloc_functions+448]
    mov rdi, rbx
    call [g_strdup]
    mov rbx, rax
L630:
    mov rdi, QWORD [filename_global]
    call [g_free]
    mov QWORD [filename_global], rbx
    pop rbx
    ret
L629:
    lea rcx, [LC79]
    mov esi, 128
    xor edi, edi
    xor eax, eax
    lea rdx, [LC133]
    call [g_log]
    jmp L630
LC134:
    db `simple_file_open_global: buffer_global is NULL. Cannot open file.\n`, 0
LC135:
    db `_Open`, 0
LC136:
    db `_Cancel`, 0
LC137:
    db `Open File`, 0
LC138:
    db `Failed to open %s (errno %d)\n`, 0
LC139:
    db `Failed to stat %s (errno %d)\n`, 0
LC140:
    db `Not a regular file: %s\n`, 0
LC141:
    db `Failed to allocate memory for file content.\n`, 0
LC142:
    db `Error reading %s (errno %d)\n`, 0
simple_file_open_global:
    endbr64
    push r15
    push r14
    push r13
    push r12
    push rbp
    push rbx
    sub rsp, 184
    mov rax, QWORD fs:40
    mov QWORD [rsp+168], rax
    xor eax, eax
    test rdi, rdi
    je L633
    call QWORD [reloc_functions+768]
    mov rdi, rax
L633:
    cmp QWORD [buffer_global], 0
    je L668
    push 0
    lea rcx, [LC136]
    xor edx, edx
    mov rsi, rdi
    push -3
    lea r9, [LC135]
    mov r8d, -6
    xor eax, eax
    lea rdi, [LC137]
    call [gtk_file_chooser_dialog_new]
    mov rdi, rax
    mov rbx, rax
    xor eax, eax
    call QWORD [reloc_functions+152]
    pop rdx
    pop rcx
    cmp eax, -3
    je L669
L636:
    mov rax, QWORD [rsp+168]
    sub rax, QWORD fs:40
    jne L667
    add rsp, 184
    mov rdi, rbx
    xor eax, eax
    pop rbx
    pop rbp
    pop r12
    pop r13
    pop r14
    pop r15
    jmp QWORD [reloc_functions+368]
L669:
    mov rdi, rbx
    xor eax, eax
    xor ebp, ebp
    call QWORD [reloc_functions+160]
    mov rdi, rax
    mov r12, rax
    call main_set_filename
    mov rdi, r12
    mov rsi, rbp
    mov rdx, rbp
    mov rax, $2
    syscall
    mov rbp, rax
    test rax, rax
    js L670
    cmp eax, -1
    je L671
    movsx r13, eax
    lea rbp, [rsp+16]
    mov rdi, r13
    mov rsi, rbp
    mov rax, $5
    syscall
    mov rbp, rax
    test rax, rax
    js L672
    mov eax, DWORD [rsp+40]
    and eax, 61440
    cmp eax, 32768
    je L664
    mov rsi, r12
    lea rdi, [LC140]
    xor eax, eax
    call [g_printerr]
    mov rdi, r13
    mov rax, $3
    syscall
    mov r13, rax
    test rax, rax
    js L666
L640:
    mov rdi, r12
    call [g_free]
    jmp L636
L671:
    call [__errno_location]
    mov edx, DWORD [rax]
L638:
    mov rsi, r12
    lea rdi, [LC138]
    xor eax, eax
    call [g_printerr]
    jmp L640
L668:
    mov rax, QWORD [rsp+168]
    sub rax, QWORD fs:40
    jne L667
    add rsp, 184
    lea rdi, [LC134]
    xor eax, eax
    pop rbx
    pop rbp
    pop r12
    pop r13
    pop r14
    pop r15
    jmp [g_printerr]
L664:
    mov r9, QWORD [rsp+64]
    xor eax, eax
    mov QWORD [rsp+8], r9
    lea rdi, [r9+1]
    call QWORD [reloc_functions+72]
    mov r14, rax
    test rax, rax
    je L647
    mov r9, QWORD [rsp+8]
    xor r8d, r8d
    mov r15, rax
    test r9, r9
    jg L648
L649:
    mov BYTE [r15], 0
    mov rdi, r13
    mov rax, $3
    syscall
    mov r13, rax
    test rax, rax
    js L673
L652:
    mov rdi, QWORD [buffer_global]
    mov edx, -1
    mov rsi, r14
    xor eax, eax
    call QWORD [reloc_functions+360]
    mov rdi, r14
    call [g_free]
    jmp L640
L676:
    add r8, rax
    cmp r9, r8
    jle L674
L648:
    mov rbp, r9
    lea r15, [r14+r8]
    sub rbp, r8
    mov rdi, r13
    mov rsi, r15
    mov rdx, rbp
    mov rax, $0
    syscall
    mov rbp, rax
    test rax, rax
    js L675
    jne L676
    jmp L649
L673:
    call [__errno_location]
    mov ebp, r13d
    neg ebp
    mov DWORD [rax], ebp
    jmp L652
L672:
    call [__errno_location]
    mov edx, ebp
    mov rsi, r12
    lea rdi, [LC139]
    neg edx
    mov r14, rax
    mov DWORD [rax], edx
    xor eax, eax
    call [g_printerr]
    mov rdi, r13
    mov rax, $3
    syscall
    test rax, rax
    jns L640
    neg eax
    mov DWORD [r14], eax
    jmp L640
L647:
    lea rdi, [LC141]
    xor eax, eax
    call [g_printerr]
    mov rdi, r13
    mov rax, $3
    syscall
    mov r13, rax
    test rax, rax
    jns L640
L666:
    call [__errno_location]
    mov ebp, r13d
    neg ebp
    mov DWORD [rax], ebp
    jmp L640
L674:
    lea r15, [r14+r8]
    jmp L649
L675:
    call [__errno_location]
    mov edx, ebp
    mov rsi, r12
    lea rdi, [LC142]
    neg edx
    mov DWORD [rax], edx
    xor eax, eax
    call [g_printerr]
    jmp L649
L670:
    call [__errno_location]
    mov edx, ebp
    neg edx
    mov DWORD [rax], edx
    jmp L638
L667:
    call [__stack_chk_fail]
LC143:
    db `simple_file_save_global: buffer_global is NULL. Cannot save file.\n`, 0
LC144:
    db `_Save`, 0
LC145:
    db `Save File As`, 0
LC146:
    db `Failed to open file %s for writing (errno %d)\n`, 0
LC147:
    db `Error writing to %s (errno %d)\n`, 0
LC148:
    db `Error closing %s (errno %d)\n`, 0
simple_file_save_global:
    endbr64
    push r14
    push r13
    push r12
    push rbp
    push rbx
    sub rsp, 176
    mov rax, QWORD fs:40
    mov QWORD [rsp+168], rax
    xor eax, eax
    test rdi, rdi
    je L678
    call QWORD [reloc_functions+768]
    mov rdi, rax
L678:
    cmp QWORD [buffer_global], 0
    je L711
    mov rbx, QWORD [filename_global]
    test rbx, rbx
    je L712
    mov rdi, rbx
    xor eax, eax
    call QWORD [reloc_functions+448]
    mov rdi, rbx
    call [g_strdup]
    mov rbp, rax
L685:
    lea r12, [rsp+80]
    mov rbx, rsp
    mov rdi, QWORD [buffer_global]
    xor eax, eax
    mov rsi, rbx
    call QWORD [reloc_functions+344]
    mov rsi, r12
    mov rdi, QWORD [buffer_global]
    xor eax, eax
    call QWORD [reloc_functions+336]
    mov rdx, r12
    xor ecx, ecx
    mov rsi, rbx
    mov rdi, QWORD [buffer_global]
    xor eax, eax
    mov r12d, 577
    call QWORD [reloc_functions+352]
    mov r13, rax
    mov eax, 438
    mov rdi, rbp
    mov rsi, r12
    mov rdx, rax
    mov rax, $2
    syscall
    mov r12, rax
    test rax, rax
    js L713
    cmp eax, -1
    je L714
    mov rdi, r13
    xor eax, eax
    movsx r12, r12d
    call QWORD [reloc_functions+448]
    mov r9, rax
    test rax, rax
    je L689
    xor r8d, r8d
    xor edx, edx
L693:
    mov rax, r9
    lea rbx, 0[r13+rdx]
    sub rax, rdx
    mov rdi, r12
    mov rsi, rbx
    mov rdx, rax
    mov rax, $1
    syscall
    mov rbx, rax
    test rax, rax
    js L715
    add r8, rax
    mov rdx, r8
    cmp r8, r9
    jb L693
L689:
    mov rdi, r12
    mov rax, $3
    syscall
    mov r12, rax
    test rax, rax
    js L716
L694:
    mov rdi, r13
    call [g_free]
    mov rdi, rbp
    call [g_free]
L677:
    mov rax, QWORD [rsp+168]
    sub rax, QWORD fs:40
    jne L710
    add rsp, 176
    pop rbx
    pop rbp
    pop r12
    pop r13
    pop r14
    ret
L712:
    push 0
    lea rcx, [LC136]
    mov edx, 1
    xor eax, eax
    push -3
    lea r9, [LC144]
    mov r8d, -6
    mov rsi, rdi
    lea rdi, [LC145]
    call [gtk_file_chooser_dialog_new]
    mov esi, 1
    mov rbx, rax
    mov rdi, rax
    xor eax, eax
    call QWORD [reloc_functions+168]
    mov rdi, rbx
    xor eax, eax
    call QWORD [reloc_functions+152]
    pop rdx
    pop rcx
    cmp eax, -3
    je L682
    mov rax, QWORD [rsp+168]
    sub rax, QWORD fs:40
    jne L710
    add rsp, 176
    mov rdi, rbx
    xor eax, eax
    pop rbx
    pop rbp
    pop r12
    pop r13
    pop r14
    jmp QWORD [reloc_functions+368]
L711:
    mov rax, QWORD [rsp+168]
    sub rax, QWORD fs:40
    jne L710
    add rsp, 176
    lea rdi, [LC143]
    xor eax, eax
    pop rbx
    pop rbp
    pop r12
    pop r13
    pop r14
    jmp [g_printerr]
L714:
    call [__errno_location]
    mov edx, DWORD [rax]
L687:
    mov rsi, rbp
    lea rdi, [LC146]
L708:
    xor eax, eax
    call [g_printerr]
    jmp L694
L716:
    call [__errno_location]
    mov edx, r12d
    mov rsi, rbp
    lea rdi, [LC148]
    neg edx
    mov DWORD [rax], edx
    jmp L708
L682:
    mov rdi, rbx
    xor eax, eax
    call QWORD [reloc_functions+160]
    mov rbp, rax
    mov rdi, rax
    call main_set_filename
    xor eax, eax
    mov rdi, rbx
    call QWORD [reloc_functions+368]
    test rbp, rbp
    jne L685
    jmp L677
L715:
    call [__errno_location]
    mov edx, ebx
    mov rsi, rbp
    lea rdi, [LC147]
    neg edx
    mov r14, rax
    mov DWORD [rax], edx
    xor eax, eax
    call [g_printerr]
    mov rdi, r12
    mov rax, $3
    syscall
    test rax, rax
    jns L694
    neg eax
    mov DWORD [r14], eax
    jmp L694
L713:
    call [__errno_location]
    mov edx, r12d
    neg edx
    mov DWORD [rax], edx
    jmp L687
L710:
    call [__stack_chk_fail]
simple_file_saveas_global:
    endbr64
    push rbp
    mov rbp, rdi
    push rbx
    sub rsp, 8
    mov rbx, QWORD [filename_global]
    test rbx, rbx
    je L718
    mov rdi, rbx
    xor eax, eax
    call QWORD [reloc_functions+448]
    mov rdi, rbx
    call [g_strdup]
    lea rcx, [LC79]
    lea rdx, [LC133]
    xor edi, edi
    mov esi, 128
    mov rbx, rax
    xor eax, eax
    call [g_log]
    mov rdi, QWORD [filename_global]
    call [g_free]
    mov rdi, rbp
    mov QWORD [filename_global], 0
    call simple_file_save_global
    cmp QWORD [filename_global], 0
    jne L720
    test rbx, rbx
    jne L728
L720:
    add rsp, 8
    mov rdi, rbx
    pop rbx
    pop rbp
    jmp [g_free]
L718:
    lea rcx, [LC79]
    mov esi, 128
    xor eax, eax
    xor edi, edi
    lea rdx, [LC133]
    call [g_log]
    mov rdi, QWORD [filename_global]
    call [g_free]
    mov rdi, rbp
    mov QWORD [filename_global], 0
    call simple_file_save_global
    add rsp, 8
    mov rdi, rbx
    pop rbx
    pop rbp
    jmp [g_free]
L728:
    mov rdi, rbx
    call main_set_filename
    add rsp, 8
    mov rdi, rbx
    pop rbx
    pop rbp
    jmp [g_free]
main_get_source_buffer:
    endbr64
    mov rax, QWORD [buffer_global]
    ret
LC149:
    db `Main.main`, 0
LC150:
    db `/usr/bin/sbcl`, 0
LC151:
    db `delete-event`, 0
LC152:
    db `commonlisp`, 0
LC153:
    db `key-press-event`, 0
LC154:
    db `File`, 0
LC155:
    db `Open\342\200\246`, 0
LC156:
    db `Save`, 0
LC157:
    db `Save as\342\200\246`, 0
LC158:
    db `Quit`, 0
LC159:
    db `activate`, 0
main:
    endbr64
    push r15
    lea rdx, [LC149]
    xor eax, eax
    push r14
    push r13
    push r12
    push rbp
    lea rbp, [reloc_functions]
    push rbx
    lea r12, [rbp+824]
    lea rbx, [reloc_names]
    sub rsp, 56
    mov DWORD [rsp+44], edi
    xor edi, edi
    mov QWORD [rsp+32], rsi
    mov esi, 128
    call [g_log]
L731:
    lea rsi, [rbx+1]
    xor edi, edi
    add rbp, 8
    call [dlsym]
    mov QWORD [rbp-8], rax
    movsx rax, BYTE [rbx]
    add rbx, rax
    cmp rbp, r12
    jne L731
    lea rsi, [rsp+32]
    lea rdi, [rsp+44]
    xor eax, eax
    call QWORD [reloc_functions+208]
    lea rdi, [LC150]
    call process_init_globals
    xor eax, eax
    call swank_process_init_globals
    xor eax, eax
    call swank_session_init_globals
    xor edi, edi
    xor eax, eax
    call QWORD [reloc_functions+416]
    mov edx, 600
    mov esi, 800
    mov rbp, rax
    mov rdi, rax
    xor eax, eax
    call QWORD [reloc_functions+424]
    xor r9d, r9d
    xor r8d, r8d
    xor ecx, ecx
    lea rdx, [quit_delete_event_handler]
    lea rsi, [LC151]
    mov rdi, rbp
    xor eax, eax
    call QWORD [reloc_functions+80]
    xor esi, esi
    xor edi, edi
    xor eax, eax
    call QWORD [reloc_functions+280]
    mov edx, 1
    mov esi, 1
    mov rdi, rax
    mov r12, rax
    xor eax, eax
    call QWORD [reloc_functions+288]
    xor eax, eax
    call QWORD [reloc_functions+304]
    lea rsi, [LC152]
    mov rdi, rax
    xor eax, eax
    call QWORD [reloc_functions+312]
    mov rdi, rax
    xor eax, eax
    call QWORD [reloc_functions+296]
    mov rdi, rax
    mov QWORD [buffer_global], rax
    xor eax, eax
    call QWORD [reloc_functions+320]
    mov esi, 1
    mov rbx, rax
    mov rdi, rax
    xor eax, eax
    call QWORD [reloc_functions+328]
    mov rsi, rbx
    mov rdi, r12
    xor eax, eax
    call QWORD [reloc_functions+136]
    xor r9d, r9d
    xor r8d, r8d
    xor ecx, ecx
    lea rdx, [on_key_press_handler]
    lea rsi, [LC153]
    mov rdi, rbx
    xor eax, eax
    call QWORD [reloc_functions+80]
    xor eax, eax
    call QWORD [reloc_functions+240]
    mov QWORD [rsp+24], rax
    xor eax, eax
    call QWORD [reloc_functions+264]
    lea rdi, [LC154]
    mov rbx, rax
    xor eax, eax
    call QWORD [reloc_functions+248]
    lea rdi, [LC155]
    mov QWORD [rsp+16], rax
    xor eax, eax
    call QWORD [reloc_functions+248]
    lea rdi, [LC156]
    mov r15, rax
    xor eax, eax
    call QWORD [reloc_functions+248]
    lea rdi, [LC157]
    mov r14, rax
    xor eax, eax
    call QWORD [reloc_functions+248]
    lea rdi, [LC158]
    mov r13, rax
    xor eax, eax
    call QWORD [reloc_functions+248]
    mov rsi, rbx
    mov rdi, QWORD [rsp+16]
    mov QWORD [rsp+8], rax
    xor eax, eax
    call QWORD [reloc_functions+256]
    mov rdi, rbx
    mov rsi, r15
    xor eax, eax
    call QWORD [reloc_functions+272]
    mov rdi, rbx
    mov rsi, r14
    xor eax, eax
    call QWORD [reloc_functions+272]
    mov rsi, r13
    mov rdi, rbx
    xor eax, eax
    call QWORD [reloc_functions+272]
    mov rdi, rbx
    mov rsi, QWORD [rsp+8]
    xor eax, eax
    call QWORD [reloc_functions+272]
    lea rbx, [LC159]
    xor eax, eax
    mov rsi, QWORD [rsp+16]
    mov rdi, QWORD [rsp+24]
    call QWORD [reloc_functions+272]
    xor r9d, r9d
    xor r8d, r8d
    mov rsi, rbx
    mov rcx, r15
    lea rdx, [simple_file_open_global]
    mov rdi, r15
    xor eax, eax
    call QWORD [reloc_functions+80]
    xor r9d, r9d
    xor r8d, r8d
    mov rsi, rbx
    mov rcx, r14
    lea rdx, [simple_file_save_global]
    mov rdi, r14
    xor eax, eax
    call QWORD [reloc_functions+80]
    xor r9d, r9d
    xor r8d, r8d
    mov rcx, r13
    mov rsi, rbx
    mov rdi, r13
    lea rdx, [simple_file_saveas_global]
    xor eax, eax
    call QWORD [reloc_functions+80]
    xor r9d, r9d
    xor r8d, r8d
    xor ecx, ecx
    mov rsi, rbx
    lea rdx, [quit_menu_item_handler]
    mov rdi, QWORD [rsp+8]
    xor eax, eax
    call QWORD [reloc_functions+80]
    xor eax, eax
    call interactions_view_new
    mov edi, 1
    mov QWORD [interactions_view_global], rax
    call [gtk_paned_new]
    mov ecx, 1
    mov edx, 1
    mov rsi, r12
    mov rdi, rax
    mov r13, rax
    call [gtk_paned_pack1]
    xor edx, edx
    mov ecx, 1
    mov rdi, r13
    mov rsi, QWORD [interactions_view_global]
    call [gtk_paned_pack2]
    xor esi, esi
    mov edi, 1
    xor eax, eax
    call QWORD [reloc_functions+96]
    mov rsi, QWORD [rsp+24]
    xor r8d, r8d
    xor ecx, ecx
    mov rbx, rax
    mov rdi, rax
    xor edx, edx
    xor eax, eax
    call QWORD [reloc_functions+104]
    xor r8d, r8d
    mov ecx, 1
    mov rsi, r13
    mov edx, 1
    mov rdi, rbx
    xor eax, eax
    call QWORD [reloc_functions+104]
    mov rsi, rbx
    mov rdi, rbp
    xor eax, eax
    call QWORD [reloc_functions+136]
    mov rdi, rbp
    xor eax, eax
    call QWORD [reloc_functions+408]
    xor eax, eax
    call QWORD [reloc_functions+232]
    mov rdi, QWORD [filename_global]
    call [g_free]
    xor eax, eax
    call swank_session_cleanup_globals
    xor eax, eax
    call process_cleanup_globals
    xor eax, eax
    call swank_process_cleanup_globals
    xor edi, edi
    xor eax, eax
    call QWORD [reloc_functions]
    align 32
__func__.0:
    db `interactions_view_update_interaction`, 0
    align 32
__func__.1:
    db `interactions_view_add_interaction`, 0
    align 8
    align 8
    align 8
    align 32
    align 32
reloc_names:
    db 6
    db `exit`, 0
    db 15
    db `g_clear_error`, 0
    db 23
    db `g_get_user_config_dir`, 0
    db 24
    db `g_intern_static_string`, 0
    db 17
    db `g_key_file_free`, 0
    db 23
    db `g_key_file_get_string`, 0
    db 27
    db `g_key_file_load_from_file`, 0
    db 16
    db `g_key_file_new`, 0
    db 23
    db `g_key_file_set_string`, 0
    db 10
    db `g_malloc`, 0
    db 23
    db `g_signal_connect_data`, 0
    db 11
    db `g_strcmp0`, 0
    db 13
    db `gtk_box_new`, 0
    db 20
    db `gtk_box_pack_start`, 0
    db 29
    db `gtk_combo_box_set_active_id`, 0
    db 27
    db `gtk_combo_box_text_append`, 0
    db 24
    db `gtk_combo_box_text_new`, 0
    db 19
    db `gtk_container_add`, 0
    db 29
    db `gtk_dialog_get_content_area`, 0
    db 16
    db `gtk_dialog_run`, 0
    db 31
    db `gtk_file_chooser_get_filename`, 0
    db 48
    db `gtk_file_chooser_set_do_overwrite_confirmation`, 0
    db 17
    db `gtk_grid_attach`, 0
    db 14
    db `gtk_grid_new`, 0
    db 29
    db `gtk_grid_set_column_spacing`, 0
    db 26
    db `gtk_grid_set_row_spacing`, 0
    db 10
    db `gtk_init`, 0
    db 15
    db `gtk_label_new`, 0
    db 15
    db `gtk_main_quit`, 0
    db 10
    db `gtk_main`, 0
    db 18
    db `gtk_menu_bar_new`, 0
    db 30
    db `gtk_menu_item_new_with_label`, 0
    db 27
    db `gtk_menu_item_set_submenu`, 0
    db 14
    db `gtk_menu_new`, 0
    db 23
    db `gtk_menu_shell_append`, 0
    db 25
    db `gtk_scrolled_window_new`, 0
    db 32
    db `gtk_scrolled_window_set_policy`, 0
    db 37
    db `gtk_source_buffer_new_with_language`, 0
    db 41
    db `gtk_source_language_manager_get_default`, 0
    db 42
    db `gtk_source_language_manager_get_language`, 0
    db 33
    db `gtk_source_view_new_with_buffer`, 0
    db 39
    db `gtk_source_view_set_show_line_numbers`, 0
    db 30
    db `gtk_text_buffer_get_end_iter`, 0
    db 32
    db `gtk_text_buffer_get_start_iter`, 0
    db 26
    db `gtk_text_buffer_get_text`, 0
    db 26
    db `gtk_text_buffer_set_text`, 0
    db 20
    db `gtk_widget_destroy`, 0
    db 30
    db `gtk_widget_set_margin_bottom`, 0
    db 27
    db `gtk_widget_set_margin_end`, 0
    db 29
    db `gtk_widget_set_margin_start`, 0
    db 27
    db `gtk_widget_set_margin_top`, 0
    db 21
    db `gtk_widget_show_all`, 0
    db 16
    db `gtk_window_new`, 0
    db 29
    db `gtk_window_set_default_size`, 0
    db 26
    db `g_type_class_peek_parent`, 0
    db 31
    db `g_type_register_static_simple`, 0
    db 8
    db `strlen`, 0
    db 36
    db `g_type_class_adjust_private_offset`, 0
    db 25
    db `g_key_file_save_to_file`, 0
    db 17
    db `g_ptr_array_add`, 0
    db 17
    db `gtk_widget_hide`, 0
    db 32
    db `g_ptr_array_new_with_free_func`, 0
    db 29
    db `gtk_combo_box_get_active_id`, 0
    db 11
    db `g_malloc0`, 0
    db 18
    db `g_ptr_array_free`, 0
    db 35
    db `gtk_text_iter_forward_to_line_end`, 0
    db 26
    db `g_return_if_fail_warning`, 0
    db 32
    db `g_socket_connection_get_socket`, 0
    db 33
    db `g_socket_client_connect_to_host`, 0
    db 21
    db `g_string_insert_len`, 0
    db 14
    db `g_thread_new`, 0
    db 31
    db `gtk_text_iter_set_line_offset`, 0
    db 17
    db `g_socket_get_fd`, 0
    db 14
    db `g_string_new`, 0
    db 21
    db `g_socket_client_new`, 0
    db 19
    db `g_string_insert_c`, 0
    db 34
    db `gtk_text_buffer_get_iter_at_mark`, 0
    db 25
    db `g_string_free_and_steal`, 0
    db 8
    db `strstr`, 0
    db 26
    db `g_spawn_async_with_pipes`, 0
    db 13
    db `g_file_test`, 0
    db 16
    db `g_object_unref`, 0
    db 19
    db `g_application_run`, 0
    db 28
    db `gtk_text_buffer_get_insert`, 0
    db 28
    db `g_type_check_instance_is_a`, 0
    db 14
    db `g_mutex_lock`, 0
    db 16
    db `g_mutex_unlock`, 0
    db 14
    db `g_mutex_init`, 0
    db 15
    db `g_mutex_clear`, 0
    db 14
    db `g_cond_clear`, 0
    db 13
    db `g_cond_wait`, 0
    db 13
    db `g_cond_init`, 0
    db 18
    db `g_cond_broadcast`, 0
    db 24
    db `g_key_file_get_integer`, 0
    db 20
    db `g_application_quit`, 0
    db 20
    db `g_path_get_dirname`, 0
    db 25
    db `gtk_widget_get_toplevel`, 0
    db 12
    db `g_strerror`, 0
    db 28
    db `gtk_application_window_new`, 0
    db 22
    db `g_mkdir_with_parents`, 0
    db 26
    db `gtk_application_get_type`, 0
    db 24
    db `g_key_file_set_integer`, 0
    db 10
    db `g_usleep`, 0
    align 4
g_swank_session_next_tag:
    dd 1
    align 4
g_swank_port_number:
    dd 4005
    align 4
g_swank_fd:
    dd -1
    align 4
g_process_err_fd:
    dd -1
    align 4
g_process_out_fd:
    dd -1
    align 4
g_process_in_fd:
    dd -1

dynsym:
    dq 0, 0, 0
    dd str_g_spawn_close_pid - dynstr, 0x12, 0, 0, 0, 0
    dd str_g_hash_table_new_full - dynstr, 0x12, 0, 0, 0, 0
    dd str___errno_location - dynstr, 0x12, 0, 0, 0, 0
    dd str_g_strdup - dynstr, 0x12, 0, 0, 0, 0
    dd str_g_strdupv - dynstr, 0x12, 0, 0, 0, 0
    dd str_g_object_new - dynstr, 0x12, 0, 0, 0, 0
    dd str_g_hash_table_lookup - dynstr, 0x12, 0, 0, 0, 0
    dd str_g_strfreev - dynstr, 0x12, 0, 0, 0, 0
    dd str_g_str_has_prefix - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_text_view_set_editable - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_css_provider_load_from_data - dynstr, 0x12, 0, 0, 0, 0
    dd str_g_thread_join - dynstr, 0x12, 0, 0, 0, 0
    dd str_g_string_append_len - dynstr, 0x12, 0, 0, 0, 0
    dd str_g_direct_equal - dynstr, 0x12, 0, 0, 0, 0
    dd str_g_hash_table_insert - dynstr, 0x12, 0, 0, 0, 0
    dd str_g_once_init_enter_pointer - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_orientable_set_orientation - dynstr, 0x12, 0, 0, 0, 0
    dd str_g_log - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_widget_show - dynstr, 0x12, 0, 0, 0, 0
    dd str_g_main_context_invoke - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_widget_get_screen - dynstr, 0x12, 0, 0, 0, 0
    dd str_g_io_stream_close - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_box_get_type - dynstr, 0x12, 0, 0, 0, 0
    dd str_perror - dynstr, 0x12, 0, 0, 0, 0
    dd str_g_strdup_printf - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_box_reorder_child - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_text_view_set_wrap_mode - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_widget_get_style_context - dynstr, 0x12, 0, 0, 0, 0
    dd str_g_free - dynstr, 0x12, 0, 0, 0, 0
    dd str_dlsym - dynstr, 0x12, 0, 0, 0, 0
    dd str_memmove - dynstr, 0x12, 0, 0, 0, 0
    dd str_close - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_frame_new - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_style_context_add_provider_for_screen - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_file_chooser_dialog_new - dynstr, 0x12, 0, 0, 0, 0
    dd str_memcpy - dynstr, 0x12, 0, 0, 0, 0
    dd str_g_direct_hash - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_paned_new - dynstr, 0x12, 0, 0, 0, 0
    dd str_g_string_set_size - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_text_view_get_buffer - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_paned_pack1 - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_text_view_new - dynstr, 0x12, 0, 0, 0, 0
    dd str_setsid - dynstr, 0x12, 0, 0, 0, 0
    dd str_g_string_free - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_widget_set_vexpand - dynstr, 0x12, 0, 0, 0, 0
    dd str_g_hash_table_destroy - dynstr, 0x12, 0, 0, 0, 0
    dd str_g_error_free - dynstr, 0x12, 0, 0, 0, 0
    dd str_g_printerr - dynstr, 0x12, 0, 0, 0, 0
    dd str_g_ascii_strtoull - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_css_provider_new - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_style_context_add_class - dynstr, 0x12, 0, 0, 0, 0
    dd str_g_string_new_len - dynstr, 0x12, 0, 0, 0, 0
    dd str_g_strndup - dynstr, 0x12, 0, 0, 0, 0
    dd str_prctl - dynstr, 0x12, 0, 0, 0, 0
    dd str_gtk_paned_pack2 - dynstr, 0x12, 0, 0, 0, 0
    dd str___stack_chk_fail - dynstr, 0x12, 0, 0, 0, 0
    dd str_g_snprintf - dynstr, 0x12, 0, 0, 0, 0
    dd str_g_cond_signal - dynstr, 0x12, 0, 0, 0, 0
    dd str_g_string_erase - dynstr, 0x12, 0, 0, 0, 0
    dd str_g_once_init_leave_pointer - dynstr, 0x12, 0, 0, 0, 0

dynstr:
    db 0
str_g_spawn_close_pid: db "g_spawn_close_pid", 0
str_g_hash_table_new_full: db "g_hash_table_new_full", 0
str___errno_location: db "__errno_location", 0
str_g_strdup: db "g_strdup", 0
str_g_strdupv: db "g_strdupv", 0
str_g_object_new: db "g_object_new", 0
str_g_hash_table_lookup: db "g_hash_table_lookup", 0
str_g_strfreev: db "g_strfreev", 0
str_g_str_has_prefix: db "g_str_has_prefix", 0
str_gtk_text_view_set_editable: db "gtk_text_view_set_editable", 0
str_gtk_css_provider_load_from_data: db "gtk_css_provider_load_from_data", 0
str_g_thread_join: db "g_thread_join", 0
str_g_string_append_len: db "g_string_append_len", 0
str_g_direct_equal: db "g_direct_equal", 0
str_g_hash_table_insert: db "g_hash_table_insert", 0
str_g_once_init_enter_pointer: db "g_once_init_enter_pointer", 0
str_gtk_orientable_set_orientation: db "gtk_orientable_set_orientation", 0
str_g_log: db "g_log", 0
str_gtk_widget_show: db "gtk_widget_show", 0
str_g_main_context_invoke: db "g_main_context_invoke", 0
str_gtk_widget_get_screen: db "gtk_widget_get_screen", 0
str_g_io_stream_close: db "g_io_stream_close", 0
str_gtk_box_get_type: db "gtk_box_get_type", 0
str_perror: db "perror", 0
str_g_strdup_printf: db "g_strdup_printf", 0
str_gtk_box_reorder_child: db "gtk_box_reorder_child", 0
str_gtk_text_view_set_wrap_mode: db "gtk_text_view_set_wrap_mode", 0
str_gtk_widget_get_style_context: db "gtk_widget_get_style_context", 0
str_g_free: db "g_free", 0
str_dlsym: db "dlsym", 0
str_memmove: db "memmove", 0
str_close: db "close", 0
str_gtk_frame_new: db "gtk_frame_new", 0
str_gtk_style_context_add_provider_for_screen: db "gtk_style_context_add_provider_for_screen", 0
str_gtk_file_chooser_dialog_new: db "gtk_file_chooser_dialog_new", 0
str_memcpy: db "memcpy", 0
str_g_direct_hash: db "g_direct_hash", 0
str_gtk_paned_new: db "gtk_paned_new", 0
str_g_string_set_size: db "g_string_set_size", 0
str_gtk_text_view_get_buffer: db "gtk_text_view_get_buffer", 0
str_gtk_paned_pack1: db "gtk_paned_pack1", 0
str_gtk_text_view_new: db "gtk_text_view_new", 0
str_setsid: db "setsid", 0
str_g_string_free: db "g_string_free", 0
str_gtk_widget_set_vexpand: db "gtk_widget_set_vexpand", 0
str_g_hash_table_destroy: db "g_hash_table_destroy", 0
str_g_error_free: db "g_error_free", 0
str_g_printerr: db "g_printerr", 0
str_g_ascii_strtoull: db "g_ascii_strtoull", 0
str_gtk_css_provider_new: db "gtk_css_provider_new", 0
str_gtk_style_context_add_class: db "gtk_style_context_add_class", 0
str_g_string_new_len: db "g_string_new_len", 0
str_g_strndup: db "g_strndup", 0
str_prctl: db "prctl", 0
str_gtk_paned_pack2: db "gtk_paned_pack2", 0
str___stack_chk_fail: db "__stack_chk_fail", 0
str_g_snprintf: db "g_snprintf", 0
str_g_cond_signal: db "g_cond_signal", 0
str_g_string_erase: db "g_string_erase", 0
str_g_once_init_leave_pointer: db "g_once_init_leave_pointer", 0
    libraries_strings
dynstr_end:

rela_text:
    dq (BASE_ADDR + g_spawn_close_pid), (1 << 32) | 1, 0
    dq (BASE_ADDR + g_hash_table_new_full), (2 << 32) | 1, 0
    dq (BASE_ADDR + __errno_location), (3 << 32) | 1, 0
    dq (BASE_ADDR + g_strdup), (4 << 32) | 1, 0
    dq (BASE_ADDR + g_strdupv), (5 << 32) | 1, 0
    dq (BASE_ADDR + g_object_new), (6 << 32) | 1, 0
    dq (BASE_ADDR + g_hash_table_lookup), (7 << 32) | 1, 0
    dq (BASE_ADDR + g_strfreev), (8 << 32) | 1, 0
    dq (BASE_ADDR + g_str_has_prefix), (9 << 32) | 1, 0
    dq (BASE_ADDR + gtk_text_view_set_editable), (10 << 32) | 1, 0
    dq (BASE_ADDR + gtk_css_provider_load_from_data), (11 << 32) | 1, 0
    dq (BASE_ADDR + g_thread_join), (12 << 32) | 1, 0
    dq (BASE_ADDR + g_string_append_len), (13 << 32) | 1, 0
    dq (BASE_ADDR + g_direct_equal), (14 << 32) | 1, 0
    dq (BASE_ADDR + g_hash_table_insert), (15 << 32) | 1, 0
    dq (BASE_ADDR + g_once_init_enter_pointer), (16 << 32) | 1, 0
    dq (BASE_ADDR + gtk_orientable_set_orientation), (17 << 32) | 1, 0
    dq (BASE_ADDR + g_log), (18 << 32) | 1, 0
    dq (BASE_ADDR + gtk_widget_show), (19 << 32) | 1, 0
    dq (BASE_ADDR + g_main_context_invoke), (20 << 32) | 1, 0
    dq (BASE_ADDR + gtk_widget_get_screen), (21 << 32) | 1, 0
    dq (BASE_ADDR + g_io_stream_close), (22 << 32) | 1, 0
    dq (BASE_ADDR + gtk_box_get_type), (23 << 32) | 1, 0
    dq (BASE_ADDR + perror), (24 << 32) | 1, 0
    dq (BASE_ADDR + g_strdup_printf), (25 << 32) | 1, 0
    dq (BASE_ADDR + gtk_box_reorder_child), (26 << 32) | 1, 0
    dq (BASE_ADDR + gtk_text_view_set_wrap_mode), (27 << 32) | 1, 0
    dq (BASE_ADDR + gtk_widget_get_style_context), (28 << 32) | 1, 0
    dq (BASE_ADDR + g_free), (29 << 32) | 1, 0
    dq (BASE_ADDR + dlsym), (30 << 32) | 1, 0
    dq (BASE_ADDR + memmove), (31 << 32) | 1, 0
    dq (BASE_ADDR + close), (32 << 32) | 1, 0
    dq (BASE_ADDR + gtk_frame_new), (33 << 32) | 1, 0
    dq (BASE_ADDR + gtk_style_context_add_provider_for_screen), (34 << 32) | 1, 0
    dq (BASE_ADDR + gtk_file_chooser_dialog_new), (35 << 32) | 1, 0
    dq (BASE_ADDR + memcpy), (36 << 32) | 1, 0
    dq (BASE_ADDR + g_direct_hash), (37 << 32) | 1, 0
    dq (BASE_ADDR + gtk_paned_new), (38 << 32) | 1, 0
    dq (BASE_ADDR + g_string_set_size), (39 << 32) | 1, 0
    dq (BASE_ADDR + gtk_text_view_get_buffer), (40 << 32) | 1, 0
    dq (BASE_ADDR + gtk_paned_pack1), (41 << 32) | 1, 0
    dq (BASE_ADDR + gtk_text_view_new), (42 << 32) | 1, 0
    dq (BASE_ADDR + setsid), (43 << 32) | 1, 0
    dq (BASE_ADDR + g_string_free), (44 << 32) | 1, 0
    dq (BASE_ADDR + gtk_widget_set_vexpand), (45 << 32) | 1, 0
    dq (BASE_ADDR + g_hash_table_destroy), (46 << 32) | 1, 0
    dq (BASE_ADDR + g_error_free), (47 << 32) | 1, 0
    dq (BASE_ADDR + g_printerr), (48 << 32) | 1, 0
    dq (BASE_ADDR + g_ascii_strtoull), (49 << 32) | 1, 0
    dq (BASE_ADDR + gtk_css_provider_new), (50 << 32) | 1, 0
    dq (BASE_ADDR + gtk_style_context_add_class), (51 << 32) | 1, 0
    dq (BASE_ADDR + g_string_new_len), (52 << 32) | 1, 0
    dq (BASE_ADDR + g_strndup), (53 << 32) | 1, 0
    dq (BASE_ADDR + prctl), (54 << 32) | 1, 0
    dq (BASE_ADDR + gtk_paned_pack2), (55 << 32) | 1, 0
    dq (BASE_ADDR + __stack_chk_fail), (56 << 32) | 1, 0
    dq (BASE_ADDR + g_snprintf), (57 << 32) | 1, 0
    dq (BASE_ADDR + g_cond_signal), (58 << 32) | 1, 0
    dq (BASE_ADDR + g_string_erase), (59 << 32) | 1, 0
    dq (BASE_ADDR + g_once_init_leave_pointer), (60 << 32) | 1, 0
rela_text_end:

eof:
    section .bss
bss:
g_spawn_close_pid: resq 1
g_hash_table_new_full: resq 1
__errno_location: resq 1
g_strdup: resq 1
g_strdupv: resq 1
g_object_new: resq 1
g_hash_table_lookup: resq 1
g_strfreev: resq 1
g_str_has_prefix: resq 1
gtk_text_view_set_editable: resq 1
gtk_css_provider_load_from_data: resq 1
g_thread_join: resq 1
g_string_append_len: resq 1
g_direct_equal: resq 1
g_hash_table_insert: resq 1
g_once_init_enter_pointer: resq 1
gtk_orientable_set_orientation: resq 1
g_log: resq 1
gtk_widget_show: resq 1
g_main_context_invoke: resq 1
gtk_widget_get_screen: resq 1
g_io_stream_close: resq 1
gtk_box_get_type: resq 1
perror: resq 1
g_strdup_printf: resq 1
gtk_box_reorder_child: resq 1
gtk_text_view_set_wrap_mode: resq 1
gtk_widget_get_style_context: resq 1
g_free: resq 1
dlsym: resq 1
memmove: resq 1
close: resq 1
gtk_frame_new: resq 1
gtk_style_context_add_provider_for_screen: resq 1
gtk_file_chooser_dialog_new: resq 1
memcpy: resq 1
g_direct_hash: resq 1
gtk_paned_new: resq 1
g_string_set_size: resq 1
gtk_text_view_get_buffer: resq 1
gtk_paned_pack1: resq 1
gtk_text_view_new: resq 1
setsid: resq 1
g_string_free: resq 1
gtk_widget_set_vexpand: resq 1
g_hash_table_destroy: resq 1
g_error_free: resq 1
g_printerr: resq 1
g_ascii_strtoull: resq 1
gtk_css_provider_new: resq 1
gtk_style_context_add_class: resq 1
g_string_new_len: resq 1
g_strndup: resq 1
prctl: resq 1
gtk_paned_pack2: resq 1
__stack_chk_fail: resq 1
g_snprintf: resq 1
g_cond_signal: resq 1
g_string_erase: resq 1
g_once_init_leave_pointer: resq 1
interactions_view_global: resb 8
filename_global: resb 8
buffer_global: resb 8
reloc_functions: resb 824
static_g_define_type_id.2: resb 8
g_swank_session_interactions_table: resb 8
g_swank_session_started: resb 4
g_swank_process_started: resb 4
g_swank_reader_thread: resb 8
g_swank_message_cb_data: resb 8
g_swank_message_cb: resb 8
g_swank_incoming_mutex: resb 8
g_swank_incoming_consumed: resb 8
g_swank_incoming_data_buffer: resb 8
g_swank_out_cond: resb 16
g_swank_out_mutex: resb 8
g_swank_out_consumed: resb 8
g_swank_out_buffer: resb 8
g_swank_connection: resb 8
g_process_started: resb 4
g_process_argv: resb 8
g_process_err_thread: resb 8
g_process_out_thread: resb 8
g_process_err_user_data: resb 8
g_process_err_cb: resb 8
g_process_out_user_data: resb 8
g_process_out_cb: resb 8
g_process_pid: resb 4
InteractionsView_private_offset: resb 4
interactions_view_parent_class: resb 8
bss_end:
bss_size equ bss_end - bss
