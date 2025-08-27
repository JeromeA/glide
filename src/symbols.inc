S(exit, NORETURN, void, (int status), (status))
S(g_application_quit, VOID, void, (GApplication *application), (application))
S(g_application_run, RETURN, gint, (GApplication *application, int argc, char **argv), (application, argc, argv))
S(g_clear_error, VOID, void, (GError **err), (err))
S(g_cond_broadcast, VOID, void, (GCond *cond), (cond))
S(g_cond_clear, VOID, void, (GCond *cond), (cond))
S(g_cond_init, VOID, void, (GCond *cond), (cond))
S(g_cond_signal, VOID, void, (GCond *cond), (cond))
S(g_cond_wait, VOID, void, (GCond *cond, GMutex *mutex), (cond, mutex))
S(g_direct_equal, RETURN, gboolean, (gconstpointer v1, gconstpointer v2), (v1, v2))
S(g_direct_hash, RETURN, guint, (gconstpointer v), (v))
S(g_file_test, RETURN, gboolean, (const gchar *filename, GFileTest test), (filename, test))
S(g_get_user_config_dir, RETURN, const gchar *, (void), ())
S(g_hash_table_insert, RETURN, gboolean, (GHashTable *hash_table, gpointer key, gpointer value), (hash_table, key, value))
S(g_hash_table_lookup, RETURN, void *, (GHashTable *h, const void *v), (h, v))
S(g_has_table_new_full, RETURN, GHashTable *, (GHashFunc hash_func, GEqualFunc key_equal_func), (hash_func, key_equal_func))
S(g_intern_static_string, RETURN, const gchar *, (const gchar *string), (string))
S(g_key_file_free, VOID, void, (GKeyFile *key_file), (key_file))
S(g_key_file_get_integer, RETURN, gint, (GKeyFile *key_file, const gchar *group_name, const gchar *key, GError **error), (key_file, group_name, key, error))
S(g_key_file_get_string, RETURN, gchar *, (GKeyFile *key_file, const gchar *group_name, const gchar *key, GError **error), (key_file, group_name, key, error))
S(g_key_file_load_from_file, RETURN, gboolean, (GKeyFile *key_file, const gchar *file, GKeyFileFlags flags, GError **error), (key_file, file, flags, error))
S(g_key_file_new, RETURN, GKeyFile *, (void), ())
S(g_key_file_save_to_file, RETURN, gboolean, (GKeyFile *key_file, const gchar *file, GError **error), (key_file, file, error))
S(g_key_file_set_integer, VOID, void, (GKeyFile *key_file, const gchar *group_name, const gchar *key, gint value), (key_file, group_name, key, value))
S(g_key_file_set_string, VOID, void, (GKeyFile *key_file, const gchar *group_name, const gchar *key, const gchar *value), (key_file, group_name, key, value))
S(g_malloc0, RETURN, gpointer, (gsize n_bytes), (n_bytes))
S(g_malloc, RETURN, gpointer, (gsize n_bytes), (n_bytes))
S(g_mkdir_with_parents, RETURN, gint, (const gchar *pathname, gint mode), (pathname, mode))
S(g_mutex_clear, VOID, void, (GMutex *mutex), (mutex))
S(g_mutex_init, VOID, void, (GMutex *mutex), (mutex))
S(g_mutex_lock, VOID, void, (GMutex *mutex), (mutex))
S(g_mutex_unlock, VOID, void, (GMutex *mutex), (mutex))
S(g_object_unref, VOID, void, (void *object), (object))
S(g_path_get_dirname, RETURN, gchar *, (const gchar *file_name), (file_name))
S(g_ptr_array_add, VOID, void, (GPtrArray *array, gpointer data), (array, data))
S(g_ptr_array_free, RETURN, void **, (GPtrArray *array, gboolean free_segment), (array, free_segment))
S(g_ptr_array_new_with_free_func, RETURN, GPtrArray *, (GDestroyNotify element_free_func), (element_free_func))
S(g_return_if_fail_warning, VOID, void, (const gchar *log_domain, const gchar *func, const gchar *expression), (log_domain, func, expression))
S(g_signal_connect_data, RETURN, gulong, (gpointer instance, const gchar *detailed_signal, GCallback c_handler, gpointer data, GClosureNotify destroy_data, GConnectFlags connect_flags), (instance, detailed_signal, c_handler, data, destroy_data, connect_flags))
S(g_socket_client_connect_to_host, RETURN, GSocketConnection *, (GSocketClient *client, const gchar *host_name, guint16 port, GCancellable *cancellable, GError **error), (client, host_name, port, cancellable, error))
S(g_socket_client_new, RETURN, GSocketClient *, (void), ())
S(g_socket_connection_get_socket, RETURN, GSocket *, (GSocketConnection *connection), (connection))
S(g_socket_get_fd, RETURN, gint, (GSocket *socket), (socket))
S(g_spawn_async_with_pipes, RETURN, gboolean, (const gchar *working_directory, gchar **argv, gchar **envp, GSpawnFlags flags, GSpawnChildSetupFunc child_setup, gpointer user_data, GPid *child_pid, gint *standard_input, gint *standard_output, gint *standard_error, GError **error), (working_directory, argv, envp, flags, child_setup, user_data, child_pid, standard_input, standard_output, standard_error, error))
S(g_strcmp0, RETURN, gint, (const gchar *str1, const gchar *str2), (str1, str2))
S(g_strerror, RETURN, const gchar *, (int errnum), (errnum))
S(g_strfreev, VOID, void, (gchar **c), (c))
S(g_string_erase, RETURN, GString *, (GString *string, gssize pos, gssize len), (string, pos, len))
S(g_string_free_and_steal, RETURN, gchar *, (GString *string), (string))
S(g_string_insert_c, RETURN, GString *, (GString *string, gssize pos, gchar c), (string, pos, c))
S(g_string_insert_len, RETURN, GString *, (GString *string, gssize pos, const gchar *val, gssize len), (string, pos, val, len))
S(g_string_new, RETURN, GString *, (const gchar *init), (init))
S(g_string_set_size, RETURN, GString *, (GString *string, gsize len), (string, len))
S(g_thread_new, RETURN, GThread *, (const gchar *name, GThreadFunc func, gpointer data), (name, func, data))
S(gtk_application_get_type, RETURN, GType, (void), ())
S(gtk_application_window_new, RETURN, GtkWidget *, (GtkApplication *application), (application))
S(gtk_box_get_type, RETURN, GType, (), ())
S(gtk_box_new, RETURN, GtkWidget *, (GtkOrientation orientation, gint spacing), (orientation, spacing))
S(gtk_box_pack_start, VOID, void, (GtkBox *box, GtkWidget *child, gboolean expand, gboolean fill, guint padding), (box, child, expand, fill, padding))
S(gtk_box_reorder_child, VOID, void, (GtkBox *box, GtkWidget *child, gint position), (box, child, position))
S(gtk_combo_box_get_active_id, RETURN, const gchar *, (GtkComboBox *combo_box), (combo_box))
S(gtk_combo_box_set_active_id, RETURN, gboolean, (GtkComboBox *combo_box, const gchar *id), (combo_box, id))
S(gtk_combo_box_text_append, VOID, void, (GtkComboBoxText *combo_box, const gchar *text, const gchar *id), (combo_box, text, id))
S(gtk_combo_box_text_new, RETURN, GtkWidget *, (void), ())
S(gtk_container_add, VOID, void, (GtkContainer *container, GtkWidget *widget), (container, widget))
S(gtk_css_provider_load_from_data, RETURN, gboolean, (GtkCssProvider *provider, const gchar *data, gssize length, GError **error), (provider, data, length, error))
S(gtk_css_provider_new, RETURN, GtkCssProvider *, (void), ())
S(gtk_dialog_get_content_area, RETURN, GtkWidget *, (GtkDialog *dialog), (dialog))
S(gtk_dialog_run, RETURN, gint, (GtkDialog *dialog), (dialog))
S(gtk_file_chooser_get_filename, RETURN, gchar *, (GtkFileChooser *chooser), (chooser))
S(gtk_file_chooser_set_do_overwrite_confirmation, VOID, void, (GtkFileChooser *chooser, gboolean do_overwrite_confirmation), (chooser, do_overwrite_confirmation))
S(gtk_file_chooser_set_current_folder, RETURN, gboolean, (GtkFileChooser *chooser, const gchar *filename), (chooser, filename))
S(gtk_frame_new, RETURN, GtkWidget *, (const gchar *label), (label))
S(gtk_grid_attach, VOID, void, (GtkGrid *grid, GtkWidget *child, gint left, gint top, gint width, gint height), (grid, child, left, top, width, height))
S(gtk_grid_new, RETURN, GtkWidget *, (void), ())
S(gtk_grid_set_column_spacing, VOID, void, (GtkGrid *grid, guint spacing), (grid, spacing))
S(gtk_grid_set_row_spacing, VOID, void, (GtkGrid *grid, guint spacing), (grid, spacing))
S(gtk_init, VOID, void, (int *argc, char ***argv), (argc, argv))
S(gtk_label_new, RETURN, GtkWidget *, (const gchar *str), (str))
S(gtk_main_context_invoke, VOID, void, (GMainContext* c, GSourceFunc f, gpointer p), (c, f, p))
S(gtk_main_quit, VOID, void, (void), ())
S(gtk_main, VOID, void, (void), ())
S(gtk_menu_bar_new, RETURN, GtkWidget *, (void), ())
S(gtk_menu_item_new_with_label, RETURN, GtkWidget *, (const gchar *label), (label))
S(gtk_menu_item_set_submenu, VOID, void, (GtkMenuItem *menu_item, GtkWidget *submenu), (menu_item, submenu))
S(gtk_menu_new, RETURN, GtkWidget *, (void), ())
S(gtk_menu_shell_append, VOID, void, (GtkMenuShell *menu_shell, GtkWidget *child), (menu_shell, child))
S(gtk_orientable_set_orientation, VOID, void, (GtkOrientable *orientable, GtkOrientation orientation), (orientable, orientation))
S(gtk_pane_new, RETURN, GtkWidget *, (GtkOrientation orientation), (orientation))
S(gtk_pane_pack1, VOID, void, (GtkPaned *paned, GtkWidget *child, gboolean resize), (paned, child, resize))
S(gtk_pane_pack2, VOID, void, (GtkPaned *paned, GtkWidget *child, gboolean resize), (paned, child, resize))
S(gtk_scrolled_window_new, RETURN, GtkWidget *, (GtkAdjustment *hadjustment, GtkAdjustment *vadjustment), (hadjustment, vadjustment))
S(gtk_scrolled_window_set_policy, VOID, void, (GtkScrolledWindow *scrolled_window, GtkPolicyType hscrollbar_policy, GtkPolicyType vscrollbar_policy), (scrolled_window, hscrollbar_policy, vscrollbar_policy))
S(gtk_source_buffer_new_with_language, RETURN, GtkSourceBuffer *, (GtkSourceLanguage *language), (language))
S(gtk_source_language_manager_get_default, RETURN, GtkSourceLanguageManager *, (void), ())
S(gtk_source_language_manager_get_language, RETURN, GtkSourceLanguage *, (GtkSourceLanguageManager *lm, const gchar *id), (lm, id))
S(gtk_source_view_new_with_buffer, RETURN, GtkWidget *, (GtkSourceBuffer *buffer), (buffer))
S(gtk_source_view_set_show_line_numbers, VOID, void, (GtkSourceView *source_view, gboolean show), (source_view, show))
S(gtk_style_context_add_class, VOID, void, (GtkStyleContext *sc, const gchar *c), (sc, c))
S(gtk_style_context_add_provider_for_screen, VOID, void, (GdkScreen *screen, GtkStyleProvider *provider, guint priority), (screen, provider, priority))
S(gtk_text_buffer_get_end_iter, VOID, void, (GtkTextBuffer *buffer, GtkTextIter *iter), (buffer, iter))
S(gtk_text_buffer_get_insert, RETURN, GtkTextMark *, (GtkTextBuffer *buffer), (buffer))
S(gtk_text_buffer_get_iter_at_mark, VOID, void, (GtkTextBuffer *buffer, GtkTextIter *iter, GtkTextMark *mark), (buffer, iter, mark))
S(gtk_text_buffer_get_start_iter, VOID, void, (GtkTextBuffer *buffer, GtkTextIter *iter), (buffer, iter))
S(gtk_text_buffer_get_text, RETURN, gchar *, (GtkTextBuffer *buffer, const GtkTextIter *start, const GtkTextIter *end, gboolean include_hidden_chars), (buffer, start, end, include_hidden_chars))
S(gtk_text_buffer_set_text, VOID, void, (GtkTextBuffer *buffer, const gchar *text, gint len), (buffer, text, len))
S(gtk_text_iter_forward_to_line_end, RETURN, gboolean, (GtkTextIter *iter), (iter))
S(gtk_text_iter_set_line_offset, VOID, void, (GtkTextIter *iter, gint line_offset), (iter, line_offset))
S(gtk_text_view_get_buffer, RETURN, GtkTextBuffer *, (GtkTextView *text_view), (text_view))
S(gtk_text_view_new, RETURN, GtkWidget *, (void), ())
S(gtk_text_view_set_editable, VOID, void, (GtkTextView *text_view, gboolean setting), (text_view, setting))
S(gtk_text_view_set_wrap_mode, VOID, void, (GtkTextView *tv, GtkWrapMode m), (tv, m))
S(gtk_widget_destroy, VOID, void, (GtkWidget *widget), (widget))
S(gtk_widget_get_screen, RETURN, GdkScreen*, (GtkWidget *w), (w))
S(gtk_widget_get_style_context, RETURN, GtkStyleContext*, (GtkWidget *w), (w))
S(gtk_widget_get_toplevel, RETURN, GtkWidget *, (GtkWidget *widget), (widget))
S(gtk_widget_hide, VOID, void, (GtkWidget *widget), (widget))
S(gtk_widget_set_margin_bottom, VOID, void, (GtkWidget *widget, gint margin), (widget, margin))
S(gtk_widget_set_margin_end, VOID, void, (GtkWidget *widget, gint margin), (widget, margin))
S(gtk_widget_set_margin_start, VOID, void, (GtkWidget *widget, gint margin), (widget, margin))
S(gtk_widget_set_margin_top, VOID, void, (GtkWidget *widget, gint margin), (widget, margin))
S(gtk_widget_set_vexpand, VOID, void, (GtkWidget *widget, gboolean expand), (widget, expand))
S(gtk_widget_show_all, VOID, void, (GtkWidget *widget), (widget))
S(gtk_widget_show, VOID, void, (GtkWidget *widget), (widget))
S(gtk_window_new, RETURN, GtkWidget *, (GtkWindowType type), (type))
S(gtk_window_set_default_size, VOID, void, (GtkWindow *window, gint width, gint height), (window, width, height))
S(g_type_check_instance_is_a, RETURN, gboolean, (GTypeInstance *instance, GType instance_type), (instance, instance_type))
S(g_type_class_adjust_private_offset, VOID, void, (gpointer class, gint *private_size), (class, private_size))
S(g_type_class_peek_parent, RETURN, gpointer, (gpointer type), (type))
S(g_type_register_static_simple, RETURN, GType, (GType parent_type, const gchar *type_name, guint class_size, GClassInitFunc class_init, guint instance_size, GInstanceInitFunc instance_init, GTypeFlags flags), (parent_type, type_name, class_size, class_init, instance_size, instance_init, flags))
S(g_usleep, VOID, void, (gulong usec), (usec))
S(strlen, RETURN, size_t, (const char *s), (s))
S(strstr, RETURN, char *, (const char *haystack, const char *needle), (haystack, needle))

// We need this one to load everybody else:
//   dlsym
//
// These have varargs:
//   g_build_filename
//   g_log
//   g_object_new
//   g_print
//   g_printerr
//   g_signal_emit
//   g_signal_new
//   g_snprintf
//   g_strdup_printf
//   gtk_dialog_new_with_buttons
//   gtk_file_chooser_dialog_new
//   __snprintf_chk
//
// These have weird macro expansion issues:
//   g_once_init_enter_pointer
//   g_once_init_leave_pointer
//   g_strdup
//   g_strdup
//   g_string_append_len
//   g_string_free
//   memcpy
//   memmove
//   g_object_ref
//
// These are used in generated code, so I can't control them:
//   g_cclosure_marshal_VOID__STRING
//   g_free

// TODO: investigate these symbols:
//   __stack_chk_fail
//   __errno_location
//   strerror
//   kill
//   prctl
//   getpid
//   setsid
//   getppid

