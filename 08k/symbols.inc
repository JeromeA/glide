S(exit, NORETURN, void, (int status), (status))
S(g_clear_error, VOID, void, (GError **err), (err))
S(g_free, VOID, void, (gpointer mem), (mem))
S(g_get_user_config_dir, RETURN, const gchar *, (void), ())
S(g_intern_static_string, RETURN, const gchar *, (const gchar *string), (string))
S(g_key_file_free, VOID, void, (GKeyFile *key_file), (key_file))
S(g_key_file_get_string, RETURN, gchar *, (GKeyFile *key_file, const gchar *group_name, const gchar *key, GError **error), (key_file, group_name, key, error))
S(g_key_file_load_from_file, RETURN, gboolean, (GKeyFile *key_file, const gchar *file, GKeyFileFlags flags, GError **error), (key_file, file, flags, error))
S(g_key_file_new, RETURN, GKeyFile *, (void), ())
S(g_key_file_set_string, VOID, void, (GKeyFile *key_file, const gchar *group_name, const gchar *key, const gchar *value), (key_file, group_name, key, value))
S(g_malloc, RETURN, gpointer, (gsize n_bytes), (n_bytes))
S(g_signal_connect_data, RETURN, gulong, (gpointer instance, const gchar *detailed_signal, GCallback c_handler, gpointer data, GClosureNotify destroy_data, GConnectFlags connect_flags), (instance, detailed_signal, c_handler, data, destroy_data, connect_flags))
S(g_strcmp0, RETURN, gint, (const gchar *str1, const gchar *str2), (str1, str2))
S(gtk_box_new, RETURN, GtkWidget *, (GtkOrientation orientation, gint spacing), (orientation, spacing))
S(gtk_box_pack_start, VOID, void, (GtkBox *box, GtkWidget *child, gboolean expand, gboolean fill, guint padding), (box, child, expand, fill, padding))
S(gtk_combo_box_set_active_id, RETURN, gboolean, (GtkComboBox *combo_box, const gchar *id), (combo_box, id))
S(gtk_combo_box_text_append, VOID, void, (GtkComboBoxText *combo_box, const gchar *text, const gchar *id), (combo_box, text, id))
S(gtk_combo_box_text_new, RETURN, GtkWidget *, (void), ())
S(gtk_container_add, VOID, void, (GtkContainer *container, GtkWidget *widget), (container, widget))
S(gtk_dialog_get_content_area, RETURN, GtkWidget *, (GtkDialog *dialog), (dialog))
S(gtk_dialog_run, RETURN, gint, (GtkDialog *dialog), (dialog))
S(gtk_file_chooser_get_filename, RETURN, gchar *, (GtkFileChooser *chooser), (chooser))
S(gtk_file_chooser_set_do_overwrite_confirmation, VOID, void, (GtkFileChooser *chooser, gboolean do_overwrite_confirmation), (chooser, do_overwrite_confirmation))
S(gtk_grid_attach, VOID, void, (GtkGrid *grid, GtkWidget *child, gint left, gint top, gint width, gint height), (grid, child, left, top, width, height))
S(gtk_grid_new, RETURN, GtkWidget *, (void), ())
S(gtk_grid_set_column_spacing, VOID, void, (GtkGrid *grid, guint spacing), (grid, spacing))
S(gtk_grid_set_row_spacing, VOID, void, (GtkGrid *grid, guint spacing), (grid, spacing))
S(gtk_init, VOID, void, (int *argc, char ***argv), (argc, argv))
S(gtk_label_new, RETURN, GtkWidget *, (const gchar *str), (str))
S(gtk_main_quit, VOID, void, (void), ())
S(gtk_main, VOID, void, (void), ())
S(gtk_menu_bar_new, RETURN, GtkWidget *, (void), ())
S(gtk_menu_item_new_with_label, RETURN, GtkWidget *, (const gchar *label), (label))
S(gtk_menu_item_set_submenu, VOID, void, (GtkMenuItem *menu_item, GtkWidget *submenu), (menu_item, submenu))
S(gtk_menu_new, RETURN, GtkWidget *, (void), ())
S(gtk_menu_shell_append, VOID, void, (GtkMenuShell *menu_shell, GtkWidget *child), (menu_shell, child))
S(gtk_scrolled_window_new, RETURN, GtkWidget *, (GtkAdjustment *hadjustment, GtkAdjustment *vadjustment), (hadjustment, vadjustment))
S(gtk_scrolled_window_set_policy, VOID, void, (GtkScrolledWindow *scrolled_window, GtkPolicyType hscrollbar_policy, GtkPolicyType vscrollbar_policy), (scrolled_window, hscrollbar_policy, vscrollbar_policy))
S(gtk_source_buffer_new_with_language, RETURN, GtkSourceBuffer *, (GtkSourceLanguage *language), (language))
S(gtk_source_language_manager_get_default, RETURN, GtkSourceLanguageManager *, (void), ())
S(gtk_source_language_manager_get_language, RETURN, GtkSourceLanguage *, (GtkSourceLanguageManager *lm, const gchar *id), (lm, id))
S(gtk_source_view_new_with_buffer, RETURN, GtkWidget *, (GtkSourceBuffer *buffer), (buffer))
S(gtk_source_view_set_show_line_numbers, VOID, void, (GtkSourceView *source_view, gboolean show), (source_view, show))
S(gtk_text_buffer_get_end_iter, VOID, void, (GtkTextBuffer *buffer, GtkTextIter *iter), (buffer, iter))
S(gtk_text_buffer_get_start_iter, VOID, void, (GtkTextBuffer *buffer, GtkTextIter *iter), (buffer, iter))
S(gtk_text_buffer_get_text, RETURN, gchar *, (GtkTextBuffer *buffer, const GtkTextIter *start, const GtkTextIter *end, gboolean include_hidden_chars), (buffer, start, end, include_hidden_chars))
S(gtk_text_buffer_set_text, VOID, void, (GtkTextBuffer *buffer, const gchar *text, gint len), (buffer, text, len))
S(gtk_widget_destroy, VOID, void, (GtkWidget *widget), (widget))
S(gtk_widget_set_margin_bottom, VOID, void, (GtkWidget *widget, gint margin), (widget, margin))
S(gtk_widget_set_margin_end, VOID, void, (GtkWidget *widget, gint margin), (widget, margin))
S(gtk_widget_set_margin_start, VOID, void, (GtkWidget *widget, gint margin), (widget, margin))
S(gtk_widget_set_margin_top, VOID, void, (GtkWidget *widget, gint margin), (widget, margin))
S(gtk_widget_show_all, VOID, void, (GtkWidget *widget), (widget))
S(gtk_window_new, RETURN, GtkWidget *, (GtkWindowType type), (type))
S(gtk_window_set_default_size, VOID, void, (GtkWindow *window, gint width, gint height), (window, width, height))
S(g_file_test, RETURN, gboolean, (const char *filename, GFileTest test), (filename, test))
S(g_type_class_peek_parent, RETURN, gpointer, (gpointer type), (type))
S(g_type_register_static_simple, RETURN, GType, (GType parent_type, const gchar *type_name, guint class_size, GClassInitFunc class_init, guint instance_size, GInstanceInitFunc instance_init, GTypeFlags flags), (parent_type, type_name, class_size, class_init, instance_size, instance_init, flags))
S(strlen, RETURN, size_t, (const char *s), (s))
S(g_type_class_adjust_private_offset, VOID, void, (gpointer class, gint *private_size), (class, private_size))
S(g_key_file_save_to_file, RETURN, gboolean, (GKeyFile *key_file, const gchar *file, GError **error), (key_file, file, error))
S(g_ptr_array_add, VOID, void, (GPtrArray *array, gpointer data), (array, data))
S(gtk_widget_hide, VOID, void, (GtkWidget *widget), (widget))
S(g_ptr_array_new_with_free_func, RETURN, GPtrArray *, (GDestroyNotify element_free_func), (element_free_func))
S(gtk_combo_box_get_active_id, RETURN, const gchar *, (GtkComboBox *combo_box), (combo_box))

// We need this one to load everybody else:
//   dlsym
//
// These have varargs:
//   gtk_dialog_new_with_buttons
//   gtk_file_chooser_dialog_new
//   g_printerr
//
// These have weird macro expansion issues:
//   g_once_init_enter_pointer
//   g_once_init_leave_pointer

// TODO: sort out the following ones:
//   g_strdup: resq 1
//   g_cclosure_marshal_VOID__STRING: resq 1
//   g_build_filename: resq 1
//   g_malloc0: resq 1
//   __errno_location: resq 1
//   gtk_main_quit: resq 1
//   g_signal_emit: resq 1
//   g_ptr_array_free: resq 1
//   g_object_new: resq 1
//   __stack_chk_fail: resq 1
//   g_signal_new: resq 1
//   g_free: resq 1

