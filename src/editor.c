#include "editor.h"
#include "editor_selection_manager.h"
#include "editor_tooltip_controller.h"
#include "project.h"
#include "document.h"
#include "util.h"

static const gchar *EDITOR_TAB_LABEL_MODIFIED_CLASS = "editor-tab-modified";

struct _Editor
{
  GtkScrolledWindow parent_instance;

  GtkSourceView *view;
  GtkSourceBuffer *buffer;
  Project *project;
  Document *document;
  EditorSelectionManager *selection_manager;
  GtkTextTag *function_def_tag;
  GtkTextTag *function_use_tag;
  GtkTextTag *error_tag;
  GtkTextTag *undefined_function_tag;
  GtkTextTag *ctrl_hover_tag;
  EditorTooltipController *tooltip_controller;
  gboolean ctrl_hover_active;
  gsize ctrl_hover_start;
  gsize ctrl_hover_end;
  GdkCursor *ctrl_hover_cursor;
  GdkWindow *ctrl_hover_window;
  GtkWidget *tab_label;
  gboolean auto_inserting;
};

G_DEFINE_TYPE(Editor, editor, GTK_TYPE_SCROLLED_WINDOW)

// Forward declaration for the callback
static void on_buffer_changed(GtkTextBuffer *buffer, gpointer user_data);
static void editor_on_buffer_modified_changed(GtkTextBuffer *buffer, gpointer user_data);
static gboolean editor_on_query_tooltip(GtkWidget *widget, gint x, gint y, gboolean /*keyboard_mode*/,
    GtkTooltip * /*tooltip*/, gpointer user_data);
static gboolean editor_on_button_press_event(GtkWidget *widget, GdkEventButton *event, gpointer user_data);
static gboolean editor_on_motion_notify_event(GtkWidget *widget, GdkEventMotion *event, gpointer user_data);
static gboolean editor_on_leave_notify_event(GtkWidget *widget, GdkEventCrossing *event, gpointer user_data);
static void editor_update_function_highlight(Editor *self);
static const Node *editor_find_sdt_node(Editor *self, gsize offset);
static void editor_on_mark_set(GtkTextBuffer *buffer, GtkTextIter * /*location*/, GtkTextMark *mark,
    gpointer user_data);
static void editor_clear_function_highlight(Editor *self);
static void editor_highlight_nodes(Editor *self, GPtrArray *nodes, GtkTextTag *tag);
static void editor_highlight_node(Editor *self, const Node *node, GtkTextTag *tag);
static void editor_clear_errors(Editor *self);
static void editor_update_document_from_buffer(Editor *self);
static void editor_clear_ctrl_hover(Editor *self);
static void editor_update_ctrl_hover(Editor *self, GtkWidget *widget, GdkWindow *window, gdouble x, gdouble y,
    gboolean ctrl_down);
static void editor_set_ctrl_hover_cursor(Editor *self, GtkWidget *widget, GdkWindow *window);
static void editor_update_tab_label(Editor *self);
static void editor_apply_label_color(GtkWidget *label, gboolean modified);
static void editor_on_insert_text(GtkTextBuffer *buffer, GtkTextIter *location, gchar *text, gint len,
    gpointer user_data);

static gboolean
editor_on_button_press_event(GtkWidget *widget, GdkEventButton *event, gpointer user_data)
{
  Editor *self = GLIDE_EDITOR(user_data);
  g_return_val_if_fail(GLIDE_IS_EDITOR(self), FALSE);

  if (!event || event->type != GDK_BUTTON_PRESS || event->button != 1)
    return FALSE;

  if ((event->state & GDK_CONTROL_MASK) == 0)
    return FALSE;

  GtkTextBuffer *buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(widget));
  if (!buffer)
    return FALSE;

  GtkWidget *toplevel = gtk_widget_get_toplevel(widget);
  if (!gtk_widget_is_toplevel(toplevel))
    return FALSE;

  GtkApplication *app = gtk_window_get_application(GTK_WINDOW(toplevel));
  if (!app)
    return FALSE;

  gtk_widget_grab_focus(widget);

  GtkTextWindowType window_type = event->window
      ? gtk_text_view_get_window_type(GTK_TEXT_VIEW(widget), event->window)
      : GTK_TEXT_WINDOW_WIDGET;
  gint buffer_x = 0;
  gint buffer_y = 0;
  gtk_text_view_window_to_buffer_coords(GTK_TEXT_VIEW(widget), window_type,
      (gint)event->x, (gint)event->y, &buffer_x, &buffer_y);

  GtkTextIter iter;
  gtk_text_view_get_iter_at_position(GTK_TEXT_VIEW(widget), &iter, NULL, buffer_x, buffer_y);
  gtk_text_buffer_place_cursor(buffer, &iter);

  g_action_group_activate_action(G_ACTION_GROUP(app), "goto-definition", NULL);
  editor_clear_ctrl_hover(self);
  return TRUE;
}

static gboolean
editor_on_motion_notify_event(GtkWidget *widget, GdkEventMotion *event, gpointer user_data)
{
  Editor *self = GLIDE_EDITOR(user_data);
  g_return_val_if_fail(GLIDE_IS_EDITOR(self), FALSE);
  g_return_val_if_fail(event != NULL, FALSE);

  gboolean ctrl_down = (event->state & GDK_CONTROL_MASK) != 0;
  editor_update_ctrl_hover(self, widget, event->window, event->x, event->y, ctrl_down);
  return FALSE;
}

static gboolean
editor_on_leave_notify_event(GtkWidget * /*widget*/, GdkEventCrossing * /*event*/, gpointer user_data)
{
  Editor *self = GLIDE_EDITOR(user_data);
  g_return_val_if_fail(GLIDE_IS_EDITOR(self), FALSE);
  editor_clear_ctrl_hover(self);
  return FALSE;
}

static void
editor_init(Editor *self)
{
  GtkSourceLanguageManager *lm = gtk_source_language_manager_get_default();
  GtkSourceLanguage *lang = gtk_source_language_manager_get_language(lm, "commonlisp");
  self->buffer = gtk_source_buffer_new_with_language(lang);
  self->view = GTK_SOURCE_VIEW(gtk_source_view_new());
  gtk_text_view_set_buffer(GTK_TEXT_VIEW(self->view), GTK_TEXT_BUFFER(self->buffer));
  gtk_source_view_set_show_line_numbers(self->view, TRUE);
  gtk_text_view_set_monospace(GTK_TEXT_VIEW(self->view), TRUE);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(self), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_container_add(GTK_CONTAINER(self), GTK_WIDGET(self->view));
  gtk_widget_set_has_tooltip(GTK_WIDGET(self->view), TRUE);
  gtk_widget_add_events(GTK_WIDGET(self->view),
      GDK_BUTTON_PRESS_MASK | GDK_POINTER_MOTION_MASK | GDK_LEAVE_NOTIFY_MASK);
  self->project = NULL;
  self->document = NULL;
  self->selection_manager = editor_selection_manager_new();
  self->tooltip_controller = NULL;
  GtkTextBuffer *buffer = GTK_TEXT_BUFFER(self->buffer);
  self->function_def_tag = gtk_text_buffer_create_tag(buffer, "function-def-highlight", "background", "#fef", NULL);
  self->function_use_tag = gtk_text_buffer_create_tag(buffer, "function-use-highlight", "background", "#eef", NULL);
  self->error_tag = gtk_text_buffer_create_tag(buffer, "error-highlight", "underline", PANGO_UNDERLINE_ERROR, NULL);
  self->undefined_function_tag = gtk_text_buffer_create_tag(buffer,
      "undefined-function-highlight", "foreground", "#c00", NULL);
  self->ctrl_hover_tag = gtk_text_buffer_create_tag(buffer, "function-ctrl-hover", "underline", PANGO_UNDERLINE_LOW,
      "foreground", "#06c", NULL);
  self->ctrl_hover_active = FALSE;
  self->ctrl_hover_start = 0;
  self->ctrl_hover_end = 0;
  self->ctrl_hover_cursor = NULL;
  self->ctrl_hover_window = NULL;
  self->tab_label = NULL;
  self->auto_inserting = FALSE;
}

// Callback for when the GtkTextBuffer changes
static void
on_buffer_changed(GtkTextBuffer * /*buffer*/, gpointer user_data)
{
  Editor *self = GLIDE_EDITOR(user_data);
  if (!self) {
    g_message("Editor buffer change callback invoked with NULL editor");
    return;
  }

  editor_update_document_from_buffer(self);
  editor_set_errors(self, document_get_errors(self->document));
  editor_update_function_highlight(self);
}

static void
editor_on_buffer_modified_changed(GtkTextBuffer * /*buffer*/, gpointer user_data)
{
  Editor *self = GLIDE_EDITOR(user_data);
  if (!self)
    return;

  if (!self->tab_label)
    return;

  editor_update_tab_label(self);
}

static void
editor_dispose(GObject *object)
{
  Editor *self = GLIDE_EDITOR(object);

  if (self->buffer)
    g_signal_handlers_disconnect_by_data(self->buffer, self);

  if (self->tab_label) {
    editor_apply_label_color(self->tab_label, FALSE);
    g_object_remove_weak_pointer(G_OBJECT(self->tab_label), (gpointer *) &self->tab_label);
    self->tab_label = NULL;
  }

  if (self->project) {
    project_unref(self->project);
    self->project = NULL;
  }

  self->function_def_tag = NULL;
  self->function_use_tag = NULL;
  self->error_tag = NULL;
  self->undefined_function_tag = NULL;
  self->ctrl_hover_tag = NULL;
  g_clear_object(&self->buffer);
  self->view = NULL;
  g_clear_object(&self->selection_manager);
  self->ctrl_hover_active = FALSE;
  g_clear_object(&self->ctrl_hover_cursor);
  g_clear_object(&self->ctrl_hover_window);
  if (self->tooltip_controller) {
    editor_tooltip_controller_free(self->tooltip_controller);
    self->tooltip_controller = NULL;
  }

  G_OBJECT_CLASS(editor_parent_class)->dispose(object);
}

static void
editor_class_init(EditorClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS(klass);
  object_class->dispose = editor_dispose;
}

GtkWidget *
editor_new_for_document(Project *project, Document *document)
{
  g_return_val_if_fail(project != NULL, NULL);
  g_return_val_if_fail(document != NULL, NULL);

  Editor *self = g_object_new(EDITOR_TYPE, NULL);
  self->project = project_ref(project);
  self->document = document;
  self->tooltip_controller = editor_tooltip_controller_new(GTK_WIDGET(self->view), project);

  const GString *existing = document_get_content(self->document);
  if (existing && existing->str) {
    gtk_source_buffer_begin_not_undoable_action(self->buffer);
    gtk_text_buffer_set_text(GTK_TEXT_BUFFER(self->buffer), existing->str, -1);
    gtk_source_buffer_end_not_undoable_action(self->buffer);
  }

  gtk_text_buffer_set_modified(GTK_TEXT_BUFFER(self->buffer), FALSE);
  GtkWidget *view_widget = GTK_WIDGET(self->view);
  g_signal_connect(view_widget, "query-tooltip", G_CALLBACK(editor_on_query_tooltip), self);
  g_signal_connect(view_widget, "button-press-event", G_CALLBACK(editor_on_button_press_event), self);
  g_signal_connect(view_widget, "motion-notify-event", G_CALLBACK(editor_on_motion_notify_event), self);
  g_signal_connect(view_widget, "leave-notify-event", G_CALLBACK(editor_on_leave_notify_event), self);

  GtkTextBuffer *buffer = GTK_TEXT_BUFFER(self->buffer);
  g_signal_connect(buffer, "mark-set", G_CALLBACK(editor_on_mark_set), self);
  g_signal_connect_after(buffer, "insert-text", G_CALLBACK(editor_on_insert_text), self);
  g_signal_connect(buffer, "changed", G_CALLBACK(on_buffer_changed), self);
  g_signal_connect(buffer, "modified-changed", G_CALLBACK(editor_on_buffer_modified_changed), self);
  editor_update_function_highlight(self);
  return GTK_WIDGET(self);
}


GtkSourceBuffer *
editor_get_buffer(Editor *self)
{
  g_return_val_if_fail(GLIDE_IS_EDITOR(self), NULL);
  return self->buffer;
}

Document *
editor_get_document(Editor *self)
{
  g_return_val_if_fail(GLIDE_IS_EDITOR(self), NULL);
  return self->document;
}

GtkWidget *
editor_get_view(Editor *self)
{
  g_return_val_if_fail(GLIDE_IS_EDITOR(self), NULL);
  return GTK_WIDGET(self->view);
}

EditorTooltipController *
editor_get_tooltip_controller(Editor *self)
{
  g_return_val_if_fail(GLIDE_IS_EDITOR(self), NULL);
  return self->tooltip_controller;
}

void
editor_set_tab_label(Editor *self, GtkWidget *label)
{
  g_return_if_fail(GLIDE_IS_EDITOR(self));

  if (self->tab_label) {
    editor_apply_label_color(self->tab_label, FALSE);
    g_object_remove_weak_pointer(G_OBJECT(self->tab_label), (gpointer *) &self->tab_label);
  }

  self->tab_label = label;

  if (!self->tab_label)
    return;

  g_object_add_weak_pointer(G_OBJECT(self->tab_label), (gpointer *) &self->tab_label);
  editor_update_tab_label(self);
}

static void
editor_update_tab_label(Editor *self)
{
  g_return_if_fail(GLIDE_IS_EDITOR(self));

  if (!self->tab_label)
    return;

  gboolean modified = FALSE;
  if (self->buffer)
    modified = gtk_text_buffer_get_modified(GTK_TEXT_BUFFER(self->buffer));

  editor_apply_label_color(self->tab_label, modified);
}

static void
editor_apply_label_color(GtkWidget *label, gboolean modified)
{
  g_return_if_fail(GTK_IS_WIDGET(label));

  GtkStyleContext *context = gtk_widget_get_style_context(label);
  g_return_if_fail(context != NULL);

  if (modified)
    gtk_style_context_add_class(context, EDITOR_TAB_LABEL_MODIFIED_CLASS);
  else
    gtk_style_context_remove_class(context, EDITOR_TAB_LABEL_MODIFIED_CLASS);
}

static void
editor_on_insert_text(GtkTextBuffer *buffer, GtkTextIter *location, gchar *text, gint len, gpointer user_data)
{
  Editor *self = GLIDE_EDITOR(user_data);
  g_return_if_fail(GLIDE_IS_EDITOR(self));
  g_return_if_fail(buffer != NULL);
  g_return_if_fail(location != NULL);

  if (self->auto_inserting)
    return;

  if (!text || len <= 0)
    return;

  if (g_utf8_strlen(text, len) != 1)
    return;

  gunichar inserted = g_utf8_get_char(text);
  gunichar closing = 0;

  if (inserted == '(')
    closing = ')';
  else if (inserted == '"')
    closing = '"';

  if (closing == 0)
    return;

  gchar closing_str[7];
  gint closing_len = g_unichar_to_utf8(closing, closing_str);
  closing_str[closing_len] = '\0';

  GtkTextIter iter = *location;
  self->auto_inserting = TRUE;
  gtk_text_buffer_insert(buffer, &iter, closing_str, closing_len);
  self->auto_inserting = FALSE;

  gtk_text_iter_assign(location, &iter);

  GtkTextMark *insert_mark = gtk_text_buffer_get_insert(buffer);
  GtkTextIter cursor_iter;
  gtk_text_buffer_get_iter_at_mark(buffer, &cursor_iter, insert_mark);
  if (gtk_text_iter_backward_char(&cursor_iter))
    gtk_text_buffer_place_cursor(buffer, &cursor_iter);
}

static void
editor_clear_function_highlight(Editor *self)
{
  if (!self->buffer)
    return;
  GtkTextBuffer *buffer = GTK_TEXT_BUFFER(self->buffer);
  GtkTextIter start;
  GtkTextIter end;
  gtk_text_buffer_get_bounds(buffer, &start, &end);
  if (self->function_def_tag)
    gtk_text_buffer_remove_tag(buffer, self->function_def_tag, &start, &end);
  if (self->function_use_tag)
    gtk_text_buffer_remove_tag(buffer, self->function_use_tag, &start, &end);
}

static void
editor_clear_errors(Editor *self)
{
  if (!self || !self->buffer)
    return;
  GtkTextBuffer *buffer = GTK_TEXT_BUFFER(self->buffer);
  GtkTextIter start;
  GtkTextIter end;
  gtk_text_buffer_get_bounds(buffer, &start, &end);
  if (self->error_tag)
    gtk_text_buffer_remove_tag(buffer, self->error_tag, &start, &end);
  if (self->undefined_function_tag)
    gtk_text_buffer_remove_tag(buffer, self->undefined_function_tag, &start, &end);
}

static void
editor_update_document_from_buffer(Editor *self)
{
  g_return_if_fail(GLIDE_IS_EDITOR(self));
  g_return_if_fail(self->buffer != NULL);

  editor_clear_ctrl_hover(self);

  GtkTextBuffer *buffer = GTK_TEXT_BUFFER(self->buffer);
  GtkTextIter start;
  GtkTextIter end;
  gtk_text_buffer_get_start_iter(buffer, &start);
  gtk_text_buffer_get_end_iter(buffer, &end);
  gchar *text = gtk_text_buffer_get_text(buffer, &start, &end, FALSE);
  GString *content = g_string_new(text ? text : "");
  g_free(text);
  document_set_content(self->document, content);
}

static void
editor_clear_ctrl_hover(Editor *self)
{
  g_return_if_fail(GLIDE_IS_EDITOR(self));
  g_return_if_fail(self->buffer != NULL);

  if (!self->ctrl_hover_active || !self->ctrl_hover_tag)
    return;

  GtkTextBuffer *buffer = GTK_TEXT_BUFFER(self->buffer);
  GtkTextIter start;
  GtkTextIter end;
  gtk_text_buffer_get_iter_at_offset(buffer, &start, (gint) self->ctrl_hover_start);
  gtk_text_buffer_get_iter_at_offset(buffer, &end, (gint) self->ctrl_hover_end);
  gtk_text_buffer_remove_tag(buffer, self->ctrl_hover_tag, &start, &end);
  if (self->ctrl_hover_window)
    gdk_window_set_cursor(self->ctrl_hover_window, NULL);
  g_clear_object(&self->ctrl_hover_window);
  self->ctrl_hover_active = FALSE;
  self->ctrl_hover_start = 0;
  self->ctrl_hover_end = 0;
}

static void
editor_update_ctrl_hover(Editor *self, GtkWidget *widget, GdkWindow *window, gdouble x, gdouble y, gboolean ctrl_down)
{
  g_return_if_fail(GLIDE_IS_EDITOR(self));
  g_return_if_fail(self->buffer != NULL);

  if (!ctrl_down) {
    editor_clear_ctrl_hover(self);
    return;
  }

  if (!self->ctrl_hover_tag || !widget)
    return;

  GtkTextView *view = GTK_TEXT_VIEW(widget);
  GtkTextWindowType window_type =
      window ? gtk_text_view_get_window_type(view, window) : GTK_TEXT_WINDOW_WIDGET;

  if (window_type != GTK_TEXT_WINDOW_TEXT && window_type != GTK_TEXT_WINDOW_WIDGET) {
    editor_clear_ctrl_hover(self);
    return;
  }

  gint buffer_x = 0;
  gint buffer_y = 0;
  gtk_text_view_window_to_buffer_coords(view, window_type, (gint) x, (gint) y, &buffer_x, &buffer_y);

  GtkTextIter iter;
  gtk_text_view_get_iter_at_position(view, &iter, NULL, buffer_x, buffer_y);
  gsize offset = gtk_text_iter_get_offset(&iter);
  const Node *node = editor_find_sdt_node(self, offset);

  if (!node || node->sd_type != SDT_FUNCTION_USE || node->document != self->document) {
    editor_clear_ctrl_hover(self);
    return;
  }

  const Node *highlight = node_get_symbol_name_node_const(node);
  if (!highlight)
    highlight = node;

  gsize start = node_get_start_offset(highlight);
  gsize end = node_get_end_offset(highlight);
  if (end <= start) {
    editor_clear_ctrl_hover(self);
    return;
  }

  if (self->ctrl_hover_active && self->ctrl_hover_start == start && self->ctrl_hover_end == end)
    return;

  GtkTextBuffer *buffer = GTK_TEXT_BUFFER(self->buffer);
  GtkTextIter it_start;
  GtkTextIter it_end;
  gtk_text_buffer_get_iter_at_offset(buffer, &it_start, (gint) start);
  gtk_text_buffer_get_iter_at_offset(buffer, &it_end, (gint) end);

  editor_clear_ctrl_hover(self);
  gtk_text_buffer_apply_tag(buffer, self->ctrl_hover_tag, &it_start, &it_end);
  editor_set_ctrl_hover_cursor(self, widget, window);
  self->ctrl_hover_active = TRUE;
  self->ctrl_hover_start = start;
  self->ctrl_hover_end = end;
}

static void
editor_set_ctrl_hover_cursor(Editor *self, GtkWidget *widget, GdkWindow *window)
{
  g_return_if_fail(GLIDE_IS_EDITOR(self));

  if (!widget)
    return;

  GdkWindow *target = NULL;
  if (window)
    target = window;
  if (!target)
    target = gtk_widget_get_window(widget);
  if (!target)
    return;

  if (!self->ctrl_hover_cursor) {
    GdkDisplay *display = gdk_window_get_display(target);
    self->ctrl_hover_cursor = gdk_cursor_new_for_display(display, GDK_HAND2);
  }

  if (!self->ctrl_hover_cursor)
    return;

  if (self->ctrl_hover_window != target) {
    if (self->ctrl_hover_window)
      gdk_window_set_cursor(self->ctrl_hover_window, NULL);
    g_clear_object(&self->ctrl_hover_window);
    g_object_ref(target);
    self->ctrl_hover_window = target;
  }

  gdk_window_set_cursor(target, self->ctrl_hover_cursor);
}

static void
editor_highlight_nodes(Editor *self, GPtrArray *nodes, GtkTextTag *tag)
{
  if (!nodes || !tag)
    return;
  for (guint i = 0; i < nodes->len; i++) {
    Node *node = g_ptr_array_index(nodes, i);
    editor_highlight_node(self, node, tag);
  }
}

void
editor_set_errors(Editor *self, const GArray *errors)
{
  g_return_if_fail(GLIDE_IS_EDITOR(self));
  editor_clear_errors(self);
  if (!errors || !self->buffer)
    return;
  GtkTextBuffer *buffer = GTK_TEXT_BUFFER(self->buffer);
  for (guint i = 0; i < errors->len; i++) {
    const DocumentError *err = &g_array_index(errors, DocumentError, i);
    GtkTextTag *tag = NULL;
    switch (err->type) {
      case DOCUMENT_ERROR_TYPE_UNDEFINED_FUNCTION:
        tag = self->undefined_function_tag;
        break;
      default:
        tag = self->error_tag;
        break;
    }
    if (!tag)
      continue;
    GtkTextIter start;
    GtkTextIter end;
    gtk_text_buffer_get_iter_at_offset(buffer, &start, (gint)err->start);
    gtk_text_buffer_get_iter_at_offset(buffer, &end, (gint)err->end);
    gtk_text_buffer_apply_tag(buffer, tag, &start, &end);
  }
}

static void
editor_highlight_node(Editor *self, const Node *node, GtkTextTag *tag)
{
  if (!node || !tag)
    return;
  if (node->document != self->document)
    return;
  const Node *highlight = node_get_symbol_name_node_const(node);
  if (!highlight)
    highlight = node;
  gsize start = node_get_start_offset(highlight);
  gsize end = node_get_end_offset(highlight);
  if (end <= start)
    return;
  GtkTextIter it_start;
  GtkTextIter it_end;
  GtkTextBuffer *buffer = GTK_TEXT_BUFFER(self->buffer);
  gtk_text_buffer_get_iter_at_offset(buffer, &it_start, (gint) start);
  gtk_text_buffer_get_iter_at_offset(buffer, &it_end, (gint) end);
  gtk_text_buffer_apply_tag(buffer, tag, &it_start, &it_end);
}

static const Node *
editor_find_sdt_node(Editor *self, gsize offset)
{
  g_return_val_if_fail(GLIDE_IS_EDITOR(self), NULL);
  if (!self->document)
    return NULL;

  const Node *ast = document_get_ast(self->document);
  if (!ast)
    return NULL;

  return node_find_sdt_containing_offset(ast, offset);
}

static void
editor_update_function_highlight(Editor *self)
{
  g_return_if_fail(GLIDE_IS_EDITOR(self));
  editor_clear_function_highlight(self);
  if (!self->project || !self->document)
    return;
  GtkTextBuffer *buffer = GTK_TEXT_BUFFER(self->buffer);
  GtkTextMark *insert = gtk_text_buffer_get_insert(buffer);
  GtkTextIter iter;
  gtk_text_buffer_get_iter_at_mark(buffer, &iter, insert);
  gsize offset = gtk_text_iter_get_offset(&iter);
  const Node *node = editor_find_sdt_node(self, offset);
  while (node && node->sd_type != SDT_FUNCTION_DEF && node->sd_type != SDT_FUNCTION_USE)
    node = node->parent;
  if (!node)
    return;
  const gchar *name = node_get_name(node);
  if (!name)
    return;
  GHashTable *def_table = project_get_index(self->project, SDT_FUNCTION_DEF);
  GHashTable *use_table = project_get_index(self->project, SDT_FUNCTION_USE);
  GPtrArray *defs = def_table ? g_hash_table_lookup(def_table, name) : NULL;
  GPtrArray *uses = use_table ? g_hash_table_lookup(use_table, name) : NULL;
  if (!defs && node->sd_type == SDT_FUNCTION_DEF)
    editor_highlight_node(self, node, self->function_def_tag);
  else
    editor_highlight_nodes(self, defs, self->function_def_tag);
  if (!uses && node->sd_type == SDT_FUNCTION_USE)
    editor_highlight_node(self, node, self->function_use_tag);
  else
    editor_highlight_nodes(self, uses, self->function_use_tag);
}

static void
editor_on_mark_set(GtkTextBuffer *buffer, GtkTextIter * /*location*/, GtkTextMark *mark, gpointer user_data)
{
  Editor *self = GLIDE_EDITOR(user_data);
  if (mark != gtk_text_buffer_get_insert(buffer))
    return;
  editor_update_function_highlight(self);
}

gboolean
editor_get_toplevel_range(Editor *self, gsize offset, gsize *start, gsize *end)
{
  g_return_val_if_fail(GLIDE_IS_EDITOR(self), FALSE);
  g_return_val_if_fail(start != NULL, FALSE);
  g_return_val_if_fail(end != NULL, FALSE);
  g_return_val_if_fail(self->selection_manager != NULL, FALSE);
  GtkTextBuffer *buffer = GTK_TEXT_BUFFER(self->buffer);
  if (!self->document)
    return FALSE;

  GtkTextIter end_iter;
  gtk_text_buffer_get_end_iter(buffer, &end_iter);
  gsize len = gtk_text_iter_get_offset(&end_iter);

  gsize cur_start = offset;
  gsize cur_end = offset;
  gsize new_start;
  gsize new_end;

  while (editor_selection_manager_find_parent_range(self->selection_manager, buffer, self->document,
      cur_start, cur_end, &new_start, &new_end)) {
    cur_start = new_start;
    cur_end = new_end;
    if (cur_start == 0 && cur_end == len)
      break;
  }

  if (cur_start == offset && cur_end == offset)
    return FALSE;

  *start = cur_start;
  *end = cur_end;
  return TRUE;
}

static gboolean
editor_on_query_tooltip(GtkWidget *widget, gint x, gint y, gboolean /*keyboard_mode*/,
    GtkTooltip * /*tooltip*/, gpointer user_data)
{
  g_assert(glide_is_ui_thread());
  Editor *self = GLIDE_EDITOR(user_data);
  g_return_val_if_fail(self != NULL, FALSE);

  return editor_tooltip_controller_query(self->tooltip_controller, self, widget, x, y);
}

void
editor_extend_selection(Editor *self)
{
  g_return_if_fail(GLIDE_IS_EDITOR(self));
  if (!self->selection_manager)
    return;

  GtkTextBuffer *buffer = GTK_TEXT_BUFFER(self->buffer);
  editor_selection_manager_extend(self->selection_manager, buffer, self->document);
}

void
editor_shrink_selection(Editor *self)
{
  g_return_if_fail(GLIDE_IS_EDITOR(self));
  if (!self->selection_manager)
    return;

  GtkTextBuffer *buffer = GTK_TEXT_BUFFER(self->buffer);
  editor_selection_manager_shrink(self->selection_manager, buffer);
}

