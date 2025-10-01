#include "actions.h"
#include "file_save.h"
#include "project_open.h"
#include "file_rename.h"
#include "file_new.h"
#include "file_add.h"
#include "file_delete.h"
#include "project_new_wizard.h"
#include "preferences_dialog.h"
#include "evaluate.h"
#include "lisp_parser_view.h"
#include "document.h"
#include "editor_container.h"
#include "editor.h"
#include "node.h"
#include "util.h"

static gboolean app_maybe_save_all(App *self);
static void show_parser(App *self);
static void show_editor_tooltip(App *self);
static void goto_definition(GSimpleAction *action, GVariant *param, gpointer data);

/* === Action callbacks ==================================================== */

static void
project_new_cb(GSimpleAction * /*action*/, GVariant * /*param*/, gpointer data)
{
  project_new_wizard(NULL, data);
}

static void
project_open_cb(GSimpleAction * /*action*/, GVariant * /*param*/, gpointer data)
{
  project_open(NULL, data);
}

static void
recent_project(GSimpleAction * /*action*/, GVariant *param, gpointer data)
{
  App *self = data;
  const gchar *path = g_variant_get_string(param, NULL);
  if (path)
    project_open_path(self, path);
}

static void
file_new_action(GSimpleAction * /*action*/, GVariant * /*param*/, gpointer data)
{
  file_new(NULL, data);
}

static void
file_add_action(GSimpleAction * /*action*/, GVariant * /*param*/, gpointer data)
{
  file_add(NULL, data);
}

static void
save_all(GSimpleAction * /*action*/, GVariant * /*param*/, gpointer data)
{
  App *self = data;
  file_save_all(app_get_editor_manager(self), app_get_project(self));
}

static void
close_project(GSimpleAction * /*action*/, GVariant * /*param*/, gpointer data)
{
  app_close_project(data, TRUE);
}

static void
preferences(GSimpleAction * /*action*/, GVariant * /*param*/, gpointer data)
{
  on_preferences(NULL, data);
}

static void
quit(GSimpleAction * /*action*/, GVariant * /*param*/, gpointer data)
{
  app_on_quit(data);
}

static void
extend_selection(GSimpleAction * /*action*/, GVariant * /*param*/, gpointer data)
{
  App *self = data;
  EditorContainer *editor_container = app_get_editor_container(self);
  Editor *view = editor_container_get_current_editor(editor_container);
  if (view)
    editor_extend_selection(view);
}

static void
shrink_selection(GSimpleAction * /*action*/, GVariant * /*param*/, gpointer data)
{
  App *self = data;
  EditorContainer *editor_container = app_get_editor_container(self);
  Editor *view = editor_container_get_current_editor(editor_container);
  if (view)
    editor_shrink_selection(view);
}

static void
rename_file(GSimpleAction * /*action*/, GVariant * /*param*/, gpointer data)
{
  file_rename(NULL, data);
}

static void
delete_file(GSimpleAction * /*action*/, GVariant * /*param*/, gpointer data)
{
  file_delete(NULL, data);
}

static void
show_parser_action(GSimpleAction * /*action*/, GVariant * /*param*/, gpointer data)
{
  show_parser(data);
}

static void
show_tooltip_action(GSimpleAction * /*action*/, GVariant * /*param*/, gpointer data)
{
  show_editor_tooltip(data);
}

static void
goto_definition(GSimpleAction * /*action*/, GVariant * /*param*/, gpointer data)
{
  App *self = data;
  LOG(1, "Actions.goto_definition");
  g_return_if_fail(GLIDE_IS_APP(self));

  Editor *editor = app_get_editor(self);
  if (!editor) {
    LOG(1, "Actions.goto_definition: no active editor");
    return;
  }

  GtkTextBuffer *buffer = GTK_TEXT_BUFFER(editor_get_buffer(editor));
  g_return_if_fail(buffer != NULL);

  GtkTextIter iter;
  gtk_text_buffer_get_iter_at_mark(buffer, &iter, gtk_text_buffer_get_insert(buffer));
  gsize offset = gtk_text_iter_get_offset(&iter);

  Document *document = editor_get_document(editor);
  g_return_if_fail(document != NULL);

  const Node *ast = document_get_ast(document);
  g_return_if_fail(ast != NULL);

  const Node *node = node_find_sdt_containing_offset(ast, offset);
  while (node && node->sd_type != SDT_FUNCTION_USE)
    node = node->parent;
  if (!node) {
    LOG(1, "Actions.goto_definition: no function use at caret");
    return;
  }

  const gchar *name = node_get_name(node);
  if (!name) {
    LOG(1, "Actions.goto_definition: unnamed node");
    return;
  }

  Project *project = app_get_project(self);
  g_return_if_fail(project != NULL);

  GHashTable *def_table = project_get_index(project, SDT_FUNCTION_DEF);
  GPtrArray *defs = def_table ? g_hash_table_lookup(def_table, name) : NULL;
  if (!defs || defs->len == 0) {
    LOG(1, "Actions.goto_definition: no definitions for %s", name);
    return;
  }

  Node *target = NULL;
  for (guint i = 0; i < defs->len && !target; i++)
    target = g_ptr_array_index(defs, i);
  if (!target) {
    LOG(1, "Actions.goto_definition: null definition node");
    return;
  }

  Document *target_document = target->document;
  if (!target_document) {
    LOG(1, "Actions.goto_definition: definition without document");
    return;
  }

  EditorManager *manager = app_get_editor_manager(self);
  Editor *target_editor = manager ? editor_manager_get_editor(manager, target_document) : NULL;
  if (!target_editor) {
    LOG(1, "Actions.goto_definition: no editor for definition");
    return;
  }

  EditorContainer *container = app_get_editor_container(self);
  g_return_if_fail(container != NULL);

  editor_container_focus_editor(container, target_editor);

  GtkTextBuffer *target_buffer = GTK_TEXT_BUFFER(editor_get_buffer(target_editor));
  if (!target_buffer) {
    LOG(1, "Actions.goto_definition: missing target buffer");
    return;
  }

  const Node *symbol = node_get_symbol_name_node_const(target);
  const Node *jump_node = symbol ? symbol : target;
  gsize start_offset = node_get_start_offset(jump_node);
  GtkTextIter def_iter;
  gtk_text_buffer_get_iter_at_offset(target_buffer, &def_iter, (gint)start_offset);
  gtk_text_buffer_place_cursor(target_buffer, &def_iter);
  GtkWidget *view = editor_get_view(target_editor);
  gtk_text_view_scroll_to_iter(GTK_TEXT_VIEW(view), &def_iter, 0.25, FALSE, 0, 0);
  gtk_widget_grab_focus(view);
}

static void
eval_toplevel(GSimpleAction * /*action*/, GVariant * /*param*/, gpointer data)
{
  on_evaluate_toplevel(NULL, data);
}

static void
eval_selection(GSimpleAction * /*action*/, GVariant * /*param*/, gpointer data)
{
  on_evaluate_selection(NULL, data);
}

static void
eval_current(GSimpleAction * /*action*/, GVariant * /*param*/, gpointer data)
{
  App *self = data;
  EditorContainer *editor_container = app_get_editor_container(self);
  Editor *view = editor_container_get_current_editor(editor_container);
  GtkTextBuffer *buffer = view ? GTK_TEXT_BUFFER(editor_get_buffer(view)) : NULL;
  GtkTextIter it_start;
  GtkTextIter it_end;
  if (buffer && gtk_text_buffer_get_selection_bounds(buffer,
      &it_start, &it_end))
    on_evaluate_selection(NULL, self);
  else
    on_evaluate_toplevel(NULL, self);
}

/* === Action table ======================================================== */

static const GActionEntry app_entries[] = {
  { .name = "project-new", .activate = project_new_cb },
  { .name = "project-open", .activate = project_open_cb },
  { .name = "recent-project", .activate = recent_project, .parameter_type = "s" },
  { .name = "file-new", .activate = file_new_action },
  { .name = "file-add", .activate = file_add_action },
  { .name = "save-all", .activate = save_all },
  { .name = "close-project", .activate = close_project },
  { .name = "preferences", .activate = preferences },
  { .name = "quit", .activate = quit },
  { .name = "extend-selection", .activate = extend_selection },
  { .name = "shrink-selection", .activate = shrink_selection },
  { .name = "file-rename", .activate = rename_file },
  { .name = "file-delete", .activate = delete_file },
  { .name = "show-parser", .activate = show_parser_action },
  { .name = "show-tooltip", .activate = show_tooltip_action },
  { .name = "goto-definition", .activate = goto_definition },
  { .name = "eval-toplevel", .activate = eval_toplevel },
  { .name = "eval-selection", .activate = eval_selection },
  { .name = "eval", .activate = eval_current },
};

/* === Public functions ==================================================== */

void
actions_init(App *self)
{
  g_action_map_add_action_entries(G_ACTION_MAP(self), app_entries,
      G_N_ELEMENTS(app_entries), self);

  const gchar *eval_accels[] = {"<Alt>Return", NULL};
  gtk_application_set_accels_for_action(GTK_APPLICATION(self),
      "app.eval", eval_accels);
  const gchar *parser_accels[] = {"<Alt>p", NULL};
  gtk_application_set_accels_for_action(GTK_APPLICATION(self),
      "app.show-parser", parser_accels);
  const gchar *tooltip_accels[] = {"<Alt>t", NULL};
  gtk_application_set_accels_for_action(GTK_APPLICATION(self),
      "app.show-tooltip", tooltip_accels);
  const gchar *save_all_accels[] = {"<Primary>s", NULL};
  gtk_application_set_accels_for_action(GTK_APPLICATION(self),
      "app.save-all", save_all_accels);
  const gchar *shrink_accels[] = {"<Primary><Shift>w", NULL};
  gtk_application_set_accels_for_action(GTK_APPLICATION(self),
      "app.shrink-selection", shrink_accels);
  const gchar *extend_accels[] = {"<Primary>w", NULL};
  gtk_application_set_accels_for_action(GTK_APPLICATION(self),
      "app.extend-selection", extend_accels);
  const gchar *rename_accels[] = {"<Shift>F6", NULL};
  gtk_application_set_accels_for_action(GTK_APPLICATION(self),
      "app.file-rename", rename_accels);
  const gchar *goto_definition_accels[] = {"<Primary>b", NULL};
  gtk_application_set_accels_for_action(GTK_APPLICATION(self),
      "app.goto-definition", goto_definition_accels);
  const gchar *quit_accels[] = {"<Primary>q", NULL};
  gtk_application_set_accels_for_action(GTK_APPLICATION(self),
      "app.quit", quit_accels);
}

gboolean
on_quit_delete_event(GtkWidget * /*widget*/, GdkEvent * /*event*/, gpointer data)
{
  LOG(1, "Actions.on_quit_delete_event");
  app_on_quit(GLIDE_APP(data));
  return TRUE;
}

/* === Helpers ============================================================== */

static void
show_parser(App *self)
{
  GtkWidget *win = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(win), "Parser View");
  EditorContainer *editor_container = app_get_editor_container(self);
  Editor *current = editor_container_get_current_editor(editor_container);
  Document *document = editor_get_document(current);
  EditorManager *manager = app_get_editor_manager(self);
  GtkWidget *view = lisp_parser_view_new(manager, document);
  GtkWidget *scrolled = gtk_scrolled_window_new(NULL, NULL);
  gtk_container_add(GTK_CONTAINER(scrolled), view);
  gtk_container_add(GTK_CONTAINER(win), scrolled);
  gtk_widget_show_all(win);
}

static void
show_editor_tooltip(App *self)
{
  g_assert(glide_is_ui_thread());
  g_return_if_fail(self != NULL);

  EditorContainer *editor_container = app_get_editor_container(self);
  g_return_if_fail(editor_container != NULL);

  Editor *current = editor_container_get_current_editor(editor_container);
  if (!current) {
    LOG(1, "Actions.show_editor_tooltip: no current editor");
    return;
  }

  if (!editor_show_tooltip_window(current))
    LOG(1, "Actions.show_editor_tooltip: no tooltip content");
}

static gboolean
app_maybe_save_all(App *self)
{
  EditorContainer *editor_container = app_get_editor_container(self);
  if (!editor_container)
    return TRUE;
  gint pages = gtk_notebook_get_n_pages(GTK_NOTEBOOK(editor_container));
  gboolean modified = FALSE;
  for (gint i = 0; i < pages; i++) {
    GtkWidget *view = gtk_notebook_get_nth_page(GTK_NOTEBOOK(editor_container), i);
    GtkTextBuffer *buffer = view ? GTK_TEXT_BUFFER(editor_get_buffer(GLIDE_EDITOR(view))) : NULL;
    if (buffer && gtk_text_buffer_get_modified(buffer)) {
      modified = TRUE;
      break;
    }
  }
  if (!modified)
    return TRUE;
  GtkWidget *dialog = gtk_message_dialog_new(NULL, GTK_DIALOG_MODAL,
      GTK_MESSAGE_QUESTION, GTK_BUTTONS_NONE,
      "Save changes to project?");
  gtk_dialog_add_button(GTK_DIALOG(dialog), "_Cancel", GTK_RESPONSE_CANCEL);
  gtk_dialog_add_button(GTK_DIALOG(dialog), "_Discard", GTK_RESPONSE_REJECT);
  gtk_dialog_add_button(GTK_DIALOG(dialog), "_Save", GTK_RESPONSE_ACCEPT);
  gint res = gtk_dialog_run(GTK_DIALOG(dialog));
  gtk_widget_destroy(dialog);
  if (res == GTK_RESPONSE_CANCEL)
    return FALSE;
  if (res == GTK_RESPONSE_ACCEPT)
    file_save_all(app_get_editor_manager(self), app_get_project(self));
  return TRUE;
}

gboolean
app_close_project(App *self, gboolean forget_project)
{
  LOG(1, "Actions.app_close_project forget=%d", forget_project);
  g_return_val_if_fail(GLIDE_IS_APP(self), FALSE);
  if (!app_maybe_save_all(self))
    return FALSE;
  Project *project = app_get_project(self);
  project_clear(project);
  Preferences *prefs = app_get_preferences(self);
  if (prefs && forget_project) {
    preferences_set_project_file(prefs, NULL);
    preferences_set_last_file(prefs, NULL);
    preferences_set_cursor_position(prefs, 0);
  }
  app_update_project_view(self);
  return TRUE;
}

