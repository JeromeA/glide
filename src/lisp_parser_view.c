#include "lisp_parser_view.h"
#include "gtk_text_provider.h"
#include "project.h"

struct _LispParserView {
  GtkTreeView parent_instance;
  ProjectFile *file;
  GtkTreeStore *store;
};

G_DEFINE_TYPE(LispParserView, lisp_parser_view, GTK_TYPE_TREE_VIEW)

enum { PARSER_VIEW_COL_TYPE, PARSER_VIEW_COL_TEXT, PARSER_VIEW_COL_INFO, PARSER_VIEW_N_COLS };

static void parser_view_populate_store(LispParserView *self);
static void parser_view_buffer_changed(GtkTextBuffer * /*buffer*/, gpointer data);

static void
lisp_parser_view_init(LispParserView *self)
{
  GtkCellRenderer *renderer;

  self->file = NULL;
  self->store = gtk_tree_store_new(PARSER_VIEW_N_COLS, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING);

  gtk_tree_view_set_model(GTK_TREE_VIEW(self), GTK_TREE_MODEL(self->store));

  renderer = gtk_cell_renderer_text_new();
  gtk_tree_view_insert_column_with_attributes(GTK_TREE_VIEW(self), -1, "Type",
      renderer, "text", PARSER_VIEW_COL_TYPE, NULL);

  renderer = gtk_cell_renderer_text_new();
  gtk_tree_view_insert_column_with_attributes(GTK_TREE_VIEW(self), -1, "Text",
      renderer, "text", PARSER_VIEW_COL_TEXT, NULL);

  renderer = gtk_cell_renderer_text_new();
  gtk_tree_view_insert_column_with_attributes(GTK_TREE_VIEW(self), -1, "Info",
      renderer, "text", PARSER_VIEW_COL_INFO, NULL);
}

static void
lisp_parser_view_dispose(GObject *object)
{
  LispParserView *self = LISP_PARSER_VIEW(object);

  if (self->file) {
    GtkTextBuffer *buf = project_file_get_buffer(self->file);
    if (buf)
      g_signal_handlers_disconnect_by_data(buf, self);
    self->file = NULL;
  }
  g_clear_object(&self->store);

  G_OBJECT_CLASS(lisp_parser_view_parent_class)->dispose(object);
}

static void
lisp_parser_view_class_init(LispParserViewClass *klass)
{
  GObjectClass *object_class = G_OBJECT_CLASS(klass);
  object_class->dispose = lisp_parser_view_dispose;
}

static const gchar *
node_type_to_string(LispAstNodeType type) {
  switch(type) {
    case LISP_AST_NODE_TYPE_NUMBER: return "Number";
    case LISP_AST_NODE_TYPE_SYMBOL: return "Symbol";
    case LISP_AST_NODE_TYPE_SYMBOL_PACKAGE: return "SymbolPackage";
    case LISP_AST_NODE_TYPE_SYMBOL_SEPARATOR: return "SymbolSeparator";
    case LISP_AST_NODE_TYPE_SYMBOL_NAME: return "SymbolName";
    case LISP_AST_NODE_TYPE_LIST: return "List";
    case LISP_AST_NODE_TYPE_STRING: return "String";
    default: return "Unknown";
  }
}

static void
add_ast_node(LispParserView *self, const Node *node, GtkTreeIter *parent) {
  GtkTreeIter iter;
  const gchar *type = node_type_to_string(node->type);
  const gchar *start_text = node->start_token ? node->start_token->text : "";
  gchar *text;
  gchar *info = NULL;

  if (node->end_token && node->end_token != node->start_token) {
    TextProvider *provider = project_file_get_provider(self->file);
    gsize start = node->start_token->start_offset;
    gsize end = node->end_token->end_offset;
    gchar *full = text_provider_get_text(provider, start, end);
    gsize len = g_utf8_strlen(full, -1);
    if (len < 10) {
      text = full;
    } else if (len > 100) {
      gchar *head = g_utf8_substring(full, 0, 9);
      text = g_strconcat(head, "\xE2\x80\xA6", NULL);
      g_free(head);
      g_free(full);
    } else {
      text = full;
    }
  } else {
    text = g_strdup(start_text);
  }

  if (node->sd_type)
    info = node_to_string(node);

  gtk_tree_store_append(self->store, &iter, parent);
  gtk_tree_store_set(self->store, &iter,
      PARSER_VIEW_COL_TYPE, type,
      PARSER_VIEW_COL_TEXT, text,
      PARSER_VIEW_COL_INFO, info,
      -1);
  g_free(text);
  g_free(info);

  if (node->children) {
    for (guint i = 0; i < node->children->len; i++) {
      Node *child = g_array_index(node->children, Node*, i);
      add_ast_node(self, child, &iter);
    }
  }
}

static void
parser_view_populate_store(LispParserView *self) {
  if (!self->file)
    return;

  gtk_tree_store_clear(self->store);
  LispParser *parser = project_file_get_parser(self->file);
  const Node *ast = lisp_parser_get_ast(parser);
  if (ast) {
    add_ast_node(self, ast, NULL);
    GtkTreePath *path = gtk_tree_path_new_from_indices(0, -1);
    gtk_tree_view_expand_row(GTK_TREE_VIEW(self), path, FALSE);
    gtk_tree_path_free(path);
  }
}

static void
parser_view_buffer_changed(GtkTextBuffer * /*buffer*/, gpointer data)
{
  LispParserView *self = LISP_PARSER_VIEW(data);
  parser_view_populate_store(self);
}

GtkWidget *
lisp_parser_view_new(ProjectFile *file)
{
  g_return_val_if_fail(file != NULL, NULL);
  LispParserView *self = g_object_new(LISP_TYPE_PARSER_VIEW, NULL);
  self->file = file;
  parser_view_populate_store(self);
  GtkTextBuffer *buf = project_file_get_buffer(file);
  if (buf)
    g_signal_connect_after(buf, "changed", G_CALLBACK(parser_view_buffer_changed), self);
  return GTK_WIDGET(self);
}

