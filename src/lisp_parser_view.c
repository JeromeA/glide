#include "lisp_parser_view.h"
#include "project.h"

struct _LispParserView {
  GtkTreeView parent_instance;
  Document *document;
  GtkTreeStore *store;
  GtkTextBuffer *buffer;
};

G_DEFINE_TYPE(LispParserView, lisp_parser_view, GTK_TYPE_TREE_VIEW)

enum { PARSER_VIEW_COL_TYPE, PARSER_VIEW_COL_TEXT, PARSER_VIEW_COL_INFO, PARSER_VIEW_N_COLS };

static void parser_view_populate_store(LispParserView *self);
static void parser_view_buffer_changed(GtkTextBuffer * /*buffer*/, gpointer data);

static void
lisp_parser_view_init(LispParserView *self)
{
  GtkCellRenderer *renderer;

  self->document = NULL;
  self->store = gtk_tree_store_new(PARSER_VIEW_N_COLS, G_TYPE_STRING, G_TYPE_STRING, G_TYPE_STRING);
  self->buffer = NULL;

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

  if (self->document) {
    if (self->buffer) {
      g_signal_handlers_disconnect_by_data(self->buffer, self);
      g_clear_object(&self->buffer);
    }
    self->document = NULL;
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
    case LISP_AST_NODE_TYPE_QUOTE: return "Quote";
    case LISP_AST_NODE_TYPE_BACKQUOTE: return "BackQuote";
    case LISP_AST_NODE_TYPE_UNQUOTE: return "Unquote";
    case LISP_AST_NODE_TYPE_UNQUOTE_SPLICING: return "UnquoteSplicing";
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
  const GString *content = document_get_content(self->document);

  if (node->end_token && node->end_token != node->start_token) {
    gsize start = node_get_start_offset(node);
    gsize end = node_get_end_offset(node);
    if (!content || start >= end || end > content->len) {
      text = g_strdup("");
    } else {
      gchar *full = g_strndup(content->str + start, end - start);
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
    }
  } else {
    text = g_strdup(start_text);
  }

  if (node->sd_type)
    info = node_debug_string(node);

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
  if (!self->document)
    return;

  gtk_tree_store_clear(self->store);
  const Node *ast = document_get_ast(self->document);
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
lisp_parser_view_new(EditorManager *manager, Document *document)
{
  g_return_val_if_fail(document != NULL, NULL);
  LispParserView *self = g_object_new(LISP_TYPE_PARSER_VIEW, NULL);
  self->document = document;
  parser_view_populate_store(self);
  GtkTextBuffer *buf = manager ? editor_manager_get_buffer(manager, document) : NULL;
  if (buf) {
    self->buffer = GTK_TEXT_BUFFER(g_object_ref(buf));
    g_signal_connect_after(buf, "changed", G_CALLBACK(parser_view_buffer_changed), self);
  }
  return GTK_WIDGET(self);
}

