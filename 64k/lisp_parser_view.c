#include "lisp_parser_view.h"
#include "gtk_text_provider.h"

struct _LispParserView
{
  GtkTreeView parent_instance;
  GtkSourceBuffer *buffer;
  TextProvider *provider;
  LispParser *parser;
  GtkTreeStore *store;
};

G_DEFINE_TYPE(LispParserView, lisp_parser_view, GTK_TYPE_TREE_VIEW)

enum { COL_TYPE, COL_TEXT, N_COLS };

static void populate_store(LispParserView *self);
static void parser_view_buffer_changed(GtkTextBuffer * /*buffer*/, gpointer data);

static void
lisp_parser_view_init(LispParserView *self)
{
  GtkCellRenderer *renderer;

  self->buffer = NULL;
  self->parser = NULL;
  self->store = gtk_tree_store_new(N_COLS, G_TYPE_STRING, G_TYPE_STRING);

  gtk_tree_view_set_model(GTK_TREE_VIEW(self), GTK_TREE_MODEL(self->store));

  renderer = gtk_cell_renderer_text_new();
  gtk_tree_view_insert_column_with_attributes(GTK_TREE_VIEW(self), -1, "Type",
      renderer, "text", COL_TYPE, NULL);

  renderer = gtk_cell_renderer_text_new();
  gtk_tree_view_insert_column_with_attributes(GTK_TREE_VIEW(self), -1, "Text",
      renderer, "text", COL_TEXT, NULL);
}

static void
lisp_parser_view_dispose(GObject *object)
{
  LispParserView *self = LISP_PARSER_VIEW(object);

  if (self->parser)
  {
    lisp_parser_free(self->parser);
    self->parser = NULL;
  }

  g_clear_object(&self->provider);

  if (self->buffer)
    g_signal_handlers_disconnect_by_data(self->buffer, self);

  g_clear_object(&self->buffer);
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
node_type_to_string(LispAstNodeType type)
{
  switch(type)
  {
    case LISP_AST_NODE_TYPE_ATOM: return "Atom";
    case LISP_AST_NODE_TYPE_LIST: return "List";
    case LISP_AST_NODE_TYPE_STRING: return "String";
    default: return "Unknown";
  }
}

static void
add_ast_node(LispParserView *self, const LispAstNode *node, GtkTreeIter *parent)
{
  GtkTreeIter iter;
  const gchar *type = node_type_to_string(node->type);
  const gchar *start_text = node->start_token ? node->start_token->text : "";
  gchar *text;

  if (node->end_token && node->end_token != node->start_token)
    text = g_strdup_printf("%s ... %s", start_text, node->end_token->text);
  else
    text = g_strdup(start_text);

  gtk_tree_store_append(self->store, &iter, parent);
  gtk_tree_store_set(self->store, &iter,
      COL_TYPE, type,
      COL_TEXT, text,
      -1);
  g_free(text);

  if (node->children)
  {
    for (guint i = 0; i < node->children->len; i++)
    {
      LispAstNode *child = g_array_index(node->children, LispAstNode*, i);
      add_ast_node(self, child, &iter);
    }
  }
}

static void
populate_store(LispParserView *self)
{
  if (!self->parser)
    return;

  gtk_tree_store_clear(self->store);
  lisp_parser_parse(self->parser);

  const LispAstNode *ast = lisp_parser_get_ast(self->parser);
  if (ast)
    add_ast_node(self, ast, NULL);

  gtk_tree_view_expand_all(GTK_TREE_VIEW(self));
}

static void
parser_view_buffer_changed(GtkTextBuffer * /*buffer*/, gpointer data)
{
  LispParserView *self = LISP_PARSER_VIEW(data);
  populate_store(self);
}

GtkWidget *
lisp_parser_view_new(GtkSourceBuffer *buffer)
{
  LispParserView *self = g_object_new(LISP_TYPE_PARSER_VIEW, NULL);
  self->buffer = g_object_ref(buffer);
  self->provider = gtk_text_provider_new(GTK_TEXT_BUFFER(buffer));
  self->parser = lisp_parser_new(self->provider);
  populate_store(self);
  g_signal_connect(self->buffer, "changed", G_CALLBACK(parser_view_buffer_changed), self);
  return GTK_WIDGET(self);
}

