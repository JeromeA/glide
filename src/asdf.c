#include "asdf.h"
#include "lisp_lexer.h"
#include "lisp_parser.h"
#include "string_text_provider.h"
#include <glib/gstdio.h>

struct _Asdf {
  GObject parent_instance;
  gchar *filename;
  gchar *description;
  gboolean serial;
  GPtrArray *components; /* gchar* */
  GPtrArray *depends_on; /* gchar* */
};

static void asdf_finalize(GObject *object);

G_DEFINE_TYPE(Asdf, asdf, G_TYPE_OBJECT)

static void asdf_class_init(AsdfClass *klass) {
  GObjectClass *obj = G_OBJECT_CLASS(klass);
  obj->finalize = asdf_finalize;
}

static void asdf_init(Asdf *self) {
  self->filename = NULL;
  self->description = NULL;
  self->serial = FALSE;
  self->components = g_ptr_array_new_with_free_func(g_free);
  self->depends_on = g_ptr_array_new_with_free_func(g_free);
}

static void asdf_finalize(GObject *object) {
  Asdf *self = GLIDE_ASDF(object);
  g_free(self->filename);
  g_free(self->description);
  if (self->components)
    g_ptr_array_free(self->components, TRUE);
  if (self->depends_on)
    g_ptr_array_free(self->depends_on, TRUE);
  G_OBJECT_CLASS(asdf_parent_class)->finalize(object);
}

Asdf *asdf_new(void) {
  return g_object_new(ASDF_TYPE, NULL);
}

static void parse_file_contents(Asdf *self, const gchar *contents);

Asdf *asdf_new_from_file(const gchar *filename) {
  Asdf *self = asdf_new();
  self->filename = g_strdup(filename);
  gchar *contents = NULL;
  if (g_file_get_contents(filename, &contents, NULL, NULL)) {
    parse_file_contents(self, contents);
    g_free(contents);
  }
  return self;
}

const gchar *asdf_get_filename(Asdf *self) {
  g_return_val_if_fail(GLIDE_IS_ASDF(self), NULL);
  return self->filename;
}

void asdf_set_description(Asdf *self, const gchar *description) {
  g_return_if_fail(GLIDE_IS_ASDF(self));
  g_free(self->description);
  self->description = g_strdup(description);
}

const gchar *asdf_get_description(Asdf *self) {
  g_return_val_if_fail(GLIDE_IS_ASDF(self), NULL);
  return self->description;
}

void asdf_set_serial(Asdf *self, gboolean serial) {
  g_return_if_fail(GLIDE_IS_ASDF(self));
  self->serial = serial;
}

gboolean asdf_get_serial(Asdf *self) {
  g_return_val_if_fail(GLIDE_IS_ASDF(self), FALSE);
  return self->serial;
}

void asdf_add_component(Asdf *self, const gchar *file) {
  g_return_if_fail(GLIDE_IS_ASDF(self));
  g_ptr_array_add(self->components, g_strdup(file));
}

const gchar *asdf_get_component(Asdf *self, guint index) {
  g_return_val_if_fail(GLIDE_IS_ASDF(self), NULL);
  if (index >= self->components->len)
    return NULL;
  return g_ptr_array_index(self->components, index);
}

uint asdf_get_component_count(Asdf *self) {
  g_return_val_if_fail(GLIDE_IS_ASDF(self), 0);
  return self->components->len;
}

void asdf_remove_component(Asdf *self, const gchar *file) {
  g_return_if_fail(GLIDE_IS_ASDF(self));
  for (guint i = 0; i < self->components->len; i++) {
    gchar *comp = g_ptr_array_index(self->components, i);
    if (g_strcmp0(comp, file) == 0) {
      g_ptr_array_remove_index(self->components, i);
      break;
    }
  }
}

void asdf_rename_component(Asdf *self, const gchar *old_file, const gchar *new_file) {
  g_return_if_fail(GLIDE_IS_ASDF(self));
  for (guint i = 0; i < self->components->len; i++) {
    gchar *comp = g_ptr_array_index(self->components, i);
    if (g_strcmp0(comp, old_file) == 0) {
      g_free(comp);
      g_ptr_array_index(self->components, i) = g_strdup(new_file);
      break;
    }
  }
}

void asdf_add_dependency(Asdf *self, const gchar *dep) {
  g_return_if_fail(GLIDE_IS_ASDF(self));
  g_ptr_array_add(self->depends_on, g_strdup(dep));
}

const gchar *asdf_get_dependency(Asdf *self, guint index) {
  g_return_val_if_fail(GLIDE_IS_ASDF(self), NULL);
  if (index >= self->depends_on->len)
    return NULL;
  return g_ptr_array_index(self->depends_on, index);
}

uint asdf_get_dependency_count(Asdf *self) {
  g_return_val_if_fail(GLIDE_IS_ASDF(self), 0);
  return self->depends_on->len;
}

static void parse_file_contents(Asdf *self, const gchar *contents) {
  TextProvider *provider = string_text_provider_new(contents);
  LispLexer *lexer = lisp_lexer_new(provider);
  lisp_lexer_lex(lexer);
  LispParser *parser = lisp_parser_new();
  GArray *tokens = lisp_lexer_get_tokens(lexer);
  lisp_parser_parse(parser, tokens, NULL);

  const Node *ast = lisp_parser_get_ast(parser);
  if (!ast)
    goto cleanup;

  const Node *defsystem = NULL;
  for (guint i = 0; i < ast->children->len; i++) {
    const Node *child = g_array_index(ast->children, Node*, i);
    if (child->type != LISP_AST_NODE_TYPE_LIST || child->children->len < 2)
      continue;
    const Node *head = g_array_index(child->children, Node*, 0);
    if (head->type == LISP_AST_NODE_TYPE_SYMBOL &&
        g_strcmp0(node_get_name(head), "DEFSYSTEM") == 0) {
      defsystem = child;
      break;
    }
  }

  if (!defsystem)
    goto cleanup;

  for (guint i = 2; i + 1 < defsystem->children->len; i += 2) {
    const Node *key = g_array_index(defsystem->children, Node*, i);
    const Node *val = g_array_index(defsystem->children, Node*, i + 1);
    if (key->type != LISP_AST_NODE_TYPE_SYMBOL)
      continue;
    const gchar *kw = node_get_name(key);
    if (g_strcmp0(kw, "DESCRIPTION") == 0) {
      if (val->type == LISP_AST_NODE_TYPE_STRING)
        asdf_set_description(self, node_get_name(val));
    } else if (g_strcmp0(kw, "SERIAL") == 0) {
      if (val->type == LISP_AST_NODE_TYPE_SYMBOL || val->type == LISP_AST_NODE_TYPE_NUMBER) {
        const gchar *v = val->type == LISP_AST_NODE_TYPE_SYMBOL ?
          node_get_name(val) : val->start_token->text;
        self->serial = g_strcmp0(v, "T") == 0 || g_strcmp0(v, "1") == 0;
      }
    } else if (g_strcmp0(kw, "COMPONENTS") == 0) {
      if (val->type == LISP_AST_NODE_TYPE_LIST) {
        for (guint j = 0; j < val->children->len; j++) {
          const Node *comp = g_array_index(val->children, Node*, j);
          if (comp->type != LISP_AST_NODE_TYPE_LIST || comp->children->len < 2)
            continue;
          const Node *sym = g_array_index(comp->children, Node*, 0);
          const Node *fname = g_array_index(comp->children, Node*, 1);
          if (sym->type == LISP_AST_NODE_TYPE_SYMBOL &&
              g_strcmp0(node_get_name(sym), "FILE") == 0) {
            asdf_add_component(self, node_get_name(fname));
          }
        }
      }
    } else if (g_strcmp0(kw, "DEPENDS-ON") == 0) {
      if (val->type == LISP_AST_NODE_TYPE_LIST) {
        for (guint j = 0; j < val->children->len; j++) {
          const Node *dep = g_array_index(val->children, Node*, j);
          asdf_add_dependency(self, node_get_name(dep));
        }
      }
    }
  }

cleanup:
  lisp_parser_free(parser);
  lisp_lexer_free(lexer);
  text_provider_unref(provider);
}

static char *get_system_name(Asdf *self) {
  if (!self->filename)
    return g_strdup("system");
  gchar *base = g_path_get_basename(self->filename);
  gchar *dot = g_strrstr(base, ".");
  if (dot)
    *dot = '\0';
  return base;
}

char *asdf_to_string(Asdf *self) {
  g_return_val_if_fail(GLIDE_IS_ASDF(self), NULL);
  GString *out = g_string_new(NULL);
  gchar *name = get_system_name(self);
  g_string_append_printf(out, "(defsystem \"%s\"\n", name);
  g_free(name);
  if (self->description)
    g_string_append_printf(out, "  :description \"%s\"\n", self->description);
  g_string_append_printf(out, "  :serial %s\n", self->serial ? "t" : "nil");
  if (self->components->len > 0) {
    g_string_append(out, "  :components (");
    for (guint i = 0; i < self->components->len; i++) {
      const gchar *comp = g_ptr_array_index(self->components, i);
      g_string_append_printf(out, "(:file \"%s\")", comp);
      if (i + 1 < self->components->len)
        g_string_append(out, " ");
    }
    g_string_append(out, ")\n");
  }
  if (self->depends_on->len > 0) {
    g_string_append(out, "  :depends-on (");
    for (guint i = 0; i < self->depends_on->len; i++) {
      const gchar *dep = g_ptr_array_index(self->depends_on, i);
      g_string_append_printf(out, "\"%s\"", dep);
      if (i + 1 < self->depends_on->len)
        g_string_append(out, " ");
    }
    g_string_append(out, ")\n");
  }
  g_string_append(out, ")\n");
  return g_string_free(out, FALSE);
}

gboolean asdf_save(Asdf *self, const gchar *filename) {
  g_return_val_if_fail(GLIDE_IS_ASDF(self), FALSE);
  gchar *text = asdf_to_string(self);
  gboolean ok = g_file_set_contents(filename, text, -1, NULL);
  g_free(text);
  return ok;
}

