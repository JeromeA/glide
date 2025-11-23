#include "marker_manager.h"

typedef struct _MarkerNode MarkerNode;

struct _MarkerNode {
  gssize relative_offset; /* relative to parent, or absolute when root */
  gboolean valid;
  guint ref_count;
  MarkerNode *left;
  MarkerNode *right;
  MarkerNode *parent;
  int height;
};

struct _MarkerManager {
  MarkerNode *root;
};

static MarkerNode *marker_node_new(gsize offset);
static void        marker_node_free(MarkerNode *node);
static void        marker_manager_rebalance_upwards(MarkerManager *manager, MarkerNode *node);
static MarkerNode *marker_manager_rotate_left(MarkerManager *manager, MarkerNode *node);
static MarkerNode *marker_manager_rotate_right(MarkerManager *manager, MarkerNode *node);
static gsize       marker_node_absolute_offset(const MarkerNode *node);
static MarkerNode *marker_manager_lower_bound(MarkerManager *manager, gsize offset,
                                              gssize *absolute_out);
static void        marker_manager_mark_invalid(MarkerNode *node, gssize absolute,
                                               gsize start, gsize end);
static void        marker_manager_remove_node(MarkerManager *manager, MarkerNode *node);
static void        marker_manager_replace_node(MarkerManager *manager, MarkerNode *node,
                                               MarkerNode *child);
static MarkerNode *marker_from_handle(Marker *marker);
static int         marker_node_height(MarkerNode *node);
static void        marker_node_update_height(MarkerNode *node);
static int         marker_node_balance(MarkerNode *node);

MarkerManager *marker_manager_new(void) {
  MarkerManager *manager = g_new0(MarkerManager, 1);
  manager->root = NULL;
  return manager;
}

void marker_manager_free(MarkerManager *manager) {
  if (!manager)
    return;
  marker_node_free(manager->root);
  g_free(manager);
}

Marker *marker_manager_get_marker(MarkerManager *manager, gsize offset) {
  g_return_val_if_fail(manager != NULL, NULL);

  if (!manager->root) {
    MarkerNode *node = marker_node_new(offset);
    manager->root = node;
    return (Marker *)node;
  }

  MarkerNode *current = manager->root;
  gssize current_abs = current->relative_offset;
  while (TRUE) {
    gssize node_abs = current_abs;
    if ((gssize)offset == node_abs) {
      current->ref_count++;
      return (Marker *)current;
    }

    if ((gssize)offset < node_abs) {
      if (current->left) {
        current = current->left;
        current_abs = node_abs + current->relative_offset;
        continue;
      }
      MarkerNode *node = marker_node_new(offset);
      current->left = node;
      node->parent = current;
      node->relative_offset = (gssize)offset - node_abs;
      marker_manager_rebalance_upwards(manager, node->parent);
      return (Marker *)node;
    }

    if (current->right) {
      current = current->right;
      current_abs = node_abs + current->relative_offset;
      continue;
    }

    MarkerNode *node = marker_node_new(offset);
    current->right = node;
    node->parent = current;
    node->relative_offset = (gssize)offset - node_abs;
    marker_manager_rebalance_upwards(manager, node->parent);
    return (Marker *)node;
  }
}

void marker_manager_unref_marker(MarkerManager *manager, Marker *marker) {
  g_return_if_fail(manager != NULL);
  g_return_if_fail(marker != NULL);
  MarkerNode *node = marker_from_handle(marker);
  g_return_if_fail(node->ref_count > 0);
  node->ref_count--;
  if (node->ref_count > 0)
    return;

  marker_manager_remove_node(manager, node);
}

gsize marker_get_offset(Marker *marker) {
  g_return_val_if_fail(marker != NULL, 0);
  MarkerNode *node = marker_from_handle(marker);
  return marker_node_absolute_offset(node);
}

gboolean marker_is_valid(Marker *marker) {
  g_return_val_if_fail(marker != NULL, FALSE);
  MarkerNode *node = marker_from_handle(marker);
  return node->valid;
}

void marker_manager_handle_insert(MarkerManager *manager, gsize offset, gsize length) {
  g_return_if_fail(manager != NULL);
  if (length == 0 || !manager->root)
    return;

  gssize node_abs = 0;
  MarkerNode *node = marker_manager_lower_bound(manager, offset, &node_abs);
  if (!node)
    return;

  node->relative_offset += (gssize)length;
  marker_manager_rebalance_upwards(manager, node);
}

void marker_manager_handle_delete(MarkerManager *manager, gsize start, gsize end) {
  g_return_if_fail(manager != NULL);
  g_return_if_fail(start <= end);
  if (!manager->root || start == end)
    return;

  gsize length = end - start;
  gssize absolute = manager->root->relative_offset;
  marker_manager_mark_invalid(manager->root, absolute, start, end);

  MarkerNode *node = marker_manager_lower_bound(manager, end, NULL);
  if (node)
    node->relative_offset -= (gssize)length;

  marker_manager_rebalance_upwards(manager, node ? node : manager->root);
}

static MarkerNode *marker_node_new(gsize offset) {
  MarkerNode *node = g_new0(MarkerNode, 1);
  node->relative_offset = (gssize)offset;
  node->valid = TRUE;
  node->ref_count = 1;
  node->height = 1;
  return node;
}

static void marker_node_free(MarkerNode *node) {
  if (!node)
    return;
  marker_node_free(node->left);
  marker_node_free(node->right);
  g_free(node);
}

static gsize marker_node_absolute_offset(const MarkerNode *node) {
  gssize offset = 0;
  const MarkerNode *current = node;
  while (current) {
    offset += current->relative_offset;
    current = current->parent;
  }
  return (gsize)offset;
}

static MarkerNode *marker_manager_lower_bound(MarkerManager *manager, gsize offset, gssize *absolute_out) {
  MarkerNode *current = manager->root;
  MarkerNode *candidate = NULL;
  gssize candidate_abs = 0;
  if (!current)
    return NULL;

  gssize current_abs = current->relative_offset;
  while (current) {
    gssize node_abs = current_abs;
    if ((gssize)offset <= node_abs) {
      candidate = current;
      candidate_abs = node_abs;
      if (!current->left)
        break;
      current = current->left;
      current_abs = node_abs + current->relative_offset;
    } else {
      if (!current->right)
        break;
      current = current->right;
      current_abs = node_abs + current->relative_offset;
    }
  }

  if (candidate && absolute_out)
    *absolute_out = candidate_abs;
  return candidate;
}

static void marker_manager_mark_invalid(MarkerNode *node, gssize absolute, gsize start, gsize end) {
  if (!node)
    return;

  if (absolute >= (gssize)start && absolute < (gssize)end)
    node->valid = FALSE;

  if (node->left) {
    gssize left_abs = absolute + node->left->relative_offset;
    if (left_abs < (gssize)end)
      marker_manager_mark_invalid(node->left, left_abs, start, end);
  }

  if (node->right) {
    gssize right_abs = absolute + node->right->relative_offset;
    if (right_abs >= (gssize)start)
      marker_manager_mark_invalid(node->right, right_abs, start, end);
  }
}

static void marker_manager_replace_node(MarkerManager *manager, MarkerNode *node, MarkerNode *child) {
  MarkerNode *parent = node->parent;
  if (child) {
    child->parent = parent;
    child->relative_offset += node->relative_offset;
  }

  if (!parent) {
    manager->root = child;
  } else if (parent->left == node) {
    parent->left = child;
  } else {
    parent->right = child;
  }
}

static void marker_manager_remove_node(MarkerManager *manager, MarkerNode *node) {
  if (!node)
    return;

  MarkerNode *rebalance_from = node->parent;
  if (!node->left || !node->right) {
    MarkerNode *child = node->left ? node->left : node->right;
    marker_manager_replace_node(manager, node, child);
    g_free(node);
    marker_manager_rebalance_upwards(manager, rebalance_from);
    return;
  }

  MarkerNode *successor = node->right;
  while (successor->left) {
    successor = successor->left;
  }

  MarkerNode *successor_parent = successor->parent;
  MarkerNode *successor_child = successor->right;
  marker_manager_replace_node(manager, successor, successor_child);

  successor->parent = node->parent;
  successor->left = node->left;
  successor->right = node->right;
  successor->relative_offset = node->relative_offset;

  if (successor->left)
    successor->left->parent = successor;
  if (successor->right)
    successor->right->parent = successor;

  if (!successor->parent) {
    manager->root = successor;
  } else if (successor->parent->left == node) {
    successor->parent->left = successor;
  } else {
    successor->parent->right = successor;
  }

  gssize successor_new_abs =
      successor->parent ? (gssize)marker_node_absolute_offset(successor->parent) + successor->relative_offset
                        : successor->relative_offset;
  if (successor->left) {
    gssize child_abs = marker_node_absolute_offset(successor->left);
    successor->left->relative_offset = child_abs - successor_new_abs;
  }
  if (successor->right) {
    gssize child_abs = marker_node_absolute_offset(successor->right);
    successor->right->relative_offset = child_abs - successor_new_abs;
  }

  rebalance_from = successor_parent && successor_parent != node ? successor_parent : successor;

  g_free(node);
  marker_manager_rebalance_upwards(manager, rebalance_from);
}

static MarkerNode *marker_manager_rotate_left(MarkerManager *manager, MarkerNode *node) {
  MarkerNode *pivot = node->right;
  g_return_val_if_fail(pivot != NULL, node);
  gssize pivot_rel = pivot->relative_offset;

  node->right = pivot->left;
  if (pivot->left) {
    pivot->left->parent = node;
    pivot->left->relative_offset += pivot_rel;
  }

  pivot->parent = node->parent;
  pivot->relative_offset += node->relative_offset;

  node->parent = pivot;
  node->relative_offset = -pivot_rel;

  pivot->left = node;

  if (!pivot->parent) {
    manager->root = pivot;
  } else if (pivot->parent->left == node) {
    pivot->parent->left = pivot;
  } else {
    pivot->parent->right = pivot;
  }

  marker_node_update_height(node);
  marker_node_update_height(pivot);
  return pivot;
}

static MarkerNode *marker_manager_rotate_right(MarkerManager *manager, MarkerNode *node) {
  MarkerNode *pivot = node->left;
  g_return_val_if_fail(pivot != NULL, node);
  gssize pivot_rel = pivot->relative_offset;

  node->left = pivot->right;
  if (pivot->right) {
    pivot->right->parent = node;
    pivot->right->relative_offset += pivot_rel;
  }

  pivot->parent = node->parent;
  pivot->relative_offset += node->relative_offset;

  node->parent = pivot;
  node->relative_offset = -pivot_rel;

  pivot->right = node;

  if (!pivot->parent) {
    manager->root = pivot;
  } else if (pivot->parent->left == node) {
    pivot->parent->left = pivot;
  } else {
    pivot->parent->right = pivot;
  }

  marker_node_update_height(node);
  marker_node_update_height(pivot);
  return pivot;
}

static int marker_node_height(MarkerNode *node) {
  return node ? node->height : 0;
}

static void marker_node_update_height(MarkerNode *node) {
  if (!node)
    return;
  int left_h = marker_node_height(node->left);
  int right_h = marker_node_height(node->right);
  node->height = 1 + (left_h > right_h ? left_h : right_h);
}

static int marker_node_balance(MarkerNode *node) {
  return node ? marker_node_height(node->left) - marker_node_height(node->right) : 0;
}

static void marker_manager_rebalance_upwards(MarkerManager *manager, MarkerNode *node) {
  MarkerNode *current = node;
  while (current) {
    marker_node_update_height(current);
    int balance = marker_node_balance(current);
    if (balance > 1) {
      if (marker_node_balance(current->left) < 0)
        marker_manager_rotate_left(manager, current->left);
      current = marker_manager_rotate_right(manager, current);
    } else if (balance < -1) {
      if (marker_node_balance(current->right) > 0)
        marker_manager_rotate_right(manager, current->right);
      current = marker_manager_rotate_left(manager, current);
    }
    current = current->parent;
  }
}

static MarkerNode *marker_from_handle(Marker *marker) {
  return (MarkerNode *)marker;
}

