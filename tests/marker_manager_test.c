#include "marker_manager.h"
#include <glib.h>

static void test_add_and_offsets(void) {
  MarkerManager *manager = marker_manager_new();
  Marker *first = marker_manager_get_marker(manager, 10);
  Marker *second = marker_manager_get_marker(manager, 20);
  Marker *third = marker_manager_get_marker(manager, 5);

  g_assert_cmpuint(marker_get_offset(first), ==, 10);
  g_assert_cmpuint(marker_get_offset(second), ==, 20);
  g_assert_cmpuint(marker_get_offset(third), ==, 5);

  marker_manager_free(manager);
}

static void test_insert_shifts_following_markers(void) {
  MarkerManager *manager = marker_manager_new();
  Marker *before = marker_manager_get_marker(manager, 5);
  Marker *pivot = marker_manager_get_marker(manager, 15);
  Marker *after = marker_manager_get_marker(manager, 25);

  marker_manager_handle_insert(manager, 15, 4);

  g_assert_cmpuint(marker_get_offset(before), ==, 5);
  g_assert_cmpuint(marker_get_offset(pivot), ==, 19);
  g_assert_cmpuint(marker_get_offset(after), ==, 29);

  marker_manager_free(manager);
}

static void test_delete_invalidates_and_shifts(void) {
  MarkerManager *manager = marker_manager_new();
  Marker *before = marker_manager_get_marker(manager, 5);
  Marker *inside_one = marker_manager_get_marker(manager, 12);
  Marker *inside_two = marker_manager_get_marker(manager, 18);
  Marker *after = marker_manager_get_marker(manager, 30);

  marker_manager_handle_delete(manager, 10, 20);

  g_assert_true(marker_is_valid(before));
  g_assert_cmpuint(marker_get_offset(before), ==, 5);
  g_assert_false(marker_is_valid(inside_one));
  g_assert_false(marker_is_valid(inside_two));
  g_assert_true(marker_is_valid(after));
  g_assert_cmpuint(marker_get_offset(after), ==, 20);

  marker_manager_free(manager);
}

static void test_unref_marker_keeps_tree_balanced(void) {
  MarkerManager *manager = marker_manager_new();
  Marker *root = marker_manager_get_marker(manager, 20);
  Marker *left = marker_manager_get_marker(manager, 10);
  Marker *right = marker_manager_get_marker(manager, 30);
  Marker *right_left = marker_manager_get_marker(manager, 25);
  Marker *right_right = marker_manager_get_marker(manager, 40);

  marker_manager_unref_marker(manager, root);

  g_assert_cmpuint(marker_get_offset(left), ==, 10);
  g_assert_cmpuint(marker_get_offset(right), ==, 30);
  g_assert_cmpuint(marker_get_offset(right_left), ==, 25);
  g_assert_cmpuint(marker_get_offset(right_right), ==, 40);

  marker_manager_free(manager);
}

static void test_get_marker_reuses_existing(void) {
  MarkerManager *manager = marker_manager_new();
  Marker *first = marker_manager_get_marker(manager, 10);
  Marker *second = marker_manager_get_marker(manager, 10);

  g_assert_true(first == second);
  marker_manager_unref_marker(manager, first);
  g_assert_cmpuint(marker_get_offset(second), ==, 10);
  marker_manager_unref_marker(manager, second);

  marker_manager_free(manager);
}

int main(int argc, char *argv[]) {
  g_test_init(&argc, &argv, NULL);
  g_test_add_func("/marker_manager/add_and_offsets", test_add_and_offsets);
  g_test_add_func("/marker_manager/insert_shifts", test_insert_shifts_following_markers);
  g_test_add_func("/marker_manager/delete_invalidates", test_delete_invalidates_and_shifts);
  g_test_add_func("/marker_manager/unref_marker", test_unref_marker_keeps_tree_balanced);
  g_test_add_func("/marker_manager/reuse_existing", test_get_marker_reuses_existing);
  return g_test_run();
}
