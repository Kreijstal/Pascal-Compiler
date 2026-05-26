/*
 * codegen_symbol_resolution.h — Symbol/mangling resolution and label collection
 */
#ifndef CODEGEN_SYMBOL_RESOLUTION_H
#define CODEGEN_SYMBOL_RESOLUTION_H

#include "../../Parser/List/List.h"
#include "../../Parser/ParseTree/tree.h"
#include "../../Parser/SemanticCheck/SymTab/SymTab.h"

/* ---------------------------------------------------------------------------
 * CgCollisionMap: tracks (mangled_id, source_unit_index) pairs across all
 * eligible subprograms to detect cross-unit mangled name collisions.
 * ---------------------------------------------------------------------------*/
#define CG_COLLISION_BUCKETS 256

typedef struct CgCollisionEntry {
  char *mangled_id;
  int first_unit_index;
  int is_colliding;
  struct CgCollisionEntry *next;
} CgCollisionEntry;

typedef struct {
  CgCollisionEntry *buckets[CG_COLLISION_BUCKETS];
} CgCollisionMap;

void cg_collision_destroy(CgCollisionMap *map);

/* ---------------------------------------------------------------------------
 * Symbol / mangling resolution
 * ---------------------------------------------------------------------------*/

int codegen_runtime_owns_exported_symbol(const char *symbol);

void codegen_collect_mangled_collisions(ListNode_t *sub_list,
                                        CgCollisionMap *map);
void codegen_apply_collision_prefixes(ListNode_t *sub_list, SymTab_t *symtab,
                                      const CgCollisionMap *map);

void codegen_collect_available_subprogram_labels(ListNode_t *sub_list);
void codegen_collect_callable_export_names(ListNode_t *sub_list);

int codegen_self_param_is_class(Tree_t *arg_decl, SymTab_t *symtab);

/* Defined in codegen.c — used by codegen_collect_available_subprogram_labels */
int codegen_float_native_distance(Tree_t *sub);

#endif /* CODEGEN_SYMBOL_RESOLUTION_H */
