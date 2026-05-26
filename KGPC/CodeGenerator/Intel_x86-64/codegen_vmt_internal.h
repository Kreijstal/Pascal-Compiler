#ifndef CODEGEN_VMT_INTERNAL_H
#define CODEGEN_VMT_INTERNAL_H

#include "codegen.h"
#include "codegen_string_set.h"

/* EmittedClassSet: tracks which class/GUID labels have been VMT-emitted */
typedef struct EmittedClassSet {
  const char **labels;
  int count;
  int capacity;
} EmittedClassSet;

/* Globals defined in codegen.c, needed by codegen_vmt.c */
extern ListNode_t *g_codegen_available_subprograms;
extern CodeGenStringSet g_available_subprograms_set;

/* Helpers defined in codegen.c, promoted from static for cross-TU use */
HashNode_t *codegen_pick_type_node_by_name(SymTab_t *symtab,
                                           const char *type_name);
struct RecordType *codegen_lookup_record_type_for_node(SymTab_t *symtab,
                                                       HashNode_t *node,
                                                       const char *type_name);
int codegen_parse_guid_literal(const char *guid, uint32_t *d1, uint16_t *d2,
                               uint16_t *d3, uint8_t d4[8]);
int codegen_resolve_record_guid(SymTab_t *symtab,
                                const struct RecordType *record, uint32_t *d1,
                                uint16_t *d2, uint16_t *d3, uint8_t d4[8]);
int codegen_template_matches_methodinfo(const struct MethodTemplate *tmpl,
                                        const struct MethodInfo *method);
struct RecordType *codegen_lookup_record_type_by_name(SymTab_t *symtab,
                                                      const char *type_name,
                                                      int prefer_guid);
int emitted_class_set_contains(const EmittedClassSet *set, const char *label);
int emitted_class_set_add(EmittedClassSet *set, const char *label);
void emitted_class_set_destroy(EmittedClassSet *set);
int codegen_type_decl_suppressed(const Tree_t *decl);
struct RecordType *codegen_record_from_type_decl_ex(Tree_t *decl,
                                                    SymTab_t *symtab);
struct RecordType *codegen_record_from_type_decl(Tree_t *decl);
int record_has_class_vars(const struct RecordType *record);
int record_has_class_method_templates(const struct RecordType *record);
int record_has_method_decls(const struct RecordType *record);
int codegen_class_var_field_size(SymTab_t *symtab,
                                 const struct RecordField *field);
int codegen_record_field_alignment(const struct RecordField *field,
                                   int field_size);
long long codegen_class_var_storage_size(SymTab_t *symtab,
                                         const struct RecordType *record_info,
                                         int include_all_fields);
void codegen_keep_subprogram_label(const char *label);
int codegen_list_contains_string(ListNode_t *list, const char *value);

/* codegen_vmt is the only PUBLIC entry point of codegen_vmt.c.
 * Called from codegen() and codegen_unit() in codegen.c. */
void codegen_vmt(CodeGenContext *ctx, SymTab_t *symtab, Tree_t *tree,
                 CompilationContext *comp_ctx);

#endif /* CODEGEN_VMT_INTERNAL_H */
