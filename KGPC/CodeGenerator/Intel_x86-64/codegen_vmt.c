/*
    VMT (Virtual Method Table) and class-variable emission for x86-64.
    Extracted from codegen.c — see codegen_vmt_internal.h for shared helpers.
*/

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <assert.h>
#include <string.h>
#include <limits.h>
#include <ctype.h>
#include "register_types.h"
#include "codegen.h"
#include "codegen_string_set.h"
#include "codegen_symbol_resolution.h"
#include "codegen_statement.h"
#include "stackmng/stackmng.h"
#include "expr_tree/expr_tree.h"
#include "codegen_expression.h"
#include "../../flags.h"
#include "../../Parser/List/List.h"
#include "../../Parser/ParseTree/tree.h"
#include "../../Parser/ParseTree/tree_types.h"
#include "../../Parser/ParseTree/type_tags.h"
#include "../../Parser/ParseTree/KgpcType.h"
#include "../../Parser/ParseTree/from_cparser.h"
#include "../../Parser/SemanticCheck/HashTable/HashTable.h"
#include "../../Parser/SemanticCheck/NameMangling.h"
#include "../../Parser/SemanticCheck/SemChecks/SemCheck_expr.h"
#include "../../Parser/SemanticCheck/SemChecks/SemCheck_sizeof.h"
#include "../../Parser/SemanticCheck/SemCheck.h"

#include "../../identifier_utils.h"
#include "../../unit_registry.h"
#include "ir/ir_inst.h"
#include "ir/ir_cfg.h"
#include "ir/ir_liveness.h"
#if USE_GRAPH_COLORING_ALLOCATOR
#include "graph_coloring_allocator.h"
#endif

#include "codegen_subprograms_internal.h"

#include "codegen_vmt_internal.h"

static void codegen_collect_inferred_interfaces(SymTab_t *symtab,
    const struct RecordType *record, const char *class_label,
    const char ***out_names, int *out_count);
static const struct RecordType *codegen_record_parent(
    const struct RecordType *record, SymTab_t *symtab);
static void codegen_emit_global_jump_stub(CodeGenContext *ctx,
    const char *exported_symbol, const char *target_symbol);
static void codegen_assert_interface_impl_resolved(const char *iface_name,
    const char *method_name, const char *class_label,
    const char *iface_symbol, const char *impl_symbol);

static inline struct RecordType* get_record_type_from_node(HashNode_t *node)
{
    return hashnode_get_record_type(node);
}

static int codegen_method_uses_sret(CodeGenContext *ctx, SymTab_t *symtab,
    const char *owner_name, const char *fallback_owner, const char *method_name)
{
    char lookup_name[512];
    snprintf(lookup_name, sizeof(lookup_name), "%s__%s", owner_name, method_name);
    HashNode_t *method_sym = NULL;
    FindSymbol(&method_sym, symtab, lookup_name);
    if (method_sym == NULL && fallback_owner != NULL) {
        snprintf(lookup_name, sizeof(lookup_name), "%s__%s", fallback_owner, method_name);
        FindSymbol(&method_sym, symtab, lookup_name);
    }
    if (method_sym == NULL || method_sym->type == NULL)
        return 0;
    KgpcType *ret_type = kgpc_type_get_return_type(method_sym->type);
    if (ret_type == NULL)
        return 0;
    if (kgpc_type_is_shortstring(ret_type))
        return 1;
    if (kgpc_type_is_record(ret_type)) {
        long long ret_size = 0;
        struct RecordType *ret_rec = kgpc_type_get_record(ret_type);
        if (ret_rec != NULL &&
            codegen_sizeof_type_reference(ctx, RECORD_TYPE, NULL,
                ret_rec, &ret_size) == 0 && ret_size > 8)
            return 1;
    }
    return 0;
}

/* Emit a GUID constant and pguid reference pointer for an interface.
 * Handles dedup checking/adding via the emitted_classes set, section
 * selection (Windows COFF vs ELF), and emits both the 16-byte GUID
 * data constant and the __kgpc_guidref_ pointer.
 * Returns 1 if emitted, 0 if skipped (already emitted or error). */
static int codegen_emit_interface_guid(CodeGenContext *ctx,
    const char *iface_name, uint32_t d1, uint16_t d2, uint16_t d3,
    const uint8_t d4[8], const char *comment_suffix,
    EmittedClassSet *emitted_classes)
{
    if (ctx == NULL || iface_name == NULL || emitted_classes == NULL)
        return 0;

    char guid_dedup_buf[512];
    snprintf(guid_dedup_buf, sizeof(guid_dedup_buf),
             "__kgpc_guid_%s", iface_name);
    if (emitted_class_set_contains(emitted_classes, guid_dedup_buf))
        return 0;

    if (emitted_class_set_add(emitted_classes, guid_dedup_buf) != 0)
        return 0;

    int is_win = codegen_target_is_windows();
    fprintf(ctx->output_file, "\n# GUID constant for interface %s%s\n",
            iface_name, comment_suffix != NULL ? comment_suffix : "");
    if (is_win) {
        fprintf(ctx->output_file,
                "\t.section\t.rdata$__kgpc_guid_%s,\"dr\"\n", iface_name);
        fprintf(ctx->output_file, "\t.linkonce discard\n");
    } else {
        fprintf(ctx->output_file, "\t.data\n");
    }
    fprintf(ctx->output_file, "\t.align 8\n");
    fprintf(ctx->output_file, ".globl __kgpc_guid_%s\n", iface_name);
    fprintf(ctx->output_file, "__kgpc_guid_%s:\n", iface_name);
    fprintf(ctx->output_file, "\t.long\t0x%08X\n", d1);
    fprintf(ctx->output_file, "\t.short\t0x%04X\n", (unsigned)d2);
    fprintf(ctx->output_file, "\t.short\t0x%04X\n", (unsigned)d3);
    fprintf(ctx->output_file,
            "\t.byte\t0x%02X, 0x%02X, 0x%02X, 0x%02X, "
            "0x%02X, 0x%02X, 0x%02X, 0x%02X\n",
            d4[0], d4[1], d4[2], d4[3], d4[4], d4[5], d4[6], d4[7]);
    /* Emit pguid pointer */
    if (is_win) {
        fprintf(ctx->output_file,
                "\t.section\t.rdata$__kgpc_guidref_%s,\"dr\"\n", iface_name);
        fprintf(ctx->output_file, "\t.linkonce discard\n");
    }
    fprintf(ctx->output_file, "\t.align 8\n");
    fprintf(ctx->output_file, ".globl __kgpc_guidref_%s\n", iface_name);
    fprintf(ctx->output_file, "__kgpc_guidref_%s:\n", iface_name);
    fprintf(ctx->output_file, "\t.quad\t__kgpc_guid_%s\n", iface_name);
    fprintf(ctx->output_file, "%s\n", codegen_readonly_section_directive());
    return 1;
}

static int codegen_iface_name_list_contains(const char **iface_names,
    int iface_count, const char *iface_name)
{
    if (iface_names == NULL || iface_name == NULL || iface_count <= 0)
        return 0;
    for (int i = 0; i < iface_count; i++) {
        if (iface_names[i] != NULL &&
            pascal_identifier_equals(iface_names[i], iface_name))
            return 1;
    }
    return 0;
}

static int codegen_class_overrides_inherited_interface_method(SymTab_t *symtab,
    const struct RecordType *record, const char *class_label,
    const struct RecordType *parent_record, const char *iface_name)
{
    if (symtab == NULL || record == NULL || parent_record == NULL || iface_name == NULL)
        return 0;

    struct RecordType *iface_record =
        codegen_lookup_record_type_by_name(symtab, iface_name, 0);
    if (iface_record == NULL || iface_record->method_templates == NULL)
        return 0;

    const char *parent_label = parent_record->type_id;
    if (parent_label == NULL)
        parent_label = record->parent_class_name;

    for (ListNode_t *method_cur = iface_record->method_templates;
         method_cur != NULL;
         method_cur = method_cur->next) {
        struct MethodTemplate *method_tmpl =
            (struct MethodTemplate *)method_cur->cur;
        if (method_tmpl == NULL || method_tmpl->name == NULL)
            continue;

        const char *child_impl = codegen_find_class_method_impl_id(
            symtab, record, class_label, iface_name, method_tmpl->name);
        const char *parent_impl = codegen_find_class_method_impl_id(
            symtab, parent_record, parent_label, iface_name, method_tmpl->name);
        if (child_impl == NULL && parent_impl == NULL)
            continue;
        if (child_impl == NULL || parent_impl == NULL ||
            strcmp(child_impl, parent_impl) != 0)
            return 1;
    }

    return 0;
}

static void codegen_emit_class_vmt(CodeGenContext *ctx, SymTab_t *symtab,
    struct RecordType *record_info, const char *class_label,
    EmittedClassSet *emitted_classes)
{
    if (record_info == NULL || !record_type_is_class(record_info) || class_label == NULL)
        return;

    if (emitted_class_set_contains(emitted_classes, class_label))
        return;

    if (emitted_class_set_add(emitted_classes, class_label) != 0)
        return;

    /* Emit FPC-compatible interface table (tinterfacetable) if this class
     * implements interfaces.  Layout per FPC objpash.inc:
     *   tinterfacetable: EntryCount (sizeuint=8), then entries (40 bytes each)
     *   tinterfaceentry: IIDRef(^pguid,8) VTable(8) IOffset(8)|IOffsetAsCodePtr(8)+IIDStrRef(8)+IType(4)+pad(4)
     * For each interface, emit standalone GUID constant + pointer indirection:
     *   __kgpc_guid_<Name>     = 16-byte GUID data
     *   __kgpc_guidref_<Name>  = pointer to __kgpc_guid_<Name>  (pguid)
     * The entry's IIDRef field points to __kgpc_guidref_<Name>. */
    int actual_iface_count = 0;
    const char **effective_iface_names = (const char **)record_info->interface_names;
    int effective_iface_count = record_info->num_interfaces;
    int free_effective_iface_names = 0;
    long long base_instance_size = 0;

    int effective_iface_append_failed = 0;
    const struct RecordType *parent_record = codegen_record_parent(record_info, symtab);
    for (const struct RecordType *iface_parent = parent_record;
         iface_parent != NULL;
         iface_parent = codegen_record_parent(iface_parent, symtab)) {
        if (effective_iface_append_failed)
            break;
        if (iface_parent->num_interfaces <= 0 || iface_parent->interface_names == NULL)
            continue;
        for (int iidx = 0; iidx < iface_parent->num_interfaces; iidx++) {
            const char *iface_name = iface_parent->interface_names[iidx];
            if (iface_name == NULL ||
                codegen_iface_name_list_contains(effective_iface_names,
                    effective_iface_count, iface_name))
                continue;
            if (!codegen_class_overrides_inherited_interface_method(symtab,
                    record_info, class_label, parent_record, iface_name))
                continue;

            if (!free_effective_iface_names) {
                const char **copied = NULL;
                if (effective_iface_count > 0) {
                    copied = (const char **)malloc(sizeof(char *) * effective_iface_count);
                    if (copied == NULL) {
                        effective_iface_append_failed = 1;
                        break;
                    }
                    memcpy((void *)copied, effective_iface_names,
                        sizeof(char *) * effective_iface_count);
                }
                effective_iface_names = copied;
                free_effective_iface_names = 1;
            }

            const char **grown = (const char **)realloc((void *)effective_iface_names,
                sizeof(char *) * (effective_iface_count + 1));
            if (grown == NULL) {
                effective_iface_append_failed = 1;
                break;
            }
            effective_iface_names = grown;
            effective_iface_names[effective_iface_count] = iface_name;
            effective_iface_count++;
        }
    }

    if (effective_iface_count > 0) {
        /* First pass: emit standalone GUID constants for each interface
         * (deduplicated via emitted_classes set with "__kgpc_guid_" prefix). */
        for (int iidx = 0; iidx < effective_iface_count; iidx++) {
            const char *iface_name = effective_iface_names[iidx];
            if (iface_name == NULL) continue;
            /* Look up the interface type to get its GUID.
             * Use FindAllIdents to handle forward-declared interfaces where
             * the forward decl (without GUID) and full decl (with GUID) are
             * separate symbol table entries.  Prefer the one with has_guid. */
            struct RecordType *iface_record =
                codegen_lookup_record_type_by_name(symtab, iface_name, 1);
            uint32_t d1 = 0;
            uint16_t d2 = 0, d3 = 0;
            unsigned char d4[8] = {0};
            if (iface_record != NULL)
                codegen_resolve_record_guid(symtab, iface_record, &d1, &d2, &d3, d4);
            codegen_emit_interface_guid(ctx, iface_name, d1, d2, d3, d4,
                NULL, emitted_classes);
        }

        /* Count valid interfaces */
        for (int iidx = 0; iidx < effective_iface_count; iidx++) {
            if (effective_iface_names[iidx] != NULL)
                actual_iface_count++;
        }

        /* Compute base instance size early — needed for IOffset values in the
         * interface table and for thunk adjustment.  codegen_sizeof_record_type
         * only counts fields in this record's field list; for classes with
         * parent classes whose fields were NOT merged into this record (common
         * when the record comes from a cached unit), we must add the parent
         * class size explicitly. */
        codegen_sizeof_record_type(ctx, record_info, &base_instance_size);
        if (record_info->parent_class_name != NULL) {
            /* Check if parent fields are included in the field list by
             * comparing against the parent's own size.  If the parent has
             * a larger base size than our field-only size, the parent
             * fields aren't merged; use parent_size + own_members instead. */
            HashNode_t *parent_cls_node = NULL;
            struct RecordType *parent_cls_rec = NULL;
            if (FindSymbol(&parent_cls_node, symtab, record_info->parent_class_name) != 0 &&
                parent_cls_node != NULL) {
                parent_cls_rec = get_record_type_from_node(parent_cls_node);
                if (parent_cls_rec == NULL && parent_cls_node->type != NULL &&
                    parent_cls_node->type->kind == TYPE_KIND_POINTER &&
                    parent_cls_node->type->info.points_to != NULL &&
                    parent_cls_node->type->info.points_to->kind == TYPE_KIND_RECORD)
                    parent_cls_rec = parent_cls_node->type->info.points_to->info.record_info;
            }
            if (parent_cls_rec != NULL) {
                long long parent_base = 0;
                codegen_sizeof_record_type(ctx, parent_cls_rec, &parent_base);
                if (parent_base > 8) {
                    /* Add parent's fields to the child's base size.
                     * own_members = base_instance_size - 8 (VMT already
                     * counted in parent_base).  Total = parent_base +
                     * own_members, but only if the result is larger than
                     * the current base (to avoid shrinking when parent
                     * fields WERE already merged). */
                    long long own_members = base_instance_size - 8;
                    if (own_members < 0) own_members = 0;
                    long long own_start = parent_base;
                    if (own_start % 8 != 0)
                        own_start = (own_start + 7) & ~7LL;
                    long long new_base = own_start + own_members;
                    if (new_base > base_instance_size)
                        base_instance_size = new_base;
                }
            }
        }

        /* Emit FPC-compatible tinterfacetable */
        fprintf(ctx->output_file, "\n# Interface table (tinterfacetable) for class %s\n", class_label);
        fprintf(ctx->output_file, "\t.data\n");
        fprintf(ctx->output_file, "\t.align 8\n");
        fprintf(ctx->output_file, "%s_INTFTABLE:\n", class_label);
        fprintf(ctx->output_file, "\t.quad\t%d\t# EntryCount\n", actual_iface_count);
        int iface_slot_idx = 0;
        for (int iidx = 0; iidx < effective_iface_count; iidx++) {
            const char *iface_name = effective_iface_names[iidx];
            if (iface_name == NULL) continue;
            long long ioffset = base_instance_size + iface_slot_idx * 8;
            fprintf(ctx->output_file, "\t# Entry for %s (40 bytes = tinterfaceentry)\n", iface_name);
            /* offset +0: IIDRef (^pguid) — pointer to the pguid indirection cell */
            fprintf(ctx->output_file, "\t.quad\t__kgpc_guidref_%s\n", iface_name);
            /* offset +8: VTable — pointer to interface vtable for this class */
            fprintf(ctx->output_file, "\t.quad\t%s_INTF_%s_VTABLE\n", class_label, iface_name);
            /* offset +16: IOffset (sizeuint) — byte offset from object start to interface slot */
            fprintf(ctx->output_file, "\t.quad\t%lld\n", ioffset);
            /* offset +24: IIDStrRef (^pshortstring) — NULL for now */
            fprintf(ctx->output_file, "\t.quad\t0\n");
            /* offset +32: IType (tinterfaceentrytype enum, 4 bytes) = etStandard = 0 */
            fprintf(ctx->output_file, "\t.long\t0\n");
            /* offset +36: padding to 40 bytes */
            fprintf(ctx->output_file, "\t.zero\t4\n");
            iface_slot_idx++;
        }
        fprintf(ctx->output_file, "%s\n", codegen_readonly_section_directive());

        /* Emit interface vtable arrays — one per interface this class implements.
         * Each vtable contains a .quad entry per method_template in the interface,
         * in declaration order, pointing to a thunk that adjusts Self back from
         * the interface pointer to the raw object pointer before jumping to the
         * implementing class method. */
        int vtbl_iface_slot_idx = 0;
        for (int iidx = 0; iidx < effective_iface_count; iidx++) {
            const char *iface_name = effective_iface_names[iidx];
            if (iface_name == NULL) continue;
            long long ioffset_for_this_iface = base_instance_size + vtbl_iface_slot_idx * 8;
            vtbl_iface_slot_idx++;
            /* Look up the interface record to get its method_templates */
            struct RecordType *vtbl_iface_record =
                codegen_lookup_record_type_by_name(symtab, iface_name, 0);
            if (vtbl_iface_record == NULL) {
                /* No interface record — emit an empty vtable label */
                fprintf(ctx->output_file, "\n# Interface vtable for %s implementing %s (empty)\n", class_label, iface_name);
                fprintf(ctx->output_file, "\t.data\n");
                fprintf(ctx->output_file, "\t.align 8\n");
                fprintf(ctx->output_file, ".globl\t%s_INTF_%s_VTABLE\n", class_label, iface_name);
                fprintf(ctx->output_file, "%s_INTF_%s_VTABLE:\n", class_label, iface_name);
                fprintf(ctx->output_file, "%s\n", codegen_readonly_section_directive());
                continue;
            }
            fprintf(ctx->output_file, "\n# Interface vtable for %s implementing %s\n", class_label, iface_name);
            fprintf(ctx->output_file, "\t.data\n");
            fprintf(ctx->output_file, "\t.align 8\n");
            fprintf(ctx->output_file, ".globl\t%s_INTF_%s_VTABLE\n", class_label, iface_name);
            fprintf(ctx->output_file, "%s_INTF_%s_VTABLE:\n", class_label, iface_name);
            /* Iterate interface method_templates directly — inherited parent
             * methods were already prepended during semcheck. */
            ListNode_t *vtbl_iface_method = vtbl_iface_record->method_templates;
            while (vtbl_iface_method != NULL) {
                struct MethodTemplate *vtbl_imethod = (struct MethodTemplate *)vtbl_iface_method->cur;
                if (vtbl_imethod != NULL && vtbl_imethod->name != NULL) {
                    const char *vtbl_resolved_id = codegen_find_class_method_impl_id(
                        symtab, record_info, class_label, iface_name, vtbl_imethod->name);
                    if (vtbl_resolved_id != NULL) {
                        /* Emit thunk in .text that adjusts Self back from interface
                         * pointer to raw object pointer, then jumps to the real method.
                         * The thunk handles both adjusted interface pointers (Self =
                         * object + ioffset) and raw object pointers (Self = object)
                         * by checking if *(Self) is a VMT (vInstanceSize + vInstanceSize2
                         * == 0) or an interface vtable. */
                        char thunk_label[768];
                        snprintf(thunk_label, sizeof(thunk_label), "%s_INTF_%s_THUNK_%s",
                            class_label, iface_name, vtbl_imethod->name);
                        /* Thunks must always be in an executable .text section.
                         * codegen_text_section_resume() uses .previous when
                         * --function-sections is active, but .previous may
                         * return to .rodata (non-executable) here. */
                        fprintf(ctx->output_file, "\t.text\n");
                        fprintf(ctx->output_file, "%s:\n", thunk_label);
                        /* If the method returns a large type (SRET), Self
                         * shifts from the first to the second arg register. */
                        int method_uses_sret = codegen_method_uses_sret(
                            ctx, symtab, iface_name, class_label, vtbl_imethod->name);
                        const char *self_reg = codegen_target_is_windows()
                            ? (method_uses_sret ? "%rdx" : "%rcx")
                            : (method_uses_sret ? "%rsi" : "%rdi");
                        /* Check if Self points to a VMT (raw object pointer) or
                         * an interface vtable (adjusted pointer).
                         * VMT has vInstanceSize at [0] and -vInstanceSize at [8],
                         * so their sum is 0.  Interface vtable entries are code
                         * pointers whose sum is extremely unlikely to be 0. */
                        fprintf(ctx->output_file, "\tmovq\t(%s), %%r11\n", self_reg);
                        fprintf(ctx->output_file, "\tmovq\t(%%r11), %%rax\n");
                        fprintf(ctx->output_file, "\taddq\t8(%%r11), %%rax\n");
                        fprintf(ctx->output_file, "\tjnz\t.L%s_adj\n", thunk_label);
                        /* Raw object pointer — no adjustment needed */
                        fprintf(ctx->output_file, "\tjmp\t%s\n", vtbl_resolved_id);
                        fprintf(ctx->output_file, ".L%s_adj:\n", thunk_label);
                        /* Adjusted interface pointer — subtract ioffset */
                        fprintf(ctx->output_file, "\tsubq\t$%lld, %s\n",
                            ioffset_for_this_iface, self_reg);
                        fprintf(ctx->output_file, "\tjmp\t%s\n", vtbl_resolved_id);
                        /* Switch back to data for the vtable entry */
                        fprintf(ctx->output_file, "\t.data\n");
                        fprintf(ctx->output_file, "\t.quad\t%s\t# %s\n", thunk_label, vtbl_imethod->name);
                    } else {
                        fprintf(ctx->output_file, "\t.quad\t__kgpc_abstract_method_error\t# %s (not implemented)\n", vtbl_imethod->name);
                    }
                }
                vtbl_iface_method = vtbl_iface_method->next;
            }
            fprintf(ctx->output_file, "%s\n", codegen_readonly_section_directive());
        }
    }

    /* Resolve the canonical label for the parent class (handles case mismatches
     * e.g. math.pp declares "EInvalidArgument = class(ematherror)" with lowercase,
     * but EMathError's VMT is emitted with its declaration-case label "EMathError"). */
    const char *parent_vmt_label = record_info->parent_class_name;
    if (parent_vmt_label != NULL) {
        HashNode_t *parent_node = NULL;
        if (FindSymbol(&parent_node, symtab, parent_vmt_label) != 0 && parent_node != NULL) {
            struct RecordType *parent_rec = get_record_type_from_node(parent_node);
            /* Only use the resolved type_id if it's actually a class. If FindIdent
             * resolved to a plain record (e.g. TTimeZone = timezone record alias
             * instead of TTimeZone = class abstract), keep the original name which
             * matches the class VMT label. */
            if (parent_rec != NULL && parent_rec->is_class && parent_rec->type_id != NULL)
                parent_vmt_label = parent_rec->type_id;
            else if (parent_rec != NULL && !parent_rec->is_class) {
                /* Resolved to a non-class record; keep original parent_class_name */
            } else if (parent_node->id != NULL)
                parent_vmt_label = parent_node->id;
        }
    }

    fprintf(ctx->output_file, "\n# RTTI for class %s\n", class_label);
    fprintf(ctx->output_file, "\t.align 8\n");
    fprintf(ctx->output_file, ".globl %s_TYPEINFO\n", class_label);
    fprintf(ctx->output_file, "%s_TYPEINFO:\n", class_label);
    if (record_info->parent_class_name != NULL)
        fprintf(ctx->output_file, "\t.quad\t%s_TYPEINFO\n", parent_vmt_label);
    else
        fprintf(ctx->output_file, "\t.quad\t0\n");

    char name_label[256];
    snprintf(name_label, sizeof(name_label), "__kgpc_typeinfo_name_%s", class_label);
    fprintf(ctx->output_file, "\t.quad\t%s\n", name_label);
    /* Always emit VMT reference, even if no methods */
    fprintf(ctx->output_file, "\t.quad\t%s_VMT\n", class_label);
    /* Interface table pointer and count */
    if (actual_iface_count > 0)
        fprintf(ctx->output_file, "\t.quad\t%s_INTFTABLE\n", class_label);
    else
        fprintf(ctx->output_file, "\t.quad\t0\n");
    fprintf(ctx->output_file, "\t.quad\t%d\n", actual_iface_count);
    {
        char escaped_label[CODEGEN_MAX_INST_BUF];
        escape_string(escaped_label, class_label, sizeof(escaped_label));
        fprintf(ctx->output_file, "%s:\n\t.string \"%s\"\n", name_label, escaped_label);
    }

    /* Emit class name as ShortString data for vClassName.
     * The generated TObject.ClassName body loads the slot once and then treats
     * the resulting address as a PShortString, so the VMT must point directly to
     * the ShortString payload rather than an intermediate pointer cell. */
    {
        char classname_ss_label[256];
        char escaped_classname[256];
        snprintf(classname_ss_label, sizeof(classname_ss_label),
            "__kgpc_vmt_classname_%s", class_label);
        escape_string(escaped_classname, class_label, sizeof(escaped_classname));
        fprintf(ctx->output_file, "%s:\n", classname_ss_label);
        fprintf(ctx->output_file, "\t.byte\t%d\n", (int)strlen(class_label));
        fprintf(ctx->output_file, "\t.ascii\t\"%s\"\n", escaped_classname);
    }

    /* Emit parent VMT reference storage for vParentRef (PPVmt) */
    if (record_info->parent_class_name != NULL) {
        fprintf(ctx->output_file, "\t.align 8\n");
        fprintf(ctx->output_file, "__kgpc_vmt_parentref_%s:\n", class_label);
        fprintf(ctx->output_file, "\t.quad\t%s_VMT\n", parent_vmt_label);
    }

    /* Compute instance size for vInstanceSize.
     * base_instance_size was computed earlier (before interface table emission)
     * when the class has interfaces.  For classes without interfaces, compute now. */
    if (base_instance_size == 0)
        codegen_sizeof_record_type(ctx, record_info, &base_instance_size);
    long long instance_size = base_instance_size;
    if (actual_iface_count > 0) {
        instance_size += actual_iface_count * 8;
        /* Update cached_size so constructor allocations use the new size */
        record_info->cached_size = instance_size;
        record_info->has_cached_size = 1;
    }
    /* Always emit VMT for classes, even if no virtual methods.
     * FPC VMT layout (TVmt record from objpash.inc):
     *   offset 0:  vInstanceSize      (SizeInt)
     *   offset 8:  vInstanceSize2     (SizeInt = -InstanceSize)
     *   offset 16: vParentRef         (PPVmt)
     *   offset 24: vClassName         (PShortString)
     *   offset 32: vDynamicTable      (Pointer)
     *   offset 40: vMethodTable       (Pointer)
     *   offset 48: vFieldTable        (Pointer)
     *   offset 56: vTypeInfo          (Pointer)
     *   offset 64: vInitTable         (Pointer)
     *   offset 72: vAutoTable         (Pointer)
     *   offset 80: vIntfTable         (PInterfaceTable)
     *   offset 88: vMsgStrPtr         (Pointer)
     *   offset 96+: virtual methods   (vmt_index 12+)
     */
    fprintf(ctx->output_file, "\n# VMT for class %s\n", class_label);
    fprintf(ctx->output_file, "\t.align 8\n");
    fprintf(ctx->output_file, ".globl %s_VMT\n", class_label);
    fprintf(ctx->output_file, "%s_VMT:\n", class_label);
    /* Slot 0: vInstanceSize */
    fprintf(ctx->output_file, "\t.quad\t%lld\n", instance_size);
    /* Slot 1: vInstanceSize2 = -InstanceSize */
    fprintf(ctx->output_file, "\t.quad\t%lld\n", -instance_size);
    /* Slot 2: vParentRef (PPVmt - pointer to location storing parent VMT pointer) */
    if (record_info->parent_class_name != NULL)
        fprintf(ctx->output_file, "\t.quad\t__kgpc_vmt_parentref_%s\n", class_label);
    else
        fprintf(ctx->output_file, "\t.quad\t0\n");
    /* Slot 3: vClassName (PShortString) */
    fprintf(ctx->output_file, "\t.quad\t__kgpc_vmt_classname_%s\n", class_label);
    /* Slot 4: vDynamicTable */
    fprintf(ctx->output_file, "\t.quad\t0\n");
    /* Slot 5: vMethodTable */
    fprintf(ctx->output_file, "\t.quad\t0\n");
    /* Slot 6: vFieldTable */
    fprintf(ctx->output_file, "\t.quad\t0\n");
    /* Slot 7: vTypeInfo - point to our RTTI */
    fprintf(ctx->output_file, "\t.quad\t%s_TYPEINFO\n", class_label);
    /* Slot 8: vInitTable */
    fprintf(ctx->output_file, "\t.quad\t0\n");
    /* Slot 9: vAutoTable */
    fprintf(ctx->output_file, "\t.quad\t0\n");
    /* Slot 10: vIntfTable (PInterfaceTable — FPC tinterfacetable) */
    if (actual_iface_count > 0)
        fprintf(ctx->output_file, "\t.quad\t%s_INTFTABLE\n", class_label);
    else
        fprintf(ctx->output_file, "\t.quad\t0\n");
    /* Slot 11: vMsgStrPtr */
    fprintf(ctx->output_file, "\t.quad\t0\n");

    /* Generic specializations can carry a cloned VMT from semcheck while the
     * actual specialized methods are only visible here by their emitted
     * symbols. Refresh matching virtual slots by method name before emitting
     * the final table so inherited slots point at the specialized overrides. */
    if (record_info->method_templates != NULL && record_info->methods != NULL &&
        class_label != NULL) {
        for (ListNode_t *tmpl_node = record_info->method_templates;
             tmpl_node != NULL; tmpl_node = tmpl_node->next) {
            if (tmpl_node->type != LIST_METHOD_TEMPLATE || tmpl_node->cur == NULL)
                continue;
            struct MethodTemplate *tmpl = (struct MethodTemplate *)tmpl_node->cur;
            if (tmpl->name == NULL || (!tmpl->is_virtual && !tmpl->is_override))
                continue;

            size_t base_len = strlen(class_label) + 2 + strlen(tmpl->name) + 1;
            char *base_name = (char *)malloc(base_len);
            if (base_name == NULL)
                continue;
            snprintf(base_name, base_len, "%s__%s", class_label, tmpl->name);

            const char *resolved_id = NULL;
            int wanted_params = from_cparser_count_params_ast(tmpl->params_ast);
            ListNode_t *matches = FindAllIdents(symtab, base_name);
            int matching_defined_candidate_count = 0;
            for (ListNode_t *m = matches; m != NULL; m = m->next) {
                HashNode_t *cand = (HashNode_t *)m->cur;
                if (cand == NULL || cand->type == NULL ||
                    cand->type->kind != TYPE_KIND_PROCEDURE ||
                    cand->type->info.proc_info.definition == NULL)
                    continue;
                int count = ListLength(cand->type->info.proc_info.params);
                if (!tmpl->is_static && count > 0)
                    count -= 1;
                if (count != wanted_params)
                    continue;
                matching_defined_candidate_count++;
                resolved_id = cand->mangled_id;
                if (cand->type->info.proc_info.definition->tree_data.subprogram_data.mangled_id != NULL)
                    resolved_id = cand->type->info.proc_info.definition->tree_data.subprogram_data.mangled_id;
                break;
            }
            if (matches != NULL)
                DestroyList(matches);
            free(base_name);

            KGPC_COMPILER_HARD_ASSERT(resolved_id != NULL || matching_defined_candidate_count == 0,
                "generic VMT specialization '%s.%s' had a same-signature implementation but no resolved id",
                class_label, tmpl->name);
            if (resolved_id == NULL)
                continue;

            for (ListNode_t *method_node = record_info->methods;
                 method_node != NULL; method_node = method_node->next) {
                struct MethodInfo *method = (struct MethodInfo *)method_node->cur;
                if (!codegen_template_matches_methodinfo(tmpl, method))
                    continue;
                if (method->resolved_mangled_id != NULL &&
                    method->resolved_mangled_id != method->mangled_name)
                    free(method->resolved_mangled_id);
                method->resolved_mangled_id = strdup(resolved_id);
                break;
            }
        }
    }

    /* Slots 12+: virtual methods.  Emit by vmt_index, not list order: imported
     * parents can contribute sparse inherited slots while subclasses add new
     * virtuals after the highest inherited index. */
    int max_vmt_index = 11;
    for (struct RecordType *cur_record = record_info; cur_record != NULL; ) {
        for (ListNode_t *method_node = cur_record->methods;
             method_node != NULL; method_node = method_node->next) {
            struct MethodInfo *method = (struct MethodInfo *)method_node->cur;
            if (method != NULL && method->vmt_index > max_vmt_index)
                max_vmt_index = method->vmt_index;
        }

        if (cur_record->parent_class_name == NULL)
            break;
        cur_record = codegen_lookup_record_type_by_name(
            symtab, cur_record->parent_class_name, 0);
    }

    for (int slot = 12; slot <= max_vmt_index; slot++) {
        struct MethodInfo *method = NULL;
        for (struct RecordType *cur_record = record_info;
             cur_record != NULL && method == NULL; ) {
            for (ListNode_t *method_node = cur_record->methods;
                 method_node != NULL; method_node = method_node->next) {
                struct MethodInfo *candidate = (struct MethodInfo *)method_node->cur;
                if (candidate != NULL && candidate->vmt_index == slot) {
                    method = candidate;
                    break;
                }
            }

            if (method != NULL || cur_record->parent_class_name == NULL)
                break;
            cur_record = codegen_lookup_record_type_by_name(
                symtab, cur_record->parent_class_name, 0);
        }

        if (method == NULL || method->mangled_name == NULL) {
            fprintf(ctx->output_file, "\t.quad\t__kgpc_abstract_method_error\n");
            continue;
        }

        const char *full_mangled = method->resolved_mangled_id;
        const char *slot_label = NULL;
        if (full_mangled != NULL && g_codegen_available_subprograms != NULL &&
            codegen_set_contains(&g_available_subprograms_set, full_mangled))
            slot_label = full_mangled;
        if (slot_label == NULL && full_mangled != NULL) {
            /* Not in available subprograms — check symtab for a real
             * implementation (has statement_list).  When present, pin it as
             * used so the method body survives DCE and the VMT slot links. */
            HashNode_t *sym = NULL;
            if (FindSymbol(&sym, symtab, full_mangled) != 0 && sym != NULL &&
                sym->type != NULL && sym->type->kind == TYPE_KIND_PROCEDURE &&
                sym->type->info.proc_info.definition != NULL &&
                sym->type->info.proc_info.definition->tree_data.subprogram_data.statement_list != NULL)
            {
                Tree_t *def = sym->type->info.proc_info.definition;
                const char *emit_target = codegen_subprogram_emission_symbol(sym);
                if (emit_target == NULL)
                    emit_target = full_mangled;
                def->tree_data.subprogram_data.is_used = 1;
                codegen_keep_subprogram_label(emit_target);
                slot_label = emit_target;
            }
        }
        if (slot_label != NULL) {
            fprintf(ctx->output_file, "\t.quad\t%s\n", slot_label);
        } else {
            fprintf(ctx->output_file, "\t.quad\t__kgpc_abstract_method_error\n");
        }
    }

    /* Emit interface method dispatch thunks.
     * For each interface a class directly implements, generate global symbols
     * for the interface method names that forward to the implementing class methods.
     * This enables interface method calls (e.g., FStream.Read(...)) to link
     * when emitted as direct calls to the interface method mangled name.
     *
     * Only direct implementers participate here. Inherited implementers reuse
     * the ancestor's interface entry points; emitting another global symbol for
     * the same interface method would not be well-defined.
     *
     * TODO: Replace with proper vtable-based interface dispatch for cases
     * where multiple classes implement the same interface. */
    if (record_info->num_interfaces > 0 && !record_info->is_interface) {
        for (int iidx = 0; iidx < record_info->num_interfaces; iidx++) {
            const char *iface_name = record_info->interface_names[iidx];
            if (iface_name == NULL) continue;
            /* Look up the interface to get its method list */
            struct RecordType *iface_record =
                codegen_lookup_record_type_by_name(symtab, iface_name, 0);
            if (iface_record == NULL) continue;
            /* Iterate interface method_templates directly — inherited parent
             * methods were already prepended during semcheck. */
            if (iface_record->method_templates == NULL) continue;
            ListNode_t *iface_method = iface_record->method_templates;
            while (iface_method != NULL) {
                struct MethodTemplate *imethod = (struct MethodTemplate *)iface_method->cur;
                if (imethod != NULL && imethod->name != NULL) {
                    const char *impl_resolved_id = codegen_find_class_method_impl_id(
                        symtab, record_info, class_label, iface_name, imethod->name);
                    /* Build the interface method mangled name: InterfaceName__MethodName */
                    char iface_base[512];
                    snprintf(iface_base, sizeof(iface_base), "%s__%s", iface_name, imethod->name);
                    /* Find the interface method's full mangled name */
                    ListNode_t *iface_candidates = FindAllIdents(symtab, iface_base);
                    HashNode_t *iface_func = NULL;
                    for (ListNode_t *ic = iface_candidates; ic != NULL; ic = ic->next) {
                        HashNode_t *cand = (HashNode_t *)ic->cur;
                        if (cand != NULL && cand->mangled_id != NULL &&
                            (cand->hash_type == HASHTYPE_FUNCTION ||
                             cand->hash_type == HASHTYPE_PROCEDURE)) {
                            iface_func = cand;
                            break;
                        }
                    }
                    if (iface_func != NULL && iface_func->mangled_id != NULL) {
                        /* Mark this interface method as handled so the
                         * symtab-wide abstract stub pass does not emit a
                         * duplicate definition. */
                        char stub_dedup[640];
                        snprintf(stub_dedup, sizeof(stub_dedup),
                                 "__kgpc_abstub_%s", iface_func->mangled_id);
                        if (emitted_class_set_contains(emitted_classes, stub_dedup)) {
                            /* Already emitted by a previous class — skip. */
                        } else {
                            emitted_class_set_add(emitted_classes, stub_dedup);
                            /* If the class doesn't provide an implementation
                             * (e.g. TObject-derived class implementing an interface
                             * without inheriting TInterfacedObject), fall back to
                             * runtime default helpers for the IInterface trio. */
                            const char *final_target = impl_resolved_id;
                            if (final_target == NULL) {
                                if (strcasecmp(imethod->name, "QueryInterface") == 0)
                                    final_target = "kgpc_default_queryinterface";
                                else if (strcasecmp(imethod->name, "_AddRef") == 0)
                                    final_target = "kgpc_default_addref";
                                else if (strcasecmp(imethod->name, "_Release") == 0)
                                    final_target = "kgpc_default_release";
                                else
                                    codegen_assert_interface_impl_resolved(
                                        iface_name, imethod->name, class_label,
                                        iface_func->mangled_id, impl_resolved_id);
                            }
                            if (final_target != NULL) {
                                fprintf(ctx->output_file, "\n# Interface dispatch: %s.%s -> %s\n",
                                    iface_name, imethod->name, final_target);
                                codegen_emit_global_jump_stub(ctx,
                                    iface_func->mangled_id, final_target);
                            }
                        }
                    }
                    if (iface_candidates != NULL) DestroyList(iface_candidates);
                }
                iface_method = iface_method->next;
            }
        }
    }

    /* Emit writable storage for class vars. */
    if (record_type_is_class(record_info) || record_has_class_vars(record_info) ||
        record_has_class_method_templates(record_info) || record_has_method_decls(record_info))
    {
        int include_all_fields = (!record_has_class_vars(record_info) &&
            (record_has_class_method_templates(record_info) || record_has_method_decls(record_info)));
        long long class_var_size = codegen_class_var_storage_size(symtab, record_info,
            include_all_fields ? 1 : 0);
        if (class_var_size <= 0)
            class_var_size = 8;

        fprintf(ctx->output_file, "\n# Class variables for %s\n", class_label);
        fprintf(ctx->output_file, "\t.data\n");
        fprintf(ctx->output_file, "\t.align 8\n");
        fprintf(ctx->output_file, ".globl %s_CLASSVAR\n", class_label);
        fprintf(ctx->output_file, "%s_CLASSVAR:\n", class_label);

        /* For interfaces with GUIDs, store the 16-byte GUID as the class var
         * data.  The codegen references this via ClassName_CLASSVAR when passing
         * an interface type where a TGUID is expected. */
        uint32_t iface_d1 = 0;
        uint16_t iface_d2 = 0, iface_d3 = 0;
        uint8_t iface_d4[8] = {0};
        if (record_info->is_interface &&
            codegen_resolve_record_guid(symtab, record_info, &iface_d1, &iface_d2, &iface_d3, iface_d4)) {
            fprintf(ctx->output_file, "\t.long\t0x%08lX\n",
                (unsigned long)iface_d1);
            fprintf(ctx->output_file, "\t.short\t0x%04X\n",
                (unsigned int)iface_d2);
            fprintf(ctx->output_file, "\t.short\t0x%04X\n",
                (unsigned int)iface_d3);
            fprintf(ctx->output_file, "\t.byte\t0x%02X, 0x%02X, 0x%02X, 0x%02X, 0x%02X, 0x%02X, 0x%02X, 0x%02X\n",
                iface_d4[0], iface_d4[1], iface_d4[2], iface_d4[3],
                iface_d4[4], iface_d4[5], iface_d4[6], iface_d4[7]);
        } else {
            /* Emit per-field labels for class var fields */
            {
                long long offset = 0;
                ListNode_t *fn = record_info->fields;
                while (fn != NULL) {
                    if (fn->type == LIST_RECORD_FIELD && fn->cur != NULL) {
                        struct RecordField *f = (struct RecordField *)fn->cur;
                        if (f != NULL && (include_all_fields || f->is_class_var == 1)) {
                            int fsz = codegen_class_var_field_size(symtab, f);
                            int align = (fsz >= 8) ? 8 : ((fsz >= 4) ? 4 : 1);
                            long long aligned_off = (offset + align - 1) & ~(align - 1);
                            long long pad = aligned_off - offset;
                            if (pad > 0)
                                fprintf(ctx->output_file, "\t.zero\t%lld\n", pad);
                            if (f->name != NULL && f->is_class_var == 1) {
                                fprintf(ctx->output_file, "%s\t%s\n", codegen_weak_or_globl(), f->name);
                                fprintf(ctx->output_file, "%s:\n", f->name);
                            }
                            fprintf(ctx->output_file, "\t.zero\t%d\n", fsz);
                            offset = aligned_off + fsz;
                        }
                    }
                    fn = fn->next;
                }
                if (offset < class_var_size)
                    fprintf(ctx->output_file, "\t.zero\t%lld\n", class_var_size - offset);
            }
        }

        fprintf(ctx->output_file, "%s\n", codegen_readonly_section_directive());
    }

    /* Emit GUID for standalone interfaces (their VMT is emitted above but
     * a separate GUID pass may miss them if they are not in the main scope). */
    if (record_info->is_interface && class_label != NULL)
    {
        uint32_t gd1 = 0;
        uint16_t gd2 = 0, gd3 = 0;
        uint8_t gd4[8] = {0};
        codegen_resolve_record_guid(symtab, record_info, &gd1, &gd2, &gd3, gd4);
        codegen_emit_interface_guid(ctx, class_label, gd1, gd2, gd3, gd4,
            " (from VMT emission)", emitted_classes);
    }

    if (free_effective_iface_names)
        free((void *)effective_iface_names);
}

static void codegen_emit_record_classvar_storage(CodeGenContext *ctx, SymTab_t *symtab,
    struct RecordType *record_info, const char *class_label,
    EmittedClassSet *emitted_classes)
{
    if (record_info == NULL || record_type_is_class(record_info) || class_label == NULL)
        return;

    int has_class_vars = record_has_class_vars(record_info);
    int has_class_methods = record_has_class_method_templates(record_info) ||
        record_has_method_decls(record_info);
    if (!has_class_vars && !has_class_methods)
        return;

    if (emitted_class_set_contains(emitted_classes, class_label))
        return;

    if (emitted_class_set_add(emitted_classes, class_label) != 0)
        return;

    long long class_var_size = codegen_class_var_storage_size(symtab, record_info,
        has_class_vars ? 0 : 1);
    if (class_var_size <= 0)
        class_var_size = 8;

    fprintf(ctx->output_file, "\n# Class var storage for record %s\n", class_label);
    fprintf(ctx->output_file, "\t.data\n");
    fprintf(ctx->output_file, "\t.align 8\n");
    fprintf(ctx->output_file, ".globl %s_CLASSVAR\n", class_label);
    /* Emit a weak alias from the bare type name to the _CLASSVAR label
       so that codegen references like "leaq HeapInc(%rip)" resolve. */
    fprintf(ctx->output_file, "%s\t%s\n", codegen_weak_or_globl(), class_label);
    fprintf(ctx->output_file, "%s:\n", class_label);
    fprintf(ctx->output_file, "%s_CLASSVAR:\n", class_label);

    /* Emit per-field labels so inline asm can reference class vars by bare name.
       Walk fields and emit .globl + label at each class var's offset. */
    {
        int include_all = has_class_vars ? 0 : 1;
        long long offset = 0;
        ListNode_t *fn = record_info->fields;
        while (fn != NULL) {
            if (fn->type == LIST_RECORD_FIELD && fn->cur != NULL) {
                struct RecordField *f = (struct RecordField *)fn->cur;
                if (f != NULL && (include_all || f->is_class_var == 1)) {
                    int fsz = codegen_class_var_field_size(symtab, f);
                    int align = (fsz >= 8) ? 8 : ((fsz >= 4) ? 4 : 1);
                    long long aligned_off = (offset + align - 1) & ~(align - 1);
                    long long pad = aligned_off - offset;
                    if (pad > 0)
                        fprintf(ctx->output_file, "\t.zero\t%lld\n", pad);
                    /* Emit a weak label with the bare field name, but only for
                       actual class vars (not regular fields that happen to be included). */
                    if (f->name != NULL && f->is_class_var == 1) {
                        fprintf(ctx->output_file, "%s\t%s\n", codegen_weak_or_globl(), f->name);
                        fprintf(ctx->output_file, "%s:\n", f->name);
                    }
                    fprintf(ctx->output_file, "\t.zero\t%d\n", fsz);
                    offset = aligned_off + fsz;
                }
            }
            fn = fn->next;
        }
        /* Emit remaining padding if offset < class_var_size */
        if (offset < class_var_size)
            fprintf(ctx->output_file, "\t.zero\t%lld\n", class_var_size - offset);
    }

    /* Emit stub TYPEINFO/VMT symbols for advanced records.
     * This record was added to emitted_classes[], so the alias loop will
     * generate .set directives referencing these symbols (e.g., for pointer
     * type aliases like PPropInfo = ^TPropInfo).  Without these stubs,
     * strict linkers like ld.lld report undefined symbol errors. */
    fprintf(ctx->output_file, "%s\n", codegen_readonly_section_directive());
    fprintf(ctx->output_file, "\t.align 8\n");
    fprintf(ctx->output_file, ".globl %s_TYPEINFO\n", class_label);
    fprintf(ctx->output_file, "%s_TYPEINFO:\n", class_label);
    fprintf(ctx->output_file, "\t.quad\t0\n");  /* No parent class */
    fprintf(ctx->output_file, ".globl %s_VMT\n", class_label);
    fprintf(ctx->output_file, "%s_VMT:\n", class_label);
    fprintf(ctx->output_file, "\t.quad\t%s_TYPEINFO\n", class_label);
}

static int codegen_should_emit_plain_record_typeinfo(const struct RecordType *record_info,
    const char *record_label)
{
    if (record_info == NULL || record_label == NULL || record_label[0] == '\0')
        return 0;
    if (record_info->type_id == NULL || record_info->type_id[0] == '\0')
        return 0;
    return strcmp(record_info->type_id, record_label) == 0;
}

static void codegen_emit_plain_record_typeinfo(CodeGenContext *ctx, const struct RecordType *record_info,
    const char *record_label, EmittedClassSet *emitted_classes)
{
    if (ctx == NULL || ctx->output_file == NULL || record_label == NULL)
        return;
    if (!codegen_should_emit_plain_record_typeinfo(record_info, record_label))
        return;
    if (emitted_class_set_contains(emitted_classes, record_label))
        return;
    if (emitted_class_set_add(emitted_classes, record_label) != 0)
        return;

    fprintf(ctx->output_file, "\n# TYPEINFO/VMT stubs for record %s\n", record_label);
    fprintf(ctx->output_file, "%s\n", codegen_readonly_section_directive());
    fprintf(ctx->output_file, "\t.align 8\n");
    fprintf(ctx->output_file, ".globl %s_TYPEINFO\n", record_label);
    fprintf(ctx->output_file, "%s_TYPEINFO:\n", record_label);
    fprintf(ctx->output_file, "\t.quad\t0\n");
    fprintf(ctx->output_file, ".globl %s_VMT\n", record_label);
    fprintf(ctx->output_file, "%s_VMT:\n", record_label);
    fprintf(ctx->output_file, "\t.quad\t%s_TYPEINFO\n", record_label);
}

static int codegen_record_visible_field_count(const struct RecordType *record)
{
    int count = 0;
    if (record == NULL)
        return 0;
    for (ListNode_t *cur = record->fields; cur != NULL; cur = cur->next) {
        if (cur->type != LIST_RECORD_FIELD || cur->cur == NULL)
            continue;
        struct RecordField *field = (struct RecordField *)cur->cur;
        if (!record_field_is_hidden(field))
            count++;
    }
    return count;
}

static int codegen_record_is_forward_stub(const struct RecordType *record)
{
    if (record == NULL)
        return 0;
    if (!record->is_class && !record->is_interface)
        return 0;
    if (record->parent_class_name != NULL)
        return 0;
    if (record->num_interfaces > 0)
        return 0;
    if (record->method_templates != NULL)
        return 0;
    if (record->properties != NULL)
        return 0;
    return codegen_record_visible_field_count(record) == 0;
}

static const struct RecordType *codegen_record_parent(const struct RecordType *record,
    SymTab_t *symtab)
{
    if (record == NULL || symtab == NULL || record->parent_class_name == NULL)
        return NULL;

    ListNode_t *matches = FindAllIdents(symtab, record->parent_class_name);
    if (matches == NULL)
        return NULL;

    HashNode_t *best_node = NULL;
    int best_score = INT_MIN;

    for (ListNode_t *cur = matches; cur != NULL; cur = cur->next) {
        HashNode_t *cand = (HashNode_t *)cur->cur;
        if (cand == NULL || cand->hash_type != HASHTYPE_TYPE)
            continue;

        int score = 0;
        if (cand->defined_in_unit)
            score += 1000;
        if (cand->unit_is_public)
            score += 100;
        if (cand->source_unit_index > 0)
            score += 10 + cand->source_unit_index;

        if (best_node == NULL || score > best_score) {
            best_node = cand;
            best_score = score;
        }
    }
    DestroyList(matches);
    if (best_node == NULL)
        return NULL;

    if (best_node->source_unit_index > 0)
    {
        const char *unit_name = unit_registry_get(best_node->source_unit_index);
        if (unit_name != NULL)
        {
            size_t qualified_len = strlen(unit_name) + 1 + strlen(record->parent_class_name) + 1;
            char *qualified_id = (char *)malloc(qualified_len);
            if (qualified_id != NULL)
            {
                snprintf(qualified_id, qualified_len, "%s.%s", unit_name, record->parent_class_name);
                HashNode_t *qualified = NULL;
                if (FindSymbol(&qualified, symtab, qualified_id) != 0 && qualified != NULL)
                {
                    const struct RecordType *qualified_record = get_record_type_from_node(qualified);
                    if (qualified_record == NULL && qualified->type != NULL &&
                        qualified->type->kind == TYPE_KIND_POINTER &&
                        qualified->type->info.points_to != NULL &&
                        qualified->type->info.points_to->kind == TYPE_KIND_RECORD)
                        qualified_record = qualified->type->info.points_to->info.record_info;
                    free(qualified_id);
                    if (qualified_record != NULL)
                        return qualified_record;
                }
                free(qualified_id);
            }
        }
    }

    const struct RecordType *best_record = get_record_type_from_node(best_node);
    if (best_record == NULL && best_node->type != NULL &&
        best_node->type->kind == TYPE_KIND_POINTER &&
        best_node->type->info.points_to != NULL &&
        best_node->type->info.points_to->kind == TYPE_KIND_RECORD)
        best_record = best_node->type->info.points_to->info.record_info;
    return best_record;
}


static const char *codegen_find_interface_delegate_target_name(
    const struct RecordType *record, const char *iface_name, const char *method_name)
{
    if (record == NULL || iface_name == NULL || method_name == NULL ||
        record->method_templates == NULL)
        return NULL;

    for (ListNode_t *cur = record->method_templates; cur != NULL; cur = cur->next) {
        if (cur->type != LIST_METHOD_TEMPLATE || cur->cur == NULL)
            continue;
        struct MethodTemplate *tmpl = (struct MethodTemplate *)cur->cur;
        if (!tmpl->is_interface_delegation ||
            tmpl->delegated_interface_name == NULL ||
            tmpl->name == NULL ||
            tmpl->delegated_target_name == NULL)
            continue;
        if (strcasecmp(tmpl->delegated_interface_name, iface_name) == 0 &&
            strcasecmp(tmpl->name, method_name) == 0)
            return tmpl->delegated_target_name;
    }

    return NULL;
}

const char *codegen_find_class_method_impl_id(SymTab_t *symtab,
    const struct RecordType *record, const char *fallback_class_label,
    const char *iface_name, const char *method_name)
{
    const struct RecordType *cur_record = record;
    const char *cur_label = fallback_class_label;

    const struct RecordType *origin_record = record;

    while (cur_record != NULL || cur_label != NULL) {
        const char *owner_label = cur_label;
        if (cur_record != NULL && cur_record->type_id != NULL)
            owner_label = cur_record->type_id;
        if (owner_label == NULL)
            break;

        const char *lookup_method_name = method_name;
        if (cur_record != NULL && iface_name != NULL && cur_record == origin_record) {
            const char *delegate_target = codegen_find_interface_delegate_target_name(
                cur_record, iface_name, method_name);
            if (delegate_target != NULL)
                lookup_method_name = delegate_target;
        }

        char base_name[512];
        snprintf(base_name, sizeof(base_name), "%s__%s", owner_label, lookup_method_name);
        ListNode_t *impl_candidates = FindAllIdents(symtab, base_name);
        const char *resolved_id = NULL;
        Tree_t *resolved_def = NULL;
        for (ListNode_t *ic = impl_candidates; ic != NULL; ic = ic->next) {
            HashNode_t *cand = (HashNode_t *)ic->cur;
            if (cand == NULL || cand->mangled_id == NULL ||
                cand->type == NULL || cand->type->kind != TYPE_KIND_PROCEDURE ||
                cand->type->info.proc_info.definition == NULL)
                continue;
            Tree_t *def = cand->type->info.proc_info.definition;
            const char *emit_target = codegen_subprogram_emission_symbol(cand);
            if (emit_target == NULL)
                continue;
            if (g_codegen_available_subprograms != NULL &&
                codegen_set_contains(&g_available_subprograms_set,emit_target)) {
                resolved_id = emit_target;
                resolved_def = def;
                break;
            }
            if (def->tree_data.subprogram_data.statement_list != NULL) {
                resolved_id = emit_target;
                resolved_def = def;
                break;
            }
        }
        if (impl_candidates != NULL)
            DestroyList(impl_candidates);
        if (resolved_id != NULL) {
            if (resolved_def != NULL)
                resolved_def->tree_data.subprogram_data.is_used = 1;
            codegen_keep_subprogram_label(resolved_id);
            return resolved_id;
        }

        if (cur_record == NULL)
            break;
        cur_record = codegen_record_parent(cur_record, symtab);
        cur_label = NULL;
    }

    return NULL;
}

static int codegen_class_implements_interface(SymTab_t *symtab,
    const struct RecordType *record, const char *class_label,
    const struct RecordType *iface_record)
{
    if (symtab == NULL || record == NULL || iface_record == NULL)
        return 0;

    /* method_templates already includes inherited parent methods (prepended
     * during semcheck), so iterate directly. */
    if (iface_record->method_templates == NULL)
        return 0;

    int result = 1;
    for (ListNode_t *cur = iface_record->method_templates; cur != NULL; cur = cur->next) {
        struct MethodTemplate *tmpl = (struct MethodTemplate *)cur->cur;
        if (tmpl == NULL || tmpl->name == NULL)
            continue;
        if (codegen_find_class_method_impl_id(symtab, record, class_label,
                iface_record->type_id, tmpl->name) == NULL) {
            result = 0;
            break;
        }
    }
    return result;
}

static void __attribute__((unused)) codegen_collect_inferred_interfaces(SymTab_t *symtab,
    const struct RecordType *record, const char *class_label,
    const char ***out_names, int *out_count)
{
    *out_names = NULL;
    *out_count = 0;
    if (symtab == NULL || record == NULL || !record->is_class)
        return;

    int cap = 0;
    const char **names = NULL;

    const struct RecordType *parent = codegen_record_parent(record, symtab);
    if (parent != NULL && parent->num_interfaces > 0 &&
        parent->interface_names != NULL) {
        for (int i = 0; i < parent->num_interfaces; i++) {
            const char *iface = parent->interface_names[i];
            if (iface == NULL)
                continue;
            if (*out_count == cap) {
                cap = cap == 0 ? 8 : cap * 2;
                names = (const char **)realloc((void *)names, sizeof(char *) * cap);
            }
            names[*out_count] = iface;
            (*out_count)++;
        }
    }

    for (int unit_idx = 0; unit_idx < SYMTAB_MAX_UNITS; unit_idx++) {
        ScopeNode *scope = symtab->unit_scopes[unit_idx];
        HashTable_t *table = scope != NULL ? scope->table : NULL;
        if (table == NULL)
            continue;
        for (int b = 0; b < TABLE_SIZE; b++) {
            for (ListNode_t *node = table->table[b]; node != NULL; node = node->next) {
                HashNode_t *hash_node = (HashNode_t *)node->cur;
                if (hash_node == NULL || hash_node->hash_type != HASHTYPE_TYPE)
                    continue;
                struct RecordType *iface_record = get_record_type_from_node(hash_node);
                if (iface_record == NULL && hash_node->type != NULL &&
                    hash_node->type->kind == TYPE_KIND_POINTER &&
                    hash_node->type->info.points_to != NULL &&
                    hash_node->type->info.points_to->kind == TYPE_KIND_RECORD)
                    iface_record = hash_node->type->info.points_to->info.record_info;
                if (iface_record == NULL || !iface_record->is_interface)
                    continue;
                const char *iface_name = iface_record->type_id != NULL ? iface_record->type_id : hash_node->id;
                if (iface_name == NULL)
                    continue;
                int already = 0;
                for (int i = 0; i < *out_count; i++) {
                    if (pascal_identifier_equals(names[i], iface_name)) {
                        already = 1;
                        break;
                    }
                }
                if (already)
                    continue;
                if (!codegen_class_implements_interface(symtab, record, class_label, iface_record))
                    continue;
                if (*out_count == cap) {
                    cap = cap == 0 ? 8 : cap * 2;
                    names = (const char **)realloc((void *)names, sizeof(char *) * cap);
                }
                names[*out_count] = iface_name;
                (*out_count)++;
            }
        }
    }

    *out_names = names;

}

static void codegen_canonicalize_record_for_emission(SymTab_t *symtab,
    const char **class_label, struct RecordType **record_info)
{
    if (symtab == NULL || class_label == NULL || *class_label == NULL)
        return;

    HashNode_t *best_node = codegen_pick_type_node_by_name(symtab, *class_label);
    struct RecordType *best_record =
        codegen_lookup_record_type_for_node(symtab, best_node, *class_label);
    if (best_record == NULL)
        return;

    *record_info = best_record;
    if (best_record->type_id != NULL)
        *class_label = best_record->type_id;
    else if (best_node != NULL && best_node->id != NULL)
        *class_label = best_node->id;
}

/* Helper: emit VMTs/RTTI for all type declarations in a list. */
static void codegen_vmt_from_type_list(CodeGenContext *ctx, SymTab_t *symtab,
                                        ListNode_t *type_decls,
                                        EmittedClassSet *emitted_classes)
{
    ListNode_t *cur = type_decls;
    while (cur != NULL) {
        Tree_t *type_tree = (Tree_t *)cur->cur;
        if (type_tree != NULL && type_tree->type == TREE_TYPE_DECL) {
            if (codegen_type_decl_suppressed(type_tree))
            {
                cur = cur->next;
                continue;
            }
            struct RecordType *record_info = NULL;
            const char *class_label = NULL;

            if (type_tree->tree_data.type_decl_data.kind == TYPE_DECL_RECORD) {
                record_info = codegen_record_from_type_decl_ex(type_tree, symtab);
                const char *type_name = type_tree->tree_data.type_decl_data.id;
                class_label = (record_info != NULL && record_info->type_id != NULL) ?
                    record_info->type_id : type_name;
            }
            else if (type_tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS) {
                record_info = codegen_record_from_type_decl_ex(type_tree, symtab);
                if (record_info != NULL && record_info->type_id != NULL) {
                    if (record_info->is_generic_specialization) {
                        class_label = record_info->type_id;
                    }
                }
            }

            codegen_canonicalize_record_for_emission(symtab, &class_label, &record_info);
            if (codegen_record_is_forward_stub(record_info)) {
                cur = cur->next;
                continue;
            }

            codegen_emit_class_vmt(ctx, symtab, record_info, class_label,
                emitted_classes);
            codegen_emit_record_classvar_storage(ctx, symtab, record_info, class_label,
                emitted_classes);
            if (record_info != NULL && !record_type_is_class(record_info) &&
                class_label != NULL && class_label[0] != '\0')
            {
                codegen_emit_plain_record_typeinfo(ctx, record_info, class_label, emitted_classes);
            }
        }
        cur = cur->next;
    }
}

static void codegen_emit_vmts_from_hash_table(CodeGenContext *ctx, SymTab_t *symtab,
    HashTable_t *table, EmittedClassSet *emitted_classes)
{
    if (table == NULL)
        return;

    for (int b = 0; b < TABLE_SIZE; b++)
    {
        ListNode_t *node = table->table[b];
        while (node != NULL)
        {
            HashNode_t *hash_node = (HashNode_t *)node->cur;
            if (hash_node != NULL && hash_node->hash_type == HASHTYPE_TYPE &&
                hash_node->type != NULL)
            {
                struct RecordType *record_info = NULL;
                const char *class_label = NULL;
                if (hash_node->type->kind == TYPE_KIND_RECORD)
                {
                    record_info = hash_node->type->info.record_info;
                }
                else if (hash_node->type->kind == TYPE_KIND_POINTER &&
                    hash_node->type->info.points_to != NULL &&
                    hash_node->type->info.points_to->kind == TYPE_KIND_RECORD)
                {
                    record_info = hash_node->type->info.points_to->info.record_info;
                }
                if (record_info != NULL)
                    class_label = record_info->type_id;
                if (class_label == NULL)
                    class_label = hash_node->id;

                codegen_canonicalize_record_for_emission(symtab, &class_label, &record_info);
                if (codegen_record_is_forward_stub(record_info)) {
                    node = node->next;
                    continue;
                }

                codegen_emit_class_vmt(ctx, symtab, record_info, class_label,
                    emitted_classes);
                codegen_emit_record_classvar_storage(ctx, symtab, record_info, class_label,
                    emitted_classes);
                if (record_info != NULL && !record_type_is_class(record_info) &&
                    class_label != NULL && class_label[0] != '\0')
                {
                    codegen_emit_plain_record_typeinfo(ctx, record_info, class_label, emitted_classes);
                }
            }
            node = node->next;
        }
    }
}


/* Helper: emit GUID data for all interfaces with GUIDs in a type declaration list.
 * Used to emit GUIDs from loaded units whose interfaces are not in the local scope. */
static void codegen_emit_guids_from_type_list(CodeGenContext *ctx,
                                               ListNode_t *type_decls,
                                               EmittedClassSet *emitted_classes)
{
    ListNode_t *cur = type_decls;
    while (cur != NULL) {
        Tree_t *type_tree = (Tree_t *)cur->cur;
        if (type_tree != NULL && type_tree->type == TREE_TYPE_DECL) {
            struct RecordType *record_info = codegen_record_from_type_decl(type_tree);
            if (record_info != NULL && record_info->is_interface)
            {
                uint32_t guid_d1 = 0;
                uint16_t guid_d2 = 0, guid_d3 = 0;
                uint8_t guid_d4[8] = {0};
                codegen_resolve_record_guid(ctx->symtab, record_info,
                    &guid_d1, &guid_d2, &guid_d3, guid_d4);

                const char *iface_name = record_info->type_id;
                if (iface_name == NULL)
                    iface_name = type_tree->tree_data.type_decl_data.id;
                if (iface_name == NULL) { cur = cur->next; continue; }

                codegen_emit_interface_guid(ctx, iface_name, guid_d1, guid_d2,
                    guid_d3, guid_d4, " (from unit)", emitted_classes);
            }
        }
        cur = cur->next;
    }
}

/* Interface dispatch entry points must be emitted as concrete labels rather
 * than assembler aliases so COFF and ELF toolchains see the same symbols. */
static void codegen_emit_global_jump_stub(CodeGenContext *ctx,
    const char *exported_symbol, const char *target_symbol)
{
    if (ctx == NULL || exported_symbol == NULL || target_symbol == NULL)
        return;

    fprintf(ctx->output_file, "%s\n", codegen_text_section_resume());
    fprintf(ctx->output_file, ".globl %s\n", exported_symbol);
    fprintf(ctx->output_file, "%s:\n", exported_symbol);
    fprintf(ctx->output_file, "\tjmp\t%s\n", target_symbol);
}

static void codegen_assert_interface_impl_resolved(const char *iface_name,
    const char *method_name, const char *class_label,
    const char *iface_symbol, const char *impl_symbol)
{
    if (iface_symbol == NULL || iface_symbol[0] == '\0')
        return;
    if (impl_symbol != NULL && impl_symbol[0] != '\0')
        return;

    fprintf(stderr,
        "[KGPC] unresolved interface dispatch: %s.%s for class %s (%s)\n",
        iface_name != NULL ? iface_name : "<interface>",
        method_name != NULL ? method_name : "<method>",
        class_label != NULL ? class_label : "<class>",
        iface_symbol);
    assert(0 && "unresolved interface dispatch target");
}

/* Helper: emit TYPEINFO/VMT aliases for type aliases pointing to class types. */
static void codegen_vmt_aliases_from_type_list(CodeGenContext *ctx,
                                                ListNode_t *type_decls,
                                                EmittedClassSet *emitted_classes)
{
    ListNode_t *cur = type_decls;
    while (cur != NULL) {
        Tree_t *type_tree = (Tree_t *)cur->cur;
        if (type_tree != NULL && type_tree->type == TREE_TYPE_DECL &&
            !codegen_type_decl_suppressed(type_tree) &&
            type_tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS) {
            const char *alias_name = type_tree->tree_data.type_decl_data.id;
            const char *target_name = type_tree->tree_data.type_decl_data.info.alias.target_type_id;
            if (alias_name != NULL && target_name != NULL) {
                int target_emitted = emitted_class_set_contains(emitted_classes, target_name);
                int alias_already_owned = emitted_class_set_contains(emitted_classes, alias_name);
                if (target_emitted && !alias_already_owned) {
                    fprintf(ctx->output_file, "\n# TYPEINFO alias: %s = %s\n", alias_name, target_name);
                    fprintf(ctx->output_file, "%s\t%s_TYPEINFO\n", codegen_weak_or_globl(), alias_name);
                    fprintf(ctx->output_file, "\t.set\t%s_TYPEINFO, %s_TYPEINFO\n", alias_name, target_name);
                    fprintf(ctx->output_file, "%s\t%s_VMT\n", codegen_weak_or_globl(), alias_name);
                    fprintf(ctx->output_file, "\t.set\t%s_VMT, %s_VMT\n", alias_name, target_name);
                    emitted_class_set_add(emitted_classes, alias_name);
                }
            }
        }
        cur = cur->next;
    }
}

/* Generate Virtual Method Tables (VMT) for classes with virtual methods.
 * Iterates type declarations from loaded units (via comp_ctx) and the program tree.
 * Uses a single EmittedClassSet to avoid duplicate emissions. */
void codegen_vmt(CodeGenContext *ctx, SymTab_t *symtab, Tree_t *tree,
                 CompilationContext *comp_ctx)
{
    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: ENTERING %s\n", __func__);
    #endif
    assert(ctx != NULL);
    assert(symtab != NULL);
    assert(tree != NULL);

    /* RTTI metadata and VMTs are generated as read-only data structures */
    fprintf(ctx->output_file, "\n");
    fprintf(ctx->output_file, "# Class RTTI metadata and Virtual Method Tables (VMT)\n");
    fprintf(ctx->output_file, "%s\n", codegen_readonly_section_directive());

    EmittedClassSet emitted_classes = {0};

    /* Emit VMTs from loaded units */
    if (comp_ctx != NULL) {
        for (int i = 0; i < comp_ctx->loaded_unit_count; ++i) {
            Tree_t *unit = comp_ctx->loaded_units[i].unit_tree;
            if (unit != NULL && unit->type == TREE_UNIT) {
                codegen_vmt_from_type_list(ctx, symtab,
                    unit->tree_data.unit_data.interface_type_decls, &emitted_classes);
                codegen_vmt_from_type_list(ctx, symtab,
                    unit->tree_data.unit_data.implementation_type_decls, &emitted_classes);
            }
        }
    }

    /* Emit VMTs from the current compilation unit/program declarations.
     * When compiling a unit directly, it is not yet present in loaded_units,
     * so we must emit from the current TREE_UNIT explicitly.
     * When --skip-unit-codegen is active, codegen_vmt returns early above. */
    if (tree->type == TREE_PROGRAM_TYPE)
        codegen_vmt_from_type_list(ctx, symtab,
            tree->tree_data.program_data.type_declaration, &emitted_classes);
    else if (tree->type == TREE_UNIT)
    {
        codegen_vmt_from_type_list(ctx, symtab,
            tree->tree_data.unit_data.interface_type_decls, &emitted_classes);
        codegen_vmt_from_type_list(ctx, symtab,
            tree->tree_data.unit_data.implementation_type_decls, &emitted_classes);
    }

    /* Also emit VMTs for class types that exist only in the symbol table
     * (e.g., specializations pulled in from units like FGL).
     * Restrict this fallback to whole-program codegen. Direct unit codegen
     * should emit from the unit's own declared types, not arbitrary symtab
     * entries that may include incomplete or transient records.
     * When --skip-unit-codegen is active, codegen_vmt returns early above. */
    if (ctx->is_whole_program)
    {
        for (ScopeNode *scope = symtab->current_scope; scope != NULL; scope = scope->parent)
        {
            codegen_emit_vmts_from_hash_table(ctx, symtab, scope->table, &emitted_classes);
        }

        for (int i = 0; i < SYMTAB_MAX_UNITS; i++)
        {
            ScopeNode *unit_scope = symtab->unit_scopes[i];
            if (unit_scope == NULL)
                continue;
            codegen_emit_vmts_from_hash_table(ctx, symtab, unit_scope->table, &emitted_classes);
        }
    }
    else
    {
        /* Direct unit codegen must not use this symtab-scan fallback:
         * the symbol table may contain incomplete or transient records from
         * other units.  Emit only from the unit's own declared types above. */
        assert(tree->type == TREE_UNIT &&
               "symtab VMT fallback reached in non-unit, non-whole-program context");
    }

    /* Emit GUID data for ALL interfaces with GUIDs found in the symbol table.
     * The previous passes only emit GUIDs when a class implements an interface,
     * but Supports() calls may reference interface GUIDs without any local class
     * implementing the interface.  This pass ensures all GUID symbols are defined. */
    for (ScopeNode *scope = symtab->current_scope; scope != NULL; scope = scope->parent)
    {
        HashTable_t *table = scope->table;
        if (table == NULL)
            continue;
        for (int b = 0; b < TABLE_SIZE; b++)
        {
            ListNode_t *node = table->table[b];
            while (node != NULL)
            {
                HashNode_t *hash_node = (HashNode_t *)node->cur;
                if (hash_node != NULL && hash_node->hash_type == HASHTYPE_TYPE &&
                    hash_node->type != NULL)
                {
                    struct RecordType *record_info = NULL;
                    if (hash_node->type->kind == TYPE_KIND_RECORD)
                        record_info = hash_node->type->info.record_info;
                    else if (hash_node->type->kind == TYPE_KIND_POINTER &&
                             hash_node->type->info.points_to != NULL &&
                             hash_node->type->info.points_to->kind == TYPE_KIND_RECORD)
                        record_info = hash_node->type->info.points_to->info.record_info;

                    if (record_info != NULL && record_info->is_interface)
                    {
                        uint32_t guid_d1 = 0;
                        uint16_t guid_d2 = 0, guid_d3 = 0;
                        uint8_t guid_d4[8] = {0};
                        codegen_resolve_record_guid(symtab, record_info,
                            &guid_d1, &guid_d2, &guid_d3, guid_d4);
                        const char *iface_name = record_info->type_id;
                        if (iface_name == NULL)
                            iface_name = hash_node->id;
                        if (iface_name != NULL)
                            codegen_emit_interface_guid(ctx, iface_name,
                                guid_d1, guid_d2, guid_d3, guid_d4,
                                " (standalone)", &emitted_classes);
                    }
                }
                node = node->next;
            }
        }
    }

    /* Also walk unit_scopes for interfaces (parallel to the VMT walk above). */
    for (int ui = 0; ui < SYMTAB_MAX_UNITS; ui++)
    {
        ScopeNode *unit_scope = symtab->unit_scopes[ui];
        if (unit_scope == NULL)
            continue;
        HashTable_t *table = unit_scope->table;
        if (table == NULL)
            continue;
        for (int b = 0; b < TABLE_SIZE; b++)
        {
            ListNode_t *node = table->table[b];
            while (node != NULL)
            {
                HashNode_t *hash_node = (HashNode_t *)node->cur;
                if (hash_node != NULL && hash_node->hash_type == HASHTYPE_TYPE &&
                    hash_node->type != NULL)
                {
                    struct RecordType *record_info = NULL;
                    if (hash_node->type->kind == TYPE_KIND_RECORD)
                        record_info = hash_node->type->info.record_info;
                    else if (hash_node->type->kind == TYPE_KIND_POINTER &&
                             hash_node->type->info.points_to != NULL &&
                             hash_node->type->info.points_to->kind == TYPE_KIND_RECORD)
                        record_info = hash_node->type->info.points_to->info.record_info;

                    if (record_info != NULL && record_info->is_interface)
                    {
                        uint32_t guid_d1 = 0;
                        uint16_t guid_d2 = 0, guid_d3 = 0;
                        uint8_t guid_d4[8] = {0};
                        codegen_resolve_record_guid(symtab, record_info,
                            &guid_d1, &guid_d2, &guid_d3, guid_d4);
                        const char *iface_name = record_info->type_id;
                        if (iface_name == NULL)
                            iface_name = hash_node->id;
                        if (iface_name != NULL)
                            codegen_emit_interface_guid(ctx, iface_name,
                                guid_d1, guid_d2, guid_d3, guid_d4,
                                " (from unit scope)", &emitted_classes);
                    }
                }
                node = node->next;
            }
        }
    }

    /* Emit GUIDs from loaded units whose interfaces may not be in local scope */
    if (comp_ctx != NULL) {
        for (int i = 0; i < comp_ctx->loaded_unit_count; ++i) {
            Tree_t *unit = comp_ctx->loaded_units[i].unit_tree;
            if (unit != NULL && unit->type == TREE_UNIT) {
                codegen_emit_guids_from_type_list(ctx,
                    unit->tree_data.unit_data.interface_type_decls, &emitted_classes);
                codegen_emit_guids_from_type_list(ctx,
                    unit->tree_data.unit_data.implementation_type_decls, &emitted_classes);
            }
        }
    }
    /* Also emit GUIDs from program type declarations */
    if (tree->type == TREE_PROGRAM_TYPE)
        codegen_emit_guids_from_type_list(ctx,
            tree->tree_data.program_data.type_declaration, &emitted_classes);

    /* Emit TYPEINFO/VMT aliases from loaded units and program */
    if (comp_ctx != NULL) {
        for (int i = 0; i < comp_ctx->loaded_unit_count; ++i) {
            Tree_t *unit = comp_ctx->loaded_units[i].unit_tree;
            if (unit != NULL && unit->type == TREE_UNIT) {
                codegen_vmt_aliases_from_type_list(ctx,
                    unit->tree_data.unit_data.interface_type_decls, &emitted_classes);
                codegen_vmt_aliases_from_type_list(ctx,
                    unit->tree_data.unit_data.implementation_type_decls, &emitted_classes);
            }
        }
    }
    if (tree->type == TREE_PROGRAM_TYPE)
        codegen_vmt_aliases_from_type_list(ctx,
            tree->tree_data.program_data.type_declaration, &emitted_classes);

    emitted_class_set_destroy(&emitted_classes);

    fprintf(ctx->output_file, "%s\n", codegen_text_section_resume());

    #ifdef DEBUG_CODEGEN
    CODEGEN_DEBUG("DEBUG: LEAVING %s\n", __func__);
    #endif
}
