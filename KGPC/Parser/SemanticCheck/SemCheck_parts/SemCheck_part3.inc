/* Pre-declare types so they can be used in const expressions like High(MyType).
 * This function creates stub entries in the symbol table for SIMPLE type aliases
 * BEFORE const declarations are processed.
 *
 * This enables patterns like:
 *   type
 *     MyInt = Int64;
 *   const
 *     MaxMyInt = High(MyInt);  // Works because MyInt is pre-declared
 *
 * IMPORTANT: We only pre-declare simple primitive type aliases (e.g., MyInt = Int64).
 * Complex types (pointers, arrays, records, etc.) are NOT pre-declared because
 * they may have forward references that can't be resolved yet.
 */

/* Return the storage size in bytes for a builtin Pascal type name.
 * Used to decide whether a type alias override widens or narrows. */
static long long semcheck_builtin_type_size(const char *name)
{
    if (name == NULL) return 0;
    if (pascal_identifier_equals(name, "LongInt") ||
        pascal_identifier_equals(name, "LongWord") ||
        pascal_identifier_equals(name, "DWord") ||
        pascal_identifier_equals(name, "Cardinal") ||
        pascal_identifier_equals(name, "Single"))
        return 4;
    if (pascal_identifier_equals(name, "SmallInt") ||
        pascal_identifier_equals(name, "Word") ||
        pascal_identifier_equals(name, "WideChar"))
        return 2;
    if (pascal_identifier_equals(name, "Byte") ||
        pascal_identifier_equals(name, "ShortInt") ||
        pascal_identifier_equals(name, "Boolean") ||
        pascal_identifier_equals(name, "Char") ||
        pascal_identifier_equals(name, "AnsiChar"))
        return 1;
    if (pascal_identifier_equals(name, "Int64") ||
        pascal_identifier_equals(name, "QWord") ||
        pascal_identifier_equals(name, "Double") ||
        pascal_identifier_equals(name, "Real"))
        return 8;
    return 0;
}

static int predeclare_types(SymTab_t *symtab, ListNode_t *type_decls)
{
    if (symtab == NULL)
        return 0;

    int errors = 0;
    ListNode_t *cur = type_decls;
    while (cur != NULL)
    {
        if (cur->type == LIST_TREE && cur->cur != NULL)
        {
            Tree_t *tree = (Tree_t *)cur->cur;

            if (tree->type == TREE_TYPE_DECL)
            {
                ScopeNode *saved_scope_for_type = semcheck_switch_to_unit_scope(
                    symtab, tree->tree_data.type_decl_data.source_unit_index);
                const char *type_id = tree->tree_data.type_decl_data.id;

                /* Skip if no type id */
                if (type_id == NULL)
                {
                    goto predeclare_types_next;
                }
                
                /* Debug: print predeclare order */
                if (kgpc_getenv("KGPC_DEBUG_PREDECLARE") != NULL)
                {
                    fprintf(stderr, "[predeclare] type '%s' unit=%d",
                        type_id, tree->tree_data.type_decl_data.defined_in_unit);
                    if (tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS)
                    {
                        struct TypeAlias *a = &tree->tree_data.type_decl_data.info.alias;
                        fprintf(stderr, " alias target='%s'", a->target_type_id ? a->target_type_id : "<null>");
                    }
                    fprintf(stderr, "\n");
                }
                
                /* Check if already declared (e.g., from a previous pass or builtin) */
                HashNode_t *existing = FindIdentInCurrentScope(symtab, type_id);
                int found = (existing != NULL);
                if (kgpc_getenv("KGPC_DEBUG_TFLOAT") != NULL && type_id != NULL &&
                    pascal_identifier_equals(type_id, "TFloatFormatProfile"))
                {
                    fprintf(stderr, "[TFLOAT] predeclare existing found=%d node=%p kind=%d\n",
                        found, (void *)existing,
                        tree->tree_data.type_decl_data.kind);
                }
                if (kgpc_getenv("KGPC_DEBUG_TSIZE") != NULL &&
                    pascal_identifier_equals(type_id, "TSize"))
                {
                    fprintf(stderr, "[TSIZE-FLOW] tree: defined_in_unit=%d src_unit=%d kind=%d | existing=%p found=%d",
                        tree->tree_data.type_decl_data.defined_in_unit,
                        tree->tree_data.type_decl_data.source_unit_index,
                        tree->tree_data.type_decl_data.kind,
                        (void *)existing, found);
                    if (existing != NULL)
                        fprintf(stderr, " ex_defined_in_unit=%d ex_src_unit=%d ex_has_record=%d",
                            existing->defined_in_unit, existing->source_unit_index,
                            get_record_type_from_node(existing) != NULL);
                    fprintf(stderr, "\n");
                }
                if (existing != NULL)
                {
                    /* If the existing symbol is a non-record alias and we are now defining
                     * a record with the same name, replace the alias with the record.
                     * This is required for units that import a type alias (e.g. TSize = LongInt)
                     * and then define their own record TSize with methods.
                     * However, if they come from different units, push as a new entry
                     * so per-unit type resolution can pick the right one. */
                    int cross_unit_replace = 0;
                    if (tree->tree_data.type_decl_data.source_unit_index != 0 &&
                        existing->source_unit_index != 0 &&
                        tree->tree_data.type_decl_data.source_unit_index != existing->source_unit_index)
                        cross_unit_replace = 1;
                    /* A local type (source_unit_index=0) shadowing a unit type
                     * must also be pushed as a separate entry, not replaced
                     * in-place, so unit functions still see their original type. */
                    if (tree->tree_data.type_decl_data.source_unit_index == 0 &&
                        existing->source_unit_index != 0)
                        cross_unit_replace = 1;
                    if (tree->tree_data.type_decl_data.kind == TYPE_DECL_RECORD &&
                        existing->hash_type == HASHTYPE_TYPE &&
                        get_record_type_from_node(existing) == NULL &&
                        !cross_unit_replace)
                    {
                        struct RecordType *record_info = tree->tree_data.type_decl_data.info.record;
                        if (record_info != NULL)
                        {
                            if (record_info->type_id == NULL)
                                record_info->type_id = strdup(type_id);

                            KgpcType *kgpc_type = create_record_type(record_info);
                            if (record_type_is_class(record_info))
                            {
                                KgpcType *ptr = create_pointer_type(kgpc_type);
                                kgpc_type_release(kgpc_type);
                                kgpc_type = ptr;
                            }

                            if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                            {
                                tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                                kgpc_type_retain(kgpc_type);
                            }

                            if (existing->type != NULL)
                                destroy_kgpc_type(existing->type);
                            if (kgpc_type != NULL)
                                kgpc_type_retain(kgpc_type);
                            existing->type = kgpc_type;
                            mark_hashnode_unit_info(symtab, existing,
                                tree->tree_data.type_decl_data.defined_in_unit,
                                tree->tree_data.type_decl_data.unit_is_public);
                            mark_hashnode_source_unit(existing, tree->tree_data.type_decl_data.source_unit_index);
                            /* Release creator's reference */
                            destroy_kgpc_type(kgpc_type);
                        }
                        goto predeclare_types_next;
                    }

                    /* If we already have a type alias without range/enum metadata and are now
                     * declaring a range/enum, replace the alias so Low/High work in consts.
                     * This is needed for circular unit references like Math -> Types. */
                    if (tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS &&
                        existing->hash_type == HASHTYPE_TYPE)
                    {
                        struct TypeAlias *alias = &tree->tree_data.type_decl_data.info.alias;
                        struct TypeAlias *existing_alias = get_type_alias_from_node(existing);
                        int new_has_range = (alias->is_range || alias->range_known);
                        int existing_has_range = (existing_alias != NULL && existing_alias->range_known);
                        int new_has_enum = (alias->is_enum && alias->enum_literals != NULL);
                        int existing_has_enum =
                            (existing_alias != NULL && existing_alias->is_enum &&
                             existing_alias->enum_literals != NULL);

                        if ((new_has_range && !existing_has_range) ||
                            (new_has_enum && !existing_has_enum))
                        {
                            KgpcType *kgpc_type = NULL;
                            if (new_has_enum)
                            {
                                kgpc_type = create_primitive_type(ENUM_TYPE);
                            }
                            else
                            {
                                int base_tag = alias->base_type != UNKNOWN_TYPE ? alias->base_type : INT_TYPE;
                                if (alias->storage_size > 0)
                                    kgpc_type = create_primitive_type_with_size(base_tag, (int)alias->storage_size);
                                else
                                    kgpc_type = create_primitive_type(base_tag);
                            }

                            if (kgpc_type != NULL)
                            {
                                kgpc_type_set_type_alias(kgpc_type, alias);
                                if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                                {
                                    tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                                    kgpc_type_retain(kgpc_type);
                                }
                                if (existing->type != NULL)
                                    destroy_kgpc_type(existing->type);
                                kgpc_type_retain(kgpc_type);
                                existing->type = kgpc_type;
                                mark_hashnode_unit_info(symtab, existing,
                                    tree->tree_data.type_decl_data.defined_in_unit,
                                    tree->tree_data.type_decl_data.unit_is_public);
                                mark_hashnode_source_unit(existing, tree->tree_data.type_decl_data.source_unit_index);
                                /* Release creator's reference */
                                destroy_kgpc_type(kgpc_type);
                            }
                            goto predeclare_types_next;
                        }

                        /* If we already have a non-pointer alias and the new declaration
                         * is a pointer alias (e.g., punicodemap = ^tunicodemap replacing
                         * a predeclared punicodemap = Pointer stub), upgrade the existing
                         * entry to TYPE_KIND_POINTER so pointer dereferences work. */
                        struct TypeAlias *new_alias_ptr = &tree->tree_data.type_decl_data.info.alias;
                        struct TypeAlias *existing_alias_ptr = get_type_alias_from_node(existing);
                        int new_is_pointer = (new_alias_ptr != NULL && new_alias_ptr->is_pointer);
                        int existing_is_pointer = (existing_alias_ptr != NULL && existing_alias_ptr->is_pointer);
                        if (new_is_pointer && !existing_is_pointer)
                        {
                            KgpcType *kgpc_type = create_kgpc_type_from_type_alias(
                                new_alias_ptr, symtab,
                                tree->tree_data.type_decl_data.defined_in_unit);
                            if (kgpc_type == NULL)
                            {
                                /* Fallback: create a basic pointer type */
                                kgpc_type = create_pointer_type(NULL);
                                kgpc_type_set_type_alias(kgpc_type, new_alias_ptr);
                            }
                            if (kgpc_type != NULL)
                            {
                                if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                                {
                                    tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                                    kgpc_type_retain(kgpc_type);
                                }
                                if (existing->type != NULL)
                                    destroy_kgpc_type(existing->type);
                                kgpc_type_retain(kgpc_type);
                                existing->type = kgpc_type;
                                mark_hashnode_unit_info(symtab, existing,
                                    tree->tree_data.type_decl_data.defined_in_unit,
                                    tree->tree_data.type_decl_data.unit_is_public);
                                mark_hashnode_source_unit(existing, tree->tree_data.type_decl_data.source_unit_index);
                                destroy_kgpc_type(kgpc_type);
                            }
                            goto predeclare_types_next;
                        }
                    }

                    /* Allow record types from different units to coexist as separate entries.
                     * This handles cases like System.tsiginfo vs SysUtils.tsiginfo and
                     * UnixType.TSize (alias) vs Types.TSize (record).
                     * The ranking logic picks the right one based on same-unit preference. */
                    int existing_is_record = (existing->type != NULL &&
                        existing->type->kind == TYPE_KIND_RECORD);
                    struct TypeAlias *existing_alias = get_type_alias_from_node(existing);
                    int current_is_enum_alias = (
                        tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS &&
                        tree->tree_data.type_decl_data.info.alias.is_enum);
                    int existing_is_enum_alias = (
                        existing_alias != NULL &&
                        existing_alias->is_enum);
                    int cross_unit_coexist = (
                        (tree->tree_data.type_decl_data.kind == TYPE_DECL_RECORD ||
                         (tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS &&
                            (existing_is_record || current_is_enum_alias || existing_is_enum_alias))) &&
                        existing->source_unit_index != 0 &&
                        tree->tree_data.type_decl_data.source_unit_index != 0 &&
                        existing->source_unit_index != tree->tree_data.type_decl_data.source_unit_index);
                    if (!(existing->defined_in_unit &&
                          !tree->tree_data.type_decl_data.defined_in_unit) &&
                        !cross_unit_coexist)
                    {
                        /* Check if this is a forward class declaration being completed.
                         * Forward: "TObject = class;" creates empty class, then full definition follows.
                         * Allow the full definition to replace the forward declaration. */
                        int is_forward_class_completion = 0;
                        if (tree->tree_data.type_decl_data.kind == TYPE_DECL_RECORD)
                        {
                            struct RecordType *new_record = tree->tree_data.type_decl_data.info.record;
                            struct RecordType *existing_record = get_record_type_from_node(existing);
                            if (new_record != NULL && new_record->is_class &&
                                existing_record != NULL && existing_record->is_class)
                            {
                                /* Check if existing has only hidden fields (forward decl) */
                                int has_non_hidden = 0;
                                ListNode_t *fnode = existing_record->fields;
                                while (fnode != NULL)
                                {
                                    if (fnode->type == LIST_RECORD_FIELD && fnode->cur != NULL)
                                    {
                                        struct RecordField *f = (struct RecordField *)fnode->cur;
                                        if (!record_field_is_hidden(f))
                                        {
                                            has_non_hidden = 1;
                                            break;
                                        }
                                    }
                                    fnode = fnode->next;
                                }
                                /* The full definition may have fields, properties,
                                 * method templates, or a parent class – any of these
                                 * indicate a real class body replacing the forward stub. */
                                int new_has_body = (new_record->fields != NULL ||
                                    new_record->properties != NULL ||
                                    new_record->method_templates != NULL ||
                                    new_record->parent_class_name != NULL);
                                if (!has_non_hidden && new_has_body)
                                    is_forward_class_completion = 1;
                            }
                            /* Also handle forward interface declarations:
                             * "IMyIntf = interface;" followed by the full definition. */
                            if (new_record != NULL && new_record->is_interface &&
                                existing_record != NULL && existing_record->is_interface)
                            {
                                int existing_has_body = (existing_record->fields != NULL ||
                                    existing_record->properties != NULL ||
                                    existing_record->method_templates != NULL);
                                int new_has_body = (new_record->fields != NULL ||
                                    new_record->properties != NULL ||
                                    new_record->method_templates != NULL ||
                                    new_record->parent_class_name != NULL);
                                if (!existing_has_body && new_has_body)
                                    is_forward_class_completion = 1;
                            }
                        }
                        /* Allow a simple type alias from a unit to override an
                         * existing alias when the target type differs AND both
                         * are plain (non-array, non-pointer) aliases.  This is
                         * required for objpas.pp which redefines
                         *   Integer = LongInt   (overriding system's Integer = SmallInt)
                         * when {$mode objfpc} or {$mode delphi} is active. */
                        int is_alias_override = 0;
                        if (tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS &&
                            existing->hash_type == HASHTYPE_TYPE &&
                            tree->tree_data.type_decl_data.defined_in_unit)
                        {
                            struct TypeAlias *new_alias = &tree->tree_data.type_decl_data.info.alias;
                            struct TypeAlias *old_alias = get_type_alias_from_node(existing);
                            if (new_alias != NULL && old_alias != NULL &&
                                new_alias->target_type_id != NULL &&
                                old_alias->target_type_id != NULL &&
                                !new_alias->is_array && !new_alias->is_pointer &&
                                !new_alias->is_set && !new_alias->is_file &&
                                !new_alias->inline_record_type &&
                                /* Also require the OLD alias to not be a pointer/array/set/file.
                                 * This prevents objpas PString=PAnsiString from overriding
                                 * system pstring=^shortstring (which has is_pointer=1). */
                                !old_alias->is_array && !old_alias->is_pointer &&
                                !old_alias->is_set && !old_alias->is_file &&
                                !old_alias->inline_record_type &&
                                !pascal_identifier_equals(new_alias->target_type_id, old_alias->target_type_id))
                            {
                                /* Only allow override for the specific case where
                                 * objpas.pp redefines Integer=LongInt over system's
                                 * Integer=SmallInt.  We also handle MaxInt redefinition.
                                 * Other aliases must not be overridden to avoid breaking
                                 * NativeInt, TDateTime, PString, etc. */
                                if (pascal_identifier_equals(tree->tree_data.type_decl_data.id, "Integer") ||
                                    pascal_identifier_equals(tree->tree_data.type_decl_data.id, "PInteger") ||
                                    pascal_identifier_equals(tree->tree_data.type_decl_data.id, "SizeInt") ||
                                    pascal_identifier_equals(tree->tree_data.type_decl_data.id, "PtrInt") ||
                                    pascal_identifier_equals(tree->tree_data.type_decl_data.id, "CodePtrInt") ||
                                    pascal_identifier_equals(tree->tree_data.type_decl_data.id, "ValSInt") ||
                                    pascal_identifier_equals(tree->tree_data.type_decl_data.id, "ValUInt"))
                                {
                                    /* Only allow widening overrides (e.g. Integer=LongInt
                                     * over Integer=SmallInt from objpas over system).
                                     * Prevent system's Integer=SmallInt from overriding
                                     * objpas's Integer=LongInt when the merged type
                                     * declarations are processed in an order where
                                     * system comes after objpas. */
                                    long long new_size = new_alias->storage_size;
                                    long long old_size = old_alias->storage_size;
                                    if (new_size <= 0 && new_alias->target_type_id != NULL)
                                        new_size = semcheck_builtin_type_size(new_alias->target_type_id);
                                    if (old_size <= 0 && old_alias->target_type_id != NULL)
                                        old_size = semcheck_builtin_type_size(old_alias->target_type_id);
                                    /* Allow override only if new type is at least as wide,
                                     * or if sizes can't be determined (allow by default). */
                                    if (new_size >= old_size || new_size <= 0 || old_size <= 0)
                                        is_alias_override = 1;
                                }
                            }
                        }
                        if (!is_forward_class_completion && !is_alias_override)
                        {
                            /* Already declared, skip */
                            goto predeclare_types_next;
                        }
                        if (is_alias_override)
                        {
                            /* Replace the existing type with the new alias */
                            struct TypeAlias *alias = &tree->tree_data.type_decl_data.info.alias;
                            if (alias->target_type_id != NULL)
                                apply_builtin_integer_alias_metadata(alias, alias->target_type_id);
                            int base_tag = alias->base_type != UNKNOWN_TYPE ? alias->base_type : INT_TYPE;
                            KgpcType *kgpc_type = NULL;
                            if (alias->storage_size > 0)
                                kgpc_type = create_primitive_type_with_size(base_tag, (int)alias->storage_size);
                            else
                                kgpc_type = create_primitive_type(base_tag);
                            if (kgpc_type != NULL)
                            {
                                kgpc_type_set_type_alias(kgpc_type, alias);
                                if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                                {
                                    tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                                    kgpc_type_retain(kgpc_type);
                                }
                                if (existing->type != NULL)
                                    destroy_kgpc_type(existing->type);
                                kgpc_type_retain(kgpc_type);
                                existing->type = kgpc_type;
                                mark_hashnode_unit_info(symtab, existing,
                                    tree->tree_data.type_decl_data.defined_in_unit,
                                    tree->tree_data.type_decl_data.unit_is_public);
                                mark_hashnode_source_unit(existing, tree->tree_data.type_decl_data.source_unit_index);
                                /* Release creator's reference */
                                destroy_kgpc_type(kgpc_type);
                            }
                            goto predeclare_types_next;
                        }
                        else
                        {
                            /* Forward class completion: update existing record in-place.
                             * We must update in-place because existing code (e.g. Self
                             * parameters in method implementations) already holds pointers
                             * to the forward declaration's RecordType. Shadowing would not
                             * update those existing references. */
                            struct RecordType *new_record = tree->tree_data.type_decl_data.info.record;
                            struct RecordType *existing_record = get_record_type_from_node(existing);
                            if (existing_record != NULL && new_record != NULL &&
                                existing_record != new_record)
                            {
                                /* Move new fields into existing record, keeping hidden fields at front */
                                ListNode_t *old_fields = existing_record->fields;
                                existing_record->fields = new_record->fields;
                                new_record->fields = NULL; /* Transfer ownership to existing */
                                /* Mark the depleted record so merge_parent_class_fields
                                 * won't re-merge parent fields into this empty shell. */
                                new_record->parent_fields_merged = 1;
                                /* Re-add hidden fields from forward decl at front,
                                 * but ONLY if the full declaration has no parent class.
                                 * When a parent exists, the hidden fields (e.g.
                                 * __kgpc_class_typeinfo) will be inherited from the
                                 * parent during merge_parent_class_fields, so re-adding
                                 * them here would create duplicates. */
                                if (new_record->parent_class_name == NULL)
                                {
                                    while (old_fields != NULL)
                                    {
                                        ListNode_t *next = old_fields->next;
                                        if (old_fields->type == LIST_RECORD_FIELD && old_fields->cur != NULL)
                                        {
                                            struct RecordField *f = (struct RecordField *)old_fields->cur;
                                            if (record_field_is_hidden(f))
                                            {
                                                /* Skip if transferred fields already have this hidden field
                                                 * (both forward and full root-class decls get __kgpc_class_typeinfo
                                                 * from from_cparser.c, so re-adding would create duplicates). */
                                                int already_has = 0;
                                                if (f->name != NULL) {
                                                    ListNode_t *chk = existing_record->fields;
                                                    while (chk != NULL) {
                                                        if (chk->type == LIST_RECORD_FIELD && chk->cur != NULL) {
                                                            struct RecordField *ef = (struct RecordField *)chk->cur;
                                                            if (record_field_is_hidden(ef) && ef->name != NULL &&
                                                                strcmp(ef->name, f->name) == 0) {
                                                                already_has = 1;
                                                                break;
                                                            }
                                                        }
                                                        chk = chk->next;
                                                    }
                                                }
                                                if (!already_has)
                                                {
                                                    old_fields->next = NULL;
                                                    if (existing_record->fields != NULL)
                                                        existing_record->fields = PushListNodeFront(existing_record->fields, old_fields);
                                                    else
                                                        existing_record->fields = old_fields;
                                                }
                                            }
                                        }
                                        old_fields = next;
                                    }
                                }
                                /* Transfer method templates, properties, parent class, interfaces */
                                if (new_record->method_templates != NULL)
                                {
                                    existing_record->method_templates = new_record->method_templates;
                                    new_record->method_templates = NULL;
                                }
                                if (new_record->properties != NULL)
                                {
                                    existing_record->properties = new_record->properties;
                                    new_record->properties = NULL;
                                }
                                if (new_record->parent_class_name != NULL &&
                                    existing_record != new_record &&
                                    existing_record->parent_class_name != new_record->parent_class_name)
                                {
                                    char *new_parent_copy = strdup(new_record->parent_class_name);
                                    free(existing_record->parent_class_name);
                                    existing_record->parent_class_name = new_parent_copy;
                                }
                                if (new_record->interface_names != NULL &&
                                    existing_record != new_record)
                                {
                                    existing_record->interface_names = new_record->interface_names;
                                    existing_record->num_interfaces = new_record->num_interfaces;
                                    new_record->interface_names = NULL;
                                    new_record->num_interfaces = 0;
                                }
                                /* Store kgpc_type in tree for later reuse */
                                if (tree->tree_data.type_decl_data.kgpc_type == NULL && existing->type != NULL)
                                {
                                    tree->tree_data.type_decl_data.kgpc_type = existing->type;
                                    kgpc_type_retain(existing->type);
                                }
                                mark_hashnode_unit_info(symtab, existing,
                                    tree->tree_data.type_decl_data.defined_in_unit,
                                    tree->tree_data.type_decl_data.unit_is_public);
                                mark_hashnode_source_unit(existing, tree->tree_data.type_decl_data.source_unit_index);
                                semcheck_mark_resolved_forward_stub(type_decls, cur,
                                    type_id,
                                    tree->tree_data.type_decl_data.source_unit_index,
                                    existing_record);
                            }
                            goto predeclare_types_next;
                        }
                    }
                }

                /* Predeclare record types so they can be referenced (e.g., as function returns) */
                if (tree->tree_data.type_decl_data.kind == TYPE_DECL_RECORD)
                {
                    struct RecordType *record_info = tree->tree_data.type_decl_data.info.record;

                    /* Annotate the record with its canonical name if missing */
                    if (record_info != NULL && record_info->type_id == NULL)
                        record_info->type_id = strdup(type_id);
                    
                    KgpcType *kgpc_type = create_record_type(record_info);
                    if (record_type_is_class(record_info))
                    {
                        /* Classes are reference types - register as pointers to the record */
                        KgpcType *ptr = create_pointer_type(kgpc_type);
                        kgpc_type_release(kgpc_type); /* create_pointer_type retained inner */
                        kgpc_type = ptr;
                    }
                    if (kgpc_type != NULL)
                    {
                        /* Store in tree for later reuse by semcheck_type_decls */
                        if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                        {
                            tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                            kgpc_type_retain(kgpc_type);
                        }

                        int result = PushTypeOntoScope_Typed(symtab, (char *)type_id, kgpc_type);
                        if (kgpc_getenv("KGPC_DEBUG_TSIZE") != NULL &&
                            pascal_identifier_equals(type_id, "TSize"))
                        {
                            fprintf(stderr, "[TSIZE-PUSH] push result=%d tree_defined_in_unit=%d tree_src_unit=%d\n",
                                result, tree->tree_data.type_decl_data.defined_in_unit,
                                tree->tree_data.type_decl_data.source_unit_index);
                        }
                        if (kgpc_getenv("KGPC_DEBUG_FORWARD_CLASS") != NULL && record_info != NULL && record_info->is_class)
                        {
                            fprintf(stderr, "[FWD-PRE] type='%s' push_result=%d fields=%p\n",
                                type_id, result, (void *)record_info->fields);
                            ListNode_t *dbg = record_info->fields;
                            while (dbg != NULL)
                            {
                                if (dbg->type == LIST_RECORD_FIELD && dbg->cur != NULL)
                                {
                                    struct RecordField *f = (struct RecordField *)dbg->cur;
                                    fprintf(stderr, "[FWD-PRE]   field: %s hidden=%d\n",
                                        f->name ? f->name : "<null>", record_field_is_hidden(f));
                                }
                                dbg = dbg->next;
                            }
                        }
                        if (result > 0)
                        {
                            /* Check if this is a forward class declaration being resolved.
                             * Forward class: "TObject = class;" creates an empty class,
                             * then "TObject = class ... end;" provides the full definition.
                             * Update the existing symbol's type with the full definition. */
                            if (record_info != NULL && record_info->is_class)
                            {
                                HashNode_t *existing = NULL;
                                int find_result = FindSymbol(&existing, symtab, type_id);
                                if (kgpc_getenv("KGPC_DEBUG_FORWARD_CLASS") != NULL)
                                    fprintf(stderr, "[FWD] type='%s' find=%d existing=%p hash_type=%d\n",
                                        type_id, find_result, (void*)existing,
                                        existing ? existing->hash_type : -1);
                                if (find_result &&
                                    existing != NULL &&
                                    existing->hash_type == HASHTYPE_TYPE &&
                                    existing->type != NULL)
                                {
                                    struct RecordType *existing_record = get_record_type_from_node(existing);
                                    if (existing_record != NULL && existing_record->is_class)
                                    {
                                        /* Check if existing is a forward declaration (only hidden fields) */
                                        int has_non_hidden_fields = 0;
                                        ListNode_t *fnode = existing_record->fields;
                                        while (fnode != NULL)
                                        {
                                            if (fnode->type == LIST_RECORD_FIELD && fnode->cur != NULL)
                                            {
                                                struct RecordField *f = (struct RecordField *)fnode->cur;
                                                if (!record_field_is_hidden(f))
                                                {
                                                    has_non_hidden_fields = 1;
                                                    break;
                                                }
                                            }
                                            fnode = fnode->next;
                                        }
                                        if (!has_non_hidden_fields && record_info->fields != NULL)
                                        {
                                            /* Forward declaration being resolved - update existing record's fields */
                                            /* Prepend the hidden typeinfo field from existing to new record's fields */
                                            ListNode_t *existing_fields = existing_record->fields;
                                            existing_record->fields = record_info->fields;
                                            /* Re-add hidden fields at front, skipping duplicates */
                                            while (existing_fields != NULL)
                                            {
                                                ListNode_t *next = existing_fields->next;
                                                if (existing_fields->type == LIST_RECORD_FIELD && existing_fields->cur != NULL)
                                                {
                                                    struct RecordField *f = (struct RecordField *)existing_fields->cur;
                                                    if (record_field_is_hidden(f))
                                                    {
                                                        int already_has = 0;
                                                        if (f->name != NULL) {
                                                            ListNode_t *chk = existing_record->fields;
                                                            while (chk != NULL) {
                                                                if (chk->type == LIST_RECORD_FIELD && chk->cur != NULL) {
                                                                    struct RecordField *ef = (struct RecordField *)chk->cur;
                                                                    if (record_field_is_hidden(ef) && ef->name != NULL &&
                                                                        strcmp(ef->name, f->name) == 0) {
                                                                        already_has = 1;
                                                                        break;
                                                                    }
                                                                }
                                                                chk = chk->next;
                                                            }
                                                        }
                                                        if (!already_has)
                                                        {
                                                            existing_fields->next = NULL;
                                                            existing_record->fields = PushListNodeFront(existing_record->fields, existing_fields);
                                                        }
                                                    }
                                                    else
                                                    {
                                                        /* Non-hidden field from forward decl - shouldn't happen, just free */
                                                        free(existing_fields);
                                                    }
                                                }
                                                else
                                                {
                                                    free(existing_fields);
                                                }
                                                existing_fields = next;
                                            }
                                            /* Copy over method templates and properties */
                                            if (record_info->method_templates != NULL)
                                                existing_record->method_templates = record_info->method_templates;
                                            if (record_info->properties != NULL)
                                                existing_record->properties = record_info->properties;
                                            if (record_info->parent_class_name != NULL && existing_record->parent_class_name == NULL)
                                                existing_record->parent_class_name = strdup(record_info->parent_class_name);
                                            if (record_info->interface_names != NULL)
                                            {
                                                existing_record->interface_names = record_info->interface_names;
                                                existing_record->num_interfaces = record_info->num_interfaces;
                                                record_info->interface_names = NULL;
                                                record_info->num_interfaces = 0;
                                            }
                                            semcheck_mark_resolved_forward_stub(type_decls, cur,
                                                type_id,
                                                tree->tree_data.type_decl_data.source_unit_index,
                                                existing_record);
                                            result = 0; /* Suppress the error */
                                        }
                                    }
                                }
                            }
                            if (result > 0)
                                errors += result;
                        }
                        else
                        {
                            /* Use FindIdent to get the most recently pushed entry (front of list).
                             * We can't use semcheck_find_type_node_with_unit_flag here because
                             * the newly pushed entry hasn't been marked yet (defined_in_unit=0),
                             * so that function would skip it and find an older entry instead. */
                            HashNode_t *type_node = NULL;
                            FindSymbol(&type_node, symtab, type_id);
                            if (type_node != NULL && type_node->hash_type == HASHTYPE_TYPE)
                            {
                                mark_hashnode_unit_info(symtab, type_node,
                                    tree->tree_data.type_decl_data.defined_in_unit,
                                    tree->tree_data.type_decl_data.unit_is_public);
                                mark_hashnode_source_unit(type_node, tree->tree_data.type_decl_data.source_unit_index);
                            }
                        }
                        /* Release creator's reference - tree and hash table have their own */
                        destroy_kgpc_type(kgpc_type);
                    }
                }
                /* Only handle TYPE_DECL_ALIAS cases we can resolve early */
                else if (tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS)
                {
                    struct TypeAlias *alias = &tree->tree_data.type_decl_data.info.alias;
                    alias->is_char_alias = semcheck_alias_should_be_char_like(type_id,
                        alias->target_type_id) &&
                        !alias->is_pointer && !alias->is_array && !alias->is_set &&
                        !alias->is_enum && !alias->is_file;

                    /* WideChar/UnicodeChar must be predeclared as CHAR_TYPE before
                     * apply_builtin_integer_alias_metadata converts them to WORD_TYPE. */
                    if (alias->is_char_alias && type_id != NULL &&
                        (pascal_identifier_equals(type_id, "WideChar") ||
                         pascal_identifier_equals(type_id, "UnicodeChar")))
                    {
                        if (alias->alias_name == NULL)
                            alias->alias_name = strdup(type_id);
                        alias->storage_size = 2;
                        KgpcType *kgpc_type = create_primitive_type_with_size(CHAR_TYPE, 2);
                        if (kgpc_type != NULL)
                        {
                            kgpc_type_set_type_alias(kgpc_type, alias);
                            if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                            {
                                tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                                kgpc_type_retain(kgpc_type);
                            }
                            int result = PushTypeOntoScope_Typed(symtab, (char *)type_id, kgpc_type);
                            if (result > 0)
                                errors += result;
                            else
                            {
                                mark_latest_type_node_unit_info(symtab, type_id,
                                    tree->tree_data.type_decl_data.defined_in_unit,
                                    tree->tree_data.type_decl_data.unit_is_public,
                                    tree->tree_data.type_decl_data.source_unit_index);
                            }
                            /* Release creator's reference */
                            destroy_kgpc_type(kgpc_type);
                        }
                        goto predeclare_types_next;
                    }

                    /* Do NOT apply integer alias metadata to pointer types.
                     * For PWord = ^Word, target_type_id is "Word" (the pointee),
                     * and applying Word's metadata (is_range, storage_size=2) would
                     * corrupt the pointer alias, causing it to be treated as a
                     * 2-byte range type instead of an 8-byte pointer. */
                    if (!alias->is_pointer)
                    {
                        if (alias->target_type_id != NULL)
                            apply_builtin_integer_alias_metadata(alias, alias->target_type_id);
                        else if (type_id != NULL)
                            apply_builtin_integer_alias_metadata(alias, type_id);
                    }

                    /* Predeclare range aliases early so Low/High on subranges work in consts. */
                    if (alias->is_range || alias->range_known)
                    {
                        int base_tag = alias->base_type != UNKNOWN_TYPE ? alias->base_type : INT_TYPE;
                        KgpcType *kgpc_type = NULL;
                        if (alias->storage_size > 0)
                            kgpc_type = create_primitive_type_with_size(base_tag, (int)alias->storage_size);
                        else
                            kgpc_type = create_primitive_type(base_tag);
                        if (kgpc_type != NULL)
                        {
                            kgpc_type_set_type_alias(kgpc_type, alias);
                            if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                            {
                                tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                                kgpc_type_retain(kgpc_type);
                            }
                            int result = PushTypeOntoScope_Typed(symtab, (char *)type_id, kgpc_type);
                            if (result > 0)
                            {
                                /* Push failed (duplicate).  Allow a unit-level alias to
                                 * replace the existing entry when it is one of the known
                                 * overridable types (e.g. objpas redefining Integer=LongInt
                                 * over system's Integer=SmallInt). */
                                if (tree->tree_data.type_decl_data.defined_in_unit &&
                                    (pascal_identifier_equals(type_id, "Integer") ||
                                     pascal_identifier_equals(type_id, "PInteger") ||
                                     pascal_identifier_equals(type_id, "SizeInt") ||
                                     pascal_identifier_equals(type_id, "PtrInt") ||
                                     pascal_identifier_equals(type_id, "CodePtrInt") ||
                                     pascal_identifier_equals(type_id, "ValSInt") ||
                                     pascal_identifier_equals(type_id, "ValUInt")))
                                {
                                    HashNode_t *existing_node = NULL;
                                    if (FindSymbol(&existing_node, symtab, type_id) != 0 &&
                                        existing_node != NULL &&
                                        existing_node->hash_type == HASHTYPE_TYPE)
                                    {
                                        if (existing_node->type != NULL)
                                            destroy_kgpc_type(existing_node->type);
                                        kgpc_type_retain(kgpc_type);
                                        existing_node->type = kgpc_type;
                                        mark_hashnode_unit_info(symtab, existing_node,
                                            tree->tree_data.type_decl_data.defined_in_unit,
                                            tree->tree_data.type_decl_data.unit_is_public);
                                        mark_hashnode_source_unit(existing_node,
                                            tree->tree_data.type_decl_data.source_unit_index);
                                        result = 0;
                                    }
                                }
                                if (result > 0)
                                    errors += result;
                            }
                            else
                            {
                                mark_latest_type_node_unit_info(symtab, type_id,
                                    tree->tree_data.type_decl_data.defined_in_unit,
                                    tree->tree_data.type_decl_data.unit_is_public,
                                    tree->tree_data.type_decl_data.source_unit_index);
                            }
                            /* Release creator's reference */
                            destroy_kgpc_type(kgpc_type);
                        }
                        goto predeclare_types_next;
                    }

                    /* Handle inline record aliases (e.g., generic specializations) */
                    if (alias->inline_record_type != NULL)
                    {
                        if (alias->inline_record_type->type_id == NULL)
                            alias->inline_record_type->type_id = strdup(type_id);
                        const char *record_name = alias->inline_record_type->type_id;
                        HashNode_t *existing_inline = FindIdentInCurrentScope(symtab, record_name);
                        if (existing_inline == NULL)
                        {
                            KgpcType *inline_kgpc = create_record_type(alias->inline_record_type);
                            if (record_type_is_class(alias->inline_record_type))
                                inline_kgpc = create_pointer_type(inline_kgpc);

                            if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                            {
                                tree->tree_data.type_decl_data.kgpc_type = inline_kgpc;
                                kgpc_type_retain(inline_kgpc);
                            }

                            int result = PushTypeOntoScope_Typed(symtab, (char *)record_name, inline_kgpc);
                            if (result > 0)
                                errors += result;
                            else
                            {
                                mark_latest_type_node_unit_info(symtab, record_name,
                                    tree->tree_data.type_decl_data.defined_in_unit,
                                    tree->tree_data.type_decl_data.unit_is_public,
                                    tree->tree_data.type_decl_data.source_unit_index);
                            }
                            /* Release creator's reference */
                            destroy_kgpc_type(inline_kgpc);
                        }

                        /* Also register the alias name itself */
                        KgpcType *alias_kgpc = create_record_type(alias->inline_record_type);
                        if (record_type_is_class(alias->inline_record_type))
                            alias_kgpc = create_pointer_type(alias_kgpc);
                        kgpc_type_set_type_alias(alias_kgpc, alias);
                        int alias_result = PushTypeOntoScope_Typed(symtab, (char *)type_id, alias_kgpc);
                        if (alias_result > 0)
                            errors += alias_result;
                        else
                        {
                            mark_latest_type_node_unit_info(symtab, type_id,
                                tree->tree_data.type_decl_data.defined_in_unit,
                                tree->tree_data.type_decl_data.unit_is_public,
                                tree->tree_data.type_decl_data.source_unit_index);
                        }
                        /* Release creator's reference */
                        destroy_kgpc_type(alias_kgpc);

                        goto predeclare_types_next;
                    }

                    /* Pre-declare enum types so they can be used as function return types.
                     * This creates a stub entry - the actual enum literals are registered
                     * separately in predeclare_enum_literals(). */
                    if (alias->is_enum)
                    {
                        KgpcType *kgpc_type = NULL;
                        /* Check if already created during predeclare_enum_literals */
                        if (alias->kgpc_type != NULL)
                        {
                            kgpc_type = alias->kgpc_type;
                            kgpc_type_retain(kgpc_type);  /* Own a reference so we can release uniformly */
                        }
                        else
                        {
                            kgpc_type = create_primitive_type(ENUM_TYPE);
                            if (kgpc_type != NULL)
                            {
                                /* Store in alias - alias now owns this reference */
                                alias->kgpc_type = kgpc_type;
                                kgpc_type_retain(kgpc_type);  /* Alias holds reference */
                            }
                        }
                        
                        if (kgpc_type != NULL)
                        {
                            /* Attach the type_alias so scoped enum lookup can find enum_literals */
                            kgpc_type_set_type_alias(kgpc_type, alias);
                            
                            if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                            {
                                tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                                kgpc_type_retain(kgpc_type);  /* Tree holds reference */
                            }
                            
                            /* PushTypeOntoScope_Typed will retain kgpc_type when adding to symbol table */
                            int result = PushTypeOntoScope_Typed(symtab, (char *)type_id, kgpc_type);
                            if (result > 0)
                                errors += result;
                            else
                            {
                                mark_latest_type_node_unit_info(symtab, type_id,
                                    tree->tree_data.type_decl_data.defined_in_unit,
                                    tree->tree_data.type_decl_data.unit_is_public,
                                    tree->tree_data.type_decl_data.source_unit_index);
                            }
                            /* Release creator's reference */
                            destroy_kgpc_type(kgpc_type);
                        }
                        goto predeclare_types_next;
                    }

                    /* Predeclare array/set/file aliases so return types can resolve early. */
                    if (alias->is_array || alias->is_set || alias->is_file)
                    {
                        KgpcType *kgpc_type = create_kgpc_type_from_type_alias(
                            alias, symtab, tree->tree_data.type_decl_data.defined_in_unit);
                        if (kgpc_type != NULL)
                        {
                            if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                            {
                                tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                                kgpc_type_retain(kgpc_type);
                            }
                            int result = PushTypeOntoScope_Typed(symtab, (char *)type_id, kgpc_type);
                            if (result > 0)
                                errors += result;
                            else
                            {
                                mark_latest_type_node_unit_info(symtab, type_id,
                                    tree->tree_data.type_decl_data.defined_in_unit,
                                    tree->tree_data.type_decl_data.unit_is_public,
                                    tree->tree_data.type_decl_data.source_unit_index);
                            }
                            /* Release creator's reference */
                            destroy_kgpc_type(kgpc_type);
                            goto predeclare_types_next;
                        }
                        goto predeclare_types_next;
                    }
                    
                    /* Handle pointer aliases to already known element types */
                    if (alias->is_pointer)
                    {
                        KgpcType *kgpc_type = create_kgpc_type_from_type_alias(
                            alias, symtab, tree->tree_data.type_decl_data.defined_in_unit);
                        if (kgpc_type != NULL)
                        {
                            if (kgpc_getenv("KGPC_DEBUG_PREDECLARE_POINTERS") != NULL)
                            {
                                fprintf(stderr, "[KGPC] predeclare pointer alias %s: ptr_type=%d ptr_id=%s kind=%d\n",
                                    type_id,
                                    alias->pointer_type,
                                    alias->pointer_type_id ? alias->pointer_type_id : "<null>",
                                    kgpc_type->kind);
                            }
                            if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                            {
                                tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                                kgpc_type_retain(kgpc_type);
                            }
                            
                            int result = PushTypeOntoScope_Typed(symtab, (char *)type_id, kgpc_type);
                            if (result > 0)
                                errors += result;
                            else
                            {
                                mark_latest_type_node_unit_info(symtab, type_id,
                                    tree->tree_data.type_decl_data.defined_in_unit,
                                    tree->tree_data.type_decl_data.unit_is_public,
                                    tree->tree_data.type_decl_data.source_unit_index);
                            }
                            /* Release creator's reference */
                            destroy_kgpc_type(kgpc_type);

                            goto predeclare_types_next;
                        }
                    }

                    /* Pre-declare simple alias types (e.g., TEndian = ObjPas.TEndian) so they
                     * are available for const expressions before full type checking. */
                    if (!alias->is_array && !alias->is_set && !alias->is_file &&
                        !alias->is_pointer && alias->base_type == UNKNOWN_TYPE)
                    {
                        KgpcType *kgpc_type = create_kgpc_type_from_type_alias(
                            alias, symtab, tree->tree_data.type_decl_data.defined_in_unit);
                        if (kgpc_type == NULL)
                        {
                            kgpc_type = create_primitive_type(UNKNOWN_TYPE);
                            if (kgpc_type != NULL)
                                kgpc_type_set_type_alias(kgpc_type, alias);
                        }
                        if (kgpc_type != NULL)
                        {
                            if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                            {
                                tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                                kgpc_type_retain(kgpc_type);
                            }
                            int result = PushTypeOntoScope_Typed(symtab, (char *)type_id, kgpc_type);
                            if (result > 0)
                            {
                                HashNode_t *existing_type = NULL;
                                if (FindSymbol(&existing_type, symtab, type_id) != 0 &&
                                    existing_type != NULL &&
                                    existing_type->hash_type == HASHTYPE_TYPE &&
                                    existing_type->type != NULL)
                                {
                                    struct TypeAlias *existing_alias =
                                        kgpc_type_get_type_alias(existing_type->type);
                                    int can_overlay_existing =
                                        (existing_alias != NULL) &&
                                        (alias->target_type_ref != NULL ||
                                         alias->target_type_id != NULL) &&
                                        !alias->is_pointer &&
                                        !alias->is_array &&
                                        !alias->is_set &&
                                        !alias->is_file &&
                                        existing_type->source_unit_index == 0 &&
                                        !existing_type->defined_in_unit;
                                    if (can_overlay_existing)
                                    {
                                        inherit_alias_metadata(symtab, alias);
                                        kgpc_type_set_type_alias(existing_type->type, alias);
                                        if (existing_type->type->type_alias != NULL &&
                                            alias->storage_size > 0)
                                        {
                                            existing_type->type->type_alias->storage_size =
                                                alias->storage_size;
                                        }
                                        result = 0;
                                    }
                                }
                                if (result > 0)
                                    errors += result;
                            }
                            else
                            {
                                mark_latest_type_node_unit_info(symtab, type_id,
                                    tree->tree_data.type_decl_data.defined_in_unit,
                                    tree->tree_data.type_decl_data.unit_is_public,
                                    tree->tree_data.type_decl_data.source_unit_index);
                            }
                            /* Release creator's reference */
                            destroy_kgpc_type(kgpc_type);
                            goto predeclare_types_next;
                        }
                    }

                    /* Pre-declare procedural type aliases so const initializers can use them */
                    if (alias->base_type == PROCEDURE ||
                        (tree->tree_data.type_decl_data.kgpc_type != NULL &&
                         tree->tree_data.type_decl_data.kgpc_type->kind == TYPE_KIND_PROCEDURE))
                    {
                        KgpcType *kgpc_type = tree->tree_data.type_decl_data.kgpc_type;
                        if (kgpc_type != NULL)
                        {
                            kgpc_type_set_type_alias(kgpc_type, alias);
                            if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                            {
                                tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                                kgpc_type_retain(kgpc_type);
                            }

                            int result = PushTypeOntoScope_Typed(symtab, (char *)type_id, kgpc_type);
                            if (result > 0)
                                errors += result;
                            else
                            {
                                mark_latest_type_node_unit_info(symtab, type_id,
                                    tree->tree_data.type_decl_data.defined_in_unit,
                                    tree->tree_data.type_decl_data.unit_is_public,
                                    tree->tree_data.type_decl_data.source_unit_index);
                            }
                        }
                        goto predeclare_types_next;
                    }

                    /* Only pre-declare simple primitive type aliases */
                    KgpcType *kgpc_type = NULL;
                    int created_new_type = 0;

                    /* WideChar/UnicodeChar should be treated as 2-byte CHAR_TYPE, even if aliased to Word. */
                    if (type_id != NULL &&
                        (pascal_identifier_equals(type_id, "WideChar") ||
                         pascal_identifier_equals(type_id, "UnicodeChar")))
                    {
                        kgpc_type = create_primitive_type_with_size(CHAR_TYPE, 2);
                        if (kgpc_type != NULL)
                            created_new_type = 1;
                    }
                    
                    /* Case 1: Direct primitive type tag (e.g., MyInt = Integer where base_type is set)
                     * Exclude PROCEDURE - procedure types are NOT primitive and need special handling.
                     * IMPORTANT: If target_type_id is "WideChar", skip to Case 2 to look it up
                     * from the symbol table where it has correct storage_size=2. Without this check,
                     * WideChar aliases would get 4 bytes (INT_TYPE) instead of 2 bytes. */
                    int skip_case1_for_widechar = (alias->target_type_id != NULL &&
                        pascal_identifier_equals(alias->target_type_id, "WideChar"));
                    if (kgpc_type == NULL && !skip_case1_for_widechar &&
                        alias->base_type != UNKNOWN_TYPE && alias->base_type != 0 &&
                        alias->base_type != PROCEDURE)
                    {
                        if (alias->storage_size > 0)
                            kgpc_type = create_primitive_type_with_size(alias->base_type,
                                (int)alias->storage_size);
                        else
                            kgpc_type = create_primitive_type(alias->base_type);
                        if (kgpc_type != NULL)
                            created_new_type = 1;
                    }
                    /* Case 2: Reference to a known primitive type name */
                    else if (kgpc_type == NULL && alias->target_type_id != NULL)
                    {
                        /* Check if target is a known builtin primitive type */
                        const TypeRef *target_ref = alias->target_type_ref;
                        const char *target = alias->target_type_id;
                        const char *target_base = (target_ref != NULL)
                            ? type_ref_base_name(target_ref)
                            : target;
                        if (pascal_identifier_equals(target_base, "Int64") ||
                            pascal_identifier_equals(target_base, "QWord") ||
                            pascal_identifier_equals(target_base, "UInt64") ||
                            pascal_identifier_equals(target_base, "SizeUInt") ||
                            pascal_identifier_equals(target_base, "SizeInt") ||
                            pascal_identifier_equals(target_base, "PtrUInt") ||
                            pascal_identifier_equals(target_base, "PtrInt") ||
                            pascal_identifier_equals(target_base, "NativeUInt") ||
                            pascal_identifier_equals(target_base, "NativeInt"))
                        {
                            kgpc_type = create_primitive_type(INT64_TYPE);
                            if (kgpc_type != NULL)
                                created_new_type = 1;
                        }
                        else if (pascal_identifier_equals(target_base, "LongInt") ||
                            pascal_identifier_equals(target_base, "Cardinal") ||
                            pascal_identifier_equals(target_base, "LongWord") ||
                            pascal_identifier_equals(target_base, "DWord"))
                        {
                            kgpc_type = create_primitive_type(LONGINT_TYPE);
                            if (kgpc_type != NULL)
                                created_new_type = 1;
                        }
                        else if (pascal_identifier_equals(target_base, "Integer") ||
                            pascal_identifier_equals(target_base, "SmallInt") ||
                            pascal_identifier_equals(target_base, "ShortInt") ||
                            pascal_identifier_equals(target_base, "Byte") ||
                            pascal_identifier_equals(target_base, "Word"))
                        {
                            int storage_size = 0;
                            if (pascal_identifier_equals(target_base, "Byte") ||
                                pascal_identifier_equals(target_base, "ShortInt"))
                                storage_size = 1;
                            else if (pascal_identifier_equals(target_base, "Word") ||
                                     pascal_identifier_equals(target_base, "SmallInt"))
                                storage_size = 2;
                            if (storage_size > 0)
                                kgpc_type = create_primitive_type_with_size(INT_TYPE, storage_size);
                            else
                                kgpc_type = create_primitive_type(INT_TYPE);
                            if (kgpc_type != NULL)
                                created_new_type = 1;
                        }
                        else if (pascal_identifier_equals(target_base, "Real") ||
                                 pascal_identifier_equals(target_base, "Double") ||
                                 pascal_identifier_equals(target_base, "Single"))
                        {
                            kgpc_type = create_primitive_type(REAL_TYPE);
                            if (kgpc_type != NULL)
                                created_new_type = 1;
                        }
                        else if (pascal_identifier_equals(target_base, "Extended"))
                        {
                            kgpc_type = create_primitive_type(EXTENDED_TYPE);
                            if (kgpc_type != NULL)
                                created_new_type = 1;
                        }
                        else if (pascal_identifier_equals(target_base, "Boolean"))
                        {
                            kgpc_type = create_primitive_type(BOOL);
                            if (kgpc_type != NULL)
                                created_new_type = 1;
                        }
                        else if (pascal_identifier_equals(target_base, "Char") ||
                                 pascal_identifier_equals(target_base, "AnsiChar"))
                        {
                            kgpc_type = create_primitive_type(CHAR_TYPE);
                            if (kgpc_type != NULL)
                                created_new_type = 1;
                        }
                        /* If target is another user-defined type, check if it's already declared */
                        else
                        {
                            HashNode_t *target_node = NULL;
                            char *lookup_target = NULL;
                            char *lookup_unqualified = NULL;
                            const char *lookup_name = target;
                            int has_qualification = 0;
                            if (target_ref != NULL)
                            {
                                lookup_target = type_ref_render_mangled(target_ref);
                                if (lookup_target != NULL)
                                    lookup_name = lookup_target;
                                if (target_ref->name != NULL && target_ref->name->count > 1)
                                    has_qualification = 1;
                            }

                            /* Handle qualified type names like "UnixType.culong" */
                            int found = FindSymbol(&target_node, symtab, lookup_name);
                            if (!found || target_node == NULL)
                            {
                                /* Try unqualified name if we have structured qualification */
                                if (has_qualification)
                                {
                                    lookup_unqualified = type_ref_render_mangled_unqualified(target_ref);
                                    if (lookup_unqualified != NULL)
                                    {
                                        lookup_name = lookup_unqualified;
                                        found = FindSymbol(&target_node, symtab, lookup_name);
                                    }
                                }
                            }
                            if (found && target_node != NULL && has_qualification &&
                                target_node->type != NULL &&
                                kgpc_type_get_type_alias(target_node->type) == alias)
                            {
                                target_node = semcheck_find_type_excluding_alias(symtab, lookup_name, alias);
                            }
                            free(lookup_target);
                            free(lookup_unqualified);
                            
                            if (found &&
                                target_node != NULL && target_node->hash_type == HASHTYPE_TYPE &&
                                target_node->type != NULL)
                            {
                                /* Target type already exists, retain and use it.
                                 * Flag that we're reusing an existing type so we don't
                                 * overwrite its alias info later. */
                                kgpc_type = target_node->type;
                                kgpc_type_retain(kgpc_type);
                            }
                            /* Otherwise, skip - can't resolve this type yet */
                        }
                    }
                    
                    /* Flag to track if we're reusing an existing type (vs creating new) */
                    int reusing_existing = (!created_new_type && kgpc_type != NULL &&
                        kgpc_type_get_type_alias(kgpc_type) != NULL &&
                        kgpc_type_get_type_alias(kgpc_type) != alias);
                    
                    if (kgpc_type != NULL)
                    {
                        if (kgpc_getenv("KGPC_DEBUG_PREDECLARE") != NULL)
                        {
                            fprintf(stderr, "[predeclare] SUCCESS type '%s' kgpc_type=%p kind=%d reusing=%d\n",
                                type_id, (void*)kgpc_type, kgpc_type->kind, reusing_existing);
                        }
                        
                        /* Only modify alias info for newly created types, not reused ones */
                        if (!reusing_existing)
                        {
                            /* IMPORTANT: Inherit storage_size from the original type's alias.
                             * This is critical for types like WideChar (2 bytes) where we retain
                             * the original type but need to preserve its custom storage_size. */
                            struct TypeAlias *original_alias = kgpc_type_get_type_alias(kgpc_type);
                            if (original_alias != NULL && original_alias->storage_size > 0 &&
                                alias->storage_size <= 0)
                            {
                                alias->storage_size = original_alias->storage_size;
                            }
                            
                            /* Set type_alias on KgpcType - only for newly created types */
                            kgpc_type_set_type_alias(kgpc_type, alias);
                        }
                        
                        /* Store in tree for later use by semcheck_type_decls */
                        if (tree->tree_data.type_decl_data.kgpc_type == NULL)
                        {
                            tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                            kgpc_type_retain(kgpc_type);  /* Tree owns a reference */
                        }
                        
                        /* Push onto symbol table */
                        int result = PushTypeOntoScope_Typed(symtab, (char *)type_id, kgpc_type);
                        if (result > 0)
                        {
                            /* Should not happen since we checked above, but handle gracefully */
                            errors++;
                        }
                        else
                        {
                            mark_latest_type_node_unit_info(symtab, type_id,
                                tree->tree_data.type_decl_data.defined_in_unit,
                                tree->tree_data.type_decl_data.unit_is_public,
                                tree->tree_data.type_decl_data.source_unit_index);
                        }
                        /* Release creator's reference */
                        destroy_kgpc_type(kgpc_type);
                    }
                    else if (kgpc_getenv("KGPC_DEBUG_PREDECLARE") != NULL)
                    {
                        fprintf(stderr, "[predeclare] SKIP type '%s' - kgpc_type is NULL\n", type_id);
                    }
                }
                /* Skip TYPE_DECL_RECORD, TYPE_DECL_GENERIC, TYPE_DECL_RANGE - 
                 * these have complex dependencies and are handled by semcheck_type_decls */
predeclare_types_next:
                /* Ensure every record type_decl tree has kgpc_type set, even
                 * if the predeclare pass skipped it as a duplicate.  Without
                 * this, codegen falls back to info.record which may be a
                 * depleted shell after forward-declaration resolution. */
                if (tree->tree_data.type_decl_data.kgpc_type == NULL &&
                    tree->tree_data.type_decl_data.kind == TYPE_DECL_RECORD)
                {
                    const char *tid = tree->tree_data.type_decl_data.id;
                    if (tid != NULL)
                    {
                        HashNode_t *canon_node = NULL;
                        if (FindSymbol(&canon_node, symtab, tid) &&
                            canon_node != NULL &&
                            canon_node->hash_type == HASHTYPE_TYPE &&
                            canon_node->type != NULL)
                        {
                            tree->tree_data.type_decl_data.kgpc_type = canon_node->type;
                            kgpc_type_retain(canon_node->type);
                        }
                    }
                }
                semcheck_restore_scope(symtab, saved_scope_for_type);
            }
        }
        cur = cur->next;
    }

    return errors;
}

/* Helper function to check if a statement contains an asm block */
__attribute__((unused)) static int statement_contains_asm_block(struct Statement *stmt)
{
    if (stmt == NULL)
        return 0;
    
    if (stmt->type == STMT_ASM_BLOCK)
        return 1;
    
    if (stmt->type == STMT_COMPOUND_STATEMENT)
    {
        ListNode_t *cur = stmt->stmt_data.compound_statement;
        while (cur != NULL)
        {
            if (cur->type == LIST_STMT && cur->cur != NULL)
            {
                struct Statement *child_stmt = (struct Statement *)cur->cur;
                if (statement_contains_asm_block(child_stmt))
                    return 1;
            }
            cur = cur->next;
        }
    }
    
    return 0;
}

static int record_has_property(struct RecordType *record_info, const char *property_name)
{
    if (record_info == NULL || property_name == NULL)
        return 0;

    ListNode_t *node = record_info->properties;
    while (node != NULL)
    {
        if (node->type == LIST_CLASS_PROPERTY && node->cur != NULL)
        {
            struct ClassProperty *property = (struct ClassProperty *)node->cur;
            if (property->name != NULL &&
                pascal_identifier_equals(property->name, property_name))
                return 1;
        }
        node = node->next;
    }
    return 0;
}

static struct ClassProperty *clone_class_property(const struct ClassProperty *property)
{
    if (property == NULL)
        return NULL;

    struct ClassProperty *clone = (struct ClassProperty *)calloc(1, sizeof(struct ClassProperty));
    if (clone == NULL)
        return NULL;

    clone->name = property->name != NULL ? strdup(property->name) : NULL;
    clone->type = property->type;
    clone->type_id = property->type_id != NULL ? strdup(property->type_id) : NULL;
    clone->type_ref = property->type_ref != NULL ? type_ref_clone(property->type_ref) : NULL;
    clone->read_accessor = property->read_accessor != NULL ? strdup(property->read_accessor) : NULL;
    clone->write_accessor = property->write_accessor != NULL ? strdup(property->write_accessor) : NULL;
    clone->is_indexed = property->is_indexed;
    clone->is_default = property->is_default;

    if ((property->name != NULL && clone->name == NULL) ||
        (property->type_id != NULL && clone->type_id == NULL) ||
        (property->read_accessor != NULL && clone->read_accessor == NULL) ||
        (property->write_accessor != NULL && clone->write_accessor == NULL))
    {
        free(clone->name);
        free(clone->type_id);
        if (clone->type_ref != NULL)
            type_ref_free(clone->type_ref);
        free(clone->read_accessor);
        free(clone->write_accessor);
        free(clone);
        return NULL;
    }

    return clone;
}

static ListNode_t *clone_property_list_unique(struct RecordType *record_info,
    const ListNode_t *properties)
{
    ListNode_t *head = NULL;
    ListNode_t **tail = &head;

    while (properties != NULL)
    {
        if (properties->type == LIST_CLASS_PROPERTY && properties->cur != NULL)
        {
            struct ClassProperty *property = (struct ClassProperty *)properties->cur;
            if (!record_has_property(record_info, property->name))
            {
                struct ClassProperty *clone = clone_class_property(property);
                if (clone != NULL)
                {
                    ListNode_t *node = CreateListNode(clone, LIST_CLASS_PROPERTY);
                    if (node != NULL)
                    {
                        *tail = node;
                        tail = &node->next;
                    }
                    else
                    {
                        free(clone->name);
                        free(clone->type_id);
                        free(clone->read_accessor);
                        free(clone->write_accessor);
                        free(clone);
                    }
                }
            }
        }
        properties = properties->next;
    }

    return head;
}

/* Helper function to check for circular inheritance */
static int check_circular_inheritance(SymTab_t *symtab, const char *class_name, const char *parent_name, int max_depth)
{
    if (class_name == NULL || parent_name == NULL)
        return 0;
    
    /* Check if parent is the same as this class (direct circular reference) */
    if (strcmp(class_name, parent_name) == 0)
        return 1;
    
    /* Prevent infinite recursion by limiting depth */
    if (max_depth <= 0)
        return 1;
    
    /* Look up parent class */
    HashNode_t *parent_node = NULL;
    if (FindSymbol(&parent_node, symtab, parent_name) == 0 || parent_node == NULL)
        return 0;  /* Parent not found yet, not necessarily circular */
    
    struct RecordType *parent_record = get_record_type_from_node(parent_node);
    if (parent_record == NULL || parent_record->parent_class_name == NULL)
        return 0;  /* Parent has no parent, no circular reference */
    
    /* Recursively check parent's parent */
    return check_circular_inheritance(symtab, class_name, parent_record->parent_class_name, max_depth - 1);
}

static int add_class_padding_field(struct RecordType *record_info, long long padding_bytes)
{
    if (record_info == NULL || padding_bytes <= 0)
        return 0;

    struct RecordField *padding = (struct RecordField *)calloc(1, sizeof(struct RecordField));
    if (padding == NULL)
        return 1;

    padding->name = NULL;
    padding->type = CHAR_TYPE;
    padding->is_array = 1;
    padding->array_start = 0;
    padding->array_end = (int)padding_bytes - 1;
    padding->array_element_type = CHAR_TYPE;
    padding->array_is_open = 0;
    padding->is_hidden = 1;

    ListNode_t *node = CreateListNode(padding, LIST_RECORD_FIELD);
    if (record_info->fields == NULL)
        record_info->fields = node;
    else
        record_info->fields = PushListNodeBack(record_info->fields, node);
    return 0;
}

static int compute_class_record_size(SymTab_t *symtab, struct RecordType *record_info,
    long long *size_out, int line_num)
{
    if (record_info == NULL || size_out == NULL)
        return 1;

    return semcheck_compute_record_size(symtab, record_info, size_out,
        line_num >= 0 ? line_num : 0);
}

static int ensure_class_storage_capacity(SymTab_t *symtab, struct RecordType *record_info,
    long long required_size, int line_num)
{
    if (record_info == NULL)
        return 0;

    long long current_size = 0;
    if (compute_class_record_size(symtab, record_info, &current_size, line_num) != 0)
        return 1;

    if (current_size >= required_size)
        return 0;

    long long padding_bytes = required_size - current_size;
    return add_class_padding_field(record_info, padding_bytes);
}

/*
 * Detect and populate default_indexed_property for a class.
 * This enables array-like indexing (obj[i]) to be transformed to field access (obj.field[i]).
 * 
 * Detection rules:
 * 1. If a class has an open array field (array_is_open == 1) and it's the only such field,
 *    or specifically named "FItems" (Delphi convention), use it as the default indexed property.
 * 2. Also inherit from parent class if parent has a default_indexed_property and child doesn't.
 * 
 * @param record_info The class RecordType to configure. Must not be NULL.
 * @param parent_record The parent class RecordType, or NULL for base classes like TObject.
 */
static void detect_default_indexed_property(struct RecordType *record_info, const struct RecordType *parent_record)
{
    assert(record_info != NULL);
    
    /* Already set - don't override */
    if (record_info->default_indexed_property != NULL)
        return;

    /* Prefer explicitly default indexed properties when available */
    for (int pass = 0; pass < 2; ++pass)
    {
        ListNode_t *prop_node = (pass == 0) ? record_info->properties : record_info->record_properties;
        while (prop_node != NULL)
        {
            if (prop_node->type == LIST_CLASS_PROPERTY && prop_node->cur != NULL)
            {
                struct ClassProperty *prop = (struct ClassProperty *)prop_node->cur;
                if (prop->is_default && prop->is_indexed && prop->name != NULL)
                {
                    record_info->default_indexed_property = strdup(prop->name);
                    record_info->default_indexed_element_type = prop->type;
                    record_info->default_indexed_element_type_id = prop->type_id != NULL ?
                        strdup(prop->type_id) : NULL;
                    return;
                }
            }
            prop_node = prop_node->next;
        }
    }
    
    /* First, try to detect from this class's fields */
    struct RecordField *candidate = NULL;
    ListNode_t *cur = record_info->fields;
    while (cur != NULL)
    {
        if (cur->type == LIST_RECORD_FIELD)
        {
            struct RecordField *field = (struct RecordField *)cur->cur;
            if (field != NULL && field->name != NULL && field->is_array && field->array_is_open)
            {
                /* Found an open array field - it's a candidate for default indexed property */
                /* Prefer "FItems" if we find it (Delphi naming convention for list items) */
                if (pascal_identifier_equals(field->name, "FItems"))
                {
                    candidate = field;
                    break;  /* FItems is the preferred choice */
                }
                else if (candidate == NULL)
                {
                    /* First open array found - tentative candidate */
                    candidate = field;
                }
            }
        }
        cur = cur->next;
    }
    
    if (candidate != NULL)
    {
        record_info->default_indexed_property = strdup(candidate->name);
        record_info->default_indexed_element_type = candidate->array_element_type;
        record_info->default_indexed_element_type_id = candidate->array_element_type_id != NULL ?
            strdup(candidate->array_element_type_id) : NULL;
        return;
    }
    
    /* Inherit from parent if available */
    if (parent_record != NULL && parent_record->default_indexed_property != NULL)
    {
        record_info->default_indexed_property = strdup(parent_record->default_indexed_property);
        record_info->default_indexed_element_type = parent_record->default_indexed_element_type;
        record_info->default_indexed_element_type_id = parent_record->default_indexed_element_type_id != NULL ?
            strdup(parent_record->default_indexed_element_type_id) : NULL;
    }
}

/* Helper function to merge parent class fields into derived class */
static int merge_parent_class_fields(SymTab_t *symtab, struct RecordType *record_info, const char *class_name, int line_num)
{
    if (record_info == NULL || record_info->parent_class_name == NULL)
        return 0;  /* No parent class to merge */

    record_info->has_cached_size = 0;

    /* Guard against double-merging: if parent fields have already been
     * merged into this record, skip to avoid duplicating inherited fields.
     * Also check the canonical RecordType in the symbol table — if another
     * RecordType for the same class was already merged (e.g. forward decl
     * vs full decl creating separate RecordType objects), skip this one. */
    if (record_info->parent_fields_merged)
        return 0;
    if (class_name != NULL) {
        HashNode_t *self_node = semcheck_find_preferred_type_node(symtab, class_name);
        if (self_node != NULL) {
            struct RecordType *canonical = get_record_type_from_node(self_node);
            if (canonical != NULL && canonical != record_info && canonical->parent_fields_merged)
            {
                /* Canonical instance was already merged — propagate its merged
                 * state to this instance so codegen (which may use this instance
                 * via the Tree_t node) sees the correct fields and interfaces. */
                if (!record_info->parent_fields_merged) {
                    record_info->fields = canonical->fields;
                    record_info->num_interfaces = canonical->num_interfaces;
                    record_info->interface_names = canonical->interface_names;
                    record_info->properties = canonical->properties;
                    record_info->cached_size = canonical->cached_size;
                    record_info->has_cached_size = canonical->has_cached_size;
                    record_info->parent_fields_merged = 1;
                }
                return 0;
            }
        }
    }

    /* Check for circular inheritance */
    if (check_circular_inheritance(symtab, class_name, record_info->parent_class_name, 100))
    {
        semcheck_error_with_context("Error on line %d, circular inheritance detected for class '%s'!\n",
                line_num, class_name ? class_name : "<unknown>");
        return 1;
    }

    /* Look up parent class in symbol table */
    HashNode_t *parent_node = semcheck_find_preferred_type_node(symtab, record_info->parent_class_name);
    if (parent_node == NULL)
    {
        semcheck_error_with_context("Error on line %d, parent class '%s' not found!\n",
                line_num, record_info->parent_class_name);
        return 1;
    }

    /* Get parent's RecordType */
    struct RecordType *parent_record = get_record_type_from_node(parent_node);
    if (parent_record == NULL)
    {
        semcheck_error_with_context("Error on line %d, parent class '%s' is not a class/record type!\n",
                line_num, record_info->parent_class_name);
        return 1;
    }

    /* Ensure parent has its own parent fields merged before we clone from it.
     * Switch to the parent's unit scope so that the grandparent lookup
     * searches the parent unit's dependencies (e.g. if A uses B uses C,
     * and B's class inherits from C's class, we need C's scope visible). */
    if (parent_record->parent_class_name != NULL && !parent_record->parent_fields_merged)
    {
        ScopeNode *saved_scope_for_parent = NULL;
        if (parent_node->source_unit_index > 0)
            saved_scope_for_parent = semcheck_switch_to_unit_scope(symtab, parent_node->source_unit_index);
        int parent_merge_result = merge_parent_class_fields(symtab, parent_record,
            record_info->parent_class_name, line_num);
        if (saved_scope_for_parent != NULL)
            semcheck_restore_scope(symtab, saved_scope_for_parent);
        if (parent_merge_result > 0)
            return parent_merge_result;
    }

    /* Clone parent's fields and prepend them to this record's fields */
    ListNode_t *parent_fields = parent_record->fields;
    if (parent_fields != NULL)
    {
        /* We need to clone the parent's field list and prepend it */
        ListNode_t *cloned_parent_fields = NULL;
        ListNode_t *cur = parent_fields;
        ListNode_t *last_cloned = NULL;
        
        while (cur != NULL)
        {
            if (cur->type != LIST_RECORD_FIELD || cur->cur == NULL)
            {
                cur = cur->next;
                continue;
            }
            struct RecordField *original_field = (struct RecordField *)cur->cur;

            /* Skip hidden fields from parent if the record already has them
             * (e.g., __kgpc_class_typeinfo added to forward declarations). */
            if (record_field_is_hidden(original_field) && original_field->name != NULL)
            {
                int already_has = 0;
                ListNode_t *own = record_info->fields;
                while (own != NULL)
                {
                    if (own->type == LIST_RECORD_FIELD && own->cur != NULL)
                    {
                        struct RecordField *f = (struct RecordField *)own->cur;
                        if (record_field_is_hidden(f) && f->name != NULL &&
                            strcmp(f->name, original_field->name) == 0)
                        {
                            already_has = 1;
                            break;
                        }
                    }
                    own = own->next;
                }
                if (already_has)
                {
                    cur = cur->next;
                    continue;
                }
            }

            /* Clone the field */
            struct RecordField *cloned_field = (struct RecordField *)calloc(1, sizeof(struct RecordField));
            if (cloned_field == NULL)
            {
                /* Clean up previously allocated fields */
                while (cloned_parent_fields != NULL)
                {
                    ListNode_t *temp = cloned_parent_fields;
                    cloned_parent_fields = cloned_parent_fields->next;
                    struct RecordField *field = (struct RecordField *)temp->cur;
                    free(field->name);
                    free(field->type_id);
                    if (field->type_ref != NULL)
                        type_ref_free(field->type_ref);
                    free(field->array_element_type_id);
                    if (field->array_element_type_ref != NULL)
                        type_ref_free(field->array_element_type_ref);
                    if (field->pointer_type_ref != NULL)
                        type_ref_free(field->pointer_type_ref);
                    if (field->proc_type != NULL)
                        kgpc_type_release(field->proc_type);
                    free(field);
                    free(temp);
                }
                return 1;
            }
            
            cloned_field->name = original_field->name ? strdup(original_field->name) : NULL;
            cloned_field->type = original_field->type;
            cloned_field->type_id = original_field->type_id ? strdup(original_field->type_id) : NULL;
            cloned_field->type_ref = original_field->type_ref != NULL
                ? type_ref_clone(original_field->type_ref) : NULL;
            cloned_field->nested_record = original_field->nested_record;  /* Share the nested record */
            cloned_field->proc_type = original_field->proc_type;
            if (cloned_field->proc_type != NULL)
                kgpc_type_retain(cloned_field->proc_type);
            cloned_field->is_array = original_field->is_array;
            cloned_field->array_start = original_field->array_start;
            cloned_field->array_end = original_field->array_end;
            cloned_field->array_element_type = original_field->array_element_type;
            cloned_field->array_element_type_id = original_field->array_element_type_id ? 
                strdup(original_field->array_element_type_id) : NULL;
            cloned_field->array_element_type_ref = original_field->array_element_type_ref != NULL
                ? type_ref_clone(original_field->array_element_type_ref) : NULL;
            cloned_field->array_is_open = original_field->array_is_open;
            cloned_field->is_hidden = original_field->is_hidden;
            cloned_field->is_class_var = original_field->is_class_var;
            cloned_field->is_pointer = original_field->is_pointer;
            cloned_field->pointer_type = original_field->pointer_type;
            cloned_field->pointer_type_id = original_field->pointer_type_id ?
                strdup(original_field->pointer_type_id) : NULL;
            cloned_field->pointer_type_ref = original_field->pointer_type_ref != NULL
                ? type_ref_clone(original_field->pointer_type_ref) : NULL;
            /* array_element_record is zeroed by calloc — not cloned to avoid double-free */
            cloned_field->enum_literals = NULL;
            
            /* Create list node for cloned field */
            ListNode_t *new_node = (ListNode_t *)malloc(sizeof(ListNode_t));
            if (new_node == NULL)
            {
                free(cloned_field->name);
                free(cloned_field->type_id);
                if (cloned_field->type_ref != NULL)
                    type_ref_free(cloned_field->type_ref);
                free(cloned_field->array_element_type_id);
                if (cloned_field->array_element_type_ref != NULL)
                    type_ref_free(cloned_field->array_element_type_ref);
                if (cloned_field->pointer_type_ref != NULL)
                    type_ref_free(cloned_field->pointer_type_ref);
                if (cloned_field->proc_type != NULL)
                    kgpc_type_release(cloned_field->proc_type);
                free(cloned_field);
                
                /* Clean up previously allocated fields */
                while (cloned_parent_fields != NULL)
                {
                    ListNode_t *temp = cloned_parent_fields;
                    cloned_parent_fields = cloned_parent_fields->next;
                    struct RecordField *field = (struct RecordField *)temp->cur;
                    free(field->name);
                    free(field->type_id);
                    free(field->array_element_type_id);
                    if (field->proc_type != NULL)
                        kgpc_type_release(field->proc_type);
                    free(field);
                    free(temp);
                }
                return 1;
            }
            
            new_node->type = LIST_RECORD_FIELD;
            new_node->cur = cloned_field;
            new_node->next = NULL;
            
            /* Append to cloned list */
            if (cloned_parent_fields == NULL)
            {
                cloned_parent_fields = new_node;
                last_cloned = new_node;
            }
            else
            {
                last_cloned->next = new_node;
                last_cloned = new_node;
            }
            
            cur = cur->next;
        }
        
        /* Prepend cloned parent fields to this record's fields */
        if (last_cloned != NULL)
        {
            last_cloned->next = record_info->fields;
            record_info->fields = cloned_parent_fields;
        }
    }

    if (parent_record != NULL && parent_record->properties != NULL)
    {
        ListNode_t *cloned_properties = clone_property_list_unique(record_info,
            parent_record->properties);
        if (cloned_properties != NULL)
            record_info->properties = ConcatList(cloned_properties, record_info->properties);
    }

    /* Merge parent's interfaces into child's interface_names.
     * Only add interfaces the child doesn't already declare. */
    if (parent_record != NULL && parent_record->num_interfaces > 0)
    {
        for (int pi = 0; pi < parent_record->num_interfaces; pi++)
        {
            const char *parent_iface = parent_record->interface_names[pi];
            if (parent_iface == NULL) continue;
            int already_declared = 0;
            for (int ci = 0; ci < record_info->num_interfaces; ci++)
            {
                if (record_info->interface_names[ci] != NULL &&
                    strcasecmp(record_info->interface_names[ci], parent_iface) == 0)
                {
                    already_declared = 1;
                    break;
                }
            }
            if (!already_declared)
            {
                int new_count = record_info->num_interfaces + 1;
                char **new_names = (char **)realloc(record_info->interface_names,
                    new_count * sizeof(char *));
                if (new_names != NULL)
                {
                    new_names[new_count - 1] = strdup(parent_iface);
                    record_info->interface_names = new_names;
                    record_info->num_interfaces = new_count;
                }
            }
        }
    }

    /* Detect default indexed property after merging parent fields */
    detect_default_indexed_property(record_info, parent_record);

    record_info->parent_fields_merged = 1;

    /* Class instances are accessed via pointers; parent storage need not grow.
     * Only ensure capacity for non-class (plain record) inheritance. */
    if (parent_record != NULL && !record_info->is_class)
    {
        long long derived_size = 0;
        if (compute_class_record_size(symtab, record_info, &derived_size, line_num) == 0)
            ensure_class_storage_capacity(symtab, parent_record, derived_size, line_num);
    }

    return 0;
}

/* Build Virtual Method Table for a class */
static char *semcheck_param_sig_from_params(ListNode_t *params, int skip_first_param);
static int build_class_vmt(SymTab_t *symtab, struct RecordType *record_info, 
                            const char *class_name, int line_num) {
    if (record_info == NULL || class_name == NULL)
        return 0;
    
    /* Get methods registered for this class.
     * For nested types (e.g., TMarshaller.TDeferBase), methods may have been registered
     * under the unqualified name before renaming. Fall back to unqualified name if needed. */
    ListNode_t *class_methods = NULL;
    int method_count = 0;
    get_class_methods(class_name, &class_methods, &method_count);
    if (method_count == 0 && record_info->type_id != NULL) {
        /* For nested types (e.g. TMarshaller.TDeferBase), methods may be
         * registered under the unqualified type_id. */
        get_class_methods(record_info->type_id, &class_methods, &method_count);
    }

    /* Start with parent's VMT if this class has a parent */
    ListNode_t *vmt = NULL;
    int vmt_size = 0;

if (record_info->parent_class_name != NULL) {
        /* Look up parent class */
        HashNode_t *parent_node = semcheck_find_preferred_type_node(symtab, record_info->parent_class_name);
        if (parent_node != NULL) {
            struct RecordType *parent_record = get_record_type_from_node(parent_node);
            /* If parent has method bindings but VMT not built yet, build it first */
            if (parent_record != NULL && parent_record->methods == NULL) {
                ListNode_t *parent_bindings = NULL;
                int parent_binding_count = 0;
                const char *parent_name = record_info->parent_class_name;
                /* Look up the parent's type_id from its record if available */
                if (parent_record->type_id != NULL)
                    parent_name = parent_record->type_id;
                get_class_methods(parent_name, &parent_bindings, &parent_binding_count);
                if (parent_binding_count > 0) {
                    /* Parent has methods but VMT not built — build it now */
                    build_class_vmt(symtab, parent_record, parent_name, line_num);
                }
                DestroyList(parent_bindings);
            }
            if (parent_record != NULL && parent_record->methods != NULL) {
                /* Clone parent's VMT */
                ListNode_t *parent_vmt = parent_record->methods;
                ListNode_t **tail = &vmt;
                
                while (parent_vmt != NULL) {
                    struct MethodInfo *parent_method = (struct MethodInfo *)parent_vmt->cur;
                    if (parent_method != NULL) {
                        struct MethodInfo *cloned = (struct MethodInfo *)malloc(sizeof(struct MethodInfo));
                        if (cloned != NULL) {
                            cloned->name = parent_method->name ? strdup(parent_method->name) : NULL;
                            cloned->mangled_name = parent_method->mangled_name ? strdup(parent_method->mangled_name) : NULL;
                            cloned->is_virtual = parent_method->is_virtual;
                            cloned->is_override = 0;  /* Parent's methods aren't overrides in child */
                            cloned->vmt_index = parent_method->vmt_index;
                            cloned->param_count = parent_method->param_count;
                            cloned->param_sig = parent_method->param_sig ? strdup(parent_method->param_sig) : NULL;
                            cloned->resolved_mangled_id = parent_method->resolved_mangled_id ? strdup(parent_method->resolved_mangled_id) : NULL;

                            /* For generic specializations, inherited methods must use
                             * the specialized class name, not the parent's. Clear
                             * resolved_mangled_id so the resolution pass re-resolves
                             * with the correct class name. */
                            if (record_info->is_generic_specialization && cloned->resolved_mangled_id != NULL)
                            {
                                free(cloned->resolved_mangled_id);
                                cloned->resolved_mangled_id = NULL;
                            }
                            
                            ListNode_t *node = (ListNode_t *)malloc(sizeof(ListNode_t));
                            if (node != NULL) {
                                node->type = LIST_UNSPECIFIED;
                                node->cur = cloned;
                                node->next = NULL;
                                *tail = node;
                                tail = &node->next;
                                vmt_size++;
                            } else {
                                free(cloned->name);
                                free(cloned->mangled_name);
                                free(cloned->param_sig);
                                free(cloned);
                            }
                        }
                    }
                    parent_vmt = parent_vmt->next;
                }
            }
        }
    }
    
    /* Process this class's methods */
    ListNode_t *cur_method = class_methods;
    while (cur_method != NULL) {
        ClassMethodBinding *binding = (ClassMethodBinding *)cur_method->cur;
        if (binding != NULL && binding->method_name != NULL) {
            /* Resolve the full mangled name for this method overload when possible. */
            char *base_name = make_method_lookup_key(class_name, binding->method_name);
            char *mangled = NULL;
            if (base_name != NULL && symtab != NULL)
            {
                ListNode_t *matches = FindAllIdents(symtab, base_name);
                HashNode_t *best = NULL;
                if (binding->param_sig != NULL)
                {
                    for (ListNode_t *m = matches; m != NULL; m = m->next)
                    {
                        HashNode_t *cand = (HashNode_t *)m->cur;
                        if (cand == NULL || cand->type == NULL ||
                            cand->type->kind != TYPE_KIND_PROCEDURE)
                            continue;
                        int skip_self = (!binding->is_static);
                        char *sig = semcheck_param_sig_from_params(
                            cand->type->info.proc_info.params, skip_self);
                        if (sig != NULL && strcmp(sig, binding->param_sig) == 0)
                        {
                            best = cand;
                            free(sig);
                            break;
                        }
                        if (sig != NULL)
                            free(sig);
                    }
                }
                if (best == NULL && binding->param_count >= 0)
                {
                    int match_count = 0;
                    HashNode_t *last_match = NULL;
                    for (ListNode_t *m = matches; m != NULL; m = m->next)
                    {
                        HashNode_t *cand = (HashNode_t *)m->cur;
                        if (cand == NULL || cand->type == NULL ||
                            cand->type->kind != TYPE_KIND_PROCEDURE)
                            continue;
                        int count = ListLength(cand->type->info.proc_info.params);
                        if (!binding->is_static && count > 0)
                            count -= 1; /* drop implicit Self */
                        if (count == binding->param_count)
                        {
                            match_count++;
                            last_match = cand;
                        }
                    }
                    if (match_count == 1)
                        best = last_match;
                }
                if (best != NULL) {
                    /* Prefer definition's mangled_id: declaration may have wrong type
                     * resolution (e.g., PByte → _i_ before type resolution completes) */
                    const char *chosen_id = best->mangled_id;
                    if (best->type != NULL && best->type->info.proc_info.definition != NULL) {
                        Tree_t *bdef = best->type->info.proc_info.definition;
                        if (bdef->tree_data.subprogram_data.mangled_id != NULL)
                            chosen_id = bdef->tree_data.subprogram_data.mangled_id;
                    }
                    if (chosen_id != NULL)
                        mangled = strdup(chosen_id);
                }
                DestroyList(matches);
            }
            if (mangled == NULL && base_name != NULL)
                mangled = strdup(base_name);
            if (base_name != NULL)
                free(base_name);
            
            /* Check if this method overrides a parent method */
            int is_actual_override = 0;
            if (binding->is_virtual || binding->is_override) {
                /* Check if a method with this name/signature exists in the parent VMT */
                ListNode_t *vmt_entry = vmt;
                while (vmt_entry != NULL) {
                    struct MethodInfo *info = (struct MethodInfo *)vmt_entry->cur;
                    if (info != NULL && info->name != NULL &&
                        strcasecmp(info->name, binding->method_name) == 0) {
                        int signature_matches = 0;
                        if (binding->param_sig != NULL && info->param_sig != NULL) {
                            if (strcmp(binding->param_sig, info->param_sig) == 0)
                                signature_matches = 1;
                        } else if (binding->param_count >= 0 && info->param_count >= 0) {
                            if (binding->param_count == info->param_count)
                                signature_matches = 1;
                        } else {
                            signature_matches = 1;
                        }
                        if (!signature_matches) {
                            vmt_entry = vmt_entry->next;
                            continue;
                        }
                        /* Method exists in parent - this is an override */
                        is_actual_override = 1;
                        /* Replace with derived class's version */
                        free(info->mangled_name);
                        info->mangled_name = mangled ? strdup(mangled) : NULL;
                        /* Clear resolved_mangled_id so the resolve phase re-resolves
                         * for the overriding class (the cloned parent value is stale) */
                        if (info->resolved_mangled_id != NULL) {
                            free(info->resolved_mangled_id);
                            info->resolved_mangled_id = NULL;
                        }
                        info->is_override = 1;
                        if (info->param_count < 0 && binding->param_count >= 0)
                            info->param_count = binding->param_count;
                        if (info->param_sig == NULL && binding->param_sig != NULL)
                            info->param_sig = strdup(binding->param_sig);
                        break;
                    }
                    vmt_entry = vmt_entry->next;
                }
            }
            
            if (is_actual_override) {
                /* Method was found and replaced in parent VMT - nothing more to do */
            } else if (binding->is_virtual || binding->is_override) {
                /* Add new virtual method to VMT */
                /* Note: if binding->is_override is true but no parent method was found,
                 * this could be because the parent's VMT was incompletely built
                 * (e.g., FPC system unit with unsupported features).
                 * Treat as a new virtual method instead of an error. */
                
                struct MethodInfo *new_method = (struct MethodInfo *)malloc(sizeof(struct MethodInfo));
                if (new_method != NULL) {
                    new_method->name = binding->method_name ? strdup(binding->method_name) : NULL;
                    new_method->mangled_name = mangled ? strdup(mangled) : NULL;
                    new_method->is_virtual = 1;
                    new_method->is_override = 0;
                    new_method->param_count = binding->param_count;
                    new_method->param_sig = binding->param_sig ? strdup(binding->param_sig) : NULL;
                    new_method->resolved_mangled_id = NULL;
                    /* FPC VMT has 12 metadata slots (96 bytes) before virtual methods:
                     * vInstanceSize, vInstanceSize2, vParentRef, vClassName,
                     * vDynamicTable, vMethodTable, vFieldTable, vTypeInfo,
                     * vInitTable, vAutoTable, vIntfTable, vMsgStrPtr.
                     * Virtual methods start at offset 96 (slot 12). */
                    new_method->vmt_index = vmt_size + 12;

                    ListNode_t *node = (ListNode_t *)malloc(sizeof(ListNode_t));
                    if (node != NULL) {
                        node->type = LIST_UNSPECIFIED;
                        node->cur = new_method;
                        node->next = NULL;
                        
                        /* Append to end */
                        if (vmt == NULL) {
                            vmt = node;
                        } else {
                            ListNode_t *last = vmt;
                            while (last->next != NULL)
                                last = last->next;
                            last->next = node;
                        }
                        vmt_size++;
                    } else {
                        free(new_method->name);
                        free(new_method->mangled_name);
                        free(new_method->param_sig);
                        free(new_method);
                    }
                }
            }
            
            free(mangled);
        }
        cur_method = cur_method->next;
    }
    
    /* Store VMT in record */
    record_info->methods = vmt;

    /* Resolve mangled IDs for VMT entries so codegen doesn't need to
     * do symbol table lookups or parse mangled names. */
    for (ListNode_t *vmt_node = vmt; vmt_node != NULL; vmt_node = vmt_node->next) {
        struct MethodInfo *mi = (struct MethodInfo *)vmt_node->cur;
        if (mi == NULL || mi->mangled_name == NULL)
            continue;
        if (mi->resolved_mangled_id != NULL)
            continue;  /* Already resolved (e.g. cloned from parent) */

        /* Primary lookup: direct symbol table lookup by mangled_name */
        HashNode_t *func_sym = NULL;
        if (FindSymbol(&func_sym, symtab, mi->mangled_name) != 0 &&
            func_sym != NULL && func_sym->mangled_id != NULL &&
            func_sym->type != NULL &&
            func_sym->type->kind == TYPE_KIND_PROCEDURE &&
            func_sym->type->info.proc_info.definition != NULL) {
            mi->resolved_mangled_id = strdup(func_sym->mangled_id);
            continue;
        }

        /* Fallback: search by class_name + method_name with param matching */
        if (mi->name == NULL)
            continue;
        char *base_name = make_method_lookup_key(class_name, mi->name);
        if (base_name == NULL)
            continue;
        ListNode_t *matches = FindAllIdents(symtab, base_name);

        /* Priority 0: exact mangled_id match — only for defined (non-abstract) functions
         * to avoid pointing VMT slots to labels that are never emitted */
        for (ListNode_t *m = matches; m != NULL && mi->resolved_mangled_id == NULL; m = m->next) {
            HashNode_t *cand = (HashNode_t *)m->cur;
            if (cand == NULL || cand->mangled_id == NULL ||
                cand->type == NULL || cand->type->kind != TYPE_KIND_PROCEDURE)
                continue;
            if (strcmp(cand->mangled_id, mi->mangled_name) == 0 &&
                cand->type->info.proc_info.definition != NULL)
                mi->resolved_mangled_id = strdup(cand->mangled_id);
        }

        if (mi->resolved_mangled_id == NULL) {
        const char *unique_mangled = NULL;
        const char *def_candidate_mangled = NULL;
        const char *single_mangled = NULL;
        int unique_ok = 1;
        int match_count = 0;
        int def_match_count = 0;
        int exact_name_match = 0; /* mi->mangled_name exactly matched a passing candidate */
        for (ListNode_t *m = matches; m != NULL; m = m->next) {
            HashNode_t *cand = (HashNode_t *)m->cur;
            if (cand == NULL || cand->type == NULL ||
                cand->type->kind != TYPE_KIND_PROCEDURE)
                continue;
            /* Match by param signature if available */
            if (mi->param_sig != NULL) {
                char *cand_sig = semcheck_param_sig_from_params(
                    cand->type->info.proc_info.params, 1);
                int sig_match = (cand_sig != NULL && strcmp(cand_sig, mi->param_sig) == 0);
                free(cand_sig);
                if (!sig_match)
                    continue;
            }
            /* Match by param count */
            int count = ListLength(cand->type->info.proc_info.params);
            if (count > 0) count -= 1; /* drop implicit Self */
            if (mi->param_count >= 0 && count != mi->param_count)
                continue;

            match_count++;
            /* Track whether Phase-1 mangled_name exactly matches this candidate.
             * Phase 1 uses param_sig to find the correct declaration; if it found
             * mi->mangled_name, we should trust that label for the VMT slot. */
            if (cand->mangled_id != NULL && mi->mangled_name != NULL &&
                strcmp(cand->mangled_id, mi->mangled_name) == 0)
                exact_name_match = 1;
            const char *cand_mangled = cand->mangled_id;
            if (cand->type->info.proc_info.definition != NULL) {
                Tree_t *def = cand->type->info.proc_info.definition;
                if (def != NULL &&
                    def->tree_data.subprogram_data.mangled_id != NULL)
                    cand_mangled = def->tree_data.subprogram_data.mangled_id;
                def_match_count++;
                def_candidate_mangled = cand_mangled;
            }
            if (cand_mangled != NULL) {
                if (unique_mangled == NULL)
                    unique_mangled = cand_mangled;
                else if (strcmp(unique_mangled, cand_mangled) != 0)
                    unique_ok = 0;
            } else {
                unique_ok = 0;
            }
            if (match_count == 1)
                single_mangled = (cand->type->info.proc_info.definition != NULL)
                    ? cand_mangled : NULL;
        }
        /* If Phase 1 identified mi->mangled_name via param_sig matching, and that
         * name exists as a candidate, and there is at least one definition for this
         * method (possibly under a stale mangled_id from early type resolution),
         * trust mi->mangled_name — the implementation will be emitted under that label
         * once semcheck_subprograms processes it with fully resolved types. */
        if (exact_name_match && def_match_count > 0 && mi->mangled_name != NULL)
            mi->resolved_mangled_id = strdup(mi->mangled_name);
        else if (unique_ok && unique_mangled != NULL && def_match_count > 0)
            mi->resolved_mangled_id = strdup(unique_mangled);
        else if (def_match_count == 1 && def_candidate_mangled != NULL)
            mi->resolved_mangled_id = strdup(def_candidate_mangled);
        else if (match_count == 1 && single_mangled != NULL)
            mi->resolved_mangled_id = strdup(single_mangled);
        } /* end if (mi->resolved_mangled_id == NULL) */
        if (matches != NULL)
            DestroyList(matches);
        free(base_name);
    }
    
    
    /* Clean up class_methods list (shallow - we don't own the bindings) */
    while (class_methods != NULL) {
        ListNode_t *next = class_methods->next;
        free(class_methods);
        class_methods = next;
    }
    
    return 0;
}

static int semcheck_template_matches_binding(const struct MethodTemplate *tmpl,
    const ClassMethodBinding *binding)
{
    if (tmpl == NULL || binding == NULL || tmpl->name == NULL || binding->method_name == NULL)
        return 0;
    if (strcasecmp(binding->method_name, tmpl->name) != 0)
        return 0;

    int wanted_params = from_cparser_count_params_ast(tmpl->params_ast);
    if (wanted_params >= 0 && binding->param_count >= 0)
        return wanted_params == binding->param_count;
    return 1;
}

static int semcheck_template_matches_vmt_method(const struct MethodTemplate *tmpl,
    const struct MethodInfo *mi)
{
    if (tmpl == NULL || mi == NULL || tmpl->name == NULL || mi->name == NULL)
        return 0;
    if (strcasecmp(mi->name, tmpl->name) != 0)
        return 0;

    int wanted_params = from_cparser_count_params_ast(tmpl->params_ast);
    if (wanted_params >= 0 && mi->param_count >= 0)
        return wanted_params == mi->param_count;
    return 1;
}

static void semcheck_refresh_generic_specialization_vmts(SymTab_t *symtab,
    ListNode_t *type_decls)
{
    if (symtab == NULL || type_decls == NULL)
        return;

    for (ListNode_t *cur = type_decls; cur != NULL; cur = cur->next) {
        if (cur->type != LIST_TREE || cur->cur == NULL)
            continue;

        Tree_t *tree = (Tree_t *)cur->cur;
        if (tree->type != TREE_TYPE_DECL)
            continue;

        struct RecordType *record_info = NULL;
        const char *class_name = tree->tree_data.type_decl_data.id;

        if (tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS) {
            struct TypeAlias *alias = &tree->tree_data.type_decl_data.info.alias;
            if (alias->inline_record_type != NULL)
                record_info = alias->inline_record_type;
            else if (tree->tree_data.type_decl_data.kgpc_type != NULL) {
                KgpcType *alias_type = tree->tree_data.type_decl_data.kgpc_type;
                if (kgpc_type_is_record(alias_type))
                    record_info = kgpc_type_get_record(alias_type);
                else if (kgpc_type_is_pointer(alias_type) &&
                         alias_type->info.points_to != NULL &&
                         kgpc_type_is_record(alias_type->info.points_to))
                    record_info = kgpc_type_get_record(alias_type->info.points_to);
            }
        }

        if (record_info == NULL || !record_info->is_class ||
            record_info->method_templates == NULL || class_name == NULL)
            continue;

        for (ListNode_t *tmpl_cur = record_info->method_templates;
             tmpl_cur != NULL; tmpl_cur = tmpl_cur->next) {
            if (tmpl_cur->type != LIST_METHOD_TEMPLATE || tmpl_cur->cur == NULL)
                continue;

            struct MethodTemplate *tmpl = (struct MethodTemplate *)tmpl_cur->cur;
            if (tmpl->name == NULL || tmpl->is_interface_delegation)
                continue;

            ListNode_t *bindings = NULL;
            int method_count = 0;
            int already_registered = 0;
            get_class_methods(class_name, &bindings, &method_count);
            for (ListNode_t *bcur = bindings; bcur != NULL; bcur = bcur->next) {
                ClassMethodBinding *binding = (ClassMethodBinding *)bcur->cur;
                if (semcheck_template_matches_binding(tmpl, binding)) {
                    already_registered = 1;
                    break;
                }
            }
            if (bindings != NULL)
                DestroyList(bindings);

            if (!already_registered) {
                from_cparser_register_method_template(
                    class_name,
                    tmpl->name,
                    tmpl->is_virtual,
                    tmpl->is_override,
                    tmpl->is_static,
                    from_cparser_count_params_ast(tmpl->params_ast));
            }
        }

        build_class_vmt(symtab, record_info, class_name, tree->line_num);

        for (ListNode_t *tmpl_cur = record_info->method_templates;
             tmpl_cur != NULL; tmpl_cur = tmpl_cur->next) {
            if (tmpl_cur->type != LIST_METHOD_TEMPLATE || tmpl_cur->cur == NULL)
                continue;

            struct MethodTemplate *tmpl = (struct MethodTemplate *)tmpl_cur->cur;
            if (tmpl->name == NULL || (!tmpl->is_virtual && !tmpl->is_override))
                continue;

            char *base_name = make_method_lookup_key(class_name, tmpl->name);
            if (base_name == NULL)
                continue;

            const char *resolved_id = NULL;
            int wanted_params = from_cparser_count_params_ast(tmpl->params_ast);
            ListNode_t *matches = FindAllIdents(symtab, base_name);
            for (ListNode_t *m = matches; m != NULL; m = m->next) {
                HashNode_t *cand = (HashNode_t *)m->cur;
                if (cand == NULL || cand->type == NULL ||
                    cand->type->kind != TYPE_KIND_PROCEDURE)
                    continue;
                if (cand->type->info.proc_info.definition == NULL)
                    continue;
                int count = ListLength(cand->type->info.proc_info.params);
                if (!tmpl->is_static && count > 0)
                    count -= 1;
                if (count != wanted_params)
                    continue;
                resolved_id = cand->mangled_id;
                if (cand->type->info.proc_info.definition != NULL &&
                    cand->type->info.proc_info.definition->tree_data.subprogram_data.mangled_id != NULL)
                    resolved_id = cand->type->info.proc_info.definition->tree_data.subprogram_data.mangled_id;
                break;
            }
            if (matches != NULL)
                DestroyList(matches);
            free(base_name);

            if (resolved_id == NULL)
                continue;

            for (ListNode_t *vmt_node = record_info->methods; vmt_node != NULL; vmt_node = vmt_node->next) {
                struct MethodInfo *mi = (struct MethodInfo *)vmt_node->cur;
                if (!semcheck_template_matches_vmt_method(tmpl, mi))
                    continue;
                free(mi->mangled_name);
                mi->mangled_name = strdup(resolved_id);
                if (mi->resolved_mangled_id != NULL)
                    free(mi->resolved_mangled_id);
                mi->resolved_mangled_id = strdup(resolved_id);
                mi->is_override = 1;
                break;
            }
        }
    }
}

/* Helper function to resolve constant identifier to integer value
 * Returns 0 on success, 1 on failure */
static int resolve_const_identifier(SymTab_t *symtab, const char *id, long long *out_value)
{
    if (symtab == NULL || id == NULL || out_value == NULL)
        return 1;
    
    HashNode_t *node = NULL;
    if (FindSymbol(&node, symtab, id) != 0 && 
        node != NULL && (node->hash_type == HASHTYPE_CONST || node->is_typed_const))
    {
        *out_value = node->const_int_value;
        return 0;
    }
    
    return 1;
}

static int semcheck_try_resolve_enum_literal_by_base(SymTab_t *symtab, const char *base_name,
    const char *literal_name, long long *out_value)
{
    if (symtab == NULL || base_name == NULL || literal_name == NULL || out_value == NULL)
        return 0;

    ListNode_t *matches = FindAllIdents(symtab, base_name);
    ListNode_t *cur = matches;
    while (cur != NULL)
    {
        HashNode_t *node = (HashNode_t *)cur->cur;
        if (node != NULL && node->hash_type == HASHTYPE_TYPE && node->type != NULL)
        {
            struct TypeAlias *alias = kgpc_type_get_type_alias(node->type);
            if (alias != NULL && alias->is_enum && alias->enum_literals != NULL)
            {
                int ordinal = 0;
                ListNode_t *lit = alias->enum_literals;
                while (lit != NULL)
                {
                    if (lit->cur != NULL &&
                        pascal_identifier_equals((char *)lit->cur, literal_name))
                    {
                        *out_value = ordinal;
                        if (matches != NULL)
                            DestroyList(matches);
                        return 1;
                    }
                    ++ordinal;
                    lit = lit->next;
                }
            }
        }
        cur = cur->next;
    }
    if (matches != NULL)
        DestroyList(matches);
    return 0;
}

static char *semcheck_append_param_sig(char *sig, const char *type_str)
{
    const char *part = (type_str != NULL) ? type_str : "<unknown>";
    size_t part_len = strlen(part);
    if (sig == NULL)
    {
        char *out = (char *)malloc(part_len + 1);
        if (out == NULL)
            return NULL;
        memcpy(out, part, part_len + 1);
        return out;
    }
    size_t sig_len = strlen(sig);
    char *out = (char *)realloc(sig, sig_len + 1 + part_len + 1);
    if (out == NULL)
    {
        free(sig);
        return NULL;
    }
    out[sig_len] = ',';
    memcpy(out + sig_len + 1, part, part_len + 1);
    return out;
}

static char *semcheck_param_sig_from_params(ListNode_t *params, int skip_first_param)
{
    if (params == NULL)
        return NULL;

    char *sig = NULL;
    ListNode_t *cur = params;
    int skipped = 0;
    while (cur != NULL)
    {
        Tree_t *param = (Tree_t *)cur->cur;
        cur = cur->next;
        if (skip_first_param && !skipped)
        {
            skipped = 1;
            continue;
        }
        if (param == NULL)
            continue;

        char *type_str = NULL;
        int name_count = 1;
        if (param->type == TREE_VAR_DECL)
        {
            if (param->tree_data.var_decl_data.type_ref != NULL)
                type_str = type_ref_render_mangled(param->tree_data.var_decl_data.type_ref);
            else if (param->tree_data.var_decl_data.type_id != NULL)
                type_str = strdup(param->tree_data.var_decl_data.type_id);
            if (param->tree_data.var_decl_data.ids != NULL)
                name_count = ListLength(param->tree_data.var_decl_data.ids);
        }
        else if (param->type == TREE_ARR_DECL)
        {
            if (param->tree_data.arr_decl_data.type_ref != NULL)
                type_str = type_ref_render_mangled(param->tree_data.arr_decl_data.type_ref);
            else if (param->tree_data.arr_decl_data.type_id != NULL)
                type_str = strdup(param->tree_data.arr_decl_data.type_id);
            if (param->tree_data.arr_decl_data.ids != NULL)
                name_count = ListLength(param->tree_data.arr_decl_data.ids);
        }

        if (name_count <= 0)
            name_count = 1;
        for (int i = 0; i < name_count; i++)
            sig = semcheck_append_param_sig(sig, type_str);

        if (type_str != NULL)
            free(type_str);
    }

    return sig;
}

int semcheck_resolve_scoped_enum_literal(SymTab_t *symtab, const char *type_name,
    const char *literal_name, long long *out_value)
{
    if (symtab == NULL || type_name == NULL || literal_name == NULL || out_value == NULL)
        return 0;

    QualifiedIdent *qualified_ref = qualified_ident_from_dotted(type_name);
    if (qualified_ref != NULL && qualified_ref->count > 1)
    {
        int resolved = semcheck_resolve_scoped_enum_literal_ref(symtab, qualified_ref,
            literal_name, out_value);
        qualified_ident_free(qualified_ref);
        if (resolved)
            return 1;
    }
    else if (qualified_ref != NULL)
    {
        qualified_ident_free(qualified_ref);
    }

    const char *current_type = type_name;
    char *owned_type = NULL;
    for (int depth = 0; depth < 8; ++depth)
    {
        HashNode_t *type_node = NULL;
        ListNode_t *matches = FindAllIdents(symtab, current_type);
        HashNode_t *best_type_node = NULL;
        for (ListNode_t *cur = matches; cur != NULL; cur = cur->next)
        {
            HashNode_t *candidate = (HashNode_t *)cur->cur;
            if (candidate == NULL || candidate->hash_type != HASHTYPE_TYPE)
                continue;

            struct TypeAlias *candidate_alias = kgpc_type_get_type_alias(candidate->type);
            int has_enum_literals = (candidate_alias != NULL &&
                candidate_alias->is_enum &&
                candidate_alias->enum_literals != NULL);
            int has_alias_target = (candidate_alias != NULL &&
                (candidate_alias->target_type_ref != NULL ||
                 candidate_alias->target_type_id != NULL));
            int cur_unit_idx = symtab->current_scope != NULL ? symtab->current_scope->unit_index : 0;
            int unit_match = (cur_unit_idx > 0 &&
                candidate->source_unit_index == cur_unit_idx);

            if (best_type_node == NULL)
            {
                best_type_node = candidate;
                if (has_enum_literals && unit_match)
                    break;
                continue;
            }

            struct TypeAlias *best_alias = kgpc_type_get_type_alias(best_type_node->type);
            int best_has_enum_literals = (best_alias != NULL &&
                best_alias->is_enum &&
                best_alias->enum_literals != NULL);
            int best_has_alias_target = (best_alias != NULL &&
                (best_alias->target_type_ref != NULL ||
                 best_alias->target_type_id != NULL));
            int best_unit_match = (cur_unit_idx > 0 &&
                best_type_node->source_unit_index == cur_unit_idx);

            if ((has_enum_literals && !best_has_enum_literals) ||
                (unit_match && !best_unit_match) ||
                (has_alias_target && !best_has_alias_target))
            {
                best_type_node = candidate;
                if (has_enum_literals && unit_match)
                    break;
            }
        }
        if (matches != NULL)
            DestroyList(matches);
        type_node = best_type_node;

        if (type_node == NULL || type_node->hash_type != HASHTYPE_TYPE)
        {
            QualifiedIdent *qid = qualified_ident_from_dotted(current_type);
            if (qid != NULL && qid->count > 1)
            {
                const char *base_name = qualified_ident_last(qid);
                if (base_name != NULL &&
                    semcheck_try_resolve_enum_literal_by_base(symtab, base_name, literal_name, out_value))
                {
                    qualified_ident_free(qid);
                    if (owned_type != NULL)
                        free(owned_type);
                    return 1;
                }
            }
            if (qid != NULL)
                qualified_ident_free(qid);

            const char *base = semcheck_base_type_name(current_type);
            if (base == NULL || base == current_type ||
                FindSymbol(&type_node, symtab, base) == 0 || type_node == NULL ||
                type_node->hash_type != HASHTYPE_TYPE)
            {
                break;
            }
        }

        if (type_node->type == NULL)
            return 0;
        struct TypeAlias *alias = kgpc_type_get_type_alias(type_node->type);
        if (alias != NULL && alias->is_enum && alias->enum_literals != NULL)
        {
            int ordinal = 0;
            ListNode_t *literal_node = alias->enum_literals;
            while (literal_node != NULL)
            {
                if (literal_node->cur != NULL &&
                    pascal_identifier_equals((char *)literal_node->cur, literal_name))
                {
                    *out_value = ordinal;
                    return 1;
                }
                ++ordinal;
                literal_node = literal_node->next;
            }
            return 0;
        }

        if (alias != NULL && alias->target_type_ref != NULL &&
            alias->target_type_ref->name != NULL)
        {
            char *qualified = qualified_ident_join(alias->target_type_ref->name, ".");
            if (qualified != NULL && !pascal_identifier_equals(qualified, current_type))
            {
                if (owned_type != NULL)
                    free(owned_type);
                owned_type = qualified;
                current_type = owned_type;
                continue;
            }
            free(qualified);
        }

        if (alias == NULL || alias->target_type_id == NULL ||
            pascal_identifier_equals(alias->target_type_id, current_type))
            break;
        current_type = alias->target_type_id;
    }

    if (owned_type != NULL)
        free(owned_type);
    return 0;
}

int semcheck_resolve_scoped_enum_literal_ref(SymTab_t *symtab, const QualifiedIdent *type_ref,
    const char *literal_name, long long *out_value)
{
    if (symtab == NULL || type_ref == NULL || literal_name == NULL || out_value == NULL)
        return 0;

    const QualifiedIdent *current_ref = type_ref;
    QualifiedIdent *owned_ref = NULL;
    for (int depth = 0; depth < 8; ++depth)
    {
        HashNode_t *type_node = semcheck_find_exact_type_node_for_qid(symtab, current_ref);
        if (type_node == NULL || type_node->hash_type != HASHTYPE_TYPE)
            break;

        if (type_node->type == NULL)
            return 0;
        struct TypeAlias *alias = kgpc_type_get_type_alias(type_node->type);
        if (alias != NULL && alias->is_enum && alias->enum_literals != NULL)
        {
            int ordinal = 0;
            ListNode_t *literal_node = alias->enum_literals;
            while (literal_node != NULL)
            {
                if (literal_node->cur != NULL &&
                    pascal_identifier_equals((char *)literal_node->cur, literal_name))
                {
                    *out_value = ordinal;
                    return 1;
                }
                ++ordinal;
                literal_node = literal_node->next;
            }
            return 0;
        }

        if (alias != NULL && alias->target_type_ref != NULL &&
            alias->target_type_ref->name != NULL &&
            !qualified_ident_equals_ci(alias->target_type_ref->name, current_ref))
        {
            if (owned_ref != NULL)
            {
                qualified_ident_free(owned_ref);
                owned_ref = NULL;
            }
            owned_ref = qualified_ident_clone(alias->target_type_ref->name);
            if (owned_ref == NULL)
                break;
            current_ref = owned_ref;
            continue;
        }

        if (alias != NULL && alias->target_type_ref != NULL &&
            alias->target_type_ref->name != NULL)
        {
            const char *base = qualified_ident_last(alias->target_type_ref->name);
            if (base != NULL &&
                semcheck_try_resolve_enum_literal_by_base(symtab, base, literal_name, out_value))
            {
                if (owned_ref != NULL)
                    qualified_ident_free(owned_ref);
                return 1;
            }
        }

        if (alias == NULL || alias->target_type_id == NULL)
            break;
        if (alias->target_type_ref != NULL && alias->target_type_ref->name != NULL)
        {
            char *current_name = qualified_ident_join(current_ref, ".");
            if (current_name != NULL)
            {
                int same_target = pascal_identifier_equals(alias->target_type_id, current_name);
                free(current_name);
                if (same_target)
                    break;
            }
        }
        /* Fall back to string-based resolution for non-structured aliases. */
        if (owned_ref != NULL)
        {
            qualified_ident_free(owned_ref);
            owned_ref = NULL;
        }
        int resolved = semcheck_resolve_scoped_enum_literal(symtab, alias->target_type_id,
            literal_name, out_value);
        return resolved;
    }

    if (owned_ref != NULL)
        qualified_ident_free(owned_ref);
    return 0;
}

static char *build_qualified_identifier_from_expr(struct Expression *expr)
{
    if (expr == NULL)
        return NULL;
    if (expr->type == EXPR_VAR_ID && expr->expr_data.id != NULL)
        return strdup(expr->expr_data.id);
    if (expr->type != EXPR_RECORD_ACCESS)
        return NULL;

    struct Expression *record_expr = expr->expr_data.record_access_data.record_expr;
    char *field_id = expr->expr_data.record_access_data.field_id;
    if (record_expr == NULL || field_id == NULL)
        return NULL;

    char *base = build_qualified_identifier_from_expr(record_expr);
    if (base == NULL)
        return NULL;
    size_t qualified_len = strlen(base) + 1 + strlen(field_id) + 1;
    char *qualified = (char *)malloc(qualified_len);
    if (qualified != NULL)
        snprintf(qualified, qualified_len, "%s.%s", base, field_id);
    free(base);
    return qualified;
}

static QualifiedIdent *build_qualified_ident_from_expr(struct Expression *expr)
{
    if (expr == NULL)
        return NULL;
    if (expr->type == EXPR_VAR_ID)
    {
        if (expr->id_ref != NULL)
            return qualified_ident_clone(expr->id_ref);
        if (expr->expr_data.id != NULL)
            return qualified_ident_from_single(expr->expr_data.id);
        return NULL;
    }
    if (expr->type != EXPR_RECORD_ACCESS)
        return NULL;

    struct Expression *record_expr = expr->expr_data.record_access_data.record_expr;
    char *field_id = expr->expr_data.record_access_data.field_id;
    if (record_expr == NULL || field_id == NULL)
        return NULL;

    QualifiedIdent *base = build_qualified_ident_from_expr(record_expr);
    if (base == NULL || base->count <= 0 || base->segments == NULL)
    {
        qualified_ident_free(base);
        return NULL;
    }

    int new_count = base->count + 1;
    char **segments = (char **)calloc((size_t)new_count, sizeof(char *));
    if (segments == NULL)
    {
        qualified_ident_free(base);
        return NULL;
    }
    for (int i = 0; i < base->count; ++i)
    {
        if (base->segments[i] != NULL)
            segments[i] = strdup(base->segments[i]);
    }
    segments[new_count - 1] = strdup(field_id);

    QualifiedIdent *out = qualified_ident_from_segments(segments, new_count, 1);
    if (out == NULL)
    {
        for (int i = 0; i < new_count; ++i)
            free(segments[i]);
        free(segments);
    }
    qualified_ident_free(base);
    return out;
}

static void sync_alias_array_bounds(KgpcType *kgpc_type, struct TypeAlias *alias)
{
    if (kgpc_type == NULL || alias == NULL)
        return;
    if (kgpc_type->type_alias != NULL && kgpc_type->type_alias != alias)
    {
        kgpc_type->type_alias->array_start = alias->array_start;
        kgpc_type->type_alias->array_end = alias->array_end;
        kgpc_type->type_alias->is_open_array = alias->is_open_array;
    }
}

static void sync_alias_range_bounds(KgpcType *kgpc_type, struct TypeAlias *alias)
{
    if (kgpc_type == NULL || alias == NULL)
        return;
    if (kgpc_type->type_alias != NULL && kgpc_type->type_alias != alias)
    {
        kgpc_type->type_alias->is_range = alias->is_range;
        kgpc_type->type_alias->range_known = alias->range_known;
        kgpc_type->type_alias->range_start = alias->range_start;
        kgpc_type->type_alias->range_end = alias->range_end;
    }
}

static void resolve_alias_range_bounds_in_kgpctype(SymTab_t *symtab, KgpcType *kgpc_type,
    struct TypeAlias *alias)
{
    if (symtab == NULL || alias == NULL || !alias->is_range || alias->range_known)
        return;

    const char *start_str = alias->range_start_str;
    const char *end_str = alias->range_end_str;
    if (start_str == NULL || end_str == NULL)
        return;

    while (*start_str == ' ' || *start_str == '\t')
        start_str++;
    while (*end_str == ' ' || *end_str == '\t')
        end_str++;

    long long start_val = 0;
    long long end_val = 0;
    int start_resolved = 0;
    int end_resolved = 0;

    if (resolve_const_identifier(symtab, start_str, &start_val) == 0)
    {
        start_resolved = 1;
    }
    else
    {
        int tmp_val = 0;
        if (resolve_array_bound_expr(symtab, start_str, &tmp_val) == 0)
        {
            start_val = tmp_val;
            start_resolved = 1;
        }
        else
        {
            char *endptr = NULL;
            long long parsed = strtoll(start_str, &endptr, 10);
            if (endptr != start_str && *endptr == '\0')
            {
                start_val = parsed;
                start_resolved = 1;
            }
        }
    }

    if (resolve_const_identifier(symtab, end_str, &end_val) == 0)
    {
        end_resolved = 1;
    }
    else
    {
        int tmp_val = 0;
        if (resolve_array_bound_expr(symtab, end_str, &tmp_val) == 0)
        {
            end_val = tmp_val;
            end_resolved = 1;
        }
        else
        {
            char *endptr = NULL;
            long long parsed = strtoll(end_str, &endptr, 10);
            if (endptr != end_str && *endptr == '\0')
            {
                end_val = parsed;
                end_resolved = 1;
            }
        }
    }

    if (!start_resolved || !end_resolved)
        return;

    alias->range_known = 1;
    alias->range_start = start_val;
    alias->range_end = end_val;
    if (alias->storage_size <= 0)
    {
        if (start_val >= -128 && end_val <= 127)
            alias->storage_size = 1;
        else if (start_val >= -32768 && end_val <= 32767)
            alias->storage_size = 2;
        else if (start_val >= INT32_MIN && end_val <= INT32_MAX)
            alias->storage_size = 4;
        else
            alias->storage_size = 8;
    }
    sync_alias_range_bounds(kgpc_type, alias);
}

/* Resolves array bounds specified as constant identifiers in a KgpcType
 * This is needed because parsing happens before constants are declared */
static void resolve_array_bounds_in_kgpctype(SymTab_t *symtab, KgpcType *kgpc_type, struct TypeAlias *alias)
{
    if (kgpc_type == NULL || alias == NULL || symtab == NULL)
        return;
    
    /* Only process array types */
    if (kgpc_type->kind != TYPE_KIND_ARRAY)
        return;
    
    /* Check if we have array_dimensions with symbolic bounds */
    if (alias->array_dimensions != NULL)
    {
        /* Use structured bounds from parser if available */
        const char *start_str_ref = alias->array_dim_start_str;
        const char *end_str_ref = alias->array_dim_end_str;
        char *start_str_alloc = NULL;
        char *end_str_alloc = NULL;
        /* No fallback: all dimension paths in from_cparser.c now set
         * array_dim_start_str/array_dim_end_str structurally. */
        if (start_str_ref != NULL && end_str_ref != NULL)
        {
            const char *start_str = start_str_ref;
            const char *end_str = end_str_ref;
                    if (kgpc_getenv("KGPC_DEBUG_ARRAY_BOUNDS") != NULL)
                        fprintf(stderr, "[KGPC] array dim start='%s' end='%s'\n",
                            start_str, end_str);

                    /* Trim whitespace */
                    while (*start_str == ' ') start_str++;
                    while (*end_str == ' ') end_str++;
                    
                    /* Try to resolve as constants or parse as integers */
                    long long start_val = 0;
                    long long end_val = 0;
                    int start_resolved = 0;
                    int end_resolved = 0;
                    
                    /* Try constant lookup first */
                    int tmp_val = 0;
                    int tmp_resolved = (resolve_array_bound_expr(symtab, start_str, &tmp_val) == 0);
                    if (resolve_const_identifier(symtab, start_str, &start_val) == 0 || tmp_resolved)
                    {
                        if (tmp_resolved)
                            start_val = tmp_val;
                        start_resolved = 1;
                    }
                    else
                    {
                        /* Try parsing as integer */
                        char *endptr;
                        long val = strtol(start_str, &endptr, 10);
                        if (endptr != start_str && *endptr == '\0')
                        {
                            start_val = val;
                            start_resolved = 1;
                        }
                    }
                    
                    int tmp_end_val = 0;
                    int tmp_end_resolved = (resolve_array_bound_expr(symtab, end_str, &tmp_end_val) == 0);
                    if (resolve_const_identifier(symtab, end_str, &end_val) == 0 || tmp_end_resolved)
                    {
                        if (tmp_end_resolved)
                            end_val = tmp_end_val;
                        end_resolved = 1;
                    }
                    else
                    {
                        /* Try parsing as integer */
                        char *endptr;
                        long val = strtol(end_str, &endptr, 10);
                        if (endptr != end_str && *endptr == '\0')
                        {
                            end_val = val;
                            end_resolved = 1;
                        }
                    }
                    
                    /* Update KgpcType with resolved bounds */
                    if (start_resolved && end_resolved)
                    {
                        kgpc_type->info.array_info.start_index = (int)start_val;
                        kgpc_type->info.array_info.end_index = (int)end_val;
                        
                        /* Also update the TypeAlias for consistency */
                        alias->array_start = (int)start_val;
                        alias->array_end = (int)end_val;
                        
                        /* Re-evaluate is_open_array based on resolved bounds */
                        alias->is_open_array = (alias->array_end < alias->array_start);
                        sync_alias_array_bounds(kgpc_type, alias);
                    }
                    
            free(start_str_alloc);
            free(end_str_alloc);
        }
        else
        {
            /* Single identifier dimension (e.g., array[Boolean] or array[MyEnum]) */
            ListNode_t *first_dim = alias->array_dimensions;
            const char *dim_str = (first_dim != NULL && first_dim->type == LIST_STRING) ?
                (const char *)first_dim->cur : NULL;
            HashNode_t *type_node = NULL;
            if (dim_str != NULL &&
                FindSymbol(&type_node, symtab, dim_str) != 0 && type_node != NULL &&
                    type_node->hash_type == HASHTYPE_TYPE)
                {
                    struct TypeAlias *type_alias = get_type_alias_from_node(type_node);
                    if (type_alias != NULL)
                    {
                        if (type_alias->is_enum && type_alias->enum_literals != NULL)
                        {
                            int count = ListLength(type_alias->enum_literals);
                            if (count > 0)
                            {
                                kgpc_type->info.array_info.start_index = 0;
                                kgpc_type->info.array_info.end_index = count - 1;
                                alias->array_start = 0;
                                alias->array_end = count - 1;
                                alias->is_open_array = 0;
                                sync_alias_array_bounds(kgpc_type, alias);
                            }
                        }
                        else if (type_alias->is_range && type_alias->range_known)
                        {
                            kgpc_type->info.array_info.start_index = type_alias->range_start;
                            kgpc_type->info.array_info.end_index = type_alias->range_end;
                            alias->array_start = type_alias->range_start;
                            alias->array_end = type_alias->range_end;
                            alias->is_open_array = 0;
                            sync_alias_array_bounds(kgpc_type, alias);
                        }
                    }
                }
                else if (pascal_identifier_equals(dim_str, "Boolean"))
                {
                    kgpc_type->info.array_info.start_index = 0;
                    kgpc_type->info.array_info.end_index = 1;
                    alias->array_start = 0;
                    alias->array_end = 1;
                    alias->is_open_array = 0;
                    sync_alias_array_bounds(kgpc_type, alias);
                }
                else if (pascal_identifier_equals(dim_str, "Char") ||
                         pascal_identifier_equals(dim_str, "AnsiChar"))
                {
                    kgpc_type->info.array_info.start_index = 0;
                    kgpc_type->info.array_info.end_index = 255;
                    alias->array_start = 0;
                    alias->array_end = 255;
                    alias->is_open_array = 0;
                    sync_alias_array_bounds(kgpc_type, alias);
                }
            }
        }
    }

int semcheck_type_decls(SymTab_t *symtab, ListNode_t *type_decls)
{
    ListNode_t *cur;
    Tree_t *tree;
    int return_val, func_return = 0;
    enum VarType var_type;

    assert(symtab != NULL);

    return_val = 0;
    cur = type_decls;
    while(cur != NULL)
    {
        int loop_start_errors = return_val;
        assert(cur->cur != NULL);
        assert(cur->type == LIST_TREE);
        tree = (Tree_t *)cur->cur;
        assert(tree->type == TREE_TYPE_DECL);

        ScopeNode *saved_scope_for_type = semcheck_switch_to_unit_scope(
            symtab, tree->tree_data.type_decl_data.source_unit_index);

        /* Set error context to the type's source unit so that errors
         * (e.g. parent class not found) report the correct file/line
         * instead of the main program's file. */
        const char *saved_error_unit_name = g_semcheck_error_unit_name;
        int saved_error_source_index = g_semcheck_error_source_index;
        if (tree->tree_data.type_decl_data.defined_in_unit &&
            tree->tree_data.type_decl_data.source_unit_index > 0)
        {
            const char *uname = unit_registry_get(tree->tree_data.type_decl_data.source_unit_index);
            if (uname != NULL)
            {
                g_semcheck_error_unit_name = uname;
                /* Reset source_index so resolve_unit_error_location uses
                 * the AST line_num and finds the file path by unit name
                 * instead of by byte offset into the wrong buffer. */
                g_semcheck_error_source_index = -1;
            }
        }

        const char *debug_pss = kgpc_getenv("KGPC_DEBUG_PSHORTSTRING");
        if (debug_pss != NULL &&
            tree->tree_data.type_decl_data.id != NULL &&
            pascal_identifier_equals(tree->tree_data.type_decl_data.id, "PShortString"))
        {
            struct TypeAlias *a = &tree->tree_data.type_decl_data.info.alias;
            fprintf(stderr,
                "[PShortString] decl kind=%d is_pointer=%d pointer_type_id=%s target=%s\n",
                tree->tree_data.type_decl_data.kind,
                a->is_pointer,
                a->pointer_type_id ? a->pointer_type_id : "<null>",
                a->target_type_id ? a->target_type_id : "<null>");
        }
        if (kgpc_getenv("KGPC_DEBUG_TFLOAT") != NULL &&
            tree->tree_data.type_decl_data.id != NULL &&
            pascal_identifier_equals(tree->tree_data.type_decl_data.id, "TFloatFormatProfile"))
        {
            fprintf(stderr, "[TFLOAT] semcheck_type_decls kind=%d defined_in_unit=%d\n",
                tree->tree_data.type_decl_data.kind,
                tree->tree_data.type_decl_data.defined_in_unit);
        }
#ifdef DEBUG
        fprintf(stderr, "DEBUG: semcheck_type_decls processing type: %s kind=%d\n",
            tree->tree_data.type_decl_data.id ? tree->tree_data.type_decl_data.id : "<null>",
            tree->tree_data.type_decl_data.kind);
#endif

        const char *debug_env_check = kgpc_getenv("KGPC_DEBUG_TFPG");
        if (debug_env_check != NULL)
        {
            fprintf(stderr, "[KGPC] semcheck_type_decls processing: id=%s kind=%d return_val=%d\n",
                tree->tree_data.type_decl_data.id ? tree->tree_data.type_decl_data.id : "<null>",
                tree->tree_data.type_decl_data.kind, loop_start_errors);
        }

        int before_switch_errors = return_val;

        struct RecordType *record_info = NULL;
        struct TypeAlias *alias_info = NULL;
        

        switch (tree->tree_data.type_decl_data.kind)
        {
            case TYPE_DECL_RECORD:
                var_type = HASHVAR_RECORD;
                record_info = semcheck_record_from_type_decl(tree, symtab);

                /* Set the type_id on the RecordType so operator overloading can find it */
                if (record_info != NULL && record_info->type_id == NULL && tree->tree_data.type_decl_data.id != NULL)
                {
                    record_info->type_id = strdup(tree->tree_data.type_decl_data.id);
                }

                semcheck_qualify_nested_types_for_record(symtab, record_info);

                if (record_info != NULL && record_info->is_type_helper)
                    semcheck_register_type_helper(record_info, symtab);

                /* In objfpc mode, classes without explicit parent inherit from TObject,
                 * unless this IS TObject itself (to avoid circular inheritance). */
                if (record_info != NULL && record_info->is_class &&
                    record_info->parent_class_name == NULL &&
                    pascal_frontend_is_objfpc_mode())
                {
                    const char *class_name = tree->tree_data.type_decl_data.id;
                    if (class_name == NULL || strcasecmp(class_name, "TObject") != 0)
                    {
                        record_info->parent_class_name = strdup("TObject");
                    }
                }

                /* Handle class inheritance - merge parent fields */
                if (record_info != NULL && record_info->parent_class_name != NULL)
                {
                    int merge_result = merge_parent_class_fields(symtab, record_info,
                                                                  tree->tree_data.type_decl_data.id,
                                                                  tree->line_num);
                    if (merge_result > 0)
                    {
                        return_val += merge_result;
                        /* Continue processing other type declarations even if this one failed */
                    }
                }
                else if (record_info != NULL && record_info->is_class)
                {
                    /* Classes without parents (like TObject) still need default indexed property detection */
                    detect_default_indexed_property(record_info, NULL);
                }
                else if (record_info != NULL && !record_info->is_class)
                {
                    /* Advanced records can declare default indexed properties too. */
                    detect_default_indexed_property(record_info, NULL);
                }

                /* For interfaces with a parent interface, prepend the parent's
                 * method_templates so that the full list (parent methods first)
                 * is available for vtable index assignment and codegen.  This
                 * replaces the recursive collection previously done in codegen. */
                if (record_info != NULL && record_info->is_interface &&
                    record_info->parent_class_name != NULL)
                {
                    HashNode_t *parent_iface_node = semcheck_find_preferred_type_node(
                        symtab, record_info->parent_class_name);
                    struct RecordType *parent_iface = parent_iface_node != NULL
                        ? get_record_type_from_node(parent_iface_node) : NULL;
                    if (parent_iface != NULL && parent_iface->is_interface &&
                        parent_iface->method_templates != NULL)
                    {
                        /* Clone parent's method_templates and prepend them.
                         * Parent's list already includes grandparent methods
                         * (processed earlier due to top-down declaration order). */
                        ListNode_t *cloned_head = NULL;
                        ListNode_t *cloned_tail = NULL;
                        for (ListNode_t *pm = parent_iface->method_templates; pm != NULL; pm = pm->next)
                        {
                            struct MethodTemplate *orig = (struct MethodTemplate *)pm->cur;
                            if (orig == NULL) continue;
                            /* Check if this method already exists in the current
                             * interface's own method_templates (override). */
                            int already_present = 0;
                            for (ListNode_t *own = record_info->method_templates; own != NULL; own = own->next)
                            {
                                struct MethodTemplate *own_mt = (struct MethodTemplate *)own->cur;
                                if (own_mt != NULL && own_mt->name != NULL && orig->name != NULL &&
                                    strcasecmp(own_mt->name, orig->name) == 0)
                                {
                                    already_present = 1;
                                    break;
                                }
                            }
                            if (already_present) continue;
                            struct MethodTemplate *cloned = clone_method_template_detached(orig);
                            if (cloned == NULL)
                                continue;
                            ListNode_t *node = CreateListNode(cloned, LIST_METHOD_TEMPLATE);
                            if (node == NULL)
                            {
                                free(cloned->name);
                                free(cloned->delegated_interface_name);
                                free(cloned->delegated_target_name);
                                free(cloned);
                                continue;
                            }
                            if (cloned_head == NULL) {
                                cloned_head = node;
                                cloned_tail = node;
                            } else {
                                cloned_tail->next = node;
                                cloned_tail = node;
                            }
                        }
                        if (cloned_head != NULL) {
                            /* Prepend: parent methods first, then own methods */
                            cloned_tail->next = record_info->method_templates;
                            record_info->method_templates = cloned_head;
                        }
                    }
                }

                /* Process method templates and register them with class method bindings.
                 * Methods declared in class type sections (e.g., abstract methods)
                 * may not have been registered during parsing if they were only forward
                 * declarations. We need to ensure they're registered for VMT building. */
                if (record_info->method_templates != NULL)
                {
                    ListNode_t *template_cur = record_info->method_templates;
                    while (template_cur != NULL)
                    {
                        if (template_cur->type == LIST_METHOD_TEMPLATE)
                        {
                            struct MethodTemplate *tmpl = (struct MethodTemplate *)template_cur->cur;
                            if (tmpl != NULL && tmpl->name != NULL &&
                                !tmpl->is_interface_delegation)
                            {
                                char *mangled = make_method_lookup_key(
                                    tree->tree_data.type_decl_data.id, tmpl->name);
                                if (mangled != NULL)
                                {

                                    /* Check if already registered in class_method_bindings */
                                    ListNode_t *bindings = NULL;
                                    int method_count = 0;
                                    get_class_methods(tree->tree_data.type_decl_data.id, &bindings, &method_count);
                                    
                                    int already_registered = 0;
                                    if (bindings != NULL)
                                    {
                                        if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                                            fprintf(stderr, "[SemCheck] Checking %d existing methods for %s\n",
                                                method_count, tree->tree_data.type_decl_data.id);
                                        }
                                        ListNode_t *cur = bindings;
                                        while (cur != NULL)
                                        {
                                            ClassMethodBinding *binding = (ClassMethodBinding *)cur->cur;
                                            if (binding != NULL && binding->method_name != NULL)
                                            {
                                                if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                                                    fprintf(stderr, "[SemCheck] Existing method: %s.%s\n",
                                                        binding->class_name, binding->method_name);
                                                }
                                                if (strcasecmp(binding->method_name, tmpl->name) == 0)
                                                {
                                                    already_registered = 1;
                                                    break;
                                                }
                                            }
                                            cur = cur->next;
                                        }
                                    }
                                    else
                                    {
                                        if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                                            fprintf(stderr, "[SemCheck] No existing methods found for %s\n",
                                                tree->tree_data.type_decl_data.id);
                                        }
                                    }
                                    DestroyList(bindings);

                                    /* If not already registered, register it now */
                                    if (!already_registered)
                                    {
                                        from_cparser_register_method_template(
                                            tree->tree_data.type_decl_data.id,
                                            tmpl->name,
                                            tmpl->is_virtual,
                                            tmpl->is_override,
                                            tmpl->is_static,
                                            from_cparser_count_params_ast(tmpl->params_ast));

                                        if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                                            fprintf(stderr, "[SemCheck] Registered method template: %s.%s (virtual=%d, override=%d, static=%d)\n",
                                                tree->tree_data.type_decl_data.id,
                                                tmpl->name,
                                                tmpl->is_virtual,
                                                tmpl->is_override,
                                                tmpl->is_static);
                                        }
                                    }

                                    /* Add as forward declaration to symbol table (allow overloads). */
                                    HashNode_t *existing = NULL;
                                    ListNode_t *existing_list = FindAllIdents(symtab, mangled);
                                    int has_matching_signature = 0;

                                    ListNode_t *params = NULL;
                                    if (tmpl->params_ast != NULL)
                                        params = from_cparser_convert_params_ast(tmpl->params_ast);
                                    if (!tmpl->is_static &&
                                        tmpl->kind != METHOD_TEMPLATE_OPERATOR)
                                    {
                                        const char *self_type_id = tree->tree_data.type_decl_data.id;
                                        int self_is_var = 1;
                                        if (record_info != NULL && record_info->is_class)
                                        {
                                            /* Class instances are already pointers; Self should not be var. */
                                            self_is_var = 0;
                                        }
                                        if (record_info != NULL && record_info->is_type_helper)
                                        {
                                            if (record_info->helper_base_type_id != NULL)
                                                self_type_id = record_info->helper_base_type_id;
                                            self_is_var = semcheck_helper_self_is_var(symtab, self_type_id);
                                        }
                                        ListNode_t *self_ids = CreateListNode(strdup("Self"), LIST_STRING);
                                        if (self_ids != NULL && self_type_id != NULL)
                                        {
                                            Tree_t *self_param = mk_vardecl(0, self_ids, UNKNOWN_TYPE,
                                                strdup(self_type_id), self_is_var, 0, NULL, NULL, NULL, NULL);
                                            ListNode_t *self_node = CreateListNode(self_param, LIST_TREE);
                                            if (self_node != NULL)
                                            {
                                                self_node->next = params;
                                                params = self_node;
                                            }
                                        }
                                    }

                                    if (existing_list != NULL)
                                    {
                                        ListNode_t *cur = existing_list;
                                        while (cur != NULL)
                                        {
                                            HashNode_t *candidate = (HashNode_t *)cur->cur;
                                            if (candidate != NULL && candidate->type != NULL &&
                                                candidate->type->kind == TYPE_KIND_PROCEDURE)
                                            {
                                                if (semcheck_param_list_equivalent(
                                                        candidate->type->info.proc_info.params, params))
                                                {
                                                    existing = candidate;
                                                    has_matching_signature = 1;
                                                    break;
                                                }
                                            }
                                            cur = cur->next;
                                        }
                                    }

                                    if (!has_matching_signature)
                                    {
                                        KgpcType *return_type = NULL;
                                        char *return_type_id = NULL;
                                        ast_t *return_type_node = tmpl->return_type_ast;
                                        if (return_type_node == NULL && tmpl->method_ast != NULL)
                                        {
                                            struct ast_t *method_ast = tmpl->method_ast;
                                            for (ast_t *child = method_ast->child; child != NULL; child = child->next)
                                            {
                                                if (child->typ == PASCAL_T_RETURN_TYPE)
                                                {
                                                    return_type_node = child;
                                                    if (child->sym != NULL && child->sym->name != NULL)
                                                        return_type_id = strdup(child->sym->name);
                                                    break;
                                                }
                                            }
                                        }
                                        int is_function_template = (tmpl->kind == METHOD_TEMPLATE_FUNCTION ||
                                            tmpl->kind == METHOD_TEMPLATE_CONSTRUCTOR ||
                                            tmpl->has_return_type ||
                                            return_type_node != NULL);

                                        if (return_type_node != NULL)
                                        {
                                            if (return_type_node->child != NULL)
                                                return_type = convert_type_spec_to_kgpctype(return_type_node->child, symtab);
                                            if (return_type_id == NULL)
                                                return_type_id = semcheck_dup_type_id_from_ast(return_type_node);
                                        }
                                        if (return_type_id != NULL)
                                        {
                                            const char *owner_full = (record_info != NULL && record_info->type_id != NULL)
                                                ? record_info->type_id : tree->tree_data.type_decl_data.id;
                                            const char *owner_outer = (record_info != NULL) ? record_info->outer_type_id : NULL;
                                            if (owner_full != NULL)
                                                semcheck_maybe_qualify_nested_type(symtab, owner_full, owner_outer,
                                                    &return_type_id, NULL);
                                        }
                                        if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL && tmpl->name != NULL &&
                                            (strcasecmp(tmpl->name, "GetAnsiString") == 0 ||
                                             strcasecmp(tmpl->name, "GetString") == 0))
                                        {
                                            fprintf(stderr,
                                                "[SemCheck] tmpl %s return_type_node=%p return_type_id=%s return_type=%p\n",
                                                tmpl->name,
                                                (void *)return_type_node,
                                                return_type_id != NULL ? return_type_id : "<null>",
                                                (void *)return_type);
                                        }
                                        if (return_type == NULL && return_type_id != NULL)
                                        {
                                            HashNode_t *type_node = NULL;
                                            if (FindSymbol(&type_node, symtab, return_type_id) != 0 &&
                                                type_node != NULL && type_node->type != NULL)
                                            {
                                                kgpc_type_retain(type_node->type);
                                                return_type = type_node->type;
                                            }
                                            else
                                            {
                                                int tag = semcheck_map_builtin_type_name_local(return_type_id);
                                                if (tag != UNKNOWN_TYPE)
                                                    return_type = create_primitive_type(tag);
                                            }
                                        }

                                        /* For constructors without explicit return type, set the
                                         * return type to a pointer to the owning class. This ensures
                                         * that constructor calls like ClassName.ppuload() return the
                                         * class instance type, not procedure/unknown. */
                                        if (return_type == NULL &&
                                            tmpl->kind == METHOD_TEMPLATE_CONSTRUCTOR &&
                                            record_info != NULL &&
                                            record_type_is_class(record_info))
                                        {
                                            KgpcType *rec_type = create_record_type(record_info);
                                            if (rec_type != NULL)
                                            {
                                                return_type = create_pointer_type(rec_type);
                                                /* Release local ref; create_pointer_type retained its own */
                                                kgpc_type_release(rec_type);
                                            }
                                        }

                                        KgpcType *proc_type = create_procedure_type(params, return_type);
                                        /* create_procedure_type retains return_type; release our ref */
                                        if (return_type != NULL)
                                            kgpc_type_release(return_type);
                                        if (proc_type != NULL) {
                                            if (return_type_id != NULL)
                                                proc_type->info.proc_info.return_type_id = strdup(return_type_id);
                                            /* Transfer param ownership to proc_type's shallow copy */
                                            proc_type->info.proc_info.owns_params = 1;
                                        }

                                        if (return_type_id != NULL)
                                            free(return_type_id);

                                        char *overload_mangled = MangleFunctionName(mangled, params, symtab);
                                        if (overload_mangled == NULL)
                                            overload_mangled = strdup(mangled);
                                        /* Free original param list nodes (data owned by proc_type now) */
                                        DestroyList(params);
                                        params = NULL;

                                        if (is_function_template)
                                            PushFunctionOntoScope_Typed(symtab, mangled, overload_mangled, proc_type);
                                        else
                                            PushProcedureOntoScope_Typed(symtab, mangled, overload_mangled, proc_type);

                                        /* Set method identity on the newly-pushed symbol */
                                        {
                                            HashNode_t *pushed_node = NULL;
                                            if (FindSymbol(&pushed_node, symtab, mangled) != 0 && pushed_node != NULL)
                                            {
                                                if (pushed_node->method_name == NULL && tmpl->name != NULL)
                                                    pushed_node->method_name = strdup(tmpl->name);
                                                if (pushed_node->owner_class == NULL)
                                                    pushed_node->owner_class = strdup(tree->tree_data.type_decl_data.id);
                                                if (tree->tree_data.type_decl_data.defined_in_unit)
                                                {
                                                    pushed_node->defined_in_unit = 1;
                                                    pushed_node->unit_is_public =
                                                        tree->tree_data.type_decl_data.unit_is_public ? 1 : 0;
                                                    mark_hashnode_source_unit(pushed_node,
                                                        tree->tree_data.type_decl_data.source_unit_index);
                                                }
                                            }
                                        }

                                        if (proc_type != NULL)
                                            destroy_kgpc_type(proc_type);
                                        free(overload_mangled);

                                        if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                                            fprintf(stderr, "[SemCheck] Added method forward declaration: %s -> %s\n",
                                                tmpl->name, mangled);
                                        }
                                    }
                                    else
                                    {
                                        int is_function_template = (tmpl->kind == METHOD_TEMPLATE_FUNCTION ||
                                            tmpl->kind == METHOD_TEMPLATE_CONSTRUCTOR ||
                                            tmpl->has_return_type ||
                                            tmpl->return_type_ast != NULL);
                                        if (is_function_template && existing != NULL &&
                                            existing->hash_type == HASHTYPE_PROCEDURE)
                                            existing->hash_type = HASHTYPE_FUNCTION;
                                        if (existing != NULL && existing->type != NULL &&
                                            existing->type->kind == TYPE_KIND_PROCEDURE &&
                                            existing->type->info.proc_info.return_type == NULL)
                                        {
                                            KgpcType *return_type = NULL;
                                            char *return_type_id = NULL;
                                        ast_t *return_type_node = tmpl->return_type_ast;
                                        if (return_type_node == NULL && tmpl->method_ast != NULL)
                                        {
                                            struct ast_t *method_ast = tmpl->method_ast;
                                            for (ast_t *child = method_ast->child; child != NULL; child = child->next)
                                            {
                                                if (child->typ == PASCAL_T_RETURN_TYPE)
                                                {
                                                    return_type_node = child;
                                                    if (child->sym != NULL && child->sym->name != NULL)
                                                        return_type_id = strdup(child->sym->name);
                                                    break;
                                                }
                                            }
                                        }

                                        if (return_type_node != NULL)
                                        {
                                            if (return_type_node->child != NULL)
                                                return_type = convert_type_spec_to_kgpctype(return_type_node->child, symtab);
                                            if (return_type_id == NULL)
                                                return_type_id = semcheck_dup_type_id_from_ast(return_type_node);
                                        }
                                        if (return_type_id != NULL)
                                        {
                                            const char *owner_full = (record_info != NULL && record_info->type_id != NULL)
                                                ? record_info->type_id : tree->tree_data.type_decl_data.id;
                                            const char *owner_outer = (record_info != NULL) ? record_info->outer_type_id : NULL;
                                            if (owner_full != NULL)
                                                semcheck_maybe_qualify_nested_type(symtab, owner_full, owner_outer,
                                                    &return_type_id, NULL);
                                        }
                                        if (return_type == NULL && return_type_id != NULL)
                                        {
                                            HashNode_t *type_node = NULL;
                                            if (FindSymbol(&type_node, symtab, return_type_id) != 0 &&
                                                    type_node != NULL && type_node->type != NULL)
                                                {
                                                    kgpc_type_retain(type_node->type);
                                                    return_type = type_node->type;
                                                }
                                                else
                                                {
                                                    int tag = semcheck_map_builtin_type_name_local(return_type_id);
                                                    if (tag != UNKNOWN_TYPE)
                                                        return_type = create_primitive_type(tag);
                                                }
                                            }

                                            if (return_type != NULL)
                                                existing->type->info.proc_info.return_type = return_type;
                                            if (return_type_id != NULL)
                                                free(return_type_id);
                                        }
                                        if (existing != NULL && existing->type != NULL &&
                                            existing->type->kind == TYPE_KIND_PROCEDURE &&
                                            existing->type->info.proc_info.params == NULL)
                                        {
                                            existing->type->info.proc_info.params = CopyListShallow(params);
                                            /* Transfer Tree_t ownership to existing type's shallow copy */
                                            existing->type->info.proc_info.owns_params = 1;
                                            DestroyList(params);
                                            params = NULL;
                                        }
                                        else if (existing != NULL && existing->type != NULL &&
                                            existing->type->kind == TYPE_KIND_PROCEDURE &&
                                            existing->type->info.proc_info.params != NULL &&
                                            params != NULL)
                                        {
                                            /* Keep defaults declared in the type/template declaration
                                             * when the same signature was already introduced by an
                                             * implementation header that omits them. */
                                            copy_default_values_to_impl_params(
                                                params,
                                                existing->type->info.proc_info.params);
                                            /* params not stored; free everything */
                                            destroy_list(params);
                                            params = NULL;
                                        }
                                        /* Free any remaining params not consumed above */
                                        if (params != NULL)
                                            destroy_list(params);
                                    }

                                    /* Set method identity on existing node */
                                    if (existing != NULL)
                                    {
                                        if (existing->method_name == NULL && tmpl->name != NULL)
                                            existing->method_name = strdup(tmpl->name);
                                        if (existing->owner_class == NULL)
                                            existing->owner_class = strdup(tree->tree_data.type_decl_data.id);
                                    }

                                    if (params != NULL)
                                        DestroyList(params);
                                    if (existing_list != NULL)
                                        DestroyList(existing_list);
                                    free(mangled);
                                }
                            }
                        }
                        template_cur = template_cur->next;
                    }
                }

                /* Interface methods have declarations but no implementations.
                 * Register synthetic symbols so method resolution can succeed. */
                if (record_info != NULL && record_info->is_interface &&
                    record_info->method_templates != NULL &&
                    record_info->type_id != NULL)
                {
                    ListNode_t *tmpl_cur = record_info->method_templates;
                    while (tmpl_cur != NULL)
                    {
                        if (tmpl_cur->type == LIST_METHOD_TEMPLATE)
                        {
                            struct MethodTemplate *tmpl = (struct MethodTemplate *)tmpl_cur->cur;
                            if (tmpl != NULL && tmpl->name != NULL)
                            {
                                size_t class_len = strlen(record_info->type_id);
                                size_t method_len = strlen(tmpl->name);
                                char *mangled = (char *)malloc(class_len + 2 + method_len + 1);
                                if (mangled != NULL)
                                {
                                    snprintf(mangled, class_len + 2 + method_len + 1,
                                        "%s__%s", record_info->type_id, tmpl->name);
                                    HashNode_t *existing = NULL;
                                    if (FindSymbol(&existing, symtab, mangled) == 0)
                                    {
                                        KgpcType *proc_type =
                                            from_cparser_method_template_to_proctype(tmpl, record_info, symtab);
                                        if (proc_type != NULL)
                                        {
                                            char *mangled_dup = strdup(mangled);
                                            if (proc_type->info.proc_info.return_type != NULL ||
                                                tmpl->has_return_type)
                                            {
                                                PushFunctionOntoScope_Typed(symtab, mangled, mangled_dup, proc_type);
                                            }
                                            else
                                            {
                                                PushProcedureOntoScope_Typed(symtab, mangled, mangled_dup, proc_type);
                                            }
                                            /* Set method identity on the newly-pushed symbol */
                                            {
                                                HashNode_t *pushed_node = NULL;
                                                if (FindSymbol(&pushed_node, symtab, mangled) != 0 && pushed_node != NULL)
                                                {
                                                    if (pushed_node->method_name == NULL && tmpl->name != NULL)
                                                        pushed_node->method_name = strdup(tmpl->name);
                                                    if (pushed_node->owner_class == NULL && record_info->type_id != NULL)
                                                        pushed_node->owner_class = strdup(record_info->type_id);
                                                }
                                            }
                                            mangled = NULL;
                                        }
                                    }
                                    else if (existing != NULL)
                                    {
                                        /* Set method identity on existing interface method node */
                                        if (existing->method_name == NULL && tmpl->name != NULL)
                                            existing->method_name = strdup(tmpl->name);
                                        if (existing->owner_class == NULL && record_info->type_id != NULL)
                                            existing->owner_class = strdup(record_info->type_id);
                                    }
                                    if (mangled != NULL)
                                        free(mangled);
                                }
                            }
                        }
                        tmpl_cur = tmpl_cur->next;
                    }
                }

                /* Now also check if this class has method implementations that need to be added.
                 * These are in the subprogram list (nested_subs of the class type decl) */
                if (tree->tree_data.type_decl_data.info.record != NULL &&
                    tree->tree_data.type_decl_data.info.record->methods != NULL)
                {
                    /* Methods have been processed - nothing more to do */
                }

                /* Build VMT for classes with virtual methods */
                if (record_info != NULL)
                {
                    int vmt_result = build_class_vmt(symtab, record_info,
                                                      tree->tree_data.type_decl_data.id,
                                                      tree->line_num);
                    if (vmt_result > 0)
                    {
                        return_val += vmt_result;
                    }

                    /* Skip size computation for generic templates (not yet specialized)
                     * Size can only be computed after type parameters are substituted */
                    int is_unspecialized_generic = (record_info->generic_decl != NULL && 
                                                     record_info->num_generic_args == 0);
                    
                    if (!is_unspecialized_generic)
                    {
                        long long record_size = 0;
                        int size_result = semcheck_compute_record_size(symtab, record_info, &record_size,
                                tree->line_num);
                        if (size_result != 0)
                        {
#ifdef DEBUG
                            fprintf(stderr, "DEBUG: semcheck_compute_record_size FAILED for %s: result=%d\n",
                                tree->tree_data.type_decl_data.id, size_result);
#endif
                            if (kgpc_getenv("KGPC_DEBUG_SEMSTEPS") != NULL)
                            {
                                fprintf(stderr, "[SemCheck] record size failed for %s (line %d)\n",
                                        tree->tree_data.type_decl_data.id ? tree->tree_data.type_decl_data.id : "<null>",
                                        tree->line_num);
                            }
                            return_val += 1;
                        }
                    }
                }
                break;
            case TYPE_DECL_ALIAS:
            {
                alias_info = &tree->tree_data.type_decl_data.info.alias;
                if (alias_info->alias_name == NULL && tree->tree_data.type_decl_data.id != NULL)
                    alias_info->alias_name = strdup(tree->tree_data.type_decl_data.id);

                if (alias_info->alias_name != NULL &&
                    (pascal_identifier_equals(alias_info->alias_name, "UnicodeString") ||
                     pascal_identifier_equals(alias_info->alias_name, "WideString")))
                {
                    alias_info->is_wide_string = 1;
                }
                else if (alias_info->target_type_id != NULL &&
                         (pascal_identifier_equals(alias_info->target_type_id, "UnicodeString") ||
                          pascal_identifier_equals(alias_info->target_type_id, "WideString")))
                {
                    alias_info->is_wide_string = 1;
                }
                
                alias_info->is_char_alias = semcheck_alias_should_be_char_like(
                    tree->tree_data.type_decl_data.id, alias_info->target_type_id) &&
                    !alias_info->is_pointer && !alias_info->is_array && !alias_info->is_set &&
                    !alias_info->is_enum && !alias_info->is_file;
                if (tree->tree_data.type_decl_data.kgpc_type == NULL &&
                    (alias_info->is_array || alias_info->is_pointer ||
                     alias_info->is_set || alias_info->is_file))
                {
                    KgpcType *alias_type = create_kgpc_type_from_type_alias(
                        alias_info, symtab, tree->tree_data.type_decl_data.defined_in_unit);
                    if (alias_type != NULL)
                    {
                        tree->tree_data.type_decl_data.kgpc_type = alias_type;
                        kgpc_type_retain(alias_type);
                    }
                }
                if (alias_info->inline_record_type != NULL)
                {
                    if (alias_info->inline_record_type->type_id == NULL &&
                        tree->tree_data.type_decl_data.id != NULL)
                    {
                        alias_info->inline_record_type->type_id =
                            strdup(tree->tree_data.type_decl_data.id);
                    }
                    if (tree->tree_data.type_decl_data.kgpc_type == NULL ||
                        (record_type_is_class(alias_info->inline_record_type) &&
                         !kgpc_type_is_pointer(tree->tree_data.type_decl_data.kgpc_type)))
                    {
                        KgpcType *inline_kgpc = create_record_type(alias_info->inline_record_type);
                        if (record_type_is_class(alias_info->inline_record_type))
                            inline_kgpc = create_pointer_type(inline_kgpc);
                        kgpc_type_set_type_alias(inline_kgpc, alias_info);
                        if (tree->tree_data.type_decl_data.kgpc_type != NULL)
                            destroy_kgpc_type(tree->tree_data.type_decl_data.kgpc_type);
                        tree->tree_data.type_decl_data.kgpc_type = inline_kgpc;
                        kgpc_type_retain(inline_kgpc);
                    }
                }
                if (alias_info->is_pointer &&
                    tree->tree_data.type_decl_data.kgpc_type != NULL &&
                    kgpc_type_is_pointer(tree->tree_data.type_decl_data.kgpc_type) &&
                    tree->tree_data.type_decl_data.kgpc_type->info.points_to == NULL)
                {
                    HashNode_t *pointee_node = NULL;
                    if (alias_info->pointer_type_ref != NULL)
                    {
                        pointee_node = semcheck_find_preferred_type_node_with_ref(symtab,
                            alias_info->pointer_type_ref, alias_info->pointer_type_id);
                    }
                    else if (alias_info->pointer_type_id != NULL)
                    {
                        pointee_node = semcheck_find_preferred_type_node(symtab,
                            alias_info->pointer_type_id);
                    }
                    if (pointee_node != NULL && pointee_node->type != NULL)
                    {
                        kgpc_type_retain(pointee_node->type);
                        tree->tree_data.type_decl_data.kgpc_type->info.points_to = pointee_node->type;
                    }
                }
                if (kgpc_getenv("KGPC_DEBUG_PROC_TYPE") != NULL &&
                    tree->tree_data.type_decl_data.id != NULL &&
                    pascal_identifier_equals(tree->tree_data.type_decl_data.id, "TCreateEncodingProc"))
                {
                    fprintf(stderr,
                        "[KGPC_DEBUG_PROC_TYPE] alias=%s base=%d kgpc=%s kind=%d return_id=%s\n",
                        tree->tree_data.type_decl_data.id,
                        alias_info->base_type,
                        tree->tree_data.type_decl_data.kgpc_type ?
                            kgpc_type_to_string(tree->tree_data.type_decl_data.kgpc_type) : "<null>",
                        tree->tree_data.type_decl_data.kgpc_type ?
                            tree->tree_data.type_decl_data.kgpc_type->kind : -1,
                        (tree->tree_data.type_decl_data.kgpc_type &&
                         tree->tree_data.type_decl_data.kgpc_type->kind == TYPE_KIND_PROCEDURE &&
                         tree->tree_data.type_decl_data.kgpc_type->info.proc_info.return_type_id) ?
                            tree->tree_data.type_decl_data.kgpc_type->info.proc_info.return_type_id : "<null>");
                }
                if (alias_info->is_array)
                {
                    int element_type = alias_info->array_element_type;
                    if (is_real_family_type(element_type))
                        var_type = HASHVAR_REAL;
                    else if (element_type == LONGINT_TYPE)
                        var_type = HASHVAR_LONGINT;
                    else if (element_type == STRING_TYPE)
                        var_type = HASHVAR_PCHAR;
                    else if (element_type == BOOL)
                        var_type = HASHVAR_BOOLEAN;
                    else if (element_type == CHAR_TYPE)
                        var_type = HASHVAR_CHAR;
                    else if (element_type == POINTER_TYPE)
                        var_type = HASHVAR_POINTER;
                    else if (element_type == SET_TYPE)
                        var_type = HASHVAR_SET;
                    else if (element_type == ENUM_TYPE)
                        var_type = HASHVAR_ENUM;
                    else if (element_type == FILE_TYPE)
                        var_type = HASHVAR_FILE;
                    else if (element_type == TEXT_TYPE)
                        var_type = HASHVAR_TEXT;
                    else
                        var_type = HASHVAR_INTEGER;
                }
                else
                {
                    int base_type = alias_info->base_type;
                    if (is_real_family_type(base_type))
                        var_type = HASHVAR_REAL;
                    else if (base_type == LONGINT_TYPE)
                        var_type = HASHVAR_LONGINT;
                    else if (base_type == STRING_TYPE)
                        var_type = HASHVAR_PCHAR;
                    else if (base_type == BOOL)
                        var_type = HASHVAR_BOOLEAN;
                    else if (base_type == CHAR_TYPE)
                        var_type = HASHVAR_CHAR;
                    else if (base_type == POINTER_TYPE)
                        var_type = HASHVAR_POINTER;
                    else if (base_type == SET_TYPE)
                        var_type = HASHVAR_SET;
                    else if (base_type == ENUM_TYPE)
                        var_type = HASHVAR_ENUM;
                    else if (base_type == FILE_TYPE)
                        var_type = HASHVAR_FILE;
                    else if (base_type == TEXT_TYPE)
                        var_type = HASHVAR_TEXT;
                    else if (base_type == INT_TYPE)
                        var_type = HASHVAR_INTEGER;
                    else if (base_type == PROCEDURE)
                        var_type = HASHVAR_PROCEDURE;
                    else
                        var_type = HASHVAR_UNTYPED;

                    if (alias_info->is_pointer)
                        var_type = HASHVAR_POINTER;
                    else if (alias_info->is_set)
                        var_type = HASHVAR_SET;
                    else if (alias_info->is_enum)
                        var_type = HASHVAR_ENUM;
                    else if (alias_info->is_file)
                        var_type = HASHVAR_FILE;

                    if (var_type == HASHVAR_UNTYPED &&
                        (alias_info->target_type_ref != NULL || alias_info->target_type_id != NULL))
                    {
                        HashNode_t *target_node = NULL;
                        if (alias_info->target_type_ref != NULL)
                            target_node = semcheck_find_preferred_type_node_with_ref(symtab,
                                alias_info->target_type_ref, alias_info->target_type_id);
                        else
                            target_node = semcheck_find_preferred_type_node(symtab,
                                alias_info->target_type_id);

                        if (target_node != NULL && target_node->type != NULL)
                        {
                            struct TypeAlias *target_alias = kgpc_type_get_type_alias(target_node->type);
                            if (target_alias == alias_info)
                            {
                                char *lookup = NULL;
                                if (alias_info->target_type_ref != NULL)
                                    lookup = type_ref_render_mangled(alias_info->target_type_ref);
                                const char *name = (lookup != NULL) ? lookup : alias_info->target_type_id;
                                HashNode_t *alt = semcheck_find_type_excluding_alias(symtab, name, alias_info);
                                if (alt != NULL)
                                    target_node = alt;
                                free(lookup);
                            }
                        }

                        if (target_node != NULL)
                        {
                            if (alias_info->target_type_ref != NULL &&
                                semcheck_is_explicit_unit_qualified_type_ref(alias_info->target_type_ref) &&
                                target_node->hash_type == HASHTYPE_TYPE &&
                                !target_node->defined_in_unit)
                            {
                                const char *unit_name =
                                    (alias_info->target_type_ref->name != NULL &&
                                     alias_info->target_type_ref->name->segments != NULL &&
                                     alias_info->target_type_ref->name->count > 0)
                                        ? alias_info->target_type_ref->name->segments[0]
                                        : NULL;
                                const char *base_name = type_ref_base_name(alias_info->target_type_ref);
                                if (base_name != NULL)
                                {
                                    ListNode_t *matches = FindAllIdents(symtab, base_name);
                                    HashNode_t *unit_match = NULL;
                                    for (ListNode_t *mcur = matches; mcur != NULL; mcur = mcur->next)
                                    {
                                        HashNode_t *candidate = (HashNode_t *)mcur->cur;
                                        if (candidate != NULL &&
                                            candidate->hash_type == HASHTYPE_TYPE)
                                        {
                                            if (unit_name != NULL)
                                            {
                                                const char *candidate_unit =
                                                    unit_registry_get(candidate->source_unit_index);
                                                if (candidate_unit != NULL &&
                                                    pascal_identifier_equals(candidate_unit, unit_name))
                                                {
                                                    unit_match = candidate;
                                                    break;
                                                }
                                            }
                                            if (unit_match == NULL && candidate->defined_in_unit)
                                                unit_match = candidate;
                                        }
                                    }
                                    if (unit_match != NULL)
                                        target_node = unit_match;
                                    if (matches != NULL)
                                        DestroyList(matches);
                                }
                            }

                            var_type = get_var_type_from_node(target_node);
                            if (target_node->type != NULL &&
                                !alias_info->is_array && !alias_info->is_pointer &&
                                !alias_info->is_set && !alias_info->is_enum &&
                                !alias_info->is_file && alias_info->inline_record_type == NULL)
                            {
                                KgpcType *existing = tree->tree_data.type_decl_data.kgpc_type;
                                int replace_existing = 0;
                                if (existing == NULL)
                                    replace_existing = 1;
                                else if (existing->kind == TYPE_KIND_PRIMITIVE &&
                                    existing->info.primitive_type_tag == UNKNOWN_TYPE)
                                    replace_existing = 1;
                                if (replace_existing)
                                {
                                    if (existing != NULL)
                                        destroy_kgpc_type(existing);
                                    tree->tree_data.type_decl_data.kgpc_type = target_node->type;
                                    kgpc_type_retain(target_node->type);
                                }
                            }
                        }
                    }
                }

                if (alias_info->base_type == RECORD_TYPE)
                {
                    struct RecordType *alias_record = NULL;
                    KgpcType *alias_type = tree->tree_data.type_decl_data.kgpc_type;

                    if (alias_info->inline_record_type != NULL)
                    {
                        alias_record = alias_info->inline_record_type;
                    }
                    else if (alias_type != NULL)
                    {
                        if (kgpc_type_is_record(alias_type))
                        {
                            alias_record = kgpc_type_get_record(alias_type);
                        }
                        else if (kgpc_type_is_pointer(alias_type))
                        {
                            KgpcType *pointee = alias_type->info.points_to;
                            if (pointee != NULL && kgpc_type_is_record(pointee))
                                alias_record = kgpc_type_get_record(pointee);
                        }
                    }

                    if (alias_record != NULL)
                    {
                        /* Set the type_id on the RecordType for operator overloading */
                        if (alias_record->type_id == NULL && tree->tree_data.type_decl_data.id != NULL)
                        {
                            alias_record->type_id = strdup(tree->tree_data.type_decl_data.id);
                        }

                        /* For generic specializations that are classes with a parent,
                         * merge parent class fields into the specialized record.
                         * This handles e.g. TFPGList<TMyRecord> inheriting FCapacity,
                         * FList etc. from TFPSList. */
                        if (alias_record->parent_class_name != NULL &&
                            alias_record->is_class)
                        {
                            int merge_result = merge_parent_class_fields(symtab, alias_record,
                                tree->tree_data.type_decl_data.id, tree->line_num);
                            if (merge_result > 0)
                                return_val += merge_result;
                        }
                        else if (alias_record->is_class)
                        {
                            detect_default_indexed_property(alias_record, NULL);
                        }

                        if (alias_record->is_class)
                        {
                            int vmt_result = build_class_vmt(symtab, alias_record,
                                tree->tree_data.type_decl_data.id, tree->line_num);
                            if (vmt_result > 0)
                                return_val += vmt_result;
                        }
                        
                        /* Skip size computation for generic templates (not yet specialized)
                         * Size can only be computed after type parameters are substituted */
                        int is_unspecialized_generic = (alias_record->generic_decl != NULL && 
                                                         alias_record->num_generic_args == 0);
                        
                        if (!is_unspecialized_generic)
                        {
                            const char *debug_env3 = kgpc_getenv("KGPC_DEBUG_TFPG");
                            if (debug_env3 != NULL)
                            {
                                fprintf(stderr, "[KGPC] Computing size for alias %s, generic_decl=%p num_args=%d\n",
                                    tree->tree_data.type_decl_data.id,
                                    (void*)alias_record->generic_decl,
                                    alias_record->num_generic_args);
                            }
                            
                            long long record_size = 0;
                            if (semcheck_compute_record_size(symtab, alias_record, &record_size,
                                    tree->line_num) != 0)
                            {
                                if (debug_env3 != NULL)
                                {
                                    fprintf(stderr, "[KGPC] Size computation FAILED for %s\n",
                                        tree->tree_data.type_decl_data.id);
                                }
                                if (kgpc_getenv("KGPC_DEBUG_SEMSTEPS") != NULL)
                                {
                                    fprintf(stderr, "[SemCheck] alias record size failed for %s (line %d)\n",
                                            tree->tree_data.type_decl_data.id ? tree->tree_data.type_decl_data.id : "<null>",
                                            tree->line_num);
                                }
                                return_val += 1;
                            }
                            else if (debug_env3 != NULL)
                            {
                                fprintf(stderr, "[KGPC] Size computation succeeded for %s: %lld bytes\n",
                                    tree->tree_data.type_decl_data.id, record_size);
                            }
                        }
                    }
                }

                const char *debug_alias = kgpc_getenv("KGPC_DEBUG_GENERIC_CLONES");
                if (debug_alias != NULL && tree->tree_data.type_decl_data.id != NULL)
                {
                    fprintf(stderr, "[KGPC] semcheck alias %s: base_type=%d target=%s kgpc_type=%p kind=%d\n",
                        tree->tree_data.type_decl_data.id,
                        alias_info->base_type,
                        alias_info->target_type_id != NULL ? alias_info->target_type_id : "<null>",
                        (void *)tree->tree_data.type_decl_data.kgpc_type,
                        tree->tree_data.type_decl_data.kgpc_type != NULL ?
                            tree->tree_data.type_decl_data.kgpc_type->kind : -1);
                }

                break;
            }
            case TYPE_DECL_GENERIC:
            {
                /* Register generic type declaration in the generic registry */
                struct GenericDecl *generic_info = &tree->tree_data.type_decl_data.info.generic;
                
                const char *debug_env = kgpc_getenv("KGPC_DEBUG_TFPG");
                if (debug_env != NULL)
                {
                    fprintf(stderr, "[KGPC] semcheck TYPE_DECL_GENERIC: id=%s num_params=%d\n",
                        tree->tree_data.type_decl_data.id ? tree->tree_data.type_decl_data.id : "<null>",
                        generic_info ? generic_info->num_type_params : -1);
                }
                
                if (generic_info != NULL && generic_info->num_type_params > 0 && 
                    generic_info->type_parameters != NULL)
                {
                    /* Register the generic declaration */
                    GenericTypeDecl *generic_decl = generic_registry_add_decl(
                        tree->tree_data.type_decl_data.id,
                        generic_info->type_parameters,
                        generic_info->num_type_params,
                        tree
                    );
                    
                    if (generic_decl != NULL)
                    {
                        /* Store reference to record template if this is a class/record */
                        if (generic_info->record_template != NULL)
                        {
                            generic_decl->record_template = generic_info->record_template;
                        }
                        
                        if (debug_env != NULL)
                        {
                            fprintf(stderr, "[KGPC] Registered generic type %s with %d parameters\n",
                                tree->tree_data.type_decl_data.id, generic_info->num_type_params);
                        }
                    }
                    else if (debug_env != NULL)
                    {
                        fprintf(stderr, "[KGPC] Failed to register generic type %s\n",
                            tree->tree_data.type_decl_data.id ? tree->tree_data.type_decl_data.id : "<null>");
                    }
                }
                
                /* Generics don't go in symbol table directly, only specialized instances do */
                var_type = HASHVAR_INTEGER;  /* Placeholder */
                /* Skip the normal PushTypeOntoScope for generics */
                func_return = 0;
                break;
            }
            default:
                var_type = HASHVAR_INTEGER;
                break;
        }

        if (debug_env_check != NULL && return_val > before_switch_errors)
        {
            fprintf(stderr, "[KGPC] Error in switch for type %s (kind=%d), was %d now %d\n",
                tree->tree_data.type_decl_data.id ? tree->tree_data.type_decl_data.id : "<null>",
                tree->tree_data.type_decl_data.kind, before_switch_errors, return_val);
        }

        const char *debug_env2 = kgpc_getenv("KGPC_DEBUG_TFPG");
        int before_symtab_errors = return_val;

        KgpcType *kgpc_type = tree->tree_data.type_decl_data.kgpc_type;
        if (record_info != NULL && record_type_is_class(record_info) &&
            kgpc_type != NULL && kgpc_type->kind == TYPE_KIND_RECORD)
        {
            /* Classes are reference types - represent them as pointers to the class record */
            KgpcType *wrapped = create_pointer_type(kgpc_type);
            kgpc_type_release(tree->tree_data.type_decl_data.kgpc_type); /* release old tree ref */
            kgpc_type = wrapped;
            tree->tree_data.type_decl_data.kgpc_type = wrapped;
        }
        if (tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS &&
            alias_info != NULL)
        {
            /* Rebuild KgpcType from alias when parser's pre-built type is missing,
             * wrong (e.g., string[3] parsed as primitive SHORTSTRING instead of
             * array[0..3] of char, or pointer alias parsed as primitive),
             * or is an UNKNOWN_TYPE placeholder from predeclare_types. */
            int need_rebuild = (kgpc_type == NULL);
            if (!need_rebuild && kgpc_type->kind == TYPE_KIND_PRIMITIVE &&
                (alias_info->is_array || alias_info->is_pointer))
                need_rebuild = 1;
            if (!need_rebuild && kgpc_type->kind == TYPE_KIND_PRIMITIVE &&
                kgpc_type->info.primitive_type_tag == UNKNOWN_TYPE)
                need_rebuild = 1;
            if (need_rebuild)
            {
                KgpcType *rebuilt = create_kgpc_type_from_type_alias(alias_info, symtab,
                    tree->tree_data.type_decl_data.defined_in_unit);
                if (rebuilt != NULL)
                {
                    if (kgpc_type != NULL)
                        kgpc_type_release(kgpc_type);
                    kgpc_type = rebuilt;
                    tree->tree_data.type_decl_data.kgpc_type = kgpc_type;
                }
            }
            const char *trace_sym = kgpc_getenv("KGPC_TRACE_NONLOCAL");
            if (trace_sym != NULL &&
                tree->tree_data.type_decl_data.id != NULL &&
                pascal_identifier_equals(tree->tree_data.type_decl_data.id, trace_sym))
            {
                fprintf(stderr,
                    "[KGPC_TRACE_NONLOCAL] sem_type_decl id=%s alias target=%s is_array=%d [%d,%d] short=%d kgpc=%p kind=%d\n",
                    tree->tree_data.type_decl_data.id,
                    alias_info->target_type_id != NULL ? alias_info->target_type_id : "<null>",
                    alias_info->is_array,
                    alias_info->array_start,
                    alias_info->array_end,
                    alias_info->is_shortstring,
                    (void *)kgpc_type,
                    kgpc_type != NULL ? kgpc_type->kind : -1);
            }
        }

        /* Check if this type was already pre-declared by predeclare_types().
         * If so, skip the push to avoid "redeclaration" errors.
         * IMPORTANT: Only skip if the type exists in the CURRENT scope (scope level 0),
         * not if it exists as a builtin or in a parent scope.
         * Also don't treat as predeclared if existing is from a different source unit. */
        HashNode_t *existing_type = FindIdentInCurrentScope(symtab, tree->tree_data.type_decl_data.id);
        int already_predeclared = (existing_type != NULL &&
                                   existing_type->hash_type == HASHTYPE_TYPE);
        if (debug_pss != NULL &&
            tree->tree_data.type_decl_data.id != NULL &&
            pascal_identifier_equals(tree->tree_data.type_decl_data.id, "PShortString"))
        {
            fprintf(stderr, "[PShortString] predeclared=%d existing_type=%p\n",
                already_predeclared, (void *)existing_type);
            if (existing_type != NULL)
            {
                fprintf(stderr,
                    "[PShortString] existing defined_in_unit=%d unit_is_public=%d source_unit_index=%d\n",
                    existing_type->defined_in_unit, existing_type->unit_is_public,
                    existing_type->source_unit_index);
            }
        }

        /* If existing type is from a different source than the current declaration,
         * find the correct predeclared entry from the same source. With cross-unit
         * coexistence, multiple entries with the same name may exist in the same scope.
         * This handles:
         *  - record-record conflicts (e.g. System.tsiginfo vs SysUtils.tsiginfo)
         *  - alias-record conflicts (e.g. System.TSize alias vs Types.TSize record)
         *  - unit-vs-program conflicts (e.g. prelude TGUID vs program's own TGUID) */
        int skip_type_tree = 0;
        if (already_predeclared &&
            ((existing_type->source_unit_index != 0 &&
              tree->tree_data.type_decl_data.source_unit_index != 0 &&
              existing_type->source_unit_index != tree->tree_data.type_decl_data.source_unit_index) ||
             (tree->tree_data.type_decl_data.defined_in_unit &&
              !existing_type->defined_in_unit)))
        {
            /* Search all entries for the one from the same source unit */
            ListNode_t *all_matches = FindAllIdents(symtab, tree->tree_data.type_decl_data.id);
            HashNode_t *same_unit_entry = NULL;
            ListNode_t *m = all_matches;
            int target_idx = tree->tree_data.type_decl_data.source_unit_index;
            while (m != NULL)
            {
                HashNode_t *candidate = (HashNode_t *)m->cur;
                if (candidate != NULL &&
                    candidate->hash_type == HASHTYPE_TYPE &&
                    candidate->source_unit_index == target_idx)
                {
                    same_unit_entry = candidate;
                    break;
                }
                m = m->next;
            }
            destroy_list(all_matches);
            if (same_unit_entry != NULL)
                existing_type = same_unit_entry;
            else
            {
                /* No predeclared entry from the same unit exists.
                 * For the specific case of an alias conflicting with an
                 * existing record from a different unit (e.g. UnixType.TSize
                 * alias vs Types.TSize record), treat as a fresh declaration
                 * so it gets pushed as a new entry.  The hash table's
                 * TYPE+TYPE collision allowance (HashTable.c) permits this
                 * when the existing entry is from a unit.
                 * Only do this when both have non-zero source_unit_index
                 * (true cross-unit conflict).  When existing has index 0
                 * it's a stale/builtin entry and should be skipped. */
                int existing_is_record = (existing_type->type != NULL &&
                    existing_type->type->kind == TYPE_KIND_RECORD);
                struct TypeAlias *existing_alias = get_type_alias_from_node(existing_type);
                int current_is_enum_alias = (
                    tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS &&
                    tree->tree_data.type_decl_data.info.alias.is_enum);
                int existing_is_enum_alias = (
                    existing_alias != NULL &&
                    existing_alias->is_enum);
                if (tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS &&
                    (existing_is_record || current_is_enum_alias || existing_is_enum_alias) &&
                    existing_type->defined_in_unit &&
                    existing_type->source_unit_index != 0 &&
                    tree->tree_data.type_decl_data.source_unit_index != 0)
                    already_predeclared = 0;
                else
                    skip_type_tree = 1;
            }
        }
        if (skip_type_tree)
        {
            if (debug_pss != NULL &&
                tree->tree_data.type_decl_data.id != NULL &&
                pascal_identifier_equals(tree->tree_data.type_decl_data.id, "PShortString"))
            {
                fprintf(stderr, "[PShortString] skip_type_tree=1\n");
            }
            /* Clean up kgpc_type ownership before skipping */
            if (tree->tree_data.type_decl_data.kgpc_type != NULL)
            {
                destroy_kgpc_type(tree->tree_data.type_decl_data.kgpc_type);
                tree->tree_data.type_decl_data.kgpc_type = NULL;
            }
            /* Mark as suppressed so codegen does not try to emit VMTs
             * from this duplicate tree (its info.record may be depleted
             * after forward-declaration resolution moved fields away). */
            tree->tree_data.type_decl_data.suppress_codegen = 1;
            goto semcheck_type_decls_next;
        }


        /* Skip symbol table registration for generic declarations - they're only in the registry */
        if (tree->tree_data.type_decl_data.kind == TYPE_DECL_GENERIC)
        {
            /* Generic declarations are already registered in generic_registry */
            /* Skip the rest of symbol table handling */
            if (func_return == 0)
            {
                /* Continue to next type declaration */
            }
        }
        else if (already_predeclared)
        {
            if (debug_pss != NULL &&
                tree->tree_data.type_decl_data.id != NULL &&
                pascal_identifier_equals(tree->tree_data.type_decl_data.id, "PShortString"))
            {
                fprintf(stderr,
                    "[PShortString] already_predeclared kgpc_type=%p existing_type->type=%p\n",
                    (void *)kgpc_type,
                    existing_type ? (void *)existing_type->type : NULL);
            }
            mark_hashnode_source_unit(existing_type, tree->tree_data.type_decl_data.source_unit_index);

            /* Type was already registered by predeclare_types().
             * We still need to update any additional metadata like array bounds. */
            if (tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS && alias_info != NULL &&
                alias_info->is_pointer &&
                kgpc_type != NULL &&
                kgpc_type_is_pointer(kgpc_type) &&
                (existing_type->type == NULL || !kgpc_type_is_pointer(existing_type->type)))
            {
                /* Replace predeclared non-pointer placeholder with proper pointer alias type.
                 * Properly manage ref counts: retain new, release old. */
                kgpc_type_retain(kgpc_type);
                if (existing_type->type != NULL)
                    kgpc_type_release(existing_type->type);
                existing_type->type = kgpc_type;
                if (debug_pss != NULL &&
                    tree->tree_data.type_decl_data.id != NULL &&
                    pascal_identifier_equals(tree->tree_data.type_decl_data.id, "PShortString"))
                {
                    fprintf(stderr, "[PShortString] updated existing pointer type=%p\n",
                        (void *)existing_type->type);
                }
            }
            else if (tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS &&
                     kgpc_type != NULL &&
                     existing_type->type != NULL &&
                     existing_type->type->kind == TYPE_KIND_PRIMITIVE &&
                     existing_type->type->info.primitive_type_tag == UNKNOWN_TYPE &&
                     !(kgpc_type->kind == TYPE_KIND_PRIMITIVE &&
                       kgpc_type->info.primitive_type_tag == UNKNOWN_TYPE))
            {
                /* Replace predeclared UNKNOWN placeholders for simple aliases once the
                 * real target type becomes available (e.g. SysUtils.TEndian -> ObjPas.TEndian). */
                kgpc_type_retain(kgpc_type);
                kgpc_type_release(existing_type->type);
                existing_type->type = kgpc_type;
            }

            if (tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS && alias_info != NULL && 
                existing_type->type != NULL)
            {
                struct TypeAlias *existing_alias = kgpc_type_get_type_alias(existing_type->type);
                int can_override_alias = 1;
                if (existing_alias != NULL && existing_alias != alias_info &&
                    alias_info->target_type_id != NULL)
                {
                    int existing_is_unknown_placeholder =
                        (existing_type->type->kind == TYPE_KIND_PRIMITIVE &&
                         existing_type->type->info.primitive_type_tag == UNKNOWN_TYPE);
                    if (!existing_is_unknown_placeholder &&
                        !semcheck_alias_targets_match(existing_alias, alias_info))
                    {
                        /* Avoid overwriting unrelated alias metadata when reusing another type. */
                        can_override_alias = 0;
                    }
                }
                if (existing_alias != NULL)
                {
                    if (alias_info->storage_size <= 0 && existing_alias->storage_size > 0)
                        alias_info->storage_size = existing_alias->storage_size;
                    if (!alias_info->is_range && existing_alias->is_range && existing_alias->range_known)
                    {
                        alias_info->is_range = 1;
                        alias_info->range_known = existing_alias->range_known;
                        alias_info->range_start = existing_alias->range_start;
                        alias_info->range_end = existing_alias->range_end;
                    }
                }

                if (can_override_alias)
                {
                    inherit_alias_metadata(symtab, alias_info);
                    kgpc_type_set_type_alias(existing_type->type, alias_info);
                    if (existing_type->type->type_alias != NULL && alias_info->storage_size > 0)
                        existing_type->type->type_alias->storage_size = alias_info->storage_size;
                }
                else
                {
                    inherit_alias_metadata(symtab, alias_info);
                }

                /* Resolve array bounds from constant identifiers now that constants are in scope */
                if (alias_info->is_array)
                {
                    resolve_array_bounds_in_kgpctype(symtab, existing_type->type, alias_info);
                }
                if (alias_info->is_range)
                {
                    resolve_alias_range_bounds_in_kgpctype(symtab, existing_type->type, alias_info);
                }
                if (alias_info->is_pointer &&
                    kgpc_type_is_pointer(existing_type->type) &&
                    existing_type->type->info.points_to == NULL)
                {
                    HashNode_t *pointee_node = NULL;
                    if (alias_info->pointer_type_ref != NULL)
                    {
                        pointee_node = semcheck_find_preferred_type_node_with_ref(symtab,
                            alias_info->pointer_type_ref, alias_info->pointer_type_id);
                    }
                    else if (alias_info->pointer_type_id != NULL)
                    {
                        pointee_node = semcheck_find_preferred_type_node(symtab,
                            alias_info->pointer_type_id);
                    }
                    if (pointee_node != NULL && pointee_node->type != NULL)
                    {
                        kgpc_type_retain(pointee_node->type);
                        existing_type->type->info.points_to = pointee_node->type;
                    }
                }
            }
            /* Release tree's retained reference (predeclare_types retained it). */
            if (tree->tree_data.type_decl_data.kgpc_type != NULL)
            {
                destroy_kgpc_type(tree->tree_data.type_decl_data.kgpc_type);
                tree->tree_data.type_decl_data.kgpc_type = NULL;
            }
            func_return = 0;  /* No error */
        }
        else if (kgpc_type != NULL) {
            /* Set type_alias on KgpcType before pushing */
            if (tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS && alias_info != NULL)
            {
                struct TypeAlias *existing_alias = kgpc_type_get_type_alias(kgpc_type);
                int can_override_alias = 1;
                if (existing_alias != NULL && existing_alias != alias_info &&
                    alias_info->target_type_id != NULL)
                {
                    if (!semcheck_alias_targets_match(existing_alias, alias_info))
                    {
                        /* Avoid overwriting unrelated alias metadata when reusing another type. */
                        can_override_alias = 0;
                    }
                }
                if (existing_alias != NULL)
                {
                    if (alias_info->storage_size <= 0 && existing_alias->storage_size > 0)
                        alias_info->storage_size = existing_alias->storage_size;
                    if (!alias_info->is_range && existing_alias->is_range && existing_alias->range_known)
                    {
                        alias_info->is_range = 1;
                        alias_info->range_known = existing_alias->range_known;
                        alias_info->range_start = existing_alias->range_start;
                        alias_info->range_end = existing_alias->range_end;
                    }
                }
                /* IMPORTANT: Inherit storage_size from target type for type aliases.
                 * This is critical for types like WideChar (2 bytes) that are represented
                 * as INT_TYPE (4 bytes) in the primitive type system but have a custom
                 * storage_size defined. Without this, SizeOf(TMyChar) where TMyChar = WideChar
                 * would return 4 instead of the correct 2 bytes. */
                inherit_alias_metadata(symtab, alias_info);
                if (can_override_alias)
                {
                    kgpc_type_set_type_alias(kgpc_type, alias_info);
                }
                if (can_override_alias &&
                    kgpc_type_get_type_alias(kgpc_type) != NULL &&
                    alias_info->storage_size > 0)
                {
                    kgpc_type_get_type_alias(kgpc_type)->storage_size = alias_info->storage_size;
                }
                
                /* Resolve array bounds from constant identifiers now that constants are in scope */
                if (alias_info->is_array)
                {
                    resolve_array_bounds_in_kgpctype(symtab, kgpc_type, alias_info);
                }
                if (alias_info->is_range)
                {
                    resolve_alias_range_bounds_in_kgpctype(symtab, kgpc_type, alias_info);
                }
            }
            if (tree->tree_data.type_decl_data.kind == TYPE_DECL_RECORD && record_info != NULL && kgpc_type->kind == TYPE_KIND_RECORD)
                kgpc_type->info.record_info = record_info;
            
            if (kgpc_getenv("KGPC_DEBUG_SEMCHECK") != NULL) {
                fprintf(stderr, "[SemCheck] Pushing type '%s' onto scope, kgpc_type=%p kind=%d\n", 
                    tree->tree_data.type_decl_data.id, (void*)kgpc_type, kgpc_type ? kgpc_type->kind : -1);
            }
            func_return = PushTypeOntoScope_Typed(symtab, tree->tree_data.type_decl_data.id, kgpc_type);
            if (debug_pss != NULL &&
                tree->tree_data.type_decl_data.id != NULL &&
                pascal_identifier_equals(tree->tree_data.type_decl_data.id, "PShortString"))
            {
                fprintf(stderr, "[PShortString] PushTypeOntoScope_Typed result=%d kgpc_type=%p\n",
                    func_return, (void *)kgpc_type);
            }
            if (func_return == 0)
            {
                /* Hash table retained its own reference. Release the tree/creator reference. */
                if (tree->tree_data.type_decl_data.kgpc_type != NULL)
                    destroy_kgpc_type(tree->tree_data.type_decl_data.kgpc_type);
                tree->tree_data.type_decl_data.kgpc_type = NULL;
                /* Note: var_type is automatically set from KgpcType in HashTable.c via set_var_type_from_kgpctype() */
            }
        } else {
        /* Fall back to legacy API for types we can't convert yet */
        func_return = PushTypeOntoScope(symtab, tree->tree_data.type_decl_data.id, var_type,
            record_info, alias_info);
        if (debug_pss != NULL &&
            tree->tree_data.type_decl_data.id != NULL &&
            pascal_identifier_equals(tree->tree_data.type_decl_data.id, "PShortString"))
        {
            fprintf(stderr, "[PShortString] PushTypeOntoScope (legacy) result=%d\n", func_return);
        }
        }

        /* Note: Enum literals are declared in predeclare_enum_literals() during first pass.
         * We don't redeclare them here to avoid "redeclaration" errors. */

        if(func_return > 0)
        {
            semantic_error(tree->line_num, 0, "redeclaration of name %s",
                tree->tree_data.type_decl_data.id);
            return_val += func_return;
        }
        else
        {
            /* Find the entry to mark with unit info.
             * For predeclared types, use the already-resolved existing_type
             * (which may have been redirected by the cross-unit search above).
             * For newly pushed types, use FindIdent to get the fresh entry. */
            HashNode_t *type_node = NULL;
            if (already_predeclared)
            {
                type_node = existing_type;
            }
            else
            {
                FindSymbol(&type_node, symtab, tree->tree_data.type_decl_data.id);
                if (type_node == NULL || type_node->hash_type != HASHTYPE_TYPE)
                    type_node = semcheck_find_type_node_with_unit_flag(symtab,
                        tree->tree_data.type_decl_data.id,
                        tree->tree_data.type_decl_data.defined_in_unit);
            }
            if (type_node != NULL)
            {
                mark_hashnode_unit_info(symtab, type_node,
                    tree->tree_data.type_decl_data.defined_in_unit,
                    tree->tree_data.type_decl_data.unit_is_public);
                mark_hashnode_source_unit(type_node, tree->tree_data.type_decl_data.source_unit_index);
            }

            /* Register nested types under their short name too.
             * E.g., "TCgExceptionStateHandler.TExceptionTemps" also as "TExceptionTemps". */
            {
                const char *type_id = tree->tree_data.type_decl_data.id;
                if (type_id != NULL)
                {
                    const char *last_dot = strrchr(type_id, '.');
                    if (last_dot != NULL && last_dot[1] != '\0')
                    {
                        const char *short_name = last_dot + 1;
                        HashNode_t *existing_short = NULL;
                        if (FindSymbol(&existing_short, symtab, short_name) == 0)
                        {
                            KgpcType *short_type = type_node != NULL ? type_node->type : kgpc_type;
                            if (short_type != NULL)
                                PushTypeOntoScope_Typed(symtab, strdup(short_name), short_type);
                        }
                    }
                }
            }

            /* For generic specializations with inline record types, also register the
             * record type under its mangled name so cloned methods can find it */
            if (tree->tree_data.type_decl_data.kind == TYPE_DECL_ALIAS && 
                alias_info != NULL && 
                alias_info->inline_record_type != NULL &&
                alias_info->inline_record_type->type_id != NULL)
            {
                const char *mangled_name = alias_info->inline_record_type->type_id;
                
                /* Only register if not already registered to avoid redeclaration errors */
                HashNode_t *existing = NULL;
                if (FindSymbol(&existing, symtab, mangled_name) == 0)
                {
                    /* Create a KgpcType for the inline record if not already created */
                    KgpcType *inline_kgpc_type = create_record_type(alias_info->inline_record_type);
                    if (inline_kgpc_type != NULL)
                    {
                        int push_result = PushTypeOntoScope_Typed(symtab, (char *)mangled_name, inline_kgpc_type);
                        if (push_result == 0)
                        {
                            if (debug_env2 != NULL)
                            {
                                fprintf(stderr, "[KGPC] Registered inline record type %s for alias %s\n",
                                        mangled_name, tree->tree_data.type_decl_data.id);
                            }
                        }
                        /* Release creator's reference - hash table retained on success */
                        destroy_kgpc_type(inline_kgpc_type);
                    }
                }
            }
        }

        if (debug_env2 != NULL && return_val > before_symtab_errors)
        {
            fprintf(stderr, "[KGPC] Error increased in type %s (kind=%d), was %d now %d\n",
                tree->tree_data.type_decl_data.id ? tree->tree_data.type_decl_data.id : "<null>",
                tree->tree_data.type_decl_data.kind, before_symtab_errors, return_val);
        }
        
        if (return_val > loop_start_errors)
        {
#ifdef DEBUG
            fprintf(stderr, "DEBUG: semcheck_type_decls ERROR in type %s: was %d now %d\n",
                tree->tree_data.type_decl_data.id ? tree->tree_data.type_decl_data.id : "<null>",
                loop_start_errors, return_val);
#endif
        }

semcheck_type_decls_next:
        g_semcheck_error_unit_name = saved_error_unit_name;
        g_semcheck_error_source_index = saved_error_source_index;
        semcheck_restore_scope(symtab, saved_scope_for_type);
        cur = cur->next;
    }

    /* Post-pass: resolve forward pointer aliases now that all types are in scope. */
    cur = type_decls;
    while (cur != NULL)
    {
        if (cur->type != LIST_TREE || cur->cur == NULL)
        {
            cur = cur->next;
            continue;
        }
        tree = (Tree_t *)cur->cur;
        if (tree->type != TREE_TYPE_DECL ||
            tree->tree_data.type_decl_data.kind != TYPE_DECL_ALIAS)
        {
            cur = cur->next;
            continue;
        }
        struct TypeAlias *post_alias = &tree->tree_data.type_decl_data.info.alias;
        if (post_alias == NULL || !post_alias->is_pointer ||
            post_alias->pointer_type_id == NULL)
        {
            cur = cur->next;
            continue;
        }
        HashNode_t *alias_node = semcheck_find_type_node_with_unit_flag(symtab,
            tree->tree_data.type_decl_data.id,
            tree->tree_data.type_decl_data.defined_in_unit);
        if (alias_node == NULL || alias_node->type == NULL ||
            !kgpc_type_is_pointer(alias_node->type) ||
            alias_node->type->info.points_to != NULL)
        {
            cur = cur->next;
            continue;
        }
        HashNode_t *pointee_node = NULL;
        if (post_alias->pointer_type_ref != NULL)
        {
            pointee_node = semcheck_find_preferred_type_node_with_ref(symtab,
                post_alias->pointer_type_ref, post_alias->pointer_type_id);
        }
        else
        {
            pointee_node = semcheck_find_preferred_type_node(symtab,
                post_alias->pointer_type_id);
        }
        if (pointee_node != NULL && pointee_node->type != NULL)
        {
            kgpc_type_retain(pointee_node->type);
            alias_node->type->info.points_to = pointee_node->type;
        }
        cur = cur->next;
    }

    const char *debug_env = kgpc_getenv("KGPC_DEBUG_TFPG");
    if (debug_env != NULL && return_val > 0)
    {
        fprintf(stderr, "[KGPC] semcheck_type_decls returning error count: %d\n", return_val);
    }
    else if (debug_env != NULL)
    {
        fprintf(stderr, "[KGPC] semcheck_type_decls completed successfully\n");
    }

    return return_val;
}

/* Helper function to process a single constant declaration */
