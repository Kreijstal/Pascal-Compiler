void semcheck_add_builtins(SymTab_t *symtab)
{

    if (!stdlib_loaded_flag())
    {
        /* Platform newline constants to support System/ObjPas resourcestring concatenations */
        AddBuiltinStringConst(symtab, "LineEnding", "\n");
        AddBuiltinStringConst(symtab, "sLineBreak", "\n");
        AddBuiltinCharConst(symtab, "DirectorySeparator", '/');
        AddBuiltinCharConst(symtab, "DriveSeparator", 0);
        AddBuiltinCharConst(symtab, "PathSeparator", ':');
        AddBuiltinCharConst(symtab, "ExtensionSeparator", '.');
        AddBuiltinIntConst(symtab, "MaxPathLen", 4096);

        /* Unix/Linux baseline limits needed by unix.pp aliases (UT.*) */
        AddBuiltinIntConst(symtab, "ARG_MAX", 131072);
        AddBuiltinIntConst(symtab, "NAME_MAX", 255);
        AddBuiltinIntConst(symtab, "PATH_MAX", 4095);
        AddBuiltinIntConst(symtab, "SYS_NMLN", 65);
        AddBuiltinIntConst(symtab, "SIG_MAXSIG", 128);
        AddBuiltinIntConst(symtab, "PRIO_PROCESS", 0);
        AddBuiltinIntConst(symtab, "PRIO_PGRP", 1);
        AddBuiltinIntConst(symtab, "PRIO_USER", 2);
        AddBuiltinIntConst(symtab, "UTSNAME_LENGTH", 65);
        AddBuiltinIntConst(symtab, "fmClosed", 0xD7B0);
        AddBuiltinIntConst(symtab, "fmInput", 0xD7B1);

        /* Sysutils signal helpers (interface declarations may be skipped during parsing). */
        AddBuiltinIntConst(symtab, "RTL_SIGINT", 0);
        AddBuiltinIntConst(symtab, "RTL_SIGFPE", 1);
        AddBuiltinIntConst(symtab, "RTL_SIGSEGV", 2);
        AddBuiltinIntConst(symtab, "RTL_SIGILL", 3);
        AddBuiltinIntConst(symtab, "RTL_SIGBUS", 4);
        AddBuiltinIntConst(symtab, "RTL_SIGQUIT", 5);
        AddBuiltinIntConst(symtab, "RTL_SIGLAST", 5);
        AddBuiltinIntConst(symtab, "RTL_SIGDEFAULT", -1);
        AddBuiltinIntConst(symtab, "ssNotHooked", 0);
        AddBuiltinIntConst(symtab, "ssHooked", 1);
        AddBuiltinIntConst(symtab, "ssOverridden", 2);
    }

    /* Bootstrap stability guard:
     * Some stdlib unit compile paths (notably RTL bootstrap units) may reference
     * these core symbols before imported declarations are visible in the current
     * semcheck pass. Seed only what is missing; never overwrite parsed symbols. */
    ensure_builtin_char_const_if_missing(symtab, "DirectorySeparator", '/');
    ensure_builtin_char_const_if_missing(symtab, "DriveSeparator", 0);
    ensure_builtin_char_const_if_missing(symtab, "PathSeparator", ':');
    ensure_builtin_char_const_if_missing(symtab, "ExtensionSeparator", '.');
    ensure_builtin_int_const_if_missing(symtab, "MaxPathLen", 4096);
    ensure_builtin_int_const_if_missing(symtab, "reRangeError", 4);
    ensure_builtin_int_const_if_missing(symtab, "reInvalidCast", 10);
    ensure_builtin_alias_type_if_missing(symtab, "RTLString", STRING_TYPE, 0);

    /* Integer boundary constants - required by FPC's objpas.pp and system.pp */
    {
        char *name;
        /* MaxInt: Default for Integer (16-bit in classic Pascal, but FPC often maps to 32-bit) */
        /* For FPC compatibility, MaxInt = MaxSmallInt = 32767 */
        name = strdup("MaxInt");
        if (name != NULL) {
            PushConstOntoScope(symtab, name, 32767LL);
            free(name);
        }
        /* MaxLongint: Maximum value for LongInt (32-bit signed) = 2^31 - 1 */
        name = strdup("MaxLongint");
        if (name != NULL) {
            PushConstOntoScope(symtab, name, 2147483647LL);
            free(name);
        }
        /* MaxSmallint: Maximum value for SmallInt (16-bit signed) = 2^15 - 1 */
        name = strdup("MaxSmallint");
        if (name != NULL) {
            PushConstOntoScope(symtab, name, 32767LL);
            free(name);
        }
        name = strdup("DefaultSystemCodePage");
        if (name != NULL) {
            PushConstOntoScope(symtab, name, 65001LL);
            free(name);
        }
        name = strdup("DefaultFileSystemCodePage");
        if (name != NULL) {
            PushConstOntoScope(symtab, name, 65001LL);
            free(name);
        }
        /* MaxShortint: Maximum value for ShortInt (8-bit signed) = 2^7 - 1 */
        name = strdup("MaxShortint");
        if (name != NULL) {
            PushConstOntoScope(symtab, name, 127LL);
            free(name);
        }
        /* MaxInt64: Maximum value for Int64 (64-bit signed) = 2^63 - 1 */
        name = strdup("MaxInt64");
        if (name != NULL) {
            PushConstOntoScope(symtab, name, 9223372036854775807LL);
            free(name);
        }
    }

    {
        KgpcType *pchar = create_pointer_type(create_primitive_type(CHAR_TYPE));
        KgpcType *ppchar = create_pointer_type(pchar);
        if (ppchar != NULL)
        {
            char *envp_name = strdup("EnvP");
            if (envp_name != NULL)
                PushVarOntoScope_Typed(symtab, envp_name, ppchar);
            char *envp_lower = strdup("envp");
            if (envp_lower != NULL)
                PushVarOntoScope_Typed(symtab, envp_lower, ppchar);
            destroy_kgpc_type(ppchar);
        }
    }

    /* Primitive core types required to semcheck system.p and user code.
     * Everything else should live in KGPC/Units/system.p (aliases, pointer helpers, etc.). */
    add_builtin_from_vartype(symtab, "Integer", HASHVAR_INTEGER);
    add_builtin_from_vartype(symtab, "LongInt", HASHVAR_LONGINT);
    add_builtin_type_owned(symtab, "Int64", create_primitive_type_with_size(INT64_TYPE, 8));
    if (!stdlib_loaded_flag())
        add_builtin_alias_type(symtab, "Currency", INT64_TYPE, 8);
    add_builtin_from_vartype(symtab, "Real", HASHVAR_REAL);
    add_builtin_from_vartype(symtab, "Boolean", HASHVAR_BOOLEAN);
    /* FPC-compatible extended boolean types */
    add_builtin_alias_type(symtab, "Boolean8", BOOL, 1);
    add_builtin_alias_type(symtab, "Boolean16", BOOL, 2);
    add_builtin_alias_type(symtab, "Boolean32", BOOL, 4);
    add_builtin_alias_type(symtab, "Boolean64", BOOL, 8);
    /* FPC-compatible byte/word/long boolean types (synonyms) */
    add_builtin_alias_type(symtab, "ByteBool", BOOL, 1);
    add_builtin_alias_type(symtab, "WordBool", BOOL, 2);
    add_builtin_alias_type(symtab, "LongBool", BOOL, 4);
    add_builtin_alias_type(symtab, "QWordBool", BOOL, 8);
    if (!stdlib_loaded_flag())
    {
        add_builtin_alias_type(symtab, "TSignalState", INT_TYPE, 4);
    }
    add_builtin_from_vartype(symtab, "Char", HASHVAR_CHAR);
    add_builtin_from_vartype(symtab, "AnsiChar", HASHVAR_CHAR);
    add_builtin_type_owned(symtab, "WideChar", create_primitive_type_with_size(CHAR_TYPE, 2));
    add_builtin_from_vartype(symtab, "String", HASHVAR_PCHAR);
    add_builtin_from_vartype(symtab, "OpenString", HASHVAR_PCHAR);
    add_builtin_from_vartype(symtab, "AnsiString", HASHVAR_PCHAR);
    add_builtin_string_type_with_alias(symtab, "RawByteString", HASHVAR_PCHAR);
    add_builtin_string_type_with_alias(symtab, "UnicodeString", HASHVAR_PCHAR);
    add_builtin_from_vartype(symtab, "WideString", HASHVAR_PCHAR);
    if (!stdlib_loaded_flag())
    {
        add_builtin_type_owned(symtab, "PAnsiString",
            create_pointer_type(create_primitive_type(STRING_TYPE)));
        add_builtin_type_owned(symtab, "PString",
            create_pointer_type(create_primitive_type(STRING_TYPE)));
    }
    add_builtin_type_owned(symtab, "PAnsiChar",
        create_pointer_type(create_primitive_type(CHAR_TYPE)));

    /* Primitive pointer type */
    add_builtin_type_owned(symtab, "Pointer", create_primitive_type(POINTER_TYPE));
    if (!stdlib_loaded_flag())
    {
        add_builtin_alias_type(symtab, "TClass", POINTER_TYPE, (int)sizeof(void *));
        struct RecordType *tobject = (struct RecordType *)calloc(1, sizeof(struct RecordType));
        if (tobject != NULL)
        {
            tobject->is_class = 1;
            tobject->type_id = strdup("TObject");
            KgpcType *tobject_rec = create_record_type(tobject);
            KgpcType *tobject_type = NULL;
            if (tobject_rec != NULL)
                tobject_type = create_pointer_type(tobject_rec);
            if (tobject_type != NULL)
            {
                AddBuiltinType_Typed(symtab, strdup("TObject"), tobject_type);
                destroy_kgpc_type(tobject_type);
            }
            else if (tobject_rec != NULL)
            {
                destroy_kgpc_type(tobject_rec);
            }
        }

        struct RecordType *tinterfaced = (struct RecordType *)calloc(1, sizeof(struct RecordType));
        if (tinterfaced != NULL)
        {
            tinterfaced->is_class = 1;
            tinterfaced->type_id = strdup("TInterfacedObject");
            tinterfaced->parent_class_name = strdup("TObject");
            KgpcType *tinterfaced_rec = create_record_type(tinterfaced);
            KgpcType *tinterfaced_type = NULL;
            if (tinterfaced_rec != NULL)
                tinterfaced_type = create_pointer_type(tinterfaced_rec);
            if (tinterfaced_type != NULL)
            {
                AddBuiltinType_Typed(symtab, strdup("TInterfacedObject"), tinterfaced_type);
                destroy_kgpc_type(tinterfaced_type);
            }
            else if (tinterfaced_rec != NULL)
            {
                destroy_kgpc_type(tinterfaced_rec);
            }
        }
    }

    /* Common ordinal aliases (match KGPC system.p sizes) */
    add_builtin_type_owned(symtab, "Byte", create_primitive_type_with_size(BYTE_TYPE, 1));
    add_builtin_type_owned(symtab, "ShortInt", create_primitive_type_with_size(INT_TYPE, 1));
    add_builtin_type_owned(symtab, "SmallInt", create_primitive_type_with_size(INT_TYPE, 2));
    add_builtin_type_owned(symtab, "Word", create_primitive_type_with_size(WORD_TYPE, 2));
    add_builtin_type_owned(symtab, "LongWord", create_primitive_type_with_size(LONGWORD_TYPE, 4));
    add_builtin_type_owned(symtab, "Cardinal", create_primitive_type_with_size(LONGWORD_TYPE, 4));
    add_builtin_type_owned(symtab, "DWord", create_primitive_type_with_size(LONGWORD_TYPE, 4));
    add_builtin_type_owned(symtab, "QWord", create_primitive_type_with_size(QWORD_TYPE, 8));
    add_builtin_type_owned(symtab, "UInt64", create_primitive_type_with_size(QWORD_TYPE, 8));
    add_builtin_type_owned(symtab, "Int8", create_primitive_type_with_size(INT_TYPE, 1));
    add_builtin_type_owned(symtab, "UInt8", create_primitive_type_with_size(BYTE_TYPE, 1));
    add_builtin_type_owned(symtab, "Int16", create_primitive_type_with_size(INT_TYPE, 2));
    add_builtin_type_owned(symtab, "UInt16", create_primitive_type_with_size(WORD_TYPE, 2));
    add_builtin_type_owned(symtab, "Int32", create_primitive_type_with_size(INT_TYPE, 4));
    add_builtin_type_owned(symtab, "UInt32", create_primitive_type_with_size(LONGWORD_TYPE, 4));
    add_builtin_type_owned(symtab, "Float", create_primitive_type_with_size(REAL_TYPE, 4));
    add_builtin_type_owned(symtab, "ValReal", create_primitive_type_with_size(REAL_TYPE, 8));
    add_builtin_type_owned(symtab, "Single", create_primitive_type_with_size(REAL_TYPE, 4));
    add_builtin_type_owned(symtab, "Double", create_primitive_type_with_size(REAL_TYPE, 8));
    add_builtin_type_owned(symtab, "Extended", create_primitive_type_with_size(REAL_TYPE, 8));

    /* Variant and OleVariant (COM interop) - treated as opaque 16-byte types.
     * VARIANT_TYPE auto-coerces to/from any value type at the semantic level. */
    add_builtin_type_owned(symtab, "Variant", create_primitive_type_with_size(VARIANT_TYPE, 16));
    add_builtin_type_owned(symtab, "OleVariant", create_primitive_type_with_size(VARIANT_TYPE, 16));

    /* File/Text primitives (sizes align with system.p TextRec/FileRec layout) */
    add_builtin_type_owned(symtab, "file", create_primitive_type_with_size(FILE_TYPE, 368));
    add_builtin_type_owned(symtab, "File", create_primitive_type_with_size(FILE_TYPE, 368));
    add_builtin_type_owned(symtab, "TypedFile", create_primitive_type_with_size(FILE_TYPE, 368));
    add_builtin_type_owned(symtab, "text", create_primitive_type_with_size(TEXT_TYPE, 632));
    add_builtin_type_owned(symtab, "Text", create_primitive_type_with_size(TEXT_TYPE, 632));

    AddBuiltinRealConst(symtab, "Pi", acos(-1.0));

    /* Builtin procedures - procedures have no return type */
    char *setlength_name = strdup("SetLength");
    if (setlength_name != NULL) {
        KgpcType *setlength_type = create_procedure_type(NULL, NULL);
        assert(setlength_type != NULL && "Failed to create SetLength procedure type");
        AddBuiltinProc_Typed(symtab, setlength_name, setlength_type);
        destroy_kgpc_type(setlength_type);
        free(setlength_name);
    }

    char *setstring_name = strdup("SetString");
    if (setstring_name != NULL) {
        KgpcType *setstring_type = create_procedure_type(NULL, NULL);
        assert(setstring_type != NULL && "Failed to create SetString procedure type");
        AddBuiltinProc_Typed(symtab, setstring_name, setstring_type);
        destroy_kgpc_type(setstring_type);
        free(setstring_name);
    }

    if (1)
    {
        ListNode_t *param_res = semcheck_create_builtin_param_with_id(
            "res", SHORTSTRING_TYPE, "shortstring", 1);
        ListNode_t *param_p = semcheck_create_builtin_param_with_id(
            "p", POINTER_TYPE, "PAnsiChar", 0);
        if (param_res != NULL)
        {
            param_res->next = param_p;
            KgpcType *proc_type = create_procedure_type(param_res, NULL);
            assert(proc_type != NULL &&
                "Failed to create fpc_pchar_to_shortstr procedure type");
            AddBuiltinProc_Typed(symtab, "fpc_pchar_to_shortstr", proc_type);
            destroy_kgpc_type(proc_type);
            DestroyList(param_res);
        }
    }

    char *write_name = strdup("write");
    if (write_name != NULL) {
        KgpcType *write_type = create_procedure_type(NULL, NULL);
        assert(write_type != NULL && "Failed to create write procedure type");
        AddBuiltinProc_Typed(symtab, write_name, write_type);
        destroy_kgpc_type(write_type);
        free(write_name);
    }

    char *writeln_name = strdup("writeln");
    if (writeln_name != NULL) {
        KgpcType *writeln_type = create_procedure_type(NULL, NULL);
        assert(writeln_type != NULL && "Failed to create writeln procedure type");
        AddBuiltinProc_Typed(symtab, writeln_name, writeln_type);
        destroy_kgpc_type(writeln_type);
        free(writeln_name);
    }

    char *writestr_name = strdup("writestr");
    if (writestr_name != NULL) {
        KgpcType *writestr_type = create_procedure_type(NULL, NULL);
        assert(writestr_type != NULL && "Failed to create writestr procedure type");
        AddBuiltinProc_Typed(symtab, writestr_name, writestr_type);
        destroy_kgpc_type(writestr_type);
        free(writestr_name);
    }

    char *read_name = strdup("read");
    if (read_name != NULL) {
        KgpcType *read_type = create_procedure_type(NULL, NULL);
        assert(read_type != NULL && "Failed to create read procedure type");
        AddBuiltinProc_Typed(symtab, read_name, read_type);
        destroy_kgpc_type(read_type);
        free(read_name);
    }

    char *readln_name = strdup("readln");
    if (readln_name != NULL) {
        KgpcType *readln_type = create_procedure_type(NULL, NULL);
        assert(readln_type != NULL && "Failed to create readln procedure type");
        AddBuiltinProc_Typed(symtab, readln_name, readln_type);
        destroy_kgpc_type(readln_type);
        free(readln_name);
    }
    char *halt_name = strdup("Halt");
    if (halt_name != NULL) {
        KgpcType *halt_type = create_procedure_type(NULL, NULL);
        assert(halt_type != NULL && "Failed to create Halt procedure type");
        AddBuiltinProc_Typed(symtab, halt_name, halt_type);
        destroy_kgpc_type(halt_type);
        free(halt_name);
    }
    char *error_name = strdup("Error");
    if (error_name != NULL) {
        KgpcType *error_type = create_procedure_type(NULL, NULL);
        assert(error_type != NULL && "Failed to create Error procedure type");
        AddBuiltinProc_Typed(symtab, error_name, error_type);
        destroy_kgpc_type(error_type);
        free(error_name);
    }
    char *assign_name = strdup("Assign");
    if (assign_name != NULL) {
        KgpcType *assign_type = create_procedure_type(NULL, NULL);
        assert(assign_type != NULL && "Failed to create Assign procedure type");
        AddBuiltinProc_Typed(symtab, assign_name, assign_type);
        destroy_kgpc_type(assign_type);
        free(assign_name);
    }
    const char *sysutils_hooks[] = {
        "InitExceptions",
        "InitInternational",
        "DoneExceptions",
        "FreeDriveStr",
        "FreeTerminateProcs",
        "SysBeep"
    };
    for (size_t i = 0; i < sizeof(sysutils_hooks) / sizeof(sysutils_hooks[0]); ++i)
    {
        char *hook_name = strdup(sysutils_hooks[i]);
        if (hook_name != NULL) {
            KgpcType *hook_type = create_procedure_type(NULL, NULL);
            assert(hook_type != NULL && "Failed to create sysutils hook type");
            AddBuiltinProc_Typed(symtab, hook_name, hook_type);
            destroy_kgpc_type(hook_type);
            free(hook_name);
        }
    }
    char *close_name = strdup("Close");
    if (close_name != NULL) {
        KgpcType *close_type = create_procedure_type(NULL, NULL);
        assert(close_type != NULL && "Failed to create Close procedure type");
        AddBuiltinProc_Typed(symtab, close_name, close_type);
        destroy_kgpc_type(close_type);
        free(close_name);
    }
    char *settextcp_name = strdup("SetTextCodePage");
    if (settextcp_name != NULL) {
        KgpcType *settextcp_type = create_procedure_type(NULL, NULL);
        assert(settextcp_type != NULL && "Failed to create SetTextCodePage procedure type");
        AddBuiltinProc_Typed(symtab, settextcp_name, settextcp_type);
        destroy_kgpc_type(settextcp_type);
        free(settextcp_name);
    }
    char *getmem_proc = strdup("GetMem");
    if (getmem_proc != NULL) {
        KgpcType *getmem_type = create_procedure_type(NULL, NULL);
        assert(getmem_type != NULL && "Failed to create GetMem procedure type");
        AddBuiltinProc_Typed(symtab, getmem_proc, getmem_type);
        destroy_kgpc_type(getmem_type);
        free(getmem_proc);
    }
    char *move_proc = strdup("Move");
    if (move_proc != NULL) {
        ListNode_t *move_params = NULL;
        ListNode_t *move_tail = NULL;
        ListNode_t *param = NULL;

        param = semcheck_create_builtin_param_var("source", UNKNOWN_TYPE);
        if (param != NULL) {
            move_params = param;
            move_tail = param;
        }

        param = semcheck_create_builtin_param_var("dest", UNKNOWN_TYPE);
        if (param != NULL) {
            if (move_tail != NULL)
                move_tail->next = param;
            else
                move_params = param;
            move_tail = param;
        }

        param = semcheck_create_builtin_param("count", LONGINT_TYPE);
        if (param != NULL) {
            if (move_tail != NULL)
                move_tail->next = param;
            else
                move_params = param;
        }

        KgpcType *move_type = create_procedure_type(move_params, NULL);
        assert(move_type != NULL && "Failed to create Move procedure type");
        AddBuiltinProc_Typed(symtab, move_proc, move_type);
        destroy_kgpc_type(move_type);
        free(move_proc);
    }
    char *realloc_proc = strdup("ReallocMem");
    if (realloc_proc != NULL) {
        KgpcType *realloc_type = create_procedure_type(NULL, NULL);
        assert(realloc_type != NULL && "Failed to create ReallocMem procedure type");
        AddBuiltinProc_Typed(symtab, realloc_proc, realloc_type);
        destroy_kgpc_type(realloc_type);
        free(realloc_proc);
    }
    char *setcodepage_proc = strdup("SetCodePage");
    if (setcodepage_proc != NULL) {
        KgpcType *setcodepage_type = create_procedure_type(NULL, NULL);
        assert(setcodepage_type != NULL && "Failed to create SetCodePage procedure type");
        AddBuiltinProc_Typed(symtab, setcodepage_proc, setcodepage_type);
        destroy_kgpc_type(setcodepage_type);
        free(setcodepage_proc);
    }
    char *freemem_proc = strdup("FreeMem");
    if (freemem_proc != NULL) {
        KgpcType *freemem_type = create_procedure_type(NULL, NULL);
        assert(freemem_type != NULL && "Failed to create FreeMem procedure type");
        AddBuiltinProc_Typed(symtab, freemem_proc, freemem_type);
        destroy_kgpc_type(freemem_type);
        free(freemem_proc);
    }
    char *val_name = strdup("Val");
    if (val_name != NULL) {
        KgpcType *val_type = create_procedure_type(NULL, NULL);
        assert(val_type != NULL && "Failed to create Val procedure type");
        AddBuiltinProc_Typed(symtab, val_name, val_type);
        destroy_kgpc_type(val_type);
        free(val_name);
    }
    char *str_name = strdup("Str");
    if (str_name != NULL) {
        KgpcType *str_type = create_procedure_type(NULL, NULL);
        assert(str_type != NULL && "Failed to create Str procedure type");
        AddBuiltinProc_Typed(symtab, str_name, str_type);
        destroy_kgpc_type(str_type);
        free(str_name);
    }

    char *insert_name = strdup("Insert");
    if (insert_name != NULL) {
        KgpcType *insert_type = create_procedure_type(NULL, NULL);
        assert(insert_type != NULL && "Failed to create Insert procedure type");
        AddBuiltinProc_Typed(symtab, insert_name, insert_type);
        destroy_kgpc_type(insert_type);
        free(insert_name);
    }
    char *delete_name = strdup("Delete");
    if (delete_name != NULL) {
        KgpcType *delete_type = create_procedure_type(NULL, NULL);
        assert(delete_type != NULL && "Failed to create Delete procedure type");
        AddBuiltinProc_Typed(symtab, delete_name, delete_type);
        destroy_kgpc_type(delete_type);
        free(delete_name);
    }

    {
        const char *swap_name = "SwapEndian";

        ListNode_t *param_int = semcheck_create_builtin_param("AValue", INT_TYPE);
        KgpcType *swap_int = create_procedure_type(param_int, create_primitive_type(INT_TYPE));
        if (swap_int != NULL)
        {
            AddBuiltinFunction_Typed(symtab, strdup(swap_name), swap_int);
            destroy_kgpc_type(swap_int);
        }
        if (param_int != NULL)
            DestroyList(param_int);

        ListNode_t *param_long = semcheck_create_builtin_param("AValue", LONGINT_TYPE);
        KgpcType *swap_long = create_procedure_type(param_long, create_primitive_type(LONGINT_TYPE));
        if (swap_long != NULL)
        {
            AddBuiltinFunction_Typed(symtab, strdup(swap_name), swap_long);
            destroy_kgpc_type(swap_long);
        }
        if (param_long != NULL)
            DestroyList(param_long);

        ListNode_t *param_int64 = semcheck_create_builtin_param("AValue", INT64_TYPE);
        KgpcType *swap_int64 = create_procedure_type(param_int64, create_primitive_type(INT64_TYPE));
        if (swap_int64 != NULL)
        {
            AddBuiltinFunction_Typed(symtab, strdup(swap_name), swap_int64);
            destroy_kgpc_type(swap_int64);
        }
        if (param_int64 != NULL)
            DestroyList(param_int64);
    }


    char *inc_name = strdup("Inc");
    if (inc_name != NULL) {
        KgpcType *inc_type = create_procedure_type(NULL, NULL);
        assert(inc_type != NULL && "Failed to create Inc procedure type");
        AddBuiltinProc_Typed(symtab, inc_name, inc_type);
        destroy_kgpc_type(inc_type);
        free(inc_name);
    }

    char *dec_name = strdup("Dec");
    if (dec_name != NULL) {
        KgpcType *dec_type = create_procedure_type(NULL, NULL);
        assert(dec_type != NULL && "Failed to create Dec procedure type");
        AddBuiltinProc_Typed(symtab, dec_name, dec_type);
        destroy_kgpc_type(dec_type);
        free(dec_name);
    }

    char *include_name = strdup("Include");
    if (include_name != NULL) {
        KgpcType *include_type = create_procedure_type(NULL, NULL);
        assert(include_type != NULL && "Failed to create Include procedure type");
        AddBuiltinProc_Typed(symtab, include_name, include_type);
        destroy_kgpc_type(include_type);
        free(include_name);
    }

    char *exclude_name = strdup("Exclude");
    if (exclude_name != NULL) {
        KgpcType *exclude_type = create_procedure_type(NULL, NULL);
        assert(exclude_type != NULL && "Failed to create Exclude procedure type");
        AddBuiltinProc_Typed(symtab, exclude_name, exclude_type);
        destroy_kgpc_type(exclude_type);
        free(exclude_name);
    }


    char *new_name = strdup("New");
    if (new_name != NULL) {
        KgpcType *new_type = create_procedure_type(NULL, NULL);
        assert(new_type != NULL && "Failed to create New procedure type");
        AddBuiltinProc_Typed(symtab, new_name, new_type);
        destroy_kgpc_type(new_type);
        free(new_name);
    }

    char *dispose_name = strdup("Dispose");
    if (dispose_name != NULL) {
        KgpcType *dispose_type = create_procedure_type(NULL, NULL);
        assert(dispose_type != NULL && "Failed to create Dispose procedure type");
        AddBuiltinProc_Typed(symtab, dispose_name, dispose_type);
        destroy_kgpc_type(dispose_type);
        free(dispose_name);
    }

    char *assert_name = strdup("Assert");
    if (assert_name != NULL) {
        KgpcType *assert_type = create_procedure_type(NULL, NULL);
        assert(assert_type != NULL && "Failed to create Assert procedure type");
        AddBuiltinProc_Typed(symtab, assert_name, assert_type);
        destroy_kgpc_type(assert_type);
        free(assert_name);
    }

    /* Builtin functions - functions have return types */
    char *length_name = strdup("Length");
    if (length_name != NULL) {
        KgpcType *return_type = kgpc_type_from_var_type(HASHVAR_LONGINT);
        assert(return_type != NULL && "Failed to create return type for Length");
        KgpcType *length_type = create_procedure_type(NULL, return_type);
        assert(length_type != NULL && "Failed to create Length function type");
        AddBuiltinFunction_Typed(symtab, length_name, length_type);
        destroy_kgpc_type(length_type);
        free(length_name);
    }

    char *getmem_func = strdup("GetMem");
    if (getmem_func != NULL) {
        KgpcType *return_type = create_primitive_type(POINTER_TYPE);
        assert(return_type != NULL && "Failed to create return type for GetMem");
        KgpcType *getmem_type = create_procedure_type(NULL, return_type);
        assert(getmem_type != NULL && "Failed to create GetMem function type");
        AddBuiltinFunction_Typed(symtab, getmem_func, getmem_type);
        destroy_kgpc_type(getmem_type);
        free(getmem_func);
    }
    {
        const char *interlocked_name = "InterlockedExchangeAdd";

        ListNode_t *param_target = semcheck_create_builtin_param_var("Target", INT_TYPE);
        ListNode_t *param_value = semcheck_create_builtin_param("Source", INT_TYPE);
        ListNode_t *params = ConcatList(param_target, param_value);
        KgpcType *return_type = create_primitive_type(INT_TYPE);
        KgpcType *interlocked_type = create_procedure_type(params, return_type);
        if (interlocked_type != NULL)
        {
            AddBuiltinFunction_Typed(symtab, strdup(interlocked_name), interlocked_type);
            destroy_kgpc_type(interlocked_type);
        }
        if (params != NULL)
            DestroyList(params);

        param_target = semcheck_create_builtin_param_var("Target", LONGINT_TYPE);
        param_value = semcheck_create_builtin_param("Source", LONGINT_TYPE);
        params = ConcatList(param_target, param_value);
        return_type = create_primitive_type(LONGINT_TYPE);
        interlocked_type = create_procedure_type(params, return_type);
        if (interlocked_type != NULL)
        {
            AddBuiltinFunction_Typed(symtab, strdup(interlocked_name), interlocked_type);
            destroy_kgpc_type(interlocked_type);
        }
        if (params != NULL)
            DestroyList(params);

        param_target = semcheck_create_builtin_param_var("Target", INT64_TYPE);
        param_value = semcheck_create_builtin_param("Source", INT64_TYPE);
        params = ConcatList(param_target, param_value);
        return_type = create_primitive_type(INT64_TYPE);
        interlocked_type = create_procedure_type(params, return_type);
        if (interlocked_type != NULL)
        {
            AddBuiltinFunction_Typed(symtab, strdup(interlocked_name), interlocked_type);
            destroy_kgpc_type(interlocked_type);
        }
        if (params != NULL)
            DestroyList(params);

        param_target = semcheck_create_builtin_param_var("Target", POINTER_TYPE);
        param_value = semcheck_create_builtin_param("Source", POINTER_TYPE);
        params = ConcatList(param_target, param_value);
        return_type = create_primitive_type(POINTER_TYPE);
        interlocked_type = create_procedure_type(params, return_type);
        if (interlocked_type != NULL)
        {
            AddBuiltinFunction_Typed(symtab, strdup(interlocked_name), interlocked_type);
            destroy_kgpc_type(interlocked_type);
        }
        if (params != NULL)
            DestroyList(params);
    }
    char *to_singlebyte = strdup("ToSingleByteFileSystemEncodedFileName");
    if (to_singlebyte != NULL) {
        KgpcType *return_type = kgpc_type_from_var_type(HASHVAR_PCHAR);
        assert(return_type != NULL && "Failed to create return type for ToSingleByteFileSystemEncodedFileName");
        KgpcType *to_singlebyte_type = create_procedure_type(NULL, return_type);
        assert(to_singlebyte_type != NULL && "Failed to create ToSingleByteFileSystemEncodedFileName function type");
        AddBuiltinFunction_Typed(symtab, to_singlebyte, to_singlebyte_type);
        destroy_kgpc_type(to_singlebyte_type);
        free(to_singlebyte);
    }
    char *array_to_ppchar = strdup("ArrayStringToPPchar");
    if (array_to_ppchar != NULL) {
        KgpcType *return_type = create_primitive_type(POINTER_TYPE);
        assert(return_type != NULL && "Failed to create return type for ArrayStringToPPchar");
        KgpcType *array_to_ppchar_type = create_procedure_type(NULL, return_type);
        assert(array_to_ppchar_type != NULL && "Failed to create ArrayStringToPPchar function type");
        AddBuiltinFunction_Typed(symtab, array_to_ppchar, array_to_ppchar_type);
        destroy_kgpc_type(array_to_ppchar_type);
        free(array_to_ppchar);
    }

    char *copy_name = strdup("Copy");
    if (copy_name != NULL) {
        KgpcType *return_type = kgpc_type_from_var_type(HASHVAR_PCHAR);
        assert(return_type != NULL && "Failed to create return type for Copy");
        KgpcType *copy_type = create_procedure_type(NULL, return_type);
        assert(copy_type != NULL && "Failed to create Copy function type");
        AddBuiltinFunction_Typed(symtab, copy_name, copy_type);
        destroy_kgpc_type(copy_type);
        free(copy_name);
    }
    char *concat_name = strdup("Concat");
    if (concat_name != NULL) {
        KgpcType *return_type = create_primitive_type(STRING_TYPE);
        assert(return_type != NULL && "Failed to create return type for Concat");
        KgpcType *concat_type = create_procedure_type(NULL, return_type);
        assert(concat_type != NULL && "Failed to create Concat function type");
        AddBuiltinFunction_Typed(symtab, concat_name, concat_type);
        destroy_kgpc_type(concat_type);
        free(concat_name);
    }
    char *eof_name = strdup("EOF");
    if (eof_name != NULL) {
        KgpcType *return_type = kgpc_type_from_var_type(HASHVAR_BOOLEAN);
        assert(return_type != NULL && "Failed to create return type for EOF");
        KgpcType *eof_type = create_procedure_type(NULL, return_type);
        assert(eof_type != NULL && "Failed to create EOF function type");
        AddBuiltinFunction_Typed(symtab, eof_name, eof_type);
        destroy_kgpc_type(eof_type);
        free(eof_name);
    }

    char *eoln_name = strdup("EOLN");
    if (eoln_name != NULL) {
        KgpcType *return_type = kgpc_type_from_var_type(HASHVAR_BOOLEAN);
        assert(return_type != NULL && "Failed to create return type for EOLN");
        KgpcType *eoln_type = create_procedure_type(NULL, return_type);
        assert(eoln_type != NULL && "Failed to create EOLN function type");
        AddBuiltinFunction_Typed(symtab, eoln_name, eoln_type);
        destroy_kgpc_type(eoln_type);
        free(eoln_name);
    }

    char *sizeof_name = strdup("SizeOf");
    if (sizeof_name != NULL) {
        KgpcType *return_type = kgpc_type_from_var_type(HASHVAR_LONGINT);
        assert(return_type != NULL && "Failed to create return type for SizeOf");
        KgpcType *sizeof_type = create_procedure_type(NULL, return_type);
        assert(sizeof_type != NULL && "Failed to create SizeOf function type");
        AddBuiltinFunction_Typed(symtab, sizeof_name, sizeof_type);
        destroy_kgpc_type(sizeof_type);
        free(sizeof_name);
    }

    char *chr_name = strdup("Chr");
    if (chr_name != NULL) {
        KgpcType *return_type = kgpc_type_from_var_type(HASHVAR_CHAR);
        assert(return_type != NULL && "Failed to create return type for Chr");
        KgpcType *chr_type = create_procedure_type(NULL, return_type);
        assert(chr_type != NULL && "Failed to create Chr function type");
        AddBuiltinFunction_Typed(symtab, chr_name, chr_type);
        destroy_kgpc_type(chr_type);
        free(chr_name);
    }

    char *ord_name = strdup("Ord");
    if (ord_name != NULL) {
        KgpcType *return_type = kgpc_type_from_var_type(HASHVAR_LONGINT);
        assert(return_type != NULL && "Failed to create return type for Ord");
        KgpcType *ord_type = create_procedure_type(NULL, return_type);
        assert(ord_type != NULL && "Failed to create Ord function type");
        AddBuiltinFunction_Typed(symtab, ord_name, ord_type);
        destroy_kgpc_type(ord_type);
        free(ord_name);
    }

    char *odd_name = strdup("Odd");
    if (odd_name != NULL) {
        KgpcType *return_type = kgpc_type_from_var_type(HASHVAR_BOOLEAN);
        assert(return_type != NULL && "Failed to create return type for Odd");
        KgpcType *odd_type = create_procedure_type(NULL, return_type);
        assert(odd_type != NULL && "Failed to create Odd function type");
        AddBuiltinFunction_Typed(symtab, odd_name, odd_type);
        destroy_kgpc_type(odd_type);
        free(odd_name);
    }
    char *upcase_name = strdup("UpCase");
    if (upcase_name != NULL) {
        KgpcType *return_type = kgpc_type_from_var_type(HASHVAR_CHAR);
        assert(return_type != NULL && "Failed to create return type for UpCase");
        KgpcType *upcase_type = create_procedure_type(NULL, return_type);
        assert(upcase_type != NULL && "Failed to create UpCase function type");
        AddBuiltinFunction_Typed(symtab, upcase_name, upcase_type);
        destroy_kgpc_type(upcase_type);
        free(upcase_name);
    }

    char *sqr_name = strdup("Sqr");
    if (sqr_name != NULL) {
        KgpcType *return_type = kgpc_type_from_var_type(HASHVAR_LONGINT);
        assert(return_type != NULL && "Failed to create return type for Sqr");
        KgpcType *sqr_type = create_procedure_type(NULL, return_type);
        assert(sqr_type != NULL && "Failed to create Sqr function type");
        AddBuiltinFunction_Typed(symtab, sqr_name, sqr_type);
        destroy_kgpc_type(sqr_type);
        free(sqr_name);
    }

    char *ln_name = strdup("Ln");
    if (ln_name != NULL) {
        KgpcType *return_type = kgpc_type_from_var_type(HASHVAR_REAL);
        assert(return_type != NULL && "Failed to create return type for Ln");
        KgpcType *ln_type = create_procedure_type(NULL, return_type);
        assert(ln_type != NULL && "Failed to create Ln function type");
        AddBuiltinFunction_Typed(symtab, ln_name, ln_type);
        destroy_kgpc_type(ln_type);
        free(ln_name);
    }

    char *exp_name = strdup("Exp");
    if (exp_name != NULL) {
        KgpcType *return_type = kgpc_type_from_var_type(HASHVAR_REAL);
        assert(return_type != NULL && "Failed to create return type for Exp");
        KgpcType *exp_type = create_procedure_type(NULL, return_type);
        assert(exp_type != NULL && "Failed to create Exp function type");
        AddBuiltinFunction_Typed(symtab, exp_name, exp_type);
        destroy_kgpc_type(exp_type);
        free(exp_name);
    }

    char *random_name = strdup("Random");
    if (random_name != NULL) {
        KgpcType *return_type = kgpc_type_from_var_type(HASHVAR_REAL);
        assert(return_type != NULL && "Failed to create return type for Random");
        KgpcType *random_type = create_procedure_type(NULL, return_type);
        assert(random_type != NULL && "Failed to create Random function type");
        AddBuiltinFunction_Typed(symtab, random_name, random_type);
        destroy_kgpc_type(random_type);
        free(random_name);
    }
    char *randomrange_name = strdup("RandomRange");
    if (randomrange_name != NULL) {
        KgpcType *return_type = kgpc_type_from_var_type(HASHVAR_LONGINT);
        assert(return_type != NULL && "Failed to create return type for RandomRange");
        KgpcType *randomrange_type = create_procedure_type(NULL, return_type);
        assert(randomrange_type != NULL && "Failed to create RandomRange function type");
        AddBuiltinFunction_Typed(symtab, randomrange_name, randomrange_type);
        destroy_kgpc_type(randomrange_type);
        free(randomrange_name);
    }

    char *high_name = strdup("High");
    if (high_name != NULL) {
        KgpcType *return_type = kgpc_type_from_var_type(HASHVAR_LONGINT);
        assert(return_type != NULL && "Failed to create return type for High");
        KgpcType *high_type = create_procedure_type(NULL, return_type);
        assert(high_type != NULL && "Failed to create High function type");
        AddBuiltinFunction_Typed(symtab, high_name, high_type);
        destroy_kgpc_type(high_type);
        free(high_name);
    }

    /* Standard I/O file variables - stdin, stdout, stderr */
    /* These are Text file variables that can be passed to Write/WriteLn/Read/ReadLn */
    {
        char *stdin_name = strdup("stdin");
        if (stdin_name != NULL) {
            KgpcType *stdin_type = kgpc_type_from_var_type(HASHVAR_TEXT);
            assert(stdin_type != NULL && "Failed to create stdin type");
            PushVarOntoScope_Typed(symtab, stdin_name, stdin_type);
            destroy_kgpc_type(stdin_type);
            /* Note: stdin_name ownership transferred to symtab, don't free */
        }
        char *stdout_name = strdup("stdout");
        if (stdout_name != NULL) {
            KgpcType *stdout_type = kgpc_type_from_var_type(HASHVAR_TEXT);
            assert(stdout_type != NULL && "Failed to create stdout type");
            PushVarOntoScope_Typed(symtab, stdout_name, stdout_type);
            destroy_kgpc_type(stdout_type);
        }
        char *stderr_name = strdup("stderr");
        if (stderr_name != NULL) {
            KgpcType *stderr_type = kgpc_type_from_var_type(HASHVAR_TEXT);
            assert(stderr_type != NULL && "Failed to create stderr type");
            PushVarOntoScope_Typed(symtab, stderr_name, stderr_type);
            destroy_kgpc_type(stderr_type);
        }
        /* Input and Output - standard Pascal file variables */
        char *input_name = strdup("Input");
        if (input_name != NULL) {
            KgpcType *input_type = kgpc_type_from_var_type(HASHVAR_TEXT);
            assert(input_type != NULL && "Failed to create Input type");
            PushVarOntoScope_Typed(symtab, input_name, input_type);
            destroy_kgpc_type(input_type);
        }
        char *output_name = strdup("Output");
        if (output_name != NULL) {
            KgpcType *output_type = kgpc_type_from_var_type(HASHVAR_TEXT);
            assert(output_type != NULL && "Failed to create Output type");
            PushVarOntoScope_Typed(symtab, output_name, output_type);
            destroy_kgpc_type(output_type);
        }
    }

    /* StringOfChar: function StringOfChar(c: Char/WideChar; l: SizeInt): string */
    {
        const char *func_name = "StringOfChar";
        KgpcType *return_type = create_primitive_type(STRING_TYPE);

        /* Char + LongInt */
        ListNode_t *param_c = semcheck_create_builtin_param("c", CHAR_TYPE);
        ListNode_t *param_l = semcheck_create_builtin_param("l", LONGINT_TYPE);
        ListNode_t *params = ConcatList(param_c, param_l);
        KgpcType *func_type = create_procedure_type(params, return_type);
        if (func_type != NULL)
        {
            AddBuiltinFunction_Typed(symtab, strdup(func_name), func_type);
            destroy_kgpc_type(func_type);
        }
        if (params != NULL)
            DestroyList(params);

        /* Char + Int64 (SizeInt on 64-bit) */
        param_c = semcheck_create_builtin_param("c", CHAR_TYPE);
        param_l = semcheck_create_builtin_param("l", INT64_TYPE);
        params = ConcatList(param_c, param_l);
        func_type = create_procedure_type(params, return_type);
        if (func_type != NULL)
        {
            AddBuiltinFunction_Typed(symtab, strdup(func_name), func_type);
            destroy_kgpc_type(func_type);
        }
        if (params != NULL)
            DestroyList(params);

        /* WideChar (Word) + LongInt */
        param_c = semcheck_create_builtin_param("c", WORD_TYPE);
        param_l = semcheck_create_builtin_param("l", LONGINT_TYPE);
        params = ConcatList(param_c, param_l);
        func_type = create_procedure_type(params, return_type);
        if (func_type != NULL)
        {
            AddBuiltinFunction_Typed(symtab, strdup(func_name), func_type);
            destroy_kgpc_type(func_type);
        }
        if (params != NULL)
            DestroyList(params);

        /* WideChar (Word) + Int64 (SizeInt on 64-bit) */
        param_c = semcheck_create_builtin_param("c", WORD_TYPE);
        param_l = semcheck_create_builtin_param("l", INT64_TYPE);
        params = ConcatList(param_c, param_l);
        func_type = create_procedure_type(params, return_type);
        if (func_type != NULL)
        {
            AddBuiltinFunction_Typed(symtab, strdup(func_name), func_type);
            destroy_kgpc_type(func_type);
        }
        if (params != NULL)
            DestroyList(params);
    }

    /* BinStr: function BinStr(Val: int64; cnt: byte): shortstring */
    {
        const char *func_name = "BinStr";
        ListNode_t *param_val = semcheck_create_builtin_param("Val", INT64_TYPE);
        ListNode_t *param_cnt = semcheck_create_builtin_param("cnt", BYTE_TYPE);
        ListNode_t *params = ConcatList(param_val, param_cnt);
        KgpcType *return_type = create_primitive_type(SHORTSTRING_TYPE);
        KgpcType *func_type = create_procedure_type(params, return_type);
        if (func_type != NULL)
        {
            AddBuiltinFunction_Typed(symtab, strdup(func_name), func_type);
            destroy_kgpc_type(func_type);
        }
        if (params != NULL)
            DestroyList(params);
    }

    /* PopCnt: function PopCnt(AValue: QWord): Byte - counts set bits */
    {
        const char *func_name = "PopCnt";
        /* Overloads for different integer sizes */
        ListNode_t *param_byte = semcheck_create_builtin_param("AValue", BYTE_TYPE);
        KgpcType *popcnt_byte = create_procedure_type(param_byte, create_primitive_type(BYTE_TYPE));
        if (popcnt_byte != NULL)
        {
            AddBuiltinFunction_Typed(symtab, strdup(func_name), popcnt_byte);
            destroy_kgpc_type(popcnt_byte);
        }
        if (param_byte != NULL)
            DestroyList(param_byte);

        ListNode_t *param_word = semcheck_create_builtin_param("AValue", WORD_TYPE);
        KgpcType *popcnt_word = create_procedure_type(param_word, create_primitive_type(BYTE_TYPE));
        if (popcnt_word != NULL)
        {
            AddBuiltinFunction_Typed(symtab, strdup(func_name), popcnt_word);
            destroy_kgpc_type(popcnt_word);
        }
        if (param_word != NULL)
            DestroyList(param_word);

        ListNode_t *param_dword = semcheck_create_builtin_param("AValue", LONGWORD_TYPE);
        KgpcType *popcnt_dword = create_procedure_type(param_dword, create_primitive_type(BYTE_TYPE));
        if (popcnt_dword != NULL)
        {
            AddBuiltinFunction_Typed(symtab, strdup(func_name), popcnt_dword);
            destroy_kgpc_type(popcnt_dword);
        }
        if (param_dword != NULL)
            DestroyList(param_dword);

        ListNode_t *param_qword = semcheck_create_builtin_param("AValue", QWORD_TYPE);
        KgpcType *popcnt_qword = create_procedure_type(param_qword, create_primitive_type(BYTE_TYPE));
        if (popcnt_qword != NULL)
        {
            AddBuiltinFunction_Typed(symtab, strdup(func_name), popcnt_qword);
            destroy_kgpc_type(popcnt_qword);
        }
        if (param_qword != NULL)
            DestroyList(param_qword);
    }

    /* IndexChar: function IndexChar(const buf; len: SizeInt; b: Char): SizeInt */
    {
        const char *func_name = "IndexChar";
        ListNode_t *param_buf = semcheck_create_builtin_param("buf", UNKNOWN_TYPE);
        ListNode_t *param_len = semcheck_create_builtin_param("len", LONGINT_TYPE);
        ListNode_t *param_b = semcheck_create_builtin_param("b", CHAR_TYPE);
        ListNode_t *params = ConcatList(ConcatList(param_buf, param_len), param_b);
        KgpcType *return_type = create_primitive_type(LONGINT_TYPE);
        KgpcType *func_type = create_procedure_type(params, return_type);
        if (func_type != NULL)
        {
            AddBuiltinFunction_Typed(symtab, strdup(func_name), func_type);
            destroy_kgpc_type(func_type);
        }
        if (params != NULL)
            DestroyList(params);
    }

    /* CompareByte: function CompareByte(const buf1, buf2; len: SizeInt): SizeInt */
    {
        const char *func_name = "CompareByte";
        ListNode_t *param_buf1 = semcheck_create_builtin_param("buf1", UNKNOWN_TYPE);
        ListNode_t *param_buf2 = semcheck_create_builtin_param("buf2", UNKNOWN_TYPE);
        ListNode_t *param_len = semcheck_create_builtin_param("len", LONGINT_TYPE);
        ListNode_t *params = ConcatList(ConcatList(param_buf1, param_buf2), param_len);
        KgpcType *return_type = create_primitive_type(LONGINT_TYPE);
        KgpcType *func_type = create_procedure_type(params, return_type);
        if (func_type != NULL)
        {
            AddBuiltinFunction_Typed(symtab, strdup(func_name), func_type);
            destroy_kgpc_type(func_type);
        }
        if (params != NULL)
            DestroyList(params);
    }

    /* UniqueString: procedure UniqueString(var S: String) */
    {
        const char *func_name = "UniqueString";
        ListNode_t *param_s = semcheck_create_builtin_param_var("S", STRING_TYPE);
        KgpcType *func_type = create_procedure_type(param_s, NULL);
        if (func_type != NULL)
        {
            AddBuiltinProc_Typed(symtab, strdup(func_name), func_type);
            destroy_kgpc_type(func_type);
        }
        if (param_s != NULL)
            DestroyList(param_s);
    }

    /* Bit scan builtins (FPC compatibility) */
    {
        struct {
            const char *name;
            int param_tag;
        } bsr_bsf[] = {
            {"BsrByte", BYTE_TYPE}, {"BsfByte", BYTE_TYPE},
            {"BsrWord", WORD_TYPE}, {"BsfWord", WORD_TYPE},
            {"BsrDWord", LONGWORD_TYPE}, {"BsfDWord", LONGWORD_TYPE},
            {"BsrQWord", QWORD_TYPE}, {"BsfQWord", QWORD_TYPE},
        };

        for (size_t i = 0; i < sizeof(bsr_bsf) / sizeof(bsr_bsf[0]); i++)
        {
            ListNode_t *param = semcheck_create_builtin_param("AValue", bsr_bsf[i].param_tag);
            KgpcType *return_type = create_primitive_type(LONGINT_TYPE);
            KgpcType *func_type = create_procedure_type(param, return_type);
            if (func_type != NULL)
            {
                AddBuiltinFunction_Typed(symtab, strdup(bsr_bsf[i].name), func_type);
                destroy_kgpc_type(func_type);
            }
            if (param != NULL)
                DestroyList(param);
        }
    }

    /* FPC compiler intrinsics for stack frame access.
     * These return Pointer and are used by error handling code.
     * Register both zero-arg and one-arg (Pointer) overloads to match FPC semantics.
     * Also register kgpc_get_frame (the runtime stub these get rewritten to). */
    {
        const char *frame_intrinsics[] = {
            "get_frame", "get_pc_addr", "get_caller_addr", "get_caller_frame",
            "Get_Frame", "Get_Caller_Addr", "Get_Caller_Frame",
            "kgpc_get_frame",
        };
        for (size_t i = 0; i < sizeof(frame_intrinsics) / sizeof(frame_intrinsics[0]); i++)
        {
            /* Zero-argument overload: get_frame() -> Pointer */
            {
                KgpcType *return_type = create_primitive_type(POINTER_TYPE);
                KgpcType *func_type = create_procedure_type(NULL, return_type);
                if (func_type != NULL)
                {
                    AddBuiltinFunction_Typed(symtab, strdup(frame_intrinsics[i]), func_type);
                    destroy_kgpc_type(func_type);
                }
            }
            /* One-argument overload: get_caller_addr(frame: Pointer) -> Pointer */
            {
                ListNode_t *param = semcheck_create_builtin_param("p", POINTER_TYPE);
                KgpcType *return_type = create_primitive_type(POINTER_TYPE);
                KgpcType *func_type = create_procedure_type(param, return_type);
                if (func_type != NULL)
                {
                    AddBuiltinFunction_Typed(symtab, strdup(frame_intrinsics[i]), func_type);
                    destroy_kgpc_type(func_type);
                }
                if (param != NULL)
                    DestroyList(param);
            }
        }
    }

    /* Builtins are now in system.p */

    /* Atomic operations: AtomicCmpExchange, AtomicExchange, AtomicIncrement, AtomicDecrement.
     * In FPC these are compiler intrinsics that work on any ordinal/pointer type.
     * We register them with Integer parameters as a common overload. */
    {
        /* AtomicCmpExchange(var Target: Integer; NewValue: Integer; Comparand: Integer): Integer */
        {
            ListNode_t *p1 = semcheck_create_builtin_param("Target", INT_TYPE);
            ListNode_t *p2 = semcheck_create_builtin_param("NewValue", INT_TYPE);
            ListNode_t *p3 = semcheck_create_builtin_param("Comparand", INT_TYPE);
            p1->next = p2;
            p2->next = p3;
            KgpcType *return_type = create_primitive_type(INT_TYPE);
            KgpcType *func_type = create_procedure_type(p1, return_type);
            if (func_type != NULL)
            {
                AddBuiltinFunction_Typed(symtab, strdup("AtomicCmpExchange"), func_type);
                AddBuiltinFunction_Typed(symtab, strdup("InterlockedCompareExchange"), func_type);
                destroy_kgpc_type(func_type);
            }
            DestroyList(p1);
        }
        /* Pointer overload: AtomicCmpExchange(var Target: Pointer; NewValue: Pointer; Comparand: Pointer): Pointer */
        {
            ListNode_t *p1 = semcheck_create_builtin_param("Target", POINTER_TYPE);
            ListNode_t *p2 = semcheck_create_builtin_param("NewValue", POINTER_TYPE);
            ListNode_t *p3 = semcheck_create_builtin_param("Comparand", POINTER_TYPE);
            p1->next = p2;
            p2->next = p3;
            KgpcType *return_type = create_primitive_type(POINTER_TYPE);
            KgpcType *func_type = create_procedure_type(p1, return_type);
            if (func_type != NULL)
            {
                AddBuiltinFunction_Typed(symtab, strdup("AtomicCmpExchange"), func_type);
                AddBuiltinFunction_Typed(symtab, strdup("InterlockedCompareExchange"), func_type);
                destroy_kgpc_type(func_type);
            }
            DestroyList(p1);
        }
        /* AtomicExchange(var Target: Integer; Value: Integer): Integer */
        {
            ListNode_t *p1 = semcheck_create_builtin_param("Target", INT_TYPE);
            ListNode_t *p2 = semcheck_create_builtin_param("Value", INT_TYPE);
            p1->next = p2;
            KgpcType *return_type = create_primitive_type(INT_TYPE);
            KgpcType *func_type = create_procedure_type(p1, return_type);
            if (func_type != NULL)
            {
                AddBuiltinFunction_Typed(symtab, strdup("AtomicExchange"), func_type);
                AddBuiltinFunction_Typed(symtab, strdup("InterlockedExchange"), func_type);
                destroy_kgpc_type(func_type);
            }
            DestroyList(p1);
        }
        /* Pointer overload: AtomicExchange(var Target: Pointer; Value: Pointer): Pointer */
        {
            ListNode_t *p1 = semcheck_create_builtin_param("Target", POINTER_TYPE);
            ListNode_t *p2 = semcheck_create_builtin_param("Value", POINTER_TYPE);
            p1->next = p2;
            KgpcType *return_type = create_primitive_type(POINTER_TYPE);
            KgpcType *func_type = create_procedure_type(p1, return_type);
            if (func_type != NULL)
            {
                AddBuiltinFunction_Typed(symtab, strdup("AtomicExchange"), func_type);
                AddBuiltinFunction_Typed(symtab, strdup("InterlockedExchange"), func_type);
                destroy_kgpc_type(func_type);
            }
            DestroyList(p1);
        }
        /* AtomicIncrement/AtomicDecrement(var Target: Integer; Value: Integer): Integer */
        {
            const char *names[] = {
                "AtomicIncrement", "AtomicDecrement",
                "InterlockedIncrement", "InterlockedDecrement",
            };
            for (size_t i = 0; i < sizeof(names) / sizeof(names[0]); i++)
            {
                /* One-arg overload */
                {
                    ListNode_t *p1 = semcheck_create_builtin_param("Target", INT_TYPE);
                    KgpcType *return_type = create_primitive_type(INT_TYPE);
                    KgpcType *func_type = create_procedure_type(p1, return_type);
                    if (func_type != NULL)
                    {
                        AddBuiltinFunction_Typed(symtab, strdup(names[i]), func_type);
                        destroy_kgpc_type(func_type);
                    }
                    DestroyList(p1);
                }
                /* Two-arg overload */
                {
                    ListNode_t *p1 = semcheck_create_builtin_param("Target", INT_TYPE);
                    ListNode_t *p2 = semcheck_create_builtin_param("Value", INT_TYPE);
                    p1->next = p2;
                    KgpcType *return_type = create_primitive_type(INT_TYPE);
                    KgpcType *func_type = create_procedure_type(p1, return_type);
                    if (func_type != NULL)
                    {
                        AddBuiltinFunction_Typed(symtab, strdup(names[i]), func_type);
                        destroy_kgpc_type(func_type);
                    }
                    DestroyList(p1);
                }
            }
        }
        /* bitsizeof(T): Integer - returns size in bits */
        {
            ListNode_t *p1 = semcheck_create_builtin_param("x", INT_TYPE);
            KgpcType *return_type = create_primitive_type(INT_TYPE);
            KgpcType *func_type = create_procedure_type(p1, return_type);
            if (func_type != NULL)
            {
                AddBuiltinFunction_Typed(symtab, strdup("bitsizeof"), func_type);
                AddBuiltinFunction_Typed(symtab, strdup("BitSizeOf"), func_type);
                destroy_kgpc_type(func_type);
            }
            DestroyList(p1);
        }
        /* Finalize(var v): frees managed resources.
         * POINTER_TYPE is used as a placeholder parameter type; actual type
         * validation is bypassed in the builtin handler which accepts any type. */
        {
            ListNode_t *p1 = semcheck_create_builtin_param("v", POINTER_TYPE);
            KgpcType *proc_type = create_procedure_type(p1, NULL);
            if (proc_type != NULL)
            {
                AddBuiltinProc_Typed(symtab, strdup("Finalize"), proc_type);
                destroy_kgpc_type(proc_type);
            }
            DestroyList(p1);
        }
        /* Initialize(var v): initializes managed resources.
         * POINTER_TYPE is used as a placeholder parameter type; actual type
         * validation is bypassed in the builtin handler which accepts any type. */
        {
            ListNode_t *p1 = semcheck_create_builtin_param("v", POINTER_TYPE);
            KgpcType *proc_type = create_procedure_type(p1, NULL);
            if (proc_type != NULL)
            {
                AddBuiltinProc_Typed(symtab, strdup("Initialize"), proc_type);
                destroy_kgpc_type(proc_type);
            }
            DestroyList(p1);
        }
    }
}

/* Semantic check for a program */
#define SEMCHECK_TIMINGS_ENABLED() (getenv("KGPC_DEBUG_TIMINGS") != NULL)

static double semcheck_now_ms(void) {
    return (double)clock() * 1000.0 / (double)CLOCKS_PER_SEC;
}

static void semcheck_timing_step(const char *label, double *last_ms) {
    if (!SEMCHECK_TIMINGS_ENABLED() || last_ms == NULL)
        return;
    double now = semcheck_now_ms();
    fprintf(stderr, "[timing] semcheck_program %s: %.2f ms\n", label, now - *last_ms);
    *last_ms = now;
}

int semcheck_program(SymTab_t *symtab, Tree_t *tree)
{
    int return_val;
    assert(tree != NULL);
    assert(symtab != NULL);
    assert(tree->type == TREE_PROGRAM_TYPE);

    return_val = 0;
    double t0 = 0.0;
    if (SEMCHECK_TIMINGS_ENABLED())
        t0 = semcheck_now_ms();

    PushScope(symtab);

    semcheck_unit_names_reset();
    semcheck_unit_name_add("System");
    semcheck_unit_names_add_list(tree->tree_data.program_data.uses_units);
    semcheck_timing_step("unit names", &t0);

    return_val += semcheck_id_not_main(tree->tree_data.program_data.program_id);
    semcheck_timing_step("id check", &t0);

    /* TODO: Push program name onto scope */

    /* TODO: Fix line number bug here */
    return_val += semcheck_args(symtab, tree->tree_data.program_data.args_char,
      tree->line_num);
    semcheck_timing_step("args", &t0);
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_program error after args: %d\n", return_val);
#endif

    return_val += predeclare_enum_literals(symtab, tree->tree_data.program_data.type_declaration);
    /* Pre-declare types so they're available for const expressions like High(MyType) */
    return_val += predeclare_types(symtab, tree->tree_data.program_data.type_declaration);
    semcheck_timing_step("predeclare types", &t0);
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_program error after type predeclare: %d\n", return_val);
#endif

    /* Predeclare subprograms so they can be referenced in const initializers */
    return_val += predeclare_subprograms(symtab, tree->tree_data.program_data.subprograms, 0, NULL);
    semcheck_timing_step("predeclare subprograms", &t0);
    if (getenv("KGPC_DEBUG_GENERIC_CLONES") != NULL)
    {
        ListNode_t *debug_cur = tree->tree_data.program_data.type_declaration;
        while (debug_cur != NULL)
        {
            if (debug_cur->type == LIST_TREE && debug_cur->cur != NULL)
            {
                Tree_t *debug_tree = (Tree_t *)debug_cur->cur;
                if (debug_tree->type == TREE_TYPE_DECL && debug_tree->tree_data.type_decl_data.id != NULL)
                {
                    fprintf(stderr, "[KGPC] program type decl: %s kind=%d\n",
                        debug_tree->tree_data.type_decl_data.id,
                        debug_tree->tree_data.type_decl_data.kind);
                }
            }
            debug_cur = debug_cur->next;
        }
    }

    /* Four-pass processing for constants to handle all reference patterns:
     *
     * Pass 1: Imported unit untyped constants.
     * Pass 2: Imported unit typed constants (e.g., DirectorySeparator from system.p).
     * Pass 3: Local untyped constants.
     * Pass 4: Local typed constants.
     */

    /* Pre-push trivially evaluable imported consts (literal integers, reals, etc.)
     * to handle cross-unit forward references due to merge ordering. */
    prepush_trivial_imported_consts(symtab, tree->tree_data.program_data.const_declaration);

    /* Pass 1: Imported unit untyped constants */
    return_val += semcheck_const_decls_imported(symtab, tree->tree_data.program_data.const_declaration);
    semcheck_timing_step("consts pass1 imported untyped", &t0);

    /* Pass 2: Imported unit typed constants */
    ListNode_t *unit_typed_consts = collect_typed_const_decls_filtered(symtab,
        tree->tree_data.program_data.var_declaration, 1);  /* from_unit_only=true */
    if (unit_typed_consts != NULL)
    {
        return_val += semcheck_decls(symtab, unit_typed_consts);
        DestroyList(unit_typed_consts);
    }
    semcheck_timing_step("consts pass2 imported typed", &t0);

    /* Pass 3: Local untyped constants */
    return_val += semcheck_const_decls_local(symtab, tree->tree_data.program_data.const_declaration);
    semcheck_timing_step("consts pass3 local untyped", &t0);
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_program error after consts: %d\n", return_val);
#endif

    /* Pass 4: Local typed constants (can reference both unit typed consts and local untyped consts) */
    ListNode_t *local_typed_consts = collect_typed_const_decls_filtered(symtab,
        tree->tree_data.program_data.var_declaration, 0);  /* from_unit_only=false */
    if (local_typed_consts != NULL)
    {
        return_val += semcheck_decls(symtab, local_typed_consts);
        DestroyList(local_typed_consts);
    }
    semcheck_timing_step("consts pass4 local typed", &t0);

    return_val += semcheck_type_decls(symtab, tree->tree_data.program_data.type_declaration);
    semcheck_timing_step("type decls", &t0);
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_program error after types: %d\n", return_val);
#endif

    ListNode_t *program_vars = collect_non_typed_var_decls(tree->tree_data.program_data.var_declaration);
    if (program_vars != NULL)
    {
        return_val += semcheck_decls(symtab, program_vars);
        DestroyList(program_vars);
    }
    semcheck_timing_step("var decls", &t0);
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_program error after vars: %d\n", return_val);
#endif

    return_val += semcheck_subprograms(symtab, tree->tree_data.program_data.subprograms, 0, NULL);
    semcheck_timing_step("subprograms", &t0);
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_program error after subprograms: %d\n", return_val);
#endif

    return_val += semcheck_stmt(symtab, tree->tree_data.program_data.body_statement, INT_MAX);
    semcheck_timing_step("body", &t0);
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_program error after body: %d\n", return_val);
#endif

    // Semantic check finalization statements from units
    if (tree->tree_data.program_data.finalization_statements != NULL) {
        ListNode_t *final_node = tree->tree_data.program_data.finalization_statements;
        while (final_node != NULL) {
            if (final_node->type == LIST_STMT && final_node->cur != NULL) {
                struct Statement *final_stmt = (struct Statement *)final_node->cur;
                return_val += semcheck_stmt(symtab, final_stmt, INT_MAX);
            }
            final_node = final_node->next;
        }
    }
    semcheck_timing_step("finalization", &t0);

    if(optimize_flag() > 0 && return_val == 0)
    {
        if (getenv("KGPC_DEBUG_BODY") != NULL) {
            fprintf(stderr, "[KGPC] Before optimize: body_statement = %p\n",
                    (void*)tree->tree_data.program_data.body_statement);
            if (tree->tree_data.program_data.body_statement != NULL) {
                fprintf(stderr, "[KGPC] Body statement type: %d\n",
                        tree->tree_data.program_data.body_statement->type);
                if (tree->tree_data.program_data.body_statement->type == STMT_COMPOUND_STATEMENT) {
                    fprintf(stderr, "[KGPC] Compound statement list: %p\n",
                            (void*)tree->tree_data.program_data.body_statement->stmt_data.compound_statement);
                }
            }
        }
        optimize(symtab, tree);
        if (getenv("KGPC_DEBUG_BODY") != NULL) {
            fprintf(stderr, "[KGPC] After optimize: body_statement = %p\n",
                    (void*)tree->tree_data.program_data.body_statement);
        }
    }

    /* Keep the outermost scope alive for code generation. DestroySymTab will clean it up. */
    semcheck_unit_names_reset();
    return return_val;
}

/* Semantic check for a unit */
int semcheck_unit(SymTab_t *symtab, Tree_t *tree)
{
    int return_val;
    const char *debug_steps = getenv("KGPC_DEBUG_SEMSTEPS");
    assert(tree != NULL);
    assert(symtab != NULL);
    assert(tree->type == TREE_UNIT);

    return_val = 0;

    PushScope(symtab);

    semcheck_unit_names_reset();
    semcheck_unit_name_add("System");
    semcheck_unit_name_add(tree->tree_data.unit_data.unit_id);
    if (tree->tree_data.unit_data.unit_id != NULL)
        g_semcheck_current_unit_name = strdup(tree->tree_data.unit_data.unit_id);
    semcheck_unit_names_add_list(tree->tree_data.unit_data.interface_uses);
    semcheck_unit_names_add_list(tree->tree_data.unit_data.implementation_uses);

    /* Check interface section */
    int before = return_val;
    return_val += predeclare_enum_literals(symtab, tree->tree_data.unit_data.interface_type_decls);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] interface enum predeclare +%d (total %d)\n",
                return_val - before, return_val);
    /* Pre-declare types so they're available for const expressions like High(MyType) */
    before = return_val;
    return_val += predeclare_types(symtab, tree->tree_data.unit_data.interface_type_decls);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] interface type predeclare +%d (total %d)\n",
                return_val - before, return_val);

    /* Check implementation section - predeclare types BEFORE subprograms */
    before = return_val;
    return_val += predeclare_enum_literals(symtab, tree->tree_data.unit_data.implementation_type_decls);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] impl enum predeclare +%d (total %d)\n",
                return_val - before, return_val);
    /* Pre-declare types so they're available for const expressions like High(MyType) */
    before = return_val;
    return_val += predeclare_types(symtab, tree->tree_data.unit_data.implementation_type_decls);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] impl type predeclare +%d (total %d)\n",
                return_val - before, return_val);

    /* Now predeclare subprograms AFTER all types (interface + implementation) are predeclared.
     * This ensures function return types referencing implementation types can be resolved.
     * The same subprograms list is shared by interface and implementation. */
    before = return_val;

    /* Debug: dump subprograms list before predeclaration */
    if (getenv("KGPC_DEBUG_SUBPROGRAMS_LIST") != NULL)
    {
        fprintf(stderr, "[SUBPROGRAMS_LIST] Dumping subprograms list for unit:\n");
        ListNode_t *debug_cur = tree->tree_data.unit_data.subprograms;
        int count = 0;
        while (debug_cur != NULL)
        {
            count++;
            if (debug_cur->type == LIST_TREE && debug_cur->cur != NULL)
            {
                Tree_t *sub = (Tree_t *)debug_cur->cur;
                if (sub->type == TREE_SUBPROGRAM)
                {
                    fprintf(stderr, "[SUBPROGRAMS_LIST] %d: %s (line %d)\n",
                            count, sub->tree_data.subprogram_data.id, sub->line_num);
                }
            }
            debug_cur = debug_cur->next;
        }
        fprintf(stderr, "[SUBPROGRAMS_LIST] Total: %d subprograms\n", count);
    }

    return_val += predeclare_subprograms(symtab, tree->tree_data.unit_data.subprograms, 0, NULL);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] subprogram predeclare +%d (total %d)\n",
                return_val - before, return_val);

    /* Continue interface section processing */
    /* Pre-push trivially evaluable imported consts for cross-unit forward references */
    prepush_trivial_imported_consts(symtab, tree->tree_data.unit_data.interface_const_decls);
    /* Pass 1: Imported unit untyped constants. */
    before = return_val;
    return_val += semcheck_const_decls_imported_filtered(symtab,
        tree->tree_data.unit_data.interface_const_decls, 1);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] interface unit consts +%d (total %d)\n",
                return_val - before, return_val);

    /* Pass 2: Imported unit typed constants (e.g., prelude/system typed consts). */
    before = return_val;
    ListNode_t *typed_iface_unit_consts = collect_typed_const_decls_filtered(symtab,
        tree->tree_data.unit_data.interface_var_decls, 1);
    if (typed_iface_unit_consts != NULL)
    {
        return_val += semcheck_decls(symtab, typed_iface_unit_consts);
        DestroyList(typed_iface_unit_consts);
    }
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] interface typed unit consts +%d (total %d)\n",
                return_val - before, return_val);

    /* Pass 3: Local untyped const declarations. */
    before = return_val;
    return_val += semcheck_const_decls_local(symtab, tree->tree_data.unit_data.interface_const_decls);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] interface consts +%d (total %d)\n",
                return_val - before, return_val);
    before = return_val;
    return_val += semcheck_const_decls_imported_filtered(symtab,
        tree->tree_data.unit_data.interface_const_decls, 0);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] interface deferred imported consts +%d (total %d)\n",
                return_val - before, return_val);

    /* Pass 4: Local interface typed constants - they can reference regular constants */
    before = return_val;
    ListNode_t *typed_iface_consts = collect_typed_const_decls_filtered(symtab,
        tree->tree_data.unit_data.interface_var_decls, 0);
    if (typed_iface_consts != NULL)
    {
        return_val += semcheck_decls(symtab, typed_iface_consts);
        DestroyList(typed_iface_consts);
    }
    before = return_val;
    return_val += semcheck_type_decls(symtab, tree->tree_data.unit_data.interface_type_decls);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] interface types +%d (total %d)\n",
                return_val - before, return_val);
    before = return_val;
    ListNode_t *iface_vars = collect_non_typed_var_decls(tree->tree_data.unit_data.interface_var_decls);
    if (iface_vars != NULL)
    {
        return_val += semcheck_decls(symtab, iface_vars);
        DestroyList(iface_vars);
    }
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] interface vars +%d (total %d)\n",
                return_val - before, return_val);

    /* Continue implementation section processing */
    /* Pre-push trivially evaluable imported consts from implementation */
    prepush_trivial_imported_consts(symtab, tree->tree_data.unit_data.implementation_const_decls);
    /* Pass 1: Imported unit untyped constants from implementation section. */
    before = return_val;
    return_val += semcheck_const_decls_imported_filtered(symtab,
        tree->tree_data.unit_data.implementation_const_decls, 1);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] impl unit consts +%d (total %d)\n",
                return_val - before, return_val);

    /* Pass 2: Imported unit typed constants from implementation section. */
    before = return_val;
    ListNode_t *typed_impl_unit_consts = collect_typed_const_decls_filtered(symtab,
        tree->tree_data.unit_data.implementation_var_decls, 1);
    if (typed_impl_unit_consts != NULL)
    {
        return_val += semcheck_decls(symtab, typed_impl_unit_consts);
        DestroyList(typed_impl_unit_consts);
    }
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] impl typed unit consts +%d (total %d)\n",
                return_val - before, return_val);

    /* Pass 3: Local untyped const declarations. */
    before = return_val;
    return_val += semcheck_const_decls_local(symtab, tree->tree_data.unit_data.implementation_const_decls);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] impl consts +%d (total %d)\n",
                return_val - before, return_val);
    before = return_val;
    return_val += semcheck_const_decls_imported_filtered(symtab,
        tree->tree_data.unit_data.implementation_const_decls, 0);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] impl deferred imported consts +%d (total %d)\n",
                return_val - before, return_val);

    /* Pass 4: Local implementation typed constants */
    before = return_val;
    ListNode_t *typed_impl_consts = collect_typed_const_decls_filtered(symtab,
        tree->tree_data.unit_data.implementation_var_decls, 0);
    if (typed_impl_consts != NULL)
    {
        return_val += semcheck_decls(symtab, typed_impl_consts);
        DestroyList(typed_impl_consts);
    }
    before = return_val;
    return_val += semcheck_type_decls(symtab, tree->tree_data.unit_data.implementation_type_decls);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] impl types +%d (total %d)\n",
                return_val - before, return_val);
    before = return_val;
    ListNode_t *impl_vars = collect_non_typed_var_decls(tree->tree_data.unit_data.implementation_var_decls);
    if (impl_vars != NULL)
    {
        return_val += semcheck_decls(symtab, impl_vars);
        DestroyList(impl_vars);
    }
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] impl vars +%d (total %d)\n",
                return_val - before, return_val);

    /* Check subprograms */
    before = return_val;
    return_val += semcheck_subprograms(symtab, tree->tree_data.unit_data.subprograms, 0, NULL);
    if (debug_steps != NULL && return_val != before)
        fprintf(stderr, "[SemCheck] subprograms +%d (total %d)\n",
                return_val - before, return_val);

    /* Check initialization section if present */
    if (tree->tree_data.unit_data.initialization != NULL) {
        before = return_val;
        return_val += semcheck_stmt(symtab, tree->tree_data.unit_data.initialization, INT_MAX);
        if (debug_steps != NULL && return_val != before)
            fprintf(stderr, "[SemCheck] initialization +%d (total %d)\n",
                    return_val - before, return_val);
    }

    /* Check finalization section if present */
    if (tree->tree_data.unit_data.finalization != NULL) {
        before = return_val;
        return_val += semcheck_stmt(symtab, tree->tree_data.unit_data.finalization, INT_MAX);
        if (debug_steps != NULL && return_val != before)
            fprintf(stderr, "[SemCheck] finalization +%d (total %d)\n",
                    return_val - before, return_val);
    }

    semcheck_unit_names_reset();
    return return_val;
}


/* Adds arguments to the symbol table */
/* A return value greater than 0 indicates how many errors occurred */
int semcheck_args(SymTab_t *symtab, ListNode_t *args, int line_num)
{
    ListNode_t *cur;
    int return_val, func_return;
    assert(symtab != NULL);

    return_val = 0;

    cur = args;

    /* Checking if they are declarations
        NOTE: Mismatching arg types is an error */
    if(cur != NULL)
        if(cur->type == LIST_TREE)
            return semcheck_decls(symtab, args);

    while(cur != NULL)
    {
        /* If not a list of declarations, must be a list of strings */
        assert(cur->type == LIST_STRING);

        /* UNTYPED procedure parameters - use NULL KgpcType */
        func_return = PushVarOntoScope_Typed(symtab, (char *)cur->cur, NULL);

        /* Greater than 0 signifies an error */
        if(func_return > 0)
        {
            semcheck_error_with_context("Error on line %d, redeclaration of name %s!\n",
                line_num, (char *)cur->cur);
            return_val += func_return;
        }

        cur = cur->next;
    }

    return return_val;
}

/* Pushes a bunch of declarations onto the current scope */
/* A return value greater than 0 indicates how many errors occurred */
int semcheck_decls(SymTab_t *symtab, ListNode_t *decls)
{
    ListNode_t *cur, *ids, *ids_head;
    Tree_t *tree;
    int return_val, func_return;

    enum VarType var_type;

    assert(symtab != NULL);

    return_val = 0;
    cur = decls;
    while(cur != NULL)
    {
        /* Any declaration is always a tree */
        assert(cur->cur != NULL);
        assert(cur->type == LIST_TREE);
        tree = (Tree_t *)cur->cur;
        assert(tree->type == TREE_VAR_DECL || tree->type == TREE_ARR_DECL);

        if (tree->type == TREE_VAR_DECL)
            ids_head = tree->tree_data.var_decl_data.ids;
        else
            ids_head = tree->tree_data.arr_decl_data.ids;

        ids = ids_head;

        HashNode_t *resolved_type = NULL;
        int owner_type_match = 0;
        const TypeRef *decl_type_ref = NULL;
        const char *decl_type_id = NULL;
        const char *decl_type_base = NULL;
        if (tree->type == TREE_VAR_DECL)
        {
            decl_type_ref = tree->tree_data.var_decl_data.type_ref;
            decl_type_id = tree->tree_data.var_decl_data.type_id;
            decl_type_base = (decl_type_ref != NULL)
                ? type_ref_base_name(decl_type_ref)
                : decl_type_id;
        }
        if (tree->type == TREE_VAR_DECL && decl_type_id != NULL)
        {
            if (tree->tree_data.var_decl_data.defined_in_unit)
            {
                /* Imported declarations must stay bound to imported symbols.
                 * Never allow local fallback here: local shadow types can
                 * silently corrupt signatures (e.g. UnixType.TSize). */
                resolved_type = semcheck_find_type_node_with_unit_flag_ref(symtab,
                    decl_type_ref, decl_type_id, 1);
                if (resolved_type != NULL && resolved_type->type != NULL &&
                    resolved_type->type->kind == TYPE_KIND_RECORD &&
                    (decl_type_base != NULL &&
                     (pascal_identifier_equals(decl_type_base, "TSize") ||
                      pascal_identifier_equals(decl_type_base, "tsize"))))
                {
                    HashNode_t *size_node = semcheck_find_type_node_with_unit_flag(symtab,
                        "size_t", 1);
                    if (size_node == NULL || size_node->type == NULL)
                        size_node = semcheck_find_preferred_type_node(symtab, "size_t");
                    if (size_node != NULL && size_node->type != NULL &&
                        size_node->type->kind != TYPE_KIND_RECORD)
                        resolved_type = size_node;
                }
            }
            else
            {
                /* In method parameter declarations, a type_id matching the current
                 * owner must bind to that owner type before generic lookup. This
                 * prevents imported aliases with the same name from corrupting
                 * Self/parameter record field resolution. */
                const char *owner_id = semcheck_get_current_method_owner();
                const char *owner_innermost = semcheck_get_current_subprogram_owner_class();
                if (owner_id != NULL &&
                    (decl_type_base != NULL &&
                     (pascal_identifier_equals(decl_type_base, owner_id) ||
                      (owner_innermost != NULL && pascal_identifier_equals(decl_type_base, owner_innermost)))))
                {
                    owner_type_match = 1;
                    if (tree->tree_data.var_decl_data.cached_kgpc_type != NULL &&
                        semcheck_kgpc_type_is_record_like(
                            tree->tree_data.var_decl_data.cached_kgpc_type))
                    {
                        /* Method-owner parameters (Self / owner-typed args) may already
                         * carry precise record KgpcType from parser conversion.
                         * Never rebind them through ambiguous type_id lookup. */
                        resolved_type = NULL;
                    }
                    else
                    {
                    resolved_type = semcheck_find_owner_record_type_node(symtab, owner_id);
                    if (resolved_type == NULL && owner_innermost != NULL)
                        resolved_type = semcheck_find_owner_record_type_node(symtab, owner_innermost);
                    if (resolved_type == NULL)
                        resolved_type = semcheck_find_preferred_type_node(symtab, owner_id);
                    }
                }
                if (resolved_type == NULL && !owner_type_match)
                    resolved_type = semcheck_find_preferred_type_node_with_ref(symtab,
                        decl_type_ref, decl_type_id);
            }
        }
        if (tree->type == TREE_VAR_DECL &&
            decl_type_id != NULL &&
            resolved_type == NULL)
        {
            const char *owner_full = semcheck_get_current_subprogram_owner_class_full();
            const char *owner_outer = semcheck_get_current_subprogram_owner_class_outer();
            if (owner_full == NULL)
                owner_full = semcheck_get_current_method_owner();
            if (owner_full != NULL)
            {
                semcheck_maybe_qualify_nested_type(symtab, owner_full, owner_outer,
                    &tree->tree_data.var_decl_data.type_id,
                    &tree->tree_data.var_decl_data.type_ref);
                decl_type_ref = tree->tree_data.var_decl_data.type_ref;
                decl_type_id = tree->tree_data.var_decl_data.type_id;
                decl_type_base = (decl_type_ref != NULL)
                    ? type_ref_base_name(decl_type_ref)
                    : decl_type_id;
                if (decl_type_id != NULL)
                {
                    resolved_type = semcheck_find_preferred_type_node_with_ref(symtab,
                        decl_type_ref, decl_type_id);
                }
            }
        }
        if (tree->type == TREE_VAR_DECL)
        {
            int keep_imported_cached =
                (tree->tree_data.var_decl_data.defined_in_unit &&
                 tree->tree_data.var_decl_data.cached_kgpc_type != NULL);
            if (resolved_type != NULL && resolved_type->type != NULL)
            {
                if (keep_imported_cached)
                {
                    /* Imported declarations should retain their original resolved type.
                     * Rebinding in later scopes can corrupt aliases like BaseUnix.TSize. */
                }
                else
                {
                /* Clear any pre-existing cached type if we're replacing with resolved type */
                if (tree->tree_data.var_decl_data.cached_kgpc_type != NULL)
                {
                    destroy_kgpc_type(tree->tree_data.var_decl_data.cached_kgpc_type);
                    tree->tree_data.var_decl_data.cached_kgpc_type = NULL;
                }
                kgpc_type_retain(resolved_type->type);
                tree->tree_data.var_decl_data.cached_kgpc_type = resolved_type->type;
                }
            }
            else if (resolved_type != NULL && resolved_type->type == NULL && decl_type_id != NULL)
            {
                const char *type_name = decl_type_base != NULL ? decl_type_base : decl_type_id;
                int builtin_tag = semcheck_map_builtin_type_name_local(type_name);
                if (builtin_tag == STRING_TYPE || builtin_tag == SHORTSTRING_TYPE)
                {
                    tree->tree_data.var_decl_data.cached_kgpc_type =
                        create_primitive_type(builtin_tag);
                }
            }
            else if (tree->tree_data.var_decl_data.cached_kgpc_type == NULL &&
                     resolved_type == NULL && decl_type_id != NULL)
            {
                /* Fallback: Create KgpcType for built-in type names not in symbol table */
                const char *type_id = decl_type_base != NULL ? decl_type_base : decl_type_id;
                int builtin_tag = semcheck_map_builtin_type_name_local(type_id);
                if (builtin_tag != UNKNOWN_TYPE)
                {
                    tree->tree_data.var_decl_data.cached_kgpc_type = create_primitive_type(builtin_tag);
                }
                else if (tree->tree_data.var_decl_data.type_ref != NULL &&
                         tree->tree_data.var_decl_data.type_ref->num_generic_args > 0)
                {
                    /* Handle parameterized string types like AnsiString(CP_NONE) */
                    const char *base_name = decl_type_base != NULL ? decl_type_base : decl_type_id;
                    int base_tag = semcheck_map_builtin_type_name_local(base_name);
                    if (base_tag == STRING_TYPE || base_tag == SHORTSTRING_TYPE)
                    {
                        tree->tree_data.var_decl_data.cached_kgpc_type =
                            create_primitive_type(base_tag);
                    }
                    else
                    {
                        tree->tree_data.var_decl_data.cached_kgpc_type =
                            create_primitive_type(POINTER_TYPE);
                    }
                }
            }
            /* Note: If cached_kgpc_type is already set (e.g., for inline procedure types from parser),
             * preserve it rather than destroying it. */
            /* AUDIT: Log when KgpcType is missing for a type_id */
            if (getenv("KGPC_AUDIT_TYPES") != NULL && tree->tree_data.var_decl_data.type_id != NULL)
            {
                const char *var_name = (ids_head && ids_head->cur) ? (char*)ids_head->cur : "<unknown>";
                const char *type_id = tree->tree_data.var_decl_data.type_id;
                KgpcType *kgpc = tree->tree_data.var_decl_data.cached_kgpc_type;
                if (kgpc == NULL)
                {
                    fprintf(stderr, "[TYPE_AUDIT] MISSING: var=%s type_id=%s resolved_node=%p\n",
                        var_name, type_id, (void*)resolved_type);
                }
                else if (kgpc->kind == TYPE_KIND_PRIMITIVE && kgpc->info.primitive_type_tag == POINTER_TYPE)
                {
                    /* Pointer type but no points_to info - this is a problem */
                    fprintf(stderr, "[TYPE_AUDIT] INCOMPLETE_PTR: var=%s type_id=%s kind=PRIMITIVE tag=POINTER (should be TYPE_KIND_POINTER)\n",
                        var_name, type_id);
                }
            }
        }
        int skip_initializer = 0;
        while(ids != NULL)
        {
            assert(ids->cur != NULL);
            assert(ids->type == LIST_STRING);

            /* Variable declarations */
            if(tree->type == TREE_VAR_DECL)
            {
                if (tree->tree_data.var_decl_data.is_typed_const)
                {
                    HashNode_t *existing_node = NULL;
                    if (FindIdent(&existing_node, symtab, ids->cur) >= 0 &&
                        existing_node != NULL && existing_node->is_typed_const)
                    {
                        mark_hashnode_unit_info(symtab, existing_node,
                            tree->tree_data.var_decl_data.defined_in_unit,
                            tree->tree_data.var_decl_data.unit_is_public);
                        skip_initializer = 1;
                        goto next_identifier;
                    }
                }
                if (ids->cur != NULL && strcmp((char *)ids->cur, "Sock") == 0) {
#ifdef DEBUG
                     fprintf(stderr, "DEBUG: semcheck_decls processing Sock. type_id=%s resolved_type=%p\n",
                         tree->tree_data.var_decl_data.type_id ? tree->tree_data.var_decl_data.type_id : "<null>",
                         resolved_type);
#endif
                }

                if (tree->tree_data.var_decl_data.type_id != NULL)
                {
                    if (getenv("KGPC_DEBUG_SEMCHECK") != NULL && tree->tree_data.var_decl_data.is_typed_const)
                        fprintf(stderr, "[SemCheck] Typed const with type_id: %s, var: %s\n",
                            tree->tree_data.var_decl_data.type_id,
                            ids && ids->cur ? (char*)ids->cur : "<null>");
                    HashNode_t *type_node = resolved_type;
                    const char *type_id = tree->tree_data.var_decl_data.type_id;
                    int declared_type = tree->tree_data.var_decl_data.type;
                    if (getenv("KGPC_DEBUG_TYPE_HELPER") != NULL && ids && ids->cur &&
                        pascal_identifier_equals((char*)ids->cur, "Self"))
                    {
                        fprintf(stderr, "[KGPC] semcheck_decls Self: type_id=%s declared_type=%d type_node=%p type_node->type=%p kind=%d\n",
                            type_id ? type_id : "<null>", declared_type, (void*)type_node,
                            type_node && type_node->type ? (void*)type_node->type : NULL,
                            type_node && type_node->type ? type_node->type->kind : -1);
                    }
                    if (getenv("KGPC_DEBUG_VAR_TYPES") != NULL && ids && ids->cur)
                    {
                        fprintf(stderr,
                            "[KGPC] semcheck_decls var=%s type_id=%s declared_type=%d type_node=%p type_node->type=%p cached=%p cached_kind=%d\n",
                            (char*)ids->cur,
                            type_id ? type_id : "<null>",
                            declared_type,
                            (void*)type_node,
                            type_node && type_node->type ? (void*)type_node->type : NULL,
                            (void*)tree->tree_data.var_decl_data.cached_kgpc_type,
                            tree->tree_data.var_decl_data.cached_kgpc_type ?
                                tree->tree_data.var_decl_data.cached_kgpc_type->kind : -1);
                    }

                    if (tree->tree_data.var_decl_data.cached_kgpc_type != NULL &&
                        (type_node == NULL || type_node->type == NULL))
                    {
                        KgpcType *cached = tree->tree_data.var_decl_data.cached_kgpc_type;
                        kgpc_type_retain(cached);
                        func_return = PushVarOntoScope_Typed(symtab, (char *)ids->cur, cached);
                        if (func_return == 0)
                        {
                            HashNode_t *var_node = NULL;
                            if (FindIdent(&var_node, symtab, ids->cur) != -1 && var_node != NULL)
                            {
                                var_node->is_var_parameter =
                                    (tree->tree_data.var_decl_data.is_var_param ||
                                     tree->tree_data.var_decl_data.is_untyped_param) ? 1 : 0;
                                mark_hashnode_unit_info(symtab, var_node,
                                    tree->tree_data.var_decl_data.defined_in_unit,
                                    tree->tree_data.var_decl_data.unit_is_public);
                            }
                        }
                        goto next_identifier;
                    }

                    if (declared_type == SET_TYPE)
                    {
                        KgpcType *set_type = create_primitive_type(SET_TYPE);
                        if (set_type != NULL &&
                            tree->tree_data.var_decl_data.inline_type_alias != NULL)
                        {
                            kgpc_type_set_type_alias(set_type,
                                tree->tree_data.var_decl_data.inline_type_alias);
                        }
                        if (tree->tree_data.var_decl_data.cached_kgpc_type != NULL)
                        {
                            destroy_kgpc_type(tree->tree_data.var_decl_data.cached_kgpc_type);
                            tree->tree_data.var_decl_data.cached_kgpc_type = NULL;
                        }
                        if (set_type != NULL)
                        {
                            kgpc_type_retain(set_type);
                            tree->tree_data.var_decl_data.cached_kgpc_type = set_type;
                        }
                        func_return = PushVarOntoScope_Typed(symtab, (char *)ids->cur, set_type);
                        if (func_return == 0)
                        {
                            HashNode_t *var_node = NULL;
                            if (FindIdent(&var_node, symtab, ids->cur) != -1 && var_node != NULL)
                            {
                                var_node->is_var_parameter =
                                    (tree->tree_data.var_decl_data.is_var_param ||
                                     tree->tree_data.var_decl_data.is_untyped_param) ? 1 : 0;
                                mark_hashnode_unit_info(symtab, var_node,
                                    tree->tree_data.var_decl_data.defined_in_unit,
                                    tree->tree_data.var_decl_data.unit_is_public);
                            }
                        }
                        goto next_identifier;
                    }

                    /* If declared as pointer type (^TypeName), handle inline pointer */
                    if (declared_type == POINTER_TYPE)
                    {
                        /* This is ^TypeName where TypeName is stored in type_id.
                         * We need to create a pointer type pointing to TypeName. */
                        KgpcType *points_to = NULL;

                        /* Try to resolve the target type */
                        if (type_node != NULL && type_node->type != NULL)
                        {
                            kgpc_type_retain(type_node->type);
                            points_to = type_node->type;
                        }
                        else
                        {
                            /* Check builtin types for the pointer target */
                            int target_tag = semcheck_map_builtin_type_name_local(type_id);
                            if (target_tag != UNKNOWN_TYPE)
                            {
                                points_to = create_primitive_type(target_tag);
                            }
                            else
                            {
                                /* Unknown target type - create a placeholder */
                                points_to = NULL;
                            }
                        }

                        KgpcType *pointer_type = create_pointer_type(points_to);
                        func_return = PushVarOntoScope_Typed(symtab, (char *)ids->cur, pointer_type);
                        if (func_return == 0)
                        {
                            HashNode_t *var_node = NULL;
                            if (FindIdent(&var_node, symtab, ids->cur) != -1 && var_node != NULL)
                            {
                                var_node->is_var_parameter =
                                    (tree->tree_data.var_decl_data.is_var_param ||
                                     tree->tree_data.var_decl_data.is_untyped_param) ? 1 : 0;
                                mark_hashnode_unit_info(symtab, var_node,
                                    tree->tree_data.var_decl_data.defined_in_unit,
                                    tree->tree_data.var_decl_data.unit_is_public);
                            }
                        }
                        goto next_identifier;
                    }

                    /* Check if it's a builtin type even if not in symbol table */
                    if (type_node == NULL)
                    {
                        if (getenv("KGPC_DEBUG_TYPE_HELPER") != NULL && ids && ids->cur)
                            fprintf(stderr, "[KGPC] semcheck_decls: %s has type_id=%s but type_node is NULL\n",
                                (char*)ids->cur, type_id);
                        /* Check for builtin types */
                        if (pascal_identifier_equals(type_id, "Integer"))
                            var_type = HASHVAR_INTEGER;
                        else if (pascal_identifier_equals(type_id, "LongInt"))
                            var_type = HASHVAR_LONGINT;
                        else if (pascal_identifier_equals(type_id, "SizeUInt") || pascal_identifier_equals(type_id, "QWord") ||
                                 pascal_identifier_equals(type_id, "NativeUInt"))
                            var_type = HASHVAR_LONGINT;  /* Unsigned 64-bit on x86-64 */
                        else if (pascal_identifier_equals(type_id, "Real") || pascal_identifier_equals(type_id, "Double"))
                            var_type = HASHVAR_REAL;
                        else if (pascal_identifier_equals(type_id, "String") || pascal_identifier_equals(type_id, "AnsiString") ||
                                 pascal_identifier_equals(type_id, "RawByteString") ||
                                 pascal_identifier_equals(type_id, "UnicodeString") ||
                                 pascal_identifier_equals(type_id, "WideString"))
                            var_type = HASHVAR_PCHAR;
                        else if (pascal_identifier_equals(type_id, "ShortString"))
                        {
                            /* ShortString is array[0..255] of Char with length at index 0 */
                            var_type = HASHVAR_ARRAY;
                        }
                        else if (pascal_identifier_equals(type_id, "Char") ||
                                 pascal_identifier_equals(type_id, "AnsiChar"))
                            var_type = HASHVAR_CHAR;
                        else if (pascal_identifier_equals(type_id, "Boolean"))
                            var_type = HASHVAR_BOOLEAN;
                        else if (pascal_identifier_equals(type_id, "Pointer"))
                            var_type = HASHVAR_POINTER;
                        else if (pascal_identifier_equals(type_id, "Byte") || pascal_identifier_equals(type_id, "Word"))
                            var_type = HASHVAR_INTEGER;
                        /* Handle FPC system pointer types (PInt64, PByte, etc.) */
                        else if (semcheck_is_builtin_pointer_type_id(type_id))
                            var_type = HASHVAR_POINTER;
                        else if (tree->tree_data.var_decl_data.type_ref != NULL &&
                                 tree->tree_data.var_decl_data.type_ref->num_generic_args > 0)
                            var_type = HASHVAR_POINTER;
                        else
                        {
                            semantic_error(tree->line_num, 0, "undefined type %s", type_id);
                            return_val++;
                            var_type = HASHVAR_UNTYPED;
                        }
                    }
                    else
                    {
                        if (type_node->type == NULL && type_id != NULL)
                        {
                            int builtin_tag = semcheck_map_builtin_type_name_local(type_id);
                            if (builtin_tag != UNKNOWN_TYPE)
                            {
                                KgpcType *builtin_type = create_primitive_type(builtin_tag);
                                func_return = PushVarOntoScope_Typed(symtab, (char *)ids->cur, builtin_type);
                                if (func_return == 0)
                                {
                                    HashNode_t *var_node = NULL;
                                    if (FindIdent(&var_node, symtab, ids->cur) != -1 && var_node != NULL)
                                    {
                                        var_node->is_var_parameter =
                                            (tree->tree_data.var_decl_data.is_var_param ||
                                             tree->tree_data.var_decl_data.is_untyped_param) ? 1 : 0;
                                        mark_hashnode_unit_info(symtab, var_node,
                                            tree->tree_data.var_decl_data.defined_in_unit,
                                            tree->tree_data.var_decl_data.unit_is_public);
                                    }
                                }
                                goto next_identifier;
                            }
                        }

                        if (type_node->type != NULL && type_node->type->kind == TYPE_KIND_PROCEDURE)
                        {
                            kgpc_type_retain(type_node->type);
                            func_return = PushVarOntoScope_Typed(symtab, (char *)ids->cur, type_node->type);
                            if (func_return == 0)
                            {
                                HashNode_t *var_node = NULL;
                                if (FindIdent(&var_node, symtab, ids->cur) != -1 && var_node != NULL)
                                {
                                    var_node->is_var_parameter =
                                        (tree->tree_data.var_decl_data.is_var_param ||
                                         tree->tree_data.var_decl_data.is_untyped_param) ? 1 : 0;
                                    mark_hashnode_unit_info(symtab, var_node,
                                        tree->tree_data.var_decl_data.defined_in_unit,
                                        tree->tree_data.var_decl_data.unit_is_public);
                                }
                            }
                            goto next_identifier;
                        }

                        int declared_type_tag = tree->tree_data.var_decl_data.type;
                        int needs_inline_pointer = (declared_type_tag == POINTER_TYPE &&
                            (type_node->type == NULL || type_node->type->kind != TYPE_KIND_POINTER));

                        if (needs_inline_pointer)
                        {
                            KgpcType *points_to = NULL;
                            if (type_node->type != NULL)
                            {
                                kgpc_type_retain(type_node->type);
                                points_to = type_node->type;
                            }
                            else
                            {
                                struct RecordType *target_record = get_record_type_from_node(type_node);
                                if (target_record != NULL)
                                {
                                    points_to = create_record_type(target_record);
                                }
                                else
                                {
                                    enum VarType target_var_type = get_var_type_from_node(type_node);
                                    int primitive_tag = map_var_type_to_type_tag(target_var_type);
                                    if (primitive_tag != UNKNOWN_TYPE)
                                        points_to = create_primitive_type(primitive_tag);
                                }
                            }

                            KgpcType *pointer_type = create_pointer_type(points_to);
                            func_return = PushVarOntoScope_Typed(symtab, (char *)ids->cur, pointer_type);
                            if (func_return == 0)
                            {
                                HashNode_t *var_node = NULL;
                                if (FindIdent(&var_node, symtab, ids->cur) != -1 && var_node != NULL)
                                {
                                    var_node->is_var_parameter =
                                        (tree->tree_data.var_decl_data.is_var_param ||
                                         tree->tree_data.var_decl_data.is_untyped_param) ? 1 : 0;
                                    mark_hashnode_unit_info(symtab, var_node,
                                        tree->tree_data.var_decl_data.defined_in_unit,
                                        tree->tree_data.var_decl_data.unit_is_public);
                                }
                            }
                            goto next_identifier;
                        }

                        var_type = get_var_type_from_node(type_node);
                        int resolved_tag = map_var_type_to_type_tag(var_type);
                        if (resolved_tag != UNKNOWN_TYPE)
                            tree->tree_data.var_decl_data.type = resolved_tag;
                        struct TypeAlias *alias = get_type_alias_from_node(type_node);
                        if (alias != NULL && alias->is_array)
                        {
                            /* Ensure array bounds are resolved for aliases using symbolic dimensions. */
                            if (alias->array_dimensions != NULL &&
                                type_node->type != NULL &&
                                type_node->type->kind == TYPE_KIND_ARRAY)
                            {
                                resolve_array_bounds_in_kgpctype(symtab, type_node->type, alias);
                            }

                            /* Prefer the KgpcType from the type node if it is already an array. */
                            if (type_node->type != NULL &&
                                type_node->type->kind == TYPE_KIND_ARRAY)
                            {
                                kgpc_type_retain(type_node->type);
                                func_return = PushArrayOntoScope_Typed(symtab, (char *)ids->cur, type_node->type);
                                if (func_return == 0)
                                {
                                    HashNode_t *var_node = NULL;
                                    if (FindIdent(&var_node, symtab, ids->cur) != -1 && var_node != NULL)
                                    {
                                        mark_hashnode_unit_info(symtab, var_node,
                                            tree->tree_data.var_decl_data.defined_in_unit,
                                            tree->tree_data.var_decl_data.unit_is_public);
                                    }
                                }

                                goto next_identifier;
                            }

                            int start = alias->array_start;
                            int end = alias->array_end;
                            if (alias->is_open_array)
                            {
                                start = 0;
                                end = -1;
                            }

                            /* Get element type - it might be a primitive type or a type reference */
                            KgpcType *element_type = NULL;
                            int element_type_tag = alias->array_element_type;

                            /* If element type is a type reference, resolve it */
                            if (element_type_tag == UNKNOWN_TYPE && alias->array_element_type_id != NULL)
                            {
                                HashNode_t *element_type_node = NULL;
                                if (FindIdent(&element_type_node, symtab, alias->array_element_type_id) >= 0 &&
                                    element_type_node != NULL && element_type_node->type != NULL)
                                {
                                    element_type = element_type_node->type;
                                    /* CRITICAL: Retain element_type since it's borrowed from symbol table
                                     * and create_array_type takes ownership. Without this, both the symbol
                                     * table and array_type would try to free the same type, causing double-free. */
                                    kgpc_type_retain(element_type);
                                }
                            }
                            else if (element_type_tag != UNKNOWN_TYPE)
                            {
                                /* Direct primitive type tag - use create_primitive_type */
                                element_type = create_primitive_type(element_type_tag);
                            }

                            /* If element type is still NULL, create an unknown type to avoid crash */
                            if (element_type == NULL)
                            {
                                element_type = create_primitive_type(UNKNOWN_TYPE);
                            }

                            /* Create array KgpcType - takes ownership of element_type */
                            KgpcType *array_type = create_array_type(element_type, start, end);
                            assert(array_type != NULL && "Failed to create array type");

                            /* Set type_alias on KgpcType so it's properly propagated */
                            kgpc_type_set_type_alias(array_type, alias);

                            func_return = PushArrayOntoScope_Typed(symtab, (char *)ids->cur, array_type);
                            if (func_return == 0)
                            {
                                HashNode_t *var_node = NULL;
                                if (FindIdent(&var_node, symtab, ids->cur) != -1 && var_node != NULL)
                                {
                                    mark_hashnode_unit_info(symtab, var_node,
                                        tree->tree_data.var_decl_data.defined_in_unit,
                                        tree->tree_data.var_decl_data.unit_is_public);
                                }
                            }

                            goto next_identifier;
                        }

                        /* For non-array type references (e.g., enum, set, file, record), create KgpcType from type_node */
                        KgpcType *var_kgpc_type = NULL;
                        if (type_node->type != NULL)
                        {
                            /* Type node already has a KgpcType - reference it (don't clone) */
                            var_kgpc_type = type_node->type;
                            if (getenv("KGPC_DEBUG_TYPE_HELPER") != NULL && ids && ids->cur &&
                                pascal_identifier_equals((char*)ids->cur, "Self"))
                            {
                                fprintf(stderr, "[KGPC] semcheck_decls: Pushing Self with type_node->type kind=%d\n",
                                    var_kgpc_type ? var_kgpc_type->kind : -1);
                            }
                            if (var_kgpc_type != NULL)
                                kgpc_type_retain(var_kgpc_type);
                            func_return = PushVarOntoScope_Typed(symtab, (char *)ids->cur, var_kgpc_type);
                        }
                        else
                        {
                            /* Fallback: create KgpcType from legacy fields using helpers */
                            struct RecordType *record_type = get_record_type_from_node(type_node);
                            struct TypeAlias *type_alias = get_type_alias_from_node(type_node);
                            if (type_alias != NULL)
                            {
                                var_kgpc_type = create_kgpc_type_from_type_alias(
                                    type_alias, symtab, tree->tree_data.var_decl_data.defined_in_unit);
                            }
                            if (var_kgpc_type != NULL)
                            {
                                /* No further legacy handling needed */
                            }
                            else if (type_id != NULL && semcheck_is_builtin_pointer_type_id(type_id))
                            {
                                var_type = HASHVAR_POINTER;
                                var_kgpc_type = create_pointer_type(NULL);
                            }
                            else if (record_type != NULL)
                            {
                                /* Use the canonical RecordType, not a clone */
                                var_kgpc_type = create_record_type(record_type);
                            }
                            else if (var_type == HASHVAR_POINTER)
                            {
                                /* For pointer types, we need to create a pointer KgpcType */
                                /* Get the TypeAlias to find what the pointer points to */
                                if (type_alias != NULL && type_alias->is_pointer)
                                {
                                    KgpcType *points_to = NULL;

                                    /* Try to resolve the target type */
                                    if (type_alias->pointer_type_id != NULL)
                                    {
                                        HashNode_t *target_node = NULL;
                                        if (FindIdent(&target_node, symtab, type_alias->pointer_type_id) >= 0 &&
                                            target_node != NULL && target_node->type != NULL)
                                        {
                                            kgpc_type_retain(target_node->type);
                                            points_to = target_node->type;
                                        }
                                    }

                                    /* If we couldn't resolve it, create a placeholder based on pointer_type */
                                    if (points_to == NULL && type_alias->pointer_type != UNKNOWN_TYPE)
                                    {
                                        points_to = create_primitive_type(type_alias->pointer_type);
                                    }

                                    if (points_to != NULL)
                                    {
                                        var_kgpc_type = create_pointer_type(points_to);
                                        kgpc_type_set_type_alias(var_kgpc_type, type_alias);
                                    }
                                }
                            }
                            else
                            {
                                var_kgpc_type = kgpc_type_from_var_type(var_type);
                            }

                            if (var_kgpc_type != NULL && type_alias != NULL && var_type != HASHVAR_POINTER)
                            {
                                kgpc_type_set_type_alias(var_kgpc_type, type_alias);
                            }

                            /* Always use _Typed variant, even if KgpcType is NULL (UNTYPED) */
                            func_return = PushVarOntoScope_Typed(symtab, (char *)ids->cur, var_kgpc_type);
                            if (getenv("KGPC_DEBUG_TYPE_HELPER") != NULL && ids && ids->cur &&
                                pascal_identifier_equals((char*)ids->cur, "Self"))
                                fprintf(stderr, "[KGPC] semcheck_decls: PushVarOntoScope_Typed for Self returned %d\n", func_return);
                        }

                        if (func_return == 0)
                        {
                            HashNode_t *var_node = NULL;
                            if (FindIdent(&var_node, symtab, ids->cur) != -1 && var_node != NULL)
                            {
                                var_node->is_var_parameter =
                                    (tree->tree_data.var_decl_data.is_var_param ||
                                     tree->tree_data.var_decl_data.is_untyped_param) ? 1 : 0;
                                mark_hashnode_unit_info(symtab, var_node,
                                    tree->tree_data.var_decl_data.defined_in_unit,
                                    tree->tree_data.var_decl_data.unit_is_public);
                            }
                        }
                        goto next_identifier;
                    }
                }
                else if (tree->tree_data.var_decl_data.inferred_type)
                {
                    /* For type inference, use INTEGER as placeholder - will be replaced later */
                    var_type = HASHVAR_INTEGER;  /* Placeholder */
                }
                else if (tree->tree_data.var_decl_data.type == UNKNOWN_TYPE &&
                    tree->tree_data.var_decl_data.type_id == NULL &&
                    tree->tree_data.var_decl_data.is_untyped_param)
                {
                    var_type = HASHVAR_UNTYPED;
                }
                else if(tree->tree_data.var_decl_data.type == INT_TYPE)
                    var_type = HASHVAR_INTEGER;
                else if(tree->tree_data.var_decl_data.type == LONGINT_TYPE)
                    var_type = HASHVAR_LONGINT;
                else if(tree->tree_data.var_decl_data.type == BOOL)
                    var_type = HASHVAR_BOOLEAN;
                else if(tree->tree_data.var_decl_data.type == SET_TYPE)
                    var_type = HASHVAR_SET;
                else if(tree->tree_data.var_decl_data.type == ENUM_TYPE)
                    var_type = HASHVAR_ENUM;
                else if(tree->tree_data.var_decl_data.type == STRING_TYPE)
                    var_type = HASHVAR_PCHAR;
                else if(tree->tree_data.var_decl_data.type == RECORD_TYPE)
                    var_type = HASHVAR_RECORD;
                else if(tree->tree_data.var_decl_data.type == FILE_TYPE)
                    var_type = HASHVAR_FILE;
                else if(tree->tree_data.var_decl_data.type == TEXT_TYPE)
                    var_type = HASHVAR_TEXT;
                else if(tree->tree_data.var_decl_data.type == PROCEDURE)
                    var_type = HASHVAR_PROCEDURE;
                else
                    var_type = HASHVAR_REAL;

                /* Create KgpcType for typed variables */
                KgpcType *var_kgpc_type = NULL;
                int var_kgpc_borrowed = 0;
                if (resolved_type != NULL && resolved_type->type != NULL)
                {
                    /* Use KgpcType from resolved type if available */
                    var_kgpc_type = resolved_type->type;
                    var_kgpc_borrowed = 1;
                }
                else if (tree->tree_data.var_decl_data.cached_kgpc_type != NULL)
                {
                    /* Use pre-cached KgpcType (e.g., for inline procedure types) */
                    var_kgpc_type = tree->tree_data.var_decl_data.cached_kgpc_type;
                    var_kgpc_borrowed = 1;
                }
                else if (tree->tree_data.var_decl_data.inline_record_type != NULL)
                {
                    /* Handle inline record type declarations */
                    var_kgpc_type = create_record_type(clone_record_type(tree->tree_data.var_decl_data.inline_record_type));
                }
                else
                {
                    /* Special handling for ShortString - create as array[0..255] of Char */
                    if (var_type == HASHVAR_ARRAY &&
                        tree->tree_data.var_decl_data.type_id != NULL &&
                        pascal_identifier_equals(tree->tree_data.var_decl_data.type_id, "ShortString"))
                    {
                        /* Create ShortString as array[0..255] of Char */
                        KgpcType *char_type = create_primitive_type(CHAR_TYPE);
                        var_kgpc_type = create_array_type(char_type, 0, 255);
                        if (var_kgpc_type != NULL)
                        {
                            struct TypeAlias alias = {0};
                            alias.is_array = 1;
                            alias.array_start = 0;
                            alias.array_end = 255;
                            alias.array_element_type = CHAR_TYPE;
                            alias.array_element_type_id = "char";  /* Will be duplicated by copy_type_alias */
                            alias.is_shortstring = 1;
                            kgpc_type_set_type_alias(var_kgpc_type, &alias);
                        }
                    }
                    /* Handle inline array types (e.g., array[0..2] of PChar) */
                    else if (tree->tree_data.var_decl_data.inline_type_alias != NULL &&
                             tree->tree_data.var_decl_data.inline_type_alias->is_array)
                    {
                        if (getenv("KGPC_DEBUG_SEMCHECK") != NULL)
                            fprintf(stderr, "[SemCheck] Processing inline array for var: %s\n",
                                ids && ids->cur ? (char*)ids->cur : "<null>");
                        struct TypeAlias *alias = tree->tree_data.var_decl_data.inline_type_alias;
                        int start = alias->array_start;
                        int end = alias->array_end;
                        if (alias->is_open_array)
                        {
                            start = 0;
                            end = -1;
                        }

                        /* Get element type */
                        KgpcType *element_type = NULL;
                        int element_type_tag = alias->array_element_type;
                        int element_type_borrowed = 0;  /* Track if borrowed from symbol table */

                        if (element_type_tag == UNKNOWN_TYPE && alias->array_element_type_id != NULL)
                        {
                            HashNode_t *element_type_node = NULL;
                            if (FindIdent(&element_type_node, symtab, alias->array_element_type_id) >= 0 &&
                                element_type_node != NULL && element_type_node->type != NULL)
                            {
                                element_type = element_type_node->type;
                                element_type_borrowed = 1;
                            }
                            else
                            {
                                /* Check for builtin type */
                                int builtin_tag = semcheck_map_builtin_type_name_local(alias->array_element_type_id);
                                if (builtin_tag != UNKNOWN_TYPE)
                                    element_type = create_primitive_type(builtin_tag);
                            }
                        }
                        else if (element_type_tag != UNKNOWN_TYPE)
                        {
                            element_type = create_primitive_type(element_type_tag);
                        }

                        if (element_type != NULL)
                        {
                            /* CRITICAL: Retain element_type if borrowed from symbol table
                             * since create_array_type takes ownership. */
                            if (element_type_borrowed)
                                kgpc_type_retain(element_type);
                            var_kgpc_type = create_array_type(element_type, start, end);
                            kgpc_type_set_type_alias(var_kgpc_type, alias);
                        }
                        else
                        {
                            /* Fallback to var_type if element type unresolved */
                            var_kgpc_type = kgpc_type_from_var_type(var_type);
                        }
                    }
                    else if (tree->tree_data.var_decl_data.inline_type_alias != NULL)
                    {
                        struct TypeAlias *alias = tree->tree_data.var_decl_data.inline_type_alias;
                        if (alias->storage_size > 0)
                        {
                            var_kgpc_type = create_primitive_type_with_size(alias->base_type,
                                (int)alias->storage_size);
                        }
                        else
                        {
                            var_kgpc_type = create_primitive_type(alias->base_type);
                        }
                        if (var_kgpc_type != NULL)
                            kgpc_type_set_type_alias(var_kgpc_type, alias);
                    }
                    else
                    {
                        /* Create KgpcType from var_type */
                        var_kgpc_type = kgpc_type_from_var_type(var_type);
                        if (var_kgpc_type == NULL && var_type == HASHVAR_POINTER)
                        {
                            var_kgpc_type = create_pointer_type(NULL);
                        }
                    }

                    if (var_kgpc_type != NULL)
                    {
                        if (tree->tree_data.var_decl_data.cached_kgpc_type != NULL)
                        {
                            destroy_kgpc_type(tree->tree_data.var_decl_data.cached_kgpc_type);
                            tree->tree_data.var_decl_data.cached_kgpc_type = NULL;
                        }
                        kgpc_type_retain(var_kgpc_type);
                        tree->tree_data.var_decl_data.cached_kgpc_type = var_kgpc_type;
                    }

                    /* Add metadata from resolved_type if present */
                    if (var_kgpc_type != NULL && resolved_type != NULL)
                    {
                        struct TypeAlias *type_alias = get_type_alias_from_node(resolved_type);
                        if (type_alias != NULL)
                        {
                            kgpc_type_set_type_alias(var_kgpc_type, type_alias);
                        }
                        struct RecordType *record_type = get_record_type_from_node(resolved_type);
                        if (record_type != NULL && var_kgpc_type->kind == TYPE_KIND_RECORD)
                        {
                            /* Use the canonical RecordType from the symbol table,
                             * not a clone. This ensures type identity checks work correctly. */
                            var_kgpc_type->info.record_info = record_type;
                        }
                    }
                    else if (tree->tree_data.var_decl_data.inline_type_alias != NULL &&
                        var_kgpc_type != NULL)
                    {
                        kgpc_type_set_type_alias(var_kgpc_type,
                            tree->tree_data.var_decl_data.inline_type_alias);
                    }
                }

                if (var_kgpc_type != NULL && kgpc_type_is_record(var_kgpc_type))
                {
                    struct RecordType *var_record = kgpc_type_get_record(var_kgpc_type);
                    if (var_record != NULL)
                    {
                        /* Skip size computation for generic templates (not yet specialized)
                         * Size can only be computed after type parameters are substituted */
                        int is_unspecialized_generic = (var_record->generic_decl != NULL &&
                                                         var_record->num_generic_args == 0);

                        if (!is_unspecialized_generic)
                        {
                            long long record_size = 0;
                            if (semcheck_compute_record_size(symtab, var_record, &record_size,
                                    tree->line_num) != 0)
                            {
                                return_val += 1;
                            }
                        }
                    }
                }

                /* Always use _Typed variant, even if KgpcType is NULL (UNTYPED) */
                /* Use PushArrayOntoScope_Typed for ShortString (which is array[0..255] of Char) */
                int is_shortstring = (var_type == HASHVAR_ARRAY &&
                                     tree->tree_data.var_decl_data.type_id != NULL &&
                                     pascal_identifier_equals(tree->tree_data.var_decl_data.type_id, "ShortString"));

                if (is_shortstring)
                {
                    if (var_kgpc_borrowed && var_kgpc_type != NULL)
                        kgpc_type_retain(var_kgpc_type);
                    func_return = PushArrayOntoScope_Typed(symtab, (char *)ids->cur, var_kgpc_type);
                }
                else
                {
                    if (var_kgpc_borrowed && var_kgpc_type != NULL)
                        kgpc_type_retain(var_kgpc_type);
                    func_return = PushVarOntoScope_Typed(symtab, (char *)ids->cur, var_kgpc_type);
                }

                if (func_return == 0)
                {
                    HashNode_t *var_node = NULL;
                    if (FindIdent(&var_node, symtab, ids->cur) != -1 && var_node != NULL)
                    {
                        var_node->is_var_parameter =
                            (tree->tree_data.var_decl_data.is_var_param ||
                             tree->tree_data.var_decl_data.is_untyped_param) ? 1 : 0;
                        mark_hashnode_unit_info(symtab, var_node,
                            tree->tree_data.var_decl_data.defined_in_unit,
                            tree->tree_data.var_decl_data.unit_is_public);
                    }
                }
            }
            /* Array declarations */
            else
            {
                assert(tree->type == TREE_ARR_DECL);
                if (getenv("KGPC_DEBUG_SEMCHECK") != NULL)
                    fprintf(stderr, "[SemCheck] Processing TREE_ARR_DECL: %s is_typed_const=%d\n",
                        ids && ids->cur ? (char*)ids->cur : "<null>",
                        tree->tree_data.arr_decl_data.is_typed_const);

                KgpcType *element_type = NULL;
                int element_type_borrowed = 0;  /* Track if borrowed from symbol table */
                int is_array_of_const = (tree->tree_data.arr_decl_data.type == ARRAY_OF_CONST_TYPE);

                /* If type_id is specified, resolve it to get the element type */
                const TypeRef *element_type_ref = tree->tree_data.arr_decl_data.type_ref;
                if (!is_array_of_const &&
                    (tree->tree_data.arr_decl_data.type_id != NULL || element_type_ref != NULL))
                {
                    HashNode_t *element_type_node = NULL;
                    element_type_node = semcheck_find_preferred_type_node_with_ref(symtab,
                        element_type_ref, tree->tree_data.arr_decl_data.type_id);
                    if (element_type_node == NULL &&
                        !tree->tree_data.arr_decl_data.defined_in_unit)
                    {
                        const char *owner_full = semcheck_get_current_subprogram_owner_class_full();
                        const char *owner_outer = semcheck_get_current_subprogram_owner_class_outer();
                        if (owner_full == NULL)
                            owner_full = semcheck_get_current_method_owner();
                        if (owner_full != NULL)
                        {
                            semcheck_maybe_qualify_nested_type(symtab, owner_full, owner_outer,
                                &tree->tree_data.arr_decl_data.type_id,
                                &tree->tree_data.arr_decl_data.type_ref);
                            element_type_ref = tree->tree_data.arr_decl_data.type_ref;
                            element_type_node = semcheck_find_preferred_type_node_with_ref(symtab,
                                element_type_ref, tree->tree_data.arr_decl_data.type_id);
                        }
                    }
                    if (element_type_node != NULL)
                    {
                        /* Use the KgpcType from the resolved type node */
                        element_type = element_type_node->type;
                        element_type_borrowed = 1;  /* Mark as borrowed */
                        if (element_type == NULL)
                        {
                            /* Fallback for migration: some nodes may not have KgpcType populated yet.
                             * Try to construct KgpcType from legacy record type information. */
                            struct RecordType *record_type = get_record_type_from_node(element_type_node);
                            if (record_type != NULL)
                            {
                                /* Use the canonical RecordType, not a clone */
                                element_type = create_record_type(record_type);
                                element_type_borrowed = 0;  /* New type, not borrowed */
                            }
                        }
                    }
                    else
                    {
                        /* Fallback: check for builtin types not in symbol table */
                        const char *type_id = tree->tree_data.arr_decl_data.type_id;
                        if (type_id == NULL && element_type_ref != NULL)
                            type_id = type_ref_base_name(element_type_ref);
                        int builtin_type = semcheck_map_builtin_type_name_local(type_id);
                        if (builtin_type != UNKNOWN_TYPE)
                        {
                            element_type = create_primitive_type(builtin_type);
                        }
                        else
                        {
                            semcheck_error_with_context("Error on line %d: undefined type %s\n",
                                tree->line_num, tree->tree_data.arr_decl_data.type_id);
                            return_val++;
                        }
                    }
                }

                /* If element type not resolved from type_id, use primitive type */
                if (element_type == NULL && !is_array_of_const &&
                    tree->tree_data.arr_decl_data.inline_record_type != NULL)
                {
                    element_type = create_record_type(tree->tree_data.arr_decl_data.inline_record_type);
                    element_type_borrowed = 0;
                }

                /* If element type not resolved from type_id, use primitive type */
                if (element_type == NULL && !is_array_of_const)
                {
                    if(tree->tree_data.arr_decl_data.type == INT_TYPE)
                        var_type = HASHVAR_INTEGER;
                    else if(tree->tree_data.arr_decl_data.type == LONGINT_TYPE)
                        var_type = HASHVAR_LONGINT;
                    else if(tree->tree_data.arr_decl_data.type == BOOL)
                        var_type = HASHVAR_BOOLEAN;
                    else if(tree->tree_data.arr_decl_data.type == STRING_TYPE)
                        var_type = HASHVAR_PCHAR;
                    else if(tree->tree_data.arr_decl_data.type == SHORTSTRING_TYPE)
                        var_type = HASHVAR_PCHAR;  /* ShortString is array of char */
                    else if(tree->tree_data.arr_decl_data.type == CHAR_TYPE)
                        var_type = HASHVAR_CHAR;
                    else if(tree->tree_data.arr_decl_data.type == REAL_TYPE)
                        var_type = HASHVAR_REAL;
                    else {
                        semcheck_error_with_context(
                            "Error on line %d, unknown array element type %d for %s.\n\n",
                            tree->line_num,
                            tree->tree_data.arr_decl_data.type,
                            ids && ids->cur ? (char*)ids->cur : "<unknown>");
                        return_val++;
                        var_type = HASHVAR_REAL;
                    }

                    element_type = kgpc_type_from_var_type(var_type);
                    assert(element_type != NULL && "Array element type must be createable from VarType");
                }

                if (element_type != NULL && kgpc_type_is_record(element_type))
                {
                    struct RecordType *element_record = kgpc_type_get_record(element_type);
                    if (element_record != NULL && !element_record->has_cached_size)
                    {
                        long long record_size = 0;
                        if (semcheck_compute_record_size(symtab, element_record, &record_size,
                                tree->line_num) != 0)
                        {
                            return_val += 1;
                        }
                    }
                }

                /* Resolve array bounds from constant identifiers if necessary.
                 * This handles inline array declarations like: var arr: array[1..N] of integer
                 * where N is a const. The parser stores the original range string (e.g., "1..N")
                 * in range_str, which we parse and resolve here. */
                int start_bound = tree->tree_data.arr_decl_data.s_range;
                int end_bound = tree->tree_data.arr_decl_data.e_range;

                if (tree->tree_data.arr_decl_data.range_str != NULL)
                {
                    char *range_str = tree->tree_data.arr_decl_data.range_str;
                    char *sep = strstr(range_str, "..");

                    if (sep != NULL)
                    {
                        /* Parse "start..end" format */
                        size_t start_len = sep - range_str;
                        char *start_str = (char *)malloc(start_len + 1);
                        char *end_str = strdup(sep + 2);

                        if (start_str != NULL && end_str != NULL)
                        {
                            strncpy(start_str, range_str, start_len);
                            start_str[start_len] = '\0';

                            /* Trim whitespace */
                            char *s = start_str;
                            while (*s == ' ' || *s == '\t') s++;
                            char *e = end_str;
                            while (*e == ' ' || *e == '\t') e++;
                            char *p = s + strlen(s) - 1;
                            while (p > s && (*p == ' ' || *p == '\t')) *p-- = '\0';
                            p = e + strlen(e) - 1;
                            while (p > e && (*p == ' ' || *p == '\t')) *p-- = '\0';

                            /* Try to resolve start bound as constant */
                            long long start_val = 0;
                            if (resolve_const_identifier(symtab, s, &start_val) == 0)
                            {
                                start_bound = (int)start_val;
                            }
                            else
                            {
                                /* Try parsing as integer literal */
                                char *endptr;
                                long num = strtol(s, &endptr, 10);
                                if (*endptr == '\0' && num >= INT_MIN && num <= INT_MAX)
                                    start_bound = (int)num;
                            }

                            /* Try to resolve end bound as constant */
                            long long end_val = 0;
                            if (resolve_const_identifier(symtab, e, &end_val) == 0)
                            {
                                end_bound = (int)end_val;
                            }
                            else
                            {
                                /* Try parsing as integer literal */
                                char *endptr;
                                long num = strtol(e, &endptr, 10);
                                if (*endptr == '\0' && num >= INT_MIN && num <= INT_MAX)
                                    end_bound = (int)num;
                            }

                            free(start_str);
                            free(end_str);
                        }
                    }
                }

                /* CRITICAL FIX: Update the tree's s_range and e_range fields with the resolved bounds.
                 * Downstream code (especially the x86-64 code generator) checks arr->e_range < arr->s_range
                 * to determine if an array is dynamic. Without updating these fields, arrays declared with
                 * constant expressions (e.g., array[1..N] where N is a const) would be incorrectly treated
                 * as dynamic arrays, leading to segmentation faults. */
                tree->tree_data.arr_decl_data.s_range = start_bound;
                tree->tree_data.arr_decl_data.e_range = end_bound;

                KgpcType *array_type = NULL;
                if (is_array_of_const)
                {
                    array_type = create_array_of_const_type();
                }
                else
                {
                    /* CRITICAL: Retain element_type if borrowed from symbol table
                     * since create_array_type takes ownership. */
                    if (element_type_borrowed && element_type != NULL)
                        kgpc_type_retain(element_type);
                    array_type = create_array_type(
                        element_type,
                        start_bound,
                        end_bound
                    );
                }
                assert(array_type != NULL && "Failed to create array type");

                /* If the element type was specified by a type_id (like TAlfa), preserve that information
                 * by creating a minimal TypeAlias and attaching it to the array_type. This allows
                 * nested array indexing to work correctly (e.g., Keywords[1][1] where Keywords is
                 * array[1..5] of TAlfa and TAlfa is array[1..10] of char). */
                if (!is_array_of_const)
                {
                    struct TypeAlias temp_alias = {0};
                    int has_alias = 0;

                    if (tree->tree_data.arr_decl_data.type_id != NULL)
                    {
                        temp_alias.is_array = 1;
                        temp_alias.array_start = start_bound;
                        temp_alias.array_end = end_bound;
                        temp_alias.array_element_type_id = tree->tree_data.arr_decl_data.type_id;  /* Will be duplicated by copy_type_alias */
                        temp_alias.array_element_type = tree->tree_data.arr_decl_data.type;
                        has_alias = 1;
                    }

                    if (tree->tree_data.arr_decl_data.is_shortstring)
                    {
                        if (!has_alias)
                        {
                            temp_alias.is_array = 1;
                            temp_alias.array_start = start_bound;
                            temp_alias.array_end = end_bound;
                            temp_alias.array_element_type = CHAR_TYPE;
                            temp_alias.array_element_type_id = "char";  /* Will be duplicated by copy_type_alias */
                            has_alias = 1;
                        }
                        temp_alias.is_shortstring = 1;
                    }

                    if (has_alias)
                        kgpc_type_set_type_alias(array_type, &temp_alias);
                }

                if (getenv("KGPC_DEBUG_SEMCHECK") != NULL)
                    fprintf(stderr, "[SemCheck] Pushing array: %s, array_type=%p kind=%d elem_kind=%d\n",
                        ids && ids->cur ? (char*)ids->cur : "<null>",
                        (void*)array_type, array_type ? array_type->kind : -1,
                        (array_type && array_type->kind == TYPE_KIND_ARRAY && array_type->info.array_info.element_type) ?
                            array_type->info.array_info.element_type->kind : -1);
                func_return = PushArrayOntoScope_Typed(symtab, (char *)ids->cur, array_type);
                if (getenv("KGPC_DEBUG_SEMCHECK") != NULL)
                    fprintf(stderr, "[SemCheck] PushArrayOntoScope_Typed returned: %d\n", func_return);
            }

            /* Greater than 0 signifies an error */
            if(func_return > 0)
            {
                semantic_error(tree->line_num, 0, "redeclaration of name %s",
                    (char *)ids->cur);
                return_val += func_return;
            }
            else
            {
                HashNode_t *decl_node = NULL;
                if (FindIdent(&decl_node, symtab, ids->cur) != -1 && decl_node != NULL)
                {
                    if (tree->type == TREE_VAR_DECL)
                    {
                        decl_node->is_var_parameter =
                            (tree->tree_data.var_decl_data.is_var_param ||
                             tree->tree_data.var_decl_data.is_untyped_param) ? 1 : 0;
                        mark_hashnode_unit_info(symtab, decl_node,
                            tree->tree_data.var_decl_data.defined_in_unit,
                            tree->tree_data.var_decl_data.unit_is_public);
                    }
                    else
                    {
                        mark_hashnode_unit_info(symtab, decl_node,
                            tree->tree_data.arr_decl_data.defined_in_unit,
                            tree->tree_data.arr_decl_data.unit_is_public);
                        if (tree->tree_data.arr_decl_data.is_typed_const)
                            decl_node->is_typed_const = 1;
                    }
                }
            }

next_identifier:
            ids = ids->next;
        }

        cur = cur->next;
        if (skip_initializer)
            continue;

        if (tree->type == TREE_VAR_DECL && tree->tree_data.var_decl_data.initializer != NULL)
        {
            struct Statement *init_stmt = tree->tree_data.var_decl_data.initializer;

            /* Handle COMPOUND_STATEMENT initializers (from record const lowering) separately */
            if (init_stmt->type == STMT_COMPOUND_STATEMENT)
            {
                /* This is a lowered record const - just semantic check the compound statement */
                return_val += semcheck_stmt(symtab, init_stmt, INT_MAX);
            }
            else if (ids_head == NULL || ids_head->next != NULL)
            {
                semcheck_error_with_context("Error on line %d, type inference initializers must declare a single identifier.\n",
                    tree->line_num);
                ++return_val;
            }
            else
            {
                char *var_name = (char *)ids_head->cur;
                HashNode_t *var_node = NULL;
                if (FindIdent(&var_node, symtab, var_name) == -1 || var_node == NULL)
                {
                    semcheck_error_with_context("Error on line %d, failed to resolve variable %s for initializer.\n",
                        tree->line_num, var_name);
                    ++return_val;
                }
                else
                {
                    struct Statement *init_stmt = tree->tree_data.var_decl_data.initializer;
                    struct Expression *init_expr = init_stmt->stmt_data.var_assign_data.expr;
                    int is_default_param = (init_stmt->type == STMT_VAR_ASSIGN &&
                        init_stmt->stmt_data.var_assign_data.var == NULL);
                    if (init_expr == NULL)
                    {
                        semcheck_error_with_context("Error on line %d, initializer expression is NULL for %s.\n",
                            tree->line_num, var_name);
                        ++return_val;
                        /* Skip remaining processing for this variable but continue with the loop */
                    }
                    else
                    {
                        if (tree->tree_data.var_decl_data.is_typed_const && var_node != NULL)
                            var_node->is_typed_const = 1;

                        if (tree->tree_data.var_decl_data.is_typed_const &&
                            !tree->tree_data.var_decl_data.currency_scaled &&
                            semcheck_is_currency_type_id(tree->tree_data.var_decl_data.type_id))
                        {
                            double real_value = 0.0;
                            if (evaluate_real_const_expr(symtab, init_expr, &real_value) == 0)
                            {
                                long long scaled = llround(real_value * 10000.0);
                                destroy_expr(init_expr);
                                init_expr = mk_inum(tree->line_num, scaled);
                                init_stmt->stmt_data.var_assign_data.expr = init_expr;
                                tree->tree_data.var_decl_data.currency_scaled = 1;
                            }
                        }

                        KgpcType *expr_type = NULL;
                        if (init_expr->type == EXPR_RECORD_CONSTRUCTOR &&
                            (init_expr->resolved_kgpc_type == NULL ||
                             !kgpc_type_is_record(init_expr->resolved_kgpc_type)))
                        {
                            struct RecordType *record_type = NULL;
                            if (var_node->type != NULL && kgpc_type_is_record(var_node->type))
                                record_type = kgpc_type_get_record(var_node->type);
                            else if (var_node->type != NULL && kgpc_type_is_pointer(var_node->type) &&
                                var_node->type->info.points_to != NULL &&
                                kgpc_type_is_record(var_node->type->info.points_to))
                                record_type = kgpc_type_get_record(var_node->type->info.points_to);
                            else if (tree->tree_data.var_decl_data.type_id != NULL)
                            {
                                HashNode_t *type_node = NULL;
                                if (FindIdent(&type_node, symtab,
                                        tree->tree_data.var_decl_data.type_id) >= 0 &&
                                    type_node != NULL)
                                {
                                    record_type = hashnode_get_record_type(type_node);
                                    if (record_type == NULL && type_node->type != NULL)
                                    {
                                        if (kgpc_type_is_record(type_node->type))
                                            record_type = kgpc_type_get_record(type_node->type);
                                        else if (kgpc_type_is_pointer(type_node->type) &&
                                            type_node->type->info.points_to != NULL &&
                                            kgpc_type_is_record(type_node->type->info.points_to))
                                            record_type = kgpc_type_get_record(type_node->type->info.points_to);
                                        else if (type_node->type->type_alias != NULL &&
                                            type_node->type->type_alias->target_type_id != NULL)
                                        {
                                            HashNode_t *target_node = NULL;
                                            if (FindIdent(&target_node, symtab,
                                                    type_node->type->type_alias->target_type_id) >= 0 &&
                                                target_node != NULL)
                                            {
                                                record_type = hashnode_get_record_type(target_node);
                                                if (record_type == NULL && target_node->type != NULL &&
                                                    kgpc_type_is_record(target_node->type))
                                                    record_type = kgpc_type_get_record(target_node->type);
                                            }
                                        }
                                    }
                                }
                                if (record_type == NULL)
                                {
                                    HashNode_t *decl_type_node = NULL;
                                    if (FindIdent(&decl_type_node, symtab,
                                            tree->tree_data.var_decl_data.type_id) >= 0 &&
                                        decl_type_node != NULL)
                                    {
                                        record_type = hashnode_get_record_type(decl_type_node);
                                        if (record_type == NULL && decl_type_node->type != NULL &&
                                            kgpc_type_is_record(decl_type_node->type))
                                            record_type = kgpc_type_get_record(decl_type_node->type);
                                    }
                                }
                            }
                            if (record_type != NULL)
                            {
                                init_expr->record_type = record_type;
                                KgpcType *record_kgpc = create_record_type(record_type);
                                if (record_kgpc != NULL)
                                {
                                    if (init_expr->resolved_kgpc_type != NULL)
                                        destroy_kgpc_type(init_expr->resolved_kgpc_type);
                                    init_expr->resolved_kgpc_type = record_kgpc;
                                    kgpc_type_retain(record_kgpc);
                                    destroy_kgpc_type(record_kgpc);
                                }
                            }
                        }
                        if (init_expr->type == EXPR_ARRAY_LITERAL &&
                            init_expr->array_element_type == UNKNOWN_TYPE &&
                            init_expr->array_element_type_id == NULL)
                        {
                            KgpcType *var_type = (var_node != NULL) ? var_node->type : NULL;
                            if ((var_type == NULL || !kgpc_type_is_array(var_type)) &&
                                tree->tree_data.var_decl_data.type_id != NULL)
                            {
                                HashNode_t *type_node = semcheck_find_preferred_type_node(symtab,
                                    tree->tree_data.var_decl_data.type_id);
                                if (type_node != NULL)
                                    var_type = type_node->type;
                            }
                            if (var_type != NULL && kgpc_type_is_array(var_type))
                            {
                                KgpcType *elem_type = kgpc_type_get_array_element_type_resolved(var_type, symtab);
                                if (elem_type != NULL)
                                    init_expr->array_element_type = semcheck_tag_from_kgpc(elem_type);
                            }
                        }
                        return_val += semcheck_expr_main(symtab, init_expr, INT_MAX, NO_MUTATE, &expr_type);

                        if (tree->tree_data.var_decl_data.is_typed_const && var_node != NULL &&
                            init_expr->type != EXPR_ADDR && init_expr->type != EXPR_ADDR_OF_PROC &&
                            init_expr->type != EXPR_RECORD_CONSTRUCTOR &&
                            init_expr->type != EXPR_ARRAY_LITERAL)
                        {
                        if (expression_is_string(symtab, init_expr))
                            {
                                char *string_value = NULL;
                                if (evaluate_string_const_expr(symtab, init_expr, &string_value) == 0)
                                {
                                    if (var_node->const_string_value != NULL)
                                        free(var_node->const_string_value);
                                    var_node->const_string_value = string_value;
                                    if (string_value != NULL &&
                                        string_value[0] != '\0' && string_value[1] == '\0')
                                    {
                                        var_node->const_int_value = (unsigned char)string_value[0];
                                    }
                                }
                            }
                            else if (expression_contains_real_literal_impl(symtab, init_expr))
                            {
                                double real_value = 0.0;
                                if (evaluate_real_const_expr(symtab, init_expr, &real_value) == 0)
                                    var_node->const_real_value = real_value;
                            }
                            else if (expression_is_set_const_expr(symtab, init_expr))
                            {
                                unsigned char set_bytes[32];
                                size_t set_size = 0;
                                long long mask = 0;
                                int is_char_set = 0;
                                if (evaluate_set_const_bytes(symtab, init_expr, set_bytes, sizeof(set_bytes),
                                        &set_size, &mask, &is_char_set) == 0)
                                {
                                    unsigned char *copy = (unsigned char *)malloc(set_size);
                                    if (copy != NULL)
                                    {
                                        memcpy(copy, set_bytes, set_size);
                                        if (var_node->const_set_value != NULL)
                                            free(var_node->const_set_value);
                                        var_node->const_set_value = copy;
                                        var_node->const_set_size = (int)set_size;
                                    }
                                }
                            }
                            else
                            {
                                long long value = 0;
                                if (evaluate_const_expr(symtab, init_expr, &value) == 0)
                                    var_node->const_int_value = value;
                            }
                        }

                    int expr_tag = expr_type != NULL ? semcheck_tag_from_kgpc(expr_type) : UNKNOWN_TYPE;
                    if (expr_tag == UNKNOWN_TYPE)
                    {
                        if (!is_default_param)
                        {
                            semcheck_error_with_context("Error on line %d, unable to infer type for %s.\n", tree->line_num, var_name);
                            ++return_val;
                        }
                    }
                    else
                    {
                        enum VarType inferred_var_type = HASHVAR_UNTYPED;
                        int normalized_type = expr_tag;

                        switch(expr_tag)
                        {
                            case INT_TYPE:
                            case LONGINT_TYPE:
                                inferred_var_type = HASHVAR_INTEGER;
                                normalized_type = (expr_tag == LONGINT_TYPE) ? LONGINT_TYPE : INT_TYPE;
                                break;
                            case BYTE_TYPE:
                            case WORD_TYPE:
                                inferred_var_type = HASHVAR_INTEGER;
                                normalized_type = expr_tag;
                                break;
                            case LONGWORD_TYPE:
                                inferred_var_type = HASHVAR_LONGINT;
                                normalized_type = LONGWORD_TYPE;
                                break;
                            case QWORD_TYPE:
                                inferred_var_type = HASHVAR_INT64;
                                normalized_type = QWORD_TYPE;
                                break;
                            case INT64_TYPE:
                                inferred_var_type = HASHVAR_INT64;
                                normalized_type = INT64_TYPE;
                                break;
                            case BOOL:
                                inferred_var_type = HASHVAR_BOOLEAN;
                                normalized_type = BOOL;
                                break;
                            case REAL_TYPE:
                                inferred_var_type = HASHVAR_REAL;
                                normalized_type = REAL_TYPE;
                                break;
                            case CHAR_TYPE:
                                inferred_var_type = HASHVAR_CHAR;
                                normalized_type = CHAR_TYPE;
                                if (tree->tree_data.var_decl_data.type_id == NULL)
                                    tree->tree_data.var_decl_data.type_id = strdup("char");
                                break;
                            case STRING_TYPE:
                                inferred_var_type = HASHVAR_PCHAR;
                                normalized_type = STRING_TYPE;
                                if (tree->tree_data.var_decl_data.type_id == NULL)
                                    tree->tree_data.var_decl_data.type_id = strdup("string");
                                break;
                            case SET_TYPE:
                                inferred_var_type = HASHVAR_SET;
                                normalized_type = SET_TYPE;
                                break;
                            case ENUM_TYPE:
                                inferred_var_type = HASHVAR_ENUM;
                                normalized_type = ENUM_TYPE;
                                break;
                            case POINTER_TYPE:
                                inferred_var_type = HASHVAR_POINTER;
                                normalized_type = POINTER_TYPE;
                                break;
                            case RECORD_TYPE:
                                inferred_var_type = HASHVAR_RECORD;
                                normalized_type = RECORD_TYPE;
                                break;
                            default:
                                semcheck_error_with_context("Error on line %d, unsupported inferred type for %s.\n",
                                    tree->line_num, var_name);
                                ++return_val;
                                inferred_var_type = HASHVAR_UNTYPED;
                                break;
                        }

                        if (tree->tree_data.var_decl_data.inferred_type)
                        {
                            if (inferred_var_type != HASHVAR_UNTYPED)
                            {
                                tree->tree_data.var_decl_data.type = normalized_type;
                                /* Replace or create KgpcType with inferred type */
                                KgpcType *inferred_kgpc_type = create_primitive_type(normalized_type);
                                if (inferred_kgpc_type != NULL)
                                {
                                    if (var_node->type != NULL)
                                    {
                                        /* Free old type and replace */
                                        destroy_kgpc_type(var_node->type);
                                    }
                                    var_node->type = inferred_kgpc_type;
                                    /* Legacy field will be populated by helper if needed */
                                }
                            }
                        }
                        else if (!is_default_param)
                        {
                            enum VarType current_var_type = get_var_type_from_node(var_node);
                            int compatible = (inferred_var_type == current_var_type);

                            /* Allow mixing Integer and LongInt for initializers */
                            if (!compatible)
                            {
                                if ((inferred_var_type == HASHVAR_INTEGER || inferred_var_type == HASHVAR_LONGINT) &&
                                    (current_var_type == HASHVAR_INTEGER || current_var_type == HASHVAR_LONGINT))
                                {
                                    compatible = 1;
                                }
                            }
                            if (!compatible)
                            {
                                if (current_var_type == HASHVAR_REAL &&
                                    (inferred_var_type == HASHVAR_INTEGER ||
                                     inferred_var_type == HASHVAR_LONGINT ||
                                     inferred_var_type == HASHVAR_INT64))
                                {
                                    compatible = 1;
                                }
                            }
                            if (!compatible)
                            {
                                if (current_var_type == HASHVAR_INT64 &&
                                    (inferred_var_type == HASHVAR_INTEGER || inferred_var_type == HASHVAR_LONGINT ||
                                     inferred_var_type == HASHVAR_INT64))
                                {
                                    compatible = 1;
                                }
                                else if (inferred_var_type == HASHVAR_INT64 &&
                                         (current_var_type == HASHVAR_INTEGER || current_var_type == HASHVAR_LONGINT))
                                {
                                    compatible = 1;
                                }
                            }

                            if (!compatible && current_var_type == HASHVAR_PCHAR && expr_tag == CHAR_TYPE)
                            {
                                compatible = 1;
                            }
                            /* Allow string literal initializer for array of char or shortstring-like arrays */
                            if (!compatible && current_var_type == HASHVAR_ARRAY && expr_tag == STRING_TYPE)
                            {
                                KgpcType *var_type = (var_node != NULL) ? var_node->type : NULL;
                                if (var_type != NULL && var_type->kind == TYPE_KIND_ARRAY &&
                                    var_type->info.array_info.element_type != NULL &&
                                    var_type->info.array_info.element_type->kind == TYPE_KIND_PRIMITIVE &&
                                    var_type->info.array_info.element_type->info.primitive_type_tag == CHAR_TYPE)
                                {
                                    char *string_value = NULL;
                                    size_t string_len = 0;
                                    if (init_expr != NULL && init_expr->type == EXPR_STRING &&
                                        init_expr->expr_data.string != NULL)
                                    {
                                        string_len = strlen(init_expr->expr_data.string);
                                    }
                                    else if (evaluate_string_const_expr(symtab, init_expr, &string_value) == 0)
                                    {
                                        if (string_value != NULL)
                                            string_len = strlen(string_value);
                                    }
                                    if (string_value != NULL)
                                        free(string_value);

                                    int array_size = var_type->info.array_info.end_index -
                                                     var_type->info.array_info.start_index + 1;
                                    if (array_size < 0)
                                        array_size = 0;
                                    if (string_len <= (size_t)array_size)
                                    {
                                        compatible = 1;
                                    }
                                    else
                                    {
                                        semcheck_error_with_context(
                                            "Error on line %d, initializer string literal too long for %s (string length: %zu, array size: %d).\n",
                                            tree->line_num, var_name, string_len, array_size);
                                        ++return_val;
                                    }
                                }
                            }

                            /* Allow array literal initializer for array variables when element types match */
                            if (!compatible && current_var_type == HASHVAR_ARRAY &&
                                init_expr != NULL && init_expr->type == EXPR_ARRAY_LITERAL)
                            {
                                KgpcType *var_type = (var_node != NULL) ? var_node->type : NULL;
                                if (var_type != NULL && kgpc_type_is_array(var_type))
                                {
                                    KgpcType *elem_type = kgpc_type_get_array_element_type_resolved(var_type, symtab);
                                    int elem_tag = semcheck_tag_from_kgpc(elem_type);
                                    int literal_elem_tag = init_expr->array_element_type;
                                    if (elem_tag != UNKNOWN_TYPE && literal_elem_tag != UNKNOWN_TYPE &&
                                        elem_tag == literal_elem_tag)
                                    {
                                        compatible = 1;
                                    }
                                }
                            }

                            /* Allow pointer type initializers (including nil) for pointer variables.
                             * Both the initializer expression type and the declared variable type must
                             * be pointer types. This enables patterns like: var my_ptr: PChar = nil; */
                            if (!compatible)
                            {
                                int inferred_is_pointer = (inferred_var_type == HASHVAR_POINTER || expr_tag == POINTER_TYPE);
                                int current_is_pointer = (current_var_type == HASHVAR_POINTER);
                                int current_is_proc = (current_var_type == HASHVAR_PROCEDURE);
                                if (var_node->type != NULL)
                                {
                                    current_is_pointer |= kgpc_type_is_pointer(var_node->type);
                                    if (semcheck_tag_from_kgpc(var_node->type) == POINTER_TYPE)
                                        current_is_pointer = 1;
                                    if (var_node->type->kind == TYPE_KIND_PROCEDURE)
                                        current_is_proc = 1;
                                }
                                if (tree->tree_data.var_decl_data.type == POINTER_TYPE)
                                    current_is_pointer = 1;
                                if (tree->tree_data.var_decl_data.type == PROCEDURE)
                                    current_is_proc = 1;
                                if (inferred_is_pointer && current_is_pointer)
                                    compatible = 1;
                                if (!compatible && inferred_is_pointer && current_is_proc)
                                    compatible = 1;
                                if (!compatible && inferred_is_pointer && var_node != NULL)
                                {
                                    struct TypeAlias *alias = hashnode_get_type_alias(var_node);
                                    if (alias != NULL && alias->is_pointer)
                                        compatible = 1;
                                }
                            }

                            if (!compatible && current_var_type == HASHVAR_RECORD && expr_tag == STRING_TYPE)
                            {
                                const char *record_id = NULL;
                                if (var_node->type != NULL)
                                {
                                    struct RecordType *record = kgpc_type_get_record(var_node->type);
                                    if (record != NULL && record->type_id != NULL)
                                        record_id = record->type_id;
                                }
                                if (record_id == NULL)
                                    record_id = tree->tree_data.var_decl_data.type_id;
                                if (record_id != NULL &&
                                    (strcasecmp(record_id, "TGuid") == 0 || strcasecmp(record_id, "GUID") == 0))
                                {
                                    compatible = 1;
                                }
                            }

                            if (!compatible)
                            {
                                semcheck_error_with_context("Error on line %d, initializer type mismatch for %s.\n",
                                    tree->line_num, var_name);
                                ++return_val;
                            }
                        }
                    }
                    }  /* Close else at line 2054 */
                }  /* Close else at line 2043 */
            }  /* Close else at line 2033 */
        }  /* Close if at line 2017 */
        else if (tree->type == TREE_ARR_DECL && tree->tree_data.arr_decl_data.initializer != NULL)
        {
            return_val += semcheck_stmt(symtab, tree->tree_data.arr_decl_data.initializer, INT_MAX);
        }

    }

    return return_val;
}

/* Semantic check on an entire subprogram */
/* A return value greater than 0 indicates how many errors occurred */
int semcheck_subprogram(SymTab_t *symtab, Tree_t *subprogram, int max_scope_lev)
{
    int return_val, func_return;
    int new_max_scope;
    enum TreeType sub_type;
    struct Statement *body;
    HashNode_t *hash_return;

    assert(symtab != NULL);
    assert(subprogram != NULL);

#ifdef DEBUG
    fprintf(stderr, "DEBUG: semcheck_subprogram called for %s\n", subprogram->tree_data.subprogram_data.id);
#endif

    assert(subprogram->type == TREE_SUBPROGRAM);

    Tree_t *prev_current_subprogram = g_semcheck_current_subprogram;
    g_semcheck_current_subprogram = subprogram;

    const char *prev_owner = semcheck_get_current_method_owner();
    char *owner_copy = NULL;
    if (subprogram->tree_data.subprogram_data.owner_class != NULL)
    {
        /* Use full dotted path when available (e.g. "TOuter.TInner") so that
         * semcheck_get_current_method_owner can walk up to outer classes. */
        const char *owner_src = subprogram->tree_data.subprogram_data.owner_class_full;
        if (owner_src == NULL)
            owner_src = subprogram->tree_data.subprogram_data.owner_class;
        owner_copy = strdup(owner_src);
        if (owner_copy != NULL)
            semcheck_set_current_method_owner(owner_copy);
    }

    /* For class methods, copy default parameter values from the class declaration
     * to the implementation. This is needed because Pascal allows defaults only in
     * the declaration, not in the implementation. */
    copy_method_decl_defaults_to_impl(symtab, subprogram);

    /* Record lexical nesting depth so codegen can reason about static links accurately.
     * Store depth as parent depth + 1 so the top-level program has depth 1 and
     * nested subprograms continue to increase. */
    subprogram->tree_data.subprogram_data.nesting_level = max_scope_lev + 1;
    subprogram->tree_data.subprogram_data.is_nested =
        (subprogram->tree_data.subprogram_data.owner_class == NULL &&
         subprogram->tree_data.subprogram_data.nesting_level > 1);
    int default_requires = (subprogram->tree_data.subprogram_data.nesting_level > 1 &&
        !subprogram->tree_data.subprogram_data.defined_in_unit);
    subprogram->tree_data.subprogram_data.requires_static_link = default_requires ? 1 : 0;

    char *id_to_use_for_lookup;

    sub_type = subprogram->tree_data.subprogram_data.sub_type;
    assert(sub_type == TREE_SUBPROGRAM_PROC || sub_type == TREE_SUBPROGRAM_FUNC);

    return_val = 0;
    return_val += semcheck_id_not_main(subprogram->tree_data.subprogram_data.id);

    /* Imported subprogram parameter declarations must retain imported-origin
     * metadata; otherwise semcheck_decls may rebind type_id-based cached types
     * (e.g. UnixType.TSize) against local shadows (e.g. Types.TSize record). */
    if (subprogram->tree_data.subprogram_data.defined_in_unit)
    {
        ListNode_t *arg_mark = subprogram->tree_data.subprogram_data.args_var;
        while (arg_mark != NULL)
        {
            if (arg_mark->type == LIST_TREE && arg_mark->cur != NULL)
            {
                Tree_t *arg_tree = (Tree_t *)arg_mark->cur;
                if (arg_tree->type == TREE_VAR_DECL)
                {
                    arg_tree->tree_data.var_decl_data.defined_in_unit = 1;
                    if (subprogram->tree_data.subprogram_data.unit_is_public)
                        arg_tree->tree_data.var_decl_data.unit_is_public = 1;
                }
                else if (arg_tree->type == TREE_ARR_DECL)
                {
                    arg_tree->tree_data.arr_decl_data.defined_in_unit = 1;
                    if (subprogram->tree_data.subprogram_data.unit_is_public)
                        arg_tree->tree_data.arr_decl_data.unit_is_public = 1;
                }
            }
            arg_mark = arg_mark->next;
        }
    }

    // --- Name Mangling Logic ---
    // Only set mangled_id if not already set by predeclare_subprogram (which handles
    // nested functions with unique parent$child naming)
    if (subprogram->tree_data.subprogram_data.mangled_id == NULL) {
        static int debug_external = -1;
        if (debug_external == -1)
            debug_external = (getenv("KGPC_DEBUG_EXTERNAL") != NULL);
        const char *explicit_name = subprogram->tree_data.subprogram_data.cname_override;
        if (explicit_name != NULL) {
            char *overload_mangled = MangleFunctionName(
                subprogram->tree_data.subprogram_data.id,
                subprogram->tree_data.subprogram_data.args_var,
                symtab);
            if (overload_mangled != NULL)
                subprogram->tree_data.subprogram_data.mangled_id = overload_mangled;
            else
                subprogram->tree_data.subprogram_data.mangled_id = strdup(explicit_name);
        } else if (subprogram->tree_data.subprogram_data.cname_flag) {
            const char *export_name = subprogram->tree_data.subprogram_data.id;
            if (debug_external) {
                fprintf(stderr, "[SemCheck] cname_flag id=%s alias=%s\n",
                    subprogram->tree_data.subprogram_data.id,
                    export_name != NULL ? export_name : "(null)");
            }
            char *overload_mangled = MangleFunctionName(
                subprogram->tree_data.subprogram_data.id,
                subprogram->tree_data.subprogram_data.args_var,
                symtab);
            if (overload_mangled != NULL)
                subprogram->tree_data.subprogram_data.mangled_id = overload_mangled;
            else if (export_name != NULL)
                subprogram->tree_data.subprogram_data.mangled_id = strdup(export_name);
            else
                subprogram->tree_data.subprogram_data.mangled_id = NULL;
        } else {
            // Pass the symbol table to the mangler
            subprogram->tree_data.subprogram_data.mangled_id = MangleFunctionName(
                subprogram->tree_data.subprogram_data.id,
                subprogram->tree_data.subprogram_data.args_var,
                symtab);
        }
    }
    id_to_use_for_lookup = subprogram->tree_data.subprogram_data.id;

    /* Check if already declared (e.g., by predeclare_subprogram in two-pass approach) */
    HashNode_t *existing_decl = NULL;
    int already_declared = 0;

    /* For overloaded functions, find the correct overload by matching mangled name */
    if (subprogram->tree_data.subprogram_data.mangled_id != NULL)
    {
        ListNode_t *all_matches = FindAllIdents(symtab, id_to_use_for_lookup);
        ListNode_t *cur = all_matches;

        if (getenv("KGPC_DEBUG_OVERLOAD_MATCH") != NULL)
        {
            fprintf(stderr, "[KGPC] Matching impl %s mangled=%s\n",
                id_to_use_for_lookup ? id_to_use_for_lookup : "<null>",
                subprogram->tree_data.subprogram_data.mangled_id);
            ListNode_t *debug_cur = all_matches;
            while (debug_cur != NULL)
            {
                HashNode_t *debug_cand = (HashNode_t *)debug_cur->cur;
                fprintf(stderr, "  candidate: id=%s mangled=%s\n",
                    debug_cand && debug_cand->id ? debug_cand->id : "<null>",
                    debug_cand && debug_cand->mangled_id ? debug_cand->mangled_id : "<null>");
                debug_cur = debug_cur->next;
            }
        }

        HashNode_t *first_mangled_match = NULL;
        while (cur != NULL)
        {
            HashNode_t *candidate = (HashNode_t *)cur->cur;
            if (candidate != NULL && candidate->mangled_id != NULL &&
                strcmp(candidate->mangled_id, subprogram->tree_data.subprogram_data.mangled_id) == 0)
            {
                if (first_mangled_match == NULL)
                    first_mangled_match = candidate;
                /* When multiple candidates share the same mangled name (e.g.,
                 * FpFStat and FPFStat both mangle to fpfstat_li), prefer the
                 * one with an equivalent signature. */
                if (candidate->type != NULL &&
                    candidate->type->kind == TYPE_KIND_PROCEDURE &&
                    candidate->type->info.proc_info.definition != NULL &&
                    semcheck_subprogram_signatures_equivalent(
                        subprogram, candidate->type->info.proc_info.definition))
                {
                    existing_decl = candidate;
                    already_declared = 1;
                    break;
                }
            }
            cur = cur->next;
        }
        /* If no signature-exact match found, fall back to the first mangled match */
        if (existing_decl == NULL && first_mangled_match != NULL)
        {
            existing_decl = first_mangled_match;
            already_declared = 1;
        }

        if (all_matches != NULL)
            DestroyList(all_matches);
    }

    /* Fallback to simple lookup if no mangled name or no match found */
    if (!already_declared)
        already_declared = (FindIdent(&existing_decl, symtab, id_to_use_for_lookup) == 0);

    if (already_declared && existing_decl != NULL &&
        subprogram->tree_data.subprogram_data.mangled_id != NULL)
    {
        if (existing_decl->mangled_id != NULL)
            free(existing_decl->mangled_id);
        existing_decl->mangled_id = strdup(subprogram->tree_data.subprogram_data.mangled_id);
    }

    /**** FIRST PLACING SUBPROGRAM ON THE CURRENT SCOPE ****/
    if(sub_type == TREE_SUBPROGRAM_PROC)
    {
        /* Create KgpcType for the procedure */
        /* Create KgpcType for the procedure */
        KgpcType *proc_type = NULL;
        int created_new_type = 0;

        if (already_declared && existing_decl != NULL && existing_decl->type != NULL)
        {
            proc_type = existing_decl->type;
            Tree_t *prev_def = proc_type->info.proc_info.definition;
            if (subprogram->tree_data.subprogram_data.statement_list != NULL &&
                (prev_def == NULL || prev_def->tree_data.subprogram_data.statement_list == NULL))
            {
                proc_type->info.proc_info.definition = subprogram;
            }
            /* Copy default parameter values from forward declaration to implementation */
            copy_default_values_to_impl_params(
                proc_type->info.proc_info.params,
                subprogram->tree_data.subprogram_data.args_var);
        }
        else
        {
            proc_type = create_procedure_type(
                subprogram->tree_data.subprogram_data.args_var,
                NULL  /* procedures have no return type */
            );
            if (proc_type != NULL)
            {
                proc_type->info.proc_info.definition = subprogram;
                if (subprogram->tree_data.subprogram_data.return_type_id != NULL)
                    proc_type->info.proc_info.return_type_id =
                        strdup(subprogram->tree_data.subprogram_data.return_type_id);
            }
            created_new_type = 1;
        }

        /* ARCHITECTURAL FIX: Resolve array bounds in parameter types now that constants are in scope */
        if (proc_type != NULL && proc_type->info.proc_info.params != NULL)
        {
            ListNode_t *param = proc_type->info.proc_info.params;
            while (param != NULL)
            {
                if (param->type == LIST_TREE && param->cur != NULL)
                {
                    Tree_t *param_tree = (Tree_t *)param->cur;
                    /* For array parameters, resolve the bounds */
                    if (param_tree->type == TREE_ARR_DECL)
                    {
                        /* If element type is a named type, look it up to get proper KgpcType */
                        if (param_tree->tree_data.arr_decl_data.type_id != NULL)
                        {
                            HashNode_t *elem_type_node = NULL;
                            if (FindIdent(&elem_type_node, symtab, param_tree->tree_data.arr_decl_data.type_id) != -1 &&
                                elem_type_node != NULL && elem_type_node->type != NULL)
                            {
                                /* Element type resolved - bounds should be in the tree node */
                                /* Nothing to do here */
                            }
                        }
                    }
                }
                param = param->next;
            }
        }

        // Use the typed version to properly set the KgpcType
        // Skip if already declared
        if (!already_declared)
        {
            func_return = PushProcedureOntoScope_Typed(symtab, id_to_use_for_lookup,
                            subprogram->tree_data.subprogram_data.mangled_id,
                            proc_type);
            semcheck_update_symbol_alias(symtab, id_to_use_for_lookup,
                subprogram->tree_data.subprogram_data.mangled_id);
            FindIdent(&existing_decl, symtab, id_to_use_for_lookup);
        }
        else
        {
            func_return = 0;  /* No error since it's expected to be already declared */

            /* If we created a new type but it was already declared (e.g. existing had no type), update it */
            if (created_new_type && existing_decl != NULL && existing_decl->type == NULL)
            {
                existing_decl->type = proc_type;
            }
            else if (created_new_type)
            {
                /* We created a type but didn't use it (shouldn't happen if logic is correct, but for safety) */
                destroy_kgpc_type(proc_type);
            }
        }

        /* Copy method identity fields from the subprogram tree to the symbol. */
        if (existing_decl != NULL && subprogram->tree_data.subprogram_data.method_name != NULL)
        {
            if (existing_decl->method_name == NULL)
                existing_decl->method_name = strdup(subprogram->tree_data.subprogram_data.method_name);
            if (existing_decl->owner_class == NULL && subprogram->tree_data.subprogram_data.owner_class != NULL)
                existing_decl->owner_class = strdup(subprogram->tree_data.subprogram_data.owner_class);
            if (existing_decl->owner_class_full == NULL &&
                subprogram->tree_data.subprogram_data.owner_class_full != NULL)
                existing_decl->owner_class_full = strdup(subprogram->tree_data.subprogram_data.owner_class_full);
            if (existing_decl->owner_class_outer == NULL &&
                subprogram->tree_data.subprogram_data.owner_class_outer != NULL)
                existing_decl->owner_class_outer = strdup(subprogram->tree_data.subprogram_data.owner_class_outer);
        }
        semcheck_propagate_method_identity(symtab, subprogram);

        PushScope(symtab);

        /* For method implementations, add class vars to scope */
        add_class_vars_to_method_scope(symtab, subprogram);
        /* For nested types (e.g. TOuter.TInner), also add outer class
         * vars/consts to scope. owner_class_full contains the dotted path. */
        if (subprogram->tree_data.subprogram_data.owner_class_full != NULL)
            add_outer_class_vars_to_method_scope(symtab, subprogram);

        if (existing_decl != NULL && existing_decl->type != NULL)
        {
            kgpc_type_retain(existing_decl->type);
            PushProcedureOntoScope_Typed(symtab, id_to_use_for_lookup,
                subprogram->tree_data.subprogram_data.mangled_id,
                existing_decl->type);
            semcheck_update_symbol_alias(symtab, id_to_use_for_lookup,
                subprogram->tree_data.subprogram_data.mangled_id);
            destroy_kgpc_type(existing_decl->type);
        }

        new_max_scope = max_scope_lev+1;
    }
    else // Function
    {
        KgpcType *return_kgpc_type = NULL;

        /* Reuse the type created during predeclaration when possible. */
        if (already_declared && existing_decl != NULL &&
            existing_decl->type != NULL &&
            existing_decl->type->kind == TYPE_KIND_PROCEDURE)
        {
            return_kgpc_type = kgpc_type_get_return_type(existing_decl->type);
            if (subprogram->tree_data.subprogram_data.statement_list != NULL)
            {
                Tree_t *prev_def = existing_decl->type->info.proc_info.definition;
                if (prev_def == NULL || prev_def->tree_data.subprogram_data.statement_list == NULL)
                {
                    existing_decl->type->info.proc_info.definition = subprogram;
                }
            }
            /* Copy default parameter values from forward declaration to implementation */
            copy_default_values_to_impl_params(
                existing_decl->type->info.proc_info.params,
                subprogram->tree_data.subprogram_data.args_var);
        }

        /* If the predeclare step could not resolve the type (e.g., inline array),
         * build it now and update the existing declaration. */
        if (return_kgpc_type == NULL)
        {
            int before_ret = return_val;
            return_kgpc_type = build_function_return_type(subprogram, symtab, &return_val, 0);
            semcheck_debug_error_step("build_return_type", subprogram, before_ret, return_val);
#ifdef DEBUG
            if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_subprogram %s error after build_function_return_type: %d\n", subprogram->tree_data.subprogram_data.id, return_val);
#endif
        }

        KgpcType *func_type = NULL;
        if (!already_declared)
        {
#ifdef DEBUG
            fprintf(stderr, "DEBUG: semcheck_subprogram %s NOT already declared in Pass 2!\n", subprogram->tree_data.subprogram_data.id);
#endif
            func_type = create_procedure_type(
                subprogram->tree_data.subprogram_data.args_var,
                return_kgpc_type
            );
            if (func_type != NULL)
            {
                func_type->info.proc_info.definition = subprogram;
                if (subprogram->tree_data.subprogram_data.return_type_id != NULL)
                    func_type->info.proc_info.return_type_id =
                        strdup(subprogram->tree_data.subprogram_data.return_type_id);
            }
            func_return = PushFunctionOntoScope_Typed(symtab, id_to_use_for_lookup,
                            subprogram->tree_data.subprogram_data.mangled_id,
                            func_type);
            semcheck_update_symbol_alias(symtab, id_to_use_for_lookup,
                subprogram->tree_data.subprogram_data.mangled_id);
        }
        else
        {
            func_return = 0;
            if (existing_decl != NULL)
            {
                if (existing_decl->type == NULL)
                {
                    func_type = create_procedure_type(
                        subprogram->tree_data.subprogram_data.args_var,
                        return_kgpc_type
                    );
                    if (func_type != NULL)
                    {
                        func_type->info.proc_info.definition = subprogram;
                        if (subprogram->tree_data.subprogram_data.return_type_id != NULL)
                            func_type->info.proc_info.return_type_id =
                                strdup(subprogram->tree_data.subprogram_data.return_type_id);
                    }
                    existing_decl->type = func_type;
                }
                else if (return_kgpc_type != NULL)
                {
                    existing_decl->type->info.proc_info.return_type = return_kgpc_type;
                }
            }
        }

        /* Copy method identity fields from the subprogram tree to the symbol. */
        if (existing_decl != NULL && subprogram->tree_data.subprogram_data.method_name != NULL)
        {
            if (existing_decl->method_name == NULL)
                existing_decl->method_name = strdup(subprogram->tree_data.subprogram_data.method_name);
            if (existing_decl->owner_class == NULL && subprogram->tree_data.subprogram_data.owner_class != NULL)
                existing_decl->owner_class = strdup(subprogram->tree_data.subprogram_data.owner_class);
            if (existing_decl->owner_class_full == NULL &&
                subprogram->tree_data.subprogram_data.owner_class_full != NULL)
                existing_decl->owner_class_full = strdup(subprogram->tree_data.subprogram_data.owner_class_full);
            if (existing_decl->owner_class_outer == NULL &&
                subprogram->tree_data.subprogram_data.owner_class_outer != NULL)
                existing_decl->owner_class_outer = strdup(subprogram->tree_data.subprogram_data.owner_class_outer);
        }
        semcheck_propagate_method_identity(symtab, subprogram);

        PushScope(symtab);
        if (getenv("KGPC_DEBUG_TYPE_HELPER") != NULL)
            fprintf(stderr, "[KGPC] semcheck_subprogram (func): PushScope for %s\n",
                subprogram->tree_data.subprogram_data.id);

        /* For method implementations, add class vars to scope */
        add_class_vars_to_method_scope(symtab, subprogram);
        /* For nested types (e.g. TOuter.TInner), also add outer class
         * vars/consts to scope. owner_class_full contains the dotted path. */
        if (subprogram->tree_data.subprogram_data.owner_class_full != NULL)
            add_outer_class_vars_to_method_scope(symtab, subprogram);

        // **THIS IS THE FIX FOR THE RETURN VALUE**:
        // Use the ORIGINAL name for the internal return variable with KgpcType
        // Always use _Typed variant, even if KgpcType is NULL
        PushFuncRetOntoScope_Typed(symtab, id_to_use_for_lookup, return_kgpc_type);

/* Also add "Result" as an alias for the return variable for Pascal compatibility */
        /* Check if "Result" is already used in the current scope */
        HashNode_t *result_check = NULL;
        HashTable_t *cur_hash = (HashTable_t *)symtab->stack_head->cur;
        result_check = (cur_hash != NULL) ? FindIdentInTable(cur_hash, "Result") : NULL;
        if (result_check == NULL)
        {
            /* "Result" is not already declared in this scope, so add it as an alias */
            if (return_kgpc_type != NULL)
                kgpc_type_retain(return_kgpc_type);
            PushFuncRetOntoScope_Typed(symtab, "Result", return_kgpc_type);
            if (getenv("KGPC_DEBUG_RESULT") != NULL)
            {
                fprintf(stderr,
                    "[KGPC] add Result alias for %s type=%s\n",
                    subprogram->tree_data.subprogram_data.id ?
                        subprogram->tree_data.subprogram_data.id : "<anon>",
                    kgpc_type_to_string(return_kgpc_type));
            }
        }
        else if (getenv("KGPC_DEBUG_RESULT") != NULL)
        {
            fprintf(stderr,
                "[KGPC] Result alias already exists for %s existing_type=%s\n",
                subprogram->tree_data.subprogram_data.id ?
                    subprogram->tree_data.subprogram_data.id : "<anon>",
                kgpc_type_to_string(result_check->type));
        }

        /* For operator declarations with named result variables (e.g., "operator :=(src) dest: variant"),
         * push the named result variable as an additional alias for the return variable. */
        if (subprogram->tree_data.subprogram_data.result_var_name != NULL)
        {
            PushFuncRetOntoScope_Typed(symtab, subprogram->tree_data.subprogram_data.result_var_name, return_kgpc_type);
        }

        /* For class methods, also add an alias using the unmangled method name.
         * The method_name field contains the bare name (e.g., "_AddRef", "ReadNext"). */
        const char *alias_suffix = subprogram->tree_data.subprogram_data.method_name;
        if (alias_suffix != NULL && alias_suffix[0] != '\0')
        {
            size_t alias_len = strlen(alias_suffix);
            if (alias_len > 0 && alias_len < 128)
            {
                char alias_buf[128];
                memcpy(alias_buf, alias_suffix, alias_len);
                alias_buf[alias_len] = '\0';

                HashNode_t *suffix_check = NULL;
                if (FindIdent(&suffix_check, symtab, alias_buf) == -1)
                    PushFuncRetOntoScope_Typed(symtab, alias_buf, return_kgpc_type);
            }
        }
        /* Note: We don't check for "result" anymore since it conflicts with built-in Result alias */

        /* Note: Type metadata now in KgpcType, no post-creation writes needed */

        new_max_scope = max_scope_lev+1;
    }

    /**** Check the subprogram internals now ****/

    /* Greater than 0 signifies an error */
    if (func_return > 0)
    {
        int current_has_body = (subprogram->tree_data.subprogram_data.statement_list != NULL);
        HashNode_t *existing_decl = NULL;
        if (FindIdent(&existing_decl, symtab, id_to_use_for_lookup) == 0 &&
            existing_decl != NULL &&
            existing_decl->mangled_id != NULL &&
            subprogram->tree_data.subprogram_data.mangled_id != NULL &&
            strcmp(existing_decl->mangled_id, subprogram->tree_data.subprogram_data.mangled_id) == 0)
        {
            func_return = 0;
        }
        else
        {
            ListNode_t *candidates = FindAllIdents(symtab, id_to_use_for_lookup);
            ListNode_t *iter = candidates;
            while (iter != NULL)
            {
                HashNode_t *candidate = (HashNode_t *)iter->cur;
                if (candidate != NULL && candidate->type != NULL &&
                    candidate->type->kind == TYPE_KIND_PROCEDURE)
                {
                    Tree_t *def = candidate->type->info.proc_info.definition;
                    int existing_has_body = (def != NULL &&
                        def->tree_data.subprogram_data.statement_list != NULL);
                    if (existing_has_body != current_has_body)
                    {
                        func_return = 0;
                        break;
                    }
                }
                iter = iter->next;
            }
            if (candidates != NULL)
                DestroyList(candidates);
        }
    }
    if(func_return > 0)
    {
        fprintf(stderr, "On line %d: redeclaration of name %s!\n",
            subprogram->line_num, subprogram->tree_data.subprogram_data.id);

        return_val += func_return;
    }

    /* These arguments are themselves like declarations */
    if (getenv("KGPC_DEBUG_TYPE_HELPER") != NULL) {
        ListNode_t *arg_debug = subprogram->tree_data.subprogram_data.args_var;
        fprintf(stderr, "[KGPC] semcheck_subprogram: %s args:\n", subprogram->tree_data.subprogram_data.id);
        while (arg_debug != NULL) {
            if (arg_debug->type == LIST_TREE && arg_debug->cur != NULL) {
                Tree_t *arg_tree = (Tree_t *)arg_debug->cur;
                if (arg_tree->type == TREE_VAR_DECL) {
                    ListNode_t *ids = arg_tree->tree_data.var_decl_data.ids;
                    if (ids != NULL && ids->cur != NULL)
                        fprintf(stderr, "  - %s (type_id=%s)\n", (char*)ids->cur,
                            arg_tree->tree_data.var_decl_data.type_id ? arg_tree->tree_data.var_decl_data.type_id : "<null>");
                }
            }
            arg_debug = arg_debug->next;
        }
    }
    /* Register generic type parameters (e.g., T, U) as opaque types in the function scope.
     * This allows parameters of type T and expressions using T to pass semantic checking. */
    if (subprogram->tree_data.subprogram_data.num_generic_type_params > 0) {
        for (int i = 0; i < subprogram->tree_data.subprogram_data.num_generic_type_params; i++) {
            const char *tparam = subprogram->tree_data.subprogram_data.generic_type_params[i];
            assert(tparam != NULL);
            KgpcType *opaque = create_primitive_type(POINTER_TYPE);
            PushTypeOntoScope_Typed(symtab, (char *)tparam, opaque);
            destroy_kgpc_type(opaque);
        }
    } else {
        /* Some parsed method implementations (notably generic class methods in FPC RTL)
         * may omit explicit generic_type_params metadata even though their signatures
         * contain placeholders like T/U. Infer only single-letter uppercase params. */
        ListNode_t *arg_node = subprogram->tree_data.subprogram_data.args_var;
        while (arg_node != NULL) {
            if (arg_node->type == LIST_TREE && arg_node->cur != NULL) {
                Tree_t *arg_tree = (Tree_t *)arg_node->cur;
                const char *type_id = NULL;
                if (arg_tree->type == TREE_VAR_DECL)
                    type_id = arg_tree->tree_data.var_decl_data.type_id;
                else if (arg_tree->type == TREE_ARR_DECL)
                    type_id = arg_tree->tree_data.arr_decl_data.type_id;
                if (type_id != NULL && type_id[0] >= 'A' && type_id[0] <= 'Z' &&
                    type_id[1] == '\0')
                {
                    HashNode_t *existing = NULL;
                    if (FindIdent(&existing, symtab, type_id) == -1) {
                        KgpcType *opaque = create_primitive_type(POINTER_TYPE);
                        PushTypeOntoScope_Typed(symtab, (char *)type_id, opaque);
                        destroy_kgpc_type(opaque);
                    }
                }
            }
            arg_node = arg_node->next;
        }

        const char *ret_type_id = subprogram->tree_data.subprogram_data.return_type_id;
        if (ret_type_id != NULL && ret_type_id[0] >= 'A' && ret_type_id[0] <= 'Z' &&
            ret_type_id[1] == '\0')
        {
            HashNode_t *existing = NULL;
            if (FindIdent(&existing, symtab, ret_type_id) == -1) {
                KgpcType *opaque = create_primitive_type(POINTER_TYPE);
                PushTypeOntoScope_Typed(symtab, (char *)ret_type_id, opaque);
                destroy_kgpc_type(opaque);
            }
        }
    }
    {
        int before_args = return_val;
        return_val += semcheck_decls(symtab, subprogram->tree_data.subprogram_data.args_var);
        semcheck_debug_error_step("args_decls", subprogram, before_args, return_val);
    }
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_subprogram %s error after args: %d\n", subprogram->tree_data.subprogram_data.id, return_val);
#endif

    /* Ensure parameter identifiers are present in scope even if parsing produced
     * incomplete type info (e.g., untyped const params in helper methods). */
    {
        ListNode_t *param_node = subprogram->tree_data.subprogram_data.args_var;
        while (param_node != NULL)
        {
            if (param_node->type == LIST_TREE && param_node->cur != NULL)
            {
                Tree_t *param_tree = (Tree_t *)param_node->cur;
                ListNode_t *ids = NULL;
                if (param_tree->type == TREE_VAR_DECL)
                    ids = param_tree->tree_data.var_decl_data.ids;
                else if (param_tree->type == TREE_ARR_DECL)
                    ids = param_tree->tree_data.arr_decl_data.ids;

                while (ids != NULL)
                {
                    HashNode_t *existing = NULL;
                    if (FindIdent(&existing, symtab, ids->cur) == -1)
                    {
                        KgpcType *param_type = NULL;
                        if (param_tree->type == TREE_VAR_DECL)
                        {
                            param_type = param_tree->tree_data.var_decl_data.cached_kgpc_type;
                            if (param_type == NULL && param_tree->tree_data.var_decl_data.type_id != NULL)
                            {
                                int builtin_tag = semcheck_map_builtin_type_name_local(
                                    param_tree->tree_data.var_decl_data.type_id);
                                if (builtin_tag != UNKNOWN_TYPE)
                                    param_type = create_primitive_type(builtin_tag);
                            }
                            if (param_type != NULL &&
                                param_type == param_tree->tree_data.var_decl_data.cached_kgpc_type)
                            {
                                kgpc_type_retain(param_type);
                            }
                            PushVarOntoScope_Typed(symtab, (char *)ids->cur, param_type);
                            if (param_type != NULL &&
                                param_type != param_tree->tree_data.var_decl_data.cached_kgpc_type)
                            {
                                destroy_kgpc_type(param_type);
                            }
                        }
                        else if (param_tree->type == TREE_ARR_DECL)
                        {
                            PushArrayOntoScope_Typed(symtab, (char *)ids->cur, NULL);
                        }

                        if (FindIdent(&existing, symtab, ids->cur) != -1 && existing != NULL)
                        {
                            int is_var_param = 0;
                            int is_untyped = 0;
                            if (param_tree->type == TREE_VAR_DECL)
                            {
                                is_var_param = param_tree->tree_data.var_decl_data.is_var_param;
                                is_untyped = param_tree->tree_data.var_decl_data.is_untyped_param;
                            }
                            existing->is_var_parameter = (is_var_param || is_untyped) ? 1 : 0;
                        }
                    }
                    ids = ids->next;
                }
            }
            param_node = param_node->next;
        }
    }

    /* Ensure methods always have an implicit Self in scope when missing.
     * This is critical for record/class method bodies that access bare fields
     * (e.g. cx/cy) and for helper methods with expanded signatures. */
    {
        HashNode_t *self_node = NULL;
        const char *owner_id = semcheck_get_current_method_owner();
        struct RecordType *owner_record = NULL;
        if (owner_id != NULL)
        {
            HashNode_t *owner_node = semcheck_find_owner_record_type_node(symtab, owner_id);
            if (owner_node == NULL)
                owner_node = semcheck_find_preferred_type_node(symtab, owner_id);
            if (owner_node != NULL)
                owner_record = get_record_type_from_node(owner_node);
        }
        if (owner_record != NULL)
        {
            KgpcType *self_type = NULL;

            if (owner_record->is_type_helper && owner_record->helper_base_type_id != NULL)
            {
                HashNode_t *type_node = semcheck_find_preferred_type_node(symtab,
                    owner_record->helper_base_type_id);
                if (type_node != NULL && type_node->type != NULL)
                {
                    kgpc_type_retain(type_node->type);
                    self_type = type_node->type;
                }
                else
                {
                    int builtin_tag = semcheck_map_builtin_type_name_local(
                        owner_record->helper_base_type_id);
                    if (builtin_tag != UNKNOWN_TYPE)
                        self_type = create_primitive_type(builtin_tag);
                }
            }
            else
            {
                KgpcType *owner_kgpc = create_record_type(owner_record);
                if (owner_kgpc != NULL && record_type_is_class(owner_record))
                    owner_kgpc = create_pointer_type(owner_kgpc);
                self_type = owner_kgpc;
            }

            if (self_type != NULL)
            {
                if (FindIdent(&self_node, symtab, "Self") == -1)
                {
                    kgpc_type_retain(self_type);
                    PushVarOntoScope_Typed(symtab, "Self", self_type);
                }
                else if (self_node->type == NULL || !kgpc_type_equals(self_node->type, self_type))
                {
                    if (self_node->type != NULL)
                        destroy_kgpc_type(self_node->type);
                    kgpc_type_retain(self_type);
                    self_node->type = self_type;
                }
                destroy_kgpc_type(self_type);
            }
        }
    }

    {
        int before_local = return_val;
        return_val += predeclare_enum_literals(symtab, subprogram->tree_data.subprogram_data.type_declarations);
        /* Pre-declare types so they're available for const expressions like High(MyType) */
        return_val += predeclare_types(symtab, subprogram->tree_data.subprogram_data.type_declarations);
        return_val += semcheck_const_decls(symtab, subprogram->tree_data.subprogram_data.const_declarations);
        return_val += semcheck_type_decls(symtab, subprogram->tree_data.subprogram_data.type_declarations);
        return_val += semcheck_decls(symtab, subprogram->tree_data.subprogram_data.declarations);
        semcheck_debug_error_step("local_decls", subprogram, before_local, return_val);
    }
#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_subprogram %s error after decls: %d\n", subprogram->tree_data.subprogram_data.id, return_val);
#endif

    {
        int before_nested = return_val;
        return_val += semcheck_subprograms(symtab, subprogram->tree_data.subprogram_data.subprograms,
                        new_max_scope, subprogram);
        semcheck_debug_error_step("nested_subprograms", subprogram, before_nested, return_val);
    }

    body = subprogram->tree_data.subprogram_data.statement_list;
    if (body == NULL)
    {
        g_semcheck_current_subprogram = prev_current_subprogram;
        PopScope(symtab);
#ifdef DEBUG
        fprintf(stderr, "DEBUG: semcheck_subprogram %s returning (no body): %d\n", subprogram->tree_data.subprogram_data.id, return_val);
#endif
        return return_val;
    }

    /* Functions cannot have side effects, so need to call a special function in that case */
    if(sub_type == TREE_SUBPROGRAM_PROC)
    {
        {
            int before_body = return_val;
            return_val += semcheck_stmt(symtab, body, new_max_scope);
            semcheck_debug_error_step("proc_body", subprogram, before_body, return_val);
        }
    }
    else
    {
        assert(FindIdent(&hash_return, symtab, subprogram->tree_data.subprogram_data.id)
                    == 0);

        ResetHashNodeStatus(hash_return);
        int func_stmt_ret = 0;
        {
            int before_body = return_val;
            func_stmt_ret = semcheck_func_stmt(symtab, body, new_max_scope);
            return_val += func_stmt_ret;
            semcheck_debug_error_step("func_body", subprogram, before_body, return_val);
        }
#ifdef DEBUG
        if (func_stmt_ret > 0) fprintf(stderr, "DEBUG: semcheck_subprogram %s error after semcheck_func_stmt: %d\n", subprogram->tree_data.subprogram_data.id, func_stmt_ret);
#endif

        /* Allow functions with asm blocks to skip explicit return assignment */
        int has_asm = statement_contains_asm_block(body);

        /* Constructors implicitly yield the constructed instance, so do not
         * require an explicit assignment to the return variable. */
        int is_constructor = 0;
        {
            const char *name = subprogram->tree_data.subprogram_data.method_name;
            if (name == NULL)
                name = subprogram->tree_data.subprogram_data.id;
            if (name != NULL && strcasecmp(name, "create") == 0)
                is_constructor = 1;
        }

        /* Check if either the function name or "Result" was assigned to */
        int return_was_assigned = is_constructor ? 1 : (hash_return->mutated != NO_MUTATE);
        if (!return_was_assigned)
        {
            /* Also check if "Result" was mutated */
            HashNode_t *result_node = NULL;
            if (FindIdent(&result_node, symtab, "Result") == 0 && result_node != NULL)
            {
                return_was_assigned = (result_node->mutated != NO_MUTATE);
            }
        }

        /* Methods use mangled identifiers like Class__Func; allow assignments to the
         * unmangled method name to satisfy the return check. */
        if (!return_was_assigned && subprogram->tree_data.subprogram_data.method_name != NULL) {
            const char *mname = subprogram->tree_data.subprogram_data.method_name;
            HashNode_t *suffix_node = NULL;
            if (FindIdent(&suffix_node, symtab, mname) == 0 && suffix_node != NULL) {
                return_was_assigned = (suffix_node->mutated != NO_MUTATE);
            }
        }

        if(!return_was_assigned && !has_asm)
        {
#ifdef DEBUG
            fprintf(stderr, "DEBUG: Checking return for %s. cname_override=%s cname_flag=%d return_was_assigned=%d\n",
                subprogram->tree_data.subprogram_data.id,
                subprogram->tree_data.subprogram_data.cname_override ? subprogram->tree_data.subprogram_data.cname_override : "<null>",
                subprogram->tree_data.subprogram_data.cname_flag,
                return_was_assigned);
#endif

            /* Skip check for external functions (cname_flag or cname_override) or declarations without bodies */
            int is_external = (subprogram->tree_data.subprogram_data.cname_override != NULL) ||
                              (subprogram->tree_data.subprogram_data.cname_flag != 0);
            int has_body = (subprogram->tree_data.subprogram_data.statement_list != NULL);
            if (is_external || !has_body)
            {
                /* External function, no body expected */
            }
            else
            {
                /* FPC treats this as a warning, not an error - function result may be uninitialized */
                fprintf(stderr,
                    "Warning in function %s: function result does not seem to be set\n\n",
                    subprogram->tree_data.subprogram_data.id);
                g_semcheck_warning_count++;
            }
        }
    }

    if(optimize_flag() > 0 && return_val == 0)
    {
        optimize(symtab, subprogram);
    }

    if (subprogram->tree_data.subprogram_data.id != NULL)
    {
        ListNode_t *defs = FindAllIdents(symtab, subprogram->tree_data.subprogram_data.id);
        ListNode_t *iter = defs;
        while (iter != NULL)
        {
            if (iter->cur != NULL)
            {
                HashNode_t *node = (HashNode_t *)iter->cur;
                if (node != NULL &&
                    node->mangled_id != NULL &&
                    subprogram->tree_data.subprogram_data.mangled_id != NULL &&
                    strcmp(node->mangled_id, subprogram->tree_data.subprogram_data.mangled_id) == 0)
                {
                    node->requires_static_link =
                        subprogram->tree_data.subprogram_data.requires_static_link ? 1 : 0;
                    node->has_nested_requiring_link =
                        subprogram->tree_data.subprogram_data.has_nested_requiring_link ? 1 : 0;
                }
            }
            iter = iter->next;
        }
        DestroyList(defs);
    }

    g_semcheck_current_subprogram = prev_current_subprogram;
    PopScope(symtab);

#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_subprogram %s returning at end: %d\n", subprogram->tree_data.subprogram_data.id, return_val);
#endif
    if (owner_copy != NULL)
    {
        semcheck_set_current_method_owner(prev_owner);
        free(owner_copy);
    }

    return return_val;
}


/* Pre-declare a subprogram (add to symbol table without processing body)
 * This is used for forward declarations so all procedures are visible
 * before any bodies are processed.
 */
static int predeclare_subprogram(SymTab_t *symtab, Tree_t *subprogram, int max_scope_lev, Tree_t *parent_subprogram)
{
    int return_val = 0;
    int func_return;
    enum TreeType sub_type;

    assert(symtab != NULL);
    assert(subprogram != NULL);
    assert(subprogram->type == TREE_SUBPROGRAM);

    char *id_to_use_for_lookup;

    sub_type = subprogram->tree_data.subprogram_data.sub_type;
    assert(sub_type == TREE_SUBPROGRAM_PROC || sub_type == TREE_SUBPROGRAM_FUNC);

    return_val += semcheck_id_not_main(subprogram->tree_data.subprogram_data.id);

    /* Debug output for procedure predeclaration */
    if (getenv("KGPC_DEBUG_PREDECLARE_PROC") != NULL)
    {
        fprintf(stderr, "[PREDECLARE_PROC] %s (line %d) parent=%s\n",
                subprogram->tree_data.subprogram_data.id,
                subprogram->line_num,
                parent_subprogram != NULL ? parent_subprogram->tree_data.subprogram_data.id : "(null)");
    }

    // --- Name Mangling Logic ---
    /* Preserve any dotted class prefix from the original mangled_id set by
     * convert_method_impl (e.g. "TOuter.TInner__DoWork").  This is needed
     * so that semcheck_get_current_method_owner can walk up to outer classes
     * when resolving class vars / consts in nested object methods. */
    if (subprogram->tree_data.subprogram_data.mangled_id != NULL)
    {
        free(subprogram->tree_data.subprogram_data.mangled_id);
        subprogram->tree_data.subprogram_data.mangled_id = NULL;
    }

    const char *predeclare_name = subprogram->tree_data.subprogram_data.cname_override;
    if (predeclare_name != NULL) {
        subprogram->tree_data.subprogram_data.mangled_id = strdup(predeclare_name);
    } else if (subprogram->tree_data.subprogram_data.cname_flag) {
        subprogram->tree_data.subprogram_data.mangled_id = strdup(subprogram->tree_data.subprogram_data.id);
    } else {
        // Pass the symbol table to the mangler
        char *base_mangled = MangleFunctionName(
            subprogram->tree_data.subprogram_data.id,
            subprogram->tree_data.subprogram_data.args_var,
            symtab);

        // For nested functions, prepend the parent's mangled_id to make the name unique
        if (parent_subprogram != NULL && parent_subprogram->tree_data.subprogram_data.mangled_id != NULL) {
            const char *parent_mangled = parent_subprogram->tree_data.subprogram_data.mangled_id;
            size_t len = strlen(parent_mangled) + 2 + strlen(base_mangled) + 1; // parent$nested\0
            char *nested_mangled = malloc(len);
            if (nested_mangled != NULL) {
                snprintf(nested_mangled, len, "%s$%s", parent_mangled, base_mangled);
                free(base_mangled);
                subprogram->tree_data.subprogram_data.mangled_id = nested_mangled;
            } else {
                subprogram->tree_data.subprogram_data.mangled_id = base_mangled;
            }
        } else {
            subprogram->tree_data.subprogram_data.mangled_id = base_mangled;
        }
    }

    if (getenv("KGPC_DEBUG_PREDECLARE_PROC") != NULL)
    {
        fprintf(stderr, "[PREDECLARE_PROC]   -> mangled_id=%s\n",
                subprogram->tree_data.subprogram_data.mangled_id);
    }

    id_to_use_for_lookup = subprogram->tree_data.subprogram_data.id;

    /* Check if this specific overload is already declared (by matching mangled name) */
    if (subprogram->tree_data.subprogram_data.mangled_id != NULL)
    {
        ListNode_t *all_matches = FindAllIdents(symtab, id_to_use_for_lookup);
        ListNode_t *cur = all_matches;
        int already_exists = 0;
        while (cur != NULL)
        {
            HashNode_t *candidate = (HashNode_t *)cur->cur;
            if (candidate != NULL && candidate->mangled_id != NULL &&
                strcmp(candidate->mangled_id, subprogram->tree_data.subprogram_data.mangled_id) == 0)
            {
                int same_signature = 1;
                if (candidate->type != NULL && candidate->type->kind == TYPE_KIND_PROCEDURE)
                {
                    Tree_t *candidate_def = candidate->type->info.proc_info.definition;
                    if (candidate_def != NULL &&
                        !semcheck_subprogram_signatures_equivalent(subprogram, candidate_def))
                    {
                        same_signature = 0;
                    }
                }
                if (same_signature)
                {
                    already_exists = 1;
                    break;
                }
            }
            /* If this exact subprogram was already registered (even with a different
             * mangled name), reuse that declaration instead of creating a duplicate. */
            if (candidate != NULL && candidate->type != NULL &&
                candidate->type->kind == TYPE_KIND_PROCEDURE &&
                candidate->type->info.proc_info.definition == subprogram)
            {
                if (candidate->mangled_id != NULL)
                    free(candidate->mangled_id);
                candidate->mangled_id = strdup(subprogram->tree_data.subprogram_data.mangled_id);
                already_exists = 1;
                break;
            }
            cur = cur->next;
        }
        if (all_matches != NULL)
            DestroyList(all_matches);

        if (already_exists)
            return 0;  /* Already declared - skip to avoid duplicates */
    }

    /* If a declaration already exists for this name/signature, skip predeclaring
     * the matching implementation body. */
    {
        int current_has_body = (subprogram->tree_data.subprogram_data.statement_list != NULL);
        ListNode_t *all_matches = FindAllIdents(symtab, id_to_use_for_lookup);
        ListNode_t *cur = all_matches;
        while (cur != NULL)
        {
            HashNode_t *candidate = (HashNode_t *)cur->cur;
            if (candidate == NULL || candidate->hash_type == HASHTYPE_BUILTIN_PROCEDURE)
            {
                cur = cur->next;
                continue;
            }
            if (candidate->type != NULL && candidate->type->kind == TYPE_KIND_PROCEDURE)
            {
                Tree_t *def = candidate->type->info.proc_info.definition;
                int existing_has_body = (def != NULL &&
                    def->tree_data.subprogram_data.statement_list != NULL);
                int same_signature = 0;

                if (def != NULL &&
                    semcheck_subprogram_signatures_equivalent(subprogram, def))
                {
                    same_signature = 1;
                }
                else if (candidate->mangled_id != NULL &&
                         subprogram->tree_data.subprogram_data.mangled_id != NULL &&
                         strcmp(candidate->mangled_id,
                                subprogram->tree_data.subprogram_data.mangled_id) == 0)
                {
                    same_signature = 1;
                }

                if (same_signature && existing_has_body != current_has_body)
                {
                    if (all_matches != NULL)
                        DestroyList(all_matches);
                    return 0;
                }
            }
            cur = cur->next;
        }
        if (all_matches != NULL)
            DestroyList(all_matches);
    }

    /**** PLACE SUBPROGRAM ON THE CURRENT SCOPE ****/
    if(sub_type == TREE_SUBPROGRAM_PROC)
    {
        /* Create KgpcType for the procedure */
        KgpcType *proc_type = create_procedure_type(
            subprogram->tree_data.subprogram_data.args_var,
            NULL  /* procedures have no return type */
        );
        if (proc_type != NULL) {
            proc_type->info.proc_info.definition = subprogram;
            if (subprogram->tree_data.subprogram_data.return_type_id != NULL)
                proc_type->info.proc_info.return_type_id =
                    strdup(subprogram->tree_data.subprogram_data.return_type_id);
        }

        // Add to current scope
        func_return = PushProcedureOntoScope_Typed(symtab, id_to_use_for_lookup,
                        subprogram->tree_data.subprogram_data.mangled_id,
                        proc_type);

        if(func_return > 0)
        {
            fprintf(stderr, "On line %d: redeclaration of name %s!\n",
                subprogram->line_num, subprogram->tree_data.subprogram_data.id);
            return_val += func_return;
        }
    }
    else // Function
    {
        KgpcType *return_kgpc_type = build_function_return_type(subprogram, symtab, &return_val, 1);
#ifdef DEBUG
        if (return_val > 0) fprintf(stderr, "DEBUG: predeclare_subprogram %s error after build_function_return_type: %d\n", subprogram->tree_data.subprogram_data.id, return_val);
#endif

        /* Create function KgpcType */
        KgpcType *func_type = create_procedure_type(
            subprogram->tree_data.subprogram_data.args_var,
            return_kgpc_type  /* functions have a return type */
        );
        if (func_type != NULL) {
            func_type->info.proc_info.definition = subprogram;
            if (subprogram->tree_data.subprogram_data.return_type_id != NULL)
                func_type->info.proc_info.return_type_id =
                    strdup(subprogram->tree_data.subprogram_data.return_type_id);
        }

        // Add to current scope
        func_return = PushFunctionOntoScope_Typed(symtab, id_to_use_for_lookup,
                        subprogram->tree_data.subprogram_data.mangled_id,
                        func_type);

        if(func_return > 0)
        {
            fprintf(stderr, "On line %d: redeclaration of name %s!\n",
                subprogram->line_num, subprogram->tree_data.subprogram_data.id);
            return_val += func_return;
        }
    }

#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: predeclare_subprogram %s returning error: %d\n", subprogram->tree_data.subprogram_data.id, return_val);
#endif

    return return_val;
}


/* Semantic check on multiple subprograms */
/* A return value greater than 0 indicates how many errors occurred */
/* Forward declaration - we'll define this after semcheck_subprogram */
static int predeclare_subprogram(SymTab_t *symtab, Tree_t *subprogram, int max_scope_lev, Tree_t *parent_subprogram);
static void semcheck_update_symbol_alias(SymTab_t *symtab, const char *id, const char *alias);

/* Predeclare a list of subprograms without processing bodies.
 * Safe to call multiple times thanks to duplicate checks in predeclare_subprogram. */
static int predeclare_subprograms(SymTab_t *symtab, ListNode_t *subprograms, int max_scope_lev,
    Tree_t *parent_subprogram)
{
    int return_val = 0;
    ListNode_t *cur = subprograms;
    while (cur != NULL)
    {
        assert(cur->cur != NULL);
        assert(cur->type == LIST_TREE);
        Tree_t *child = (Tree_t *)cur->cur;
        return_val += predeclare_subprogram(symtab, child, max_scope_lev, parent_subprogram);
        /* Note: We intentionally do NOT predeclare nested subprograms here globally.
         * Nested subprograms are predeclared within their parent's scope when
         * semcheck_subprogram calls semcheck_subprograms (which has its own Pass 1).
         * This ensures nested functions with the same name in different parents
         * don't shadow each other. */
        cur = cur->next;
    }
    return return_val;
}

int semcheck_subprograms(SymTab_t *symtab, ListNode_t *subprograms, int max_scope_lev,
    Tree_t *parent_subprogram)
{
    ListNode_t *cur;
    int return_val;
    assert(symtab != NULL);

#ifdef DEBUG
    fprintf(stderr, "DEBUG: semcheck_subprograms called. subprograms=%p\n", subprograms);
#endif

    return_val = 0;

    /* ARCHITECTURAL FIX: Two-pass approach to ensure all procedure declarations
     * are visible before processing any bodies. This fixes the issue where unit
     * procedures were not visible in nested user procedures because they came
     * later in the subprograms list.
     *
     * Pass 1: Declare all procedures (add to symbol table)
     * Pass 2: Process bodies (which may reference procedures declared in pass 1)
     */

    /* Pass 1: Pre-declare all procedures at this level */
    cur = subprograms;
    while(cur != NULL)
    {
        assert(cur->cur != NULL);
        assert(cur->type == LIST_TREE);
        return_val += predeclare_subprogram(symtab, (Tree_t *)cur->cur, max_scope_lev, parent_subprogram);
        cur = cur->next;
    }

#ifdef DEBUG
    if (return_val > 0) fprintf(stderr, "DEBUG: semcheck_subprograms error after Pass 1: %d\n", return_val);
#endif

    /* Pass 2: Process full semantic checking including bodies */
    cur = subprograms;
    while(cur != NULL)
    {
        assert(cur->cur != NULL);
        assert(cur->type == LIST_TREE);
        Tree_t *child = (Tree_t *)cur->cur;
        if (child != NULL &&
            child->tree_data.subprogram_data.defined_in_unit &&
            child->tree_data.subprogram_data.statement_list != NULL &&
            getenv("KGPC_SKIP_IMPORTED_IMPL_BODIES") != NULL)
        {
            /* Imported unit implementation bodies are not part of the consumer
             * unit's semantic pass. Keep declarations (pass 1) but skip bodies. */
            cur = cur->next;
            continue;
        }
        if (child != NULL &&
            child->tree_data.subprogram_data.statement_list == NULL &&
            child->tree_data.subprogram_data.cname_flag == 0 &&
            child->tree_data.subprogram_data.cname_override == NULL)
        {
            /* Interface/forward declaration with no body. */
            cur = cur->next;
            continue;
        }
        return_val += semcheck_subprogram(symtab, child, max_scope_lev);
        /* If child needs a static link, mark parent as having nested children that need links.
         * This is used by codegen to know when to PASS a static link when calling nested functions.
         * We do NOT propagate requires_static_link to parent - the parent only needs to RECEIVE
         * a static link if it's nested itself or accesses outer scope variables. */
        if (parent_subprogram != NULL &&
            child != NULL &&
            child->tree_data.subprogram_data.requires_static_link)
        {
            parent_subprogram->tree_data.subprogram_data.has_nested_requiring_link = 1;
        }
        cur = cur->next;
    }

    return return_val;
}
