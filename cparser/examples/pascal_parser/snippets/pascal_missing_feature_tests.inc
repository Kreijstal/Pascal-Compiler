void test_pascal_unit_declaration(void) {
    combinator_t* p = get_unit_parser();

    input_t* input = new_input();
    input->buffer = strdup("unit MyUnit; interface implementation end.");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    // This test is expected to fail because unit parsing is not implemented.
    TEST_ASSERT(res.is_success);

    if (res.is_success) {
        TEST_ASSERT(res.value.ast->typ == PASCAL_T_UNIT_DECL);
        ast_t* unit_name = res.value.ast->child;
        TEST_ASSERT(unit_name->typ == PASCAL_T_IDENTIFIER);
        TEST_ASSERT(strcmp(unit_name->sym->name, "MyUnit") == 0);

        ast_t* interface_sec = unit_name->next;
        TEST_ASSERT(interface_sec->typ == PASCAL_T_INTERFACE_SECTION);

        ast_t* implementation_sec = interface_sec->next;
        TEST_ASSERT(implementation_sec->typ == PASCAL_T_IMPLEMENTATION_SECTION);
        free_ast(res.value.ast);
    } else {
        // We expect this path to be taken.
        // We must free the error object.
        free_error(res.value.error);
    }
    
    // Don't free the shared parser
    free(input->buffer);
    free(input);
}

void test_pascal_pointer_type_declaration(void) {
    combinator_t* p = get_program_parser();

    input_t* input = new_input();
    char* program = "program Test;\n"
                   "type\n"
                   "  PMyRec = ^TMyRec;\n"
                   "begin\n"
                   "end.\n";
    input->buffer = strdup(program);
    input->length = strlen(program);

    ParseResult res = parse(input, p);

    // This test is expected to fail because pointer types are not implemented.
    TEST_ASSERT(res.is_success);

    if (res.is_success) {
        // If it succeeds, we should check the structure
        ast_t* program_decl = res.value.ast;
        TEST_ASSERT(program_decl->typ == PASCAL_T_PROGRAM_DECL);

        // Find type section
        ast_t* current = program_decl->child;
        while(current && current->typ != PASCAL_T_TYPE_SECTION) {
            current = current->next;
        }
        TEST_ASSERT(current != NULL);
        TEST_ASSERT(current->typ == PASCAL_T_TYPE_SECTION);

        ast_t* type_decl = current->child;
        TEST_ASSERT(type_decl->typ == PASCAL_T_TYPE_DECL);

        ast_t* type_spec = type_decl->child->next;
        TEST_ASSERT(type_spec->typ == PASCAL_T_TYPE_SPEC);

        ast_t* pointer_type = type_spec->child;
        TEST_ASSERT(pointer_type->typ == PASCAL_T_POINTER_TYPE);

        ast_t* referenced_type = pointer_type->child;
        TEST_ASSERT(referenced_type->typ == PASCAL_T_IDENTIFIER);
        TEST_ASSERT(strcmp(referenced_type->sym->name, "TMyRec") == 0);

        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }    free(input->buffer);
    free(input);
}

void test_pascal_method_implementation(void) {
    combinator_t* p = get_program_parser();

    input_t* input = new_input();
    char* program = "program Test;\n"
                   "type\n"
                   "  TMyObject = class\n"
                   "    procedure MyMethod;\n"
                   "  end;\n"
                   "procedure TMyObject.MyMethod;\n"
                   "begin\n"
                   "end;\n"
                   "begin\n"
                   "end.\n";
    input->buffer = strdup(program);
    input->length = strlen(program);

    ParseResult res = parse(input, p);

    // This test is expected to fail because method implementations are not fully supported.
    TEST_ASSERT(res.is_success);

    if (res.is_success) {
        // If it succeeds, we should check the structure
        ast_t* program_decl = res.value.ast;
        TEST_ASSERT(program_decl->typ == PASCAL_T_PROGRAM_DECL);

        // Find the method implementation
        ast_t* current = program_decl->child;
        while(current && current->typ != PASCAL_T_METHOD_IMPL) {
            current = current->next;
        }
        TEST_ASSERT(current != NULL);
        TEST_ASSERT(current->typ == PASCAL_T_METHOD_IMPL);

        // The first child of the METHOD_IMPL node should be the QUALIFIED_IDENTIFIER node.
        ast_t* qualified_id_node = current->child;
        TEST_ASSERT(qualified_id_node != NULL);
        TEST_ASSERT(qualified_id_node->typ == PASCAL_T_QUALIFIED_IDENTIFIER);

        // Now, check the children of the QUALIFIED_IDENTIFIER node.
        // First child is the ClassName.
        ast_t* class_name_node = qualified_id_node->child;
        TEST_ASSERT(class_name_node != NULL);
        TEST_ASSERT(class_name_node->typ == PASCAL_T_IDENTIFIER);
        TEST_ASSERT(strcmp(class_name_node->sym->name, "TMyObject") == 0);

        // Second child is the MethodName.
        ast_t* method_name_node = class_name_node->next;
        TEST_ASSERT(method_name_node != NULL);
        TEST_ASSERT(method_name_node->typ == PASCAL_T_IDENTIFIER);
        TEST_ASSERT(strcmp(method_name_node->sym->name, "MyMethod") == 0);

        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }    free(input->buffer);
    free(input);
}

void test_pascal_with_statement(void) {
    combinator_t* p = new_combinator();
    init_pascal_statement_parser(&p);

    input_t* input = new_input();
    input->buffer = strdup("with MyRecord do field := 1;");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    // This test is expected to fail because 'with' statements are not implemented.
    TEST_ASSERT(res.is_success);

    if (res.is_success) {
        ast_t* with_stmt = res.value.ast;
        TEST_ASSERT(with_stmt->typ == PASCAL_T_WITH_STMT);

        ast_t* record_var = with_stmt->child;
        TEST_ASSERT(record_var->typ == PASCAL_T_IDENTIFIER);
        TEST_ASSERT(strcmp(record_var->sym->name, "MyRecord") == 0);

        ast_t* statement = record_var->next;
        TEST_ASSERT(statement->typ == PASCAL_T_ASSIGNMENT);

        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }    free(input->buffer);
    free(input);
}

void test_pascal_exit_statement(void) {
    combinator_t* p = new_combinator();
    init_pascal_statement_parser(&p);

    input_t* input = new_input();
    input->buffer = strdup("exit;");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    // This test is expected to fail because 'exit' statements are not implemented.
    TEST_ASSERT(res.is_success);

    if (res.is_success) {
        ast_t* exit_stmt = res.value.ast;
        TEST_ASSERT(exit_stmt->typ == PASCAL_T_EXIT_STMT);

        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }    free(input->buffer);
    free(input);
}

void test_pascal_include_directive(void) {
    combinator_t* p = get_program_parser();

    input_t* input = new_input();
    char* program = "program Test;\n"
                   "begin\n"
                   "  {$I test.inc}\n"
                   "end.\n";
    input->buffer = strdup(program);
    input->length = strlen(program);

    ParseResult res = parse(input, p);

    // The parser should succeed, treating the include as a comment.
    TEST_ASSERT(res.is_success);

    if (res.is_success) {
        // The main block should be empty, as the include was ignored.
        ast_t* program_decl = res.value.ast;
        ast_t* main_block = NULL;
        ast_t* current = program_decl->child;
        while(current) {
            if (current->typ == PASCAL_T_MAIN_BLOCK) {
                main_block = current;
                break;
            }
            current = current->next;
        }

        TEST_ASSERT(main_block != NULL);
        TEST_ASSERT(main_block->child == NULL);

        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }    free(input->buffer);
    free(input);
}

void test_pascal_forward_declared_function(void) {
    combinator_t* p = get_unit_parser();

    input_t* input = new_input();
    char* unit_code = "unit MyUnit;\n"
                      "interface\n"
                      "  procedure DoSomething;\n"
                      "implementation\n"
                      "  procedure DoSomething;\n"
                      "  begin\n"
                      "  end;\n"
                      "begin\n"
                      "  DoSomething;\n"
                      "end.\n";
    input->buffer = strdup(unit_code);
    input->length = strlen(unit_code);

    ParseResult res = parse(input, p);

    // This test is expected to fail because unit parsing is not fully implemented.
    TEST_ASSERT(res.is_success);

    if (res.is_success) {
        // If it succeeds, we'd check the AST to ensure the call is resolved.
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }    free(input->buffer);
    free(input);
}
