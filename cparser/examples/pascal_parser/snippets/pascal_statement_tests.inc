// --- Pascal Statement Tests ---

void test_pascal_assignment_statement(void) {
    combinator_t* p = new_combinator();
    init_pascal_program_parser(&p);  // Use program parser for terminated statements

    input_t* input = new_input();
    input->buffer = strdup("x := 42;");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    // The program parser wraps in a NONE node, so get the actual statement
    ast_t* stmt = res.value.ast;
    // For terminated statements, we expect the actual statement type directly
    if (stmt->typ == PASCAL_T_NONE) {
        stmt = stmt->child;
    }
    TEST_ASSERT(stmt->typ == PASCAL_T_ASSIGNMENT);
    
    // Check identifier
    ast_t* identifier = stmt->child;
    TEST_ASSERT(identifier->typ == PASCAL_T_IDENTIFIER);
    TEST_ASSERT(strcmp(identifier->sym->name, "x") == 0);
    
    // Check assignment operator (skipped in AST)
    
    // Check expression value
    ast_t* value = identifier->next;
    TEST_ASSERT(value->typ == PASCAL_T_INTEGER);
    TEST_ASSERT(strcmp(value->sym->name, "42") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free(input);
}

void test_pascal_expression_statement(void) {
    combinator_t* p = new_combinator();
    init_pascal_program_parser(&p);  // Use program parser for terminated statements

    input_t* input = new_input();
    input->buffer = strdup("writeln(\"Hello\");");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    TEST_CHECK(res.is_success);
    if (!res.is_success) {
        if (res.value.error) {
            printf("Parse error: %s at line %d, col %d\n", 
                   res.value.error->message, res.value.error->line, res.value.error->col);
            free_error(res.value.error);
        }        free(input->buffer);
        free(input);
        return;
    }
    
    // Get the actual statement
    ast_t* stmt = res.value.ast;
    if (stmt->typ == PASCAL_T_NONE) {
        stmt = stmt->child;
    }
    
    TEST_CHECK(stmt && stmt->typ == PASCAL_T_STATEMENT);
    if (!stmt || stmt->typ != PASCAL_T_STATEMENT) {
        free_ast(res.value.ast);        free(input->buffer);
        free(input);
        return;
    }
    
    // Check function call
    ast_t* func_call = stmt->child;
    TEST_CHECK(func_call && func_call->typ == PASCAL_T_FUNC_CALL);
    if (!func_call || func_call->typ != PASCAL_T_FUNC_CALL) {
        free_ast(res.value.ast);        free(input->buffer);
        free(input);
        return;
    }
    
    // Check function name
    ast_t* func_name = func_call->child;
    TEST_CHECK(func_name && func_name->typ == PASCAL_T_IDENTIFIER);
    if (!func_name || func_name->typ != PASCAL_T_IDENTIFIER) {
        free_ast(res.value.ast);        free(input->buffer);
        free(input);
        return;
    }
    
    // Handle both regular identifiers and built-in function structure
    ast_t* actual_name_node = func_name;
    if (func_name->child && func_name->child->typ == PASCAL_T_IDENTIFIER) {
        // Use the child identifier for built-in functions
        actual_name_node = func_name->child;
        
        // For built-in functions, the symbol might be in the grandchild
        if (!actual_name_node->sym && actual_name_node->child && 
            actual_name_node->child->typ == PASCAL_T_IDENTIFIER) {
            actual_name_node = actual_name_node->child;
        }
    }
    
    TEST_CHECK(actual_name_node->sym && 
               actual_name_node->sym->name && 
               strcmp(actual_name_node->sym->name, "writeln") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free(input);
}

void test_pascal_if_statement(void) {
    combinator_t* p = new_combinator();
    init_pascal_program_parser(&p);  // Use program parser for terminated statements

    input_t* input = new_input();
    input->buffer = strdup("if x > 0 then y := 1;");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    // Get the actual statement
    ast_t* stmt = res.value.ast;
    if (stmt->typ == PASCAL_T_NONE) {
        stmt = stmt->child;
    }
    TEST_ASSERT(stmt->typ == PASCAL_T_IF_STMT);
    
    // Check condition (x > 0)
    ast_t* condition = stmt->child;
    TEST_ASSERT(condition->typ == PASCAL_T_GT);
    
    // Check then statement
    ast_t* then_stmt = condition->next;
    TEST_ASSERT(then_stmt->typ == PASCAL_T_ASSIGNMENT);

    free_ast(res.value.ast);    free(input->buffer);
    free(input);
}

void test_pascal_if_else_statement(void) {
    combinator_t* p = new_combinator();
    init_pascal_program_parser(&p);  // Use program parser for terminated statements

    input_t* input = new_input();
    input->buffer = strdup("if x > 0 then y := 1 else y := -1;");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    // Get the actual statement
    ast_t* stmt = res.value.ast;
    if (stmt->typ == PASCAL_T_NONE) {
        stmt = stmt->child;
    }
    TEST_ASSERT(stmt->typ == PASCAL_T_IF_STMT);
    
    // Check condition
    ast_t* condition = stmt->child;
    TEST_ASSERT(condition->typ == PASCAL_T_GT);
    
    // Check then statement
    ast_t* then_stmt = condition->next;
    TEST_ASSERT(then_stmt->typ == PASCAL_T_ASSIGNMENT);
    
    // Check else clause
    ast_t* else_clause = then_stmt->next;
    TEST_ASSERT(else_clause->typ == PASCAL_T_ELSE);
    
    // Check else statement
    ast_t* else_stmt = else_clause->child;
    TEST_ASSERT(else_stmt->typ == PASCAL_T_ASSIGNMENT);

    free_ast(res.value.ast);    free(input->buffer);
    free(input);
}

void test_pascal_begin_end_block(void) {
    combinator_t* p = new_combinator();
    init_pascal_statement_parser(&p);

    input_t* input = new_input();
    input->buffer = strdup("begin x := 1; y := 2 end");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_BEGIN_BLOCK);
    
    // The child should be the statement list
    ast_t* stmt_list = res.value.ast->child;
    TEST_ASSERT(stmt_list != NULL);

    free_ast(res.value.ast);    free(input->buffer);
    free(input);
}

void test_pascal_for_statement(void) {
    combinator_t* p = new_combinator();
    init_pascal_program_parser(&p);  // Use program parser for terminated statements

    input_t* input = new_input();
    input->buffer = strdup("for i := 1 to 10 do x := x + i;");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    // Get the actual statement
    ast_t* stmt = res.value.ast;
    if (stmt->typ == PASCAL_T_NONE) {
        stmt = stmt->child;
    }
    TEST_ASSERT(stmt->typ == PASCAL_T_FOR_STMT);

    // Check loop initializer
    ast_t* initializer = stmt->child;
    TEST_ASSERT(initializer->typ == PASCAL_T_ASSIGNMENT);

    ast_t* loop_var = initializer->child;
    TEST_ASSERT(loop_var->typ == PASCAL_T_IDENTIFIER);
    TEST_ASSERT(strcmp(loop_var->sym->name, "i") == 0);

    ast_t* start_value = loop_var->next;
    TEST_ASSERT(start_value->typ == PASCAL_T_INTEGER);
    TEST_ASSERT(strcmp(start_value->sym->name, "1") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free(input);
}

void test_pascal_for_statement_without_assignment(void) {
    combinator_t* p = new_combinator();
    init_pascal_program_parser(&p);

    input_t* input = new_input();
    input->buffer = strdup("for j to 3 do writeln(j);");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    ast_t* stmt = res.value.ast;
    if (stmt->typ == PASCAL_T_NONE) {
        stmt = stmt->child;
    }

    TEST_ASSERT(stmt->typ == PASCAL_T_FOR_STMT);

    ast_t* initializer = stmt->child;
    TEST_ASSERT(initializer->typ == PASCAL_T_IDENTIFIER);
    TEST_ASSERT(strcmp(initializer->sym->name, "j") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free(input);
}

void test_pascal_while_statement(void) {
    combinator_t* p = new_combinator();
    init_pascal_program_parser(&p);  // Use program parser for terminated statements

    input_t* input = new_input();
    input->buffer = strdup("while x > 0 do x := x - 1;");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    // Get the actual statement
    ast_t* stmt = res.value.ast;
    if (stmt->typ == PASCAL_T_NONE) {
        stmt = stmt->child;
    }
    TEST_ASSERT(stmt->typ == PASCAL_T_WHILE_STMT);
    
    // Check condition
    ast_t* condition = stmt->child;
    TEST_ASSERT(condition->typ == PASCAL_T_GT);

    free_ast(res.value.ast);    free(input->buffer);
    free(input);
}

void test_pascal_simple_asm_block(void) {
    combinator_t* p = new_combinator();
    init_pascal_program_parser(&p);  // Use program parser for terminated statements

    input_t* input = new_input();
    input->buffer = strdup("asm mov ax, 5 end;");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    // Get the actual statement
    ast_t* stmt = res.value.ast;
    if (stmt->typ == PASCAL_T_NONE) {
        stmt = stmt->child;
    }
    TEST_ASSERT(stmt->typ == PASCAL_T_ASM_BLOCK);
    
    // Check the ASM body content
    ast_t* asm_body = stmt->child;
    TEST_ASSERT(asm_body->typ == PASCAL_T_NONE);
    TEST_ASSERT(strcmp(asm_body->sym->name, "mov ax, 5 ") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free(input);
}

void test_pascal_multiline_asm_block(void) {
    combinator_t* p = new_combinator();
    init_pascal_program_parser(&p);  // Use program parser for terminated statements

    input_t* input = new_input();
    input->buffer = strdup("asm\n  mov ax, bx\n  add ax, 10\n  int 21h\nend;");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    // Get the actual statement
    ast_t* stmt = res.value.ast;
    if (stmt->typ == PASCAL_T_NONE) {
        stmt = stmt->child;
    }
    TEST_ASSERT(stmt->typ == PASCAL_T_ASM_BLOCK);
    
    // Check the ASM body content - it should contain newlines and instructions
    ast_t* asm_body = stmt->child;
    TEST_ASSERT(asm_body->typ == PASCAL_T_NONE);
    TEST_ASSERT(strstr(asm_body->sym->name, "mov ax, bx") != NULL);
    TEST_ASSERT(strstr(asm_body->sym->name, "add ax, 10") != NULL);
    TEST_ASSERT(strstr(asm_body->sym->name, "int 21h") != NULL);

    free_ast(res.value.ast);    free(input->buffer);
    free(input);
}

void test_pascal_empty_asm_block(void) {
    combinator_t* p = new_combinator();
    init_pascal_program_parser(&p);  // Use program parser for terminated statements

    input_t* input = new_input();
    input->buffer = strdup("asm end;");
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    // Get the actual statement
    ast_t* stmt = res.value.ast;
    if (stmt->typ == PASCAL_T_NONE) {
        stmt = stmt->child;
    }
    TEST_ASSERT(stmt->typ == PASCAL_T_ASM_BLOCK);
    
    // Check the ASM body content - should be empty except for space
    ast_t* asm_body = stmt->child;
    TEST_ASSERT(asm_body->typ == PASCAL_T_NONE);
    TEST_ASSERT(strcmp(asm_body->sym->name, "") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free(input);
}

void test_pascal_unterminated_asm_block(void) {
    combinator_t* p = new_combinator();
    init_pascal_program_parser(&p);  // Use program parser for terminated statements

    input_t* input = new_input();
    input->buffer = strdup("asm mov ax, 5");  // Missing 'end'
    input->length = strlen(input->buffer);

    ParseResult res = parse(input, p);

    TEST_ASSERT(!res.is_success);
    // Could be "Expected ';'" from program parser or "Unterminated ASM block" from ASM parser
    TEST_ASSERT(res.value.error->message != NULL);

    free_error(res.value.error);    free(input->buffer);
    free(input);
}
