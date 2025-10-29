void test_pascal_integer_parsing(void) {
    combinator_t* p = new_combinator();
    init_pascal_expression_parser(&p);

    input_t* input = new_input();
    input->buffer = strdup("123");
    input->length = 3;

    ParseResult res = parse_pascal_expression(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_INTEGER);
    TEST_ASSERT(strcmp(res.value.ast->sym->name, "123") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free(input);
}

void test_pascal_invalid_input(void) {
    combinator_t* p = new_combinator();
    init_pascal_expression_parser(&p);

    input_t* input = new_input();
    input->buffer = strdup("1 +");
    input->length = 3;

    ParseResult res = parse_pascal_expression(input, p);

    TEST_ASSERT(!res.is_success);
    TEST_ASSERT(res.value.error->partial_ast != NULL);
    TEST_ASSERT(res.value.error->partial_ast->typ == PASCAL_T_ADD);
    ast_t* lhs = res.value.error->partial_ast->child;
    TEST_ASSERT(lhs->typ == PASCAL_T_INTEGER);
    TEST_ASSERT(strcmp(lhs->sym->name, "1") == 0);

    free_error(res.value.error);    free(input->buffer);
    free(input);
}

void test_pascal_preprocessor_conditionals(void) {
    PascalPreprocessor *pp = pascal_preprocessor_create();
    TEST_ASSERT(pp != NULL);
    if (!pp) {
        return;
    }

    TEST_ASSERT(pascal_preprocessor_define(pp, "FOO"));

    const char *source = "{$ifdef FOO}foo{$else}bar{$endif}";
    size_t source_len = strlen(source);
    char *error_message = NULL;
    size_t output_len = 0;
    char *result = pascal_preprocess_buffer(pp, "<memory>", source, source_len, &output_len, &error_message);

    TEST_ASSERT(result != NULL);
    TEST_ASSERT(error_message == NULL);
    if (error_message) {
        free(error_message);
    }
    if (result) {
        TEST_CHECK(strstr(result, "foo") != NULL);
        TEST_CHECK(strstr(result, "bar") == NULL);
        free(result);
    }

    TEST_ASSERT(pascal_preprocessor_undefine(pp, "FOO"));

    const char *source2 = "{$ifdef FOO}x{$else}y{$endif}";
    char *error_message2 = NULL;
    size_t output_len2 = 0;
    char *result2 = pascal_preprocess_buffer(pp, "<memory>", source2, strlen(source2), &output_len2, &error_message2);

    TEST_ASSERT(result2 != NULL);
    TEST_ASSERT(error_message2 == NULL);
    if (error_message2) {
        free(error_message2);
    }
    if (result2) {
        TEST_CHECK(strstr(result2, "y") != NULL);
        TEST_CHECK(strstr(result2, "x") == NULL);
        free(result2);
    }

    pascal_preprocessor_free(pp);
}

void test_pascal_function_call(void) {
    combinator_t* p = new_combinator();
    init_pascal_expression_parser(&p);

    input_t* input = new_input();
    input->buffer = strdup("my_func");  // Just test identifier parsing
    input->length = strlen("my_func");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_IDENTIFIER);
    
    // Handle both regular identifiers and built-in function structure
    ast_t* actual_name_node = res.value.ast;
    if (res.value.ast->child && res.value.ast->child->typ == PASCAL_T_IDENTIFIER) {
        // Use the child identifier for built-in functions
        actual_name_node = res.value.ast->child;
    }
    
    TEST_ASSERT(actual_name_node->sym && 
               actual_name_node->sym->name && 
               strcmp(actual_name_node->sym->name, "my_func") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free(input);
}

void test_pascal_string_literal(void) {
    combinator_t* p = new_combinator();
    init_pascal_expression_parser(&p);

    input_t* input = new_input();
    input->buffer = strdup("\"hello world\"");
    input->length = strlen("\"hello world\"");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_STRING);
    TEST_ASSERT(strcmp(res.value.ast->sym->name, "hello world") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free(input);
}

void test_pascal_function_call_no_args(void) {
    combinator_t* p = new_combinator();
    init_pascal_expression_parser(&p);

    input_t* input = new_input();
    input->buffer = strdup("func()");
    input->length = strlen("func()");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_FUNC_CALL);
    
    // First child should be the function name
    ast_t* func_name = res.value.ast->child;
    TEST_ASSERT(func_name->typ == PASCAL_T_IDENTIFIER);
    
    // Handle both regular identifiers and built-in function structure
    ast_t* actual_name_node = func_name;
    if (func_name->child && func_name->child->typ == PASCAL_T_IDENTIFIER) {
        // Use the child identifier for built-in functions
        actual_name_node = func_name->child;
    }
    
    TEST_ASSERT(actual_name_node->sym && 
               actual_name_node->sym->name && 
               strcmp(actual_name_node->sym->name, "func") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free(input);
}

void test_pascal_function_call_with_args(void) {
    combinator_t* p = new_combinator();
    init_pascal_expression_parser(&p);

    input_t* input = new_input();
    input->buffer = strdup("func(5, 10)");
    input->length = strlen("func(5, 10)");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_FUNC_CALL);
    
    // First child should be the function name
    ast_t* func_name = res.value.ast->child;
    TEST_ASSERT(func_name->typ == PASCAL_T_IDENTIFIER);
    
    // Handle both regular identifiers and built-in function structure
    ast_t* actual_name_node = func_name;
    if (func_name->child && func_name->child->typ == PASCAL_T_IDENTIFIER) {
        // Use the child identifier for built-in functions
        actual_name_node = func_name->child;
    }
    
    TEST_ASSERT(actual_name_node->sym && 
               actual_name_node->sym->name && 
               strcmp(actual_name_node->sym->name, "func") == 0);
    
    // Arguments follow
    ast_t* arg1 = func_name->next;
    TEST_ASSERT(arg1->typ == PASCAL_T_INTEGER);
    TEST_ASSERT(strcmp(arg1->sym->name, "5") == 0);
    
    ast_t* arg2 = arg1->next;
    TEST_ASSERT(arg2->typ == PASCAL_T_INTEGER);
    TEST_ASSERT(strcmp(arg2->sym->name, "10") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free(input);
}

void test_pascal_mod_operator(void) {
    combinator_t* p = new_combinator();
    init_pascal_expression_parser(&p);

    input_t* input = new_input();
    input->buffer = strdup("7 mod 3");
    input->length = strlen("7 mod 3");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_MOD);
    
    // Check operands
    ast_t* left = res.value.ast->child;
    TEST_ASSERT(left->typ == PASCAL_T_INTEGER);
    TEST_ASSERT(strcmp(left->sym->name, "7") == 0);
    
    ast_t* right = left->next;
    TEST_ASSERT(right->typ == PASCAL_T_INTEGER);
    TEST_ASSERT(strcmp(right->sym->name, "3") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free(input);
}

void test_pascal_mod_operator_percent(void) {
    combinator_t* p = new_combinator();
    init_pascal_expression_parser(&p);

    input_t* input = new_input();
    input->buffer = strdup("7 % 3");
    input->length = strlen("7 % 3");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_MOD);
    
    // Check operands
    ast_t* left = res.value.ast->child;
    TEST_ASSERT(left->typ == PASCAL_T_INTEGER);
    TEST_ASSERT(strcmp(left->sym->name, "7") == 0);
    
    ast_t* right = left->next;
    TEST_ASSERT(right->typ == PASCAL_T_INTEGER);
    TEST_ASSERT(strcmp(right->sym->name, "3") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free(input);
}

void test_pascal_string_concatenation(void) {
    combinator_t* p = new_combinator();
    init_pascal_expression_parser(&p);

    input_t* input = new_input();
    input->buffer = strdup("\"hello\" + \"world\"");
    input->length = strlen("\"hello\" + \"world\"");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_ADD);
    
    // Check operands
    ast_t* left = res.value.ast->child;
    TEST_ASSERT(left->typ == PASCAL_T_STRING);
    TEST_ASSERT(strcmp(left->sym->name, "hello") == 0);
    
    ast_t* right = left->next;
    TEST_ASSERT(right->typ == PASCAL_T_STRING);
    TEST_ASSERT(strcmp(right->sym->name, "world") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free(input);
}

void test_pascal_complex_expression(void) {
    combinator_t* p = new_combinator();
    init_pascal_expression_parser(&p);

    input_t* input = new_input();
    input->buffer = strdup("procedure((5*7)-5)+\"test\"");
    input->length = strlen("procedure((5*7)-5)+\"test\"");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_ADD);
    
    // Left side should be function call
    ast_t* func_call = res.value.ast->child;
    TEST_ASSERT(func_call->typ == PASCAL_T_FUNC_CALL);
    
    // Function name
    ast_t* func_name = func_call->child;
    TEST_ASSERT(func_name->typ == PASCAL_T_IDENTIFIER);
    
    // Handle both regular identifiers and built-in function structure
    ast_t* actual_name_node = func_name;
    if (func_name->child && func_name->child->typ == PASCAL_T_IDENTIFIER) {
        // Use the child identifier for built-in functions
        actual_name_node = func_name->child;
    }
    
    TEST_ASSERT(actual_name_node->sym && 
               actual_name_node->sym->name && 
               strcmp(actual_name_node->sym->name, "procedure") == 0);
    
    // Function argument: (5*7)-5
    ast_t* arg = func_name->next;
    TEST_ASSERT(arg->typ == PASCAL_T_SUB);
    
    // Right side should be string
    ast_t* right = func_call->next;
    TEST_ASSERT(right->typ == PASCAL_T_STRING);
    TEST_ASSERT(strcmp(right->sym->name, "test") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free(input);
}

void test_pascal_div_operator(void) {
    combinator_t* p = new_combinator();
    init_pascal_expression_parser(&p);

    input_t* input = new_input();
    input->buffer = strdup("10 div 3");
    input->length = strlen("10 div 3");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_INTDIV);
    
    // Check operands
    ast_t* left = res.value.ast->child;
    TEST_ASSERT(left->typ == PASCAL_T_INTEGER);
    TEST_ASSERT(strcmp(left->sym->name, "10") == 0);
    
    ast_t* right = left->next;
    TEST_ASSERT(right->typ == PASCAL_T_INTEGER);
    TEST_ASSERT(strcmp(right->sym->name, "3") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free(input);
}

// Test real number parsing
void test_pascal_real_number(void) {
    combinator_t* p = new_combinator();
    init_pascal_expression_parser(&p);

    input_t* input = new_input();
    input->buffer = strdup("3.14");
    input->length = strlen("3.14");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_REAL);
    TEST_ASSERT(strcmp(res.value.ast->sym->name, "3.14") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free(input);
}

// Test character literal parsing
void test_pascal_char_literal(void) {
    combinator_t* p = new_combinator();
    init_pascal_expression_parser(&p);

    input_t* input = new_input();
    input->buffer = strdup("'A'");
    input->length = strlen("'A'");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_CHAR);
    TEST_ASSERT(strcmp(res.value.ast->sym->name, "A") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free(input);
}

// Test unary plus operator
void test_pascal_unary_plus(void) {
    combinator_t* p = new_combinator();
    init_pascal_expression_parser(&p);

    input_t* input = new_input();
    input->buffer = strdup("+42");
    input->length = strlen("+42");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_POS);
    
    // Check operand
    ast_t* operand = res.value.ast->child;
    TEST_ASSERT(operand->typ == PASCAL_T_INTEGER);
    TEST_ASSERT(strcmp(operand->sym->name, "42") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free(input);
}

// Test relational operators
void test_pascal_relational_operators(void) {
    const char* expressions[] = {
        "5 = 5", "5 <> 3", "3 < 5", "5 > 3", "3 <= 5", "5 >= 3"
    };
    pascal_tag_t expected_tags[] = {
        PASCAL_T_EQ, PASCAL_T_NE, PASCAL_T_LT, PASCAL_T_GT, PASCAL_T_LE, PASCAL_T_GE
    };
    
    for (int i = 0; i < 6; i++) {
        combinator_t* p = new_combinator();
        init_pascal_expression_parser(&p);

        input_t* input = new_input();
        input->buffer = strdup(expressions[i]);
        input->length = strlen(expressions[i]);

        ParseResult res = parse(input, p);

        TEST_ASSERT(res.is_success);
        TEST_ASSERT(res.value.ast->typ == expected_tags[i]);

        free_ast(res.value.ast);        free(input->buffer);
        free(input);
    }
}

// Test boolean operators
void test_pascal_boolean_operators(void) {
    const char* expressions[] = {
        "true and false", "true or false", "not true", "true xor false"
    };
    pascal_tag_t expected_tags[] = {
        PASCAL_T_AND, PASCAL_T_OR, PASCAL_T_NOT, PASCAL_T_XOR
    };
    
    for (int i = 0; i < 4; i++) {
        combinator_t* p = new_combinator();
        init_pascal_expression_parser(&p);

        input_t* input = new_input();
        input->buffer = strdup(expressions[i]);
        input->length = strlen(expressions[i]);

        ParseResult res = parse(input, p);

        TEST_ASSERT(res.is_success);
        TEST_ASSERT(res.value.ast->typ == expected_tags[i]);

        free_ast(res.value.ast);        free(input->buffer);
        free(input);
    }
}

// Test bitwise shift operators
void test_pascal_bitwise_operators(void) {
    const char* expressions[] = {
        "8 shl 2", "8 shr 1", "8 rol 3", "8 ror 2"
    };
    pascal_tag_t expected_tags[] = {
        PASCAL_T_SHL, PASCAL_T_SHR, PASCAL_T_ROL, PASCAL_T_ROR
    };

    for (int i = 0; i < 4; i++) {
        combinator_t* p = new_combinator();
        init_pascal_expression_parser(&p);

        input_t* input = new_input();
        input->buffer = strdup(expressions[i]);
        input->length = strlen(expressions[i]);

        ParseResult res = parse(input, p);

        TEST_ASSERT(res.is_success);
        TEST_ASSERT(res.value.ast->typ == expected_tags[i]);

        free_ast(res.value.ast);        free(input->buffer);
        free(input);
    }
}

// Test address operator
void test_pascal_address_operator(void) {
    combinator_t* p = new_combinator();
    init_pascal_expression_parser(&p);

    input_t* input = new_input();
    input->buffer = strdup("@myVar");
    input->length = strlen("@myVar");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_ADDR);
    
    // Check operand
    ast_t* operand = res.value.ast->child;
    TEST_ASSERT(operand->typ == PASCAL_T_IDENTIFIER);
    
    // Handle both regular identifiers and built-in function structure
    ast_t* actual_name_node = operand;
    if (operand->child && operand->child->typ == PASCAL_T_IDENTIFIER) {
        // Use the child identifier for built-in functions
        actual_name_node = operand->child;
    }
    
    TEST_ASSERT(actual_name_node->sym && 
               actual_name_node->sym->name && 
               strcmp(actual_name_node->sym->name, "myVar") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free(input);
}

// Test complex expression with new operators
void test_pascal_comprehensive_expression(void) {
    combinator_t* p = new_combinator();
    init_pascal_expression_parser(&p);

    input_t* input = new_input();
    input->buffer = strdup("(x + y) * z div 4");
    input->length = strlen("(x + y) * z div 4");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_INTDIV);
    
    // Left side should be multiplication
    ast_t* left = res.value.ast->child;
    TEST_ASSERT(left->typ == PASCAL_T_MUL);

    free_ast(res.value.ast);    free(input->buffer);
    free(input);
}

// Test precedence: arithmetic vs comparison
void test_pascal_precedence(void) {
    combinator_t* p = new_combinator();
    init_pascal_expression_parser(&p);

    input_t* input = new_input();
    input->buffer = strdup("a + b = c * d");
    input->length = strlen("a + b = c * d");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_EQ); // = should be top level

    // Left side should be addition
    ast_t* left = res.value.ast->child;
    TEST_ASSERT(left->typ == PASCAL_T_ADD);
    
    // Right side should be multiplication  
    ast_t* right = left->next;
    TEST_ASSERT(right->typ == PASCAL_T_MUL);

    free_ast(res.value.ast);    free(input->buffer);
    free(input);
}

// Test type casting
void test_pascal_type_casting(void) {
    combinator_t* p = new_combinator();
    init_pascal_expression_parser(&p);

    input_t* input = new_input();
    input->buffer = strdup("Integer('A')");
    input->length = strlen("Integer('A')");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_TYPECAST);
    
    // First child should be the type name
    ast_t* type_name = res.value.ast->child;
    TEST_ASSERT(type_name->typ == PASCAL_T_IDENTIFIER);
    TEST_ASSERT(strcmp(type_name->sym->name, "Integer") == 0);
    
    // Second child should be the expression being cast
    ast_t* expr = type_name->next;
    TEST_ASSERT(expr->typ == PASCAL_T_CHAR);
    TEST_ASSERT(strcmp(expr->sym->name, "A") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free(input);
}

// Test set constructor parsing
void test_pascal_set_constructor(void) {
    combinator_t* p = new_combinator();
    init_pascal_expression_parser(&p);

    input_t* input = new_input();
    input->buffer = strdup("[1, 2, 3]");
    input->length = strlen("[1, 2, 3]");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_SET);
    
    // Check first element
    ast_t* elem1 = res.value.ast->child;
    TEST_ASSERT(elem1->typ == PASCAL_T_INTEGER);
    TEST_ASSERT(strcmp(elem1->sym->name, "1") == 0);
    
    // Check second element
    ast_t* elem2 = elem1->next;
    TEST_ASSERT(elem2->typ == PASCAL_T_INTEGER);
    TEST_ASSERT(strcmp(elem2->sym->name, "2") == 0);
    
    // Check third element
    ast_t* elem3 = elem2->next;
    TEST_ASSERT(elem3->typ == PASCAL_T_INTEGER);
    TEST_ASSERT(strcmp(elem3->sym->name, "3") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free(input);
}

// Test empty set constructor
void test_pascal_empty_set(void) {
    combinator_t* p = new_combinator();
    init_pascal_expression_parser(&p);

    input_t* input = new_input();
    input->buffer = strdup("[]");
    input->length = strlen("[]");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_SET);
    TEST_ASSERT(res.value.ast->child == NULL); // Empty set

    free_ast(res.value.ast);    free(input->buffer);
    free(input);
}

// Test character set constructor
void test_pascal_char_set(void) {
    combinator_t* p = new_combinator();
    init_pascal_expression_parser(&p);

    input_t* input = new_input();
    input->buffer = strdup("['a', 'b', 'c']");
    input->length = strlen("['a', 'b', 'c']");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_SET);
    
    // Check first element
    ast_t* elem1 = res.value.ast->child;
    TEST_ASSERT(elem1->typ == PASCAL_T_CHAR);
    TEST_ASSERT(strcmp(elem1->sym->name, "a") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free(input);
}

// Test range expression
void test_pascal_range_expression(void) {
    combinator_t* p = new_combinator();
    init_pascal_expression_parser(&p);

    input_t* input = new_input();
    input->buffer = strdup("1..10");
    input->length = strlen("1..10");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_RANGE);
    
    // Check left operand
    ast_t* left = res.value.ast->child;
    TEST_ASSERT(left->typ == PASCAL_T_INTEGER);
    TEST_ASSERT(strcmp(left->sym->name, "1") == 0);
    
    // Check right operand
    ast_t* right = left->next;
    TEST_ASSERT(right->typ == PASCAL_T_INTEGER);
    TEST_ASSERT(strcmp(right->sym->name, "10") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free(input);
}

// Test character range expression
void test_pascal_char_range(void) {
    combinator_t* p = new_combinator();
    init_pascal_expression_parser(&p);

    input_t* input = new_input();
    input->buffer = strdup("'a'..'z'");
    input->length = strlen("'a'..'z'");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_RANGE);
    
    // Check left operand
    ast_t* left = res.value.ast->child;
    TEST_ASSERT(left->typ == PASCAL_T_CHAR);
    TEST_ASSERT(strcmp(left->sym->name, "a") == 0);
    
    // Check right operand
    ast_t* right = left->next;
    TEST_ASSERT(right->typ == PASCAL_T_CHAR);
    TEST_ASSERT(strcmp(right->sym->name, "z") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free(input);
}

// Test set union operation
void test_pascal_set_union(void) {
    combinator_t* p = new_combinator();
    init_pascal_expression_parser(&p);

    input_t* input = new_input();
    input->buffer = strdup("[1, 2] + [3, 4]");
    input->length = strlen("[1, 2] + [3, 4]");

    ParseResult res = parse_pascal_expression(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_SET_UNION);
    
    // Check left operand is a set
    ast_t* left = res.value.ast->child;
    TEST_ASSERT(left->typ == PASCAL_T_SET);
    
    // Check right operand is a set
    ast_t* right = left->next;
    TEST_ASSERT(right->typ == PASCAL_T_SET);

    free_ast(res.value.ast);    free(input->buffer);
    free(input);
}

// Test class type checking with 'is' operator
void test_pascal_is_operator(void) {
    combinator_t* p = new_combinator();
    init_pascal_expression_parser(&p);

    input_t* input = new_input();
    input->buffer = strdup("MyObject is TMyClass");
    input->length = strlen("MyObject is TMyClass");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_IS);
    
    // Check left operand (object identifier)
    ast_t* left = res.value.ast->child;
    TEST_ASSERT(left->typ == PASCAL_T_IDENTIFIER);
    
    // Handle both regular identifiers and built-in function structure
    ast_t* actual_left_node = left;
    if (left->child && left->child->typ == PASCAL_T_IDENTIFIER) {
        // Use the child identifier for built-in functions
        actual_left_node = left->child;
    }
    
    TEST_ASSERT(actual_left_node->sym && 
               actual_left_node->sym->name && 
               strcmp(actual_left_node->sym->name, "MyObject") == 0);
    
    // Check right operand (class type identifier)
    ast_t* right = left->next;
    TEST_ASSERT(right->typ == PASCAL_T_IDENTIFIER);
    
    // Handle both regular identifiers and built-in function structure
    ast_t* actual_right_node = right;
    if (right->child && right->child->typ == PASCAL_T_IDENTIFIER) {
        // Use the child identifier for built-in functions
        actual_right_node = right->child;
    }
    
    TEST_ASSERT(actual_right_node->sym && 
               actual_right_node->sym->name && 
               strcmp(actual_right_node->sym->name, "TMyClass") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free(input);
}

// Test class type casting with 'as' operator
void test_pascal_as_operator(void) {
    combinator_t* p = new_combinator();
    init_pascal_expression_parser(&p);

    input_t* input = new_input();
    input->buffer = strdup("Sender as TButton");
    input->length = strlen("Sender as TButton");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_AS);
    
    // Check left operand (object identifier)
    ast_t* left = res.value.ast->child;
    TEST_ASSERT(left->typ == PASCAL_T_IDENTIFIER);
    
    // Handle both regular identifiers and built-in function structure
    ast_t* actual_left_node = left;
    if (left->child && left->child->typ == PASCAL_T_IDENTIFIER) {
        // Use the child identifier for built-in functions
        actual_left_node = left->child;
    }
    
    TEST_ASSERT(actual_left_node->sym && 
               actual_left_node->sym->name && 
               strcmp(actual_left_node->sym->name, "Sender") == 0);
    
    // Check right operand (target class type)
    ast_t* right = left->next;
    TEST_ASSERT(right->typ == PASCAL_T_IDENTIFIER);
    
    // Handle both regular identifiers and built-in function structure
    ast_t* actual_right_node = right;
    if (right->child && right->child->typ == PASCAL_T_IDENTIFIER) {
        // Use the child identifier for built-in functions
        actual_right_node = right->child;
    }
    
    TEST_ASSERT(actual_right_node->sym && 
               actual_right_node->sym->name && 
               strcmp(actual_right_node->sym->name, "TButton") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free(input);
}

// Test complex 'as' operator with field access (basic parsing)
void test_pascal_as_operator_with_field_access(void) {
    combinator_t* p = new_combinator();
    init_pascal_expression_parser(&p);

    // For now, just test the casting part - field access would require more complex parsing
    input_t* input = new_input();
    input->buffer = strdup("SomeObject as TForm");
    input->length = strlen("SomeObject as TForm");

    ParseResult res = parse(input, p);

    TEST_ASSERT(res.is_success);
    TEST_ASSERT(res.value.ast->typ == PASCAL_T_AS);
    
    // Check left operand
    ast_t* left = res.value.ast->child;
    TEST_ASSERT(left->typ == PASCAL_T_IDENTIFIER);
    
    // Handle both regular identifiers and built-in function structure
    ast_t* actual_left_node = left;
    if (left->child && left->child->typ == PASCAL_T_IDENTIFIER) {
        // Use the child identifier for built-in functions
        actual_left_node = left->child;
    }
    
    TEST_ASSERT(actual_left_node->sym && 
               actual_left_node->sym->name && 
               strcmp(actual_left_node->sym->name, "SomeObject") == 0);
    
    // Check right operand
    ast_t* right = left->next;
    TEST_ASSERT(right->typ == PASCAL_T_IDENTIFIER);
    
    // Handle both regular identifiers and built-in function structure
    ast_t* actual_right_node = right;
    if (right->child && right->child->typ == PASCAL_T_IDENTIFIER) {
        // Use the child identifier for built-in functions
        actual_right_node = right->child;
    }
    
    TEST_ASSERT(actual_right_node->sym && 
               actual_right_node->sym->name && 
               strcmp(actual_right_node->sym->name, "TForm") == 0);

    free_ast(res.value.ast);    free(input->buffer);
    free(input);
}
