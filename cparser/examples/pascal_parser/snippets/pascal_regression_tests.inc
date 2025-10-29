void test_pascal_enumerated_type_declaration(void) {
    combinator_t* p = get_program_parser();
    input_t* input = new_input();
    char* program = "program Test; type TMyEnum = (Value1, Value2, Value3); begin end.";
    input->buffer = strdup(program);
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }    free(input->buffer);
    free(input);
}

void test_pascal_simple_const_declaration(void) {
    combinator_t* p = get_program_parser();
    input_t* input = new_input();
    char* program = "program Test; const MyConst = 10; begin end.";
    input->buffer = strdup(program);
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }    free(input->buffer);
    free(input);
}

void test_pascal_var_section(void) {
    combinator_t* p = get_program_parser();
    input_t* input = new_input();
    char* program = "program Test; var i: integer; begin end.";
    input->buffer = strdup(program);
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }    free(input->buffer);
    free(input);
}

void test_pascal_set_operations_program(void) {
    combinator_t* p = new_combinator();
    init_pascal_expression_parser(&p);
    input_t* input = new_input();
    const char* union_expr = "[1, 3] + [5]";
    input->buffer = strdup(union_expr);
    input->length = strlen(union_expr);

    ParseResult res = parse_pascal_expression(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        TEST_ASSERT(res.value.ast->typ == PASCAL_T_SET_UNION);
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }    free(input->buffer);
    free(input);

    p = new_combinator();
    init_pascal_expression_parser(&p);
    input = new_input();
    const char* intersect_expr = "[1, 3] * [3]";
    input->buffer = strdup(intersect_expr);
    input->length = strlen(intersect_expr);

    res = parse_pascal_expression(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        TEST_ASSERT(res.value.ast->typ == PASCAL_T_MUL);
        ast_t* set_literal = find_first_node_of_type(res.value.ast, PASCAL_T_SET);
        TEST_ASSERT(set_literal != NULL);
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }    free(input->buffer);
    free(input);

    // Parse membership expression to ensure "in" is recognised with set unions
    p = new_combinator();
    init_pascal_expression_parser(&p);
    input = new_input();
    const char* in_expr = "3 in ([1, 3] + [5])";
    input->buffer = strdup(in_expr);
    input->length = strlen(in_expr);

    res = parse_pascal_expression(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        TEST_ASSERT(res.value.ast->typ == PASCAL_T_IN);
        ast_t* union_node = find_first_node_of_type(res.value.ast, PASCAL_T_SET_UNION);
        TEST_ASSERT(union_node != NULL);
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }    free(input->buffer);
    free(input);

    // Parse full program exercising set operations and subrange set types
    p = new_combinator();
    init_pascal_complete_program_parser(&p);
    input = new_input();
    const char* program_source =
        "program SetOperations;\n"
        "var\n"
        "  odds: set of 1..10;\n"
        "  evens: set of 1..10;\n"
        "  mix: set of 1..10;\n"
        "  result: set of 1..10;\n"
        "begin\n"
        "  odds := [1, 3, 5, 7, 9];\n"
        "  evens := [2, 4, 6, 8, 10];\n"
        "  mix := [3, 4, 5];\n"
        "\n"
        "  result := odds + mix;\n"
        "  if 4 in result then\n"
        "    writeln('union-has-4')\n"
        "  else\n"
        "    writeln('union-missing-4');\n"
        "\n"
        "  result := mix * evens;\n"
        "  if 4 in result then\n"
        "    writeln('intersection-has-4')\n"
        "  else\n"
        "    writeln('intersection-missing-4');\n"
        "\n"
        "  if 2 in [1, 2, 3] then\n"
        "    writeln('constructor-has-2')\n"
        "  else\n"
        "    writeln('constructor-missing-2');\n"
        "end.";
    input->buffer = strdup(program_source);
    input->length = strlen(program_source);

    res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }    free(input->buffer);
    free(input);
}

void test_pascal_pointer_operations_program(void) {
    combinator_t* p = new_combinator();
    init_pascal_expression_parser(&p);
    input_t* input = new_input();
    const char* addr_expr = "@value";
    input->buffer = strdup(addr_expr);
    input->length = strlen(addr_expr);

    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        TEST_ASSERT(res.value.ast->typ == PASCAL_T_ADDR);
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }    free(input->buffer);
    free(input);

    p = new_combinator();
    init_pascal_expression_parser(&p);
    input = new_input();
    const char* deref_expr = "ptr^";
    input->buffer = strdup(deref_expr);
    input->length = strlen(deref_expr);

    res = parse_pascal_expression(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        TEST_ASSERT(res.value.ast->typ == PASCAL_T_DEREF);
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }    free(input->buffer);
    free(input);
}

void test_pascal_record_member_access_program(void) {
    combinator_t* p = new_combinator();
    init_pascal_statement_parser(&p);
    input_t* input = new_input();
    const char* block =
        "begin\n"
        "  p.x := 1;\n"
        "  total := p.x + p.y;\n"
        "end";
    input->buffer = strdup(block);
    input->length = strlen(block);

    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        ast_t* member_node = find_first_node_of_type(res.value.ast, PASCAL_T_MEMBER_ACCESS);
        TEST_ASSERT(member_node != NULL);
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }    free(input->buffer);
    free(input);
}

void test_pascal_record_member_access_complete_program(void) {
    combinator_t* p = new_combinator();
    init_pascal_complete_program_parser(&p);

    input_t* input = new_input();
    const char* program =
        "program RecordMemberAccess;\n"
        "type\n"
        "  Point = record\n"
        "    x: Integer;\n"
        "    y: Integer;\n"
        "  end;\n"
        "var\n"
        "  p: Point;\n"
        "begin\n"
        "  p.x := 10;\n"
        "  p.y := 32;\n"
        "  writeln(p.x + p.y);\n"
        "  p.x := p.y - 2;\n"
        "  writeln(p.x);\n"
        "end.\n";
    input->buffer = strdup(program);
    input->length = strlen(program);

    ParseResult res = parse(input, p);
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        ast_t* member_node = find_first_node_of_type(res.value.ast, PASCAL_T_MEMBER_ACCESS);
        TEST_ASSERT(member_node != NULL);
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }

    free_combinator(p);
    free(input->buffer);
    free(input);
}

void test_fpc_style_unit_parsing(void) {
    combinator_t* p = get_unit_parser();
    input_t* input = new_input();
    char* program = 
        "Unit rax64int;\n"
        "interface\n"
        "uses aasmtai, rax86int;\n"
        "type\n"
        "  tx8664intreader = class(tx86intreader)\n"
        "    actsehdirective: TAsmSehDirective;\n"
        "    function is_targetdirective(const s:string):boolean;override;\n"
        "  end;\n"
        "implementation\n"
        "uses globtype, cutils;\n"
        "const\n"
        "  maxoffset: array[boolean] of aint=(high(dword), 240);\n"
        "function tx8664intreader.is_targetdirective(const s:string):boolean;\n"
        "begin\n"
        "  result:=false;\n"
        "end;\n"
        "end.";
    input->buffer = strdup(program);
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }    free(input->buffer);
    free(input);
}

// --- Helpers for advanced feature regression tests ---
static void assert_pascal_unit_parses(const char* source) {
    combinator_t* parser = get_unit_parser();

    input_t* input = new_input();
    input->buffer = strdup(source);
    input->length = strlen(source);

    ParseResult result = parse(input, parser);
    bool success = result.is_success;

    if (result.is_success && input->start < input->length) {
        TEST_MSG("Parser left trailing input: '%.*s'", 40, input->buffer + input->start);
        success = false;
    }

    if (!result.is_success && result.value.error && result.value.error->message) {
        TEST_MSG("Unexpected parse failure: %s", result.value.error->message);
    }

    if (result.is_success) {
        free_ast(result.value.ast);
    } else if (result.value.error) {
        free_error(result.value.error);
    }    free(input->buffer);
    free(input);

    TEST_ASSERT(success);
}

// --- Regression tests for features seen in the FPC corpus ---
void test_pascal_unit_with_dotted_name(void) {
    const char* source =
        "unit Generics.Collections;\n"
        "interface\n"
        "implementation\n"
        "end.\n";
    assert_pascal_unit_parses(source);
}

void test_pascal_uses_with_dotted_unit(void) {
    const char* source =
        "unit UsesDotted;\n"
        "interface\n"
        "uses Generics.Collections;\n"
        "implementation\n"
        "end.\n";
    assert_pascal_unit_parses(source);
}

void test_pascal_out_parameter_modifier(void) {
    const char* source =
        "unit OutModifierDemo;\n"
        "interface\n"
        "procedure Foo(out Value: Integer);\n"
        "implementation\n"
        "procedure Foo(out Value: Integer);\n"
        "begin\n"
        "end;\n"
        "end.\n";
    assert_pascal_unit_parses(source);
}

void test_pascal_resourcestring_section(void) {
    const char* source =
        "unit ResourceStringsDemo;\n"
        "interface\n"
        "resourcestring\n"
        "  SMessage = 'Hello';\n"
        "implementation\n"
        "end.\n";
    assert_pascal_unit_parses(source);
}

void test_pascal_threadvar_section(void) {
    const char* source =
        "unit ThreadVarDemo;\n"
        "interface\n"
        "threadvar\n"
        "  ThreadID: Cardinal;\n"
        "implementation\n"
        "end.\n";
    assert_pascal_unit_parses(source);
}

void test_pascal_generic_type_declaration(void) {
    const char* source =
        "unit GenericDemo;\n"
        "interface\n"
        "type\n"
        "  generic TBox<T> = class\n"
        "  end;\n"
        "implementation\n"
        "end.\n";
    assert_pascal_unit_parses(source);
}

void test_pascal_specialize_alias(void) {
    const char* source =
        "unit SpecializeDemo;\n"
        "interface\n"
        "type\n"
        "  TIntList = specialize TList<Integer>;\n"
        "implementation\n"
        "end.\n";
    assert_pascal_unit_parses(source);
}

void test_pascal_class_function_modifier(void) {
    const char* source =
        "unit ClassFunctionDemo;\n"
        "interface\n"
        "type\n"
        "  TFoo = class\n"
        "  public\n"
        "    class function CreateDefault: TFoo;\n"
        "  end;\n"
        "implementation\n"
        "class function TFoo.CreateDefault: TFoo;\n"
        "begin\n"
        "end;\n"
        "end.\n";
    assert_pascal_unit_parses(source);
}

void test_pascal_class_operator_overload(void) {
    const char* source =
        "unit ClassOperatorDemo;\n"
        "interface\n"
        "type\n"
        "  TFoo = class\n"
        "  public\n"
        "    class operator Equal(const A, B: TFoo): Boolean;\n"
        "  end;\n"
        "implementation\n"
        "class operator TFoo.Equal(const A, B: TFoo): Boolean;\n"
        "begin\n"
        "end;\n"
        "end.\n";
    assert_pascal_unit_parses(source);
}

void test_pascal_type_helper_for_string(void) {
    const char* source =
        "unit TypeHelperDemo;\n"
        "interface\n"
        "type\n"
        "  TStringHelper = type helper for string\n"
        "    function ToUpper: string;\n"
        "  end;\n"
        "implementation\n"
        "function TStringHelper.ToUpper: string;\n"
        "begin\n"
        "end;\n"
        "end.\n";
    assert_pascal_unit_parses(source);
}

void test_pascal_overload_directive(void) {
    const char* source =
        "unit OverloadDemo;\n"
        "interface\n"
        "procedure Foo; overload;\n"
        "implementation\n"
        "procedure Foo; overload;\n"
        "begin\n"
        "end;\n"
        "end.\n";
    assert_pascal_unit_parses(source);
}

void test_pascal_inline_directive(void) {
    const char* source =
        "unit InlineDemo;\n"
        "interface\n"
        "procedure Foo; inline;\n"
        "implementation\n"
        "procedure Foo; inline;\n"
        "begin\n"
        "end;\n"
        "end.\n";
    assert_pascal_unit_parses(source);
}

void test_complex_fpc_rax64int_unit(void) {
    combinator_t* p = get_unit_parser();
    input_t* input = new_input();
    char* program = 
        "Unit rax64int;\n"
        "\n"
        "  interface\n"
        "\n"
        "    uses\n"
        "      aasmtai,\n"
        "      rax86int;\n"
        "\n"
        "    type\n"
        "      tx8664intreader = class(tx86intreader)\n"
        "        actsehdirective: TAsmSehDirective;\n"
        "        function is_targetdirective(const s:string):boolean;override;\n"
        "        procedure HandleTargetDirective;override;\n"
        "      end;\n"
        "\n"
        "\n"
        "  implementation\n"
        "\n"
        "    uses\n"
        "      globtype,\n"
        "      cutils,\n"
        "      systems,\n"
        "      verbose,\n"
        "      cgbase,\n"
        "      symconst,\n"
        "      procinfo,\n"
        "      rabase;\n"
        "\n"
        "    const\n"
        "      { max offset and bitmask for .seh_savereg and .seh_setframe }\n"
        "      maxoffset: array[boolean] of aint=(high(dword), 240);\n"
        "      modulo: array[boolean] of integer=(7, 15);\n"
        "\n"
        "    function tx8664intreader.is_targetdirective(const s:string):boolean;\n"
        "      var\n"
        "        i: TAsmSehDirective;\n"
        "      begin\n"
        "        result:=false;\n"
        "        if target_info.system<>system_x86_64_win64 then exit;\n"
        "\n"
        "        for i:=low(TAsmSehDirective) to high(TAsmSehDirective) do\n"
        "          begin\n"
        "            if not (i in recognized_directives) then\n"
        "              continue;\n"
        "            if s=sehdirectivestr[i] then\n"
        "              begin\n"
        "                actsehdirective:=i;\n"
        "                result:=true;\n"
        "                break;\n"
        "              end;\n"
        "          end;\n"
        "      end;\n"
        "\n"
        "end.";
    input->buffer = strdup(program);
    input->length = strlen(program);
    ParseResult res = parse(input, p);
    
    TEST_ASSERT(res.is_success);
    if (res.is_success) {
        free_ast(res.value.ast);
    } else {
        free_error(res.value.error);
    }    free(input->buffer);
    free(input);
}
