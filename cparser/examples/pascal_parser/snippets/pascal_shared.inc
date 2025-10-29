// Shared parser instances to avoid expensive re-initialization
// These are initialized lazily on first use
static combinator_t* shared_expression_parser = NULL;
static combinator_t* shared_statement_parser = NULL;
static combinator_t* shared_unit_parser = NULL;
static combinator_t* shared_program_parser = NULL;

static combinator_t* get_expression_parser(void) {
    if (shared_expression_parser == NULL) {
        shared_expression_parser = new_combinator();
        init_pascal_expression_parser(&shared_expression_parser);
    }
    return shared_expression_parser;
}

static combinator_t* get_statement_parser(void) {
    if (shared_statement_parser == NULL) {
        shared_statement_parser = new_combinator();
        init_pascal_statement_parser(&shared_statement_parser);
    }
    return shared_statement_parser;
}

static combinator_t* get_unit_parser(void) {
    if (shared_unit_parser == NULL) {
        shared_unit_parser = new_combinator();
        init_pascal_unit_parser(&shared_unit_parser);
    }
    return shared_unit_parser;
}

static combinator_t* get_program_parser(void) {
    if (shared_program_parser == NULL) {
        shared_program_parser = new_combinator();
        init_pascal_complete_program_parser(&shared_program_parser);
    }
    return shared_program_parser;
}

static ast_t* find_first_node_of_type(ast_t* node, tag_t target) {
    if (node == NULL || node == ast_nil) {
        return NULL;
    }

    for (ast_t* current = node; current != NULL && current != ast_nil; current = current->next) {
        if (current->typ == target) {
            return current;
        }
        ast_t* child_result = find_first_node_of_type(current->child, target);
        if (child_result != NULL) {
            return child_result;
        }
    }

    return NULL;
}
