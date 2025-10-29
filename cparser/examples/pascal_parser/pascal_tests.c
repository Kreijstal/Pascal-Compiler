#include "acutest.h"
#include "parser.h"
#include "combinators.h"
#include "pascal_parser.h"
#include "pascal_preprocessor.h"
#include "pascal_keywords.h"
#include <stdio.h>
#include <string.h>

// Test definitions are split into logical snippets to keep this file manageable.
#include "snippets/pascal_shared.inc"
#include "snippets/pascal_expression_tests.inc"
#include "snippets/pascal_statement_tests.inc"
#include "snippets/pascal_declaration_tests.inc"
#include "snippets/pascal_missing_feature_tests.inc"
#include "snippets/pascal_program_structure_tests.inc"
#include "snippets/pascal_regression_tests.inc"

TEST_LIST = {
    { "test_pascal_integer_parsing", test_pascal_integer_parsing },
    { "test_pascal_invalid_input", test_pascal_invalid_input },
    { "test_pascal_preprocessor_conditionals", test_pascal_preprocessor_conditionals },
    { "test_pascal_function_call", test_pascal_function_call },
    { "test_pascal_string_literal", test_pascal_string_literal },
    { "test_pascal_function_call_no_args", test_pascal_function_call_no_args },
    { "test_pascal_function_call_with_args", test_pascal_function_call_with_args },
    { "test_pascal_mod_operator", test_pascal_mod_operator },
    { "test_pascal_mod_operator_percent", test_pascal_mod_operator_percent },
    { "test_pascal_string_concatenation", test_pascal_string_concatenation },
    { "test_pascal_complex_expression", test_pascal_complex_expression },
    { "test_pascal_div_operator", test_pascal_div_operator },
    { "test_pascal_real_number", test_pascal_real_number },
    { "test_pascal_char_literal", test_pascal_char_literal },
    { "test_pascal_unary_plus", test_pascal_unary_plus },
    { "test_pascal_relational_operators", test_pascal_relational_operators },
    { "test_pascal_boolean_operators", test_pascal_boolean_operators },
    { "test_pascal_bitwise_operators", test_pascal_bitwise_operators },
    { "test_pascal_address_operator", test_pascal_address_operator },
    { "test_pascal_comprehensive_expression", test_pascal_comprehensive_expression },
    { "test_pascal_precedence", test_pascal_precedence },
    { "test_pascal_type_casting", test_pascal_type_casting },
    { "test_pascal_set_constructor", test_pascal_set_constructor },
    { "test_pascal_empty_set", test_pascal_empty_set },
    { "test_pascal_char_set", test_pascal_char_set },
    { "test_pascal_range_expression", test_pascal_range_expression },
    { "test_pascal_char_range", test_pascal_char_range },
    { "test_pascal_set_union", test_pascal_set_union },
    { "test_pascal_is_operator", test_pascal_is_operator },
    { "test_pascal_as_operator", test_pascal_as_operator },
    { "test_pascal_as_operator_with_field_access", test_pascal_as_operator_with_field_access },
    // Statement tests
    { "test_pascal_assignment_statement", test_pascal_assignment_statement },
    { "test_pascal_expression_statement", test_pascal_expression_statement },
    { "test_pascal_if_statement", test_pascal_if_statement },
    { "test_pascal_if_else_statement", test_pascal_if_else_statement },
    { "test_pascal_begin_end_block", test_pascal_begin_end_block },
    { "test_pascal_for_statement", test_pascal_for_statement },
    { "test_pascal_for_statement_without_assignment", test_pascal_for_statement_without_assignment },
    { "test_pascal_while_statement", test_pascal_while_statement },
    { "test_pascal_simple_asm_block", test_pascal_simple_asm_block },
    { "test_pascal_multiline_asm_block", test_pascal_multiline_asm_block },
    { "test_pascal_empty_asm_block", test_pascal_empty_asm_block },
    { "test_pascal_unterminated_asm_block", test_pascal_unterminated_asm_block },
    // Procedure/Function Declaration tests
    { "test_pascal_simple_procedure", test_pascal_simple_procedure },
    { "test_pascal_procedure_with_params", test_pascal_procedure_with_params },
    { "test_pascal_simple_function", test_pascal_simple_function },
    { "test_pascal_function_no_params", test_pascal_function_no_params },
    { "test_pascal_function_multiple_params", test_pascal_function_multiple_params },
    // Failing tests for missing features
    { "test_pascal_record_type", test_pascal_record_type },
    { "test_pascal_unit_declaration", test_pascal_unit_declaration },
    { "test_pascal_pointer_type_declaration", test_pascal_pointer_type_declaration },
    { "test_pascal_method_implementation", test_pascal_method_implementation },
    { "test_pascal_with_statement", test_pascal_with_statement },
    { "test_pascal_exit_statement", test_pascal_exit_statement },
    { "test_pascal_include_directive", test_pascal_include_directive },
    { "test_pascal_forward_declared_function", test_pascal_forward_declared_function },
    // Case statement tests
    { "test_pascal_simple_case_statement", test_pascal_simple_case_statement },
    { "test_pascal_case_statement_with_ranges", test_pascal_case_statement_with_ranges },
    { "test_pascal_case_statement_multiple_labels", test_pascal_case_statement_multiple_labels },
    { "test_pascal_case_statement_with_else", test_pascal_case_statement_with_else },
    { "test_pascal_case_expression_labels", test_pascal_case_expression_labels },
    { "test_pascal_case_statement_char_labels", test_pascal_case_statement_char_labels },
    { "test_pascal_case_invalid_expression_labels", test_pascal_case_invalid_expression_labels },
    { "test_pascal_paren_star_comment", test_pascal_paren_star_comment },
    { "test_pascal_hex_literal", test_pascal_hex_literal },
    { "test_pascal_case_range_label", test_pascal_case_range_label },
    { "test_pascal_pointer_dereference", test_pascal_pointer_dereference },
    { "test_pascal_array_access_with_deref", test_pascal_array_access_with_deref },
    // New failing tests for missing features
    { "test_pascal_enumerated_type_declaration", test_pascal_enumerated_type_declaration },
    { "test_pascal_simple_const_declaration", test_pascal_simple_const_declaration },
    { "test_pascal_set_operations_program", test_pascal_set_operations_program },
    { "test_pascal_pointer_operations_program", test_pascal_pointer_operations_program },
    { "test_pascal_record_member_access_program", test_pascal_record_member_access_program },
    { "test_pascal_record_member_access_complete_program", test_pascal_record_member_access_complete_program },
    { "test_pascal_var_section", test_pascal_var_section },
    { "test_pascal_unit_with_dotted_name", test_pascal_unit_with_dotted_name },
    { "test_pascal_uses_with_dotted_unit", test_pascal_uses_with_dotted_unit },
    { "test_pascal_out_parameter_modifier", test_pascal_out_parameter_modifier },
    { "test_pascal_resourcestring_section", test_pascal_resourcestring_section },
    { "test_pascal_threadvar_section", test_pascal_threadvar_section },
    { "test_pascal_generic_type_declaration", test_pascal_generic_type_declaration },
    { "test_pascal_specialize_alias", test_pascal_specialize_alias },
    { "test_pascal_class_function_modifier", test_pascal_class_function_modifier },
    { "test_pascal_class_operator_overload", test_pascal_class_operator_overload },
    { "test_pascal_type_helper_for_string", test_pascal_type_helper_for_string },
    { "test_pascal_overload_directive", test_pascal_overload_directive },
    { "test_pascal_inline_directive", test_pascal_inline_directive },
    { "test_fpc_style_unit_parsing", test_fpc_style_unit_parsing },
    { "test_complex_fpc_rax64int_unit", test_complex_fpc_rax64int_unit },
    { NULL, NULL }
};
