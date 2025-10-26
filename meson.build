project('gpc-project', 'c')

# Build cparser library
cparser_core_sources = [
  'cparser/parser.c',
  'cparser/combinators.c',
]

pascal_parser_sources = [
  'cparser/examples/pascal_parser/pascal_parser.c',
  'cparser/examples/pascal_parser/pascal_expression.c',
  'cparser/examples/pascal_parser/pascal_statement.c',
  'cparser/examples/pascal_parser/pascal_declaration.c',
  'cparser/examples/pascal_parser/pascal_type.c',
  'cparser/examples/pascal_parser/pascal_keywords.c',
]

cparser_inc = include_directories('cparser', 'cparser/examples/pascal_parser')

cparser_lib = static_library('cparser',
  cparser_core_sources + pascal_parser_sources,
  include_directories: cparser_inc
)

cparser_dep = declare_dependency(
  link_with: cparser_lib,
  include_directories: cparser_inc
)

subdir('GPC')
