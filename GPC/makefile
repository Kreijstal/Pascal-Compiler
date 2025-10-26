CC = gcc
FLAGS = -g
OPTIMIZE =
LIBS = -lunwind -lunwind-x86_64 -L../cparser -lcparser

PARSER_DIR = Parser
GRAMMAR_DIR = Parser/LexAndYacc
TREE_DIR = Parser/ParseTree
CODEGEN_DIR = CodeGenerator
OPTIMIZER_DIR = Optimizer
CPARSER_DIR = ../cparser

# Base CCFLAGS without debug flags
CCFLAGS_BASE = $(FLAGS) $(OPTIMIZE) -I$(GRAMMAR_DIR) -I$(TREE_DIR) -I$(PARSER_DIR)/List -I$(CODEGEN_DIR)/Intel_x86-64/stackmng -I$(CODEGEN_DIR)/Intel_x86-64/expr_tree -I$(CPARSER_DIR) -I$(CPARSER_DIR)/examples/pascal_parser
# Extra flags that can be passed from command line
EXTRA_CCFLAGS ?=
# Final CCFLAGS used in compilation
CCFLAGS = $(CCFLAGS_BASE) $(EXTRA_CCFLAGS)

# Object files to build
GPC_OBJS = main.o flags.o stacktrace.o cparser_adapter.o cparser_frontend.o
PARSER_OBJS = $(PARSER_DIR)/ParsePascal.o
TREE_OBJS = $(PARSER_DIR)/List.o $(PARSER_DIR)/tree.o
SEM_OBJS = $(PARSER_DIR)/SemCheck.o $(PARSER_DIR)/HashTable.o $(PARSER_DIR)/SymTab.o $(PARSER_DIR)/NameMangling.o
SEM_OBJS_MORE = $(PARSER_DIR)/SemCheck_stmt.o $(PARSER_DIR)/SemCheck_expr.o
GRAMMAR_OBJS = $(GRAMMAR_DIR)/lex.yy.o $(GRAMMAR_DIR)/Grammar.tab.o
CODEGEN_OBJS = $(CODEGEN_DIR)/Intel_x86-64/codegen.o $(CODEGEN_DIR)/Intel_x86-64/codegen_statement.o $(CODEGEN_DIR)/Intel_x86-64/codegen_expression.o $(CODEGEN_DIR)/Intel_x86-64/codegen_builtins.o $(CODEGEN_DIR)/Intel_x86-64/stackmng/stackmng.o $(CODEGEN_DIR)/Intel_x86-64/expr_tree/expr_tree.o
OPTIMIZER_OBJS = optimizer.o
ALL_OBJS = $(GPC_OBJS) $(GRAMMAR_OBJS) $(PARSER_OBJS) $(TREE_OBJS) $(SEM_OBJS) $(SEM_OBJS_MORE) $(CODEGEN_OBJS) $(OPTIMIZER_OBJS)

BIN = gpc

######## THE MAIN BUILD RULES ###########
release:
	cd $(CPARSER_DIR) && $(MAKE)
	cd Parser && $(MAKE) release debug_flexbison
	$(MAKE) codegen
	$(MAKE) bin

debug_flexbison: debugFlexBison bin
debug_flex: flexDebug bin
debug_bison: bisonDebug bin


############ MAKING OBJS #############
# Making all the objects and bins across directories
# Please no touch :)
bin: $(ALL_OBJS)
	$(CC) $(CCFLAGS) -o $(BIN) $(ALL_OBJS) $(LIBS)

main.o: $(PARSER_OBJS) main.c
	$(CC) $(CCFLAGS) -c main.c

flags.o:
	$(CC) $(CCFLAGS) -c flags.c

stacktrace.o:
	$(CC) $(CCFLAGS) -c stacktrace.c

cparser_adapter.o: cparser_adapter.c cparser_adapter.h
	$(CC) $(CCFLAGS) -c cparser_adapter.c

cparser_frontend.o: cparser_frontend.c cparser_frontend.h
	$(CC) $(CCFLAGS) -c cparser_frontend.c

optimizer.o:
	$(CC) $(CCFLAGS) -c $(OPTIMIZER_DIR)/optimizer.c

parser:
	cd $(PARSER_DIR) && $(MAKE)

codegen:
	cd $(CODEGEN_DIR) && $(MAKE)

# Explicit rules for codegen objects to prevent implicit make rules from being used
$(CODEGEN_OBJS):
	cd $(CODEGEN_DIR) && $(MAKE)

debugFlexBison:
	cd $(PARSER_DIR) && $(MAKE) debug_flexbison EXTRA_CCFLAGS="-DDEBUG_BISON"

flexDebug:
	cd $(PARSER_DIR) && $(MAKE) debug_flex EXTRA_CCFLAGS=""

bisonDebug:
	cd $(PARSER_DIR) && $(MAKE) debug_bison EXTRA_CCFLAGS="-DDEBUG_BISON"



############ CLEANING ##########3
clean: clean_gpc clean_LexYacc clean_CodeGen clean_cparser
clean_gpc:
	rm -f *.o $(BIN)
clean_LexYacc:
	cd $(PARSER_DIR) && $(MAKE) clean
clean_CodeGen:
	cd $(CODEGEN_DIR) && $(MAKE) clean
clean_cparser:
	cd $(CPARSER_DIR) && $(MAKE) clean
