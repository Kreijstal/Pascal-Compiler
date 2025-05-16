CC = gcc
FLAGS = -g
OPTIMIZE =
LIBS =

PARSER_DIR = Parser
GRAMMAR_DIR = Parser/LexAndYacc
TREE_DIR = Parser/ParseTree
CODEGEN_DIR = CodeGenerator
OPTIMIZER_DIR = Optimizer

# Base CCFLAGS without debug flags
CCFLAGS_BASE = $(FLAGS) $(OPTIMIZE)
# Extra flags that can be passed from command line
EXTRA_CCFLAGS ?=
# Final CCFLAGS used in compilation
CCFLAGS = $(CCFLAGS_BASE) $(EXTRA_CCFLAGS)

# Object files to build
GPC_OBJS = main.o flags.o
PARSER_OBJS = $(PARSER_DIR)/ParsePascal.o
TREE_OBJS = $(PARSER_DIR)/List.o $(PARSER_DIR)/tree.o
SEM_OBJS = $(PARSER_DIR)/SemCheck.o $(PARSER_DIR)/HashTable.o $(PARSER_DIR)/SymTab.o
SEM_OBJS_MORE = $(PARSER_DIR)/SemCheck_stmt.o $(PARSER_DIR)/SemCheck_expr.o
GRAMMAR_OBJS = $(GRAMMAR_DIR)/lex.yy.o $(GRAMMAR_DIR)/Grammar.tab.o
CODEGEN_OBJS = $(CODEGEN_DIR)/Intel_x86-64/codegen.o $(CODEGEN_DIR)/Intel_x86-64/stackmng/stackmng.o $(CODEGEN_DIR)/Intel_x86-64/expr_tree/expr_tree.o
OPTIMIZER_OBJS = optimizer.o
ALL_OBJS = $(GPC_OBJS) $(GRAMMAR_OBJS) $(PARSER_OBJS) $(TREE_OBJS) $(SEM_OBJS) $(SEM_OBJS_MORE) $(CODEGEN_OBJS) $(OPTIMIZER_OBJS)

BIN = gpc

######## THE MAIN BUILD RULES ###########
release:
	cd Parser && $(MAKE) release
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

optimizer.o:
	$(CC) $(CCFLAGS) -c $(OPTIMIZER_DIR)/optimizer.c

parser:
	cd $(PARSER_DIR) && $(MAKE)

codegen:
	cd $(CODEGEN_DIR) && $(MAKE)

debugFlexBison:
	cd $(PARSER_DIR) && $(MAKE) debug_flexbison EXTRA_CCFLAGS="-DDEBUG_BISON"

flexDebug:
	cd $(PARSER_DIR) && $(MAKE) debug_flex EXTRA_CCFLAGS=""

bisonDebug:
	cd $(PARSER_DIR) && $(MAKE) debug_bison EXTRA_CCFLAGS="-DDEBUG_BISON"



############ CLEANING ##########3
clean: clean_gpc clean_LexYacc clean_CodeGen
clean_gpc:
	rm -f *.o $(BIN)
clean_LexYacc:
	cd $(PARSER_DIR) && $(MAKE) clean
clean_CodeGen:
	cd $(CODEGEN_DIR) && $(MAKE) clean
