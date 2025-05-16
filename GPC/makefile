CC = gcc
FLAGS = -g
OPTIMIZE =
LIBS =

PARSER_DIR = Parser
GRAMMAR_DIR = Parser/LexAndYacc
TREE_DIR = Parser/ParseTree
CODEGEN_DIR = CodeGenerator
OPTIMIZER_DIR = Optimizer

CCFLAGS = $(FLAGS) $(OPTIMIZE) -I$(PARSER_DIR) -I$(GRAMMAR_DIR)

# Object files to build
GPC_OBJS = main.o flags.o parser_main.o
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

flags.o: flags.c flags.h
	$(CC) $(CCFLAGS) -c -o $@ $<

lexer_test: lexer_test.o flags.o
	$(MAKE) -C $(GRAMMAR_DIR)
	$(MAKE) -C $(PARSER_DIR) parser
	$(CC) $(CCFLAGS) -o $@ $^ $(PARSER_DIR)/ParsePascal.o $(TREE_OBJS) $(PARSER_DIR)/SemCheck.o $(PARSER_DIR)/SymTab.o $(PARSER_DIR)/HashTable.o $(PARSER_DIR)/SemCheck_stmt.o $(PARSER_DIR)/SemCheck_expr.o $(OPTIMIZER_DIR)/optimizer.o $(GRAMMAR_DIR)/lex.yy.o $(GRAMMAR_DIR)/Grammar.tab.o $(LIBS)

lexer_test.o: lexer_test.c $(GRAMMAR_DIR)/Grammar.tab.h
	$(CC) $(CCFLAGS) -IParser -IParser/ParseTree -IParser/List -c -o $@ $<

standalone_lexer_test: standalone_lexer_test.o $(GRAMMAR_DIR)/lex.yy.o
	$(CC) $(CCFLAGS) -o $@ $^ $(LIBS)

standalone_lexer_test.o: standalone_lexer_test.c
	$(CC) $(CCFLAGS) -c -o $@ $<
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


optimizer.o:
	$(CC) $(CCFLAGS) -c $(OPTIMIZER_DIR)/optimizer.c

parser:
	$(MAKE) -C $(PARSER_DIR)
	$(MAKE) -C $(GRAMMAR_DIR)
	$(CC) $(CCFLAGS) -c parser_main.c
	$(CC) $(CCFLAGS) -c flags.c
	$(CC) $(CCFLAGS) -c $(OPTIMIZER_DIR)/optimizer.c
	$(CC) $(CCFLAGS) -o pascal_parser $(PARSER_OBJS) $(TREE_OBJS) $(SEM_OBJS) $(SEM_OBJS_MORE) $(GRAMMAR_OBJS) flags.o optimizer.o parser_main.o $(LIBS)

parser_main.o: parser_main.c
	$(CC) $(CCFLAGS) -c parser_main.c

$(PARSER_DIR)/ParsePascal.o: $(PARSER_DIR)/ParsePascal.c
	$(CC) $(CCFLAGS) -fPIC -c $(PARSER_DIR)/ParsePascal.c -o $(PARSER_DIR)/ParsePascal.o

parser_clean:
	rm -f pascal_parser parser_main.o

codegen:
	cd $(CODEGEN_DIR) && $(MAKE)

debugFlexBison:
	cd $(PARSER_DIR) && $(MAKE) debug_flexbison

flexDebug:
	cd $(PARSER_DIR) && $(MAKE) debug_flex

bisonDebug:
	cd $(PARSER_DIR) && $(MAKE) debug_bison



############ CLEANING ##########3
clean: clean_gpc clean_LexYacc clean_CodeGen
clean_gpc:
	rm -f *.o $(BIN)
clean_LexYacc:
	cd $(PARSER_DIR) && $(MAKE) clean
clean_CodeGen:
	cd $(CODEGEN_DIR) && $(MAKE) clean
