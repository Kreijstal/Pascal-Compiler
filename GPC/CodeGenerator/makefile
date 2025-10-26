CC = gcc
FLAGS = -g
OPTIMIZE =
LIBS =

PARSER_DIR = ../Parser
GRAMMAR_DIR = ../Parser/LexAndYacc
TREE_DIR = ../Parser/ParseTree
LIST_DIR = ../Parser/List

CCFLAGS = $(FLAGS) $(OPTIMIZE)

# Object files to build
TREE_OBJS = $(PARSER_DIR)/List.o $(PARSER_DIR)/tree.o
GRAMMAR_OBJS = $(GRAMMAR_DIR)/lex.yy.o $(GRAMMAR_DIR)/y.tab.o
ALL_OBJS = $(GPC_OBJS) $(GRAMMAR_OBJS) $(PARSER_OBJS) $(TREE_OBJS) $(SEM_OBJS) $(SEM_OBJS_MORE)

######## THE MAIN BUILD RULES ###########
Intel_x86-64: Intel_x86-64/codegen.o Intel_x86-64/codegen_statement.o Intel_x86-64/codegen_expression.o Intel_x86-64/codegen_builtins.o Intel_x86-64/stackmng/stackmng.o Intel_x86-64/expr_tree/expr_tree.o

############ MAKING OBJS #############
# Making all the objects and bins across directories
# Please no touch :)
Intel_x86-64/codegen.o:
	$(CC) $(CCFLAGS) -I$(GRAMMAR_DIR) -I$(TREE_DIR) -I$(LIST_DIR) -IIntel_x86-64/stackmng -IIntel_x86-64/expr_tree -c Intel_x86-64/codegen.c -o Intel_x86-64/codegen.o

Intel_x86-64/codegen_statement.o:
	$(CC) $(CCFLAGS) -I$(GRAMMAR_DIR) -I$(TREE_DIR) -I$(LIST_DIR) -IIntel_x86-64/stackmng -IIntel_x86-64/expr_tree -c Intel_x86-64/codegen_statement.c -o Intel_x86-64/codegen_statement.o

Intel_x86-64/codegen_builtins.o:
	$(CC) $(CCFLAGS) -I$(GRAMMAR_DIR) -I$(TREE_DIR) -I$(LIST_DIR) -IIntel_x86-64/stackmng -IIntel_x86-64/expr_tree -c Intel_x86-64/codegen_builtins.c -o Intel_x86-64/codegen_builtins.o

Intel_x86-64/stackmng/stackmng.o:
	$(CC) $(CCFLAGS) -I$(GRAMMAR_DIR) -I$(TREE_DIR) -I$(LIST_DIR) -c Intel_x86-64/stackmng/stackmng.c -o Intel_x86-64/stackmng/stackmng.o

Intel_x86-64/expr_tree/expr_tree.o:
	$(CC) $(CCFLAGS) -I$(GRAMMAR_DIR) -I$(TREE_DIR) -I$(LIST_DIR) -c Intel_x86-64/expr_tree/expr_tree.c -o Intel_x86-64/expr_tree/expr_tree.o

Intel_x86-64/codegen_expression.o:
	$(CC) $(CCFLAGS) -I$(GRAMMAR_DIR) -I$(TREE_DIR) -I$(LIST_DIR) -IIntel_x86-64/stackmng -IIntel_x86-64/expr_tree -c Intel_x86-64/codegen_expression.c -o Intel_x86-64/codegen_expression.o



############ CLEANING ##########3
clean:
	rm -f *.o Intel_x86-64/*.o Intel_x86-64/stackmng/*.o Intel_x86-64/expr_tree/*.o
