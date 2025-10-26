CC = gcc
FLAGS = -g
OPTIMIZE =
LIBS = -ly -ll -lm

# Directory for Lex and Yacc
GRAMMAR_DIR = LexAndYacc
TREE_DIR = ParseTree

# Base CCFLAGS without debug flags
CCFLAGS_BASE = $(FLAGS) $(OPTIMIZE)
# Extra flags that can be passed from command line
EXTRA_CCFLAGS ?=
# Final CCFLAGS used in compilation
CCFLAGS = $(CCFLAGS_BASE) $(EXTRA_CCFLAGS)

# Object files to build
GPC_OBJS = ParsePascal.o
TREE_OBJS = List/List.o $(TREE_DIR)/tree.o
SEM_OBJS = SemCheck.o SemCheck_stmt.o SemCheck_expr.o HashTable.o SymTab.o NameMangling.o
PARSER_OBJS = $(GRAMMAR_DIR)/lex.yy.o $(GRAMMAR_DIR)/Grammar.tab.o
ALL_OBJS = $(GPC_OBJS) $(TREE_OBJS) $(SEM_OBJS) $(PARSER_OBJS)

######## THE MAIN BUILD RULES ###########
release:
	cd LexAndYacc && $(MAKE)
	$(MAKE) parser

debug_flexbison:
	cd $(GRAMMAR_DIR) && $(MAKE) flexDebug
	$(MAKE) parser EXTRA_CCFLAGS="-DDEBUG_BISON"

debug_bison:
	cd $(GRAMMAR_DIR) && $(MAKE)
	$(MAKE) parser EXTRA_CCFLAGS="-DDEBUG_BISON"

debug_flex:
	cd $(GRAMMAR_DIR) && $(MAKE) flexDebug
	$(MAKE) parser EXTRA_CCFLAGS=""


############ MAKING OBJS #############
# Making all the objects and bins across directories
# Please no touch :)
parser: $(ALL_OBJS) ParsePascal.o

ParsePascal.o: $(PARSER_OBJS) $(TREE_OBJS) ParsePascal.h ParsePascal.c
	$(CC) $(CCFLAGS) -I$(GRAMMAR_DIR) -I$(TREE_DIR) -IList -c ParsePascal.c

#ParsePascal.o: $(PARSER_OBJS) $(TREE_OBJS) ParsePascal.h ParsePascal.c
#	$(CC) $(CCFLAGS) -DDEBUG_BISON -c ParsePascal.c

List/List.o:
	$(CC) $(CCFLAGS) -I$(GRAMMAR_DIR) -c List/List.c

SemCheck.o: $(PARSER_OBJS) $(TREE_OBJS) SemCheck_stmt.o SemCheck_expr.o SymTab.o HashTable.o
	$(CC) $(CCFLAGS) -I$(GRAMMAR_DIR) -I$(TREE_DIR) -IList -ISemanticCheck/SymTab -ISemanticCheck/HashTable -c SemanticCheck/SemCheck.c

SemCheck_stmt.o: $(PARSER_OBJS) $(TREE_OBJS) SymTab.o HashTable.o
	$(CC) $(CCFLAGS) -I$(GRAMMAR_DIR) -I$(TREE_DIR) -IList -ISemanticCheck/SymTab -ISemanticCheck/HashTable -c SemanticCheck/SemChecks/SemCheck_stmt.c

SemCheck_expr.o: $(PARSER_OBJS) $(TREE_OBJS) SymTab.o HashTable.o
	$(CC) $(CCFLAGS) -I$(GRAMMAR_DIR) -I$(TREE_DIR) -IList -ISemanticCheck/SymTab -ISemanticCheck/HashTable -c SemanticCheck/SemChecks/SemCheck_expr.c

HashTable.o: $(GRAMMAR_DIR)/Grammar.tab.h
	$(CC) $(CCFLAGS) -I$(GRAMMAR_DIR) -c SemanticCheck/HashTable/HashTable.c

SymTab.o: $(GRAMMAR_DIR)/Grammar.tab.h HashTable.o
	$(CC) $(CCFLAGS) -I$(GRAMMAR_DIR) -ISemanticCheck/HashTable -c SemanticCheck/SymTab/SymTab.c

$(TREE_DIR)/tree.o:
	$(CC) $(CCFLAGS) -I$(GRAMMAR_DIR) -c $(TREE_DIR)/tree.c

lexAndYacc:
	cd $(GRAMMAR_DIR) && $(MAKE)






############ CLEANING ##########3
clean: clean_parser clean_LexYacc
clean_parser:
	rm -f $(ALL_OBJS) *.o
clean_LexYacc:
	cd $(GRAMMAR_DIR) && $(MAKE) clean

NameMangling.o: $(GRAMMAR_DIR)/Grammar.tab.h SemCheck_expr.o
	$(CC) $(CCFLAGS) -I$(GRAMMAR_DIR) -I$(TREE_DIR) -IList -ISemanticCheck/SymTab -ISemanticCheck/HashTable -c SemanticCheck/NameMangling.c
