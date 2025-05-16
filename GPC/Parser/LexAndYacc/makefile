LEX = flex
YACC = bison -v
CC = gcc
FLAGS = -g

# Debug defines
FLEX_DEBUG = -DDEBUG_FLEX

all: lex.yy.o Grammar.tab.o
flexDebug: debug_lex.yy.o Grammar.tab.o

lex.yy.o: lex.yy.c Grammar.tab.h
	$(CC) $(CCFLAGS) -I. -c lex.yy.c
Grammar.tab.o: Grammar.tab.c Grammar.tab.h
	$(CC) $(CCFLAGS) -c Grammar.tab.c

debug_lex.yy.o: lex.yy.c Grammar.tab.h
	$(CC) $(CCFLAGS) $(FLEX_DEBUG) -c lex.yy.c

Grammar.tab.c Grammar.tab.h: Grammar.y
	$(YACC) -d Grammar.y

lex.yy.c: Tokenizer.l
	$(LEX) Tokenizer.l

clean:
	rm -f *.o lex.yy.c Grammar.tab.c Grammar.tab.h Grammar.output y.tab.c y.tab.h y.output
