/*
 * cparser_frontend.h
 * New parser frontend using cparser library
 */

#ifndef CPARSER_FRONTEND_H
#define CPARSER_FRONTEND_H

#include "Parser/ParseTree/tree.h"

// Parse a Pascal file using cparser and return GPC parse tree
Tree_t* parse_pascal_file_with_cparser(const char* filename);

#endif // CPARSER_FRONTEND_H
