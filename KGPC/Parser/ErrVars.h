/*
    Damon Gwinn
    Error variables for when parsing fails
*/

#ifndef ERR_VARS_H
#define ERR_VARS_H

#include <stddef.h>

extern int line_num;
extern int col_num;
extern char *file_to_parse;
extern char *preprocessed_source;
extern size_t preprocessed_length;
extern char *preprocessed_path;

#endif
