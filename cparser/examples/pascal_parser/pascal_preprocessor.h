#ifndef PASCAL_PREPROCESSOR_H
#define PASCAL_PREPROCESSOR_H

#include <stddef.h>
#include <stdbool.h>

typedef struct PascalPreprocessor PascalPreprocessor;

PascalPreprocessor *pascal_preprocessor_create(void);
void pascal_preprocessor_free(PascalPreprocessor *pp);

bool pascal_preprocessor_define(PascalPreprocessor *pp, const char *symbol);
bool pascal_preprocessor_undefine(PascalPreprocessor *pp, const char *symbol);
bool pascal_preprocessor_is_defined(const PascalPreprocessor *pp, const char *symbol);

char *pascal_preprocess_buffer(PascalPreprocessor *pp,
                               const char *filename,
                               const char *input,
                               size_t length,
                               size_t *out_length,
                               char **error_message);

char *pascal_preprocess_file(PascalPreprocessor *pp,
                             const char *filename,
                             size_t *out_length,
                             char **error_message);

#endif /* PASCAL_PREPROCESSOR_H */
