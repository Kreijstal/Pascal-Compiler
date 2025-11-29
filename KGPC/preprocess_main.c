#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#include "pascal_preprocessor.h"

static void print_usage(const char *prog_name)
{
    fprintf(stderr, "Usage: %s <input.p> [output.p]\n", prog_name);
    fprintf(stderr, "  Preprocesses Pascal source file\n");
    fprintf(stderr, "  If output file is not specified, writes to stdout\n");
    fprintf(stderr, "  Options:\n");
    fprintf(stderr, "    -D<symbol>    Define a preprocessor symbol\n");
    fprintf(stderr, "    -U<symbol>    Undefine a preprocessor symbol\n");
}

static char *read_file(const char *filename)
{
    FILE *fp = fopen(filename, "r");
    if (fp == NULL)
    {
        fprintf(stderr, "Error: Could not open file '%s'\n", filename);
        return NULL;
    }

    /* Get file size */
    fseek(fp, 0, SEEK_END);
    long size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    /* Allocate buffer */
    char *buffer = (char *)malloc(size + 1);
    if (buffer == NULL)
    {
        fprintf(stderr, "Error: Could not allocate memory\n");
        fclose(fp);
        return NULL;
    }

    /* Read file */
    size_t read_size = fread(buffer, 1, size, fp);
    buffer[read_size] = '\0';
    fclose(fp);

    return buffer;
}

int main(int argc, char **argv)
{
    const char *input_file = NULL;
    const char *output_file = NULL;
    PascalPreprocessor *preprocessor = NULL;
    char *source = NULL;
    char *preprocessed = NULL;
    int exit_code = 1;

    /* Parse arguments */
    for (int i = 1; i < argc; i++)
    {
        if (argv[i][0] == '-')
        {
            if (argv[i][1] == 'D')
            {
                /* Handle -D later after creating preprocessor */
                continue;
            }
            else if (argv[i][1] == 'U')
            {
                /* Handle -U later after creating preprocessor */
                continue;
            }
            else
            {
                fprintf(stderr, "Unknown option: %s\n", argv[i]);
                print_usage(argv[0]);
                return 1;
            }
        }
        else if (input_file == NULL)
        {
            input_file = argv[i];
        }
        else if (output_file == NULL)
        {
            output_file = argv[i];
        }
        else
        {
            fprintf(stderr, "Too many arguments\n");
            print_usage(argv[0]);
            return 1;
        }
    }

    if (input_file == NULL)
    {
        print_usage(argv[0]);
        return 1;
    }

    /* Create preprocessor */
    preprocessor = pascal_preprocessor_create();
    if (preprocessor == NULL)
    {
        fprintf(stderr, "Error: Could not create preprocessor\n");
        return 1;
    }

    /* Define default symbols (matching FPC) */
    pascal_preprocessor_define(preprocessor, "FPC");
    pascal_preprocessor_define(preprocessor, "OBJFPC");

    /* Handle -D and -U options */
    for (int i = 1; i < argc; i++)
    {
        if (argv[i][0] == '-' && argv[i][1] == 'D' && argv[i][2] != '\0')
        {
            pascal_preprocessor_define(preprocessor, &argv[i][2]);
        }
        else if (argv[i][0] == '-' && argv[i][1] == 'U' && argv[i][2] != '\0')
        {
            pascal_preprocessor_undefine(preprocessor, &argv[i][2]);
        }
    }

    /* Read input file */
    source = read_file(input_file);
    if (source == NULL)
    {
        goto cleanup;
    }

    /* Preprocess */
    char *error_message = NULL;
    size_t preprocessed_length = 0;
    preprocessed = pascal_preprocess_buffer(
        preprocessor,
        input_file,
        source,
        strlen(source),
        &preprocessed_length,
        &error_message);
    
    if (preprocessed == NULL)
    {
        fprintf(stderr, "Error: Preprocessing failed for '%s'\n", input_file);
        if (error_message != NULL)
        {
            fprintf(stderr, "%s\n", error_message);
            free(error_message);
        }
        goto cleanup;
    }
    
    if (error_message != NULL)
    {
        free(error_message);
    }

    /* Write output */
    FILE *out_fp = stdout;
    if (output_file != NULL)
    {
        out_fp = fopen(output_file, "w");
        if (out_fp == NULL)
        {
            fprintf(stderr, "Error: Could not open output file '%s'\n", output_file);
            goto cleanup;
        }
    }

    fprintf(out_fp, "%s", preprocessed);

    if (output_file != NULL)
    {
        fclose(out_fp);
    }

    exit_code = 0;

cleanup:
    if (preprocessed != NULL)
    {
        free(preprocessed);
    }
    if (source != NULL)
    {
        free(source);
    }
    if (preprocessor != NULL)
    {
        pascal_preprocessor_free(preprocessor);
    }

    return exit_code;
}
