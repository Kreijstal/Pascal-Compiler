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
    fprintf(stderr, "    --flatten-only Expand includes but keep directives/branches intact\n");
    fprintf(stderr, "    -D<symbol>    Define a preprocessor symbol\n");
    fprintf(stderr, "    -U<symbol>    Undefine a preprocessor symbol\n");
    fprintf(stderr, "    -I<path>      Add include search path\n");
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
    bool flatten_only = false;
    int exit_code = 1;

    /* Parse arguments */
    for (int i = 1; i < argc; i++)
    {
        if (strcmp(argv[i], "--flatten-only") == 0)
        {
            flatten_only = true;
        }
        else if (argv[i][0] == '-')
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
            else if (argv[i][1] == 'I')
            {
                /* Handle -I later after creating preprocessor */
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

    pascal_preprocessor_set_flatten_only(preprocessor, flatten_only);

    /* Define default symbols (matching FPC 3.2.2 for x86_64 Linux) */
    pascal_preprocessor_define(preprocessor, "FPC");
    pascal_preprocessor_define(preprocessor, "OBJFPC");
    
    /* FPC version defines (matching FPC 3.2.2) */
    pascal_preprocessor_define(preprocessor, "VER3");
    pascal_preprocessor_define(preprocessor, "VER3_2");
    pascal_preprocessor_define(preprocessor, "VER3_2_2");
    
    /* FPC version macros with values */
    pascal_preprocessor_define(preprocessor, "FPC_VERSION:=3");
    pascal_preprocessor_define(preprocessor, "FPC_RELEASE:=2");
    pascal_preprocessor_define(preprocessor, "FPC_PATCH:=2");
    pascal_preprocessor_define(preprocessor, "FPC_FULLVERSION:=30202");
    
    /* Platform-specific size constants */
    pascal_preprocessor_define(preprocessor, "__SIZEOF_PTHREAD_MUTEX_T:=40");
    pascal_preprocessor_define(preprocessor, "__SIZEOF_PTHREAD_COND_T:=48");
    
    /* FPC compiler feature defines */
    pascal_preprocessor_define(preprocessor, "FPC_HAS_FEATURE_SUPPORT");
    pascal_preprocessor_define(preprocessor, "FPC_HAS_FEATURE_EXCEPTIONS");
    pascal_preprocessor_define(preprocessor, "FPC_HAS_FEATURE_EXITCODE");
    pascal_preprocessor_define(preprocessor, "FPC_HAS_FEATURE_HEAP");
    pascal_preprocessor_define(preprocessor, "FPC_HAS_FEATURE_INITFINAL");
    pascal_preprocessor_define(preprocessor, "FPC_HAS_FEATURE_RTTI");
    pascal_preprocessor_define(preprocessor, "FPC_HAS_FEATURE_CLASSES");
    pascal_preprocessor_define(preprocessor, "FPC_HAS_FEATURE_OBJECTS");
    pascal_preprocessor_define(preprocessor, "FPC_HAS_FEATURE_DYNARRAYS");
    pascal_preprocessor_define(preprocessor, "FPC_HAS_FEATURE_ANSISTRINGS");
    pascal_preprocessor_define(preprocessor, "FPC_HAS_FEATURE_WIDESTRINGS");
    pascal_preprocessor_define(preprocessor, "FPC_HAS_FEATURE_FILEIO");
    pascal_preprocessor_define(preprocessor, "FPC_HAS_FEATURE_TEXTIO");
    pascal_preprocessor_define(preprocessor, "FPC_HAS_FEATURE_CONSOLEIO");
    pascal_preprocessor_define(preprocessor, "FPC_HAS_FEATURE_RANDOM");
    pascal_preprocessor_define(preprocessor, "FPC_HAS_FEATURE_VARIANTS");
    pascal_preprocessor_define(preprocessor, "FPC_HAS_FEATURE_RESOURCES");
    pascal_preprocessor_define(preprocessor, "FPC_HAS_FEATURE_COMMANDARGS");
    pascal_preprocessor_define(preprocessor, "FPC_HAS_FEATURE_PROCESSES");
    pascal_preprocessor_define(preprocessor, "FPC_HAS_FEATURE_THREADING");
    pascal_preprocessor_define(preprocessor, "FPC_HAS_FEATURE_DYNLIBS");
    pascal_preprocessor_define(preprocessor, "FPC_HAS_FEATURE_OBJECTIVEC1");
    pascal_preprocessor_define(preprocessor, "FPC_HAS_FEATURE_UNICODESTRINGS");
    pascal_preprocessor_define(preprocessor, "FPC_HAS_FEATURE_STACKCHECK");
    
    /* FPC internal capabilities */
    pascal_preprocessor_define(preprocessor, "FPC_HAS_SETSJMP");
    pascal_preprocessor_define(preprocessor, "FPC_HAS_OPERATOR_ENUMERATOR");
    pascal_preprocessor_define(preprocessor, "FPC_HAS_CONSTREF");
    pascal_preprocessor_define(preprocessor, "FPC_HAS_UNICODESTRING");
    pascal_preprocessor_define(preprocessor, "FPC_HAS_INTERNAL_ABS_LONG");
    pascal_preprocessor_define(preprocessor, "FPC_HAS_INTERNAL_ABS_INT64");
    pascal_preprocessor_define(preprocessor, "FPC_HAS_INTERNAL_BSF");
    pascal_preprocessor_define(preprocessor, "FPC_HAS_INTERNAL_BSR");
    pascal_preprocessor_define(preprocessor, "FPC_HAS_INTERNAL_ROX");
    pascal_preprocessor_define(preprocessor, "FPC_HAS_INTERNAL_SAR");
    pascal_preprocessor_define(preprocessor, "FPC_HAS_MEMBAR");
    pascal_preprocessor_define(preprocessor, "FPC_HAS_WINLIKERESOURCES");
    pascal_preprocessor_define(preprocessor, "FPC_HAS_CPSTRING");
    pascal_preprocessor_define(preprocessor, "FPC_HAS_CEXTENDED");
    pascal_preprocessor_define(preprocessor, "FPC_HAS_RESSTRINITS");
    pascal_preprocessor_define(preprocessor, "FPC_HAS_RIP_RELATIVE");
    pascal_preprocessor_define(preprocessor, "FPC_HAS_TYPE_SINGLE");
    pascal_preprocessor_define(preprocessor, "FPC_HAS_TYPE_DOUBLE");
    pascal_preprocessor_define(preprocessor, "FPC_HAS_TYPE_EXTENDED");
    pascal_preprocessor_define(preprocessor, "FPC_HAS_INDIRECT_ENTRY_INFORMATION");
    pascal_preprocessor_define(preprocessor, "FPC_STATICRIPFIXED");
    pascal_preprocessor_define(preprocessor, "FPC_VARIANTCOPY_FIXED");
    pascal_preprocessor_define(preprocessor, "FPC_DYNARRAYCOPY_FIXED");
    pascal_preprocessor_define(preprocessor, "FPC_SETBASE_USED");
    pascal_preprocessor_define(preprocessor, "FPC_RTTI_PACKSET1");
    pascal_preprocessor_define(preprocessor, "FPC_WIDESTRING_EQUAL_UNICODESTRING");
    pascal_preprocessor_define(preprocessor, "FPC_LINK_STATIC");
    pascal_preprocessor_define(preprocessor, "FPC_LITTLE_ENDIAN");
    
    /* Other FPC defines */
    pascal_preprocessor_define(preprocessor, "INTERNAL_BACKTRACE");
    pascal_preprocessor_define(preprocessor, "STR_CONCAT_PROCS");
    pascal_preprocessor_define(preprocessor, "REGCALL");
    pascal_preprocessor_define(preprocessor, "HASUNIX");

    /* Define Comp as Comp to satisfy {$if not declared(Comp)} check in systemh.inc */
    pascal_preprocessor_define_macro(preprocessor, "Comp", "Comp");

    /* Auto-define platform macros based on host compiler */
#if defined(__linux__)
    pascal_preprocessor_define(preprocessor, "LINUX");
    pascal_preprocessor_define(preprocessor, "UNIX");
    pascal_preprocessor_define(preprocessor, "CONSOLE");
    pascal_preprocessor_define(preprocessor, "ENDIAN_LITTLE");
    
    /* Define constants for heap.inc to avoid {$ERROR} 
       These are compile-time constants available for {$if} evaluation
       but NOT for text replacement, since they also appear as Pascal
       constant declarations in the FPC RTL source. */
    pascal_preprocessor_define_const(preprocessor, "FixedArenaOffsetShift", "5");
    pascal_preprocessor_define_const(preprocessor, "VarSizeQuant", "32");
    pascal_preprocessor_define_const(preprocessor, "FirstVarRangeP2", "10");
    pascal_preprocessor_define_const(preprocessor, "FirstVarStepP2", "5");
    pascal_preprocessor_define_const(preprocessor, "VarSizeClassesCount", "10");
    pascal_preprocessor_define_const(preprocessor, "MaxFixedHeaderAndPayload", "544");
    pascal_preprocessor_define_const(preprocessor, "MaxVarHeaderAndPayload", "1048096");
    pascal_preprocessor_define_const(preprocessor, "CommonHeaderSize", "4");
    pascal_preprocessor_define_const(preprocessor, "MinFixedHeaderAndPayload", "16");
    pascal_preprocessor_define_const(preprocessor, "MinSearchableVarHeaderAndPayload", "576");
    
    /* Heap constants from heap.inc (computed at compile time in FPC) */
    pascal_preprocessor_define_const(preprocessor, "FixedSizesCount", "16");
    pascal_preprocessor_define_const(preprocessor, "SizeIndexBits", "4"); /* 1 + trunc(ln(15)/ln(2)) = 4 */
    pascal_preprocessor_define_const(preprocessor, "SizeIndexMask", "15"); /* (1 << 4) - 1 = 15 */
    pascal_preprocessor_define_const(preprocessor, "FixedBitPos", "4");
    pascal_preprocessor_define_const(preprocessor, "VarSizesPerClass", "32");
    pascal_preprocessor_define_const(preprocessor, "VarSizesCount", "320"); /* VarSizeClassesCount * VarSizesPerClass = 10 * 32 */
    pascal_preprocessor_define_const(preprocessor, "L0BinSize", "32");
    
    /* System constants */
    pascal_preprocessor_define_const(preprocessor, "maxExitCode", "255");
#endif

#if defined(__x86_64__)
    pascal_preprocessor_define(preprocessor, "CPU64");
    pascal_preprocessor_define(preprocessor, "CPUX86_64");
    pascal_preprocessor_define(preprocessor, "CPUAMD64");
    pascal_preprocessor_define(preprocessor, "CPUX64");
    pascal_preprocessor_define(preprocessor, "CPUINT64");
    pascal_preprocessor_define(preprocessor, "X86_64");
    pascal_preprocessor_define(preprocessor, "CPUATHLON64");
    pascal_preprocessor_define(preprocessor, "FPUSSE64");
    pascal_preprocessor_define(preprocessor, "FPC_ABI_DEFAULT");
    pascal_preprocessor_define(preprocessor, "CPUX86_HAS_CMOV");
    pascal_preprocessor_define(preprocessor, "CPUX86_HAS_SSE2");
    pascal_preprocessor_define(preprocessor, "CPUX86_HAS_SSEUNIT");
    /* CPU register constants from cpubase.pas for x86_64 */
    pascal_preprocessor_define_macro(preprocessor, "first_mm_imreg", "32"); /* $20 in hex */

    /* max_operands for x86 (4 operands max in x86 instructions) */
    pascal_preprocessor_define_macro(preprocessor, "max_operands", "4");

    /* Stack alignment for x86_64 */
    pascal_preprocessor_define(preprocessor, "FPC_STACKALIGNMENT:=16");
#endif

#if defined(__i386__)
    pascal_preprocessor_define(preprocessor, "CPU32");
    pascal_preprocessor_define(preprocessor, "CPUI386");
    pascal_preprocessor_define(preprocessor, "I386");
#endif

    /* Handle -D, -U, and -I options */
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
        else if (argv[i][0] == '-' && argv[i][1] == 'I' && argv[i][2] != '\0')
        {
            pascal_preprocessor_add_include_path(preprocessor, &argv[i][2]);
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
