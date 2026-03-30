/*
 * Binary AST cache implementation.
 * See ast_cache.h for format description.
 */
#include "ast_cache.h"
#include "../common/file_lock.h"
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <unistd.h>

static const char MAGIC[] = "KGPC_AST";
#define MAGIC_LEN 9  /* includes NUL */

/* --- Writer --- */

static bool write_u8(FILE *f, uint8_t v)  { return fwrite(&v, 1, 1, f) == 1; }
static bool write_u32(FILE *f, uint32_t v) { return fwrite(&v, 4, 1, f) == 1; }
static bool write_i32(FILE *f, int32_t v)  { return fwrite(&v, 4, 1, f) == 1; }

static bool write_single_node(FILE *f, const ast_t *node)
{
    if (!write_u32(f, (uint32_t)node->typ)) return false;
    if (!write_i32(f, (int32_t)node->line)) return false;
    if (!write_i32(f, (int32_t)node->col))  return false;
    if (!write_i32(f, (int32_t)node->index)) return false;

    if (node->sym != NULL && node->sym->name != NULL)
    {
        uint32_t len = (uint32_t)strlen(node->sym->name);
        if (!write_u8(f, 1)) return false;
        if (!write_u32(f, len)) return false;
        if (fwrite(node->sym->name, 1, len, f) != len) return false;
    }
    else
    {
        if (!write_u8(f, 0)) return false;
    }

    if (!write_u8(f, node->child != NULL ? 1 : 0)) return false;
    if (!write_u8(f, node->next  != NULL ? 1 : 0)) return false;

    return true;
}

/* Iterative pre-order write to avoid stack overflow on long sibling chains. */
static bool write_node(FILE *f, const ast_t *node)
{
    /* Simple explicit stack to avoid deep recursion on next-chains */
    const ast_t *cur = node;
    while (cur != NULL)
    {
        if (!write_single_node(f, cur)) return false;

        /* Recurse into child subtree (depth is bounded by nesting) */
        if (cur->child != NULL)
        {
            if (!write_node(f, cur->child)) return false;
        }

        /* Iterate on next sibling */
        cur = cur->next;
    }
    return true;
}

bool ast_cache_save(const char *cache_path, const ast_t *root,
                    const char *preprocessed_buf, size_t preprocessed_len)
{
    if (cache_path == NULL || root == NULL)
        return false;

    if (!file_lock_acquire(cache_path, 30))
        return false;

    size_t tmp_len = strlen(cache_path) + 32;
    char *tmp_path = (char *)malloc(tmp_len);
    if (tmp_path == NULL)
    {
        file_lock_release(cache_path);
        return false;
    }

    snprintf(tmp_path, tmp_len, "%s.tmp.%ld", cache_path, (long)getpid());

    FILE *f = fopen(tmp_path, "wb");
    if (f == NULL)
    {
        file_lock_release(cache_path);
        free(tmp_path);
        return false;
    }

    /* Header */
    if (fwrite(MAGIC, 1, MAGIC_LEN, f) != MAGIC_LEN) goto fail;
    if (!write_u32(f, AST_CACHE_VERSION)) goto fail;

    /* Preprocessed source */
    uint32_t pp_len = (uint32_t)preprocessed_len;
    if (!write_u32(f, pp_len)) goto fail;
    if (preprocessed_len > 0)
    {
        if (fwrite(preprocessed_buf, 1, preprocessed_len, f) != preprocessed_len) goto fail;
    }

    /* AST */
    if (!write_node(f, root)) goto fail;

    if (fflush(f) != 0) goto fail;
    fclose(f);
    f = NULL;

    if (rename(tmp_path, cache_path) != 0)
        goto fail;

    file_lock_release(cache_path);
    free(tmp_path);
    return true;

fail:
    if (f != NULL)
        fclose(f);
    unlink(tmp_path);
    file_lock_release(cache_path);
    free(tmp_path);
    return false;
}

/* --- Reader --- */

static bool read_u8(FILE *f, uint8_t *v)  { return fread(v, 1, 1, f) == 1; }
static bool read_u32(FILE *f, uint32_t *v) { return fread(v, 4, 1, f) == 1; }
static bool read_i32(FILE *f, int32_t *v)  { return fread(v, 4, 1, f) == 1; }

static ast_t *read_single_node_fields(FILE *f)
{
    uint32_t typ;
    int32_t line, col, idx;
    if (!read_u32(f, &typ)) return NULL;
    if (!read_i32(f, &line)) return NULL;
    if (!read_i32(f, &col))  return NULL;
    if (!read_i32(f, &idx))  return NULL;

    ast_t *node = (ast_t *)calloc(1, sizeof(ast_t));
    if (node == NULL) return NULL;

    node->typ = (tag_t)typ;
    node->line = (int)line;
    node->col  = (int)col;
    node->index = (int)idx;

    uint8_t has_sym;
    if (!read_u8(f, &has_sym)) { free(node); return NULL; }
    if (has_sym)
    {
        uint32_t sym_len;
        if (!read_u32(f, &sym_len)) { free(node); return NULL; }
        sym_t *sym = (sym_t *)calloc(1, sizeof(sym_t));
        if (sym == NULL) { free(node); return NULL; }
        sym->name = (char *)malloc(sym_len + 1);
        if (sym->name == NULL) { free(sym); free(node); return NULL; }
        if (sym_len > 0 && fread(sym->name, 1, sym_len, f) != sym_len)
        {
            free(sym->name); free(sym); free(node); return NULL;
        }
        sym->name[sym_len] = '\0';
        node->sym = sym;
    }

    return node;
}

/* Iterative read to match iterative write. */
static ast_t *read_node(FILE *f)
{
    ast_t *first = NULL;
    ast_t *prev = NULL;

    /* Read nodes iteratively following the next-chain */
    for (;;)
    {
        ast_t *node = read_single_node_fields(f);
        if (node == NULL) goto fail;

        uint8_t has_child, has_next;
        if (!read_u8(f, &has_child)) { free_ast(node); goto fail; }
        if (!read_u8(f, &has_next))  { free_ast(node); goto fail; }

        if (has_child)
        {
            node->child = read_node(f);
            if (node->child == NULL) { free_ast(node); goto fail; }
        }

        if (first == NULL)
            first = node;
        if (prev != NULL)
            prev->next = node;
        prev = node;

        if (!has_next)
            break;
    }

    return first;

fail:
    if (first != NULL) free_ast(first);
    return NULL;
}

bool ast_cache_load(const char *cache_path, ast_t **out_root,
                    char **out_buf, size_t *out_len)
{
    if (out_root != NULL) *out_root = NULL;
    if (out_buf != NULL) *out_buf = NULL;
    if (out_len != NULL) *out_len = 0;

    if (cache_path == NULL)
        return false;

    FILE *f = fopen(cache_path, "rb");
    if (f == NULL)
        return false;

    /* Verify header */
    char magic[MAGIC_LEN];
    if (fread(magic, 1, MAGIC_LEN, f) != MAGIC_LEN) goto fail;
    if (memcmp(magic, MAGIC, MAGIC_LEN) != 0) goto fail;

    uint32_t version;
    if (!read_u32(f, &version)) goto fail;
    if (version != AST_CACHE_VERSION) goto fail;

    /* Read preprocessed source */
    uint32_t pp_len;
    if (!read_u32(f, &pp_len)) goto fail;
    char *buf = (char *)malloc(pp_len + 1);
    if (buf == NULL) goto fail;
    if (pp_len > 0 && fread(buf, 1, pp_len, f) != pp_len)
    {
        free(buf);
        goto fail;
    }
    buf[pp_len] = '\0';

    /* Read AST */
    ast_t *root = read_node(f);
    if (root == NULL)
    {
        free(buf);
        goto fail;
    }

    fclose(f);
    if (out_root != NULL) *out_root = root;
    if (out_buf != NULL) *out_buf = buf;
    else free(buf);
    if (out_len != NULL) *out_len = pp_len;
    return true;

fail:
    fclose(f);
    return false;
}
