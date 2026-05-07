/*
 * ir_inst.c — IR instruction implementation.
 *
 * See ir_inst.h for the design overview.
 */

#include "ir_inst.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

/* Pull in the full Register_t and ListNode_t definitions. */
#include "stackmng/stackmng.h"
#include "List.h"

/* -----------------------------------------------------------------------
 * Helpers
 * ----------------------------------------------------------------------- */

/* Strip a leading '%' from a register name string.
 * Returns a pointer into the same string (no allocation). */
static const char *reg_name_stripped(const char *bit_64)
{
    if (bit_64 == NULL)
        return "?";
    if (bit_64[0] == '%')
        return bit_64 + 1;
    return bit_64;
}

/* -----------------------------------------------------------------------
 * Construction / destruction
 * ----------------------------------------------------------------------- */

IrInst_t *ir_inst_new(const char *text,
                      const char *tmpl,
                      Register_t **defs, int n_defs,
                      Register_t **uses, int n_uses,
                      const int *vreg_ids, int n_vreg_ids)
{
    IrInst_t *inst = (IrInst_t *)calloc(1, sizeof(IrInst_t));
    if (inst == NULL)
        return NULL;

    inst->text = (text != NULL) ? strdup(text) : NULL;
    inst->tmpl = (tmpl != NULL) ? strdup(tmpl) : NULL;
    inst->owns_regs = 0;

    /* Initialise vreg_ids to -1 (unset). */
    for (int i = 0; i < IR_MAX_DEFS + IR_MAX_USES; ++i)
        inst->vreg_ids[i] = -1;
    if (vreg_ids != NULL)
    {
        int copy = n_vreg_ids;
        if (copy > IR_MAX_DEFS + IR_MAX_USES)
            copy = IR_MAX_DEFS + IR_MAX_USES;
        for (int i = 0; i < copy; ++i)
            inst->vreg_ids[i] = vreg_ids[i];
    }

    if (n_defs > IR_MAX_DEFS)
        n_defs = IR_MAX_DEFS;
    inst->n_defs = n_defs;
    for (int i = 0; i < n_defs; ++i)
        inst->defs[i] = defs[i];

    if (n_uses > IR_MAX_USES)
        n_uses = IR_MAX_USES;
    inst->n_uses = n_uses;
    for (int i = 0; i < n_uses; ++i)
        inst->uses[i] = uses[i];

    return inst;
}

void ir_inst_free(IrInst_t *inst)
{
    if (inst == NULL)
        return;

    free(inst->text);
    free(inst->tmpl);

    if (inst->owns_regs)
    {
        for (int i = 0; i < inst->n_defs; ++i)
        {
            if (inst->defs[i] != NULL)
            {
                free(inst->defs[i]->bit_64);
                free(inst->defs[i]->bit_32);
                free(inst->defs[i]);
                inst->defs[i] = NULL;
            }
        }
        for (int i = 0; i < inst->n_uses; ++i)
        {
            if (inst->uses[i] != NULL)
            {
                free(inst->uses[i]->bit_64);
                free(inst->uses[i]->bit_32);
                free(inst->uses[i]);
                inst->uses[i] = NULL;
            }
        }
    }

    free(inst);
}

/* -----------------------------------------------------------------------
 * Serialisation
 * ----------------------------------------------------------------------- */

/* Annotation separator used in the textual IR format.
 * Two spaces followed by the semicolon make it visually distinct and
 * unlikely to appear in plain assembly text (which uses '#' for comments
 * in GAS).  The ir_parse() function searches for this exact byte sequence. */
#define IR_ANNOT_SEP "  ; def: "

void ir_print_inst(FILE *out, const IrInst_t *inst)
{
    assert(out != NULL);
    if (inst == NULL)
        return;

    const char *text = inst->text ? inst->text : "";

    if (inst->n_defs == 0 && inst->n_uses == 0)
    {
        /* No annotation — emit verbatim (text already has its '\n'). */
        fputs(text, out);
        return;
    }

    /* Strip trailing newline from text so we can append the annotation
     * on the same line. */
    size_t len = strlen(text);
    if (len > 0 && text[len - 1] == '\n')
        --len;

    /* Print text (without trailing newline). */
    fwrite(text, 1, len, out);

    /* Pad to a fixed column for readability. */
    int col = 40;
    if ((int)len < col)
    {
        for (int i = (int)len; i < col; ++i)
            fputc(' ', out);
    }
    else
    {
        fputc(' ', out);
    }

    /* Emit def list. */
    fputs("; def:", out);
    if (inst->n_defs == 0)
    {
        fputs(" -", out);
    }
    else
    {
        for (int i = 0; i < inst->n_defs; ++i)
        {
            fputc(' ', out);
            fputs(reg_name_stripped(inst->defs[i] ? inst->defs[i]->bit_64 : NULL), out);
        }
    }

    /* Emit use list. */
    fputs(", use:", out);
    if (inst->n_uses == 0)
    {
        fputs(" -", out);
    }
    else
    {
        for (int i = 0; i < inst->n_uses; ++i)
        {
            fputc(' ', out);
            fputs(reg_name_stripped(inst->uses[i] ? inst->uses[i]->bit_64 : NULL), out);
        }
    }

    fputc('\n', out);
}

void ir_print_function(FILE *out, const char *fn_name, ListNode_t *inst_list)
{
    assert(out != NULL);

    fprintf(out, "; --- IR: %s ---\n", fn_name ? fn_name : "<unknown>");

    while (inst_list != NULL)
    {
        if (inst_list->type == LIST_IR_INST)
        {
            IrInst_t *inst = (IrInst_t *)inst_list->cur;
            if (inst != NULL)
                ir_print_inst(out, inst);
        }
        else
        {
            /* LIST_STRING node — no def/use metadata, emit verbatim. */
            const char *text = (const char *)inst_list->cur;
            if (text != NULL)
                fputs(text, out);
        }
        inst_list = inst_list->next;
    }

    fprintf(out, "; --- End: %s ---\n", fn_name ? fn_name : "<unknown>");
}

void ir_print_vregs(FILE *out, const char *fn_name, ListNode_t *inst_list)
{
    assert(out != NULL);

    fprintf(out, "; --- vregs: %s ---\n", fn_name ? fn_name : "<unknown>");

    while (inst_list != NULL)
    {
        if (inst_list->type == LIST_IR_INST)
        {
            IrInst_t *inst = (IrInst_t *)inst_list->cur;
            if (inst == NULL)
            {
                inst_list = inst_list->next;
                continue;
            }

            int has_vregs = 0;
            for (int i = 0; i < inst->n_defs && !has_vregs; ++i)
                if (inst->defs[i] != NULL && inst->defs[i]->vreg_id >= 0)
                    has_vregs = 1;
            for (int i = 0; i < inst->n_uses && !has_vregs; ++i)
                if (inst->uses[i] != NULL && inst->uses[i]->vreg_id >= 0)
                    has_vregs = 1;

            if (!has_vregs)
            {
                const char *text = inst->text ? inst->text : "";
                fputs(text, out);
            }
            else
            {
                /* Strip trailing newline from text for inline annotation. */
                const char *text = inst->text ? inst->text : "";
                size_t len = strlen(text);
                if (len > 0 && text[len - 1] == '\n')
                    --len;
                fwrite(text, 1, len, out);

                int col = 40;
                if ((int)len < col)
                    for (int i = (int)len; i < col; ++i)
                        fputc(' ', out);
                else
                    fputc(' ', out);

                fputs("; defs:", out);
                if (inst->n_defs == 0)
                {
                    fputs(" -", out);
                }
                else
                {
                    for (int i = 0; i < inst->n_defs; ++i)
                    {
                        Register_t *r = inst->defs[i];
                        if (r != NULL && r->vreg_id >= 0)
                            fprintf(out, " vreg_%d(%s)", r->vreg_id,
                                    reg_name_stripped(r->bit_64));
                        else
                            fputs(" -", out);
                    }
                }

                fputs(", uses:", out);
                if (inst->n_uses == 0)
                {
                    fputs(" -", out);
                }
                else
                {
                    for (int i = 0; i < inst->n_uses; ++i)
                    {
                        Register_t *r = inst->uses[i];
                        if (r != NULL && r->vreg_id >= 0)
                            fprintf(out, " vreg_%d(%s)", r->vreg_id,
                                    reg_name_stripped(r->bit_64));
                        else
                            fputs(" -", out);
                    }
                }
                fputc('\n', out);
            }
        }
        else
        {
            const char *text = (const char *)inst_list->cur;
            if (text != NULL)
                fputs(text, out);
        }
        inst_list = inst_list->next;
    }

    fprintf(out, "; --- End vregs: %s ---\n", fn_name ? fn_name : "<unknown>");
}

/* -----------------------------------------------------------------------
 * Deserialisation
 * ----------------------------------------------------------------------- */

/* Create a minimal synthetic Register_t carrying only bit_64 / bit_32.
 * name must be without the leading '%' — we prepend it here.
 * Returns NULL on allocation failure. */
static Register_t *make_synthetic_reg(const char *name)
{
    Register_t *r = (Register_t *)calloc(1, sizeof(Register_t));
    if (r == NULL)
        return NULL;

    /* Allocate bit_64 = "%" + name */
    size_t nlen = strlen(name);
    r->bit_64 = (char *)malloc(nlen + 2);
    if (r->bit_64 == NULL)
    {
        free(r);
        return NULL;
    }
    r->bit_64[0] = '%';
    memcpy(r->bit_64 + 1, name, nlen + 1);

    /* bit_32 is left as an empty string to satisfy any NULL checks. */
    r->bit_32 = strdup("");
    if (r->bit_32 == NULL)
    {
        free(r->bit_64);
        free(r);
        return NULL;
    }

    return r;
}

/* The annotation separator text we look for when parsing. */
#define IR_ANNOT_MARKER "  ; def: "
#define IR_ANNOT_MARKER_LEN (sizeof(IR_ANNOT_MARKER) - 1)
#define IR_USE_MARKER      ", use:"
#define IR_USE_MARKER_LEN  (sizeof(IR_USE_MARKER) - 1)
#define IR_MAX_REG_NAME_LEN 64

/* Parse register names from a whitespace/comma-separated list.
 * Writes synthetic Register_t pointers into out[].
 * Returns the number of registers parsed (capped at max). */
static int parse_reg_list(const char *s, Register_t **out, int max)
{
    int count = 0;
    while (*s != '\0' && count < max)
    {
        /* Skip whitespace and commas. */
        while (*s == ' ' || *s == '\t' || *s == ',')
            ++s;
        if (*s == '\0' || *s == '\n')
            break;

        /* Collect the token. */
        const char *start = s;
        while (*s != '\0' && *s != ' ' && *s != '\t' && *s != ',' && *s != '\n')
            ++s;

        size_t tlen = (size_t)(s - start);
        if (tlen == 0)
            continue;

        /* Ignore placeholder '-' for empty lists. */
        if (tlen == 1 && start[0] == '-')
            continue;

        /* Copy token. */
        char token[IR_MAX_REG_NAME_LEN];
        if (tlen >= sizeof(token))
            tlen = sizeof(token) - 1;
        memcpy(token, start, tlen);
        token[tlen] = '\0';

        Register_t *reg = make_synthetic_reg(token);
        if (reg == NULL)
            break;
        out[count++] = reg;
    }
    return count;
}

ListNode_t *ir_parse(const char *text)
{
    if (text == NULL || *text == '\0')
        return NULL;

    ListNode_t *head = NULL;
    ListNode_t **tail_next = &head;

    const char *p = text;

    while (*p != '\0')
    {
        /* Find end of line. */
        const char *eol = strchr(p, '\n');
        size_t line_len;
        if (eol != NULL)
        {
            line_len = (size_t)(eol - p) + 1; /* include the '\n' */
        }
        else
        {
            line_len = strlen(p);
        }

        /* Skip header/footer comment lines (start with "; --- IR:" etc.). */
        if (p[0] == ';')
        {
            p += line_len;
            continue;
        }

        /* Make a working copy of the line. */
        char *line = (char *)malloc(line_len + 1);
        if (line == NULL)
            break;
        memcpy(line, p, line_len);
        line[line_len] = '\0';

        /* Look for the annotation marker "  ; def: " */
        char *annot = strstr(line, IR_ANNOT_MARKER);
        IrInst_t *inst = (IrInst_t *)calloc(1, sizeof(IrInst_t));
        if (inst == NULL)
        {
            free(line);
            break;
        }
        /* Initialise vreg_ids to -1 (unset). */
        for (int i = 0; i < IR_MAX_DEFS + IR_MAX_USES; ++i)
            inst->vreg_ids[i] = -1;

        if (annot != NULL)
        {
            /* Split: text portion is [line, annot), restored with '\n'. */
            size_t text_len = (size_t)(annot - line);
            char *inst_text = (char *)malloc(text_len + 2);
            if (inst_text == NULL)
            {
                free(line);
                free(inst);
                break;
            }
            memcpy(inst_text, line, text_len);
            inst_text[text_len]     = '\n';
            inst_text[text_len + 1] = '\0';
            inst->text = inst_text;

            /* Parse "def: A B, use: C D" */
            char *after_def = annot + IR_ANNOT_MARKER_LEN;   /* past "  ; def: " */

            /* Find ", use:" separator. */
            char *use_marker = strstr(after_def, IR_USE_MARKER);
            if (use_marker != NULL)
            {
                /* Temporarily null-terminate the def section. */
                *use_marker = '\0';
                inst->n_defs = parse_reg_list(after_def, inst->defs, IR_MAX_DEFS);

                char *after_use = use_marker + IR_USE_MARKER_LEN; /* past ", use:" */
                inst->n_uses = parse_reg_list(after_use, inst->uses, IR_MAX_USES);
            }
            else
            {
                /* No ", use:" — treat everything as defs. */
                inst->n_defs = parse_reg_list(after_def, inst->defs, IR_MAX_DEFS);
                inst->n_uses = 0;
            }
            inst->owns_regs = 1;
        }
        else
        {
            /* Plain line without annotation. */
            inst->text = strdup(line);
            inst->n_defs = 0;
            inst->n_uses = 0;
            inst->owns_regs = 0;
        }

        free(line);

        ListNode_t *node = CreateListNode(inst, LIST_IR_INST);
        if (node == NULL)
        {
            ir_inst_free(inst);
            break;
        }

        *tail_next = node;
        tail_next  = &node->next;

        p += line_len;
        if (eol == NULL)
            break;
    }

    return head;
}

void ir_free_parsed_list(ListNode_t *list)
{
    while (list != NULL)
    {
        ListNode_t *next = list->next;
        if (list->type == LIST_IR_INST)
            ir_inst_free((IrInst_t *)list->cur);
        else
            free(list->cur);
        free(list);
        list = next;
    }
}

/* -----------------------------------------------------------------------
 * Template substitution pass
 * ----------------------------------------------------------------------- */

/* Substitute the %N placeholders in inst->tmpl with physical register names
 * and store the result in inst->text.
 *
 * Register lookup: registers are ordered defs[0..n_defs-1] then
 * uses[0..n_uses-1].  Placeholder %N refers to that combined array at
 * index N.  The physical name used is reg->bit_64 except when the
 * instruction mnemonic ends in 'l' (32-bit suffix), in which case
 * reg->bit_32 is used.
 *
 * The mnemonic is the first token after a leading '\t' in the template.
 * Returns 1 on success, 0 on allocation failure.
 */
static int ir_subst_tmpl(IrInst_t *inst)
{
    if (inst == NULL || inst->tmpl == NULL)
        return 0;

    /* Build the combined register array: defs then uses. */
    int n_regs = inst->n_defs + inst->n_uses;
    Register_t *regs[IR_MAX_DEFS + IR_MAX_USES];
    for (int i = 0; i < inst->n_defs; ++i)
        regs[i] = inst->defs[i];
    for (int i = 0; i < inst->n_uses; ++i)
        regs[inst->n_defs + i] = inst->uses[i];

    /* Determine whether to use bit_32 by inspecting the mnemonic.
     * The template starts with '\t'; the mnemonic follows up to '\t' or ' '.
     * If the mnemonic ends in 'l' (e.g. movl, addl) use bit_32. */
    int use_32 = 0;
    {
        const char *p = inst->tmpl;
        if (*p == '\t')
            ++p;
        /* Scan to end of mnemonic (whitespace or end of string). */
        const char *mnem_start = p;
        while (*p != '\0' && *p != '\t' && *p != ' ' && *p != '\n')
            ++p;
        if (p > mnem_start)
        {
            char last = *(p - 1);
            if (last == 'l')
                use_32 = 1;
        }
    }

    /* Scan tmpl, copy to output, substituting %N placeholders. */
    const char *src = inst->tmpl;
    /* Conservative output buffer: each %N expands to at most ~8 chars. */
    size_t out_cap = strlen(src) + (size_t)n_regs * 16 + 1;
    char *out = (char *)malloc(out_cap);
    if (out == NULL)
        return 0;

    char *dst = out;
    char *out_end = out + out_cap - 1; /* reserve 1 for '\0' */

    while (*src != '\0' && dst < out_end)
    {
        if (src[0] == '%' && src[1] >= '0' && src[1] <= '9')
        {
            /* Parse the decimal index. */
            int idx = 0;
            const char *num_start = src + 1;
            const char *num_end = num_start;
            while (*num_end >= '0' && *num_end <= '9')
                ++num_end;
            for (const char *q = num_start; q < num_end; ++q)
                idx = idx * 10 + (*q - '0');
            src = num_end;

            /* Look up the register. */
            const char *reg_name = NULL;
            if (idx >= 0 && idx < n_regs && regs[idx] != NULL)
                reg_name = use_32 ? regs[idx]->bit_32 : regs[idx]->bit_64;

            if (reg_name == NULL)
                reg_name = "?";

            /* Copy register name to output. */
            while (*reg_name != '\0' && dst < out_end)
                *dst++ = *reg_name++;
        }
        else
        {
            *dst++ = *src++;
        }
    }
    *dst = '\0';

    inst->text = out;
    return 1;
}

void ir_emit_function(ListNode_t *inst_list)
{
    while (inst_list != NULL)
    {
        if (inst_list->type == LIST_IR_INST)
        {
            IrInst_t *inst = (IrInst_t *)inst_list->cur;
            if (inst != NULL && inst->text == NULL && inst->tmpl != NULL)
                ir_subst_tmpl(inst);
        }
        inst_list = inst_list->next;
    }
}
