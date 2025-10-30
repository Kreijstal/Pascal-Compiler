#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <gmp.h>

typedef struct gpc_gmp_mpint
{
    mpz_t value;
    size_t refcount;
} gpc_gmp_mpint_t;

static gpc_gmp_mpint_t *gpc_gmp_mpint_alloc(void)
{
    gpc_gmp_mpint_t *mp = (gpc_gmp_mpint_t *)malloc(sizeof(gpc_gmp_mpint_t));
    if (mp == NULL)
        return NULL;
    mpz_init(mp->value);
    mp->refcount = 1;
    return mp;
}

static void gpc_gmp_mpint_free(gpc_gmp_mpint_t *mp)
{
    if (mp == NULL)
        return;
    mpz_clear(mp->value);
    free(mp);
}

static void gpc_gmp_mpint_acquire(gpc_gmp_mpint_t *mp)
{
    if (mp == NULL)
        return;
    mp->refcount += 1;
}

static void gpc_gmp_mpint_release(gpc_gmp_mpint_t *mp)
{
    if (mp == NULL)
        return;
    if (mp->refcount > 1)
    {
        mp->refcount -= 1;
        return;
    }
    gpc_gmp_mpint_free(mp);
}

static gpc_gmp_mpint_t *gpc_gmp_mpint_ensure_unique(void **slot_ptr)
{
    if (slot_ptr == NULL)
        return NULL;

    gpc_gmp_mpint_t *value = (gpc_gmp_mpint_t *)(*slot_ptr);
    if (value == NULL)
    {
        value = gpc_gmp_mpint_alloc();
        if (value == NULL)
            return NULL;
        *slot_ptr = value;
        return value;
    }

    if (value->refcount > 1)
    {
        gpc_gmp_mpint_t *copy = gpc_gmp_mpint_alloc();
        if (copy == NULL)
            return NULL;
        mpz_set(copy->value, value->value);
        value->refcount -= 1;
        *slot_ptr = copy;
        value = copy;
    }

    return value;
}

void gpc_gmp_z_init(void **slot_ptr)
{
    if (slot_ptr == NULL)
        return;

    gpc_gmp_mpint_t *current = (gpc_gmp_mpint_t *)(*slot_ptr);
    if (current != NULL)
        gpc_gmp_mpint_release(current);

    gpc_gmp_mpint_t *fresh = gpc_gmp_mpint_alloc();
    if (fresh == NULL)
    {
        *slot_ptr = NULL;
        return;
    }

    mpz_set_ui(fresh->value, 0);
    *slot_ptr = fresh;
}

void gpc_gmp_z_clear(void **slot_ptr)
{
    if (slot_ptr == NULL)
        return;

    gpc_gmp_mpint_t *value = (gpc_gmp_mpint_t *)(*slot_ptr);
    if (value != NULL)
        gpc_gmp_mpint_release(value);
    *slot_ptr = NULL;
}

void gpc_gmp_z_set_ui(void **slot_ptr, uint64_t operand)
{
    gpc_gmp_mpint_t *value = gpc_gmp_mpint_ensure_unique(slot_ptr);
    if (value == NULL)
        return;

    mpz_set_ui(value->value, operand);
}

void gpc_gmp_z_add_ui(void **slot_ptr, uint64_t operand)
{
    gpc_gmp_mpint_t *value = gpc_gmp_mpint_ensure_unique(slot_ptr);
    if (value == NULL)
        return;

    mpz_add_ui(value->value, value->value, operand);
}

void gpc_gmp_z_ui_pow_ui(void **slot_ptr, uint64_t base, uint64_t exponent)
{
    gpc_gmp_mpint_t *value = gpc_gmp_mpint_ensure_unique(slot_ptr);
    if (value == NULL)
        return;

    mpz_ui_pow_ui(value->value, base, exponent);
}

uint64_t gpc_gmp_z_size(void **slot_ptr)
{
    if (slot_ptr == NULL)
        return 0;

    gpc_gmp_mpint_t *value = (gpc_gmp_mpint_t *)(*slot_ptr);
    if (value == NULL)
        return 0;

    size_t limbs = mpz_size(value->value);
    return (uint64_t)limbs;
}

void gpc_gmp_z_copy(void **dest_slot, void **src_slot)
{
    if (dest_slot == NULL)
        return;

    gpc_gmp_mpint_t *src = (src_slot != NULL) ? (gpc_gmp_mpint_t *)(*src_slot) : NULL;
    gpc_gmp_mpint_t *dest = gpc_gmp_mpint_ensure_unique(dest_slot);
    if (dest == NULL)
        return;

    if (src == NULL)
    {
        mpz_set_ui(dest->value, 0);
        return;
    }

    mpz_set(dest->value, src->value);
}

void *gpc_gmp_z_add(void **target_slot, void **operand_slot)
{
    gpc_gmp_mpint_t *target = gpc_gmp_mpint_ensure_unique(target_slot);
    if (target == NULL)
        return NULL;

    gpc_gmp_mpint_t *operand = (operand_slot != NULL) ? (gpc_gmp_mpint_t *)(*operand_slot) : NULL;
    if (operand == NULL)
        return target;

    mpz_add(target->value, target->value, operand->value);
    return target;
}

static char *gpc_gmp_alloc_zero_string(void)
{
    char *buffer = (char *)malloc(2);
    if (buffer == NULL)
        return NULL;
    buffer[0] = '0';
    buffer[1] = '\0';
    return buffer;
}

char *gpc_gmp_z_get_str(uint64_t base, void **slot_ptr)
{
    if (slot_ptr == NULL)
        return gpc_gmp_alloc_zero_string();

    gpc_gmp_mpint_t *value = (gpc_gmp_mpint_t *)(*slot_ptr);
    if (value == NULL)
        return gpc_gmp_alloc_zero_string();

    char *text = mpz_get_str(NULL, (int)base, value->value);
    if (text == NULL)
        return gpc_gmp_alloc_zero_string();
    return text;
}

void gpc_gmp_mpint_assign(void **dest_slot, void *src_ptr)
{
    if (dest_slot == NULL)
        return;

    gpc_gmp_mpint_t *src = (gpc_gmp_mpint_t *)src_ptr;
    if (src != NULL)
        gpc_gmp_mpint_acquire(src);

    gpc_gmp_mpint_t *old = (gpc_gmp_mpint_t *)(*dest_slot);
    if (old != NULL)
        gpc_gmp_mpint_release(old);

    *dest_slot = src;
}
