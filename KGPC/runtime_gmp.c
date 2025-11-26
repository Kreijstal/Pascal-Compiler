#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <gmp.h>

typedef struct kgpc_gmp_mpint
{
    mpz_t value;
    size_t refcount;
} kgpc_gmp_mpint_t;

static kgpc_gmp_mpint_t *kgpc_gmp_mpint_alloc(void)
{
    kgpc_gmp_mpint_t *mp = (kgpc_gmp_mpint_t *)malloc(sizeof(kgpc_gmp_mpint_t));
    if (mp == NULL)
        return NULL;
    mpz_init(mp->value);
    mp->refcount = 1;
    return mp;
}

static void kgpc_gmp_mpint_free(kgpc_gmp_mpint_t *mp)
{
    if (mp == NULL)
        return;
    mpz_clear(mp->value);
    free(mp);
}

static void kgpc_gmp_mpint_acquire(kgpc_gmp_mpint_t *mp)
{
    if (mp == NULL)
        return;
    mp->refcount += 1;
}

static void kgpc_gmp_mpint_release(kgpc_gmp_mpint_t *mp)
{
    if (mp == NULL)
        return;
    if (mp->refcount > 1)
    {
        mp->refcount -= 1;
        return;
    }
    kgpc_gmp_mpint_free(mp);
}

static kgpc_gmp_mpint_t *kgpc_gmp_mpint_ensure_unique(void **slot_ptr)
{
    if (slot_ptr == NULL)
        return NULL;

    kgpc_gmp_mpint_t *value = (kgpc_gmp_mpint_t *)(*slot_ptr);
    if (value == NULL)
    {
        value = kgpc_gmp_mpint_alloc();
        if (value == NULL)
            return NULL;
        *slot_ptr = value;
        return value;
    }

    if (value->refcount > 1)
    {
        kgpc_gmp_mpint_t *copy = kgpc_gmp_mpint_alloc();
        if (copy == NULL)
            return NULL;
        mpz_set(copy->value, value->value);
        value->refcount -= 1;
        *slot_ptr = copy;
        value = copy;
    }

    return value;
}

void kgpc_gmp_z_init(void **slot_ptr)
{
    if (slot_ptr == NULL)
        return;

    kgpc_gmp_mpint_t *current = (kgpc_gmp_mpint_t *)(*slot_ptr);
    if (current != NULL)
        kgpc_gmp_mpint_release(current);

    kgpc_gmp_mpint_t *fresh = kgpc_gmp_mpint_alloc();
    if (fresh == NULL)
    {
        *slot_ptr = NULL;
        return;
    }

    mpz_set_ui(fresh->value, 0);
    *slot_ptr = fresh;
}

void kgpc_gmp_z_clear(void **slot_ptr)
{
    if (slot_ptr == NULL)
        return;

    kgpc_gmp_mpint_t *value = (kgpc_gmp_mpint_t *)(*slot_ptr);
    if (value != NULL)
        kgpc_gmp_mpint_release(value);
    *slot_ptr = NULL;
}

void kgpc_gmp_z_set_ui(void **slot_ptr, uint64_t operand)
{
    kgpc_gmp_mpint_t *value = kgpc_gmp_mpint_ensure_unique(slot_ptr);
    if (value == NULL)
        return;

    mpz_set_ui(value->value, operand);
}

void kgpc_gmp_z_add_ui(void **slot_ptr, uint64_t operand)
{
    kgpc_gmp_mpint_t *value = kgpc_gmp_mpint_ensure_unique(slot_ptr);
    if (value == NULL)
        return;

    mpz_add_ui(value->value, value->value, operand);
}

void kgpc_gmp_z_ui_pow_ui(void **slot_ptr, uint64_t base, uint64_t exponent)
{
    kgpc_gmp_mpint_t *value = kgpc_gmp_mpint_ensure_unique(slot_ptr);
    if (value == NULL)
        return;

    mpz_ui_pow_ui(value->value, base, exponent);
}

uint64_t kgpc_gmp_z_size(void **slot_ptr)
{
    if (slot_ptr == NULL)
        return 0;

    kgpc_gmp_mpint_t *value = (kgpc_gmp_mpint_t *)(*slot_ptr);
    if (value == NULL)
        return 0;

    size_t limbs = mpz_size(value->value);
    return (uint64_t)limbs;
}

void kgpc_gmp_z_copy(void **dest_slot, void **src_slot)
{
    if (dest_slot == NULL)
        return;

    kgpc_gmp_mpint_t *src = (src_slot != NULL) ? (kgpc_gmp_mpint_t *)(*src_slot) : NULL;
    kgpc_gmp_mpint_t *dest = kgpc_gmp_mpint_ensure_unique(dest_slot);
    if (dest == NULL)
        return;

    if (src == NULL)
    {
        mpz_set_ui(dest->value, 0);
        return;
    }

    mpz_set(dest->value, src->value);
}

void *kgpc_gmp_z_add(void **target_slot, void **operand_slot)
{
    kgpc_gmp_mpint_t *target = kgpc_gmp_mpint_ensure_unique(target_slot);
    if (target == NULL)
        return NULL;

    kgpc_gmp_mpint_t *operand = (operand_slot != NULL) ? (kgpc_gmp_mpint_t *)(*operand_slot) : NULL;
    if (operand == NULL)
        return target;

    mpz_add(target->value, target->value, operand->value);
    return target;
}

static char *kgpc_gmp_alloc_zero_string(void)
{
    char *buffer = (char *)malloc(2);
    if (buffer == NULL)
        return NULL;
    buffer[0] = '0';
    buffer[1] = '\0';
    return buffer;
}

char *kgpc_gmp_z_get_str(uint64_t base, void **slot_ptr)
{
    if (slot_ptr == NULL)
        return kgpc_gmp_alloc_zero_string();

    kgpc_gmp_mpint_t *value = (kgpc_gmp_mpint_t *)(*slot_ptr);
    if (value == NULL)
        return kgpc_gmp_alloc_zero_string();

    char *text = mpz_get_str(NULL, (int)base, value->value);
    if (text == NULL)
        return kgpc_gmp_alloc_zero_string();
    return text;
}

void kgpc_gmp_mpint_assign(void **dest_slot, void *src_ptr)
{
    if (dest_slot == NULL)
        return;

    kgpc_gmp_mpint_t *src = (kgpc_gmp_mpint_t *)src_ptr;
    if (src != NULL)
        kgpc_gmp_mpint_acquire(src);

    kgpc_gmp_mpint_t *old = (kgpc_gmp_mpint_t *)(*dest_slot);
    if (old != NULL)
        kgpc_gmp_mpint_release(old);

    *dest_slot = src;
}
