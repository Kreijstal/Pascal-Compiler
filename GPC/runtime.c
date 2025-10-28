#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <time.h>
#include <errno.h>
#include <unistd.h>
#endif

static int gpc_vprintf_impl(const char *format, va_list args) {
    return vprintf(format, args);
}

int gpc_printf(const char *format, ...) {
    va_list args;
    va_start(args, format);
    int result = gpc_vprintf_impl(format, args);
    va_end(args);
    return result;
}

static int gpc_vscanf_impl(const char *format, va_list args) {
#ifdef _WIN32
    return vscanf(format, args);
#else
    return vscanf(format, args);
#endif
}

int gpc_scanf(const char *format, ...) {
    va_list args;
    va_start(args, format);
    int result = gpc_vscanf_impl(format, args);
    va_end(args);
    return result;
}

#ifdef _WIN32
int __isoc99_scanf(const char *format, ...) {
    va_list args;
    va_start(args, format);
    int result = gpc_vscanf_impl(format, args);
    va_end(args);
    return result;
}
#endif

void print_integer(int n) {
    printf("%d\n", n);
}

uint64_t gpc_get_tick_count64(void) {
#ifdef _WIN32
    return (uint64_t) GetTickCount64();
#else
    struct timespec ts;
    if (clock_gettime(CLOCK_MONOTONIC, &ts) != 0) {
        return 0;
    }
    return (uint64_t)ts.tv_sec * 1000ULL + (uint64_t)(ts.tv_nsec / 1000000ULL);
#endif
}

void gpc_sleep_ms(int milliseconds) {
    if (milliseconds <= 0)
        return;

#ifdef _WIN32
    Sleep((DWORD)milliseconds);
#else
    struct timespec req;
    req.tv_sec = milliseconds / 1000;
    req.tv_nsec = (long)(milliseconds % 1000) * 1000000L;

    while (nanosleep(&req, &req) == -1 && errno == EINTR) {
        /* Retry until sleep completes */
    }
#endif
}

typedef struct {
    void *data;
    int64_t length;
} gpc_dynarray_descriptor_t;

void gpc_dynarray_setlength(void *descriptor_ptr, int64_t new_length, int64_t element_size)
{
    if (descriptor_ptr == NULL || element_size <= 0)
        return;

    if (new_length < 0)
        new_length = 0;

    gpc_dynarray_descriptor_t *descriptor = (gpc_dynarray_descriptor_t *)descriptor_ptr;
    size_t old_length = descriptor->length > 0 ? (size_t)descriptor->length : 0;
    size_t target_length = (size_t)new_length;

    size_t alloc_length = target_length;
    if (alloc_length < SIZE_MAX)
        alloc_length += 1; /* Provide a spare slot to tolerate off-by-one accesses. */

    size_t new_size = alloc_length * (size_t)element_size;
    if (new_size == 0)
    {
        free(descriptor->data);
        descriptor->data = NULL;
        descriptor->length = 0;
        return;
    }

    void *new_data = realloc(descriptor->data, new_size);
    if (new_data == NULL)
    {
        /* Allocation failure leaves the array unchanged. */
        return;
    }

    if (target_length > old_length)
    {
        size_t old_bytes = old_length * (size_t)element_size;
        size_t new_bytes = target_length * (size_t)element_size;
        if (new_bytes > old_bytes)
            memset((char *)new_data + old_bytes, 0, new_bytes - old_bytes);
    }

    descriptor->data = new_data;
    descriptor->length = new_length;
}

void gpc_write_integer(int width, int64_t value)
{
    if (width > 0)
        printf("%*lld", width, (long long)value);
    else
        printf("%lld", (long long)value);
}

void gpc_write_string(int width, const char *value)
{
    if (value == NULL)
        value = "";
    if (width > 0)
        printf("%*s", width, value);
    else
        printf("%s", value);
}

void gpc_write_newline(void)
{
    putchar('\n');
}

void gpc_write_boolean(int width, int value)
{
    const char *text = value ? "TRUE" : "FALSE";
    if (width > 0)
        printf("%*s", width, text);
    else
        printf("%s", text);
}

static char *gpc_alloc_empty_string(void)
{
    char *empty = (char *)malloc(1);
    if (empty != NULL)
        empty[0] = '\0';
    return empty;
}

void gpc_move(void *dest, const void *src, size_t count)
{
    if (dest == NULL || src == NULL || count == 0)
        return;

    memmove(dest, src, count);
}

char *gpc_string_concat(const char *lhs, const char *rhs)
{
    if (lhs == NULL)
        lhs = "";
    if (rhs == NULL)
        rhs = "";

    size_t lhs_len = strlen(lhs);
    size_t rhs_len = strlen(rhs);
    size_t total = lhs_len + rhs_len;

    char *result = (char *)malloc(total + 1);
    if (result == NULL)
        return gpc_alloc_empty_string();

    if (lhs_len > 0)
        memcpy(result, lhs, lhs_len);
    if (rhs_len > 0)
        memcpy(result + lhs_len, rhs, rhs_len);
    result[total] = '\0';
    return result;
}

char *gpc_chr(int64_t value)
{
    if (value < 0)
        value = 0;
    else if (value > 255)
        value = 255;

    char *result = (char *)malloc(2);
    if (result == NULL)
        return gpc_alloc_empty_string();

    result[0] = (char)(value & 0xFF);
    result[1] = '\0';
    return result;
}

int64_t gpc_ord_string(const char *value)
{
    if (value == NULL || value[0] == '\0')
        return 0;

    return (unsigned char)value[0];
}

int64_t gpc_ord_longint(int64_t value)
{
    return value;
}

typedef struct
{
    uint32_t *digits;
    size_t length;
    size_t capacity;
} gpc_mpz_object_t;

#define GPC_MPZ_BASE 1000000000U
#define GPC_MPZ_BASE_DIGITS 9

static gpc_mpz_object_t *gpc_mpz_from_handle(int64_t handle)
{
    if (handle == 0)
        return NULL;

    return (gpc_mpz_object_t *)(uintptr_t)handle;
}

static int64_t gpc_mpz_to_handle(gpc_mpz_object_t *object)
{
    return (int64_t)(uintptr_t)object;
}

static gpc_mpz_object_t *gpc_mpz_allocate(void)
{
    gpc_mpz_object_t *object = (gpc_mpz_object_t *)malloc(sizeof(gpc_mpz_object_t));
    if (object == NULL)
    {
        fprintf(stderr, "Error: failed to allocate MPInteger.\n");
        return NULL;
    }

    object->digits = NULL;
    object->length = 0;
    object->capacity = 0;
    return object;
}

static void gpc_mpz_release(gpc_mpz_object_t *object)
{
    if (object == NULL)
        return;

    free(object->digits);
    free(object);
}

static int gpc_mpz_reserve(gpc_mpz_object_t *object, size_t capacity)
{
    if (object == NULL)
        return -1;

    if (object->capacity >= capacity)
        return 0;

    size_t new_capacity = object->capacity > 0 ? object->capacity : 1;
    while (new_capacity < capacity)
        new_capacity *= 2;

    uint32_t *new_digits = (uint32_t *)realloc(object->digits, new_capacity * sizeof(uint32_t));
    if (new_digits == NULL)
    {
        fprintf(stderr, "Error: failed to allocate MPInteger digits.\n");
        return -1;
    }

    for (size_t i = object->capacity; i < new_capacity; ++i)
        new_digits[i] = 0;

    object->digits = new_digits;
    object->capacity = new_capacity;
    return 0;
}

static void gpc_mpz_set_zero(gpc_mpz_object_t *object)
{
    if (object == NULL)
        return;

    object->length = 0;
    if (object->digits != NULL && object->capacity > 0)
        object->digits[0] = 0;
}

static void gpc_mpz_normalize(gpc_mpz_object_t *object)
{
    if (object == NULL)
        return;

    while (object->length > 0 && object->digits[object->length - 1] == 0)
        object->length--;
}

static gpc_mpz_object_t *gpc_mpz_require(int64_t *handle_ptr)
{
    if (handle_ptr == NULL)
        return NULL;

    gpc_mpz_object_t *object = gpc_mpz_from_handle(*handle_ptr);
    if (object == NULL)
    {
        object = gpc_mpz_allocate();
        if (object == NULL)
            return NULL;
        *handle_ptr = gpc_mpz_to_handle(object);
    }

    return object;
}

void gpc_mpz_init(int64_t *handle_ptr)
{
    gpc_mpz_object_t *object = gpc_mpz_require(handle_ptr);
    if (object == NULL)
        return;

    gpc_mpz_set_zero(object);
}

void gpc_mpz_clear(int64_t *handle_ptr)
{
    if (handle_ptr == NULL)
        return;

    gpc_mpz_object_t *object = gpc_mpz_from_handle(*handle_ptr);
    if (object != NULL)
    {
        gpc_mpz_release(object);
        *handle_ptr = 0;
    }
}

void gpc_mpz_set(int64_t *dest_handle, int64_t src_handle)
{
    gpc_mpz_object_t *dest = gpc_mpz_require(dest_handle);
    if (dest == NULL)
        return;

    gpc_mpz_object_t *src = gpc_mpz_from_handle(src_handle);
    if (src == NULL || src->length == 0)
    {
        gpc_mpz_set_zero(dest);
        return;
    }

    if (gpc_mpz_reserve(dest, src->length) != 0)
        return;

    memcpy(dest->digits, src->digits, src->length * sizeof(uint32_t));
    dest->length = src->length;
}

void gpc_mpz_set_ui(int64_t *dest_handle, uint64_t value)
{
    gpc_mpz_object_t *dest = gpc_mpz_require(dest_handle);
    if (dest == NULL)
        return;

    gpc_mpz_set_zero(dest);
    if (value == 0)
        return;

    uint64_t remaining = value;
    size_t needed = 0;
    while (remaining > 0)
    {
        ++needed;
        remaining /= GPC_MPZ_BASE;
    }

    if (gpc_mpz_reserve(dest, needed) != 0)
        return;

    dest->length = needed;
    remaining = value;
    for (size_t i = 0; i < needed; ++i)
    {
        dest->digits[i] = (uint32_t)(remaining % GPC_MPZ_BASE);
        remaining /= GPC_MPZ_BASE;
    }
}

void gpc_mpz_add_ui(int64_t *dest_handle, uint64_t addend)
{
    gpc_mpz_object_t *dest = gpc_mpz_require(dest_handle);
    if (dest == NULL)
        return;

    size_t index = 0;
    uint64_t carry = addend;
    while (carry > 0 || index < dest->length)
    {
        if (index >= dest->length)
        {
            if (gpc_mpz_reserve(dest, index + 1) != 0)
                return;
            dest->digits[index] = 0;
            dest->length = index + 1;
        }

        uint64_t sum = (uint64_t)dest->digits[index] + carry;
        dest->digits[index] = (uint32_t)(sum % GPC_MPZ_BASE);
        carry = sum / GPC_MPZ_BASE;
        ++index;
    }

    if (carry > 0)
    {
        if (gpc_mpz_reserve(dest, dest->length + 1) != 0)
            return;
        dest->digits[dest->length++] = (uint32_t)carry;
    }
}

static void gpc_mpz_mul_small(gpc_mpz_object_t *object, uint64_t factor)
{
    if (object == NULL)
        return;

    if (object->length == 0 || factor == 0)
    {
        gpc_mpz_set_zero(object);
        if (factor == 0 && object->digits != NULL && object->capacity > 0)
            object->digits[0] = 0;
        return;
    }

    if (gpc_mpz_reserve(object, object->length + 1) != 0)
        return;

    uint64_t carry = 0;
    for (size_t i = 0; i < object->length; ++i)
    {
        uint64_t product = (uint64_t)object->digits[i] * factor + carry;
        object->digits[i] = (uint32_t)(product % GPC_MPZ_BASE);
        carry = product / GPC_MPZ_BASE;
    }

    if (carry > 0)
    {
        object->digits[object->length++] = (uint32_t)carry;
    }
}

int64_t gpc_mpz_add(int64_t lhs_handle, int64_t rhs_handle)
{
    gpc_mpz_object_t *lhs = gpc_mpz_from_handle(lhs_handle);
    if (lhs == NULL)
    {
        lhs = gpc_mpz_allocate();
        if (lhs == NULL)
            return 0;
        lhs_handle = gpc_mpz_to_handle(lhs);
    }

    gpc_mpz_object_t *rhs = gpc_mpz_from_handle(rhs_handle);
    if (rhs == NULL || rhs->length == 0)
        return gpc_mpz_to_handle(lhs);

    size_t max_len = lhs->length > rhs->length ? lhs->length : rhs->length;
    if (gpc_mpz_reserve(lhs, max_len + 1) != 0)
        return gpc_mpz_to_handle(lhs);

    if (lhs->length < max_len)
    {
        for (size_t i = lhs->length; i < max_len; ++i)
            lhs->digits[i] = 0;
        lhs->length = max_len;
    }

    uint64_t carry = 0;
    for (size_t i = 0; i < max_len; ++i)
    {
        uint64_t sum = (uint64_t)lhs->digits[i] + carry;
        if (i < rhs->length)
            sum += rhs->digits[i];
        lhs->digits[i] = (uint32_t)(sum % GPC_MPZ_BASE);
        carry = sum / GPC_MPZ_BASE;
    }

    if (carry > 0)
        lhs->digits[lhs->length++] = (uint32_t)carry;

    return gpc_mpz_to_handle(lhs);
}

void gpc_mpz_ui_pow_ui(int64_t *dest_handle, uint64_t base, uint64_t exp)
{
    if (dest_handle == NULL)
        return;

    gpc_mpz_set_ui(dest_handle, 1);
    gpc_mpz_object_t *dest = gpc_mpz_from_handle(*dest_handle);
    if (dest == NULL)
        return;

    if (exp == 0)
        return;

    if (base == 0)
    {
        gpc_mpz_set_zero(dest);
        return;
    }

    for (uint64_t i = 0; i < exp; ++i)
        gpc_mpz_mul_small(dest, base);
}

uint64_t gpc_mpz_size(int64_t handle)
{
    gpc_mpz_object_t *object = gpc_mpz_from_handle(handle);
    if (object == NULL)
        return 0;

    return (uint64_t)object->length;
}

static size_t gpc_mpz_decimal_digits(uint32_t value)
{
    size_t digits = 1;
    while (value >= 10)
    {
        value /= 10;
        ++digits;
    }
    return digits;
}

char *gpc_mpz_get_str(int base, int64_t handle)
{
    if (base < 2 || base > 36)
        base = 10;

    gpc_mpz_object_t *object = gpc_mpz_from_handle(handle);
    if (object == NULL || object->length == 0)
    {
        char *zero = (char *)malloc(2);
        if (zero == NULL)
            return gpc_alloc_empty_string();
        zero[0] = '0';
        zero[1] = '\0';
        return zero;
    }

    if (base != 10)
    {
        /* Fallback: unsupported base, return empty string */
        return gpc_alloc_empty_string();
    }

    size_t last_index = object->length - 1;
    uint32_t most_significant = object->digits[last_index];
    size_t total_digits = gpc_mpz_decimal_digits(most_significant) + last_index * GPC_MPZ_BASE_DIGITS;

    char *text = (char *)malloc(total_digits + 1);
    if (text == NULL)
        return gpc_alloc_empty_string();

    size_t pos = 0;
    int written = snprintf(text + pos, total_digits + 1 - pos, "%u", most_significant);
    if (written < 0)
    {
        free(text);
        return gpc_alloc_empty_string();
    }
    pos += (size_t)written;

    while (last_index-- > 0)
    {
        written = snprintf(text + pos, total_digits + 1 - pos, "%09u", object->digits[last_index]);
        if (written < 0)
        {
            free(text);
            return gpc_alloc_empty_string();
        }
        pos += (size_t)written;
    }

    text[total_digits] = '\0';
    return text;
}
