#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <math.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifndef _WIN32
#include <sys/mman.h>
#endif
#ifdef _WIN32
#include <conio.h>
#include <direct.h>
#include <fcntl.h>
#include <io.h>
#include <time.h>
#include <windows.h>
#else
#include <netdb.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/utsname.h>
#include <unistd.h>
#endif
#include "format_arg.h"
#include "runtime_internal.h"
#include <limits.h>

static void *const KGPC_STRING_TOMBSTONE = (void *)1;
static void **kgpc_string_set_slots = NULL;
static size_t kgpc_string_set_cap = 0;
static size_t kgpc_string_set_count = 0;
static int kgpc_string_cleanup_registered = 0;
static int kgpc_argc = 0;
static char **kgpc_argv = NULL;

static size_t kgpc_hash_ptr(const void *value) {
  uintptr_t v = (uintptr_t)value;
  v ^= v >> 33;
  v *= UINT64_C(0xff51afd7ed558ccd);
  v ^= v >> 33;
  v *= UINT64_C(0xc4ceb9fe1a85ec53);
  v ^= v >> 33;
  return (size_t)v;
}

static void kgpc_string_set_grow(size_t new_cap) {
  void **old_slots = kgpc_string_set_slots;
  size_t old_cap = kgpc_string_set_cap;

  kgpc_string_set_slots = (void **)calloc(new_cap, sizeof(void *));
  if (kgpc_string_set_slots == NULL) {
    kgpc_string_set_slots = old_slots;
    return;
  }
  kgpc_string_set_cap = new_cap;
  kgpc_string_set_count = 0;

  if (old_slots != NULL) {
    size_t mask = new_cap - 1;
    for (size_t i = 0; i < old_cap; i++) {
      void *entry = old_slots[i];
      if (entry == NULL || entry == KGPC_STRING_TOMBSTONE)
        continue;
      size_t idx = kgpc_hash_ptr(entry) & mask;
      while (kgpc_string_set_slots[idx] != NULL)
        idx = (idx + 1) & mask;
      kgpc_string_set_slots[idx] = entry;
      kgpc_string_set_count += 1;
    }
    free(old_slots);
  }
}

static void kgpc_string_set_cleanup(void) {
  if (kgpc_string_set_slots == NULL)
    return;

  for (size_t i = 0; i < kgpc_string_set_cap; i++) {
    void *entry = kgpc_string_set_slots[i];
    if (entry == NULL || entry == KGPC_STRING_TOMBSTONE)
      continue;
    KgpcStringHeader *hdr =
        (KgpcStringHeader *)((char *)entry - (int64_t)sizeof(KgpcStringHeader));
    free(hdr);
    kgpc_string_set_slots[i] = NULL;
  }

  free(kgpc_string_set_slots);
  kgpc_string_set_slots = NULL;
  kgpc_string_set_cap = 0;
  kgpc_string_set_count = 0;
  kgpc_string_cleanup_registered = 0;
}

void kgpc_string_set_insert(const void *value) {
  if (value == NULL || value == KGPC_STRING_TOMBSTONE)
    return;
  if (!kgpc_string_cleanup_registered) {
    if (atexit(kgpc_string_set_cleanup) == 0)
      kgpc_string_cleanup_registered = 1;
  }
  for (;;) {
    if (kgpc_string_set_cap == 0)
      kgpc_string_set_grow(1024);
    if (kgpc_string_set_cap == 0)
      return;
    if ((kgpc_string_set_count + 1) * 10 >= kgpc_string_set_cap * 7) {
      size_t pre_grow_cap = kgpc_string_set_cap;
      kgpc_string_set_grow(kgpc_string_set_cap * 2);
      /* If grow failed, cap is unchanged and the table is still too full.
       * Proceeding would risk a probe-loop infinite-loop below; bail out. */
      if (kgpc_string_set_cap == pre_grow_cap)
        return;
    }

    size_t mask = kgpc_string_set_cap - 1;
    size_t idx = kgpc_hash_ptr(value) & mask;
    size_t first_tombstone = (size_t)-1;
    for (size_t probe = 0; probe < kgpc_string_set_cap; probe++) {
      void *entry = kgpc_string_set_slots[idx];
      if (entry == NULL) {
        if (first_tombstone != (size_t)-1)
          idx = first_tombstone;
        kgpc_string_set_slots[idx] = (void *)value;
        kgpc_string_set_count += 1;
        return;
      }
      if (entry == value)
        return;
      if (entry == KGPC_STRING_TOMBSTONE && first_tombstone == (size_t)-1)
        first_tombstone = idx;
      idx = (idx + 1) & mask;
    }

    if (first_tombstone != (size_t)-1) {
      kgpc_string_set_slots[first_tombstone] = (void *)value;
      kgpc_string_set_count += 1;
      return;
    }

    /* Table is completely full (no NULL or tombstone slots).
     * Try to grow; if grow fails the capacity is unchanged, so
     * a second attempt would loop forever — bail out instead. */
    size_t old_cap = kgpc_string_set_cap;
    kgpc_string_set_grow(kgpc_string_set_cap * 2);
    if (kgpc_string_set_cap == old_cap)
      return; /* grow failed: drop the insert rather than loop */
  }
}

static void kgpc_string_set_remove(const void *value) {
  if (value == NULL || kgpc_string_set_cap == 0)
    return;
  size_t mask = kgpc_string_set_cap - 1;
  size_t idx = kgpc_hash_ptr(value) & mask;
  for (size_t probe = 0; probe < kgpc_string_set_cap; probe++) {
    void *entry = kgpc_string_set_slots[idx];
    if (entry == NULL)
      return;
    if (entry == value) {
      kgpc_string_set_slots[idx] = KGPC_STRING_TOMBSTONE;
      if (kgpc_string_set_count > 0)
        kgpc_string_set_count -= 1;
      if (kgpc_string_set_count == 0)
        memset(kgpc_string_set_slots, 0, kgpc_string_set_cap * sizeof(void *));
      return;
    }
    idx = (idx + 1) & mask;
  }
}

int kgpc_string_is_managed(const char *value) {
  if (value == NULL)
    return 0;
  if (value == kgpc_alloc_empty_string())
    return 1;
  if (kgpc_string_set_cap == 0)
    return 0;
  size_t mask = kgpc_string_set_cap - 1;
  size_t idx = kgpc_hash_ptr(value) & mask;
  for (size_t probe = 0; probe < kgpc_string_set_cap; probe++) {
    void *entry = kgpc_string_set_slots[idx];
    if (entry == NULL)
      return 0;
    if (entry == value)
      return 1;
    idx = (idx + 1) & mask;
  }
  return 0;
}

KgpcStringHeader *kgpc_string_header(const char *value) {
  if (!kgpc_string_is_managed(value))
    return NULL;
  return (KgpcStringHeader *)((char *)value -
                              (int64_t)sizeof(KgpcStringHeader));
}

static int kgpc_string_try_known_length(const char *value, size_t *out_length) {
  if (value == NULL || out_length == NULL)
    return 0;
  if (!kgpc_string_is_managed(value))
    return 0;

  KgpcStringHeader hdr;
  memcpy(&hdr, value - (ptrdiff_t)sizeof(hdr), sizeof(hdr));
  *out_length = (size_t)hdr.length;
  return 1;
}

size_t kgpc_string_known_length(const char *value) {
  if (value == NULL)
    return 0;
  size_t managed_len = 0;
  if (kgpc_string_try_known_length(value, &managed_len))
    return managed_len;
  return strlen(value);
}

void kgpc_string_assign_take(char **target, char *value);

/* Robust SetCodePage wrapper: accepts either a var RawByteString (by-ref)
 * or a raw string pointer (by-value) to avoid crashes when call sites
 * accidentally pass the value. */
void kgpc_setcodepage_rbs_i_b(void *s_arg, int32_t codepage, int32_t convert) {
  if (s_arg == NULL)
    return;

  char *str = NULL;
  int by_value = 0;

  /* If s_arg itself looks like a managed string, treat it as by-value. */
  if (kgpc_string_is_managed((char *)s_arg)) {
    str = (char *)s_arg;
    by_value = 1;
  } else {
    char **s_ptr = (char **)s_arg;
    str = (s_ptr != NULL) ? *s_ptr : NULL;
  }

  if (str == NULL)
    return;
  if (kgpc_string_known_length(str) == 0)
    return;

  if (!convert || by_value) {
    KgpcStringHeader *hdr = kgpc_string_header(str);
    if (hdr != NULL)
      hdr->codepage = (uint16_t)codepage;
    return;
  }

  /* Portable conversion path (no iconv): keep bytes unchanged and
   * update codepage metadata. This matches FPC behavior for raw
   * byte strings when no concrete conversion backend is linked. */
  KgpcStringHeader *hdr = kgpc_string_header(str);
  if (hdr != NULL)
    hdr->codepage = (uint16_t)codepage;
}

/* 2-arg SetCodePage overload: Convert defaults to True in FPC. */
void kgpc_setcodepage_rbs_i(void *s_arg, int32_t codepage) {
  kgpc_setcodepage_rbs_i_b(s_arg, codepage, 1);
}

/* FPC RTL compatibility: some bootstrap constants use WideChar literals
 * in PAnsiChar contexts, which KGPC currently lowers via the
 * widechar__op_assign_olevariant_wc symbol. Provide a real conversion
 * by returning a stable, null-terminated single-byte string. */
static char *kgpc_cached_widechar_pchar(uint16_t value) {
  static char *cache[256] = {0};
  static char *out_of_range_buf = NULL;

  if (value < 256) {
    if (cache[value] == NULL) {
      char *buf = (char *)malloc(2);
      if (buf == NULL)
        return kgpc_alloc_empty_string();
      buf[0] = (char)value;
      buf[1] = '\0';
      cache[value] = buf;
    }
    return cache[value];
  }

  if (out_of_range_buf == NULL) {
    out_of_range_buf = (char *)malloc(2);
    if (out_of_range_buf == NULL)
      return kgpc_alloc_empty_string();
    out_of_range_buf[0] = '?';
    out_of_range_buf[1] = '\0';
  }
  return out_of_range_buf;
}

char *widechar__op_assign_olevariant_wc(uint16_t value) {
  return kgpc_cached_widechar_pchar(value);
}

/* olevariant__op_assign_widestring_u is in runtime_olevariant_assign.c
 * (separate .o so linker only pulls it when not defined by compiler code) */

char *kgpc_string_alloc_with_length(size_t length) {
  size_t total = sizeof(KgpcStringHeader) + length + 1;
  KgpcStringHeader *hdr = (KgpcStringHeader *)malloc(total);
  if (hdr == NULL)
    return NULL;
  hdr->codepage = 0;    /* CP_ACP / default */
  hdr->elementsize = 1; /* AnsiChar = 1 byte */
  hdr->refcount = 1;
  hdr->length = (int64_t)length;
  char *data = (char *)(hdr + 1);
  data[length] = '\0';
  kgpc_string_set_insert(data);
  return data;
}

static void kgpc_string_retain(const char *value) {
  KgpcStringHeader *hdr = kgpc_string_header(value);
  if (hdr == NULL)
    return;
  if (hdr->refcount >= 0)
    hdr->refcount += 1;
}

void kgpc_string_release(char *value) {
  KgpcStringHeader *hdr = kgpc_string_header(value);
  if (hdr == NULL)
    return;
  if (hdr->refcount < 0)
    return;
  hdr->refcount -= 1;
  if (hdr->refcount == 0) {
    kgpc_string_set_remove(value);
    free(hdr);
  }
}

void FPC_ANSISTR_UNIQUE(char **value) {
  if (value == NULL || *value == NULL)
    return;
  KgpcStringHeader *hdr = kgpc_string_header(*value);
  if (hdr == NULL)
    return;
  if (hdr->refcount == 1 || hdr->refcount < 0)
    return;
  size_t len = (size_t)hdr->length;
  char *new_str = (char *)malloc(sizeof(KgpcStringHeader) + len + 1);
  if (new_str == NULL)
    return;
  KgpcStringHeader *new_hdr = (KgpcStringHeader *)new_str;
  new_hdr->codepage = hdr->codepage;
  new_hdr->elementsize = hdr->elementsize;
  new_hdr->refcount = 1;
  new_hdr->length = len;
  char *new_data = new_str + sizeof(KgpcStringHeader);
  memcpy(new_data, *value, len + 1);
  kgpc_string_set_insert(new_data);
  kgpc_string_release(*value);
  *value = new_data;
}

char *kgpc_alloc_empty_string(void) {
  static struct {
    KgpcStringHeader header;
    char data[1];
  } empty = {{0, 1, -1, 0}, {'\0'}};
  return empty.data;
}

void kgpc_init_widestringmanager(void);

void kgpc_fpc_init_os_params(int argc, char **argv, char **envp);
void kgpc_fpc_init_stack_params(void *stack_probe);
void kgpc_fpc_init_thread_manager(void);
void kgpc_fpc_init_fpu(void);

void kgpc_init_args(int argc, char **argv, char **envp) {
  kgpc_argc = (argc < 0) ? 0 : argc;
  kgpc_argv = argv;
  /* Ensure stdio pointers (stdin_ptr, stdout_ptr, stderr_ptr) are
   * initialized before any Pascal code runs.  The __attribute__((constructor))
   * in kgpc_init_stdio_constructor normally handles this, but on some
   * platforms (e.g. MSYS/Cygwin with -static linking) the constructor may
   * execute before the C library's stdio is fully set up, leaving
   * stderr_ptr pointing to an invalid FILE*.  Re-init unconditionally
   * from main() where stdio is guaranteed valid. */
  kgpc_reinit_stdio();
  kgpc_fpc_init_os_params(argc, argv, envp);
  kgpc_fpc_init_stack_params(&argc);
  kgpc_fpc_init_thread_manager();
  kgpc_init_widestringmanager();
  kgpc_fpc_init_fpu();
  /* Note: FPC heap init (initthread_u64) is NOT called here.
   * The FPC system unit's own initialization section calls it
   * before any Pascal code that needs heap allocation runs. */
}

int kgpc_param_count(void) {
  if (kgpc_argc <= 1)
    return 0;
  return kgpc_argc - 1;
}

char *kgpc_param_str(int index) {
  if (index < 0 || index >= kgpc_argc || kgpc_argv == NULL)
    return kgpc_alloc_empty_string();
  return kgpc_string_duplicate(kgpc_argv[index]);
}

char *kgpc_string_duplicate(const char *value) {
  if (value == NULL)
    return kgpc_alloc_empty_string();

  KgpcStringHeader *hdr = kgpc_string_header(value);
  if (hdr != NULL) {
    kgpc_string_retain(value);
    return (char *)value;
  }

  size_t len = strlen(value);
  char *copy = kgpc_string_alloc_with_length(len);
  if (copy == NULL)
    return kgpc_alloc_empty_string();
  if (len > 0)
    memcpy(copy, value, len);
  return copy;
}

static char *kgpc_string_duplicate_length(const char *value, size_t length) {
  char *copy = kgpc_string_alloc_with_length(length);
  if (copy == NULL)
    return kgpc_alloc_empty_string();
  if (length > 0 && value != NULL)
    memcpy(copy, value, length);
  return copy;
}

char *kgpc_windows_get_hostname_string(void) {
#ifdef _WIN32
  char buffer[256];
  DWORD size = (DWORD)sizeof(buffer);
  if (GetComputerNameA(buffer, &size)) {
    buffer[sizeof(buffer) - 1] = '\0';
    /* Convert to lowercase for consistency with Unix behavior */
    for (size_t i = 0; buffer[i] != '\0'; i++) {
      buffer[i] = (char)tolower((unsigned char)buffer[i]);
    }
    return kgpc_string_duplicate(buffer);
  }
  return kgpc_alloc_empty_string();
#else
  return kgpc_alloc_empty_string();
#endif
}

char *kgpc_windows_get_domainname_string(void) {
#ifdef _WIN32
  char fqdn[256];
  DWORD size = sizeof(fqdn);
  /* Try to get DNS domain name on Windows */
  if (GetComputerNameExA(ComputerNameDnsFullyQualified, fqdn, &size)) {
    char *dot = strchr(fqdn, '.');
    if (dot != NULL && *(dot + 1) != '\0') {
      return kgpc_string_duplicate(dot + 1);
    }
  }
  /* Alternative: try DNS hostname */
  size = sizeof(fqdn);
  if (GetComputerNameExA(ComputerNameDnsHostname, fqdn, &size)) {
    char *dot = strchr(fqdn, '.');
    if (dot != NULL && *(dot + 1) != '\0') {
      return kgpc_string_duplicate(dot + 1);
    }
  }
  return kgpc_alloc_empty_string();
#else
  return kgpc_alloc_empty_string();
#endif
}

void kgpc_string_assign(char **target, const char *value) {
  if (target == NULL)
    return;

  char *existing = *target;
  if (existing != NULL && value == existing)
    return;

  if (existing != NULL)
    kgpc_string_release(existing);

  if (value == NULL) {
    *target = kgpc_alloc_empty_string();
    return;
  }

  KgpcStringHeader *hdr = kgpc_string_header(value);
  if (hdr != NULL) {
    kgpc_string_retain(value);
    *target = (char *)value;
    return;
  }

  *target = kgpc_string_duplicate(value);
}

char *kgpc_string_unique(char **target) {
  if (target == NULL)
    return kgpc_alloc_empty_string();

  char *value = *target;
  if (value == NULL) {
    value = kgpc_alloc_empty_string();
    *target = value;
    return value;
  }

  KgpcStringHeader *hdr = kgpc_string_header(value);
  if (hdr == NULL) {
    value = kgpc_string_duplicate(value);
    *target = value;
    return value;
  }

  if (hdr->refcount <= 1)
    return value;

  char *copy = kgpc_string_duplicate_length(value, hdr->length);
  kgpc_string_release(value);
  *target = copy;
  return copy;
}

void kgpc_string_assign_take(char **target, char *value) {
  if (target == NULL) {
    if (value != NULL)
      free(value);
    return;
  }

  if (value == NULL) {
    if (*target != NULL)
      kgpc_string_release(*target);
    *target = kgpc_alloc_empty_string();
    return;
  }

  if (*target != NULL)
    kgpc_string_release(*target);

  KgpcStringHeader *hdr = kgpc_string_header(value);
  if (hdr != NULL) {
    *target = value;
    return;
  }

  size_t len = strlen(value);
  char *copy = kgpc_string_alloc_with_length(len);
  if (copy == NULL) {
    *target = kgpc_alloc_empty_string();
    free(value);
    return;
  }
  if (len > 0)
    memcpy(copy, value, len);
  free(value);
  *target = copy;
}

void kgpc_set_codepage_string(char **value, uint16_t codepage, int convert) {
  (void)value;
  (void)codepage;
  (void)convert;
}

void kgpc_string_setlength(char **target, int64_t new_length) {
  if (target == NULL)
    return;

  if (new_length < 0)
    new_length = 0;

  size_t requested = (size_t)new_length;
  char *current = *target;
  size_t current_len = kgpc_string_known_length(current);
  if (current_len > requested)
    current_len = requested;

  char *resized = kgpc_string_alloc_with_length(requested);
  if (resized == NULL) {
    fprintf(stderr, "KGPC runtime: failed to resize string to %lld bytes.\n",
            (long long)new_length);
    exit(EXIT_FAILURE);
  }

  if (current_len > 0)
    memcpy(resized, current, current_len);
  if (requested > current_len)
    memset(resized + current_len, 0, requested - current_len);

  if (current != NULL)
    kgpc_string_release(current);
  *target = resized;
}

void kgpc_unicodestring_setlength(uint16_t **target, int64_t new_length) {
  if (target == NULL)
    return;

  if (new_length <= 0) {
    if (*target != NULL)
      kgpc_string_release((char *)*target);
    *target = NULL;
    return;
  }

  size_t data_bytes = (size_t)new_length * 2 + 2;
  KgpcStringHeader *hdr =
      (KgpcStringHeader *)malloc(sizeof(KgpcStringHeader) + data_bytes);
  if (hdr == NULL) {
    fprintf(stderr,
            "KGPC runtime: failed to resize unicode string to %lld chars.\n",
            (long long)new_length);
    exit(EXIT_FAILURE);
  }

  hdr->codepage = 1200;
  hdr->elementsize = 2;
  hdr->refcount = 1;
  hdr->length = new_length;

  uint16_t *data = (uint16_t *)(hdr + 1);
  memset(data, 0, data_bytes);
  kgpc_string_set_insert((char *)data);

  if (*target != NULL)
    kgpc_string_release((char *)*target);
  *target = data;
}

void kgpc_setstring(char **target, const char *buffer, int64_t length) {
  if (target == NULL)
    return;

  if (buffer == NULL || length <= 0) {
    /* Set to empty string */
    char *empty = kgpc_alloc_empty_string();
    char *current = *target;
    if (current != NULL)
      kgpc_string_release(current);
    *target = empty;
    return;
  }

  size_t copy_len = (size_t)length;
  char *result = kgpc_string_alloc_with_length(copy_len);
  if (result == NULL) {
    fprintf(
        stderr,
        "KGPC runtime: failed to allocate string (%zu bytes including null).\n",
        copy_len + 1);
    exit(EXIT_FAILURE);
  }

  memcpy(result, buffer, copy_len);
  char *current = *target;
  if (current != NULL)
    kgpc_string_release(current);
  *target = result;
}

static int64_t kgpc_unicode_known_length(const uint16_t *value) {
  if (value == NULL)
    return 0;
  KgpcStringHeader *hdr = kgpc_string_header((const char *)value);
  if (hdr != NULL)
    return hdr->length;
  return kgpc_widechar_length(value);
}

static uint16_t *kgpc_alloc_empty_unicodestring(void) {
  static struct {
    KgpcStringHeader header;
    uint16_t data[1];
  } empty = {{1200, 2, -1, 0}, {0}};
  return empty.data;
}

void kgpc_setstring_unicode(uint16_t **target, const uint16_t *buffer,
                            int64_t length) {
  if (target == NULL)
    return;

  if (buffer == NULL || length <= 0) {
    uint16_t *empty = kgpc_alloc_empty_unicodestring();
    uint16_t *current = *target;
    if (current != NULL)
      kgpc_string_release((char *)current);
    *target = empty;
    return;
  }

  size_t data_bytes = (size_t)length * 2 + 2;
  KgpcStringHeader *hdr =
      (KgpcStringHeader *)malloc(sizeof(KgpcStringHeader) + data_bytes);
  if (hdr == NULL) {
    fprintf(stderr,
            "KGPC runtime: failed to allocate UnicodeString (%zu bytes).\n",
            data_bytes);
    exit(EXIT_FAILURE);
  }
  hdr->codepage = 1200; /* UTF-16LE */
  hdr->elementsize = 2; /* UnicodeChar = 2 bytes */
  hdr->refcount = 1;
  hdr->length = length;
  uint16_t *data = (uint16_t *)(hdr + 1);
  memcpy(data, buffer, (size_t)length * 2);
  data[length] = 0;

  kgpc_string_set_insert((char *)data);

  uint16_t *current = *target;
  if (current != NULL)
    kgpc_string_release((char *)current);
  *target = data;
}

void kgpc_write_unicodestring(KGPCTextRec *file, int width,
                              const uint16_t *value) {
  FILE *dest = kgpc_get_write_stream(file, &width);
  if (dest == NULL)
    return;

  if (value == NULL)
    value = kgpc_alloc_empty_unicodestring();

  /* KGPC may still carry "unicode" values in single-byte managed-string
   * storage (elementsize=1). In that case, print through string writer
   * instead of interpreting bytes as UTF-16 code units. */
  {
    KgpcStringHeader *hdr = kgpc_string_header((const char *)value);
    if (hdr != NULL && hdr->elementsize == 1) {
      kgpc_write_string(file, width, (const char *)value);
      return;
    }
  }

  int64_t len = kgpc_unicode_known_length(value);
  if (len <= 0 && width == 0)
    return;

  char *ansi = NULL;
  typedef void (*Unicode2AnsiProc)(const uint16_t *, char **, int32_t, int64_t);
  Unicode2AnsiProc conv = (Unicode2AnsiProc)widestringmanager[19];
  int32_t cp = DefaultSystemCodePage;
  if (conv != NULL)
    conv(value, &ansi, cp, len);
  else
    kgpc_default_unicode2ansi_move(value, &ansi, cp, len);

  kgpc_write_string(file, width, ansi);

  if (ansi != NULL)
    kgpc_string_release(ansi);
}

void kgpc_string_delete(char **target, int64_t index, int64_t count) {
  if (target == NULL || index <= 0 || count <= 0)
    return;

  char *source = *target;
  size_t length = kgpc_string_known_length(source);
  if (length == 0)
    return;

  if (index > (int64_t)length)
    return;

  size_t start = (size_t)(index - 1);
  size_t remove = (size_t)count;
  if (remove > length - start)
    remove = length - start;

  size_t new_length = length - remove;
  char *result = kgpc_string_alloc_with_length(new_length);
  if (result == NULL) {
    fprintf(stderr, "KGPC runtime: failed to delete substring (%lld bytes).\n",
            (long long)remove);
    exit(EXIT_FAILURE);
  }

  if (start > 0)
    memcpy(result, source, start);
  size_t tail = length - start - remove;
  if (tail > 0)
    memcpy(result + start, source + start + remove, tail);
  if (source != NULL)
    kgpc_string_release(source);
  *target = result;
}

void kgpc_string_insert(const char *value, char **target, int64_t index) {
  if (target == NULL || value == NULL)
    return;

  size_t insert_len = kgpc_string_known_length(value);
  if (insert_len == 0)
    return;

  char *dest = *target;
  size_t dest_len = kgpc_string_known_length(dest);

  if (index <= 0)
    index = 1;
  if (index > (int64_t)dest_len + 1)
    index = (int64_t)dest_len + 1;

  size_t pos = (size_t)(index - 1);
  size_t new_len = dest_len + insert_len;

  char *result = kgpc_string_alloc_with_length(new_len);
  if (result == NULL) {
    fprintf(stderr, "KGPC runtime: failed to insert substring (%zu bytes).\n",
            insert_len);
    exit(EXIT_FAILURE);
  }

  if (pos > 0 && dest != NULL)
    memcpy(result, dest, pos);
  if (insert_len > 0)
    memcpy(result + pos, value, insert_len);
  if (dest != NULL && pos < dest_len)
    memcpy(result + pos + insert_len, dest + pos, dest_len - pos);
  if (dest != NULL)
    kgpc_string_release(dest);
  *target = result;
}

/* Release a heap descriptor produced by kgpc_dynarray_clone_descriptor
 * whose value has been consumed inline (e.g. Length(F()), F()[i]) and is
 * no longer needed.  The descriptor block was malloc'd by the producer,
 * and so was the element data buffer it points to (allocated by SetLength
 * or kgpc_dynarray_deep_copy* inside the producing function before that
 * function abandoned its local Result slot on return).  Free both. */
void kgpc_dynarray_release_temp_descriptor(void *temp_descriptor) {
  if (temp_descriptor == NULL)
    return;
  kgpc_dynarray_descriptor_t *desc =
      (kgpc_dynarray_descriptor_t *)temp_descriptor;
  free(desc->data);
  free(desc);
}

/* Finalize an inline dynamic-array descriptor: release its element data
 * buffer and zero the descriptor in place.  Used at function/program
 * epilogue for managed dynarray locals and globals whose backing buffer
 * would otherwise outlive the only references to it.
 *
 * Unlike kgpc_dynarray_release_temp_descriptor, the descriptor block itself
 * is NOT freed — it lives inline in a stack frame, .bss slot, or a record
 * field, owned by the surrounding storage.  Only ->data is heap-owned.
 *
 * Idempotent: after this call the descriptor's data is NULL and length 0,
 * so a second invocation (e.g. from a nested cleanup path) is a no-op. */
void kgpc_dynarray_finalize_local(void *descriptor_ptr) {
  if (descriptor_ptr == NULL)
    return;
  kgpc_dynarray_descriptor_t *desc =
      (kgpc_dynarray_descriptor_t *)descriptor_ptr;
  if (desc->data != NULL) {
    free(desc->data);
    desc->data = NULL;
  }
  desc->length = 0;
}

void *kgpc_dynarray_clone_descriptor(const void *descriptor,
                                     size_t descriptor_size) {
  if (descriptor_size == 0)
    descriptor_size = sizeof(kgpc_dynarray_descriptor_t);

  void *temp = malloc(descriptor_size);
  if (temp == NULL)
    return NULL;

  if (descriptor != NULL)
    memcpy(temp, descriptor, descriptor_size);
  else
    memset(temp, 0, descriptor_size);

  return temp;
}

void kgpc_dynarray_assign_descriptor(void *dest_descriptor,
                                     const void *src_descriptor,
                                     size_t descriptor_size) {
  if (dest_descriptor == NULL || src_descriptor == NULL)
    return;

  if (descriptor_size == 0)
    descriptor_size = sizeof(kgpc_dynarray_descriptor_t);

  /* Release the dest's previous element data buffer before overwriting,
   * unless the source already references it (aliasing/self-assignment).
   * Dynamic arrays in this runtime use unique ownership — every assignment
   * either deep-copies or transfers ownership of a freshly produced buffer
   * — so the old data is no longer reachable through any other variable. */
  kgpc_dynarray_descriptor_t *dst =
      (kgpc_dynarray_descriptor_t *)dest_descriptor;
  const kgpc_dynarray_descriptor_t *src =
      (const kgpc_dynarray_descriptor_t *)src_descriptor;
  if (dst->data != NULL && dst->data != src->data)
    free(dst->data);

  memcpy(dest_descriptor, src_descriptor, descriptor_size);
}

void kgpc_dynarray_assign_from_temp(void *dest_descriptor,
                                    void *temp_descriptor,
                                    size_t descriptor_size) {
  if (dest_descriptor == NULL) {
    /* No destination, but we still own the temp by contract:
     * release it to avoid leaking the producer's clone. */
    free(temp_descriptor);
    return;
  }

  if (descriptor_size == 0)
    descriptor_size = sizeof(kgpc_dynarray_descriptor_t);

  /* Release dest's previous element data before we overwrite the slot.
   * Dynamic arrays are uniquely owned here; the old buffer would otherwise
   * be unreachable after the assignment. */
  kgpc_dynarray_descriptor_t *dst =
      (kgpc_dynarray_descriptor_t *)dest_descriptor;
  void *old_data = dst->data;

  if (temp_descriptor == NULL) {
    if (old_data != NULL)
      free(old_data);
    memset(dest_descriptor, 0, descriptor_size);
    return;
  }

  kgpc_dynarray_descriptor_t *src =
      (kgpc_dynarray_descriptor_t *)temp_descriptor;
  void *new_data = src->data;

  memcpy(dest_descriptor, temp_descriptor, descriptor_size);
  if (old_data != NULL && old_data != new_data)
    free(old_data);

  /* The temp descriptor was heap-allocated by the producer (typically
   * kgpc_dynarray_clone_descriptor at a function return).  We are the
   * sole consumer: take ownership of the descriptor block and free it.
   * The element data buffer referenced by descriptor->data is transferred
   * to the destination. */
  free(temp_descriptor);
}

/* Deep-copy a dynamic array: allocate heap buffer, copy element data,
 * and return a new descriptor. */
void *kgpc_dynarray_deep_copy(const void *src_descriptor,
                              size_t descriptor_size, size_t element_size) {
  if (src_descriptor == NULL || element_size == 0)
    return NULL;

  if (descriptor_size == 0)
    descriptor_size = sizeof(kgpc_dynarray_descriptor_t);

  const kgpc_dynarray_descriptor_t *src =
      (const kgpc_dynarray_descriptor_t *)src_descriptor;
  size_t count = src->length > 0 ? (size_t)src->length : 0;
  size_t data_bytes = count * element_size;

  void *heap_desc = malloc(descriptor_size);
  if (heap_desc == NULL)
    return NULL;

  memcpy(heap_desc, src_descriptor, descriptor_size);
  kgpc_dynarray_descriptor_t *dst = (kgpc_dynarray_descriptor_t *)heap_desc;

  if (data_bytes > 0 && src->data != NULL) {
    void *heap_data = malloc(data_bytes);
    if (heap_data != NULL) {
      memcpy(heap_data, src->data, data_bytes);
      dst->data = heap_data;
    } else {
      dst->data = NULL;
      dst->length = 0;
    }
  } else {
    dst->data = NULL;
    dst->length = 0;
  }

  return heap_desc;
}

/* Deep-copy into an existing destination descriptor. */
void kgpc_dynarray_deep_copy_into(void *dest_descriptor,
                                  const void *src_descriptor,
                                  size_t descriptor_size, size_t element_size) {
  if (dest_descriptor == NULL || src_descriptor == NULL || element_size == 0)
    return;

  if (descriptor_size == 0)
    descriptor_size = sizeof(kgpc_dynarray_descriptor_t);

  const kgpc_dynarray_descriptor_t *src =
      (const kgpc_dynarray_descriptor_t *)src_descriptor;
  kgpc_dynarray_descriptor_t *dst =
      (kgpc_dynarray_descriptor_t *)dest_descriptor;

  size_t count = src->length > 0 ? (size_t)src->length : 0;
  size_t data_bytes = count * element_size;

  void *heap_data = NULL;
  if (data_bytes > 0 && src->data != NULL) {
    heap_data = malloc(data_bytes);
    if (heap_data != NULL)
      memcpy(heap_data, src->data, data_bytes);
    else
      count = 0;
  } else {
    count = 0;
  }

  /* Release dst's previous element data before swapping in the freshly
   * heap-allocated buffer.  src->data was just deep-copied into heap_data
   * so any aliasing between dst and src has already been broken; the old
   * dst->data is owned solely by the destination and now unreachable. */
  if (dst->data != NULL && dst->data != heap_data)
    free(dst->data);

  /* Mirror the full descriptor (matching kgpc_dynarray_deep_copy) so
   * any per-descriptor metadata beyond data/length is preserved, then
   * point the destination at the freshly heap-allocated buffer. */
  memcpy(dest_descriptor, src_descriptor, descriptor_size);
  dst->data = heap_data;
  dst->length = (int64_t)count;
}

long long kgpc_dynarray_compute_high(const void *descriptor_ptr,
                                     long long lower_bound) {
  if (descriptor_ptr == NULL)
    return lower_bound - 1;

  const kgpc_dynarray_descriptor_t *descriptor =
      (const kgpc_dynarray_descriptor_t *)descriptor_ptr;
  long long length = descriptor->length;
  if (length <= 0)
    return lower_bound - 1;

  return lower_bound + length - 1;
}

char **kgpc_array_string_to_ppchar(const void *descriptor_ptr,
                                   long long reserve_entries) {
  if (descriptor_ptr == NULL)
    return NULL;

  const kgpc_dynarray_descriptor_t *descriptor =
      (const kgpc_dynarray_descriptor_t *)descriptor_ptr;

  long long length = descriptor->length;
  if (length <= 0)
    return NULL;

  char **data = (char **)descriptor->data;

  /* Allocate: reserve_entries + length + 1 (null terminator) */
  long long total = reserve_entries + length + 1;
  char **result = (char **)malloc(total * sizeof(char *));
  if (result == NULL)
    return NULL;

  /* Fill reserved slots with NULL */
  for (long long i = 0; i < reserve_entries; i++)
    result[i] = NULL;

  /* Copy string pointers from the dynamic array */
  for (long long i = 0; i < length; i++)
    result[reserve_entries + i] = data[i];

  /* Null-terminate */
  result[reserve_entries + length] = NULL;

  return result;
}

/* Copy a string literal to a char array (fixed-size buffer)
 * Fills the entire array. If the string is shorter, pads with nulls.
 * If the string is longer, truncates to fit.
 * Note: May NOT be null-terminated if string exactly fills the array!
 */
void kgpc_string_to_char_array(char *dest, const char *src, size_t dest_size) {
  if (dest == NULL || src == NULL || dest_size == 0)
    return;

  size_t src_len = kgpc_string_known_length(src);
  size_t copy_len = (src_len < dest_size) ? src_len : dest_size;

  memcpy(dest, src, copy_len);

  /* Pad remaining space with zeros if string is shorter than array */
  if (copy_len < dest_size)
    memset(dest + copy_len, 0, dest_size - copy_len);
}

/* Copy string to ShortString (Pascal string with length byte at index 0).
 *
 * Matches FPC's `fpc_AnsiStr_To_ShortStr` semantics: writes the length byte
 * at offset 0 and copies up to `min(src_len, dest_size - 1, 255)` chars at
 * offset 1..length.  Bytes past `length` in the destination are left
 * untouched — ShortStrings are length-prefixed, so trailing bytes are
 * irrelevant to readers.  Padding with zeros is unsafe when the caller
 * passes a buffer dynamically sized to `length(src) + 1` (e.g. via
 * `getmem(result, length(s) + 1); result^ := s`) and over-reports
 * `dest_size` (the conservative 256 fallback in the codegen).  Skipping
 * the padding keeps that idiomatic FPC pattern correct without losing
 * correctness for fixed-size ShortString buffers. */
void kgpc_string_to_shortstring(char *dest, const char *src, size_t dest_size) {
  if (dest == NULL || src == NULL || dest_size < 2)
    return;

  if (!kgpc_string_is_managed(src)) {
    unsigned char short_len = (unsigned char)src[0];
    size_t c_len = strlen(src);
    if (short_len > 0 && c_len == (size_t)short_len + 1) {
      size_t max_chars = (dest_size - 1 < 255) ? (dest_size - 1) : 255;
      size_t copy_len = (short_len < max_chars) ? short_len : max_chars;
      dest[0] = (char)copy_len;
      if (copy_len > 0)
        memmove(dest + 1, src + 1, copy_len);
      return;
    }
  }

  size_t src_len = kgpc_string_known_length(src);
  /* ShortString max capacity is 255 chars (indices 1..255) */
  size_t max_chars = (dest_size - 1 < 255) ? (dest_size - 1) : 255;
  size_t copy_len = (src_len < max_chars) ? src_len : max_chars;

  /* Set length byte at index 0 */
  dest[0] = (char)copy_len;

  /* Copy characters starting at index 1 */
  if (copy_len > 0)
    memcpy(dest + 1, src, copy_len);
}

void kgpc_char_array_to_shortstring(char *dest, const char *src, size_t src_len,
                                    size_t dest_size) {
  if (dest == NULL || src == NULL || dest_size < 2)
    return;

  size_t max_chars = (dest_size - 1 < 255) ? (dest_size - 1) : 255;
  size_t copy_len = (src_len < max_chars) ? src_len : max_chars;

  dest[0] = (char)copy_len;
  if (copy_len > 0)
    memcpy(dest + 1, src, copy_len);
  if (copy_len + 1 < dest_size)
    memset(dest + 1 + copy_len, 0, dest_size - 1 - copy_len);
}

void kgpc_shortstring_to_char_array(char *dest, const char *src,
                                    size_t dest_size) {
  if (dest == NULL || src == NULL || dest_size == 0)
    return;

  unsigned char src_len = (unsigned char)src[0];
  size_t copy_len = (src_len < dest_size) ? src_len : dest_size;

  if (copy_len > 0)
    memcpy(dest, src + 1, copy_len);
  if (copy_len < dest_size)
    memset(dest + copy_len, 0, dest_size - copy_len);
}

void kgpc_shortstring_to_shortstring(char *dest, size_t dest_size,
                                     const char *src) {
  if (dest == NULL || src == NULL || dest_size < 2)
    return;

  unsigned char src_len = (unsigned char)src[0];
  if (src_len > 0 && memchr(src + 1, '\0', src_len) != NULL) {
    /* Some bootstrap call paths still hand us a C string/AnsiString payload
     * even though the formal parameter is lowered as ShortString. Detect that
     * representation conservatively and convert from the C string form. */
    kgpc_string_to_shortstring(dest, src, dest_size);
    return;
  }
  size_t max_chars = (dest_size - 1 < 255) ? (dest_size - 1) : 255;
  size_t copy_len = (src_len < max_chars) ? src_len : max_chars;

  dest[0] = (char)copy_len;
  if (copy_len > 0)
    memcpy(dest + 1, src + 1, copy_len);
  if (copy_len + 1 < dest_size)
    memset(dest + 1 + copy_len, 0, dest_size - 1 - copy_len);
}

void kgpc_shortstring_setlength(char *target, int64_t new_length) {
  if (target == NULL)
    return;

  if (new_length < 0)
    new_length = 0;
  if (new_length > 255)
    new_length = 255;

  unsigned char old_len = (unsigned char)target[0];
  unsigned char new_len = (unsigned char)new_length;
  target[0] = (char)new_len;

  if (new_len > old_len)
    memset(target + 1 + old_len, 0, (size_t)(new_len - old_len));
}

void kgpc_shortstring_setstring(char *target, const char *buffer,
                                int64_t length) {
  if (target == NULL)
    return;

  if (buffer == NULL || length <= 0) {
    target[0] = 0;
    return;
  }

  if (length > 255)
    length = 255;

  target[0] = (char)length;
  memcpy(target + 1, buffer, (size_t)length);
  if (length < 255)
    memset(target + 1 + length, 0, (size_t)(255 - length));
}

char *kgpc_shortstring_to_string(const char *value) {
  if (value == NULL)
    return kgpc_string_duplicate("");

  unsigned char len = (unsigned char)value[0];
  return kgpc_string_duplicate_length(value + 1, len);
}

/* FPC_PCHAR_TO_SHORTSTR: Moved to runtime_fpc_pchar_to_shortstr.c
 * so it lives in its own .o file.  In FPC mode, the compiler provides its own
 * version and this .o is not pulled from the archive.  In non-FPC mode, this
 * .o is pulled to resolve the undefined reference. */

int64_t kgpc_shortstring_length(const char *value) {
  if (value == NULL)
    return 0;
  return (unsigned char)value[0];
}

void kgpc_shortstring_delete(char *target, int64_t index, int64_t count) {
  if (target == NULL || index <= 0 || count <= 0)
    return;

  size_t length = (unsigned char)target[0];
  if (length == 0)
    return;
  if (index > (int64_t)length)
    return;

  size_t start = (size_t)(index - 1);
  size_t remove = (size_t)count;
  if (remove > length - start)
    remove = length - start;

  size_t tail = length - start - remove;
  if (tail > 0)
    memmove(target + 1 + start, target + 1 + start + remove, tail);

  size_t new_len = length - remove;
  target[0] = (char)new_len;
  if (remove > 0)
    memset(target + 1 + new_len, 0, remove);
}

void kgpc_shortstring_insert(const char *value, char *target, int64_t index,
                             int value_is_shortstring) {
  if (target == NULL || value == NULL)
    return;

  const char *insert_ptr = value;
  size_t insert_len = 0;
  if (value_is_shortstring) {
    insert_len = (unsigned char)value[0];
    insert_ptr = value + 1;
  } else {
    insert_len = kgpc_string_known_length(value);
    insert_ptr = value;
  }

  if (insert_len == 0)
    return;

  size_t dest_len = (unsigned char)target[0];
  if (index <= 0)
    index = 1;
  if (index > (int64_t)dest_len + 1)
    index = (int64_t)dest_len + 1;

  size_t max_insert = (dest_len < 255) ? (255 - dest_len) : 0;
  if (insert_len > max_insert)
    insert_len = max_insert;
  if (insert_len == 0)
    return;

  size_t pos = (size_t)(index - 1);
  memmove(target + 1 + pos + insert_len, target + 1 + pos, dest_len - pos);
  memcpy(target + 1 + pos, insert_ptr, insert_len);

  target[0] = (char)(dest_len + insert_len);
}

static void kgpc_text_close_stream(KGPCTextRec *file) {
  if (file == NULL)
    return;
  FILE *stream = kgpc_textrec_get_stream(file, NULL);
  if (stream == NULL)
    return;
  if (stream == stdin || stream == stdout || stream == stderr)
    return;
  fclose(stream);
  kgpc_textrec_set_stream(file, NULL);
  file->mode = KGPC_FM_CLOSED;
}

static char *kgpc_text_read_line_from_stream(FILE *stream) {
  if (stream == NULL)
    return NULL;

  char chunk[4096];
  size_t capacity = 256;
  size_t length = 0;
  char *buffer = (char *)malloc(capacity);
  if (buffer == NULL)
    return NULL;

  int read_any = 0;
  while (fgets(chunk, sizeof(chunk), stream) != NULL) {
    read_any = 1;
    size_t chunk_len = strlen(chunk);
    char *line_end = strpbrk(chunk, "\r\n");
    size_t copy_len =
        (line_end != NULL) ? (size_t)(line_end - chunk) : chunk_len;

    if (length + copy_len + 1 > capacity) {
      size_t new_capacity = capacity;
      while (length + copy_len + 1 > new_capacity) {
        new_capacity =
            (new_capacity < 1024) ? new_capacity * 2 : new_capacity + 512;
        if (new_capacity <= capacity)
          new_capacity = capacity + 512;
      }
      char *new_buffer = (char *)realloc(buffer, new_capacity);
      if (new_buffer == NULL) {
        free(buffer);
        return NULL;
      }
      buffer = new_buffer;
      capacity = new_capacity;
    }

    if (copy_len > 0) {
      memcpy(buffer + length, chunk, copy_len);
      length += copy_len;
    }

    if (line_end != NULL) {
      if (*line_end == '\r' && line_end[1] != '\n') {
        int next = fgetc(stream);
        if (next != '\n' && next != EOF)
          ungetc(next, stream);
      }
      break;
    }
  }

  if (!read_any) {
    free(buffer);
    return NULL;
  }

  buffer[length] = '\0';
  return buffer;
}

void kgpc_text_assign(KGPCTextRec *file, const char *path) {
  if (file == NULL)
    return;
  kgpc_text_close_stream(file);
  kgpc_copy_name(file->name, sizeof(file->name), path);
  kgpc_textrec_init_defaults(file);
}

void kgpc_text_rewrite(KGPCTextRec *file) {
  if (file == NULL)
    return;
  kgpc_text_close_stream(file);
  kgpc_textrec_init_defaults(file);

  FILE *stream = NULL;
  if (file->name[0] == '\0')
    stream = stdout;
  else
    stream = fopen(file->name, "w");
  if (stream != NULL) {
    kgpc_textrec_set_stream(file, stream);
    if (file->bufptr != NULL && file->bufsize > 0)
      setvbuf(stream, file->bufptr, _IOFBF, (size_t)file->bufsize);
    file->mode = KGPC_FM_OUTPUT;
    kgpc_ioresult_set(0);
  } else {
    file->mode = KGPC_FM_CLOSED;
    kgpc_ioresult_set(errno);
  }
}

void kgpc_text_append(KGPCTextRec *file) {
  if (file == NULL)
    return;
  kgpc_text_close_stream(file);
  kgpc_textrec_init_defaults(file);

  FILE *stream = NULL;
  if (file->name[0] == '\0')
    stream = stdout;
  else
    stream = fopen(file->name, "a");
  if (stream != NULL) {
    kgpc_textrec_set_stream(file, stream);
    if (file->bufptr != NULL && file->bufsize > 0)
      setvbuf(stream, file->bufptr, _IOFBF, (size_t)file->bufsize);
    file->mode = KGPC_FM_OUTPUT;
    fseek(stream, 0, SEEK_END);
    kgpc_ioresult_set(0);
  } else {
    file->mode = KGPC_FM_CLOSED;
    kgpc_ioresult_set(errno);
  }
}

void kgpc_text_app(KGPCTextRec *file) { kgpc_text_append(file); }

void kgpc_text_reset(KGPCTextRec *file) {
  if (file == NULL)
    return;
  kgpc_text_close_stream(file);
  kgpc_textrec_init_defaults(file);

  FILE *stream = NULL;
  if (file->name[0] == '\0')
    stream = stdin;
  else
    stream = fopen(file->name, "r");
  if (stream != NULL) {
    kgpc_textrec_set_stream(file, stream);
    if (file->bufptr != NULL && file->bufsize > 0)
      setvbuf(stream, file->bufptr, _IOFBF, (size_t)file->bufsize);
    file->mode = KGPC_FM_INPUT;
    kgpc_ioresult_set(0);
  } else {
    file->mode = KGPC_FM_CLOSED;
    kgpc_ioresult_set(errno);
  }
}

void kgpc_text_close(KGPCTextRec *file) {
  if (file == NULL)
    return;
  kgpc_text_close_stream(file);
}

int kgpc_text_eof(KGPCTextRec *file) {
  /* Fast path: if no stdio FILE* has been created yet for this textrec,
   * check eof directly on the kernel fd via lseek+fstat.  This avoids
   * fgetc's 8KB read-ahead, which advances the kernel file position and
   * silently skips data for callers that read via raw syscalls
   * (e.g. FPC's TCFileStream / BlockRead).  For non-seekable fds
   * (pipes, sockets, ttys) lseek returns -1 and we fall back to stdio.
   *
   * Once stdio has been engaged (private_data != 0), it owns the read
   * buffer and we must use fgetc/ungetc so eof reflects what the stdio
   * reader will see, not the kernel position. */
#ifndef _WIN32
  if (file != NULL && file->private_data == 0 && file->handle >= 0 &&
      file->mode != 0 && file->mode != (int32_t)0xD7B0) {
    int fd = file->handle;
    int saved_errno = errno;
    off_t pos = lseek(fd, 0, SEEK_CUR);
    if (pos != (off_t)-1) {
      struct stat st;
      if (fstat(fd, &st) == 0 && S_ISREG(st.st_mode)) {
        errno = saved_errno;
        return pos >= st.st_size ? 1 : 0;
      }
    }
    errno = saved_errno;
    /* Non-seekable or non-regular: fall through to stdio path. */
  }
#endif

  FILE *stream = kgpc_text_input_stream(file);
  if (stream == NULL)
    return 1;

  int ch = fgetc(stream);
  if (ch == EOF)
    return 1;

  // Try to put the character back, but don't treat ungetc failure as EOF
  // ungetc can fail due to buffer limitations, but that doesn't mean we're at
  // EOF
  ungetc(ch, stream);

  return 0;
}

int kgpc_text_eof_default(void) { return kgpc_text_eof(NULL); }

int kgpc_text_eoln(KGPCTextRec *file) {
  FILE *stream = kgpc_text_input_stream(file);
  if (stream == NULL)
    return 1;

  int ch = fgetc(stream);
  if (ch == EOF)
    return 1;

  if (ch == '\r') {
    int next = fgetc(stream);
    if (next != EOF)
      ungetc(next, stream);
    ungetc('\r', stream);
    return 1;
  }

  if (ch == '\n') {
    ungetc(ch, stream);
    return 1;
  }

  ungetc(ch, stream);
  return 0;
}

int kgpc_text_eoln_default(void) { return kgpc_text_eoln(NULL); }

void kgpc_text_readln_into(KGPCTextRec *file, char **target) {
  if (target == NULL)
    return;

  FILE *stream = kgpc_text_input_stream(file);
  if (stream == NULL) {
    kgpc_string_assign(target, "");
    return;
  }

  char *line = kgpc_text_read_line_from_stream(stream);
  if (line == NULL) {
    kgpc_string_assign(target, "");
    return;
  }

  kgpc_string_assign_take(target, line);
}

void kgpc_text_readln_into_char(KGPCTextRec *file, char *target) {
  if (target == NULL)
    return;

  FILE *stream = kgpc_text_input_stream(file);
  if (stream == NULL) {
    *target = '\0';
    return;
  }

  char *line = kgpc_text_read_line_from_stream(stream);
  if (line == NULL) {
    *target = '\0';
    return;
  }

  if (line[0] != '\0')
    *target = line[0];
  else
    *target = '\0';

  free(line);
}

void kgpc_text_readln_into_shortstring(KGPCTextRec *file, unsigned char *target,
                                       int max_len) {
  /* target[0] = length byte, target[1..max_len] = character data */
  if (target == NULL)
    return;
  if (max_len < 0)
    max_len = 0;
  if (max_len > 255)
    max_len = 255;

  FILE *stream = kgpc_text_input_stream(file);
  if (stream == NULL) {
    target[0] = 0;
    return;
  }

  char *line = kgpc_text_read_line_from_stream(stream);
  if (line == NULL) {
    target[0] = 0;
    return;
  }

  int len = (int)strlen(line);
  if (len > max_len)
    len = max_len;
  target[0] = (unsigned char)len;
  memcpy(target + 1, line, len);
  free(line);
}

void kgpc_text_readln_discard(KGPCTextRec *file) {
  FILE *stream = kgpc_text_input_stream(file);
  if (stream == NULL)
    return;

  int ch;
  while ((ch = fgetc(stream)) != EOF) {
    if (ch == '\r') {
      int next = fgetc(stream);
      if (next != '\n' && next != EOF)
        ungetc(next, stream);
      break;
    }
    if (ch == '\n')
      break;
  }
}

void kgpc_move(void *dest, const void *src, size_t count) {
  if (dest == NULL || src == NULL || count == 0)
    return;

  memmove(dest, src, count);
}

void kgpc_fillchar(void *dest, size_t count, int value) {
  if (dest == NULL || count == 0)
    return;

  unsigned char byte_value = (unsigned char)(value & 0xFF);
  memset(dest, byte_value, count);
}

void kgpc_fillword(void *dest, size_t count, unsigned short value) {
  if (dest == NULL || count == 0)
    return;

  unsigned short *ptr = (unsigned short *)dest;
  for (size_t i = 0; i < count; ++i)
    ptr[i] = value;
}

void kgpc_getmem(void **target, size_t size) {
  if (target == NULL)
    return;

  if (size == 0) {
    if (*target != NULL) {
      free(*target);
      *target = NULL;
    }
    return;
  }

  void *memory = malloc(size);
  if (memory == NULL) {
    fprintf(stderr, "KGPC runtime: failed to allocate %zu bytes via GetMem.\n",
            size);
    exit(EXIT_FAILURE);
  }

  *target = memory;
}

/* AllocMem: allocates memory and zero-initializes it (like calloc) */
void *kgpc_allocmem(size_t size) {
  if (size == 0)
    return NULL;

  void *memory = calloc(1, size);
  if (memory == NULL) {
    fprintf(stderr,
            "KGPC runtime: failed to allocate %zu bytes via AllocMem.\n", size);
    exit(EXIT_FAILURE);
  }

  return memory;
}

void kgpc_freemem(void *ptr) {
  if (ptr != NULL)
    free(ptr);
}

void kgpc_reallocmem(void **target, size_t new_size) {
  if (target == NULL)
    return;

  if (new_size == 0) {
    if (*target != NULL) {
      free(*target);
      *target = NULL;
    }
    return;
  }

  void *original = *target;
  void *resized = NULL;
  if (original == NULL)
    resized = malloc(new_size);
  else
    resized = realloc(original, new_size);

  if (resized == NULL) {
    fprintf(stderr,
            "KGPC runtime: failed to (re)allocate %zu bytes via ReallocMem.\n",
            new_size);
    exit(EXIT_FAILURE);
  }

  *target = resized;
}

/* =====================================================================
 * FPC RTL heap-manager overrides.
 *
 * When compiling against the FPC RTL (--no-stdlib), the FPC system unit
 * emits its own HeapInc allocator with weak SysGetMem/SysFreeMem/etc.
 * symbols.  HeapInc requires InitHeap() to be called first, which KGPC
 * does not do.  Instead we provide strong symbols that forward straight
 * to libc, so the linker picks these over the weak HeapInc versions.
 *
 * FPC ABI: SysGetMem(size: PtrInt): Pointer
 *          SysFreeMem(p: Pointer): PtrInt  (returns 0 on success)
 *          SysReallocMem(p: Pointer; size: PtrInt): Pointer
 *          SysFreeMemSize(p: Pointer; size: PtrInt): PtrInt
 *          SysTryResizeMem(p: Pointer; size: PtrInt): Pointer
 * ===================================================================== */

void *SysGetMem(intptr_t size) {
  if (size <= 0)
    return NULL;
  return malloc((size_t)size);
}

intptr_t SysFreeMem(void *p) {
  free(p);
  return 0;
}

void *SysReallocMem(void **pp, intptr_t size) {
  if (pp == NULL)
    return NULL;
  void *original = *pp;
  if (size <= 0) {
    free(original);
    *pp = NULL;
    return NULL;
  }
  void *result = realloc(original, (size_t)size);
  if (result != NULL)
    *pp = result;
  return result;
}

intptr_t SysFreeMemSize(void *p, intptr_t size) {
  (void)size;
  free(p);
  return 0;
}

void *SysTryResizeMem(void *p, intptr_t size) {
  if (size <= 0) {
    free(p);
    return NULL;
  }
  return realloc(p, (size_t)size);
}

/* ------------------------------------------------------------------ */
/* MemoryManager initialization                                        */
/*                                                                     */
/* FPC's heap functions (GetMem, FreeMem, etc.) call through function  */
/* pointers in the MemoryManager typed constant.  The runtime startup  */
/* constructor binds the basic allocator slots (GetMem .. RelocateHeap)*/
/* to libc-backed kgpc_mm_* wrappers — this is the contract every      */
/* KGPC-built binary uses, and it works whether the host system unit   */
/* is KGPC's bundled system.p (slots all NULL in .bss) or FPC's RTL    */
/* system.pp (slots already set to SysGetMem/SysFreeMem/...).  Both    */
/* are simple malloc/free pairs at the bottom, so swapping the FPC     */
/* implementations for the libc ones is fine and matches what every    */
/* runtime entry point already expects.                                */
/*                                                                     */
/* The reporting slots GetHeapStatus (80) and GetFPCHeapStatus (88)    */
/* are different: kgpc has no libc fallback for them, and FPC RTL's    */
/* typed-const initialiser correctly binds them to its Sys* routines.  */
/* The constructor MUST NOT overwrite these slots with NULL — doing so */
/* would null-call SysGetFPCHeapStatus through heap.inc's              */
/* `Result := MemoryManager.GetFPCHeapStatus()`.  So we leave any      */
/* pre-existing pointer in place and only initialise these slots if    */
/* they start NULL (KGPC-bundled system.p case).                       */
/*                                                                     */
/* TMemoryManager layout (x86-64):                                     */
/*   offset  0: NeedLock (boolean, padded to 8 bytes)                  */
/*   offset  8: GetMem   (function pointer)                            */
/*   offset 16: FreeMem  (function pointer)                            */
/*   offset 24: FreeMemSize (function pointer)                         */
/*   offset 32: AllocMem (function pointer)                            */
/*   offset 40: ReAllocMem (function pointer)                          */
/*   offset 48: MemSize  (function pointer)                            */
/*   offset 56: InitThread (procedure pointer)                         */
/*   offset 64: DoneThread (procedure pointer)                         */
/*   offset 72: RelocateHeap (procedure pointer)                       */
/*   offset 80: GetHeapStatus (function pointer)                       */
/*   offset 88: GetFPCHeapStatus (function pointer)                    */
/* ------------------------------------------------------------------ */

/* MemoryManager — the FPC heap manager dispatch table.
 * Declared in system.p (KGPC mode) or system.pp (FPC mode).
 * We populate the function pointers at startup. */
extern char MemoryManager[];

/* Simple heap wrappers matching FPC's TMemoryManager function signatures */
static void *kgpc_mm_getmem(uintptr_t size) {
  if (size == 0)
    return NULL;
  return malloc((size_t)size);
}

static uintptr_t kgpc_mm_freemem(void *p) {
  free(p);
  return 0;
}

static uintptr_t kgpc_mm_freememsize(void *p, uintptr_t size) {
  (void)size;
  free(p);
  return 0;
}

static void *kgpc_mm_allocmem(uintptr_t size) {
  if (size == 0)
    return NULL;
  return calloc(1, (size_t)size);
}

static void *kgpc_mm_reallocmem(void **pp, uintptr_t size) {
  if (pp == NULL)
    return NULL;
  void *original = *pp;
  if (size == 0) {
    free(original);
    *pp = NULL;
    return NULL;
  }
  void *result = realloc(original, (size_t)size);
  if (result != NULL)
    *pp = result;
  return result;
}

static uintptr_t kgpc_mm_memsize(void *p) {
  (void)p;
  return (uintptr_t)-1; /* unknown */
}

static void kgpc_mm_noop(void) {}

typedef void *(*kgpc_mm_allocmem_fn)(uintptr_t size);
typedef void *(*kgpc_mm_getmem_fn)(uintptr_t size);
typedef uintptr_t (*kgpc_mm_freemem_fn)(void *p);

void *kgpc_memory_manager_allocmem(uintptr_t size) {
  if (size == 0)
    return NULL;

  kgpc_mm_allocmem_fn alloc_fn = NULL;
  memcpy(&alloc_fn, MemoryManager + 32, sizeof(alloc_fn));
  if (alloc_fn == NULL)
    return kgpc_mm_allocmem(size);
  return alloc_fn(size);
}

/* GetMem dispatch — used by kgpc_new so the matching kgpc_dispose can
 * call FreeMem on the same heap.  Reads MemoryManager.GetMem (offset 8)
 * if non-NULL, otherwise uses libc malloc directly.  The direct-malloc path
 * exists for very early startup before kgpc_init_memory_manager has run
 * (Pascal initialisers must not crash if reached during a constructor).
 */
void *kgpc_memory_manager_getmem(uintptr_t size) {
  if (size == 0)
    return NULL;

  kgpc_mm_getmem_fn get_fn = NULL;
  memcpy(&get_fn, MemoryManager + 8, sizeof(get_fn));
  if (get_fn == NULL)
    return kgpc_mm_getmem(size);
  return get_fn(size);
}

/* FreeMem dispatch — paired with kgpc_memory_manager_getmem so that
 * Dispose() releases memory through the same allocator that New()
 * obtained it from.  When the FPC RTL's heap manager has rebound
 * MemoryManager (heap.inc typed const initialiser) this routes through
 * SysFreeMem; otherwise it falls back to libc free. */
void kgpc_memory_manager_freemem(void *ptr) {
  if (ptr == NULL)
    return;

  kgpc_mm_freemem_fn free_fn = NULL;
  memcpy(&free_fn, MemoryManager + 16, sizeof(free_fn));
  if (free_fn == NULL) {
    free(ptr);
    return;
  }
  free_fn(ptr);
}

__attribute__((constructor)) static void kgpc_init_memory_manager(void) {
  char *mm = (char *)MemoryManager;
  /* Skip NeedLock at offset 0 (leave as false/0) */
  void *ptrs[] = {
      kgpc_mm_getmem,      /*  8: GetMem */
      kgpc_mm_freemem,     /* 16: FreeMem */
      kgpc_mm_freememsize, /* 24: FreeMemSize */
      kgpc_mm_allocmem,    /* 32: AllocMem */
      kgpc_mm_reallocmem,  /* 40: ReAllocMem */
      kgpc_mm_memsize,     /* 48: MemSize */
      kgpc_mm_noop,        /* 56: InitThread */
      kgpc_mm_noop,        /* 64: DoneThread */
      kgpc_mm_noop,        /* 72: RelocateHeap */
  };
  /* Slots 8..72: always install the libc-backed wrappers.  Every
   * KGPC-built binary funnels alloc/free through these helpers; the
   * pre-existing pointer (from FPC RTL's typed-const, or a NULL from
   * BSS) is intentionally replaced. */
  for (int i = 0; i < 9; i++)
    memcpy(mm + 8 + i * 8, &ptrs[i], sizeof(void *));

  /* Slots 80 (GetHeapStatus) and 88 (GetFPCHeapStatus): no libc
   * fallback, so preserve whatever the host system unit's typed-const
   * initialiser put there.  KGPC-bundled system.p leaves them NULL
   * (the user program is expected not to call them); FPC RTL binds
   * them to SysGetHeapStatus / SysGetFPCHeapStatus and must keep
   * those values, otherwise heap.inc's
   *   Result := MemoryManager.GetFPCHeapStatus()
   * crashes on a NULL call. */
  (void)0;
}

char *kgpc_string_concat(const char *lhs, const char *rhs) {
  if (lhs == NULL)
    lhs = "";
  if (rhs == NULL)
    rhs = "";

  KgpcStringHeader *lhs_hdr = kgpc_string_header(lhs);
  KgpcStringHeader *rhs_hdr = kgpc_string_header(rhs);
  int lhs_elem = (lhs_hdr != NULL && lhs_hdr->elementsize == 2) ? 2 : 1;
  int rhs_elem = (rhs_hdr != NULL && rhs_hdr->elementsize == 2) ? 2 : 1;

  size_t lhs_len = kgpc_string_known_length(lhs);
  size_t rhs_len = kgpc_string_known_length(rhs);
  size_t total = lhs_len + rhs_len;

  if (lhs_elem == 2 || rhs_elem == 2) {
    size_t data_bytes = total * 2 + 2;
    KgpcStringHeader *hdr =
        (KgpcStringHeader *)malloc(sizeof(KgpcStringHeader) + data_bytes);
    if (hdr == NULL)
      return kgpc_alloc_empty_string();

    hdr->codepage = 1200;
    hdr->elementsize = 2;
    hdr->refcount = 1;
    hdr->length = (int64_t)total;

    uint16_t *out = (uint16_t *)(hdr + 1);
    size_t pos = 0;

    if (lhs_len > 0) {
      if (lhs_elem == 2) {
        memcpy(out, lhs, lhs_len * 2);
        pos += lhs_len;
      } else {
        for (size_t i = 0; i < lhs_len; ++i)
          out[pos++] = (uint16_t)(unsigned char)lhs[i];
      }
    }

    if (rhs_len > 0) {
      if (rhs_elem == 2) {
        memcpy(out + pos, rhs, rhs_len * 2);
        pos += rhs_len;
      } else {
        for (size_t i = 0; i < rhs_len; ++i)
          out[pos++] = (uint16_t)(unsigned char)rhs[i];
      }
    }

    out[total] = 0;
    kgpc_string_set_insert((char *)out);
    return (char *)out;
  }

  char *result = kgpc_string_alloc_with_length(total);
  if (result == NULL)
    return kgpc_alloc_empty_string();

  if (lhs_len > 0)
    memcpy(result, lhs, lhs_len);
  if (rhs_len > 0)
    memcpy(result + lhs_len, rhs, rhs_len);
  return result;
}

uint16_t *kgpc_unicodestring_concat(const uint16_t *lhs, const uint16_t *rhs) {
  uint16_t *lhs_owned = NULL;
  uint16_t *rhs_owned = NULL;

  if (lhs == NULL)
    lhs = kgpc_alloc_empty_unicodestring();
  if (rhs == NULL)
    rhs = kgpc_alloc_empty_unicodestring();

  {
    KgpcStringHeader *hdr = kgpc_string_header((const char *)lhs);
    if (hdr != NULL && hdr->elementsize == 1) {
      lhs_owned = kgpc_unicodestring_from_string((const char *)lhs);
      lhs = lhs_owned;
    }
  }
  {
    KgpcStringHeader *hdr = kgpc_string_header((const char *)rhs);
    if (hdr != NULL && hdr->elementsize == 1) {
      rhs_owned = kgpc_unicodestring_from_string((const char *)rhs);
      rhs = rhs_owned;
    }
  }

  int64_t lhs_len = kgpc_unicode_known_length(lhs);
  int64_t rhs_len = kgpc_unicode_known_length(rhs);
  int64_t total_len = lhs_len + rhs_len;

  uint16_t *result = kgpc_alloc_empty_unicodestring();
  if (total_len > 0) {
    size_t data_bytes = (size_t)total_len * sizeof(uint16_t) + sizeof(uint16_t);
    KgpcStringHeader *hdr =
        (KgpcStringHeader *)malloc(sizeof(KgpcStringHeader) + data_bytes);
    if (hdr != NULL) {
      hdr->codepage = 1200;
      hdr->elementsize = 2;
      hdr->refcount = 1;
      hdr->length = total_len;
      result = (uint16_t *)(hdr + 1);
      if (lhs_len > 0)
        memcpy(result, lhs, (size_t)lhs_len * sizeof(uint16_t));
      if (rhs_len > 0)
        memcpy(result + lhs_len, rhs, (size_t)rhs_len * sizeof(uint16_t));
      result[total_len] = 0;
      kgpc_string_set_insert((char *)result);
    }
  }

  if (lhs_owned != NULL)
    kgpc_string_release((char *)lhs_owned);
  if (rhs_owned != NULL)
    kgpc_string_release((char *)rhs_owned);

  return result;
}

int64_t kgpc_string_length(const char *value) {
  return (int64_t)kgpc_string_known_length(value);
}

int64_t kgpc_widechar_length(const uint16_t *value) {
  if (value == NULL)
    return 0;
  const uint16_t *cursor = value;
  while (*cursor != 0)
    cursor++;
  return (int64_t)(cursor - value);
}

char *kgpc_string_copy(const char *value, int64_t index, int64_t count) {
  if (value == NULL)
    value = "";

  size_t len = kgpc_string_known_length(value);
  if (index < 1 || index > (int64_t)len)
    return kgpc_alloc_empty_string();

  if (count < 0)
    count = 0;

  size_t start = (size_t)(index - 1);
  size_t available = len - start;
  size_t to_copy = (size_t)count;
  if (to_copy > available)
    to_copy = available;

  char *result = kgpc_string_alloc_with_length(to_copy);
  if (result == NULL)
    return kgpc_alloc_empty_string();

  if (to_copy > 0)
    memcpy(result, value + start, to_copy);
  return result;
}

uint16_t *kgpc_unicodestring_copy(const uint16_t *value, int64_t index,
                                  int64_t count) {
  if (value == NULL)
    return kgpc_alloc_empty_unicodestring();

  int64_t len = kgpc_unicode_known_length(value);
  if (index < 1 || index > len)
    return kgpc_alloc_empty_unicodestring();

  if (count < 0)
    count = 0;

  int64_t start = index - 1;
  int64_t available = len - start;
  int64_t to_copy = count;
  if (to_copy > available)
    to_copy = available;

  if (to_copy <= 0)
    return kgpc_alloc_empty_unicodestring();

  uint16_t *result = kgpc_alloc_empty_unicodestring();
  size_t data_bytes = (size_t)to_copy * sizeof(uint16_t) + sizeof(uint16_t);
  KgpcStringHeader *hdr =
      (KgpcStringHeader *)malloc(sizeof(KgpcStringHeader) + data_bytes);
  if (hdr == NULL)
    return result;

  hdr->codepage = 1200;
  hdr->elementsize = 2;
  hdr->refcount = 1;
  hdr->length = to_copy;
  result = (uint16_t *)(hdr + 1);
  memcpy(result, value + start, (size_t)to_copy * sizeof(uint16_t));
  result[to_copy] = 0;
  kgpc_string_set_insert((char *)result);
  return result;
}

/* Copy from ShortString (length byte at index 0, chars at 1..255) */
char *kgpc_shortstring_copy(const char *value, int64_t index, int64_t count) {
  if (value == NULL)
    return kgpc_alloc_empty_string();

  /* ShortString has length byte at position 0 */
  size_t len = (unsigned char)value[0];
  const char *chars = value + 1; /* Actual characters start at position 1 */

  if (index < 1 || index > (int64_t)len)
    return kgpc_alloc_empty_string();

  if (count < 0)
    count = 0;

  size_t start = (size_t)(index - 1);
  size_t available = len - start;
  size_t to_copy = (size_t)count;
  if (to_copy > available)
    to_copy = available;

  char *result = kgpc_string_alloc_with_length(to_copy);
  if (result == NULL)
    return kgpc_alloc_empty_string();

  if (to_copy > 0)
    memcpy(result, chars + start, to_copy);
  return result;
}

int64_t kgpc_string_compare(const char *lhs, const char *rhs) {
  if (lhs == NULL)
    lhs = "";
  if (rhs == NULL)
    rhs = "";
  if (kgpc_env_flag("KGPC_DEBUG_STRTOFLOAT"))
    fprintf(stderr, "[kgpc] strcmp lhs='%s' rhs='%s'\n", lhs, rhs);

  size_t lhs_len = kgpc_string_known_length(lhs);
  size_t rhs_len = kgpc_string_known_length(rhs);
  size_t min_len = (lhs_len < rhs_len) ? lhs_len : rhs_len;

  if (min_len > 0) {
    int cmp = memcmp(lhs, rhs, min_len);
    if (cmp != 0)
      return (int64_t)cmp;
  }

  if (lhs_len < rhs_len)
    return -1;
  if (lhs_len > rhs_len)
    return 1;
  return 0;
}

static size_t kgpc_char_array_bounded_length(const char *value,
                                             size_t max_len) {
  if (value == NULL || max_len == 0)
    return 0;
  const void *pos = memchr(value, '\0', max_len);
  if (pos == NULL)
    return max_len;
  return (size_t)((const char *)pos - value);
}

int64_t kgpc_char_array_compare(const char *array_value, size_t array_len,
                                const char *rhs) {
  if (array_value == NULL)
    array_value = "";
  if (rhs == NULL)
    rhs = "";

  size_t lhs_len = kgpc_char_array_bounded_length(array_value, array_len);
  size_t rhs_len = kgpc_string_known_length(rhs);
  size_t min_len = (lhs_len < rhs_len) ? lhs_len : rhs_len;

  if (min_len > 0) {
    int cmp = memcmp(array_value, rhs, min_len);
    if (cmp != 0)
      return (int64_t)cmp;
  }

  if (lhs_len < rhs_len)
    return -1;
  if (lhs_len > rhs_len)
    return 1;
  return 0;
}

int64_t kgpc_char_array_compare_full(const char *array_value, size_t array_len,
                                     const char *rhs) {
  if (array_value == NULL)
    array_value = "";
  if (rhs == NULL)
    rhs = "";

  size_t lhs_len = array_len;
  size_t rhs_len = kgpc_string_known_length(rhs);
  size_t min_len = (lhs_len < rhs_len) ? lhs_len : rhs_len;

  if (min_len > 0) {
    int cmp = memcmp(array_value, rhs, min_len);
    if (cmp != 0)
      return (int64_t)cmp;
  }

  if (lhs_len < rhs_len)
    return -1;
  if (lhs_len > rhs_len)
    return 1;
  return 0;
}

int64_t kgpc_char_array_compare_array(const char *lhs, size_t lhs_len,
                                      const char *rhs, size_t rhs_len) {
  if (lhs == NULL)
    lhs = "";
  if (rhs == NULL)
    rhs = "";

  size_t lhs_eff = kgpc_char_array_bounded_length(lhs, lhs_len);
  size_t rhs_eff = kgpc_char_array_bounded_length(rhs, rhs_len);
  size_t min_len = (lhs_eff < rhs_eff) ? lhs_eff : rhs_eff;

  if (min_len > 0) {
    int cmp = memcmp(lhs, rhs, min_len);
    if (cmp != 0)
      return (int64_t)cmp;
  }

  if (lhs_eff < rhs_eff)
    return -1;
  if (lhs_eff > rhs_eff)
    return 1;
  return 0;
}

void kgpc_string_assign_from_shortstring(char **target, const char *ss) {
  if (target == NULL)
    return;
  char *existing = *target;
  if (existing != NULL)
    kgpc_string_release(existing);
  if (ss == NULL) {
    *target = kgpc_alloc_empty_string();
    return;
  }
  unsigned char len = (unsigned char)ss[0];
  char *copy = kgpc_string_alloc_with_length(len);
  if (copy == NULL) {
    *target = kgpc_alloc_empty_string();
    return;
  }
  if (len > 0)
    memcpy(copy, ss + 1, len);
  *target = copy;
}

void kgpc_string_assign_from_char_array(char **target, const char *value,
                                        size_t max_len) {
  if (target == NULL)
    return;

  char *existing = *target;
  if (existing != NULL)
    kgpc_string_release(existing);

  if (value == NULL || max_len == 0) {
    *target = kgpc_alloc_empty_string();
    return;
  }

  size_t len = kgpc_char_array_bounded_length(value, max_len);
  char *copy = kgpc_string_alloc_with_length(len);
  if (copy == NULL) {
    *target = kgpc_alloc_empty_string();
    return;
  }

  if (len > 0)
    memcpy(copy, value, len);
  *target = copy;
}

void kgpc_string_assign_from_unicodestring(char **target,
                                           const uint16_t *value) {
  if (target == NULL)
    return;

  if (value == NULL) {
    kgpc_string_assign(target, "");
    return;
  }

  KgpcStringHeader *hdr = kgpc_string_header((const char *)value);
  if (hdr != NULL && hdr->elementsize == 1) {
    kgpc_string_assign(target, (const char *)value);
    return;
  }

  int64_t len = kgpc_unicode_known_length(value);
  char *ansi = NULL;
  typedef void (*Unicode2AnsiProc)(const uint16_t *, char **, int32_t, int64_t);
  Unicode2AnsiProc conv = (Unicode2AnsiProc)widestringmanager[19];
  int32_t cp = DefaultSystemCodePage;
  if (conv != NULL)
    conv(value, &ansi, cp, len);
  else
    kgpc_default_unicode2ansi_move(value, &ansi, cp, len);

  kgpc_string_assign_take(target, ansi);
}

char *kgpc_string_from_unicodestring(const uint16_t *value) {
  char *result = NULL;
  kgpc_string_assign_from_unicodestring(&result, value);
  return result;
}

uint16_t *kgpc_unicodestring_from_string(const char *value) {
  if (value == NULL)
    return kgpc_alloc_empty_unicodestring();

  KgpcStringHeader *hdr = kgpc_string_header(value);
  if (hdr != NULL && hdr->elementsize == 2) {
    kgpc_string_retain(value);
    return (uint16_t *)value;
  }

  uint16_t *result = NULL;
  int64_t len = kgpc_string_length(value);
  typedef void (*Ansi2UnicodeProc)(const char *, int32_t, uint16_t **, int64_t);
  Ansi2UnicodeProc conv = (Ansi2UnicodeProc)widestringmanager[20];
  int32_t cp = DefaultSystemCodePage;
  if (conv != NULL)
    conv(value, cp, &result, len);
  else
    kgpc_default_ansi2unicode_move(value, cp, &result, len);

  if (result == NULL)
    return kgpc_alloc_empty_unicodestring();
  return result;
}

void kgpc_unicodestring_assign_from_string(uint16_t **target,
                                           const char *value) {
  if (target == NULL)
    return;

  uint16_t *converted = kgpc_unicodestring_from_string(value);
  uint16_t *current = *target;
  if (current != NULL)
    kgpc_string_release((char *)current);
  *target = converted;
}

void kgpc_unicodestring_assign(uint16_t **target, const uint16_t *value) {
  if (target == NULL)
    return;

  uint16_t *current = *target;
  if (current == value)
    return;

  if (value == NULL) {
    if (current != NULL)
      kgpc_string_release((char *)current);
    *target = kgpc_alloc_empty_unicodestring();
    return;
  }

  KgpcStringHeader *hdr = kgpc_string_header((const char *)value);
  if (hdr == NULL || hdr->elementsize == 1) {
    kgpc_unicodestring_assign_from_string(target, (const char *)value);
    return;
  }

  kgpc_string_retain((const char *)value);
  if (current != NULL)
    kgpc_string_release((char *)current);
  *target = (uint16_t *)value;
}

char *kgpc_strpas_string(const char *p) {
  if (p == NULL)
    return kgpc_alloc_empty_string();
  return kgpc_string_duplicate(p);
}

char *kgpc_strpas_len_string(const char *p, int64_t length) {
  if (p == NULL || length <= 0)
    return kgpc_alloc_empty_string();
  /* Truncate at first NUL, like a C-string — StrPas copies until NUL */
  size_t actual = strnlen(p, (size_t)length);
  return kgpc_string_duplicate_length(p, actual);
}

void kgpc_strpas(char *dest, const char *p) {
  kgpc_string_to_shortstring(dest, p != NULL ? p : "", 256);
}

void kgpc_strpas_len(char *dest, const char *p, int64_t length) {
  if (p == NULL || length <= 0) {
    kgpc_string_to_shortstring(dest, "", 256);
    return;
  }
  size_t actual = strnlen(p, (size_t)length);
  char *tmp = kgpc_string_duplicate_length(p, actual);
  kgpc_string_to_shortstring(dest, tmp, 256);
  kgpc_string_release(tmp);
}

static int64_t kgpc_pos_internal(const char *hay, size_t hay_len,
                                 const char *needle, size_t needle_len,
                                 int64_t start_index) {
  if (needle_len == 0) {
    if (start_index < 1)
      start_index = 1;
    return (size_t)start_index > hay_len + 1 ? 0 : start_index;
  }
  if (needle_len > hay_len)
    return 0;

  if (start_index < 1)
    start_index = 1;
  size_t start = (size_t)(start_index - 1);
  if (start >= hay_len)
    return 0;

  for (size_t i = start; i + needle_len <= hay_len; ++i) {
    if (memcmp(hay + i, needle, needle_len) == 0)
      return (int64_t)(i + 1);
  }
  return 0;
}

int64_t kgpc_string_pos_ca(unsigned char ch, const char *value) {
  char needle[1] = {(char)ch};
  return kgpc_pos_internal(value ? value : "",
                           kgpc_string_known_length(value ? value : ""), needle,
                           1, 1);
}

int64_t kgpc_string_pos_cs(unsigned char ch, const char *value) {
  char needle[1] = {(char)ch};
  size_t hay_len = value ? (size_t)(unsigned char)value[0] : 0;
  return kgpc_pos_internal(value ? value + 1 : "", hay_len, needle, 1, 1);
}

int64_t kgpc_string_pos_cc(unsigned char substr, unsigned char value) {
  char needle[1] = {(char)substr};
  char hay[1] = {(char)value};
  return kgpc_pos_internal(hay, 1, needle, 1, 1);
}

int64_t kgpc_string_pos_ac(const char *substr, unsigned char value) {
  char hay[1] = {(char)value};
  size_t needle_len = kgpc_string_known_length(substr ? substr : "");
  return kgpc_pos_internal(hay, 1, substr ? substr : "", needle_len, 1);
}

int64_t kgpc_string_pos_sc(const char *substr, unsigned char value) {
  if (substr == NULL)
    return 0;
  char hay[1] = {(char)value};
  size_t needle_len = (size_t)(unsigned char)substr[0];
  return kgpc_pos_internal(hay, 1, substr + 1, needle_len, 1);
}

int64_t kgpc_string_pos_sa(const char *substr, const char *value) {
  if (substr == NULL)
    return 0;
  size_t hay_len = kgpc_string_known_length(value ? value : "");
  size_t needle_len = (size_t)(unsigned char)substr[0];
  return kgpc_pos_internal(value ? value : "", hay_len, substr + 1, needle_len,
                           1);
}

int64_t kgpc_string_pos_as(const char *substr, const char *value) {
  size_t hay_len = value ? (size_t)(unsigned char)value[0] : 0;
  size_t needle_len = kgpc_string_known_length(substr ? substr : "");
  return kgpc_pos_internal(value ? value + 1 : "", hay_len,
                           substr ? substr : "", needle_len, 1);
}

int64_t kgpc_string_pos_ss(const char *substr, const char *value) {
  if (substr == NULL)
    return 0;
  size_t hay_len = value ? (size_t)(unsigned char)value[0] : 0;
  size_t needle_len = (size_t)(unsigned char)substr[0];
  return kgpc_pos_internal(value ? value + 1 : "", hay_len, substr + 1,
                           needle_len, 1);
}

int64_t kgpc_string_pos(const char *substr, const char *value) {
  size_t hay_len = kgpc_string_known_length(value ? value : "");
  size_t needle_len = kgpc_string_known_length(substr ? substr : "");
  return kgpc_pos_internal(value ? value : "", hay_len, substr ? substr : "",
                           needle_len, 1);
}

int64_t kgpc_string_pos_sa_from(const char *substr, const char *value,
                                int64_t start_index) {
  if (substr == NULL)
    return 0;
  size_t hay_len = kgpc_string_known_length(value ? value : "");
  size_t needle_len = (size_t)(unsigned char)substr[0];
  return kgpc_pos_internal(value ? value : "", hay_len, substr + 1, needle_len,
                           start_index);
}

int64_t kgpc_string_pos_as_from(const char *substr, const char *value,
                                int64_t start_index) {
  size_t hay_len = value ? (size_t)(unsigned char)value[0] : 0;
  size_t needle_len = kgpc_string_known_length(substr ? substr : "");
  return kgpc_pos_internal(value ? value + 1 : "", hay_len,
                           substr ? substr : "", needle_len, start_index);
}

int64_t kgpc_string_pos_ss_from(const char *substr, const char *value,
                                int64_t start_index) {
  if (substr == NULL)
    return 0;
  size_t hay_len = value ? (size_t)(unsigned char)value[0] : 0;
  size_t needle_len = (size_t)(unsigned char)substr[0];
  return kgpc_pos_internal(value ? value + 1 : "", hay_len, substr + 1,
                           needle_len, start_index);
}

int64_t kgpc_string_pos_from(const char *substr, const char *value,
                             int64_t start_index) {
  size_t hay_len = kgpc_string_known_length(value ? value : "");
  size_t needle_len = kgpc_string_known_length(substr ? substr : "");
  return kgpc_pos_internal(value ? value : "", hay_len, substr ? substr : "",
                           needle_len, start_index);
}

int64_t kgpc_string_pos_ca_from(unsigned char ch, const char *value,
                                int64_t start_index) {
  char needle[1] = {(char)ch};
  return kgpc_pos_internal(value ? value : "",
                           kgpc_string_known_length(value ? value : ""), needle,
                           1, start_index);
}

int64_t kgpc_string_pos_cs_from(unsigned char ch, const char *value,
                                int64_t start_index) {
  char needle[1] = {(char)ch};
  size_t hay_len = value ? (size_t)(unsigned char)value[0] : 0;
  return kgpc_pos_internal(value ? value + 1 : "", hay_len, needle, 1,
                           start_index);
}

int64_t kgpc_string_pos_cc_from(unsigned char substr, unsigned char value,
                                int64_t start_index) {
  char needle[1] = {(char)substr};
  char hay[1] = {(char)value};
  return kgpc_pos_internal(hay, 1, needle, 1, start_index);
}

int64_t kgpc_string_pos_ac_from(const char *substr, unsigned char value,
                                int64_t start_index) {
  char hay[1] = {(char)value};
  size_t needle_len = kgpc_string_known_length(substr ? substr : "");
  return kgpc_pos_internal(hay, 1, substr ? substr : "", needle_len,
                           start_index);
}

int64_t kgpc_string_pos_sc_from(const char *substr, unsigned char value,
                                int64_t start_index) {
  if (substr == NULL)
    return 0;
  char hay[1] = {(char)value};
  size_t needle_len = (size_t)(unsigned char)substr[0];
  return kgpc_pos_internal(hay, 1, substr + 1, needle_len, start_index);
}

static int kgpc_is_path_delim_char(char ch) { return ch == '/' || ch == '\\'; }

static const char *kgpc_find_last_path_delim(const char *path) {
  if (path == NULL)
    return NULL;
  const char *last = NULL;
  for (const char *ptr = path; *ptr != '\0'; ++ptr) {
    if (kgpc_is_path_delim_char(*ptr))
      last = ptr;
  }
  return last;
}

char *kgpc_extract_file_path(const char *filename) {
  if (filename == NULL)
    return kgpc_alloc_empty_string();
  const char *last = kgpc_find_last_path_delim(filename);
  if (last == NULL)
    return kgpc_alloc_empty_string();
  size_t length = (size_t)(last - filename) + 1;
  return kgpc_string_duplicate_length(filename, length);
}

char *kgpc_extract_file_name(const char *filename) {
  if (filename == NULL)
    return kgpc_alloc_empty_string();
  const char *last = kgpc_find_last_path_delim(filename);
  if (last == NULL)
    return kgpc_string_duplicate(filename);
  return kgpc_string_duplicate(last + 1);
}

char *kgpc_extract_file_ext(const char *filename) {
  if (filename == NULL)
    return kgpc_alloc_empty_string();
  size_t len = strlen(filename);
  const char *start = filename;
  const char *limit = kgpc_find_last_path_delim(filename);
  if (limit != NULL)
    start = limit + 1;
  const char *ptr = filename + len;
  while (ptr > start) {
    --ptr;
    if (kgpc_is_path_delim_char(*ptr))
      break;
    if (*ptr == '.')
      return kgpc_string_duplicate(ptr);
  }
  return kgpc_alloc_empty_string();
}

char *kgpc_change_file_ext(const char *filename, const char *extension) {
  if (filename == NULL)
    return kgpc_alloc_empty_string();
  size_t len = strlen(filename);
  const char *start = filename;
  const char *limit = kgpc_find_last_path_delim(filename);
  if (limit != NULL)
    start = limit + 1;
  const char *ptr = filename + len;
  const char *dot = NULL;
  while (ptr > start) {
    --ptr;
    if (kgpc_is_path_delim_char(*ptr))
      break;
    if (*ptr == '.') {
      dot = ptr;
      break;
    }
  }
  size_t base_len = dot ? (size_t)(dot - filename) : len;
  size_t ext_len = (extension != NULL) ? strlen(extension) : 0;
  char *result = kgpc_string_alloc_with_length(base_len + ext_len);
  if (result == NULL)
    return kgpc_alloc_empty_string();
  if (base_len > 0)
    memcpy(result, filename, base_len);
  if (ext_len > 0 && extension != NULL)
    memcpy(result + base_len, extension, ext_len);
  return result;
}

char *kgpc_exclude_trailing_path_delim(const char *path) {
  if (path == NULL)
    return kgpc_alloc_empty_string();
  size_t len = strlen(path);
  if (len == 0)
    return kgpc_alloc_empty_string();
  size_t end = len;
  while (end > 0 && kgpc_is_path_delim_char(path[end - 1]))
    --end;
  if (end == 0)
    return kgpc_alloc_empty_string();
  if (end == len)
    return kgpc_string_duplicate(path);
  return kgpc_string_duplicate_length(path, end);
}

static long long kgpc_val_error_position(const char *text,
                                         const char *error_ptr) {
  if (text == NULL || error_ptr == NULL)
    return 1;
  return (long long)((error_ptr - text) + 1);
}

static const char *kgpc_val_skip_trailing_whitespace(const char *ptr) {
  if (ptr == NULL)
    return NULL;
  while (*ptr != '\0' && isspace((unsigned char)*ptr))
    ++ptr;
  return ptr;
}

/* Detect Pascal integer prefix: $/$x=hex, %=binary, &=octal; return base and
 * skip prefix */
static int kgpc_val_detect_base(const char **ptr) {
  if (**ptr == '$' || **ptr == 'x' || **ptr == 'X') {
    (*ptr)++;
    return 16;
  }
  if (**ptr == '%') {
    (*ptr)++;
    return 2;
  }
  if (**ptr == '&') {
    (*ptr)++;
    return 8;
  }
  if (**ptr == '0' && ((*ptr)[1] == 'x' || (*ptr)[1] == 'X')) {
    (*ptr) += 2;
    return 16;
  }
  return 10;
}

static long long kgpc_val_parse_integer(const char *text, long long min_value,
                                        long long max_value,
                                        long long *out_value) {
  if (text == NULL)
    text = "";

  const char *ptr = text;
  while (*ptr != '\0' && isspace((unsigned char)*ptr))
    ++ptr;

  int negative = 0;
  if (*ptr == '-') {
    negative = 1;
    ++ptr;
  } else if (*ptr == '+') {
    ++ptr;
  }

  int base = kgpc_val_detect_base(&ptr);

  errno = 0;
  char *endptr = NULL;
  unsigned long long uvalue = strtoull(ptr, &endptr, base);
  if (endptr == ptr)
    return 1;

  /* strtoull only returns ERANGE on values exceeding ULLONG_MAX, so check
   * that the magnitude fits the requested signed range before converting to
   * long long.  Without this, positive inputs above LLONG_MAX silently wrap
   * to negative values (e.g. "9223372036854775808" became LLONG_MIN with
   * code=0), and similarly for negative overflow. */
  unsigned long long max_magnitude;
  if (negative)
    max_magnitude = (unsigned long long)(-(min_value + 1)) + 1ULL;
  else
    max_magnitude = (unsigned long long)max_value;

  if (errno == ERANGE || uvalue > max_magnitude)
    return kgpc_val_error_position(text, endptr);

  long long value;
  if (negative) {
    if (uvalue == max_magnitude && min_value == LLONG_MIN)
      value = LLONG_MIN;
    else
      value = -(long long)uvalue;
  } else
    value = (long long)uvalue;

  const char *rest = kgpc_val_skip_trailing_whitespace(endptr);
  if (rest != NULL && *rest != '\0')
    return kgpc_val_error_position(text, rest);

  if (out_value != NULL)
    *out_value = value;
  return 0;
}

static long long kgpc_val_parse_unsigned(const char *text,
                                         unsigned long long max_value,
                                         unsigned long long *out_value) {
  if (text == NULL)
    text = "";

  const char *ptr = text;
  while (*ptr != '\0' && isspace((unsigned char)*ptr))
    ++ptr;

  if (*ptr == '-')
    return kgpc_val_error_position(text, ptr);
  if (*ptr == '+')
    ++ptr;

  int base = kgpc_val_detect_base(&ptr);

  errno = 0;
  char *endptr = NULL;
  unsigned long long value = strtoull(ptr, &endptr, base);
  if (endptr == ptr)
    return 1;

  if (errno == ERANGE || value > max_value)
    return kgpc_val_error_position(text, endptr);

  const char *rest = kgpc_val_skip_trailing_whitespace(endptr);
  if (rest != NULL && *rest != '\0')
    return kgpc_val_error_position(text, rest);

  if (out_value != NULL)
    *out_value = value;
  return 0;
}

static long long kgpc_val_parse_real(const char *text, double *out_value) {
  if (text == NULL)
    text = "";

  errno = 0;
  char *endptr = NULL;
  double value = strtod(text, &endptr);
  if (endptr == text)
    return 1;

  if (errno == ERANGE)
    return kgpc_val_error_position(text, endptr);

  const char *rest = kgpc_val_skip_trailing_whitespace(endptr);
  if (rest != NULL && *rest != '\0')
    return kgpc_val_error_position(text, rest);

  if (out_value != NULL)
    *out_value = value;
  return 0;
}

long long kgpc_val_integer(const char *text, int32_t *out_value) {
  long long parsed = 0;
  long long code = kgpc_val_parse_integer(text, INT32_MIN, INT32_MAX, &parsed);
  if (out_value != NULL)
    *out_value = (int32_t)parsed;
  return code;
}

long long kgpc_val_longint(const char *text, int64_t *out_value) {
  long long parsed = 0;
  long long code = kgpc_val_parse_integer(text, INT64_MIN, INT64_MAX, &parsed);
  if (out_value != NULL)
    *out_value = parsed;
  return code;
}

long long kgpc_val_qword(const char *text, uint64_t *out_value) {
  unsigned long long parsed = 0;
  long long code = kgpc_val_parse_unsigned(text, ULLONG_MAX, &parsed);
  if (out_value != NULL)
    *out_value = (uint64_t)parsed;
  return code;
}

long long kgpc_val_real(const char *text, double *out_value) {
  double parsed = 0.0;
  long long code = kgpc_val_parse_real(text, &parsed);
  if (out_value != NULL)
    *out_value = parsed;
  return code;
}

long long kgpc_val_extended(const char *text, void *out_value) {
  double parsed = 0.0;
  long long code = kgpc_val_parse_real(text, &parsed);
  if (out_value != NULL) {
    long double ext = (long double)parsed;
    memcpy(out_value, &ext, 10);
  }
  return code;
}

/* ShortString versions of Val: take a ShortString pointer (length byte + chars)
 */
static const char *kgpc_shortstr_to_cstr(const unsigned char *ss, char *buf,
                                         int bufsize) {
  if (ss == NULL)
    return "";
  int len = ss[0];
  if (len >= bufsize)
    len = bufsize - 1;
  memcpy(buf, ss + 1, len);
  buf[len] = '\0';
  return buf;
}

long long kgpc_val_integer_ss(const unsigned char *ss, int32_t *out_value) {
  char buf[256];
  return kgpc_val_integer(kgpc_shortstr_to_cstr(ss, buf, sizeof(buf)),
                          out_value);
}

long long kgpc_val_longint_ss(const unsigned char *ss, int64_t *out_value) {
  char buf[256];
  return kgpc_val_longint(kgpc_shortstr_to_cstr(ss, buf, sizeof(buf)),
                          out_value);
}

long long kgpc_val_qword_ss(const unsigned char *ss, uint64_t *out_value) {
  char buf[256];
  return kgpc_val_qword(kgpc_shortstr_to_cstr(ss, buf, sizeof(buf)), out_value);
}

long long kgpc_val_real_ss(const unsigned char *ss, double *out_value) {
  char buf[256];
  return kgpc_val_real(kgpc_shortstr_to_cstr(ss, buf, sizeof(buf)), out_value);
}

long long kgpc_val_extended_ss(const unsigned char *ss, void *out_value) {
  char buf[256];
  return kgpc_val_extended(kgpc_shortstr_to_cstr(ss, buf, sizeof(buf)),
                           out_value);
}
