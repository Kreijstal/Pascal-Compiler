#ifndef KGPC_RUNTIME_INTERNAL_H
#define KGPC_RUNTIME_INTERNAL_H

#include <stddef.h>
#include <stdint.h>
#include "Parser/SemanticCheck/HashTable/HashTable.h"

#if !defined(_WIN32)
#if defined(__linux__) || defined(__APPLE__) || defined(__FreeBSD__) || \
    defined(__NetBSD__) || defined(__OpenBSD__) || defined(__CYGWIN__)
#define KGPC_HAVE_GETDOMAINNAME 1
#endif
#endif

typedef struct kgpc_class_typeinfo
{
    const struct kgpc_class_typeinfo *parent;
    const char *class_name;
    const void *vmt;
} kgpc_class_typeinfo;

char *kgpc_alloc_empty_string(void);
char *kgpc_string_duplicate(const char *value);
void kgpc_set_codepage_string(char **value, uint16_t codepage, int convert);
int kgpc_rtti_is(const kgpc_class_typeinfo *value_type,
    const kgpc_class_typeinfo *target_type);
void kgpc_rtti_check_cast(const kgpc_class_typeinfo *value_type,
    const kgpc_class_typeinfo *target_type);
int kgpc_ioresult_get_and_clear(void);
void kgpc_ioresult_set(int value);
int kgpc_ioresult_peek(void);
char *kgpc_get_current_dir(void);
int kgpc_set_current_dir(const char *path);
char *kgpc_get_environment_variable(const char *name);
int kgpc_set_environment_variable(const char *name, const char *value);
int kgpc_unset_environment_variable(const char *name);
int64_t kgpc_get_process_id(void);
uintptr_t kgpc_load_library(const char *path);
uintptr_t kgpc_get_proc_address(uintptr_t handle, const char *symbol);
int kgpc_free_library(uintptr_t handle);
int kgpc_directory_create(const char *path);
int kgpc_directory_remove(const char *path);
int kgpc_file_rename(const char *old_path, const char *new_path);
void *__kgpc_default_create(size_t class_size, const void *vmt_ptr);

/* Console / keyboard helpers used by KGPC RTL units (Crt/Keyboard). */
void kgpc_clrscr(void);
void kgpc_textcolor(int color);
int kgpc_crt_screen_width(void);
int kgpc_crt_screen_height(void);
int kgpc_keyboard_poll(void);
int kgpc_keyboard_get(void);
int kgpc_keyboard_read_char(void);

#endif /* KGPC_RUNTIME_INTERNAL_H */
