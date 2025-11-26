#ifndef GPC_RUNTIME_INTERNAL_H
#define GPC_RUNTIME_INTERNAL_H

#include <stddef.h>
#include <stdint.h>
#include "Parser/SemanticCheck/HashTable/HashTable.h"

#if !defined(_WIN32)
#if defined(__linux__) || defined(__APPLE__) || defined(__FreeBSD__) || \
    defined(__NetBSD__) || defined(__OpenBSD__) || defined(__CYGWIN__)
#define GPC_HAVE_GETDOMAINNAME 1
#endif
#endif

typedef struct gpc_class_typeinfo
{
    const struct gpc_class_typeinfo *parent;
    const char *class_name;
    const void *vmt;
} gpc_class_typeinfo;

char *gpc_alloc_empty_string(void);
char *gpc_string_duplicate(const char *value);
int gpc_rtti_is(const gpc_class_typeinfo *value_type,
    const gpc_class_typeinfo *target_type);
void gpc_rtti_check_cast(const gpc_class_typeinfo *value_type,
    const gpc_class_typeinfo *target_type);
int gpc_ioresult_get_and_clear(void);
void gpc_ioresult_set(int value);
int gpc_ioresult_peek(void);
char *gpc_get_current_dir(void);
int gpc_set_current_dir(const char *path);
char *gpc_get_environment_variable(const char *name);
int gpc_set_environment_variable(const char *name, const char *value);
int gpc_unset_environment_variable(const char *name);
int64_t gpc_get_process_id(void);
uintptr_t gpc_load_library(const char *path);
uintptr_t gpc_get_proc_address(uintptr_t handle, const char *symbol);
int gpc_free_library(uintptr_t handle);
int gpc_directory_create(const char *path);
int gpc_directory_remove(const char *path);
int gpc_file_rename(const char *old_path, const char *new_path);
void *__gpc_default_create(size_t class_size, const void *vmt_ptr);

#endif /* GPC_RUNTIME_INTERNAL_H */
