#ifndef KGPC_RUNTIME_INTERNAL_H
#define KGPC_RUNTIME_INTERNAL_H

#include <stdio.h>
#include <stddef.h>
#include <stdint.h>
#include "Parser/SemanticCheck/HashTable/HashTable.h"

typedef enum {
  KGPC_BINARY_UNSPECIFIED = 0,
  KGPC_BINARY_INT32,
  KGPC_BINARY_CHAR,
  KGPC_BINARY_DOUBLE
} KGPCBinaryType;

typedef struct KGPCTextRec {
  int32_t handle;
  int32_t mode;
  int64_t bufsize;
  int64_t private_data;
  int64_t bufpos;
  int64_t bufend;
  char *bufptr;
  void *openfunc;
  void *inoutfunc;
  void *flushfunc;
  void *closefunc;
  unsigned char userdata[32];
  char name[256];
  char line_end[4];
  char buffer[256];
  uint16_t codepage;
  unsigned char _pad_fullname[2];
  void *fullname;
} KGPCTextRec;

_Static_assert(
    sizeof(KGPCTextRec) == 640,
    "KGPCTextRec size must be 640 to match TEXT_TYPE in SemCheck_sizeof.c");

typedef struct KGPCFileRec {
  int32_t handle;
  int32_t mode;
  int64_t recsize;
  unsigned char private_data[64];
  unsigned char userdata[32];
  char name[256];
  void *fullname;
} KGPCFileRec;

typedef struct KGPCFilePrivate {
  FILE *handle;
  KGPCBinaryType element_type;
  size_t element_size;
} KGPCFilePrivate;

/* FPC-compatible AnsiString header (TAnsiRec on x86-64).
   Compiled FPC RTL code accesses these fields directly via
   PAnsiRec(Pointer(S) - AnsiFirstOff), so the layout MUST match. */
typedef struct KgpcStringHeader {
  uint16_t codepage;
  uint16_t elementsize;
  int32_t refcount;
  int64_t length;
} KgpcStringHeader;

#define KGPC_FM_CLOSED 0xD7B0
#define KGPC_FM_INPUT 0xD7B1
#define KGPC_FM_OUTPUT 0xD7B2
#define KGPC_FM_INOUT 0xD7B3

extern void *widestringmanager[25];
extern int32_t DefaultSystemCodePage;

#if !defined(_WIN32)
#if defined(__linux__) || defined(__APPLE__) || defined(__FreeBSD__) ||        \
    defined(__NetBSD__) || defined(__OpenBSD__) || defined(__CYGWIN__)
#define KGPC_HAVE_GETDOMAINNAME 1
#endif
#endif

/* FPC-compatible tinterfaceentry (40 bytes on x86-64).
 * Layout matches FPC objpash.inc tinterfaceentry exactly. */
typedef struct kgpc_interface_entry {
  const void **iid_ref;    /* ^pguid: pointer to a pguid cell */
  const void *vtable;      /* interface vtable (unused for now) */
  uint64_t ioffset;        /* byte offset from instance to intf pointer */
  const void **iidstr_ref; /* ^pshortstring (unused for now) */
  uint32_t itype;          /* tinterfaceentrytype: 0 = etStandard */
  uint32_t _padding;
} kgpc_interface_entry;

/* FPC-compatible tinterfacetable */
typedef struct kgpc_interface_table {
  uint64_t entry_count;
  kgpc_interface_entry entries[]; /* flexible array */
} kgpc_interface_table;

typedef struct kgpc_class_typeinfo {
  const struct kgpc_class_typeinfo *parent;
  const char *class_name;
  const void *vmt;
  const kgpc_interface_table *interfaces;
  int num_interfaces;
} kgpc_class_typeinfo;

typedef struct {
  void *data;
  int64_t length;
} kgpc_dynarray_descriptor_t;

char *kgpc_alloc_empty_string(void);
char *kgpc_string_duplicate(const char *value);
void kgpc_set_codepage_string(char **value, uint16_t codepage, int convert);
int kgpc_rtti_is(const kgpc_class_typeinfo *value_type,
                 const kgpc_class_typeinfo *target_type);
void kgpc_rtti_check_cast(const kgpc_class_typeinfo *value_type,
                          const kgpc_class_typeinfo *target_type);
const void *kgpc_class_parent(const void *self);
const char *kgpc_class_name(const void *self);
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
void __kgpc_init_interface_vtables(void *instance);
typedef void (*kgpc_method_ptr)(void);
kgpc_method_ptr __kgpc_resolve_intf_method(void *self,
                                           const void *interface_guid,
                                           int method_index);
int kgpc_get_interface(const void *self, const void *guid, void **out_intf);

/* Console / keyboard helpers used by KGPC RTL units (Crt/Keyboard). */
void kgpc_clrscr(void);
void kgpc_textcolor(int color);
int kgpc_crt_screen_width(void);
int kgpc_crt_screen_height(void);
int kgpc_keyboard_poll(void);
int kgpc_keyboard_get(void);
int kgpc_keyboard_read_char(void);

/* Promoted helpers from runtime.c: before-cluster statics */
void kgpc_reinit_stdio(void);
void kgpc_textrec_init_defaults(KGPCTextRec *file);
FILE *kgpc_textrec_get_stream(KGPCTextRec *file, FILE *default_stream);
void kgpc_textrec_set_stream(KGPCTextRec *file, FILE *stream);
FILE *kgpc_text_input_stream(KGPCTextRec *file);
void kgpc_copy_name(char *dest, size_t dest_size, const char *src);
FILE *kgpc_get_write_stream(KGPCTextRec *file, int *width);
void kgpc_write_string(KGPCTextRec *file, int width, const char *value);
int kgpc_env_flag(const char *name);
void kgpc_default_unicode2ansi_move(const uint16_t *source, char **dest,
                                    int32_t cp, int64_t len);
void kgpc_default_ansi2unicode_move(const char *source, int32_t cp,
                                    uint16_t **dest, int64_t len);

/* Promoted helpers from runtime_string.c: cluster statics */
void kgpc_string_set_insert(const void *value);
int kgpc_string_is_managed(const char *value);
KgpcStringHeader *kgpc_string_header(const char *value);
size_t kgpc_string_known_length(const char *value);
char *kgpc_string_alloc_with_length(size_t length);
void kgpc_string_release(char *value);
void *kgpc_memory_manager_allocmem(uintptr_t size);
void *kgpc_memory_manager_getmem(uintptr_t size);
void kgpc_memory_manager_freemem(void *ptr);

/* Non-static cluster functions needed by runtime.c */
void kgpc_init_args(int argc, char **argv, char **envp);
int64_t kgpc_widechar_length(const uint16_t *value);
uint16_t *kgpc_unicodestring_from_string(const char *value);
void kgpc_string_setlength(char **target, int64_t new_length);
void kgpc_string_to_shortstring(char *dest, const char *src, size_t dest_size);

#endif /* KGPC_RUNTIME_INTERNAL_H */
