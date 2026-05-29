/**
 * @file runtime_internal.h
 * @brief Private declarations for the KGPC runtime library.
 *
 * Not part of the public runtime ABI — only translation units in
 * `KGPC/runtime*.c` should include this.  Defines the layouts that
 * KGPC-emitted code interoperates with (TextRec, FileRec, AnsiString
 * header, class-typeinfo, interface-table) plus the helper functions
 * shared between the various `runtime_*.c` files.
 */
#ifndef KGPC_RUNTIME_INTERNAL_H
#define KGPC_RUNTIME_INTERNAL_H

#include "Parser/SemanticCheck/HashTable/HashTable.h"
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

/** @brief Element-type tag used by binary file operations. */
typedef enum {
  KGPC_BINARY_UNSPECIFIED = 0,
  KGPC_BINARY_INT32,
  KGPC_BINARY_CHAR,
  KGPC_BINARY_DOUBLE
} KGPCBinaryType;

/**
 * @brief Runtime representation of a Pascal `Text` file variable.
 *
 * Must stay layout-compatible with FPC's `TextRec` (640 bytes on
 * x86_64 Linux).  See `KGPC/textrec_layout.h` for the C-only mirror
 * and the `_Static_assert` that locks the byte offsets.
 */
#if defined(_WIN64) || defined(_WIN32) || defined(KGPC_WIN64_ABI)
/* Win64: FPC's THandle = QWord (8 bytes) per rtl/win/sysosh.inc.  The
 * Handle field grows by 4 bytes and 4 bytes of padding before BufSize
 * push the total layout to 648 bytes.  KGPC_WIN64_ABI is defined by the
 * build on windows hosts whose gcc does not predefine _WIN32 (MSYS), where
 * the compiled code still targets Win64. */
typedef struct KGPCTextRec {
  int64_t handle;
  int32_t mode;
  uint32_t _pad_bufsize;
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
    sizeof(KGPCTextRec) == 648,
    "KGPCTextRec size must be 648 on Win64 to match TEXT_TYPE in SemCheck_sizeof.c");
#else
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
#endif

/** @brief Runtime representation of a Pascal typed/untyped binary `File` variable. */
typedef struct KGPCFileRec {
  int32_t handle;
  int32_t mode;
  int64_t recsize;
  unsigned char private_data[64];
  unsigned char userdata[32];
  char name[256];
  void *fullname;
} KGPCFileRec;

/** @brief Internal book-keeping stored in @c KGPCFileRec::private_data. */
typedef struct KGPCFilePrivate {
  FILE *handle;
  KGPCBinaryType element_type;
  size_t element_size;
} KGPCFilePrivate;

/**
 * @brief AnsiString record header (FPC-compatible `TAnsiRec` on x86-64).
 *
 * Compiled FPC RTL code accesses these fields directly via
 * `PAnsiRec(Pointer(S) - AnsiFirstOff)`, so the layout MUST match
 * upstream.
 */
typedef struct KgpcStringHeader {
  uint16_t codepage;
  uint16_t elementsize;
  int32_t refcount;
  int64_t length;
} KgpcStringHeader;

/** @brief `TextRec.mode` sentinel: file is currently closed. */
#define KGPC_FM_CLOSED 0xD7B0
/** @brief `TextRec.mode` sentinel: file is open for reading. */
#define KGPC_FM_INPUT 0xD7B1
/** @brief `TextRec.mode` sentinel: file is open for writing. */
#define KGPC_FM_OUTPUT 0xD7B2
/** @brief `TextRec.mode` sentinel: file is open for read+write. */
#define KGPC_FM_INOUT 0xD7B3

/** @brief Active widestring manager vtable (initialised by sysutils). */
extern void *widestringmanager[25];
/** @brief Process default codepage (`DefaultSystemCodePage` in FPC RTL). */
extern int32_t DefaultSystemCodePage;

#if !defined(_WIN32)
#if defined(__linux__) || defined(__APPLE__) || defined(__FreeBSD__) ||        \
    defined(__NetBSD__) || defined(__OpenBSD__) || defined(__CYGWIN__)
#define KGPC_HAVE_GETDOMAINNAME 1
#endif
#endif

/**
 * @brief One entry of the per-class interface-implementation table.
 *
 * Layout matches FPC `objpash.inc` `tinterfaceentry` exactly (40 bytes
 * on x86-64).
 */
typedef struct kgpc_interface_entry {
  const void **iid_ref;    /**< `^pguid`: pointer to a `pguid` cell. */
  const void *vtable;      /**< Interface vtable (unused for now). */
  uint64_t ioffset;        /**< Byte offset from instance to intf pointer. */
  const void **iidstr_ref; /**< `^pshortstring` (unused for now). */
  uint32_t itype;          /**< `tinterfaceentrytype`: 0 = `etStandard`. */
  uint32_t _padding;
} kgpc_interface_entry;

/** @brief FPC-compatible `tinterfacetable` — flexible-array `entries`. */
typedef struct kgpc_interface_table {
  uint64_t entry_count;
  kgpc_interface_entry entries[]; /* flexible array */
} kgpc_interface_table;

/** @brief Per-class RTTI block referenced from the VMT. */
typedef struct kgpc_class_typeinfo {
  const struct kgpc_class_typeinfo *parent;
  const char *class_name;
  const void *vmt;
  const kgpc_interface_table *interfaces;
  int num_interfaces;
} kgpc_class_typeinfo;

/** @brief Internal descriptor used by the dynamic-array runtime. */
typedef struct {
  void *data;
  int64_t length;
} kgpc_dynarray_descriptor_t;

/** @brief Allocate a zero-length managed AnsiString. */
char *kgpc_alloc_empty_string(void);
/** @brief Duplicate @p value into a freshly managed AnsiString. */
char *kgpc_string_duplicate(const char *value);

/**
 * @brief Tag the AnsiString in @p value with @p codepage.
 *
 * Transcodes the string contents in place when @p convert is non-zero;
 * otherwise just rewrites the codepage field of the header.
 */
void kgpc_set_codepage_string(char **value, uint16_t codepage, int convert);

/**
 * @brief RTTI is-a check: returns true if @p value_type derives from @p target_type.
 */
int kgpc_rtti_is(const kgpc_class_typeinfo *value_type,
                 const kgpc_class_typeinfo *target_type);

/**
 * @brief RTTI checked-cast: raises EClass on mismatch.
 *
 * Used by KGPC-emitted code for `as`/`is` operators.
 */
void kgpc_rtti_check_cast(const kgpc_class_typeinfo *value_type,
                          const kgpc_class_typeinfo *target_type);

/** @brief Return the parent class typeinfo of @p self, or NULL for TObject. */
const void *kgpc_class_parent(const void *self);

/** @brief Return the class name of @p self as a NUL-terminated string. */
const char *kgpc_class_name(const void *self);

/** @brief Pascal `IOResult`: read-and-clear the last I/O error code. */
int kgpc_ioresult_get_and_clear(void);

/** @brief Set the `IOResult` register (used by failed I/O operations). */
void kgpc_ioresult_set(int value);

/** @brief Read the `IOResult` register without clearing it. */
int kgpc_ioresult_peek(void);

/** @brief Pascal `GetCurrentDir`: malloc'd UTF-8 cwd. */
char *kgpc_get_current_dir(void);

/** @brief Pascal `SetCurrentDir`: returns 0 on success, errno on failure. */
int kgpc_set_current_dir(const char *path);

/** @brief Pascal `GetEnvironmentVariable`; returns NULL when unset. */
char *kgpc_get_environment_variable(const char *name);

/** @brief Pascal `SetEnvironmentVariable`; 0 on success. */
int kgpc_set_environment_variable(const char *name, const char *value);

/** @brief Remove @p name from the environment; 0 on success. */
int kgpc_unset_environment_variable(const char *name);

/** @brief Pascal `GetProcessID`. */
int64_t kgpc_get_process_id(void);

/** @brief Pascal `LoadLibrary`; returns 0 on failure. */
uintptr_t kgpc_load_library(const char *path);

/** @brief Pascal `GetProcAddress`; returns 0 on failure. */
uintptr_t kgpc_get_proc_address(uintptr_t handle, const char *symbol);

/** @brief Pascal `FreeLibrary`; returns 0 on success. */
int kgpc_free_library(uintptr_t handle);

/** @brief Pascal `CreateDir`; returns 0 on success, errno on failure. */
int kgpc_directory_create(const char *path);

/** @brief Pascal `RemoveDir`; returns 0 on success, errno on failure. */
int kgpc_directory_remove(const char *path);

/** @brief Pascal `RenameFile`; returns 0 on success, errno on failure. */
int kgpc_file_rename(const char *old_path, const char *new_path);

/**
 * @brief Default constructor used by KGPC-emitted code.
 *
 * Allocates @p class_size bytes, installs @p vmt_ptr, calls
 * @ref __kgpc_init_interface_vtables, and returns the instance.
 */
void *__kgpc_default_create(size_t class_size, const void *vmt_ptr);

/** @brief Populate the embedded interface VMT slots on @p instance. */
void __kgpc_init_interface_vtables(void *instance);

/** @brief Generic class-method pointer (no self / no args). */
typedef void (*kgpc_method_ptr)(void);

/**
 * @brief Resolve an interface call to its implementation method.
 *
 * @p interface_guid identifies the implemented interface; @p method_index
 * is the slot within the interface's VMT.
 */
kgpc_method_ptr __kgpc_resolve_intf_method(void *self,
                                           const void *interface_guid,
                                           int method_index);

/**
 * @brief Pascal `IInterface` `QueryInterface`: returns 0 (S_OK) on hit.
 *
 * On success @p out_intf is set to the interface pointer (already
 * AddRef'd; caller must Release).
 */
int kgpc_get_interface(const void *self, const void *guid, void **out_intf);

/** @name Crt / Keyboard helpers used by KGPC RTL units. @{ */
void kgpc_clrscr(void);            /**< `Crt.ClrScr`. */
void kgpc_textcolor(int color);    /**< `Crt.TextColor`. */
int kgpc_crt_screen_width(void);   /**< `Crt.ScreenWidth`. */
int kgpc_crt_screen_height(void);  /**< `Crt.ScreenHeight`. */
int kgpc_keyboard_poll(void);      /**< Non-blocking `KeyPressed`. */
int kgpc_keyboard_get(void);       /**< Blocking `ReadKey`. */
int kgpc_keyboard_read_char(void); /**< Read one char from stdin. */
/** @} */

/** @brief Reset stdio after `fork(2)` so the child has fresh buffers. */
void kgpc_reinit_stdio(void);

/** @brief Initialise the standard fields of a freshly-declared TextRec. */
void kgpc_textrec_init_defaults(KGPCTextRec *file);

/** @brief Get the FILE* stored in @p file, or @p default_stream if none. */
FILE *kgpc_textrec_get_stream(KGPCTextRec *file, FILE *default_stream);

/** @brief Store @p stream into @p file's private FILE slot. */
void kgpc_textrec_set_stream(KGPCTextRec *file, FILE *stream);

/** @brief Get the read-side FILE* for @p file (stdin fallback). */
FILE *kgpc_text_input_stream(KGPCTextRec *file);

/** @brief Copy @p src into @p dest with NUL termination, bounded by @p dest_size. */
void kgpc_copy_name(char *dest, size_t dest_size, const char *src);

/** @brief Get the write-side FILE* and dereference width into @p width. */
FILE *kgpc_get_write_stream(KGPCTextRec *file, int *width);

/** @brief Pascal `Write(file, value, width)` for AnsiString. */
void kgpc_write_string(KGPCTextRec *file, int width, const char *value);

/** @brief Read the integer value of env var @p name; 0 when unset. */
int kgpc_env_flag(const char *name);

/** @brief Default `Unicode2AnsiMove` widestring-manager hook. */
void kgpc_default_unicode2ansi_move(const uint16_t *source, char **dest,
                                    int32_t cp, int64_t len);

/** @brief Default `Ansi2UnicodeMove` widestring-manager hook. */
void kgpc_default_ansi2unicode_move(const char *source, int32_t cp,
                                    uint16_t **dest, int64_t len);

/** @brief Register @p value with the managed-string set for later release. */
void kgpc_string_set_insert(const void *value);

/** @brief Returns non-zero if @p value points into a managed-string allocation. */
int kgpc_string_is_managed(const char *value);

/** @brief Return the `(hdr + 1)`-style header of @p value, or NULL. */
KgpcStringHeader *kgpc_string_header(const char *value);

/** @brief Fast `Length(s)`: read the stored header length. */
size_t kgpc_string_known_length(const char *value);

/** @brief Allocate an empty managed AnsiString of @p length bytes (zero-filled). */
char *kgpc_string_alloc_with_length(size_t length);

/** @brief Decrement @p value's refcount and free if it drops to zero. */
void kgpc_string_release(char *value);

/** @brief Pascal `AllocMem(size)`: zeroed bytes via the active memory manager. */
void *kgpc_memory_manager_allocmem(uintptr_t size);

/** @brief Pascal `GetMem(size)` via the active memory manager. */
void *kgpc_memory_manager_getmem(uintptr_t size);

/** @brief Pascal `FreeMem(ptr)` via the active memory manager. */
void kgpc_memory_manager_freemem(void *ptr);

/**
 * @brief One-time init of `ParamStr`/`ParamCount`/`envp`.
 *
 * Called early by KGPC's `_start`-equivalent so that user code reading
 * these globals sees populated values.
 */
void kgpc_init_args(int argc, char **argv, char **envp);

/** @brief Compute the UTF-16 length of @p value (in code units, not bytes). */
int64_t kgpc_widechar_length(const uint16_t *value);

/** @brief Build a managed UnicodeString from an AnsiString. */
uint16_t *kgpc_unicodestring_from_string(const char *value);

/**
 * @brief Resize the AnsiString in @p target to @p new_length characters.
 *
 * Allocates a new copy on grow; truncates on shrink.  Refcount-aware.
 */
void kgpc_string_setlength(char **target, int64_t new_length);

/**
 * @brief Copy @p src into a Pascal ShortString slot at @p dest.
 *
 * @p dest_size is the declared `string[N]` width (`N + 1` bytes
 * including the length byte at index 0).  Truncates on overflow.
 */
void kgpc_string_to_shortstring(char *dest, const char *src, size_t dest_size);

/* PChar (NUL-terminated C string) -> ShortString conversion. */
void kgpc_pchar_to_shortstring(char *dest, const char *src, size_t dest_size);

/* PWideChar (NUL-terminated UTF-16) -> UnicodeString assignment. */
void kgpc_unicodestring_assign_from_widechar(uint16_t **target,
                                             const uint16_t *value);

/* AnsiString -> array of WideChar.  @p dest_count is the WideChar element
 * count (NOT byte length).  Widens each source byte to UTF-16 and pads any
 * unused trailing elements with zero. */
void kgpc_ansistr_to_widechararray(uint16_t *dest, const char *src,
                                   size_t dest_count);

#endif /* KGPC_RUNTIME_INTERNAL_H */
