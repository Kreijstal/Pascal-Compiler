/*
 * abi_constants.h — Single source of truth for ABI constants and VMT layout.
 *
 * All magic numbers for Windows x64 shadow space, set type sizes, and
 * FPC VMT slot offsets live here.  No other file should contain bare
 * integer literals for these quantities.
 */

#ifndef KGPC_ABI_CONSTANTS_H
#define KGPC_ABI_CONSTANTS_H

/* ===================================================================
 * Windows x64 ABI: shadow (home) space reserved by the CALLER before
 * every call so the callee can spill its first four register arguments.
 * SysV (Linux/macOS) has no shadow space.
 * =================================================================== */
#define KGPC_WINDOWS_SHADOW_SPACE_BYTES 32

/* ===================================================================
 * Pascal set-type storage sizes.
 *
 * A char-set (set of Char, elements 0-255) requires 256 bits = 32 bytes.
 * A small set (subrange up to 32 elements) fits in 4 bytes.
 * =================================================================== */
#define KGPC_CHAR_SET_SIZE_BYTES 32 /* set of Char: 256 bits */
#define KGPC_SMALL_SET_SIZE_BYTES 4 /* small subrange set: 32 bits */

/* ===================================================================
 * FPC VMT layout — TVmt record from objpash.inc (x86-64, 8-byte slots).
 *
 * Each slot is 8 bytes wide (pointer-sized on x86-64).
 * The first virtual method starts at slot index 12 (offset 96).
 *
 *   slot  offset  field
 *      0       0  vInstanceSize   (SizeInt)
 *      1       8  vInstanceSize2  (SizeInt = -InstanceSize)
 *      2      16  vParentRef      (PPVmt)
 *      3      24  vClassName      (PShortString)
 *      4      32  vDynamicTable   (Pointer)
 *      5      40  vMethodTable    (Pointer)
 *      6      48  vFieldTable     (Pointer)
 *      7      56  vTypeInfo       (Pointer)
 *      8      64  vInitTable      (Pointer)
 *      9      72  vAutoTable      (Pointer)
 *     10      80  vIntfTable      (PInterfaceTable)
 *     11      88  vMsgStrPtr      (Pointer)
 *     12+    96+  virtual methods
 * =================================================================== */
#define VMT_SLOT_SIZE_BYTES 8 /* each slot is one pointer (8 bytes) */

/* Named byte offsets for the fixed VMT header slots (slots 0-11) */
#define VMT_VINSTANCESIZE_OFFSET 0  /* slot  0 */
#define VMT_VINSTANCESIZE2_OFFSET 8 /* slot  1 */
#define VMT_VPARENTREF_OFFSET 16    /* slot  2 */
#define VMT_VCLASSNAME_OFFSET 24    /* slot  3 */
#define VMT_VDYNAMICTABLE_OFFSET 32 /* slot  4 */
#define VMT_VMETHODTABLE_OFFSET 40  /* slot  5 */
#define VMT_VFIELDTABLE_OFFSET 48   /* slot  6 */
#define VMT_VTYPEINFO_OFFSET 56     /* slot  7 */
#define VMT_VINITTABLE_OFFSET 64    /* slot  8 */
#define VMT_VAUTOTABLE_OFFSET 72    /* slot  9 */
#define VMT_VINTFTABLE_OFFSET 80    /* slot 10 */
#define VMT_VMSGSTRPTR_OFFSET 88    /* slot 11 */

/* Index of the first virtual-method slot */
#define VMT_FIRST_VMETHOD_SLOT 12

/* Byte offset of virtual-method slot N (N >= VMT_FIRST_VMETHOD_SLOT) */
#define VMT_VMETHOD_OFFSET(n) ((n) * VMT_SLOT_SIZE_BYTES)

/* ===================================================================
 * KGPC RTTI record (kgpc_class_typeinfo) field offsets.
 *
 * kgpc_class_typeinfo is KGPC's own run-time type descriptor emitted
 * alongside every class VMT.  The struct is declared in runtime_internal.h;
 * the corresponding assembly layout is emitted by codegen_vmt.c
 * (codegen_emit_class_vmt_and_rtti).
 *
 * On x86-64 all pointers are 8 bytes, so:
 *
 *   offset  size  field
 *        0     8  parent         — pointer to parent kgpc_class_typeinfo (NULL
 * for TObject) 8     8  class_name     — pointer to C-string class name
 * (NUL-terminated) 16     8  vmt            — pointer to the class VMT (used to
 * verify typeinfo identity) 24     8  interfaces     — pointer to
 * kgpc_interface_table (NULL if none) 32     4  num_interfaces — int: count of
 * implemented interfaces
 *
 * Ground truth: runtime_internal.h typedef struct kgpc_class_typeinfo.
 * =================================================================== */
#define KGPC_TYPEINFO_PARENT_OFFSET 0      /* ->parent */
#define KGPC_TYPEINFO_CLASSNAME_OFFSET 8   /* ->class_name */
#define KGPC_TYPEINFO_VMT_OFFSET 16        /* ->vmt */
#define KGPC_TYPEINFO_INTERFACES_OFFSET 24 /* ->interfaces */
#define KGPC_TYPEINFO_NUMINTERFACES_OFFSET                                     \
  32 /* ->num_interfaces (int, 4 bytes) */

/* ===================================================================
 * FPC-compatible interface table entry (tinterfaceentry /
 * kgpc_interface_entry).
 *
 * Declared in runtime_internal.h as kgpc_interface_entry; matches the
 * FPC tinterfaceentry record from rtl/inc/objpash.inc exactly on x86-64.
 *
 *   offset  size  field
 *        0     8  iid_ref    — ^pguid: pointer to the pguid indirection cell
 *        8     8  vtable     — pointer to the per-interface vtable for this
 * class 16     8  ioffset    — uint64: byte offset from instance base to the
 * intf slot 24     8  iidstr_ref — ^pshortstring (currently unused / NULL) 32
 * 4  itype      — tinterfaceentrytype enum (0 = etStandard) 36     4  _padding
 * — alignment padding to reach 40 bytes total
 *
 * Ground truth: FPC rtl/inc/objpash.inc tinterfaceentry record.
 * =================================================================== */
#define KGPC_INTF_ENTRY_IIDREF_OFFSET 0     /* ->iid_ref */
#define KGPC_INTF_ENTRY_VTABLE_OFFSET 8     /* ->vtable */
#define KGPC_INTF_ENTRY_IOFFSET_OFFSET 16   /* ->ioffset */
#define KGPC_INTF_ENTRY_IIDSTRREF_OFFSET 24 /* ->iidstr_ref */
#define KGPC_INTF_ENTRY_ITYPE_OFFSET 32     /* ->itype */
#define KGPC_INTF_ENTRY_SIZE 40 /* total sizeof(kgpc_interface_entry) */

/* FPC-compatible interface table header (tinterfacetable /
 * kgpc_interface_table).
 *
 *   offset  size  field
 *        0     8  entry_count — uint64: number of tinterfaceentry elements
 *        8    40* entries[]   — flexible array of kgpc_interface_entry
 *
 * Ground truth: FPC rtl/inc/objpash.inc tinterfacetable record. */
#define KGPC_INTF_TABLE_ENTRYCOUNT_OFFSET 0 /* ->entry_count */
#define KGPC_INTF_TABLE_ENTRIES_OFFSET 8    /* &->entries[0] */

/* GUID size in bytes (standard COM/FPC TGUID = 16 bytes).
 * Used when comparing interface GUIDs with memcmp. */
#define KGPC_GUID_SIZE_BYTES 16

#endif /* KGPC_ABI_CONSTANTS_H */
