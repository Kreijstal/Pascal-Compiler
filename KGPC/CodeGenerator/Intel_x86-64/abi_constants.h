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
#define KGPC_WINDOWS_SHADOW_SPACE_BYTES  32

/* ===================================================================
 * Pascal set-type storage sizes.
 *
 * A char-set (set of Char, elements 0-255) requires 256 bits = 32 bytes.
 * A small set (subrange up to 32 elements) fits in 4 bytes.
 * =================================================================== */
#define KGPC_CHAR_SET_SIZE_BYTES   32   /* set of Char: 256 bits */
#define KGPC_SMALL_SET_SIZE_BYTES   4   /* small subrange set: 32 bits */

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
#define VMT_SLOT_SIZE_BYTES        8    /* each slot is one pointer (8 bytes) */

/* Named byte offsets for the fixed VMT header slots (slots 0-11) */
#define VMT_VINSTANCESIZE_OFFSET   0    /* slot  0 */
#define VMT_VINSTANCESIZE2_OFFSET  8    /* slot  1 */
#define VMT_VPARENTREF_OFFSET      16   /* slot  2 */
#define VMT_VCLASSNAME_OFFSET      24   /* slot  3 */
#define VMT_VDYNAMICTABLE_OFFSET   32   /* slot  4 */
#define VMT_VMETHODTABLE_OFFSET    40   /* slot  5 */
#define VMT_VFIELDTABLE_OFFSET     48   /* slot  6 */
#define VMT_VTYPEINFO_OFFSET       56   /* slot  7 */
#define VMT_VINITTABLE_OFFSET      64   /* slot  8 */
#define VMT_VAUTOTABLE_OFFSET      72   /* slot  9 */
#define VMT_VINTFTABLE_OFFSET      80   /* slot 10 */
#define VMT_VMSGSTRPTR_OFFSET      88   /* slot 11 */

/* Index of the first virtual-method slot */
#define VMT_FIRST_VMETHOD_SLOT     12

/* Byte offset of virtual-method slot N (N >= VMT_FIRST_VMETHOD_SLOT) */
#define VMT_VMETHOD_OFFSET(n)      ((n) * VMT_SLOT_SIZE_BYTES)

#endif /* KGPC_ABI_CONSTANTS_H */
