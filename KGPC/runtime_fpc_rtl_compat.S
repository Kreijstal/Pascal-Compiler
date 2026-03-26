/* FindComponentClass and ReadDeltaStream stubs moved to runtime.c.
 * FAction, FCount, FData, FRootAncestor, FitemClass_TYPEINFO,
 * PPropInfo, SetStatus removed — these are class instance fields
 * that should be accessed via Self+offset, not shared globals.
 */

#if defined(__linux__) && defined(__ELF__)
.section .note.GNU-stack,"",@progbits
#endif
