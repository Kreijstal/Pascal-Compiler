# Phase 1 IR Plan: `add_inst()` Call-Site Inventory

## Summary

| Metric | Count |
|--------|-------|
| **Total `add_inst()` call sites** | **1656** |
| Uses Register_t\* | 859 |
| Fixed reg string | 426 |
| Constant only | 371 |
| `register_set_spill_callback()` sites | 6 |

### Category Definitions

- **Uses Register_t\*** — the call passes a live `Register_t *` variable (`->bit_64` / `->bit_32`) as a format argument; these are virtual-register candidates that will gain def/use annotations in Phase 2.
- **Fixed reg string** — a hardcoded physical register name (e.g. `%rax`, `%rsp`) is baked directly into the format string; no virtual-register substitution is needed.
- **Constant only** — the emitted string contains no register reference at all (labels, immediate-only instructions, calls to named symbols, etc.).

---

## `codegen.c`

**123** call sites — 44 reg / 61 fixed / 18 const

| ~Line | Instruction pattern | Uses Register_t\* | Fixed reg string | Constant only | Notes |
|-------|---------------------|:-----------------:|:----------------:|:-------------:|-------|
| 1687 | `\tcall\t%s` |  |  | ✓ |  |
| 3137 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 3193 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 3199 | `\tmovq\t-%d(%s), %s` | ✓ |  |  |  |
| 3946 | `\tleaq\t%s_INTF_%s_VTABLE(%%rip), %s` | ✓ |  |  |  |
| 3949 | `\tmovq\t%s, %lld(%s)` | ✓ |  |  |  |
| 3970 | `\tmovq\t%s, %%r11` |  | ✓ |  |  |
| 3971 | `\tmovq\t(%r11), %r11` |  | ✓ |  |  |
| 3972 | `\tmovq\t(%r11), %rax` |  | ✓ |  |  |
| 3973 | `\taddq\t8(%r11), %rax` |  | ✓ |  |  |
| 3975 | `\tjz\t.L%s_direct_%d` |  |  | ✓ |  |
| 3978 | `\tmovq\t%d(%%r11), %%r11` |  | ✓ |  |  |
| 3983 | `\tcall\t*%r11` |  | ✓ |  |  |
| 3988 | `\tcall\t*%r11` |  | ✓ |  |  |
| 3991 | `\tjmp\t.L%s_done_%d` |  |  | ✓ |  |
| 3994 | `.L%s_direct_%d:` |  |  | ✓ |  |
| 4006 | `\tmovq\t%s, %s` |  |  | ✓ |  |
| 4010 | `\tleaq\t%s(%%rip), %s` |  |  | ✓ |  |
| 4013 | `\tmovl\t$%d, %s` |  |  | ✓ |  |
| 4014 | `\tcall\t__kgpc_resolve_intf_method` |  |  | ✓ |  |
| 4016 | `\tmovq\t%%rax, -%d(%%rbp)` |  | ✓ |  |  |
| 4023 | `\tmovq\t-%d(%%rbp), %%r11` |  | ✓ |  |  |
| 4024 | `\tcall\t*%r11` |  | ✓ |  |  |
| 4030 | `\tmovq\t-%d(%%rbp), %%r11` |  | ✓ |  |  |
| 4031 | `\tcall\t*%r11` |  | ✓ |  |  |
| 4036 | `.L%s_done_%d:` |  |  | ✓ |  |
| 4146 | `\t%s\t%s` |  |  | ✓ |  |
| 7896 | `\tmovl\t$%d, %%eax` |  | ✓ |  |  |
| 8408 | `\tmovq\t-8(%%rbp), %s` |  | ✓ |  |  |
| 8410 | `\tmovl\t$0, %eax` |  | ✓ |  |  |
| 8461 | `\tmovq\t-8(%%rbp), %%rax` |  | ✓ |  |  |
| 8474 | `\tmovq\t%s, -%d(%%rbp)` |  | ✓ |  |  |
| 9150 | `\tmovq\t%s, -%d(%%rbp)` |  | ✓ |  |  |
| 9167 | `\tmovq\t-8(%%rbp), %%rax` |  | ✓ |  |  |
| 9170 | `\tmovq\t%%rax, -%d(%%rbp)` |  | ✓ |  |  |
| 9269 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 9274 | `\tmovq\t%s, %%rcx` | ✓ |  |  |  |
| 9276 | `\tmovl\t$%d, %%edx` |  | ✓ |  |  |
| 9281 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 9283 | `\tmovl\t$%d, %%esi` |  | ✓ |  |  |
| 9321 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 9325 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 9329 | `\tmovq\t$%lld, %s` | ✓ |  |  |  |
| 9335 | `\tmovq\t%s, %%rcx` | ✓ |  |  |  |
| 9337 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 9339 | `\tmovq\t%s, %%r8` | ✓ |  |  |  |
| 9345 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 9347 | `\tmovq\t%s, %%rsi` | ✓ |  |  |  |
| 9349 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 9358 | `\tmovq\t-%d(%%rbp), %s` |  | ✓ |  |  |
| 9423 | `\tmovl\t-%d(%%rbp), %s` |  | ✓ |  |  |
| 9435 | `\tmovq\t%s, -%d(%%rbp)` |  | ✓ |  |  |
| 9905 | `\tmovq\t%%rdi, -%d(%%rbp)` |  | ✓ |  |  |
| 9966 | `\tmovl\t-%d(%%rbp), %%eax` |  | ✓ |  |  |
| 10205 | `\tmovq\t%s, -%d(%%rbp)` |  | ✓ |  |  |
| 10226 | `\tmovq\t%s, -%d(%%rbp)` |  | ✓ |  |  |
| 10610 | `\tleaq\t%d(%%rbp), %s` | ✓ |  |  |  |
| 10613 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 10618 | `\tmovq\t%s, %%rcx` | ✓ |  |  |  |
| 10620 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 10621 | `\tmovl\t$10, %r8d` |  | ✓ |  |  |
| 10626 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 10628 | `\tmovq\t%s, %%rsi` | ✓ |  |  |  |
| 10629 | `\tmovl\t$10, %edx` |  | ✓ |  |  |
| 10669 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 10683 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 10697 | `\tmovq\t%d(%%rbp), %s` | ✓ |  |  |  |
| 10730 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 10736 | `\tmovq\t%s, %%rcx` | ✓ |  |  |  |
| 10738 | `\tmovq\t$%d, %%rdx` |  | ✓ |  |  |
| 10740 | `\tmovq\t%s, %%r8` |  | ✓ |  |  |
| 10745 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 10747 | `\tmovq\t$%d, %%rsi` |  | ✓ |  |  |
| 10749 | `\tmovq\t%s, %%rdx` |  | ✓ |  |  |
| 10769 | `\tmovsd\t%s, -%d(%%rbp)` |  | ✓ |  |  |
| 10777 | `\tmovss\t%d(%%rbp), %%xmm0` |  | ✓ |  |  |
| 10785 | `\tmovsd\t%d(%%rbp), %%xmm0` |  | ✓ |  |  |
| 10789 | `\tmovsd\t%%xmm0, -%d(%%rbp)` |  | ✓ |  |  |
| 10830 | `\tmovl\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 10861 | `\tmovl\t%d(%%rbp), %s` | ✓ |  |  |  |
| 10870 | `\tmovl\t%s, -%d(%%rbp)` |  | ✓ |  |  |
| 10940 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 10954 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 10968 | `\tmovq\t%d(%%rbp), %s` | ✓ |  |  |  |
| 10997 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 11002 | `\tmovq\t%s, %%rcx` | ✓ |  |  |  |
| 11004 | `\tmovq\t$%d, %%rdx` |  | ✓ |  |  |
| 11006 | `\tmovq\t%s, %%r8` |  | ✓ |  |  |
| 11011 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 11013 | `\tmovq\t$%d, %%rsi` |  | ✓ |  |  |
| 11015 | `\tmovq\t%s, %%rdx` |  | ✓ |  |  |
| 11049 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 11068 | `\tmovq\t%d(%%rbp), %s` | ✓ |  |  |  |
| 11073 | `\tmovq\t%s, -%d(%%rbp)` |  | ✓ |  |  |
| 11131 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 11178 | `\tmovq\t%d(%%rbp), %s` | ✓ |  |  |  |
| 11199 | `\tmovq\t$%d, %s` | ✓ |  |  |  |
| 11204 | `\tmovq\t%s, %%rdx` |  | ✓ |  |  |
| 11206 | `\tleaq\t-%d(%%rbp), %%rcx` |  | ✓ |  |  |
| 11208 | `\tmovq\t%s, %%r8` | ✓ |  |  |  |
| 11213 | `\tmovq\t%s, %%rsi` |  | ✓ |  |  |
| 11215 | `\tleaq\t-%d(%%rbp), %%rdi` |  | ✓ |  |  |
| 11217 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 11268 | `\tmovq\t$64, %s` |  |  | ✓ |  |
| 11273 | `\tpushq\t%rax` |  |  | ✓ |  |
| 11275 | `\tleaq\t%s(%%rip), %%r10` |  | ✓ |  |  |
| 11276 | `\tmovq\t%r10, (%rax)` |  | ✓ |  |  |
| 11277 | `\tpopq\t%rax` |  |  | ✓ |  |
| 11289 | `\tmovq\t%%rax, -%d(%%rbp)` |  | ✓ |  |  |
| 11307 | `\tmovq\t$0, -%d(%%rbp)` |  | ✓ |  |  |
| 11337 | `\tleaq\t-%d(%%rbp), %s` |  | ✓ |  |  |
| 11340 | `\tmovq\t$%lld, %s` |  |  | ✓ |  |
| 11343 | `\tmovl\t$%d, %s` |  |  | ✓ |  |
| 11460 | `\tmovq\t$0, -%d(%%rbp)` |  | ✓ |  |  |
| 11465 | `\tmovq\t$0, -%d(%%rbp)` |  | ✓ |  |  |
| 11488 | `\tmovq\t$0, -%d(%%rbp)` |  | ✓ |  |  |
| 11551 | `\tmovq\t$0, -%d(%%rbp)` |  | ✓ |  |  |
| 11557 | `\tmovq\t$0, -%d(%%rbp)` |  | ✓ |  |  |
| 11573 | `\tmovb\t%s(%%rip), %%al` |  | ✓ |  |  |
| 11574 | `\ttestb\t%al, %al` |  | ✓ |  |  |
| 11576 | `\tjne\t%s` |  |  | ✓ |  |
| 11581 | `\tmovb\t$1, %s(%%rip)` |  |  | ✓ |  |
| 11585 | `%s:` |  |  | ✓ |  |

---

## `codegen_expression.c`

**493** call sites — 320 reg / 88 fixed / 85 const

| ~Line | Instruction pattern | Uses Register_t\* | Fixed reg string | Constant only | Notes |
|-------|---------------------|:-----------------:|:----------------:|:-------------:|-------|
| 230 | `\tleaq\t%s_CLASSVAR(%%rip), %s` |  |  | ✓ |  |
| 236 | `\taddq\t$%lld, %s` |  |  | ✓ |  |
| 437 | `\tleaq\t%s(%%rip), %s` |  |  | ✓ |  |
| 477 | `\tleaq\t%s(%%rip), %s` |  |  | ✓ |  |
| 1511 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 1515 | `\tmovq\t%%rax, %s` | ✓ |  |  |  |
| 1534 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 1547 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 1561 | `\tmovl\t%s, %s` | ✓ |  |  |  |
| 1565 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 1872 | `\tmovq\t(%s), %s` | ✓ |  |  |  |
| 1875 | `\tmovq\t56(%s), %s` | ✓ |  |  |  |
| 1901 | `\tmovq\t56(%s), %s` | ✓ |  |  |  |
| 1976 | `\tmovq\t(%s), %s` | ✓ |  |  |  |
| 2005 | `\tmovq\t%s, %%rcx` | ✓ |  |  |  |
| 2007 | `\tleaq\t%s_TYPEINFO(%%rip), %%rdx` |  | ✓ |  |  |
| 2012 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 2014 | `\tleaq\t%s_TYPEINFO(%%rip), %%rsi` |  | ✓ |  |  |
| 2309 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 2322 | `\tmovq\t%s, %%rcx` | ✓ |  |  |  |
| 2324 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 2325 | `\tmovl\t$10, %r8d` |  | ✓ |  |  |
| 2330 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 2332 | `\tmovq\t%s, %%rsi` | ✓ |  |  |  |
| 2333 | `\tmovl\t$10, %edx` |  | ✓ |  |  |
| 2352 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 2490 | `\tcvtsi2sdl\t%s, %%xmm0` | ✓ |  |  |  |
| 2493 | `\tmovq\t%%xmm0, %s` | ✓ |  |  |  |
| 2583 | `\tmovq\t%s, %%rcx` |  | ✓ |  |  |
| 2585 | `\tmovq\t%s, %%rdx` |  | ✓ |  |  |
| 2586 | `\tmovl\t$10, %r8d` |  | ✓ |  |  |
| 2592 | `\tmovq\t%s, %%rdi` |  | ✓ |  |  |
| 2594 | `\tmovq\t%s, %%rsi` |  | ✓ |  |  |
| 2595 | `\tmovl\t$10, %edx` |  | ✓ |  |  |
| 2622 | `\tmovq\t%s, %%rcx` |  | ✓ |  |  |
| 2624 | `\tmovq\t%s, %%rdx` |  | ✓ |  |  |
| 2629 | `\tmovq\t%s, %%rdi` |  | ✓ |  |  |
| 2631 | `\tmovq\t%s, %%rsi` |  | ✓ |  |  |
| 2643 | `\tmovq\t%s, %%rcx` |  | ✓ |  |  |
| 2645 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 2650 | `\tmovq\t%s, %%rdi` |  | ✓ |  |  |
| 2652 | `\tmovq\t%s, %%rsi` | ✓ |  |  |  |
| 2676 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 2689 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 2692 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 2698 | `\tmovq\t-%d(%%rbp), %%rcx` |  | ✓ |  |  |
| 2700 | `\tmovq\t-%d(%%rbp), %%rdx` |  | ✓ |  |  |
| 2705 | `\tmovq\t-%d(%%rbp), %%rdi` |  | ✓ |  |  |
| 2707 | `\tmovq\t-%d(%%rbp), %%rsi` |  | ✓ |  |  |
| 2749 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 2752 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 2755 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 2758 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 2764 | `\tmovq\t-%d(%%rbp), %%rcx` |  | ✓ |  |  |
| 2766 | `\tmovq\t-%d(%%rbp), %%rdx` |  | ✓ |  |  |
| 2768 | `\tmovq\t-%d(%%rbp), %%r8` |  | ✓ |  |  |
| 2773 | `\tmovq\t-%d(%%rbp), %%rdi` |  | ✓ |  |  |
| 2775 | `\tmovq\t-%d(%%rbp), %%rsi` |  | ✓ |  |  |
| 2777 | `\tmovq\t-%d(%%rbp), %%rdx` |  | ✓ |  |  |
| 2811 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 2827 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 3026 | `\tmovq\t56(%s), %s` | ✓ |  |  |  |
| 3054 | `\tmovq\t%s, %%rcx` | ✓ |  |  |  |
| 3056 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 3061 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 3063 | `\tmovq\t%s, %%rsi` | ✓ |  |  |  |
| 3086 | `\tmovl\t%%eax, %s` | ✓ |  |  |  |
| 3115 | `\ttestq\t%s, %s` | ✓ |  |  |  |
| 3117 | `\tje\t%s` |  |  | ✓ |  |
| 3127 | `\tsubq\t$48, %rsp` |  | ✓ |  |  |
| 3129 | `\tmovq\t%s, 32(%%rsp)` | ✓ |  |  |  |
| 3139 | `\tmovq\t32(%%rsp), %s` | ✓ |  |  |  |
| 3140 | `\taddq\t$48, %rsp` |  | ✓ |  |  |
| 3143 | `%s:` |  |  | ✓ |  |
| 3168 | `\ttestq\t%s, %s` | ✓ |  |  |  |
| 3170 | `\tje\t%s` |  |  | ✓ |  |
| 3178 | `\tsubq\t$48, %rsp` |  | ✓ |  |  |
| 3180 | `\tmovq\t%s, 32(%%rsp)` | ✓ |  |  |  |
| 3190 | `\tmovq\t32(%%rsp), %s` | ✓ |  |  |  |
| 3191 | `\taddq\t$48, %rsp` |  | ✓ |  |  |
| 3194 | `%s:` |  |  | ✓ |  |
| 3215 | `\tmovq\t(%s), %s` | ✓ |  |  |  |
| 3323 | `\tmovb\t%s, -%d(%%rbp)` |  | ✓ |  |  |
| 3330 | `\tmovw\t%s, -%d(%%rbp)` |  | ✓ |  |  |
| 3335 | `\tmovl\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 3339 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 3379 | `\tmovq\t$0, -%d(%%rbp)` |  | ✓ |  |  |
| 3381 | `\tmovq\t$0, -%d(%%rbp)` |  | ✓ |  |  |
| 3388 | `\tmovq\t$0, -%d(%%rbp)` |  | ✓ |  |  |
| 3392 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 3445 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 3447 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 3450 | `\tmovq\t$%d, %s` | ✓ |  |  |  |
| 3453 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 3460 | `\tmovq\t$0, -%d(%%rbp)` |  | ✓ |  |  |
| 3464 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 3542 | `\tmovl\t$%d, -%d(%%rbp)` |  | ✓ |  |  |
| 3545 | `\tmovl\t$0, -%d(%%rbp)` |  | ✓ |  |  |
| 3558 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 3564 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 3567 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 3576 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 3600 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 3603 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 3607 | `\tmovq\t$%d, %s` | ✓ |  |  |  |
| 3610 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 3614 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 6413 | `\tleaq\t%s(%%rip), %s` | ✓ |  |  |  |
| 6600 | `\tmovslq\t%s, %s` |  |  | ✓ |  |
| 6610 | `\tmovl\t%s, %s` |  |  | ✓ |  |
| 6671 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 6691 | `\tmovl\t(%s), %s` | ✓ |  |  |  |
| 6770 | `\tleaq\t%s(%%rip), %s` | ✓ |  |  |  |
| 6806 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 6834 | `%s%s:\t.string \` |  |  | ✓ |  |
| 6837 | `\tleaq\t%s(%%rip), %s` | ✓ |  |  |  |
| 6844 | `\tleaq\t-%d(%s), %s` | ✓ |  |  |  |
| 6855 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 6894 | `\tleaq\t%s(%%rip), %s` | ✓ |  |  |  |
| 6905 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 6924 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 7028 | `\tleaq\t%s_CLASSVAR(%%rip), %s` | ✓ |  |  |  |
| 7035 | `\taddq\t$%lld, %s` | ✓ |  |  |  |
| 7057 | `\tleaq\t%s_CLASSVAR(%%rip), %s` | ✓ |  |  |  |
| 7078 | `\tmovq\t(%s), %s` | ✓ |  |  |  |
| 7093 | `\tmovabsq\t$%lld, %%r11` |  | ✓ |  |  |
| 7100 | `\taddq\t$%lld, %s` | ✓ |  |  |  |
| 7156 | `\tmovq\t$%lld, %s` | ✓ |  |  |  |
| 7175 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 7191 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 7228 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 7270 | `\tmovl\t(%s), %s` | ✓ |  |  |  |
| 7291 | `\tcmpl\t$0, %s` | ✓ |  |  |  |
| 7293 | `\tjl\t%s` |  |  | ✓ |  |
| 7295 | `\tcmpl\t$31, %s` | ✓ |  |  |  |
| 7297 | `\tjg\t%s` |  |  | ✓ |  |
| 7300 | `\tbtsl\t%s, %s` | ✓ |  |  |  |
| 7302 | `%s:` |  |  | ✓ |  |
| 7329 | `\tcmpl\t%s, %s` | ✓ |  |  |  |
| 7331 | `\tjle\t%s` |  |  | ✓ |  |
| 7333 | `\tmovl\t%s, %s` | ✓ |  |  |  |
| 7335 | `\tmovl\t%s, %s` | ✓ |  |  |  |
| 7337 | `\tmovl\t%s, %s` | ✓ |  |  |  |
| 7339 | `%s:` |  |  | ✓ |  |
| 7343 | `\tcmpl\t$0, %s` | ✓ |  |  |  |
| 7345 | `\tjl\t%s` |  |  | ✓ |  |
| 7349 | `\tcmpl\t$0, %s` | ✓ |  |  |  |
| 7351 | `\tjge\t%s` |  |  | ✓ |  |
| 7353 | `\tmovl\t$0, %s` | ✓ |  |  |  |
| 7355 | `%s:` |  |  | ✓ |  |
| 7359 | `\tcmpl\t$31, %s` | ✓ |  |  |  |
| 7361 | `\tjg\t%s` |  |  | ✓ |  |
| 7365 | `\tcmpl\t$31, %s` | ✓ |  |  |  |
| 7367 | `\tjle\t%s` |  |  | ✓ |  |
| 7369 | `\tmovl\t$31, %s` | ✓ |  |  |  |
| 7371 | `%s:` |  |  | ✓ |  |
| 7374 | `%s:` |  |  | ✓ |  |
| 7376 | `\tcmpl\t%s, %s` | ✓ |  |  |  |
| 7378 | `\tjg\t%s` |  |  | ✓ |  |
| 7381 | `\tbtsl\t%s, %s` | ✓ |  |  |  |
| 7383 | `\tcmpl\t%s, %s` | ✓ |  |  |  |
| 7385 | `\tje\t%s` |  |  | ✓ |  |
| 7387 | `\tincl\t%s` | ✓ |  |  |  |
| 7389 | `\tjmp\t%s` |  |  | ✓ |  |
| 7391 | `%s:` |  |  | ✓ |  |
| 7424 | `\tmovl\t$0, -%d(%%rbp)` |  | ✓ |  |  |
| 7438 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 7478 | `\tbtsl\t%s, (%s)` | ✓ |  |  |  |
| 7514 | `\tincl\t%s` | ✓ |  |  |  |
| 7519 | `.L%d:` |  |  | ✓ |  |
| 7521 | `\tcmpl\t%s, %s` | ✓ |  |  |  |
| 7523 | `\tjg\t.L%d` |  |  | ✓ |  |
| 7525 | `\tbtsl\t%s, (%s)` | ✓ |  |  |  |
| 7527 | `\tincl\t%s` | ✓ |  |  |  |
| 7529 | `\tjmp\t.L%d` |  |  | ✓ |  |
| 7531 | `.L%d:` |  |  | ✓ |  |
| 7559 | `\tmovl\t$%u, %s` | ✓ |  |  |  |
| 7577 | `\tmovl\t$0, %s` | ✓ |  |  |  |
| 7683 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 7762 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 7765 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 7773 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 7778 | `\tmovq\t%s, %%rcx` | ✓ |  |  |  |
| 7780 | `\tmovq\t-%d(%%rbp), %%rdx` |  | ✓ |  |  |
| 7782 | `\tmovq\t-%d(%%rbp), %%r8` |  | ✓ |  |  |
| 7787 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 7789 | `\tmovq\t-%d(%%rbp), %%rsi` |  | ✓ |  |  |
| 7791 | `\tmovq\t-%d(%%rbp), %%rdx` |  | ✓ |  |  |
| 7802 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 8159 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 8164 | `\tmovq\t%%rax, %s` | ✓ |  |  |  |
| 8175 | `\tmovss\t(%s), %%xmm0` | ✓ |  |  |  |
| 8176 | `\tcvtss2sd\t%xmm0, %xmm0` |  | ✓ |  |  |
| 8181 | `\tmovsd\t(%s), %%xmm0` | ✓ |  |  |  |
| 8184 | `\tmovq\t%%xmm0, %s` | ✓ |  |  |  |
| 8725 | `\tsubl\t$%lld, %s` | ✓ |  |  |  |
| 8733 | `\taddl\t$%lld, %s` | ✓ |  |  |  |
| 8743 | `\tleaq\t(%s,%s,%d), %s` | ✓ |  |  |  |
| 8748 | `\timulq\t$%lld, %s` | ✓ |  |  |  |
| 8750 | `\taddq\t%s, %s` | ✓ |  |  |  |
| 9240 | `\tmovl\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 9280 | `\tmovl\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 9349 | `\tmovq\t(%s), %s` | ✓ |  |  |  |
| 9722 | `\taddq\t$1, %s` | ✓ |  |  |  |
| 9733 | `\tsubl\t$%lld, %s` | ✓ |  |  |  |
| 9741 | `\taddl\t$%lld, %s` | ✓ |  |  |  |
| 9763 | `\tleaq\t(%s,%s,%d), %s` | ✓ |  |  |  |
| 9770 | `\timulq\t$%lld, %s` | ✓ |  |  |  |
| 9774 | `\taddq\t%s, %s` | ✓ |  |  |  |
| 9797 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 9810 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 9847 | `\tsubl\t$%lld, %s` | ✓ |  |  |  |
| 9857 | `\taddl\t$%lld, %s` | ✓ |  |  |  |
| 9869 | `\timulq\t$%lld, %s` | ✓ |  |  |  |
| 9874 | `\taddq\t%s, %s` | ✓ |  |  |  |
| 9935 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 9946 | `\tmovq\t(%s), %s` | ✓ |  |  |  |
| 9978 | `\tmovl\t(%s), %s` | ✓ |  |  |  |
| 10071 | `\tbtl\t%s, (%s)` | ✓ |  |  |  |
| 10074 | `\tsbbl\t%s, %s` | ✓ |  |  |  |
| 10076 | `\ttestl\t%s, %s` | ✓ |  |  |  |
| 10106 | `\tmovl\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 10132 | `\tmovl\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 10142 | `\tcmpl\t$0, %s` | ✓ |  |  |  |
| 10144 | `\tjl\t%s` |  |  | ✓ |  |
| 10146 | `\tcmpl\t$31, %s` | ✓ |  |  |  |
| 10148 | `\tjg\t%s` |  |  | ✓ |  |
| 10152 | `\tbtl\t%s, %s` | ✓ |  |  |  |
| 10155 | `\tsbbl\t%s, %s` | ✓ |  |  |  |
| 10157 | `\tjmp\t%s` |  |  | ✓ |  |
| 10160 | `%s:` |  |  | ✓ |  |
| 10163 | `\txorl\t%s, %s` | ✓ |  |  |  |
| 10165 | `%s:` |  |  | ✓ |  |
| 10169 | `\ttestl\t%s, %s` | ✓ |  |  |  |
| 10202 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 10231 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 10251 | `\tmov%c\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 10276 | `\tmov%c\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 10368 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 10372 | `\tmovl\t%s, %s` | ✓ |  |  |  |
| 10376 | `\tmovq\t%%rax, %s` | ✓ |  |  |  |
| 10382 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 10394 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 10398 | `\tmovl\t%s, %s` | ✓ |  |  |  |
| 10402 | `\tmovq\t%%rax, %s` | ✓ |  |  |  |
| 10408 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 10471 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 10479 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 10492 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 10500 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 10521 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 10523 | `\tmovq\t$%lld, %s` |  |  | ✓ |  |
| 10525 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 10527 | `\tmovq\t$%lld, %s` |  |  | ✓ |  |
| 10534 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 10536 | `\tmovq\t$%lld, %s` |  |  | ✓ |  |
| 10538 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 10545 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 10547 | `\tmovq\t$%lld, %s` |  |  | ✓ |  |
| 10549 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 10554 | `\tcall\t%s` |  |  | ✓ |  |
| 10556 | `\tnegl\t%eax` |  |  | ✓ |  |
| 10558 | `\tmovl\t%s, %s` | ✓ |  |  |  |
| 10563 | `\tcmpl\t$0, %s` | ✓ |  |  |  |
| 10625 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 10627 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 10632 | `\tmovl\t%s, %s` | ✓ |  |  |  |
| 10637 | `\tcmpl\t$0, %s` | ✓ |  |  |  |
| 10701 | `\tleaq\t%s(%%rip), %s` | ✓ |  |  |  |
| 10714 | `\tcmpl\t$0, %s` | ✓ |  |  |  |
| 10716 | `\tjl\t%s` |  |  | ✓ |  |
| 10718 | `\tcmpl\t$%d, %s` | ✓ |  |  |  |
| 10720 | `\tjg\t%s` |  |  | ✓ |  |
| 10723 | `\tbtl\t%s, (%s)` | ✓ |  |  |  |
| 10725 | `\tsbbl\t%s, %s` | ✓ |  |  |  |
| 10727 | `\tjmp\t%s` |  |  | ✓ |  |
| 10730 | `%s:` |  |  | ✓ |  |
| 10732 | `\txorl\t%s, %s` | ✓ |  |  |  |
| 10734 | `%s:` |  |  | ✓ |  |
| 10737 | `\ttestl\t%s, %s` | ✓ |  |  |  |
| 10768 | `\tcmpl\t$0, %s` | ✓ |  |  |  |
| 10770 | `\tjl\t%s` |  |  | ✓ |  |
| 10772 | `\tcmpl\t$255, %s` | ✓ |  |  |  |
| 10774 | `\tjg\t%s` |  |  | ✓ |  |
| 10777 | `\tbtl\t%s, (%s)` | ✓ |  |  |  |
| 10779 | `\tsbbl\t%s, %s` | ✓ |  |  |  |
| 10781 | `\tjmp\t%s` |  |  | ✓ |  |
| 10784 | `%s:` |  |  | ✓ |  |
| 10786 | `\txorl\t%s, %s` | ✓ |  |  |  |
| 10788 | `%s:` |  |  | ✓ |  |
| 10791 | `\ttestl\t%s, %s` | ✓ |  |  |  |
| 10814 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 10818 | `\tcmpl\t$0, %s` | ✓ |  |  |  |
| 10820 | `\tjl\t%s` |  |  | ✓ |  |
| 10822 | `\tcmpl\t$31, %s` | ✓ |  |  |  |
| 10824 | `\tjg\t%s` |  |  | ✓ |  |
| 10828 | `\tbtl\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 10830 | `\tsbbl\t%s, %s` | ✓ |  |  |  |
| 10832 | `\tjmp\t%s` |  |  | ✓ |  |
| 10835 | `%s:` |  |  | ✓ |  |
| 10837 | `\txorl\t%s, %s` | ✓ |  |  |  |
| 10839 | `%s:` |  |  | ✓ |  |
| 10844 | `\tcmpl\t$0, %s` | ✓ |  |  |  |
| 10846 | `\tjl\t%s` |  |  | ✓ |  |
| 10848 | `\tcmpl\t$31, %s` | ✓ |  |  |  |
| 10850 | `\tjg\t%s` |  |  | ✓ |  |
| 10854 | `\tbtl\t%s, %s` | ✓ |  |  |  |
| 10857 | `\tsbbl\t%s, %s` | ✓ |  |  |  |
| 10859 | `\tjmp\t%s` |  |  | ✓ |  |
| 10862 | `%s:` |  |  | ✓ |  |
| 10864 | `\txorl\t%s, %s` | ✓ |  |  |  |
| 10866 | `%s:` |  |  | ✓ |  |
| 10870 | `\ttestl\t%s, %s` | ✓ |  |  |  |
| 10884 | `\tmovq\t%s, %%xmm1` |  | ✓ |  |  |
| 10910 | `\tmovq\t%s, %%xmm0` |  | ✓ |  |  |
| 10919 | `\tcvtsi2sdl\t%s, %%xmm0` | ✓ |  |  |  |
| 10922 | `\txorl\t%s, %s` | ✓ |  |  |  |
| 10923 | `\tucomisd\t%xmm0, %xmm1` |  | ✓ |  |  |
| 10930 | `\tjp\t%s` |  |  | ✓ |  |
| 10932 | `\tje\t%s` |  |  | ✓ |  |
| 10936 | `\tjp\t%s` |  |  | ✓ |  |
| 10938 | `\tjne\t%s` |  |  | ✓ |  |
| 10942 | `\tjp\t%s` |  |  | ✓ |  |
| 10944 | `\tjb\t%s` |  |  | ✓ |  |
| 10948 | `\tjp\t%s` |  |  | ✓ |  |
| 10950 | `\tjbe\t%s` |  |  | ✓ |  |
| 10954 | `\tjp\t%s` |  |  | ✓ |  |
| 10956 | `\tja\t%s` |  |  | ✓ |  |
| 10960 | `\tjp\t%s` |  |  | ✓ |  |
| 10962 | `\tjae\t%s` |  |  | ✓ |  |
| 10969 | `\tjmp\t%s` |  |  | ✓ |  |
| 10971 | `%s:` |  |  | ✓ |  |
| 10973 | `\tmovl\t$1, %s` | ✓ |  |  |  |
| 10975 | `%s:` |  |  | ✓ |  |
| 10977 | `\ttestl\t%s, %s` | ✓ |  |  |  |
| 11024 | `\tcmp%c\t%s, %s` |  |  | ✓ |  |
| 11160 | `\t%s\t%s` |  |  | ✓ |  |
| 11162 | `\tmovzbl\t%s, %s` | ✓ |  |  |  |
| 11238 | `\tmovq\t-%d(%%rbp), %s` |  | ✓ |  |  |
| 11244 | `\taddq\t$%lld, %s` |  |  | ✓ |  |
| 11346 | `\tleaq\t%s(%%rip), %s` |  |  | ✓ |  |
| 11395 | `\tmovq\t(%s), %s` | ✓ |  |  |  |
| 11402 | `\taddq\t$%lld, %s` | ✓ |  |  |  |
| 11411 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 11436 | `\tleaq\t%s(%%rip), %s` |  |  | ✓ |  |
| 11441 | `\tmovq\t%%rbp, %s` |  | ✓ |  |  |
| 11452 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 11462 | `\tmovq\t-8(%%rbp), %s` |  | ✓ |  |  |
| 11993 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 12050 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 12054 | `\tmovq\t%s, (%s)` | ✓ |  |  |  |
| 12059 | `\tmovq\t$%lld, 8(%s)` | ✓ |  |  |  |
| 12066 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 12100 | `%s%s:\t.string \` |  |  | ✓ |  |
| 12133 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 12138 | `\tleaq\t%s(%%rip), %s` | ✓ |  |  |  |
| 12143 | `\tmovq\t%s, (%s)` | ✓ |  |  |  |
| 12149 | `\tmovq\t$%d, 8(%s)` | ✓ |  |  |  |
| 12156 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 12199 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 12227 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 12232 | `\tmovq\t%s, %%rcx` | ✓ |  |  |  |
| 12234 | `\tmovl\t$%d, %%edx` |  | ✓ |  |  |
| 12236 | `\tmovq\t%s, %%r8` | ✓ |  |  |  |
| 12241 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 12243 | `\tmovq\t$%d, %%rsi` |  | ✓ |  |  |
| 12245 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 12248 | `\tmovl\t$0, %eax` |  | ✓ |  |  |
| 12284 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 12289 | `\tmovq\t%s, %%rcx` | ✓ |  |  |  |
| 12291 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 12293 | `\tmovq\t$%lld, %%r8` |  | ✓ |  |  |
| 12295 | `\tmovl\t$%d, %%r9d` |  | ✓ |  |  |
| 12300 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 12302 | `\tmovq\t%s, %%rsi` | ✓ |  |  |  |
| 12304 | `\tmovq\t$%lld, %%rdx` |  | ✓ |  |  |
| 12306 | `\tmovl\t$%d, %%ecx` |  | ✓ |  |  |
| 12309 | `\tmovl\t$0, %eax` |  | ✓ |  |  |
| 12338 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 12343 | `\tmovl\t%s, %s` | ✓ |  |  |  |
| 12347 | `\tmovq\t%%rax, %s` | ✓ |  |  |  |
| 12353 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 12362 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 12367 | `\tmovq\t%s, %%rcx` | ✓ |  |  |  |
| 12369 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 12371 | `\tmovl\t$%d, %%r8d` |  | ✓ |  |  |
| 12376 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 12378 | `\tmovq\t%s, %%rsi` | ✓ |  |  |  |
| 12380 | `\tmovl\t$%d, %%edx` |  | ✓ |  |  |
| 12383 | `\tmovl\t$0, %eax` |  | ✓ |  |  |
| 12392 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 12399 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 12425 | `\txorq\t%s, %s` | ✓ |  |  |  |
| 12475 | `\tmovq\t(%s), %s` | ✓ |  |  |  |
| 12486 | `\tmovq\t(%s), %s` | ✓ |  |  |  |
| 12535 | `\tmovq\t(%s), %s` | ✓ |  |  |  |
| 12595 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 12632 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 12681 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 12686 | `\tmovq\t%s, %%rcx` | ✓ |  |  |  |
| 12688 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 12690 | `\tmovl\t$%d, %%r8d` |  | ✓ |  |  |
| 12695 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 12697 | `\tmovq\t%s, %%rsi` | ✓ |  |  |  |
| 12699 | `\tmovl\t$%d, %%edx` |  | ✓ |  |  |
| 12709 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 12731 | `\tmovq\t(%s), %s` | ✓ |  |  |  |
| 12739 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 12792 | `\tleaq\t-%d(%%rbp), %%rcx` |  | ✓ |  |  |
| 12795 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 12801 | `\tleaq\t-%d(%%rbp), %%rdi` |  | ✓ |  |  |
| 12804 | `\tmovq\t%s, %%rsi` | ✓ |  |  |  |
| 12822 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 12829 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 12870 | `\tmovq\t(%s), %s` | ✓ |  |  |  |
| 12877 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 12917 | `\tmovq\t(%s), %s` | ✓ |  |  |  |
| 12924 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 12973 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 12976 | `\tleaq\t-%d(%%rbp), %%rcx` |  | ✓ |  |  |
| 12979 | `\tmovq\t$%lld, %%r8` |  | ✓ |  |  |
| 12985 | `\tmovq\t%s, %%rsi` | ✓ |  |  |  |
| 12988 | `\tleaq\t-%d(%%rbp), %%rdi` |  | ✓ |  |  |
| 12991 | `\tmovq\t$%lld, %%rdx` |  | ✓ |  |  |
| 13009 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 13016 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 13097 | `\tleaq\t__kgpc_guid_%s(%%rip), %s` | ✓ |  |  |  |
| 13133 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 13145 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 13154 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 13156 | `\tleaq\t-%d(%%rbp), %%rcx` |  | ✓ |  |  |
| 13158 | `\tmovq\t$%lld, %%r8` |  | ✓ |  |  |
| 13163 | `\tmovq\t%s, %%rsi` | ✓ |  |  |  |
| 13165 | `\tleaq\t-%d(%%rbp), %%rdi` |  | ✓ |  |  |
| 13167 | `\tmovq\t$%lld, %%rdx` |  | ✓ |  |  |
| 13202 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 13225 | `\tmovq\t(%s), %s` | ✓ |  |  |  |
| 13231 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 13240 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 13280 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 13388 | `\tmovq\t(%s), %s` | ✓ |  |  |  |
| 13406 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 13411 | `\tmovq\t%%rax, %s` | ✓ |  |  |  |
| 13447 | `\tmovl\t%s, %s` | ✓ |  |  |  |
| 13450 | `\tmovq\t%%rax, %s` | ✓ |  |  |  |
| 13460 | `\tmovl\t%s, %s` | ✓ |  |  |  |
| 13463 | `\tmovq\t%%rax, %s` | ✓ |  |  |  |
| 13465 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 13468 | `\tmovq\t%%rax, %s` | ✓ |  |  |  |
| 13478 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 13481 | `\tmovq\t%%rax, %s` | ✓ |  |  |  |
| 13493 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 13496 | `\tmovq\t%%rax, %s` | ✓ |  |  |  |
| 13507 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 13510 | `\tmovq\t%%rax, %s` | ✓ |  |  |  |
| 13521 | `\tmovq\t(%s), %s` | ✓ |  |  |  |
| 13563 | `\tmovsd\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 13569 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 13725 | `\tsubq\t$%d, %%rsp` |  | ✓ |  |  |
| 13795 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 13815 | `\tmovss\t%s, %%xmm0` | ✓ |  |  |  |
| 13821 | `\tmovd\t%s, %%xmm0` | ✓ |  |  |  |
| 13833 | `\tmovss\t%%xmm0, %s` |  | ✓ |  |  |
| 13854 | `\tmovd\t%s, %%xmm0` | ✓ |  |  |  |
| 13855 | `\tcvtss2sd\t%xmm0, %xmm0` |  | ✓ |  |  |
| 13866 | `\tmovsd\t%%xmm0, %s` |  | ✓ |  |  |
| 13883 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 13888 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 13910 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 13921 | `\tleaq\t%d(%%rsp), %s` | ✓ |  |  |  |
| 13923 | `\tmovq\t%s, %%rcx` | ✓ |  |  |  |
| 13925 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 13926 | `\tmovl\t$10, %r8d` |  | ✓ |  |  |
| 13933 | `\tleaq\t%d(%%rsp), %%rdi` |  | ✓ |  |  |
| 13935 | `\tmovq\t%s, %%rsi` | ✓ |  |  |  |
| 13936 | `\tmovl\t$10, %edx` |  | ✓ |  |  |
| 13953 | `\tmovss\t-%d(%%rbp), %%xmm0` |  | ✓ |  |  |
| 13962 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 13964 | `\tmovq\t%s, %%xmm0` | ✓ |  |  |  |
| 13965 | `\tcvtsd2ss\t%xmm0, %xmm0` |  | ✓ |  |  |
| 13977 | `\tmovss\t%%xmm0, %s` |  | ✓ |  |  |
| 13998 | `\tmovq\t-%d(%%rbp), %s` |  | ✓ |  |  |
| 14023 | `\tmovl\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 14025 | `\tmovd\t%s, %%xmm0` | ✓ |  |  |  |
| 14026 | `\tcvtss2sd\t%xmm0, %xmm0` |  | ✓ |  |  |
| 14037 | `\tmovsd\t%%xmm0, %s` |  | ✓ |  |  |
| 14051 | `\tmovslq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 14060 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 14068 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 14073 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 14108 | `\tmovq\t%s, %s` |  |  | ✓ |  |
| 14127 | `\taddq\t$%d, %%rsp` |  | ✓ |  |  |
| 14147 | `\tmovq\t(%s), %s` |  |  | ✓ |  |

---

## `codegen_stmt_assignment.c`

**127** call sites — 94 reg / 24 fixed / 9 const

| ~Line | Instruction pattern | Uses Register_t\* | Fixed reg string | Constant only | Notes |
|-------|---------------------|:-----------------:|:----------------:|:-------------:|-------|
| 74 | `\tmovq\t%s, %%rcx` | ✓ |  |  |  |
| 76 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 81 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 83 | `\tmovq\t%s, %%rsi` | ✓ |  |  |  |
| 110 | `\txchgq\t%s, %s` |  |  | ✓ |  |
| 115 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 117 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 122 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 124 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 129 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 131 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 226 | `\tmovq\t$%d, %%rdx` |  | ✓ |  |  |
| 249 | `\tmovq\t$%d, %%rdx` |  | ✓ |  |  |
| 269 | `\tmovq\t%s, %%rcx` | ✓ |  |  |  |
| 271 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 273 | `\tmovq\t$%d, %%r8` |  | ✓ |  |  |
| 275 | `\tmovq\t$%d, %%r9` |  | ✓ |  |  |
| 281 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 283 | `\tmovq\t%s, %%rsi` | ✓ |  |  |  |
| 285 | `\tmovq\t$%d, %%rdx` |  | ✓ |  |  |
| 287 | `\tmovq\t$%d, %%rcx` |  | ✓ |  |  |
| 306 | `\tmovq\t%s, %%rcx` | ✓ |  |  |  |
| 308 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 310 | `\tmovq\t$%d, %%r8` |  | ✓ |  |  |
| 315 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 317 | `\tmovq\t%s, %%rsi` | ✓ |  |  |  |
| 319 | `\tmovq\t$%d, %%rdx` |  | ✓ |  |  |
| 1033 | `\txchgq\t%rcx, %rdx` |  | ✓ |  |  |
| 1038 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 1040 | `\tmovq\t%s, %%rcx` | ✓ |  |  |  |
| 1045 | `\tmovq\t%s, %%rcx` | ✓ |  |  |  |
| 1047 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 1052 | `\tmovq\t%s, %%rcx` | ✓ |  |  |  |
| 1054 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 1057 | `\tmovq\t$%d, %%r8` |  | ✓ |  |  |
| 1067 | `\txchgq\t%rdi, %rsi` |  | ✓ |  |  |
| 1072 | `\tmovq\t%s, %%rsi` | ✓ |  |  |  |
| 1074 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 1079 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 1081 | `\tmovq\t%s, %%rsi` | ✓ |  |  |  |
| 1086 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 1088 | `\tmovq\t%s, %%rsi` | ✓ |  |  |  |
| 1091 | `\tmovq\t$%d, %%rdx` |  | ✓ |  |  |
| 1108 | `\tmovq\t%s, %%rcx` | ✓ |  |  |  |
| 1110 | `\tmovq\t$%d, %%rdx` |  | ✓ |  |  |
| 1112 | `\tmovq\t%s, %%r8` | ✓ |  |  |  |
| 1117 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 1119 | `\tmovq\t$%d, %%rsi` |  | ✓ |  |  |
| 1121 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 1124 | `\tmovl\t$0, %eax` |  | ✓ |  |  |
| 1545 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 1568 | `\tmovq\t(%s), %s` | ✓ |  |  |  |
| 1576 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 1590 | `\tmovq\t$%lld, %s` | ✓ |  |  |  |
| 1600 | `\tmovq\t%s, %%r8` | ✓ |  |  |  |
| 1602 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 1604 | `\tmovq\t%s, %%rcx` | ✓ |  |  |  |
| 1611 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 1613 | `\tmovq\t%s, %%rsi` | ✓ |  |  |  |
| 1615 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 1619 | `\tcall\tkgpc_memcpy_wrapper` |  |  | ✓ |  |
| 1711 | `\tmovq\t(%s), %s` | ✓ |  |  |  |
| 1713 | `\tmovq\t%s, (%s)` | ✓ |  |  |  |
| 1734 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 1757 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 1761 | `\tmovq\t%s, (%s)` | ✓ |  |  |  |
| 1808 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 1810 | `\txorq\t%%rax, %%rax` |  | ✓ |  |  |
| 1812 | `\tmovq\t%%rax, %s` |  | ✓ |  |  |
| 1814 | `\tmovq\t$%lld, %s` |  |  | ✓ |  |
| 1867 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 1887 | `\tmovq\t%%rax, %s` | ✓ |  |  |  |
| 1899 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 2057 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 2063 | `\tmovq\t$%lld, %s` |  |  | ✓ |  |
| 2080 | `\tmovq\t%%rax, %s` | ✓ |  |  |  |
| 2094 | `\tleaq\t%s_VMT(%%rip), %s` | ✓ |  |  |  |
| 2099 | `\tmovq\t%s, (%s)` | ✓ |  |  |  |
| 2119 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 2123 | `\tcall\t%s` |  |  | ✓ |  |
| 2130 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 2135 | `\tmovq\t%s, (%s)` | ✓ |  |  |  |
| 2168 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 2177 | `\tmovq\t-%d(%%rbp), %s` |  | ✓ |  |  |
| 2185 | `\tmovq\t(%s), %s` |  |  | ✓ |  |
| 2190 | `\tcall\t%s` |  |  | ✓ |  |
| 2214 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 2235 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 2243 | `\tmovq\t%s, (%s)` | ✓ |  |  |  |
| 2280 | `\tmovq\t$32, %s` | ✓ |  |  |  |
| 2285 | `\tmovq\t%s, %%rcx` | ✓ |  |  |  |
| 2287 | `\tmovq\t%s, %%r8` | ✓ |  |  |  |
| 2289 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 2294 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 2296 | `\tmovq\t%s, %%rsi` | ✓ |  |  |  |
| 2298 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 2302 | `\tcall\tkgpc_memcpy_wrapper` |  |  | ✓ |  |
| 2368 | `\tmovq\t%s, %%rcx` | ✓ |  |  |  |
| 2370 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 2372 | `\tmovq\t%s, %%r8` | ✓ |  |  |  |
| 2377 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 2379 | `\tmovq\t%s, %%rsi` | ✓ |  |  |  |
| 2381 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 2415 | `%s%s:\t.string \` |  |  | ✓ |  |
| 2431 | `\tleaq\t%s(%%rip), %s` | ✓ |  |  |  |
| 2443 | `\tmovq\t%s, %%rcx` | ✓ |  |  |  |
| 2445 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 2447 | `\tmovl\t$%d, %%r8d` |  | ✓ |  |  |
| 2452 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 2454 | `\tmovq\t%s, %%rsi` | ✓ |  |  |  |
| 2456 | `\tmovl\t$%d, %%edx` |  | ✓ |  |  |
| 2479 | `\tmovq\t%s, %%rcx` | ✓ |  |  |  |
| 2481 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 2483 | `\tmovq\t$%lld, %%r8` |  | ✓ |  |  |
| 2488 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 2490 | `\tmovq\t%s, %%rsi` | ✓ |  |  |  |
| 2492 | `\tmovq\t$%lld, %%rdx` |  | ✓ |  |  |
| 2538 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 2550 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 2570 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 2582 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 2609 | `\tmovq\t%s, %%rcx` | ✓ |  |  |  |
| 2611 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 2613 | `\tmovq\t$%lld, %%r8` |  | ✓ |  |  |
| 2618 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 2620 | `\tmovq\t%s, %%rsi` | ✓ |  |  |  |
| 2622 | `\tmovq\t$%lld, %%rdx` |  | ✓ |  |  |

---

## `codegen_stmt_builtins.c`

**222** call sites — 127 reg / 64 fixed / 31 const

| ~Line | Instruction pattern | Uses Register_t\* | Fixed reg string | Constant only | Notes |
|-------|---------------------|:-----------------:|:----------------:|:-------------:|-------|
| 198 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 212 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 224 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 271 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 295 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 297 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 299 | `\tmovq\t-%d(%%rbp), %s` |  | ✓ |  |  |
| 307 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 309 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 315 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 317 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 321 | `\tmovl\t$%d, %s` |  |  | ✓ |  |
| 329 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 331 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 333 | `\tmovl\t$%d, %s` |  |  | ✓ |  |
| 390 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 409 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 429 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 431 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 437 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 439 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 445 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 447 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 500 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 518 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 532 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 534 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 539 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 541 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 546 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 548 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 611 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 613 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 618 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 620 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 625 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 627 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 680 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 710 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 795 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 800 | `\tmovq\t-%d(%%rbp), %s` |  | ✓ |  |  |
| 802 | `\tmovq\t-%d(%%rbp), %s` |  | ✓ |  |  |
| 808 | `\tcall\t%s` |  |  | ✓ |  |
| 925 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 930 | `\tmovq\t%rax, %xmm0` |  | ✓ |  |  |
| 935 | `\tmovq\t%s, %%xmm0` | ✓ |  |  |  |
| 951 | `\tmovq\t$0, %s` | ✓ |  |  |  |
| 964 | `\tmovq\t$-1, %s` | ✓ |  |  |  |
| 971 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 973 | `\tmovq\t%s, %%r8` | ✓ |  |  |  |
| 977 | `\tmovq\t$%d, %%r9\tshlq\t$32, %%r9\torq\t%s, %%r9` | ✓ |  |  |  |
| 983 | `\tmovq\t%s, %%r9` | ✓ |  |  |  |
| 985 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 987 | `\tmovq\t%s, %%r8` | ✓ |  |  |  |
| 995 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 997 | `\tmovq\t%s, %%rsi` | ✓ |  |  |  |
| 1001 | `\tmovq\t$%d, %%rdx\tshlq\t$32, %%rdx\torq\t%s, %%rdx` | ✓ |  |  |  |
| 1007 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 1009 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 1011 | `\tmovq\t%s, %%rsi` | ✓ |  |  |  |
| 1019 | `\tcall\tkgpc_str_real_fmt%s` |  |  | ✓ |  |
| 1028 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 1031 | `\tmovq\t$%d, %%r8` |  | ✓ |  |  |
| 1036 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 1044 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 1047 | `\tmovq\t$%d, %%rsi` |  | ✓ |  |  |
| 1052 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 1060 | `\tcall\tkgpc_str_real%s` |  |  | ✓ |  |
| 1070 | `\tmovq\t%s, %%rcx` | ✓ |  |  |  |
| 1072 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 1076 | `\tmovq\t%s, %%r8` | ✓ |  |  |  |
| 1079 | `\tmovq\t$%d, %%r9` |  | ✓ |  |  |
| 1084 | `\tmovq\t%s, %%r8` | ✓ |  |  |  |
| 1090 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 1092 | `\tmovq\t%s, %%rsi` | ✓ |  |  |  |
| 1096 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 1099 | `\tmovq\t$%d, %%rcx` |  | ✓ |  |  |
| 1104 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 1112 | `\tcall\tkgpc_str_int64_fmt%s` |  |  | ✓ |  |
| 1119 | `\tmovq\t%s, %%rcx` | ✓ |  |  |  |
| 1123 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 1126 | `\tmovq\t$%d, %%r8` |  | ✓ |  |  |
| 1131 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 1137 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 1141 | `\tmovq\t%s, %%rsi` | ✓ |  |  |  |
| 1144 | `\tmovq\t$%d, %%rdx` |  | ✓ |  |  |
| 1149 | `\tmovq\t%s, %%rsi` | ✓ |  |  |  |
| 1158 | `\tcall\tkgpc_str_int64%s` |  |  | ✓ |  |
| 1207 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 1218 | `\txorq\t%rcx, %rcx` |  | ✓ |  |  |
| 1222 | `\txorq\t%rdi, %rdi` |  | ✓ |  |  |
| 1228 | `\tmovq\t%%rax, -%d(%%rbp)` |  | ✓ |  |  |
| 1273 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 1278 | `\tcall\t%s` |  |  | ✓ |  |
| 1286 | `\tmovq\t%%rax, -%d(%%rbp)` |  | ✓ |  |  |
| 1292 | `\tmovq\t-%d(%%rbp), %%rcx` |  | ✓ |  |  |
| 1294 | `\tmovq\t-%d(%%rbp), %%rdx` |  | ✓ |  |  |
| 1299 | `\tmovq\t-%d(%%rbp), %%rdi` |  | ✓ |  |  |
| 1301 | `\tmovq\t-%d(%%rbp), %%rsi` |  | ✓ |  |  |
| 1309 | `\tmovq\t%%rax, -%d(%%rbp)` |  | ✓ |  |  |
| 1316 | `\tmovq\t-%d(%%rbp), %%rax` |  | ✓ |  |  |
| 1318 | `\tmovq\t-%d(%%rbp), %%r11` |  | ✓ |  |  |
| 1319 | `\tmovq\t%rax, (%r11)` |  | ✓ |  |  |
| 1386 | `\tmovb\t$1, -%d(%%rbp)` |  | ✓ |  |  |
| 1388 | `\tmovb\t%s, -%d(%%rbp)` |  | ✓ |  |  |
| 1390 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 1395 | `\tmovb\t$0, -%d(%%rbp)` |  | ✓ |  |  |
| 1397 | `\tmovb\t%s, -%d(%%rbp)` |  | ✓ |  |  |
| 1399 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 1411 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 1434 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 1454 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 1463 | `\tmovq\t-%d(%%rbp), %s` |  | ✓ |  |  |
| 1465 | `\tmovq\t-%d(%%rbp), %s` |  | ✓ |  |  |
| 1467 | `\tmovq\t-%d(%%rbp), %s` |  | ✓ |  |  |
| 1474 | `\tmovq\t$%d, %s` |  |  | ✓ |  |
| 1524 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 1544 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 1564 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 1572 | `\tmovq\t-%d(%%rbp), %s` |  | ✓ |  |  |
| 1574 | `\tmovq\t-%d(%%rbp), %s` |  | ✓ |  |  |
| 1576 | `\tmovq\t-%d(%%rbp), %s` |  | ✓ |  |  |
| 1707 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 1714 | `\tmovq\t%s, %%rcx` | ✓ |  |  |  |
| 1719 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 1724 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 1729 | `\tmovq\t%s, %%rsi` | ✓ |  |  |  |
| 1733 | `\tcall\t%s` |  |  | ✓ |  |
| 1746 | `\tmovq\t%%rax, -%d(%%rbp)` |  | ✓ |  |  |
| 1752 | `\tmovq\t%s, %%rax` | ✓ |  |  |  |
| 1758 | `\tmovb\t-%d(%%rbp), %%dl` |  | ✓ |  |  |
| 1759 | `\tmovb\t%dl, (%rax)` |  | ✓ |  |  |
| 1763 | `\tmovw\t-%d(%%rbp), %%dx` |  | ✓ |  |  |
| 1764 | `\tmovw\t%dx, (%rax)` |  | ✓ |  |  |
| 1768 | `\tmovq\t-%d(%%rbp), %%rdx` |  | ✓ |  |  |
| 1769 | `\tmovq\t%rdx, (%rax)` |  | ✓ |  |  |
| 1774 | `\tmovl\t-%d(%%rbp), %%edx` |  | ✓ |  |  |
| 1775 | `\tmovl\t%edx, (%rax)` |  | ✓ |  |  |
| 1785 | `\tmovq\t-%d(%%rbp), %%rdx` |  | ✓ |  |  |
| 1787 | `\tmovq\t-%d(%%rbp), %%rax` |  | ✓ |  |  |
| 1788 | `\tmovq\t%rdx, (%rax)` |  | ✓ |  |  |
| 1793 | `\tmovl\t-%d(%%rbp), %%edx` |  | ✓ |  |  |
| 1795 | `\tmovq\t-%d(%%rbp), %%rax` |  | ✓ |  |  |
| 1796 | `\tmovl\t%edx, (%rax)` |  | ✓ |  |  |
| 1833 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 1871 | `\tmovl\t$1, %s` | ✓ |  |  |  |
| 1900 | `\timull\t$%lld, %s` | ✓ |  |  |  |
| 1909 | `\tnegq\t%s` | ✓ |  |  |  |
| 1914 | `\tnegl\t%s` | ✓ |  |  |  |
| 1950 | `\tmovl\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 1989 | `\tmovl\t-%d(%%rbp), %%eax` |  | ✓ |  |  |
| 1995 | `\taddl\t%s, (%s)` | ✓ |  |  |  |
| 2039 | `\taddl\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 2049 | `\taddl\t%s, -%d(%s)` | ✓ |  |  |  |
| 2070 | `\tmovl\t-%d(%%rbp), %%eax` |  | ✓ |  |  |
| 2076 | `\taddl\t%s, (%s)` | ✓ |  |  |  |
| 2098 | `\tmovl\t-%d(%%rbp), %%eax` |  | ✓ |  |  |
| 2104 | `\taddl\t%s, (%s)` | ✓ |  |  |  |
| 2162 | `\tandl\t$255, %s` | ✓ |  |  |  |
| 2167 | `\tbtrl\t%s, (%s)` | ✓ |  |  |  |
| 2172 | `\tbtsl\t%s, (%s)` | ✓ |  |  |  |
| 2234 | `\tmovq\t$%lld, %s` | ✓ |  |  |  |
| 2241 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 2243 | `\tmovq\t%s, %%rcx` | ✓ |  |  |  |
| 2248 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 2250 | `\tmovq\t%s, %%rsi` | ✓ |  |  |  |
| 2306 | `\tmovq\t%s, %%rcx` | ✓ |  |  |  |
| 2311 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 2350 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 2358 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 2495 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 2511 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 2590 | `\tmovq\t-%d(%%rbp), %s` |  | ✓ |  |  |
| 2595 | `\tmovq\t$-1, %s` |  |  | ✓ |  |
| 2606 | `\tmovq\t-%d(%%rbp), %s` |  | ✓ |  |  |
| 2611 | `\tmovq\t$-1, %s` |  |  | ✓ |  |
| 2617 | `\tmovq\t$-1, %s` |  |  | ✓ |  |
| 2624 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 2635 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 2640 | `\tmovq\t%%rax, %s` | ✓ |  |  |  |
| 2644 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 2650 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 2659 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 2677 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 2689 | `\tmovq\t$-1, %s` |  |  | ✓ |  |
| 2748 | `\tmovq\t-%d(%%rbp), %s` |  | ✓ |  |  |
| 2753 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 2759 | `\txorq\t%s, %s` |  |  | ✓ |  |
| 2767 | `\tmovq\t$%d, %s` |  |  | ✓ |  |
| 2798 | `\tmovq\t-%d(%%rbp), %s` |  | ✓ |  |  |
| 2803 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 2809 | `\txorq\t%s, %s` |  |  | ✓ |  |
| 2855 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 2863 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 2910 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 2923 | `\tmovq\t-%d(%%rbp), %s` |  | ✓ |  |  |
| 2928 | `\txorq\t%s, %s` |  |  | ✓ |  |
| 2933 | `\tmovq\t-%d(%%rbp), %s` |  | ✓ |  |  |
| 2965 | `\tmovq\t-%d(%%rbp), %s` |  | ✓ |  |  |
| 2970 | `\txorq\t%s, %s` |  |  | ✓ |  |
| 2975 | `\tmovq\t-%d(%%rbp), %s` |  | ✓ |  |  |
| 2979 | `\tmovl\t$255, %s` |  |  | ✓ |  |
| 3034 | `\tmovq\t-%d(%%rbp), %s` |  | ✓ |  |  |
| 3039 | `\txorq\t%s, %s` |  |  | ✓ |  |
| 3044 | `\tmovq\t-%d(%%rbp), %s` |  | ✓ |  |  |
| 3069 | `\tmovq\t-%d(%%rbp), %s` |  | ✓ |  |  |
| 3074 | `\txorq\t%s, %s` |  |  | ✓ |  |
| 3125 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 3138 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 3155 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 3162 | `\tmovq\t-%d(%%rbp), %s` |  | ✓ |  |  |
| 3168 | `\tmovq\t-%d(%%rbp), %s` |  | ✓ |  |  |
| 3400 | `%s%s:\t.string \` |  |  | ✓ |  |
| 3402 | `\tleaq\t%s(%%rip), %s` |  |  | ✓ |  |
| 3413 | `%s%s:\t.string \` |  |  | ✓ |  |
| 3415 | `\tleaq\t%s(%%rip), %s` |  |  | ✓ |  |
| 3425 | `%s%s:\t.string \` |  |  | ✓ |  |
| 3427 | `\tleaq\t%s(%%rip), %s` |  |  | ✓ |  |
| 3431 | `\tmovl\t$%d, %s` |  |  | ✓ |  |
| 3433 | `\txorl\t%eax, %eax` |  | ✓ |  |  |
| 3438 | `%s:` |  |  | ✓ |  |
| 3472 | `\tcall\t%s` |  |  | ✓ |  |

---

## `codegen_stmt_calls_and_control.c`

**265** call sites — 133 reg / 52 fixed / 80 const

| ~Line | Instruction pattern | Uses Register_t\* | Fixed reg string | Constant only | Notes |
|-------|---------------------|:-----------------:|:----------------:|:-------------:|-------|
| 312 | `\tmovq\t%s, %%rcx` | ✓ |  |  |  |
| 314 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 315 | `\tmovl\t$10, %r8d` |  | ✓ |  |  |
| 320 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 322 | `\tmovq\t%s, %%rsi` | ✓ |  |  |  |
| 323 | `\tmovl\t$10, %edx` |  | ✓ |  |  |
| 399 | `\tmovq\t$0, (%s)` | ✓ |  |  |  |
| 650 | `\tmovq\t(%s), %s` | ✓ |  |  |  |
| 653 | `\tmovq\t%s, (%s)` | ✓ |  |  |  |
| 656 | `\tmovq\t8(%s), %s` | ✓ |  |  |  |
| 659 | `\tmovq\t%s, 8(%s)` | ✓ |  |  |  |
| 868 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 874 | `\tmovq\t%%rax, %s` | ✓ |  |  |  |
| 1256 | `\tmovb\t$1, (%s)` | ✓ |  |  |  |
| 1263 | `\tmovb\t%s, 1(%s)` | ✓ |  |  |  |
| 1304 | `\tmovq\t%s, %%xmm0` | ✓ |  |  |  |
| 1305 | `\tcvtsd2ss\t%xmm0, %xmm0` |  | ✓ |  |  |
| 1307 | `\tmovd\t%%xmm0, %s` | ✓ |  |  |  |
| 1316 | `\tcvtsi2ss\t%s, %%xmm0` | ✓ |  |  |  |
| 1318 | `\tmovd\t%%xmm0, %s` | ✓ |  |  |  |
| 1496 | `\tmovl\t%s, %s(%%rip)` | ✓ |  |  |  |
| 1510 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 1519 | `\tmovl\t%s, (%s)` | ✓ |  |  |  |
| 1662 | `\tmovl\t%s, -%d(%s)` | ✓ |  |  |  |
| 1697 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 1711 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 1732 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 1782 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 1786 | `\tmovq\t%%rax, %s` | ✓ |  |  |  |
| 1797 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 1804 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 1831 | `\tmovb\t$1, (%s)` | ✓ |  |  |  |
| 1834 | `\tmovb\t%s, 1(%s)` | ✓ |  |  |  |
| 1872 | `\tmovq\t%s, (%s)` | ✓ |  |  |  |
| 1887 | `\tmovw\t%s, (%s)` | ✓ |  |  |  |
| 1901 | `\tmovb\t%s, (%s)` | ✓ |  |  |  |
| 1907 | `\tmovl\t%s, (%s)` | ✓ |  |  |  |
| 1924 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 1945 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 1993 | `\tcvtsi2ss\t%s, %%xmm0` | ✓ |  |  |  |
| 1995 | `\tmovd\t%%xmm0, %s` | ✓ |  |  |  |
| 2003 | `\tmovq\t%s, %%xmm0` | ✓ |  |  |  |
| 2004 | `\tcvtsd2ss\t%xmm0, %xmm0` |  | ✓ |  |  |
| 2006 | `\tmovd\t%%xmm0, %s` | ✓ |  |  |  |
| 2131 | `\tmovq\t%s, (%s)` | ✓ |  |  |  |
| 2146 | `\tmovb\t%s, (%s)` | ✓ |  |  |  |
| 2160 | `\tmovw\t%s, (%s)` | ✓ |  |  |  |
| 2166 | `\tmovl\t%s, (%s)` | ✓ |  |  |  |
| 2223 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 2228 | `\tmovq\t%%rax, %s` | ✓ |  |  |  |
| 2230 | `\tincq\t%s` | ✓ |  |  |  |
| 2233 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 2235 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 2237 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 2323 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 2346 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 2372 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 2397 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 2444 | `\tmovq\t%s, (%s)` | ✓ |  |  |  |
| 2459 | `\tmovb\t%s, (%s)` | ✓ |  |  |  |
| 2475 | `\tmovw\t%s, (%s)` | ✓ |  |  |  |
| 2481 | `\tmovl\t%s, (%s)` | ✓ |  |  |  |
| 2623 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 2629 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 2713 | `\tcall\t%s` |  |  | ✓ |  |
| 3001 | `\tmovq\t(%s), %s` | ✓ |  |  |  |
| 3013 | `\tmovq\t8(%s), %s` | ✓ |  |  |  |
| 3016 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 3022 | `\tmovq\t(%s), %s` | ✓ |  |  |  |
| 3028 | `\tmovq\t(%s), %s` | ✓ |  |  |  |
| 3053 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 3073 | `\tmovq\t-%d(%%rbp), %s` |  | ✓ |  |  |
| 3083 | `\tmovq\t-%d(%%rbp), %%r11` |  | ✓ |  |  |
| 3085 | `\tcall\t*%%r11` |  | ✓ |  |  |
| 3172 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 3201 | `\tmovq\t%%rbp, %s` |  | ✓ |  |  |
| 3206 | `\tmovq\t-%d(%%rbp), %s` |  | ✓ |  |  |
| 3214 | `\tmovq\t-%d(%%rbp), %s` |  | ✓ |  |  |
| 3222 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 3285 | `\tleaq\t%s_VMT(%%rip), %%r11` |  | ✓ |  |  |
| 3288 | `\tmovq\t%%r11, (%s)` |  | ✓ |  |  |
| 3291 | `\tmovq\t%s, %%r11` |  | ✓ |  |  |
| 3295 | `\tmovq\t(%%r11), %%r11` |  | ✓ |  |  |
| 3299 | `\tmovq\t%d(%%r11), %%r11` |  | ✓ |  |  |
| 3301 | `\tcall\t*%%r11` |  | ✓ |  |  |
| 3306 | `\tcall\t%s` |  |  | ✓ |  |
| 3400 | `%s:` |  |  | ✓ |  |
| 3407 | `%s:` |  |  | ✓ |  |
| 3410 | `%s:` |  |  | ✓ |  |
| 3444 | `%s:` |  |  | ✓ |  |
| 3452 | `%s:` |  |  | ✓ |  |
| 3457 | `%s:` |  |  | ✓ |  |
| 3482 | `%s:` |  |  | ✓ |  |
| 3498 | `%s:` |  |  | ✓ |  |
| 3652 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 3657 | `\tmovq\t%%rax, -%d(%%rbp)` |  | ✓ |  |  |
| 3661 | `%s:` |  |  | ✓ |  |
| 3676 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 3678 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 3703 | `\tmovq\t%%rax, (%s)` | ✓ |  |  |  |
| 3711 | `%s:` |  |  | ✓ |  |
| 3714 | `%s:` |  |  | ✓ |  |
| 3722 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 3724 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 3727 | `\ttestl\t%eax, %eax` |  | ✓ |  |  |
| 3729 | `\tjne\t%s` |  |  | ✓ |  |
| 3733 | `%s:` |  |  | ✓ |  |
| 3777 | `\tmovq\t(%s), %s` | ✓ |  |  |  |
| 3781 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 3821 | `\tmovl\t%lld(%s), %s` | ✓ |  |  |  |
| 3824 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 3828 | `\tmovq\t%lld(%s), %s` | ✓ |  |  |  |
| 3831 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 3837 | `\tmovq\t$0, -%d(%%rbp)` |  | ✓ |  |  |
| 3844 | `%s:` |  |  | ✓ |  |
| 3869 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 3884 | `\tleaq\t%lld(%s), %s` | ✓ |  |  |  |
| 3886 | `\tmovq\t(%s), %s` | ✓ |  |  |  |
| 3899 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 3983 | `\timulq\t$%d, %s` | ✓ |  |  |  |
| 4015 | `\tmovq\t(%s,%s), %s` | ✓ |  |  |  |
| 4063 | `\tmovq\t%s, (%s)` | ✓ |  |  |  |
| 4077 | `%s:` |  |  | ✓ |  |
| 4083 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 4085 | `\tincq\t%s` | ✓ |  |  |  |
| 4087 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 4093 | `%s:` |  |  | ✓ |  |
| 4098 | `\tmovq\t-%d(%%rbp), %%rax` |  | ✓ |  |  |
| 4102 | `\tcmpq\t-%d(%%rbp), %%rax` |  | ✓ |  |  |
| 4106 | `\tjl\t%s` |  |  | ✓ |  |
| 4110 | `%s:` |  |  | ✓ |  |
| 4152 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 4158 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 4167 | `\tmovq\t%%rax, -%d(%%rbp)` |  | ✓ |  |  |
| 4171 | `\tmovq\t$1, -%d(%%rbp)` |  | ✓ |  |  |
| 4178 | `%s:` |  |  | ✓ |  |
| 4194 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 4205 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 4211 | `\tdecq\t%s` | ✓ |  |  |  |
| 4225 | `\tmovzbl\t(%s,%s,1), %s` | ✓ |  |  |  |
| 4249 | `\tmovb\t%s, (%s)` | ✓ |  |  |  |
| 4261 | `%s:` |  |  | ✓ |  |
| 4265 | `\tincq\t-%d(%%rbp)` |  | ✓ |  |  |
| 4269 | `%s:` |  |  | ✓ |  |
| 4273 | `\tmovq\t-%d(%%rbp), %%rax` |  | ✓ |  |  |
| 4275 | `\tcmpq\t-%d(%%rbp), %%rax` |  | ✓ |  |  |
| 4279 | `\tjle\t%s` |  |  | ✓ |  |
| 4283 | `%s:` |  |  | ✓ |  |
| 4306 | `\tmovl\t$%lld, -%d(%%rbp)` |  | ✓ |  |  |
| 4310 | `%s:` |  |  | ✓ |  |
| 4323 | `\tmovl\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 4357 | `\tmovslq\t%s, %s` | ✓ |  |  |  |
| 4362 | `\tmovl\t%s, (%s)` | ✓ |  |  |  |
| 4372 | `%s:` |  |  | ✓ |  |
| 4374 | `\tincl\t-%d(%%rbp)` |  | ✓ |  |  |
| 4377 | `%s:` |  |  | ✓ |  |
| 4379 | `\tcmpl\t$%d, -%d(%%rbp)` |  | ✓ |  |  |
| 4381 | `\tjle\t%s` |  |  | ✓ |  |
| 4384 | `%s:` |  |  | ✓ |  |
| 4409 | `\tmovl\t$0, -%d(%%rbp)` |  | ✓ |  |  |
| 4413 | `%s:` |  |  | ✓ |  |
| 4456 | `\tmovl\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 4458 | `\tmovl\t%s, %s` | ✓ |  |  |  |
| 4460 | `\tshrl\t$3, %s` | ✓ |  |  |  |
| 4462 | `\tmovl\t%s, %s` | ✓ |  |  |  |
| 4464 | `\tandl\t$7, %s` | ✓ |  |  |  |
| 4467 | `\tmovzbl\t(%s,%s,1), %s` | ✓ |  |  |  |
| 4469 | `\tmovl\t$1, %s` | ✓ |  |  |  |
| 4483 | `\tmovb\t%s, %%cl` |  | ✓ |  |  |
| 4485 | `\tshll\t%%cl, %s` | ✓ |  |  |  |
| 4487 | `\ttestl\t%s, %s` | ✓ |  |  |  |
| 4489 | `\tjz\t%s` |  |  | ✓ |  |
| 4538 | `\tmovslq\t%s, %s` | ✓ |  |  |  |
| 4543 | `\tmovl\t%s, (%s)` | ✓ |  |  |  |
| 4557 | `%s:` |  |  | ✓ |  |
| 4561 | `%s:` |  |  | ✓ |  |
| 4563 | `\tincl\t-%d(%%rbp)` |  | ✓ |  |  |
| 4566 | `%s:` |  |  | ✓ |  |
| 4568 | `\tcmpl\t$%d, -%d(%%rbp)` |  | ✓ |  |  |
| 4570 | `\tjle\t%s` |  |  | ✓ |  |
| 4573 | `%s:` |  |  | ✓ |  |
| 4602 | `\tmovl\t$%d, -%d(%%rbp)` |  | ✓ |  |  |
| 4609 | `%s:` |  |  | ✓ |  |
| 4625 | `\tmovl\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 4642 | `\tsubl\t$%d, %s` | ✓ |  |  |  |
| 4657 | `\tsall\t$%d, %s` | ✓ |  |  |  |
| 4662 | `\timull\t$%d, %s` | ✓ |  |  |  |
| 4668 | `\tmovslq\t%s, %s` | ✓ |  |  |  |
| 4670 | `\taddq\t%s, %s` | ✓ |  |  |  |
| 4679 | `\tmovzbl\t(%s), %s` | ✓ |  |  |  |
| 4682 | `\tmovzwl\t(%s), %s` | ✓ |  |  |  |
| 4685 | `\tmovl\t(%s), %s` | ✓ |  |  |  |
| 4688 | `\tmovq\t(%s), %s` | ✓ |  |  |  |
| 4717 | `\tmovb\t%s, (%s)` | ✓ |  |  |  |
| 4728 | `\tmovw\t%s, (%s)` | ✓ |  |  |  |
| 4731 | `\tmovl\t%s, (%s)` | ✓ |  |  |  |
| 4734 | `\tmovq\t%s, (%s)` | ✓ |  |  |  |
| 4745 | `\tincl\t-%d(%%rbp)` |  | ✓ |  |  |
| 4752 | `%s:` |  |  | ✓ |  |
| 4756 | `\tcmpl\t$%d, -%d(%%rbp)` |  | ✓ |  |  |
| 4760 | `\tjle\t%s` |  |  | ✓ |  |
| 4764 | `%s:` |  |  | ✓ |  |
| 4854 | `\tmovl\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 4861 | `%s:` |  |  | ✓ |  |
| 4869 | `%s:` |  |  | ✓ |  |
| 4884 | `\tcmpl\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 4887 | `\tje\t%s` |  |  | ✓ |  |
| 4897 | `%s:` |  |  | ✓ |  |
| 4915 | `\tmovl\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 4940 | `\t%s\t%s, %s` |  |  | ✓ |  |
| 4950 | `\t%s\t%s` |  |  | ✓ |  |
| 4952 | `\tjmp\t%s` |  |  | ✓ |  |
| 4960 | `%s:` |  |  | ✓ |  |
| 5017 | `\tmovl\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 5064 | `\tmovq\t-%d(%%rbp), %s` |  | ✓ |  |  |
| 5067 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 5071 | `\tcmpl\t$0, %s` |  |  | ✓ |  |
| 5073 | `\tje\t%s` |  |  | ✓ |  |
| 5084 | `\tje\t%s` |  |  | ✓ |  |
| 5097 | `\tcmpl\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 5099 | `\tje\t%s` |  |  | ✓ |  |
| 5128 | `\tcmpl\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 5136 | `\tjl\t%s` |  |  | ✓ |  |
| 5159 | `\tcmpl\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 5167 | `\tjle\t%s` |  |  | ✓ |  |
| 5172 | `%s:` |  |  | ✓ |  |
| 5181 | `\tjmp\t%s` |  |  | ✓ |  |
| 5185 | `%s:` |  |  | ✓ |  |
| 5189 | `\tjmp\t%s` |  |  | ✓ |  |
| 5193 | `%s:` |  |  | ✓ |  |
| 5205 | `%s:` |  |  | ✓ |  |
| 5302 | `%s:` |  |  | ✓ |  |
| 5320 | `\t# TRY/EXCEPT: push runtime except frame` |  |  | ✓ |  |
| 5331 | `\tmovq\t%%rax, -%d(%%rbp)` |  | ✓ |  |  |
| 5343 | `\tmovq\t%%rax, %%rdi` |  | ✓ |  |  |
| 5352 | `\ttestl\t%eax, %eax` |  | ✓ |  |  |
| 5354 | `\tjne\t%s` |  |  | ✓ |  |
| 5364 | `\t# TRY/EXCEPT: pop runtime except frame (normal exit)` |  |  | ✓ |  |
| 5372 | `%s:` |  |  | ✓ |  |
| 5382 | `\t# EXCEPT block with no handlers` |  |  | ✓ |  |
| 5387 | `%s:` |  |  | ✓ |  |
| 5438 | `\t# ON exception type check` |  |  | ✓ |  |
| 5440 | `\tmovq\tkgpc_current_exception(%rip), %rax` |  | ✓ |  |  |
| 5442 | `\ttestq\t%rax, %rax` |  | ✓ |  |  |
| 5444 | `\tje\t%s` |  |  | ✓ |  |
| 5446 | `\tmovq\t(%rax), %rax` |  | ✓ |  |  |
| 5447 | `\tmovq\t56(%rax), %rax` |  | ✓ |  |  |
| 5452 | `\tmovq\t%rax, %rcx` |  | ✓ |  |  |
| 5454 | `\tleaq\t%s_TYPEINFO(%%rip), %%rdx` |  | ✓ |  |  |
| 5458 | `\tmovq\t%rax, %rdi` |  | ✓ |  |  |
| 5460 | `\tleaq\t%s_TYPEINFO(%%rip), %%rsi` |  | ✓ |  |  |
| 5466 | `\ttestl\t%eax, %eax` |  | ✓ |  |  |
| 5468 | `\tje\t%s` |  |  | ✓ |  |
| 5479 | `\tmovq\tkgpc_current_exception(%%rip), %%rax` |  | ✓ |  |  |
| 5481 | `\tmovq\t%%rax, -%d(%%rbp)` |  | ✓ |  |  |
| 5497 | `\tjmp\t%s` |  |  | ✓ |  |
| 5504 | `%s:` |  |  | ✓ |  |
| 5515 | `\t# RAISE statement` |  |  | ✓ |  |
| 5537 | `\t# RAISE: pop runtime except frame (local raise)` |  |  | ✓ |  |
| 5548 | `\tmovq\t$0, kgpc_current_exception(%rip)` |  |  | ✓ |  |
| 5560 | `%s:` |  |  | ✓ |  |
| 5568 | `\tmovq\tkgpc_current_exception(%%rip), %%rdi` |  | ✓ |  |  |
| 5572 | `\tud2` |  |  | ✓ |  |
| 5592 | `\t# INHERITED statement ignored (no class context)` |  |  | ✓ |  |
| 5604 | `\t# INHERITED statement without parent call` |  |  | ✓ |  |

---

## `codegen_stmt_dispatch.c`

**44** call sites — 2 reg / 28 fixed / 14 const

| ~Line | Instruction pattern | Uses Register_t\* | Fixed reg string | Constant only | Notes |
|-------|---------------------|:-----------------:|:----------------:|:-------------:|-------|
| 443 | `%s:` |  |  | ✓ |  |
| 457 | `\tjmp\t%s` |  |  | ✓ |  |
| 647 | `%s:` |  |  | ✓ |  |
| 658 | `\tjmp\t%s` |  |  | ✓ |  |
| 731 | `strdup(src` |  |  | ✓ |  |
| 776 | `strdup(buf` |  | ✓ |  | (context-inferred) |
| 779 | `strdup(buf` |  | ✓ |  | (context-inferred) |
| 781 | `strdup(buf` |  | ✓ |  | (context-inferred) |
| 783 | `strdup("\tmovl\t$0, %eax\n"` |  | ✓ |  | (context-inferred) |
| 784 | `strdup("\tcall\tfpc_in_sin_real\n"` |  | ✓ |  | (context-inferred) |
| 785 | `strdup("\tmovsd\t%xmm0, (%r12)\n"` |  | ✓ |  | (context-inferred) |
| 791 | `strdup(buf` |  | ✓ |  | (context-inferred) |
| 793 | `strdup("\tmovl\t$0, %eax\n"` |  | ✓ |  | (context-inferred) |
| 794 | `strdup("\tcall\tfpc_in_cos_real\n"` |  | ✓ |  | (context-inferred) |
| 795 | `strdup("\tmovsd\t%xmm0, (%r13)\n"` |  | ✓ |  | (context-inferred) |
| 803 | `strdup(".intel_syntax noprefix\n"` |  | ✓ |  | (context-inferred) |
| 817 | `strdup(stripped_src` |  |  | ✓ |  |
| 820 | `strdup("\n.att_syntax prefix\n"` |  |  | ✓ |  |
| 1046 | `substituted` |  |  | ✓ |  |
| 1049 | `cleaned` |  |  | ✓ |  |
| 1057 | `strdup("\n.att_syntax prefix\n"` |  |  | ✓ |  |
| 1064 | `\t# EXIT statement` |  |  | ✓ |  |
| 1128 | `\tmovl\t%s, %%eax` | ✓ |  |  |  |
| 1138 | `\tmovq\t%s, %%xmm0` | ✓ |  |  |  |
| 1168 | `\tmovq\t-%d(%%rbp), %%rcx` |  | ✓ |  |  |
| 1171 | `\tleaq\t-%d(%%rbp), %%rdx` |  | ✓ |  |  |
| 1174 | `\tmovq\t$%lld, %%r8` |  | ✓ |  |  |
| 1180 | `\tmovq\t-%d(%%rbp), %%rdi` |  | ✓ |  |  |
| 1183 | `\tleaq\t-%d(%%rbp), %%rsi` |  | ✓ |  |  |
| 1186 | `\tmovq\t$%lld, %%rdx` |  | ✓ |  |  |
| 1196 | `\tmovq\t-%d(%%rbp), %%rax` |  | ✓ |  |  |
| 1206 | `\tleaq\t-%d(%%rbp), %%rcx` |  | ✓ |  |  |
| 1208 | `\tmovl\t$%d, %%edx` |  | ✓ |  |  |
| 1213 | `\tleaq\t-%d(%%rbp), %%rdi` |  | ✓ |  |  |
| 1215 | `\tmovl\t$%d, %%esi` |  | ✓ |  |  |
| 1240 | `\tmovl\t-%d(%%rbp), %%eax` |  | ✓ |  |  |
| 1253 | `%s:` |  |  | ✓ |  |
| 1259 | `\tmovq\t-%d(%%rbp), %%rbx` |  | ✓ |  |  |
| 1264 | `\tmovq\t-%d(%%rbp), %%r12` |  | ✓ |  |  |
| 1269 | `\tmovq\t-%d(%%rbp), %%r13` |  | ✓ |  |  |
| 1274 | `\tmovq\t-%d(%%rbp), %%r14` |  | ✓ |  |  |
| 1279 | `\tmovq\t-%d(%%rbp), %%r15` |  | ✓ |  |  |
| 1281 | `\tleave` |  |  | ✓ |  |
| 1282 | `\tret` |  |  | ✓ |  |

---

## `codegen_stmt_infrastructure.c`

**68** call sites — 40 reg / 20 fixed / 8 const

| ~Line | Instruction pattern | Uses Register_t\* | Fixed reg string | Constant only | Notes |
|-------|---------------------|:-----------------:|:----------------:|:-------------:|-------|
| 15 | `buffer` |  |  | ✓ |  |
| 26 | `buffer` |  |  | ✓ |  |
| 42 | `buffer` |  |  | ✓ |  |
| 51 | `buffer` |  |  | ✓ |  |
| 198 | `\tmovabsq\t$%lld, %s` | ✓ |  |  |  |
| 201 | `\tcmpq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 213 | `\tcmpl\t$%lld, -%d(%%rbp)` |  | ✓ |  |  |
| 480 | `rodata` |  |  | ✓ |  |
| 942 | `\tcvtsi2sdl\t%s, %%xmm0` | ✓ |  |  |  |
| 945 | `\tmovq\t%%xmm0, %s` | ✓ |  |  |  |
| 1326 | `\tmovq\t%s, %%rcx` | ✓ |  |  |  |
| 1328 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 1330 | `\tmovl\t$%d, %%r8d` |  | ✓ |  |  |
| 1335 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 1337 | `\tmovq\t%s, %%rsi` | ✓ |  |  |  |
| 1339 | `\tmovl\t$%d, %%edx` |  | ✓ |  |  |
| 1358 | `\tmovq\t%s, %%rcx` | ✓ |  |  |  |
| 1360 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 1362 | `\tmovl\t$%d, %%r8d` |  | ✓ |  |  |
| 1367 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 1369 | `\tmovq\t%s, %%rsi` | ✓ |  |  |  |
| 1371 | `\tmovl\t$%d, %%edx` |  | ✓ |  |  |
| 1407 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 1429 | `\tleaq\t-%d(%%rbp), %%rcx` |  | ✓ |  |  |
| 1431 | `\txorq\t%%rdx, %%rdx` |  | ✓ |  |  |
| 1433 | `\tmovl\t$%d, %%r8d` |  | ✓ |  |  |
| 1438 | `\tleaq\t-%d(%%rbp), %%rdi` |  | ✓ |  |  |
| 1440 | `\txorq\t%%rsi, %%rsi` |  | ✓ |  |  |
| 1442 | `\tmovl\t$%d, %%edx` |  | ✓ |  |  |
| 1457 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 1461 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 1497 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 1531 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 1547 | `\tmovq\t%s, %%rcx` | ✓ |  |  |  |
| 1549 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 1551 | `\tmovl\t$%d, %%r8d` |  | ✓ |  |  |
| 1553 | `\tmovl\t$%d, %%r9d` |  | ✓ |  |  |
| 1558 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 1560 | `\tmovq\t%s, %%rsi` | ✓ |  |  |  |
| 1562 | `\tmovl\t$%d, %%edx` |  | ✓ |  |  |
| 1564 | `\tmovl\t$%d, %%ecx` |  | ✓ |  |  |
| 1593 | `\tsubq\t$%d, %%rsp` |  | ✓ |  |  |
| 1599 | `\tcall\t%s` |  |  | ✓ |  |
| 1605 | `\taddq\t$%d, %%rsp` |  | ✓ |  |  |
| 1622 | `\tmovl\t%s, %s` | ✓ |  |  |  |
| 1626 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 1647 | `\tmovq\t%s, kgpc_current_exception(%%rip)` | ✓ |  |  |  |
| 1717 | `\ttestl\t%s, %s` | ✓ |  |  |  |
| 1773 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 1799 | `\tmovq\t%s(%%rip), %s` | ✓ |  |  |  |
| 1838 | `\tleaq\t%s(%%rip), %s` | ✓ |  |  |  |
| 1881 | `\tleaq\t%s(%%rip), %s` | ✓ |  |  |  |
| 1919 | `\tleaq\t%s(%%rip), %s` | ✓ |  |  |  |
| 1952 | `%s%s:\t.string \` |  |  | ✓ |  |
| 1957 | `\tleaq\t%s(%%rip), %s` | ✓ |  |  |  |
| 1981 | `\tleaq\t-%d(%s), %s` | ✓ |  |  |  |
| 2064 | `\tleaq\t%s(%%rip), %s` | ✓ |  |  |  |
| 2081 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 2096 | `\tleaq\t-%d(%s), %s` | ✓ |  |  |  |
| 2117 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 2134 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 2173 | `\tleaq\t-%d(%%rbp), %s` |  | ✓ |  |  |
| 2175 | `\txorq\t%%rax, %%rax` |  | ✓ |  |  |
| 2177 | `\tmovq\t%%rax, %s` |  | ✓ |  |  |
| 2179 | `\tmovq\t$%lld, %s` |  |  | ✓ |  |
| 2395 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 2513 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 2518 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |

---

## `expr_tree.c`

**314** call sites — 99 reg / 89 fixed / 126 const

| ~Line | Instruction pattern | Uses Register_t\* | Fixed reg string | Constant only | Notes |
|-------|---------------------|:-----------------:|:----------------:|:-------------:|-------|
| 68 | `\tmovq\t%s, -%d(%%rbp)` |  | ✓ |  |  |
| 79 | `\tmovdqu\t%s, -%d(%%rbp)` |  | ✓ |  |  |
| 95 | `\tmovq\t-%d(%%rbp), %s` |  | ✓ |  |  |
| 104 | `\tmovdqu\t-%d(%%rbp), %s` |  | ✓ |  |  |
| 569 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 673 | `\tmovq\t8(%s), %s` | ✓ |  |  |  |
| 991 | `\tmovl\t$%lld, %s` | ✓ |  |  |  |
| 1057 | `\tmovl\t$%lld, %s` | ✓ |  |  |  |
| 1210 | `%s%s:\t.string \` |  |  | ✓ |  |
| 1213 | `\tleaq\t%s(%%rip), %s` | ✓ |  |  |  |
| 1662 | `\tmov%c\t%s, -%d(%%rbp)` |  | ✓ |  |  |
| 1702 | `\tmovl\t-%d(%%rbp), %s` |  | ✓ |  |  |
| 1768 | `\tmovq\t$%lld, %s` | ✓ |  |  |  |
| 1771 | `\t%s%c\t%s, %s` | ✓ |  |  |  |
| 1780 | `\t%s%c\t%s, %s` |  |  | ✓ |  |
| 1803 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 1810 | `\tleaq\t-%d(%%rbp), %%rdi` |  | ✓ |  |  |
| 1815 | `\tleaq\t%s, %%rcx` |  | ✓ |  |  |
| 1820 | `\tleaq\t%s, %%rdi` |  | ✓ |  |  |
| 1826 | `\tmovq\t%%rax, %s` |  | ✓ |  |  |
| 1887 | `%s%s:\t.quad %s%s` |  |  | ✓ |  |
| 1892 | `\tmovss\t%s(%%rip), %s` |  |  | ✓ |  |
| 1894 | `\tcvtss2sd\t%s, %s` |  |  | ✓ |  |
| 1897 | `\tmovsd\t%s(%%rip), %s` |  |  | ✓ |  |
| 1926 | `\tmovd\t%s, %s` |  |  | ✓ |  |
| 1928 | `\tcvtss2sd\t%s, %s` |  |  | ✓ |  |
| 1931 | `\tmovq\t%s, %s` |  |  | ✓ |  |
| 1937 | `\tmovss\t%s, %s` |  |  | ✓ |  |
| 1939 | `\tcvtss2sd\t%s, %s` |  |  | ✓ |  |
| 1943 | `\tmovq\t%s, %s` |  |  | ✓ |  |
| 1950 | `\tmovq\t%s, %s` |  |  | ✓ |  |
| 1970 | `%s%s:\t.quad %lld%s` |  |  | ✓ |  |
| 1973 | `\tmovsd\t%s(%%rip), %s` |  |  | ✓ |  |
| 1997 | `\t%s\t%s, %s` |  |  | ✓ |  |
| 2016 | `\t%s\t%%xmm1, %%xmm0` |  | ✓ |  |  |
| 2021 | `\tmovq\t%%xmm0, %s` |  | ✓ |  |  |
| 2034 | `\tmovq\t%s, %%xmm0` |  | ✓ |  |  |
| 2035 | `\tpxor\t%xmm1, %xmm1` |  | ✓ |  |  |
| 2036 | `\tsubsd\t%xmm0, %xmm1` |  | ✓ |  |  |
| 2039 | `\tmovq\t%%xmm1, %s` |  | ✓ |  |  |
| 2376 | `\tcmpl\t$0, %s` |  |  | ✓ |  |
| 2378 | `\t%s\t%s` |  |  | ✓ |  |
| 2385 | `\tcmpl\t$0, %s` |  |  | ✓ |  |
| 2387 | `\tsetne\t%s` |  |  | ✓ |  |
| 2389 | `\tmovzbl\t%s, %s` |  |  | ✓ |  |
| 2391 | `\tjmp\t%s` |  |  | ✓ |  |
| 2394 | `%s:` |  |  | ✓ |  |
| 2396 | `\tmovl\t$%d, %s` |  |  | ✓ |  |
| 2399 | `%s:` |  |  | ✓ |  |
| 2440 | `\tmov%s\t%s, %s` |  |  | ✓ |  |
| 2537 | `\tmovl\t%s, %s` | ✓ |  |  |  |
| 2542 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 2838 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 2843 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 2863 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 2867 | `\tmovq\t%%rax, %s` | ✓ |  |  |  |
| 2888 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 2892 | `\tmovq\t%%rax, %s` | ✓ |  |  |  |
| 2912 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 2916 | `\tmovq\t%%rax, %s` | ✓ |  |  |  |
| 2947 | `\tleaq\t%s, %s` | ✓ |  |  |  |
| 2993 | `\tmovq\t%s, -%d(%%rbp)` |  | ✓ |  |  |
| 3006 | `\tmovq\t-%d(%%rbp), %s` |  | ✓ |  |  |
| 3022 | `\tmovl\t%s, %s` |  |  | ✓ |  |
| 3026 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 3072 | `\tmovl\t%s, %s` |  |  | ✓ |  |
| 3085 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 3119 | `\tleaq\t%s, %s` |  |  | ✓ |  |
| 3150 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 3165 | `\tmovq\t%s, %%rcx` | ✓ |  |  |  |
| 3167 | `\tmovq\t-%d(%%rbp), %%rdx` |  | ✓ |  |  |
| 3172 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 3174 | `\tmovq\t-%d(%%rbp), %%rsi` |  | ✓ |  |  |
| 3190 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 3203 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 3211 | `\tmovq\t%s, %%rdx` | ✓ |  |  |  |
| 3213 | `\tmovq\t%s, %%rcx` | ✓ |  |  |  |
| 3218 | `\tmovq\t%s, %%rsi` | ✓ |  |  |  |
| 3220 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 3228 | `\tmovq\t%%rax, %s` | ✓ |  |  |  |
| 3259 | `\tmovl\t%s, -%d(%%rbp)` |  | ✓ |  |  |
| 3267 | `\tmovl\t%s, %s` |  |  | ✓ |  |
| 3273 | `\tmovl\t%s, %%eax` |  | ✓ |  |  |
| 3277 | `\tcltd` |  |  | ✓ |  |
| 3280 | `\tidivl\t%s` |  |  | ✓ |  |
| 3284 | `\tmovl\t%%edx, %s` |  | ✓ |  |  |
| 3372 | `\tneg%s\t%s` |  |  | ✓ |  |
| 3382 | `\tmovslq\t%s, %s` | ✓ |  |  |  |
| 3510 | `\tbswap%c\t%s` |  |  | ✓ |  |
| 3623 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 3840 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 3844 | `\tmovq\t(%s), %s` | ✓ |  |  |  |
| 3850 | `\tmovq\t$%lld, %s` |  |  | ✓ |  |
| 3858 | `\tmovq\t$%lld, %s` |  |  | ✓ |  |
| 3865 | `\tmovq\t$%lld, %s` |  |  | ✓ |  |
| 3882 | `\tmovq\t%%rax, %s` | ✓ |  |  |  |
| 3890 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 3902 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 3905 | `\tmovq\t%s, (%s)` | ✓ |  |  |  |
| 3935 | `\tleaq\t%s(%%rip), %s` | ✓ |  |  |  |
| 3938 | `\tmovq\t%s, (%s)` | ✓ |  |  |  |
| 3971 | `\tleaq\t%s(%%rip), %s` | ✓ |  |  |  |
| 3974 | `\tmovq\t%s, (%s)` | ✓ |  |  |  |
| 4012 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 4171 | `\tmovq\t8(%s), %%r11` | ✓ |  |  |  |
| 4174 | `\tmovq\t%%r11, -%d(%%rbp)` |  | ✓ |  |  |
| 4177 | `\tmovq\t(%s), %%r11` | ✓ |  |  |  |
| 4180 | `\tmovq\t%%r11, -%d(%%rbp)` |  | ✓ |  |  |
| 4229 | `\tmovq\t%%rbp, %s` |  | ✓ |  |  |
| 4234 | `\tmovq\t-%d(%%rbp), %s` |  | ✓ |  |  |
| 4241 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 4258 | `\tleaq\t-%d(%%rbp), %s` |  | ✓ |  |  |
| 4278 | `\tmovq\t%s, %s` |  |  | ✓ |  |
| 4287 | `\tmovq\t-%d(%%rbp), %s` |  | ✓ |  |  |
| 4316 | `\tmovq\t-%d(%%rbp), %%r11` |  | ✓ |  |  |
| 4318 | `\tcall\t*%%r11` |  | ✓ |  |  |
| 4333 | `\tcall\t*%s` | ✓ |  |  |  |
| 4339 | `\t# ERROR: failed to evaluate procedural expression` |  |  | ✓ |  |
| 4379 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 4383 | `\tcall\t*%s` | ✓ |  |  |  |
| 4392 | `\t# ERROR: procedural variable %s not found` |  |  | ✓ |  |
| 4398 | `\t# ERROR: procedural call target missing` |  |  | ✓ |  |
| 4480 | `\tmovq\t%s, -%d(%%rbp)` |  | ✓ |  |  |
| 4496 | `\tmovq\t-%d(%%rbp), %%r11` |  | ✓ |  |  |
| 4501 | `\tmovq\t%s, %%r11` |  | ✓ |  |  |
| 4505 | `\tmovq\t%s, (%%r11)` | ✓ |  |  |  |
| 4509 | `\tmovq\t%%r11, %s` |  | ✓ |  |  |
| 4525 | `\tleaq\t%s_VMT(%%rip), %%r11` |  | ✓ |  |  |
| 4528 | `\tmovq\t%%r11, (%s)` |  | ✓ |  |  |
| 4535 | `\tmovq\t%s, %%r11` |  | ✓ |  |  |
| 4541 | `\tmovq\t(%%r11), %%r11` |  | ✓ |  |  |
| 4546 | `\tmovq\t%d(%%r11), %%r11` |  | ✓ |  |  |
| 4551 | `\tcall\t*%%r11` |  | ✓ |  |  |
| 4618 | `\tcall\t%s` |  |  | ✓ |  |
| 4625 | `\t# ERROR: function call with NULL target` |  |  | ✓ |  |
| 4644 | `\tmovq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 4650 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 4667 | `\tfstpt\t-%d(%%rbp)` |  | ✓ |  |  |
| 4672 | `\tleaq\t-%d(%%rbp), %%rdi` |  | ✓ |  |  |
| 4682 | `\tcvtss2sd\t%xmm0, %xmm0` |  | ✓ |  |  |
| 4723 | `\tmovq\t%%rax, %s` | ✓ |  |  |  |
| 4771 | `\tmovq\t(%s), %s` | ✓ |  |  |  |
| 4779 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 5013 | `\tleaq\t%s(%%rip), %s` | ✓ |  |  |  |
| 5016 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 5041 | `\tmovq\t%s, -%d(%%rbp)` | ✓ |  |  |  |
| 5047 | `\tleaq\t-%d(%%rbp), %s` | ✓ |  |  |  |
| 5057 | `\tleaq\t%s(%%rip), %s` | ✓ |  |  |  |
| 5075 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 5110 | `\tleaq\t%s(%%rip), %s` | ✓ |  |  |  |
| 5135 | `\tmovl\t$%d, %s` | ✓ |  |  |  |
| 5157 | `\tmov%c\t%s, %s` |  |  | ✓ |  |
| 5170 | `\tmovl\t$%u, %s` | ✓ |  |  |  |
| 5195 | `\tleaq\t%s(%%rip), %s` | ✓ |  |  |  |
| 5212 | `\tmovl\t%%eax, %s` | ✓ |  |  |  |
| 5232 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 5306 | `\tleaq\t%s(%%rip), %s` | ✓ |  |  |  |
| 5329 | `\tleaq\t%s(%%rip), %s` | ✓ |  |  |  |
| 5342 | `\tleaq\t%s, %s` | ✓ |  |  |  |
| 5356 | `\tleaq\t%s, %s` | ✓ |  |  |  |
| 5368 | `\tleaq\t%s, %s` | ✓ |  |  |  |
| 5438 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 5449 | `\tmovq\t%%xmm0, %s` | ✓ |  |  |  |
| 5461 | `\tmovq\t%s, %%rdi` | ✓ |  |  |  |
| 5466 | `\tmovq\t%%rax, %s` | ✓ |  |  |  |
| 5508 | `\tmovl\t(%s), %s` | ✓ |  |  |  |
| 5519 | `\tleaq\t%s, %s` | ✓ |  |  |  |
| 5532 | `%s\t%s, %%rdi` |  | ✓ |  |  |
| 5537 | `\tmovq\t%%rax, %s` | ✓ |  |  |  |
| 5546 | `\tmovq\t%%xmm0, %s` | ✓ |  |  |  |
| 5628 | `\tmovslq\t%s, %s` | ✓ |  |  |  |
| 5634 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 5640 | `\tmovzbl\t%s, %s` | ✓ |  |  |  |
| 5651 | `\tmovq\t$%lld, %s` | ✓ |  |  |  |
| 5673 | `\tmovzwl\t%s, %s` | ✓ |  |  |  |
| 5681 | `\tmovzbl\t%s, %s` | ✓ |  |  |  |
| 5686 | `\tmovl\t%s, %s` | ✓ |  |  |  |
| 5733 | `\tmov%s\t%s, %s` |  |  | ✓ |  |
| 6136 | `%s%s:\t.string \` |  |  | ✓ |  |
| 6407 | `\torl\t%s, %s` |  |  | ✓ |  |
| 6415 | `\tandl\t$%u, %s` |  |  | ✓ |  |
| 6426 | `\tmovl\t%s, %s` |  |  | ✓ |  |
| 6428 | `\tnotl\t%s` |  |  | ✓ |  |
| 6430 | `\tandl\t%s, %s` |  |  | ✓ |  |
| 6518 | `\tsubq\t%s, %s` |  |  | ✓ |  |
| 6526 | `\tmovq\t%s, %%rax` |  | ✓ |  |  |
| 6529 | `\tcqto` |  |  | ✓ |  |
| 6532 | `\tmovq\t$%lld, %%r11` |  | ✓ |  |  |
| 6535 | `\tidivq\t%%r11` |  | ✓ |  |  |
| 6538 | `\tmovq\t%%rax, %s` |  | ✓ |  |  |
| 6561 | `\tmovslq\t%s, %s` |  |  | ✓ |  |
| 6566 | `\tmovslq\t%s, %s` |  |  | ✓ |  |
| 6603 | `\tmovq\t$%lld, %s` |  |  | ✓ |  |
| 6619 | `\timulq\t$%lld, %s, %s` |  |  | ✓ |  |
| 6644 | `\tsubq\t%s, %s` |  |  | ✓ |  |
| 6681 | `\tmovslq\t%s, %s` |  |  | ✓ |  |
| 6689 | `\tmovslq\t%s, %s` |  |  | ✓ |  |
| 6710 | `\tmovl\t%s, %s` | ✓ |  |  |  |
| 6732 | `\tinc%c\t%s` |  |  | ✓ |  |
| 6761 | `\tmovslq\t%s, %s` | ✓ |  |  |  |
| 6772 | `\tandl\t%s, %s` |  |  | ✓ |  |
| 6784 | `\tandl\t%s, %s` |  |  | ✓ |  |
| 6788 | `\txorl\t%s, %s` |  |  | ✓ |  |
| 6839 | `\tmovl\t%s, %s` |  |  | ✓ |  |
| 6848 | `\tmovl\t%s, %s` |  |  | ✓ |  |
| 6879 | `\tmovslq\t%s, %s` | ✓ |  |  |  |
| 6894 | `\tmovq\t%s, %%rax` |  | ✓ |  |  |
| 6896 | `\txorq\t%rdx, %rdx` |  | ✓ |  |  |
| 6898 | `\tcqo` |  |  | ✓ |  |
| 6902 | `\tmovq\t%s, %s` |  |  | ✓ |  |
| 6904 | `\t%s\t%s` |  |  | ✓ |  |
| 6907 | `\tmovq\t%%rdx, %s` |  | ✓ |  |  |
| 6914 | `\tmovl\t%s, %%eax` |  | ✓ |  |  |
| 6916 | `\txorl\t%edx, %edx` |  | ✓ |  |  |
| 6918 | `\tcdq` |  |  | ✓ |  |
| 6922 | `\tmovl\t%s, %s` |  |  | ✓ |  |
| 6924 | `\t%s\t%s` |  |  | ✓ |  |
| 6927 | `\tmovl\t%%edx, %s` |  | ✓ |  |  |
| 6939 | `\tpushq\t%%rdx` |  | ✓ |  |  |
| 6947 | `\tmovq\t%s, %s` |  |  | ✓ |  |
| 6950 | `\tmovq\t%s, %%rax` |  | ✓ |  |  |
| 6952 | `\txorq\t%rdx, %rdx` |  | ✓ |  |  |
| 6954 | `\tcqo` |  |  | ✓ |  |
| 6957 | `\t%s\t%s` |  |  | ✓ |  |
| 6960 | `\tmovq\t%%rax, %s` |  | ✓ |  |  |
| 6970 | `\tmovl\t%s, %s` |  |  | ✓ |  |
| 6973 | `\tmovl\t%s, %%eax` |  | ✓ |  |  |
| 6975 | `\txorl\t%edx, %edx` |  | ✓ |  |  |
| 6977 | `\tcdq` |  |  | ✓ |  |
| 6980 | `\t%s\t%s` |  |  | ✓ |  |
| 6983 | `\tmovl\t%%eax, %s` |  | ✓ |  |  |
| 6987 | `\tpopq\t%%rdx` |  | ✓ |  |  |
| 6999 | `\tmovl\t%s, %%ecx` |  | ✓ |  |  |
| 7002 | `\tsal%c\t%%cl, %s` |  | ✓ |  |  |
| 7008 | `\tmovl\t%s, %%ecx` |  | ✓ |  |  |
| 7010 | `\tshr%c\t%%cl, %s` |  | ✓ |  |  |
| 7016 | `\tmovl\t%s, %%ecx` |  | ✓ |  |  |
| 7018 | `\trol%c\t%%cl, %s` |  | ✓ |  |  |
| 7024 | `\tmovl\t%s, %%ecx` |  | ✓ |  |  |
| 7026 | `\tror%c\t%%cl, %s` |  | ✓ |  |  |
| 7050 | `\ttestl\t%s, %s` |  |  | ✓ |  |
| 7052 | `\tsete\t%s` |  |  | ✓ |  |
| 7054 | `\tmovzbl\t%s, %s` |  |  | ✓ |  |
| 7086 | `\tbtl\t%s, (%s)` | ✓ |  |  |  |
| 7088 | `\tsetc\t%s` |  |  | ✓ |  |
| 7090 | `\tmovzbl\t%s, %s` |  |  | ✓ |  |
| 7110 | `\tmovl\t%s, %s` |  |  | ✓ |  |
| 7126 | `\tcmpl\t$0, %s` |  |  | ✓ |  |
| 7128 | `\tjl\t%s` |  |  | ✓ |  |
| 7130 | `\tcmpl\t$31, %s` |  |  | ✓ |  |
| 7132 | `\tjg\t%s` |  |  | ✓ |  |
| 7135 | `\tbtl\t%s, %s` |  |  | ✓ |  |
| 7137 | `\tsetc\t%s` |  |  | ✓ |  |
| 7139 | `\tjmp\t%s` |  |  | ✓ |  |
| 7142 | `%s:` |  |  | ✓ |  |
| 7144 | `\txorb\t%s, %s` |  |  | ✓ |  |
| 7146 | `%s:` |  |  | ✓ |  |
| 7149 | `\tmovzbl\t%s, %s` |  |  | ✓ |  |
| 7254 | `\tmovq\t%s, -%d(%%rbp)` |  | ✓ |  |  |
| 7263 | `\tmovq\t-%d(%%rbp), %s` |  | ✓ |  |  |
| 7274 | `\tmovq\t%s, -%d(%%rbp)` |  | ✓ |  |  |
| 7283 | `\tmovq\t-%d(%%rbp), %s` |  | ✓ |  |  |
| 7299 | `\tmovq\t$%lld, %s` |  |  | ✓ |  |
| 7302 | `\tmovq\t$%lld, %s` |  |  | ✓ |  |
| 7310 | `\tmovq\t$%lld, %s` |  |  | ✓ |  |
| 7319 | `\tmovq\t$%lld, %s` |  |  | ✓ |  |
| 7325 | `\tcall\t%s` |  |  | ✓ |  |
| 7327 | `\tnegl\t%eax` |  |  | ✓ |  |
| 7328 | `\tcmpl\t$0, %eax` |  | ✓ |  |  |
| 7347 | `\t%s\t%s` |  |  | ✓ |  |
| 7349 | `\tmovzbl\t%s, %s` |  |  | ✓ |  |
| 7409 | `\tcmpl\t$0, %eax` |  | ✓ |  |  |
| 7434 | `\t%s\t%s` |  |  | ✓ |  |
| 7436 | `\tmovzbl\t%s, %s` |  |  | ✓ |  |
| 7473 | `\tmovq\t%s, -%d(%%rbp)` |  | ✓ |  |  |
| 7475 | `\tmovsd\t-%d(%%rbp), %%xmm1` |  | ✓ |  |  |
| 7481 | `\tmovsd\t%s, %%xmm1` |  | ✓ |  |  |
| 7509 | `\tmovq\t%s, %s` | ✓ |  |  |  |
| 7511 | `\tcvtsi2sdq\t%s, %%xmm0` | ✓ |  |  |  |
| 7523 | `%s%s:\t.quad %s%s` |  |  | ✓ |  |
| 7526 | `\tmovsd\t%s(%%rip), %%xmm0` |  | ✓ |  |  |
| 7538 | `\tcvtsi2sdq\t%s, %%xmm0` |  | ✓ |  |  |
| 7543 | `\tcvtsi2sdl\t%s, %%xmm0` |  | ✓ |  |  |
| 7558 | `\tcvtsi2sdq\t%s, %%xmm0` |  | ✓ |  |  |
| 7563 | `\tcvtsi2sdl\t%s, %%xmm0` |  | ✓ |  |  |
| 7587 | `\tmovq\t%s, -%d(%%rbp)` |  | ✓ |  |  |
| 7589 | `\tmovsd\t-%d(%%rbp), %%xmm0` |  | ✓ |  |  |
| 7597 | `\tmovsd\t%s, %%xmm0` |  | ✓ |  |  |
| 7602 | `\txorl\t%s, %s` |  |  | ✓ |  |
| 7603 | `\tucomisd\t%xmm0, %xmm1` |  | ✓ |  |  |
| 7609 | `\tjp\t%s` |  |  | ✓ |  |
| 7611 | `\tje\t%s` |  |  | ✓ |  |
| 7615 | `\tjp\t%s` |  |  | ✓ |  |
| 7617 | `\tjne\t%s` |  |  | ✓ |  |
| 7621 | `\tjp\t%s` |  |  | ✓ |  |
| 7623 | `\tjb\t%s` |  |  | ✓ |  |
| 7627 | `\tjp\t%s` |  |  | ✓ |  |
| 7629 | `\tjbe\t%s` |  |  | ✓ |  |
| 7633 | `\tjp\t%s` |  |  | ✓ |  |
| 7635 | `\tja\t%s` |  |  | ✓ |  |
| 7639 | `\tjp\t%s` |  |  | ✓ |  |
| 7641 | `\tjae\t%s` |  |  | ✓ |  |
| 7648 | `\tjmp\t%s` |  |  | ✓ |  |
| 7650 | `%s:` |  |  | ✓ |  |
| 7652 | `\tmovl\t$1, %s` |  |  | ✓ |  |
| 7654 | `%s:` |  |  | ✓ |  |
| 7678 | `\tmovq\t8(%s), %s` | ✓ |  |  |  |
| 7685 | `\tmovq\t8(%s), %s` | ✓ |  |  |  |
| 7743 | `\tmovl\t%s, %s` | ✓ |  |  |  |
| 7759 | `\tmovl\t%s, %s` | ✓ |  |  |  |
| 7797 | `\tmovq\t$%lld, %s` | ✓ |  |  |  |
| 7803 | `\tcmp%c\t%s, %s` |  |  | ✓ |  |
| 7851 | `\t%s\t%s` |  |  | ✓ |  |
| 7853 | `\tmovzbl\t%s, %s` |  |  | ✓ |  |

---

## `register_set_spill_callback()` Call-Site Inventory

These are the 6 sites that bind a spill handler to a live `Register_t *`.
They must be preserved through the virtual-register transition in Phase 5.

| File | ~Line | Register arg | Callback | Context arg | Notes |
|------|-------|--------------|----------|-------------|-------|
| `codegen.c` | 3141 | `reloaded` | `codegen_static_link_spilled` | `ctx` | |
| `codegen.c` | 3206 | `reg` | `codegen_static_link_spilled` | `ctx` | |
| `codegen_expression.c` | 1845 | `reg` | `arginfo_register_spill_handler` | `info` | |
| `expr_tree.c` | 2402 | `target_reg` | `expr_tree_register_spill_handler` | `node` | |
| `expr_tree.c` | 2425 | `target_reg` | `expr_tree_register_spill_handler` | `node` | |
| `expr_tree.c` | 2483 | `target_reg` | `expr_tree_register_spill_handler` | `node` | |

---

*Generated by `grep -n add_inst` scan of the codegen sources. No functional code changes.*
