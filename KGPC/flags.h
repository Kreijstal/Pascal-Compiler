/*
    Damon Gwinn
    Flags for turning on different compiler functionalities
*/

#ifndef FLAGS_H
#define FLAGS_H

typedef enum {
    KGPC_TARGET_ABI_SYSTEM_V = 0,
    KGPC_TARGET_ABI_WINDOWS = 1,
} kgpc_target_abi_t;

void set_nonlocal_flag(void);
void set_o1_flag(void);
void set_o2_flag(void);
void set_parse_only_flag(void);
void set_time_passes_flag(void);
void set_target_windows_flag(void);
void set_target_sysv_flag(void);
void set_dump_ast_path(const char *path);
void set_asm_debug_flag(void);

int nonlocal_flag(void);
int optimize_flag(void);
int parse_only_flag(void);
int time_passes_flag(void);
int target_windows_flag(void);
kgpc_target_abi_t current_target_abi(void);
const char *dump_ast_path(void);
void clear_dump_ast_path(void);
int asm_debug_flag(void);

#endif
