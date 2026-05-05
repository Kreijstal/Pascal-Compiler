#!/usr/bin/env python3
"""
Migration script: replace snprintf+add_inst with add_inst_du in codegen_stmt_builtins.c
"""

import re

FILE = "/home/runner/work/Pascal-Compiler/Pascal-Compiler/KGPC/CodeGenerator/Intel_x86-64/codegen_statement_parts/codegen_stmt_builtins.c"

with open(FILE, 'r') as f:
    src = f.read()

original = src  # keep for verification

def replace_once(s, old, new):
    if old not in s:
        raise ValueError(f"Pattern not found:\n{repr(old[:100])}")
    count = s.count(old)
    if count > 1:
        raise ValueError(f"Pattern found {count} times (expected 1):\n{repr(old[:100])}")
    return s.replace(old, new, 1)

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 1: setlength descriptor setup (lines 189-271)
# ─────────────────────────────────────────────────────────────────────────────

# 1a. Add _setlength_desc_emitted flag after "char buffer[128];" at line 189
src = replace_once(src,
    "    char buffer[128];\n"
    "    if (use_expr_address)\n",
    "    char buffer[128];\n"
    "    int _setlength_desc_emitted = 0;\n"
    "    if (use_expr_address)\n"
)

# 1b. Branch 1 (use_expr_address): replace add_inst with add_inst_du, keep snprintf
src = replace_once(src,
    "        snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %s\\n\",\n"
    "            addr_reg->bit_64, descriptor_reg->bit_64);\n"
    "        inst_list = add_inst(inst_list, buffer);\n"
    "        free_reg(get_reg_stack(), addr_reg);\n"
    "    }\n"
    "    else if (is_nested_array)\n",
    "        snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %s\\n\",\n"
    "            addr_reg->bit_64, descriptor_reg->bit_64);\n"
    "        { Register_t *_d[] = {descriptor_reg}; Register_t *_u[] = {addr_reg}; inst_list = add_inst_du(inst_list, ctx, _d, 1, _u, 1, \"\\tmovq\\t%1, %0\\n\"); }\n"
    "        free_reg(get_reg_stack(), addr_reg);\n"
    "    }\n"
    "    else if (is_nested_array)\n"
)

# 1c. Branch 2 (is_nested_array): replace add_inst with add_inst_du, keep snprintf
src = replace_once(src,
    "        snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %s\\n\",\n"
    "            addr_reg->bit_64, descriptor_reg->bit_64);\n"
    "        inst_list = add_inst(inst_list, buffer);\n"
    "        free_reg(get_reg_stack(), addr_reg);\n"
    "    }\n"
    "    else if (is_field_array)\n",
    "        snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %s\\n\",\n"
    "            addr_reg->bit_64, descriptor_reg->bit_64);\n"
    "        { Register_t *_d[] = {descriptor_reg}; Register_t *_u[] = {addr_reg}; inst_list = add_inst_du(inst_list, ctx, _d, 1, _u, 1, \"\\tmovq\\t%1, %0\\n\"); }\n"
    "        free_reg(get_reg_stack(), addr_reg);\n"
    "    }\n"
    "    else if (is_field_array)\n"
)

# 1d. Branch 3 (is_field_array): replace add_inst with add_inst_du, keep snprintf
src = replace_once(src,
    "        snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %s\\n\",\n"
    "            field_addr_reg->bit_64, descriptor_reg->bit_64);\n"
    "        inst_list = add_inst(inst_list, buffer);\n"
    "        free_reg(get_reg_stack(), field_addr_reg);\n"
    "    }\n"
    "    else if (array_node->is_static)\n",
    "        snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %s\\n\",\n"
    "            field_addr_reg->bit_64, descriptor_reg->bit_64);\n"
    "        { Register_t *_d[] = {descriptor_reg}; Register_t *_u[] = {field_addr_reg}; inst_list = add_inst_du(inst_list, ctx, _d, 1, _u, 1, \"\\tmovq\\t%1, %0\\n\"); }\n"
    "        free_reg(get_reg_stack(), field_addr_reg);\n"
    "    }\n"
    "    else if (array_node->is_static)\n"
)

# 1e. Branch 4 (is_static): add add_inst_du + set flag
src = replace_once(src,
    "        snprintf(buffer, sizeof(buffer), \"\\tleaq\\t%s(%%rip), %s\\n\",\n"
    "            label, descriptor_reg->bit_64);\n"
    "    }\n"
    "    else if (setlength_scope_depth > 0)\n",
    "        snprintf(buffer, sizeof(buffer), \"\\tleaq\\t%s(%%rip), %s\\n\",\n"
    "            label, descriptor_reg->bit_64);\n"
    "        { char _tmpl[128]; snprintf(_tmpl, sizeof(_tmpl), \"\\tleaq\\t%s(%%%%rip), %%0\\n\", label); Register_t *_d[] = {descriptor_reg}; inst_list = add_inst_du(inst_list, ctx, _d, 1, NULL, 0, _tmpl); }\n"
    "        _setlength_desc_emitted = 1;\n"
    "    }\n"
    "    else if (setlength_scope_depth > 0)\n"
)

# 1f. Branch 5 (scope_depth): add add_inst_du for both is_reference and else, set flag
src = replace_once(src,
    "        if (array_node->is_reference)\n"
    "        {\n"
    "            snprintf(buffer, sizeof(buffer), \"\\tmovq\\t-%d(%s), %s\\n\",\n"
    "                array_node->offset, frame_reg->bit_64, descriptor_reg->bit_64);\n"
    "        }\n"
    "        else\n"
    "        {\n"
    "            snprintf(buffer, sizeof(buffer), \"\\tleaq\\t-%d(%s), %s\\n\",\n"
    "                array_node->offset, frame_reg->bit_64, descriptor_reg->bit_64);\n"
    "        }\n"
    "    }\n"
    "    else\n"
    "    {\n"
    "        if (array_node->is_reference)\n"
    "        {\n"
    "            snprintf(buffer, sizeof(buffer), \"\\tmovq\\t-%d(%%rbp), %s\\n\",\n",
    "        if (array_node->is_reference)\n"
    "        {\n"
    "            snprintf(buffer, sizeof(buffer), \"\\tmovq\\t-%d(%s), %s\\n\",\n"
    "                array_node->offset, frame_reg->bit_64, descriptor_reg->bit_64);\n"
    "            { char _tmpl[128]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t-%%d(%%1), %%0\\n\", array_node->offset); Register_t *_d[] = {descriptor_reg}; Register_t *_u[] = {frame_reg}; inst_list = add_inst_du(inst_list, ctx, _d, 1, _u, 1, _tmpl); }\n"
    "        }\n"
    "        else\n"
    "        {\n"
    "            snprintf(buffer, sizeof(buffer), \"\\tleaq\\t-%d(%s), %s\\n\",\n"
    "                array_node->offset, frame_reg->bit_64, descriptor_reg->bit_64);\n"
    "            { char _tmpl[128]; snprintf(_tmpl, sizeof(_tmpl), \"\\tleaq\\t-%%d(%%1), %%0\\n\", array_node->offset); Register_t *_d[] = {descriptor_reg}; Register_t *_u[] = {frame_reg}; inst_list = add_inst_du(inst_list, ctx, _d, 1, _u, 1, _tmpl); }\n"
    "        }\n"
    "        _setlength_desc_emitted = 1;\n"
    "    }\n"
    "    else\n"
    "    {\n"
    "        if (array_node->is_reference)\n"
    "        {\n"
    "            snprintf(buffer, sizeof(buffer), \"\\tmovq\\t-%d(%%rbp), %s\\n\",\n"
)

# 1g. Branch 6 (else): add add_inst_du for both sub-branches, set flag
src = replace_once(src,
    "        if (array_node->is_reference)\n"
    "        {\n"
    "            snprintf(buffer, sizeof(buffer), \"\\tmovq\\t-%d(%%rbp), %s\\n\",\n"
    "                array_node->offset, descriptor_reg->bit_64);\n"
    "        }\n"
    "        else\n"
    "        {\n"
    "            snprintf(buffer, sizeof(buffer), \"\\tleaq\\t-%d(%%rbp), %s\\n\",\n"
    "                array_node->offset, descriptor_reg->bit_64);\n"
    "        }\n"
    "    }\n"
    "    inst_list = add_inst(inst_list, buffer);\n",
    "        if (array_node->is_reference)\n"
    "        {\n"
    "            snprintf(buffer, sizeof(buffer), \"\\tmovq\\t-%d(%%rbp), %s\\n\",\n"
    "                array_node->offset, descriptor_reg->bit_64);\n"
    "            { char _tmpl[128]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t-%%d(%%%%rbp), %%0\\n\", array_node->offset); Register_t *_d[] = {descriptor_reg}; inst_list = add_inst_du(inst_list, ctx, _d, 1, NULL, 0, _tmpl); }\n"
    "        }\n"
    "        else\n"
    "        {\n"
    "            snprintf(buffer, sizeof(buffer), \"\\tleaq\\t-%d(%%rbp), %s\\n\",\n"
    "                array_node->offset, descriptor_reg->bit_64);\n"
    "            { char _tmpl[128]; snprintf(_tmpl, sizeof(_tmpl), \"\\tleaq\\t-%%d(%%%%rbp), %%0\\n\", array_node->offset); Register_t *_d[] = {descriptor_reg}; inst_list = add_inst_du(inst_list, ctx, _d, 1, NULL, 0, _tmpl); }\n"
    "        }\n"
    "        _setlength_desc_emitted = 1;\n"
    "    }\n"
    "    if (!_setlength_desc_emitted) inst_list = add_inst(inst_list, buffer);\n"
)

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 2: Windows swap case (around line 294-299)
# ─────────────────────────────────────────────────────────────────────────────

# Line 294-295: movq descriptor_reg, -(off)(%rbp)
src = replace_once(src,
    "                snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, -%d(%%rbp)\\n\", descriptor_reg->bit_64, temp->offset);\n"
    "                inst_list = add_inst(inst_list, buffer);\n"
    "                snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %s\\n\", length_reg->bit_64, arg1);\n"
    "                inst_list = add_inst(inst_list, buffer);\n"
    "                snprintf(buffer, sizeof(buffer), \"\\tmovq\\t-%d(%%rbp), %s\\n\", temp->offset, arg0);\n"
    "                inst_list = add_inst(inst_list, buffer);\n",
    "                { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t%%0, -%%d(%%%%rbp)\\n\", temp->offset); Register_t *_u[] = {descriptor_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "                { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t%%0, %s\\n\", arg1); Register_t *_u[] = {length_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "                snprintf(buffer, sizeof(buffer), \"\\tmovq\\t-%d(%%rbp), %s\\n\", temp->offset, arg0);\n"
    "                inst_list = add_inst(inst_list, buffer);\n"
)

# Lines 306-317: descriptor_is_arg1 branches
src = replace_once(src,
    "            snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %s\\n\", descriptor_reg->bit_64, arg0);\n"
    "            inst_list = add_inst(inst_list, buffer);\n"
    "            snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %s\\n\", length_reg->bit_64, arg1);\n"
    "            inst_list = add_inst(inst_list, buffer);\n"
    "        }\n"
    "        else\n"
    "        {\n"
    "            /* Normal case or length in %rdx */\n"
    "            snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %s\\n\", length_reg->bit_64, arg1);\n"
    "            inst_list = add_inst(inst_list, buffer);\n"
    "            snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %s\\n\", descriptor_reg->bit_64, arg0);\n"
    "            inst_list = add_inst(inst_list, buffer);\n"
    "        }\n",
    "            { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t%%0, %s\\n\", arg0); Register_t *_u[] = {descriptor_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "            { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t%%0, %s\\n\", arg1); Register_t *_u[] = {length_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "        }\n"
    "        else\n"
    "        {\n"
    "            /* Normal case or length in %rdx */\n"
    "            { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t%%0, %s\\n\", arg1); Register_t *_u[] = {length_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "            { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t%%0, %s\\n\", arg0); Register_t *_u[] = {descriptor_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "        }\n"
)

# Lines 328-331: Linux case (arg0=rdi, arg1=rsi)
src = replace_once(src,
    "        snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %s\\n\", descriptor_reg->bit_64, arg0);\n"
    "        inst_list = add_inst(inst_list, buffer);\n"
    "        snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %s\\n\", length_reg->bit_64, arg1);\n"
    "        inst_list = add_inst(inst_list, buffer);\n"
    "        snprintf(buffer, sizeof(buffer), \"\\tmovl\\t$%d, %s\\n\", element_size, current_arg_reg32(2));\n"
    "        inst_list = add_inst(inst_list, buffer);\n"
    "    }\n"
    "\n"
    "    inst_list = codegen_vect_reg(inst_list, 0);\n"
    "    inst_list = codegen_call_with_shadow_space(inst_list, \"kgpc_dynarray_setlength\");\n",
    "        { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t%%0, %s\\n\", arg0); Register_t *_u[] = {descriptor_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "        { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t%%0, %s\\n\", arg1); Register_t *_u[] = {length_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "        snprintf(buffer, sizeof(buffer), \"\\tmovl\\t$%d, %s\\n\", element_size, current_arg_reg32(2));\n"
    "        inst_list = add_inst(inst_list, buffer);\n"
    "    }\n"
    "\n"
    "    inst_list = codegen_vect_reg(inst_list, 0);\n"
    "    inst_list = codegen_call_with_shadow_space(inst_list, \"kgpc_dynarray_setlength\");\n"
)

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 3: setlength_string (lines 388-390, 407-409, 428-447)
# ─────────────────────────────────────────────────────────────────────────────

# Lines 388-390: spill addr_reg to stack
src = replace_once(src,
    "            snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, -%d(%%rbp)\\n\",\n"
    "                addr_reg->bit_64, addr_spill_slot->offset);\n"
    "            inst_list = add_inst(inst_list, buffer);\n"
    "        }\n"
    "    }\n"
    "\n"
    "    Register_t *length_reg = NULL;\n"
    "    inst_list = codegen_expr_with_result(len_expr, inst_list, ctx, &length_reg);\n"
    "    if (codegen_had_error(ctx) || length_reg == NULL)\n"
    "    {\n"
    "        if (length_reg != NULL)\n"
    "            free_reg(get_reg_stack(), length_reg);\n"
    "        free_reg(get_reg_stack(), addr_reg);\n"
    "        return inst_list;\n"
    "    }\n"
    "\n"
    "    /* Reload addr_reg from spill slot if we spilled it */\n"
    "    if (addr_spill_slot != NULL)\n"
    "    {\n"
    "        snprintf(buffer, sizeof(buffer), \"\\tmovq\\t-%d(%%rbp), %s\\n\",\n"
    "            addr_spill_slot->offset, addr_reg->bit_64);\n"
    "        inst_list = add_inst(inst_list, buffer);\n"
    "    }\n"
    "\n"
    "    if (!expr_uses_qword_kgpctype(len_expr))\n"
    "        inst_list = codegen_sign_extend32_to64(inst_list, length_reg->bit_32, length_reg->bit_64);\n"
    "\n"
    "    const char *arg0 = current_arg_reg64(0);  /* First argument: %rcx (Win) / %rdi (SysV) */\n"
    "    const char *arg1 = current_arg_reg64(1);  /* Second argument: %rdx (Win) / %rsi (SysV) */\n"
    "    RegisterId_t arg0_id = codegen_arg_reg_id_num(0);\n"
    "    RegisterId_t arg1_id = codegen_arg_reg_id_num(1);\n"
    "    \n"
    "    /*\n"
    "     * Handle register conflicts when setting up function arguments.\n"
    "     * If length_reg is in arg0's position and we try to move addr_reg to arg0,\n"
    "     * we'll overwrite the length. In this case, move length_reg to arg1 first.\n"
    "     */\n"
    "    if (length_reg->reg_id == arg0_id)\n"
    "    {\n"
    "        /* length_reg is in arg0, which will be overwritten by addr_reg. Move length first. */\n"
    "        snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %s\\n\", length_reg->bit_64, arg1);\n"
    "        inst_list = add_inst(inst_list, buffer);\n"
    "        snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %s\\n\", addr_reg->bit_64, arg0);\n"
    "        inst_list = add_inst(inst_list, buffer);\n"
    "    }\n"
    "    else if (addr_reg->reg_id == arg1_id)\n"
    "    {\n"
    "        /* addr_reg is in arg1, which is the destination for length_reg. Move addr first. */\n"
    "        snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %s\\n\", addr_reg->bit_64, arg0);\n"
    "        inst_list = add_inst(inst_list, buffer);\n"
    "        snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %s\\n\", length_reg->bit_64, arg1);\n"
    "        inst_list = add_inst(inst_list, buffer);\n"
    "    }\n"
    "    else\n"
    "    {\n"
    "        /* No conflict, move in standard order */\n"
    "        snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %s\\n\", addr_reg->bit_64, arg0);\n"
    "        inst_list = add_inst(inst_list, buffer);\n"
    "        snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %s\\n\", length_reg->bit_64, arg1);\n"
    "        inst_list = add_inst(inst_list, buffer);\n"
    "    }\n"
    "\n"
    "    inst_list = codegen_vect_reg(inst_list, 0);\n"
    "    inst_list = codegen_call_with_shadow_space(inst_list, \"kgpc_string_setlength\");\n",
    "            { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t%%0, -%%d(%%%%rbp)\\n\", addr_spill_slot->offset); Register_t *_u[] = {addr_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "        }\n"
    "    }\n"
    "\n"
    "    Register_t *length_reg = NULL;\n"
    "    inst_list = codegen_expr_with_result(len_expr, inst_list, ctx, &length_reg);\n"
    "    if (codegen_had_error(ctx) || length_reg == NULL)\n"
    "    {\n"
    "        if (length_reg != NULL)\n"
    "            free_reg(get_reg_stack(), length_reg);\n"
    "        free_reg(get_reg_stack(), addr_reg);\n"
    "        return inst_list;\n"
    "    }\n"
    "\n"
    "    /* Reload addr_reg from spill slot if we spilled it */\n"
    "    if (addr_spill_slot != NULL)\n"
    "    {\n"
    "        { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t-%%d(%%%%rbp), %%0\\n\", addr_spill_slot->offset); Register_t *_d[] = {addr_reg}; inst_list = add_inst_du(inst_list, ctx, _d, 1, NULL, 0, _tmpl); }\n"
    "    }\n"
    "\n"
    "    if (!expr_uses_qword_kgpctype(len_expr))\n"
    "        inst_list = codegen_sign_extend32_to64(inst_list, length_reg->bit_32, length_reg->bit_64);\n"
    "\n"
    "    const char *arg0 = current_arg_reg64(0);  /* First argument: %rcx (Win) / %rdi (SysV) */\n"
    "    const char *arg1 = current_arg_reg64(1);  /* Second argument: %rdx (Win) / %rsi (SysV) */\n"
    "    RegisterId_t arg0_id = codegen_arg_reg_id_num(0);\n"
    "    RegisterId_t arg1_id = codegen_arg_reg_id_num(1);\n"
    "    \n"
    "    /*\n"
    "     * Handle register conflicts when setting up function arguments.\n"
    "     * If length_reg is in arg0's position and we try to move addr_reg to arg0,\n"
    "     * we'll overwrite the length. In this case, move length_reg to arg1 first.\n"
    "     */\n"
    "    if (length_reg->reg_id == arg0_id)\n"
    "    {\n"
    "        /* length_reg is in arg0, which will be overwritten by addr_reg. Move length first. */\n"
    "        { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t%%0, %s\\n\", arg1); Register_t *_u[] = {length_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "        { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t%%0, %s\\n\", arg0); Register_t *_u[] = {addr_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "    }\n"
    "    else if (addr_reg->reg_id == arg1_id)\n"
    "    {\n"
    "        /* addr_reg is in arg1, which is the destination for length_reg. Move addr first. */\n"
    "        { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t%%0, %s\\n\", arg0); Register_t *_u[] = {addr_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "        { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t%%0, %s\\n\", arg1); Register_t *_u[] = {length_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "    }\n"
    "    else\n"
    "    {\n"
    "        /* No conflict, move in standard order */\n"
    "        { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t%%0, %s\\n\", arg0); Register_t *_u[] = {addr_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "        { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t%%0, %s\\n\", arg1); Register_t *_u[] = {length_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "    }\n"
    "\n"
    "    inst_list = codegen_vect_reg(inst_list, 0);\n"
    "    inst_list = codegen_call_with_shadow_space(inst_list, \"kgpc_string_setlength\");\n"
)

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 4: setlength_unicodestring (lines 498-548)
# ─────────────────────────────────────────────────────────────────────────────

src = replace_once(src,
    "            snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, -%d(%%rbp)\\n\",\n"
    "                addr_reg->bit_64, addr_spill_slot->offset);\n"
    "            inst_list = add_inst(inst_list, buffer);\n"
    "        }\n"
    "    }\n"
    "\n"
    "    Register_t *length_reg = NULL;\n"
    "    inst_list = codegen_expr_with_result(len_expr, inst_list, ctx, &length_reg);\n"
    "    if (codegen_had_error(ctx) || length_reg == NULL)\n"
    "    {\n"
    "        if (length_reg != NULL)\n"
    "            free_reg(get_reg_stack(), length_reg);\n"
    "        free_reg(get_reg_stack(), addr_reg);\n"
    "        return inst_list;\n"
    "    }\n"
    "\n"
    "    if (addr_spill_slot != NULL)\n"
    "    {\n"
    "        snprintf(buffer, sizeof(buffer), \"\\tmovq\\t-%d(%%rbp), %s\\n\",\n"
    "            addr_spill_slot->offset, addr_reg->bit_64);\n"
    "        inst_list = add_inst(inst_list, buffer);\n"
    "    }\n"
    "\n"
    "    if (!expr_uses_qword_kgpctype(len_expr))\n"
    "        inst_list = codegen_sign_extend32_to64(inst_list, length_reg->bit_32, length_reg->bit_64);\n"
    "\n"
    "    const char *arg0 = current_arg_reg64(0);\n"
    "    const char *arg1 = current_arg_reg64(1);\n"
    "    RegisterId_t arg0_id = codegen_arg_reg_id_num(0);\n"
    "    RegisterId_t arg1_id = codegen_arg_reg_id_num(1);\n"
    "\n"
    "    if (length_reg->reg_id == arg0_id)\n"
    "    {\n"
    "        snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %s\\n\", length_reg->bit_64, arg1);\n"
    "        inst_list = add_inst(inst_list, buffer);\n"
    "        snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %s\\n\", addr_reg->bit_64, arg0);\n"
    "        inst_list = add_inst(inst_list, buffer);\n"
    "    }\n"
    "    else if (addr_reg->reg_id == arg1_id)\n"
    "    {\n"
    "        snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %s\\n\", addr_reg->bit_64, arg0);\n"
    "        inst_list = add_inst(inst_list, buffer);\n"
    "        snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %s\\n\", length_reg->bit_64, arg1);\n"
    "        inst_list = add_inst(inst_list, buffer);\n"
    "    }\n"
    "    else\n"
    "    {\n"
    "        snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %s\\n\", addr_reg->bit_64, arg0);\n"
    "        inst_list = add_inst(inst_list, buffer);\n"
    "        snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %s\\n\", length_reg->bit_64, arg1);\n"
    "        inst_list = add_inst(inst_list, buffer);\n"
    "    }\n"
    "\n"
    "    inst_list = codegen_vect_reg(inst_list, 0);\n"
    "    inst_list = codegen_call_with_shadow_space(inst_list, \"kgpc_unicodestring_setlength\");\n",
    "            { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t%%0, -%%d(%%%%rbp)\\n\", addr_spill_slot->offset); Register_t *_u[] = {addr_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "        }\n"
    "    }\n"
    "\n"
    "    Register_t *length_reg = NULL;\n"
    "    inst_list = codegen_expr_with_result(len_expr, inst_list, ctx, &length_reg);\n"
    "    if (codegen_had_error(ctx) || length_reg == NULL)\n"
    "    {\n"
    "        if (length_reg != NULL)\n"
    "            free_reg(get_reg_stack(), length_reg);\n"
    "        free_reg(get_reg_stack(), addr_reg);\n"
    "        return inst_list;\n"
    "    }\n"
    "\n"
    "    if (addr_spill_slot != NULL)\n"
    "    {\n"
    "        { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t-%%d(%%%%rbp), %%0\\n\", addr_spill_slot->offset); Register_t *_d[] = {addr_reg}; inst_list = add_inst_du(inst_list, ctx, _d, 1, NULL, 0, _tmpl); }\n"
    "    }\n"
    "\n"
    "    if (!expr_uses_qword_kgpctype(len_expr))\n"
    "        inst_list = codegen_sign_extend32_to64(inst_list, length_reg->bit_32, length_reg->bit_64);\n"
    "\n"
    "    const char *arg0 = current_arg_reg64(0);\n"
    "    const char *arg1 = current_arg_reg64(1);\n"
    "    RegisterId_t arg0_id = codegen_arg_reg_id_num(0);\n"
    "    RegisterId_t arg1_id = codegen_arg_reg_id_num(1);\n"
    "\n"
    "    if (length_reg->reg_id == arg0_id)\n"
    "    {\n"
    "        { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t%%0, %s\\n\", arg1); Register_t *_u[] = {length_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "        { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t%%0, %s\\n\", arg0); Register_t *_u[] = {addr_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "    }\n"
    "    else if (addr_reg->reg_id == arg1_id)\n"
    "    {\n"
    "        { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t%%0, %s\\n\", arg0); Register_t *_u[] = {addr_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "        { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t%%0, %s\\n\", arg1); Register_t *_u[] = {length_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "    }\n"
    "    else\n"
    "    {\n"
    "        { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t%%0, %s\\n\", arg0); Register_t *_u[] = {addr_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "        { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t%%0, %s\\n\", arg1); Register_t *_u[] = {length_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "    }\n"
    "\n"
    "    inst_list = codegen_vect_reg(inst_list, 0);\n"
    "    inst_list = codegen_call_with_shadow_space(inst_list, \"kgpc_unicodestring_setlength\");\n"
)

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 5: setlength_shortstring (lines 610-627)
# ─────────────────────────────────────────────────────────────────────────────

src = replace_once(src,
    "    if (length_reg->reg_id == arg0_id)\n"
    "    {\n"
    "        snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %s\\n\", length_reg->bit_64, arg1);\n"
    "        inst_list = add_inst(inst_list, buffer);\n"
    "        snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %s\\n\", addr_reg->bit_64, arg0);\n"
    "        inst_list = add_inst(inst_list, buffer);\n"
    "    }\n"
    "    else if (addr_reg->reg_id == arg1_id)\n"
    "    {\n"
    "        snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %s\\n\", addr_reg->bit_64, arg0);\n"
    "        inst_list = add_inst(inst_list, buffer);\n"
    "        snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %s\\n\", length_reg->bit_64, arg1);\n"
    "        inst_list = add_inst(inst_list, buffer);\n"
    "    }\n"
    "    else\n"
    "    {\n"
    "        snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %s\\n\", addr_reg->bit_64, arg0);\n"
    "        inst_list = add_inst(inst_list, buffer);\n"
    "        snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %s\\n\", length_reg->bit_64, arg1);\n"
    "        inst_list = add_inst(inst_list, buffer);\n"
    "    }\n"
    "\n"
    "    inst_list = codegen_vect_reg(inst_list, 0);\n"
    "    inst_list = codegen_call_with_shadow_space(inst_list, \"kgpc_shortstring_setlength\");\n",
    "    if (length_reg->reg_id == arg0_id)\n"
    "    {\n"
    "        { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t%%0, %s\\n\", arg1); Register_t *_u[] = {length_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "        { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t%%0, %s\\n\", arg0); Register_t *_u[] = {addr_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "    }\n"
    "    else if (addr_reg->reg_id == arg1_id)\n"
    "    {\n"
    "        { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t%%0, %s\\n\", arg0); Register_t *_u[] = {addr_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "        { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t%%0, %s\\n\", arg1); Register_t *_u[] = {length_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "    }\n"
    "    else\n"
    "    {\n"
    "        { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t%%0, %s\\n\", arg0); Register_t *_u[] = {addr_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "        { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t%%0, %s\\n\", arg1); Register_t *_u[] = {length_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "    }\n"
    "\n"
    "    inst_list = codegen_vect_reg(inst_list, 0);\n"
    "    inst_list = codegen_call_with_shadow_space(inst_list, \"kgpc_shortstring_setlength\");\n"
)

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 6: setstring - spill addr_reg and buffer_reg (lines 679, 709)
# ─────────────────────────────────────────────────────────────────────────────

# Line 679-680: movq addr_reg, -(off)(%rbp)
src = replace_once(src,
    "    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, -%d(%%rbp)\\n\", addr_reg->bit_64, addr_slot->offset);\n"
    "    inst_list = add_inst(inst_list, buffer);\n"
    "    free_reg(get_reg_stack(), addr_reg);\n"
    "    addr_reg = NULL;\n",
    "    { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t%%0, -%%d(%%%%rbp)\\n\", addr_slot->offset); Register_t *_u[] = {addr_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "    free_reg(get_reg_stack(), addr_reg);\n"
    "    addr_reg = NULL;\n"
)

# Line 709-710: movq buffer_reg, -(off)(%rbp)
src = replace_once(src,
    "    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, -%d(%%rbp)\\n\", buffer_reg->bit_64, buffer_slot->offset);\n"
    "    inst_list = add_inst(inst_list, buffer);\n"
    "    free_reg(get_reg_stack(), buffer_reg);\n"
    "    buffer_reg = NULL;\n",
    "    { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t%%0, -%%d(%%%%rbp)\\n\", buffer_slot->offset); Register_t *_u[] = {buffer_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "    free_reg(get_reg_stack(), buffer_reg);\n"
    "    buffer_reg = NULL;\n"
)

# Line 794-795: movq length_reg, arg2
src = replace_once(src,
    "    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %s\\n\", length_reg->bit_64, arg2);\n"
    "    inst_list = add_inst(inst_list, buffer);\n"
    "    free_reg(get_reg_stack(), length_reg);\n",
    "    { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t%%0, %s\\n\", arg2); Register_t *_u[] = {length_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "    free_reg(get_reg_stack(), length_reg);\n"
)

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 7: codegen_builtin_str - various movq sites
# ─────────────────────────────────────────────────────────────────────────────

# Lines 922-924: movq value_addr_reg, %rcx/%rdi
src = replace_once(src,
    "            if (codegen_target_is_windows())\n"
    "                snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%rcx\\n\", value_addr_reg->bit_64);\n"
    "            else\n"
    "                snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%rdi\\n\", value_addr_reg->bit_64);\n"
    "            inst_list = add_inst(inst_list, buffer);\n",
    "            { Register_t *_u[] = {value_addr_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, codegen_target_is_windows() ? \"\\tmovq\\t%0, %rcx\\n\" : \"\\tmovq\\t%0, %rdi\\n\"); }\n"
)

# Lines 934-935: movq value_reg, %xmm0
src = replace_once(src,
    "            snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%xmm0\\n\", value_reg->bit_64);\n"
    "            inst_list = add_inst(inst_list, buffer);\n",
    "            { Register_t *_u[] = {value_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, \"\\tmovq\\t%0, %xmm0\\n\"); }\n"
)

# Lines 950-951: movq $0, width_reg
src = replace_once(src,
    "                snprintf(buffer, sizeof(buffer), \"\\tmovq\\t$0, %s\\n\", width_reg->bit_64);\n"
    "                inst_list = add_inst(inst_list, buffer);\n",
    "                { Register_t *_d[] = {width_reg}; inst_list = add_inst_du(inst_list, ctx, _d, 1, NULL, 0, \"\\tmovq\\t$0, %0\\n\"); }\n"
)

# Lines 963-964: movq $-1, precision_reg
src = replace_once(src,
    "                snprintf(buffer, sizeof(buffer), \"\\tmovq\\t$-1, %s\\n\", precision_reg->bit_64);\n"
    "                inst_list = add_inst(inst_list, buffer);\n",
    "                { Register_t *_d[] = {precision_reg}; inst_list = add_inst_du(inst_list, ctx, _d, 1, NULL, 0, \"\\tmovq\\t$-1, %0\\n\"); }\n"
)

# Lines 970-971: movq width_reg, %rdx (Windows shortstring has_width case)
src = replace_once(src,
    "                    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%rdx\\n\", width_reg->bit_64);\n"
    "                    inst_list = add_inst(inst_list, buffer);\n"
    "                    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%r8\\n\", addr_reg->bit_64);\n"
    "                    inst_list = add_inst(inst_list, buffer);\n"
    "                    snprintf(buffer, sizeof(buffer),\n"
    "                        \"\\tmovq\\t$%d, %%r9\\n\\tshlq\\t$32, %%r9\\n\\torq\\t%s, %%r9\\n\",\n"
    "                        target_shortstring_capacity, precision_reg->bit_64);\n"
    "                    inst_list = add_inst(inst_list, buffer);\n",
    "                    { Register_t *_u[] = {width_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, \"\\tmovq\\t%0, %rdx\\n\"); }\n"
    "                    { Register_t *_u[] = {addr_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, \"\\tmovq\\t%0, %r8\\n\"); }\n"
    "                    snprintf(buffer, sizeof(buffer),\n"
    "                        \"\\tmovq\\t$%d, %%r9\\n\\tshlq\\t$32, %%r9\\n\\torq\\t%s, %%r9\\n\",\n"
    "                        target_shortstring_capacity, precision_reg->bit_64);\n"
    "                    inst_list = add_inst(inst_list, buffer);\n"
)

# Lines 982-987: Windows non-shortstring has_width: movq addr_reg, %r9; movq width_reg, %rdx; movq precision_reg, %r8
src = replace_once(src,
    "                    /* Move addr_reg to R9 first to avoid clobbering if addr_reg is R8 */\n"
    "                    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%r9\\n\", addr_reg->bit_64);\n"
    "                    inst_list = add_inst(inst_list, buffer);\n"
    "                    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%rdx\\n\", width_reg->bit_64);\n"
    "                    inst_list = add_inst(inst_list, buffer);\n"
    "                    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%r8\\n\", precision_reg->bit_64);\n"
    "                    inst_list = add_inst(inst_list, buffer);\n",
    "                    /* Move addr_reg to R9 first to avoid clobbering if addr_reg is R8 */\n"
    "                    { Register_t *_u[] = {addr_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, \"\\tmovq\\t%0, %r9\\n\"); }\n"
    "                    { Register_t *_u[] = {width_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, \"\\tmovq\\t%0, %rdx\\n\"); }\n"
    "                    { Register_t *_u[] = {precision_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, \"\\tmovq\\t%0, %r8\\n\"); }\n"
)

# Lines 994-1001: Linux shortstring has_width: movq width_reg, %rdi; movq addr_reg, %rsi; complex %rdx
src = replace_once(src,
    "                    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%rdi\\n\", width_reg->bit_64);\n"
    "                    inst_list = add_inst(inst_list, buffer);\n"
    "                    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%rsi\\n\", addr_reg->bit_64);\n"
    "                    inst_list = add_inst(inst_list, buffer);\n"
    "                    snprintf(buffer, sizeof(buffer),\n"
    "                        \"\\tmovq\\t$%d, %%rdx\\n\\tshlq\\t$32, %%rdx\\n\\torq\\t%s, %%rdx\\n\",\n"
    "                        target_shortstring_capacity, precision_reg->bit_64);\n"
    "                    inst_list = add_inst(inst_list, buffer);\n",
    "                    { Register_t *_u[] = {width_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, \"\\tmovq\\t%0, %rdi\\n\"); }\n"
    "                    { Register_t *_u[] = {addr_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, \"\\tmovq\\t%0, %rsi\\n\"); }\n"
    "                    snprintf(buffer, sizeof(buffer),\n"
    "                        \"\\tmovq\\t$%d, %%rdx\\n\\tshlq\\t$32, %%rdx\\n\\torq\\t%s, %%rdx\\n\",\n"
    "                        target_shortstring_capacity, precision_reg->bit_64);\n"
    "                    inst_list = add_inst(inst_list, buffer);\n"
)

# Lines 1006-1011: Linux non-shortstring has_width: movq addr_reg, %rdx; movq width_reg, %rdi; movq precision_reg, %rsi
src = replace_once(src,
    "                    /* Move addr_reg to RDX first to avoid clobbering if addr_reg is RSI */\n"
    "                    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%rdx\\n\", addr_reg->bit_64);\n"
    "                    inst_list = add_inst(inst_list, buffer);\n"
    "                    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%rdi\\n\", width_reg->bit_64);\n"
    "                    inst_list = add_inst(inst_list, buffer);\n"
    "                    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%rsi\\n\", precision_reg->bit_64);\n"
    "                    inst_list = add_inst(inst_list, buffer);\n",
    "                    /* Move addr_reg to RDX first to avoid clobbering if addr_reg is RSI */\n"
    "                    { Register_t *_u[] = {addr_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, \"\\tmovq\\t%0, %rdx\\n\"); }\n"
    "                    { Register_t *_u[] = {width_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, \"\\tmovq\\t%0, %rdi\\n\"); }\n"
    "                    { Register_t *_u[] = {precision_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, \"\\tmovq\\t%0, %rsi\\n\"); }\n"
)

# Lines 1027-1028: Windows shortstring no width: movq addr_reg, %rdx
src = replace_once(src,
    "                    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%rdx\\n\", addr_reg->bit_64);\n"
    "                    inst_list = add_inst(inst_list, buffer);\n"
    "                    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t$%d, %%r8\\n\",\n"
    "                        target_shortstring_capacity);\n",
    "                    { Register_t *_u[] = {addr_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, \"\\tmovq\\t%0, %rdx\\n\"); }\n"
    "                    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t$%d, %%r8\\n\",\n"
    "                        target_shortstring_capacity);\n"
)

# Lines 1035-1036: Windows non-shortstring no width: movq addr_reg, %rdx
src = replace_once(src,
    "                    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%rdx\\n\", addr_reg->bit_64);\n"
    "                    inst_list = add_inst(inst_list, buffer);\n"
    "                }\n"
    "            }\n"
    "            else\n"
    "            {\n"
    "                if (target_is_shortstring)\n"
    "                {\n"
    "                    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%rdi\\n\", addr_reg->bit_64);\n",
    "                    { Register_t *_u[] = {addr_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, \"\\tmovq\\t%0, %rdx\\n\"); }\n"
    "                }\n"
    "            }\n"
    "            else\n"
    "            {\n"
    "                if (target_is_shortstring)\n"
    "                {\n"
    "                    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%rdi\\n\", addr_reg->bit_64);\n"
)

# Lines 1043-1044: Linux shortstring no width: movq addr_reg, %rdi
src = replace_once(src,
    "                    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%rdi\\n\", addr_reg->bit_64);\n"
    "                    inst_list = add_inst(inst_list, buffer);\n"
    "                    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t$%d, %%rsi\\n\",\n"
    "                        target_shortstring_capacity);\n",
    "                    { Register_t *_u[] = {addr_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, \"\\tmovq\\t%0, %rdi\\n\"); }\n"
    "                    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t$%d, %%rsi\\n\",\n"
    "                        target_shortstring_capacity);\n"
)

# Lines 1051-1052: Linux non-shortstring no width: movq addr_reg, %rdi
src = replace_once(src,
    "                    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%rdi\\n\", addr_reg->bit_64);\n"
    "                    inst_list = add_inst(inst_list, buffer);\n"
    "                }\n"
    "            }\n"
    "            inst_list = codegen_vect_reg(inst_list, 0);\n"
    "            if (target_is_shortstring)\n"
    "                snprintf(buffer, sizeof(buffer), \"\\tcall\\tkgpc_str_real_bounded_shortstring\\n\");\n",
    "                    { Register_t *_u[] = {addr_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, \"\\tmovq\\t%0, %rdi\\n\"); }\n"
    "                }\n"
    "            }\n"
    "            inst_list = codegen_vect_reg(inst_list, 0);\n"
    "            if (target_is_shortstring)\n"
    "                snprintf(buffer, sizeof(buffer), \"\\tcall\\tkgpc_str_real_bounded_shortstring\\n\");\n"
)

# Lines 1069-1076: Windows has_width integer - movq value_reg, %rcx; movq width_reg, %rdx; (shortstring) movq addr_reg, %r8
src = replace_once(src,
    "                snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%rcx\\n\", value_reg->bit_64);\n"
    "                inst_list = add_inst(inst_list, buffer);\n"
    "                snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%rdx\\n\", width_reg->bit_64);\n"
    "                inst_list = add_inst(inst_list, buffer);\n"
    "                if (target_is_shortstring)\n"
    "                {\n"
    "                    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%r8\\n\", addr_reg->bit_64);\n"
    "                    inst_list = add_inst(inst_list, buffer);\n"
    "                    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t$%d, %%r9\\n\",\n"
    "                        target_shortstring_capacity);\n"
    "                    inst_list = add_inst(inst_list, buffer);\n"
    "                }\n"
    "                else\n"
    "                {\n"
    "                    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%r8\\n\", addr_reg->bit_64);\n"
    "                    inst_list = add_inst(inst_list, buffer);\n"
    "                }\n",
    "                { Register_t *_u[] = {value_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, \"\\tmovq\\t%0, %rcx\\n\"); }\n"
    "                { Register_t *_u[] = {width_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, \"\\tmovq\\t%0, %rdx\\n\"); }\n"
    "                if (target_is_shortstring)\n"
    "                {\n"
    "                    { Register_t *_u[] = {addr_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, \"\\tmovq\\t%0, %r8\\n\"); }\n"
    "                    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t$%d, %%r9\\n\",\n"
    "                        target_shortstring_capacity);\n"
    "                    inst_list = add_inst(inst_list, buffer);\n"
    "                }\n"
    "                else\n"
    "                {\n"
    "                    { Register_t *_u[] = {addr_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, \"\\tmovq\\t%0, %r8\\n\"); }\n"
    "                }\n"
)

# Lines 1089-1096: Linux has_width integer: movq value_reg, %rdi; movq width_reg, %rsi; (shortstring) movq addr_reg, %rdx; (else) movq addr_reg, %rdx
src = replace_once(src,
    "                snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%rdi\\n\", value_reg->bit_64);\n"
    "                inst_list = add_inst(inst_list, buffer);\n"
    "                snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%rsi\\n\", width_reg->bit_64);\n"
    "                inst_list = add_inst(inst_list, buffer);\n"
    "                if (target_is_shortstring)\n"
    "                {\n"
    "                    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%rdx\\n\", addr_reg->bit_64);\n"
    "                    inst_list = add_inst(inst_list, buffer);\n"
    "                    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t$%d, %%rcx\\n\",\n"
    "                        target_shortstring_capacity);\n"
    "                    inst_list = add_inst(inst_list, buffer);\n"
    "                }\n"
    "                else\n"
    "                {\n"
    "                    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%rdx\\n\", addr_reg->bit_64);\n"
    "                    inst_list = add_inst(inst_list, buffer);\n"
    "                }\n",
    "                { Register_t *_u[] = {value_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, \"\\tmovq\\t%0, %rdi\\n\"); }\n"
    "                { Register_t *_u[] = {width_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, \"\\tmovq\\t%0, %rsi\\n\"); }\n"
    "                if (target_is_shortstring)\n"
    "                {\n"
    "                    { Register_t *_u[] = {addr_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, \"\\tmovq\\t%0, %rdx\\n\"); }\n"
    "                    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t$%d, %%rcx\\n\",\n"
    "                        target_shortstring_capacity);\n"
    "                    inst_list = add_inst(inst_list, buffer);\n"
    "                }\n"
    "                else\n"
    "                {\n"
    "                    { Register_t *_u[] = {addr_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, \"\\tmovq\\t%0, %rdx\\n\"); }\n"
    "                }\n"
)

# Lines 1118-1131: Windows no-width integer: movq value_reg, %rcx; (shortstring) movq addr_reg, %rdx; (else) movq addr_reg, %rdx
src = replace_once(src,
    "                snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%rcx\\n\", value_reg->bit_64);\n"
    "                inst_list = add_inst(inst_list, buffer);\n"
    "                if (target_is_shortstring)\n"
    "                {\n"
    "                    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%rdx\\n\", addr_reg->bit_64);\n"
    "                    inst_list = add_inst(inst_list, buffer);\n"
    "                    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t$%d, %%r8\\n\",\n"
    "                        target_shortstring_capacity);\n"
    "                    inst_list = add_inst(inst_list, buffer);\n"
    "                }\n"
    "                else\n"
    "                {\n"
    "                    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%rdx\\n\", addr_reg->bit_64);\n"
    "                    inst_list = add_inst(inst_list, buffer);\n"
    "                }\n",
    "                { Register_t *_u[] = {value_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, \"\\tmovq\\t%0, %rcx\\n\"); }\n"
    "                if (target_is_shortstring)\n"
    "                {\n"
    "                    { Register_t *_u[] = {addr_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, \"\\tmovq\\t%0, %rdx\\n\"); }\n"
    "                    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t$%d, %%r8\\n\",\n"
    "                        target_shortstring_capacity);\n"
    "                    inst_list = add_inst(inst_list, buffer);\n"
    "                }\n"
    "                else\n"
    "                {\n"
    "                    { Register_t *_u[] = {addr_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, \"\\tmovq\\t%0, %rdx\\n\"); }\n"
    "                }\n"
)

# Lines 1136-1148: Linux no-width integer: movq value_reg, %rdi; (shortstring) movq addr_reg, %rsi; (else) movq addr_reg, %rsi
src = replace_once(src,
    "                snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%rdi\\n\", value_reg->bit_64);\n"
    "                inst_list = add_inst(inst_list, buffer);\n"
    "                if (target_is_shortstring)\n"
    "                {\n"
    "                    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%rsi\\n\", addr_reg->bit_64);\n"
    "                    inst_list = add_inst(inst_list, buffer);\n"
    "                    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t$%d, %%rdx\\n\",\n"
    "                        target_shortstring_capacity);\n"
    "                    inst_list = add_inst(inst_list, buffer);\n"
    "                }\n"
    "                else\n"
    "                {\n"
    "                    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%rsi\\n\", addr_reg->bit_64);\n"
    "                    inst_list = add_inst(inst_list, buffer);\n"
    "                }\n",
    "                { Register_t *_u[] = {value_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, \"\\tmovq\\t%0, %rdi\\n\"); }\n"
    "                if (target_is_shortstring)\n"
    "                {\n"
    "                    { Register_t *_u[] = {addr_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, \"\\tmovq\\t%0, %rsi\\n\"); }\n"
    "                    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t$%d, %%rdx\\n\",\n"
    "                        target_shortstring_capacity);\n"
    "                    inst_list = add_inst(inst_list, buffer);\n"
    "                }\n"
    "                else\n"
    "                {\n"
    "                    { Register_t *_u[] = {addr_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, \"\\tmovq\\t%0, %rsi\\n\"); }\n"
    "                }\n"
)

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 8: writestr - movq target_reg, -(off)(%rbp) (line 1206)
# ─────────────────────────────────────────────────────────────────────────────

src = replace_once(src,
    "    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, -%d(%%rbp)\\n\", target_reg->bit_64, target_slot->offset);\n"
    "    inst_list = add_inst(inst_list, buffer);\n"
    "    free_reg(get_reg_stack(), target_reg);\n",
    "    { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t%%0, -%%d(%%%%rbp)\\n\", target_slot->offset); Register_t *_u[] = {target_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "    free_reg(get_reg_stack(), target_reg);\n"
)

# Lines 1267-1272: writestr value_reg -> %rcx or %rdi
src = replace_once(src,
    "        if (codegen_target_is_windows())\n"
    "        {\n"
    "            snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%rcx\\n\", value_reg->bit_64);\n"
    "        }\n"
    "        else\n"
    "        {\n"
    "            snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%rdi\\n\", value_reg->bit_64);\n"
    "        }\n"
    "        inst_list = add_inst(inst_list, buffer);\n"
    "        free_reg(get_reg_stack(), value_reg);\n",
    "        { Register_t *_u[] = {value_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, codegen_target_is_windows() ? \"\\tmovq\\t%0, %rcx\\n\" : \"\\tmovq\\t%0, %rdi\\n\"); }\n"
    "        free_reg(get_reg_stack(), value_reg);\n"
)

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 9: insert - leaq and movq spill sites
# ─────────────────────────────────────────────────────────────────────────────

# Lines 1389-1390: leaq -(offset)(%rbp), source_reg (shortstring char buffer)
src = replace_once(src,
    "            snprintf(buffer, sizeof(buffer), \"\\tleaq\\t-%d(%%rbp), %s\\n\", char_buffer->offset, source_reg->bit_64);\n"
    "            inst_list = add_inst(inst_list, buffer);\n"
    "        }\n"
    "        else\n"
    "        {\n"
    "            snprintf(buffer, sizeof(buffer), \"\\tmovb\\t$0, -%d(%%rbp)\\n\", char_buffer->offset - 1);\n",
    "            { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tleaq\\t-%%d(%%%%rbp), %%0\\n\", char_buffer->offset); Register_t *_d[] = {source_reg}; inst_list = add_inst_du(inst_list, ctx, _d, 1, NULL, 0, _tmpl); }\n"
    "        }\n"
    "        else\n"
    "        {\n"
    "            snprintf(buffer, sizeof(buffer), \"\\tmovb\\t$0, -%d(%%rbp)\\n\", char_buffer->offset - 1);\n"
)

# Lines 1398-1399: leaq -(offset)(%rbp), source_reg (non-shortstring char buffer)
src = replace_once(src,
    "            snprintf(buffer, sizeof(buffer), \"\\tleaq\\t-%d(%%rbp), %s\\n\", char_buffer->offset, source_reg->bit_64);\n"
    "            inst_list = add_inst(inst_list, buffer);\n"
    "        }\n"
    "    }\n"
    "\n"
    "    StackNode_t *source_temp = add_l_t(\"insert_source\");\n",
    "            { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tleaq\\t-%%d(%%%%rbp), %%0\\n\", char_buffer->offset); Register_t *_d[] = {source_reg}; inst_list = add_inst_du(inst_list, ctx, _d, 1, NULL, 0, _tmpl); }\n"
    "        }\n"
    "    }\n"
    "\n"
    "    StackNode_t *source_temp = add_l_t(\"insert_source\");\n"
)

# Lines 1410-1411: movq source_reg, -(off)(%rbp)
src = replace_once(src,
    "    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, -%d(%%rbp)\\n\", source_reg->bit_64, source_temp->offset);\n"
    "    inst_list = add_inst(inst_list, buffer);\n"
    "    free_reg(get_reg_stack(), source_reg);\n",
    "    { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t%%0, -%%d(%%%%rbp)\\n\", source_temp->offset); Register_t *_u[] = {source_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "    free_reg(get_reg_stack(), source_reg);\n"
)

# Lines 1433-1434: movq target_reg, -(off)(%rbp)
src = replace_once(src,
    "    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, -%d(%%rbp)\\n\", target_reg->bit_64, target_temp->offset);\n"
    "    inst_list = add_inst(inst_list, buffer);\n"
    "    free_reg(get_reg_stack(), target_reg);\n"
    "\n"
    "    Register_t *index_reg = NULL;\n",
    "    { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t%%0, -%%d(%%%%rbp)\\n\", target_temp->offset); Register_t *_u[] = {target_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "    free_reg(get_reg_stack(), target_reg);\n"
    "\n"
    "    Register_t *index_reg = NULL;\n"
)

# Lines 1453-1454: movq index_reg, -(off)(%rbp) (insert)
src = replace_once(src,
    "    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, -%d(%%rbp)\\n\", index_reg->bit_64, index_temp->offset);\n"
    "    inst_list = add_inst(inst_list, buffer);\n"
    "    free_reg(get_reg_stack(), index_reg);\n"
    "\n"
    "    const char *arg0 = current_arg_reg64(0);\n"
    "    const char *arg1 = current_arg_reg64(1);\n"
    "    const char *arg2 = current_arg_reg64(2);\n"
    "    const char *arg3 = current_arg_reg64(3);\n",
    "    { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t%%0, -%%d(%%%%rbp)\\n\", index_temp->offset); Register_t *_u[] = {index_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "    free_reg(get_reg_stack(), index_reg);\n"
    "\n"
    "    const char *arg0 = current_arg_reg64(0);\n"
    "    const char *arg1 = current_arg_reg64(1);\n"
    "    const char *arg2 = current_arg_reg64(2);\n"
    "    const char *arg3 = current_arg_reg64(3);\n"
)

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 10: delete - spill sites
# ─────────────────────────────────────────────────────────────────────────────

# Lines 1523-1524: movq addr_reg, -(off)(%rbp) (delete)
src = replace_once(src,
    "    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, -%d(%%rbp)\\n\", addr_reg->bit_64, string_temp->offset);\n"
    "    inst_list = add_inst(inst_list, buffer);\n"
    "    free_reg(get_reg_stack(), addr_reg);\n"
    "\n"
    "    Register_t *index_reg = NULL;\n",
    "    { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t%%0, -%%d(%%%%rbp)\\n\", string_temp->offset); Register_t *_u[] = {addr_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "    free_reg(get_reg_stack(), addr_reg);\n"
    "\n"
    "    Register_t *index_reg = NULL;\n"
)

# Lines 1543-1544: movq index_reg, -(off)(%rbp) (delete)
src = replace_once(src,
    "    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, -%d(%%rbp)\\n\", index_reg->bit_64, index_temp->offset);\n"
    "    inst_list = add_inst(inst_list, buffer);\n"
    "    free_reg(get_reg_stack(), index_reg);\n"
    "\n"
    "    Register_t *count_reg = NULL;\n",
    "    { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t%%0, -%%d(%%%%rbp)\\n\", index_temp->offset); Register_t *_u[] = {index_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "    free_reg(get_reg_stack(), index_reg);\n"
    "\n"
    "    Register_t *count_reg = NULL;\n"
)

# Lines 1563-1564: movq count_reg, -(off)(%rbp) (delete)
src = replace_once(src,
    "    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, -%d(%%rbp)\\n\", count_reg->bit_64, count_temp->offset);\n"
    "    inst_list = add_inst(inst_list, buffer);\n"
    "    free_reg(get_reg_stack(), count_reg);\n"
    "\n"
    "    const char *arg0 = current_arg_reg64(0);\n"
    "    const char *arg1 = current_arg_reg64(1);\n"
    "    const char *arg2 = current_arg_reg64(2);\n",
    "    { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t%%0, -%%d(%%%%rbp)\\n\", count_temp->offset); Register_t *_u[] = {count_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "    free_reg(get_reg_stack(), count_reg);\n"
    "\n"
    "    const char *arg0 = current_arg_reg64(0);\n"
    "    const char *arg1 = current_arg_reg64(1);\n"
    "    const char *arg2 = current_arg_reg64(2);\n"
)

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 11: codegen_builtin_val - various sites
# ─────────────────────────────────────────────────────────────────────────────

# Line 1706-1707: movq code_addr, -(off)(%rbp)
src = replace_once(src,
    "        snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, -%d(%%rbp)\\n\", code_addr->bit_64, code_spill->offset);\n"
    "        inst_list = add_inst(inst_list, buffer);\n"
    "    }\n"
    "\n"
    "    char buffer[128];\n"
    "    if (codegen_target_is_windows())\n"
    "    {\n"
    "        snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%rcx\\n\", source_reg->bit_64);\n",
    "        { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t%%0, -%%d(%%%%rbp)\\n\", code_spill->offset); Register_t *_u[] = {code_addr}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "    }\n"
    "\n"
    "    char buffer[128];\n"
    "    if (codegen_target_is_windows())\n"
    "    {\n"
    "        snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%rcx\\n\", source_reg->bit_64);\n"
)

# Lines 1713-1714: movq source_reg, %rcx (Windows)
src = replace_once(src,
    "        snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%rcx\\n\", source_reg->bit_64);\n"
    "        inst_list = add_inst(inst_list, buffer);\n"
    "        if (value_result_spill != NULL)\n",
    "        { Register_t *_u[] = {source_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, \"\\tmovq\\t%0, %rcx\\n\"); }\n"
    "        if (value_result_spill != NULL)\n"
)

# Lines 1718-1719: movq value_addr, %rdx (Windows, value_result_spill==NULL branch)
src = replace_once(src,
    "            snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%rdx\\n\", value_addr->bit_64);\n"
    "        inst_list = add_inst(inst_list, buffer);\n"
    "    }\n"
    "    else\n"
    "    {\n"
    "        snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%rdi\\n\", source_reg->bit_64);\n",
    "            { Register_t *_u[] = {value_addr}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, \"\\tmovq\\t%0, %rdx\\n\"); }\n"
    "    }\n"
    "    else\n"
    "    {\n"
    "        snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%rdi\\n\", source_reg->bit_64);\n"
)

# Lines 1723-1724: movq source_reg, %rdi (Linux)
src = replace_once(src,
    "        snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%rdi\\n\", source_reg->bit_64);\n"
    "        inst_list = add_inst(inst_list, buffer);\n"
    "        if (value_result_spill != NULL)\n",
    "        { Register_t *_u[] = {source_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, \"\\tmovq\\t%0, %rdi\\n\"); }\n"
    "        if (value_result_spill != NULL)\n"
)

# Lines 1728-1729: movq value_addr, %rsi (Linux, value_result_spill==NULL branch)
src = replace_once(src,
    "            snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%rsi\\n\", value_addr->bit_64);\n"
    "        inst_list = add_inst(inst_list, buffer);\n"
    "    }\n"
    "    inst_list = codegen_vect_reg(inst_list, 0);\n"
    "    snprintf(buffer, sizeof(buffer), \"\\tcall\\t%s\\n\", call_target);\n",
    "            { Register_t *_u[] = {value_addr}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, \"\\tmovq\\t%0, %rsi\\n\"); }\n"
    "    }\n"
    "    inst_list = codegen_vect_reg(inst_list, 0);\n"
    "    snprintf(buffer, sizeof(buffer), \"\\tcall\\t%s\\n\", call_target);\n"
)

# Line 1751-1752: movq value_addr, %rax
src = replace_once(src,
    "        snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %%rax\\n\", value_addr->bit_64);\n"
    "        inst_list = add_inst(inst_list, buffer);\n"
    "\n"
    "        switch (value_store_size)\n",
    "        { Register_t *_u[] = {value_addr}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, \"\\tmovq\\t%0, %rax\\n\"); }\n"
    "\n"
    "        switch (value_store_size)\n"
)

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 12: prefetch (line 1832-1833)
# ─────────────────────────────────────────────────────────────────────────────

src = replace_once(src,
    "    char buffer[96];\n"
    "    const char *arg0 = current_arg_reg64(0);\n"
    "    snprintf(buffer, sizeof(buffer), \"\\tmovq\\t%s, %s\\n\", addr_reg->bit_64, arg0);\n"
    "    inst_list = add_inst(inst_list, buffer);\n"
    "    inst_list = codegen_vect_reg(inst_list, 0);\n"
    "    inst_list = codegen_call_with_shadow_space(inst_list, \"kgpc_prefetch\");\n",
    "    const char *arg0 = current_arg_reg64(0);\n"
    "    { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t%%0, %s\\n\", arg0); Register_t *_u[] = {addr_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "    inst_list = codegen_vect_reg(inst_list, 0);\n"
    "    inst_list = codegen_call_with_shadow_space(inst_list, \"kgpc_prefetch\");\n"
)

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 13: incdec - movq $1 / movl $1 (lines 1868, 1870)
# ─────────────────────────────────────────────────────────────────────────────

src = replace_once(src,
    "        if (target_expr != NULL && expr_uses_qword_kgpctype(target_expr))\n"
    "            snprintf(buffer, sizeof(buffer), \"\\tmovq\\t$1, %s\\n\", increment_reg->bit_64);\n"
    "        else\n"
    "            snprintf(buffer, sizeof(buffer), \"\\tmovl\\t$1, %s\\n\", increment_reg->bit_32);\n"
    "        inst_list = add_inst(inst_list, buffer);\n",
    "        if (target_expr != NULL && expr_uses_qword_kgpctype(target_expr))\n"
    "            { Register_t *_d[] = {increment_reg}; inst_list = add_inst_du(inst_list, ctx, _d, 1, NULL, 0, \"\\tmovq\\t$1, %0\\n\"); }\n"
    "        else\n"
    "            { Register_t *_d[] = {increment_reg}; inst_list = add_inst_du(inst_list, ctx, _d, 1, NULL, 0, \"\\tmovl\\t$1, %0\\n\"); }\n"
)

# Lines 1897/1899: imulq/imull $N, increment_reg (read-modify-write)
src = replace_once(src,
    "        if (target_uses_qword)\n"
    "            snprintf(buffer_scale, sizeof(buffer_scale), \"\\timulq\\t$%lld, %s\\n\", pointer_step, increment_reg->bit_64);\n"
    "        else\n"
    "            snprintf(buffer_scale, sizeof(buffer_scale), \"\\timull\\t$%lld, %s\\n\", pointer_step, increment_reg->bit_32);\n"
    "        inst_list = add_inst(inst_list, buffer_scale);\n",
    "        if (target_uses_qword)\n"
    "        {\n"
    "            char _stmpl[64]; snprintf(_stmpl, sizeof(_stmpl), \"\\timulq\\t$%%lld, %%0\\n\", pointer_step);\n"
    "            Register_t *_dru[] = {increment_reg};\n"
    "            inst_list = add_inst_du(inst_list, ctx, _dru, 1, _dru, 1, _stmpl);\n"
    "        }\n"
    "        else\n"
    "        {\n"
    "            char _stmpl[64]; snprintf(_stmpl, sizeof(_stmpl), \"\\timull\\t$%%lld, %%0\\n\", pointer_step);\n"
    "            Register_t *_dru[] = {increment_reg};\n"
    "            inst_list = add_inst_du(inst_list, ctx, _dru, 1, _dru, 1, _stmpl);\n"
    "        }\n"
)

# Lines 1908/1913: negq/negl increment_reg (read-modify-write)
src = replace_once(src,
    "        if (target_uses_qword)\n"
    "        {\n"
    "            snprintf(buffer_main, sizeof(buffer_main), \"\\tnegq\\t%s\\n\", increment_reg->bit_64);\n"
    "            inst_list = add_inst(inst_list, buffer_main);\n"
    "        }\n"
    "        else\n"
    "        {\n"
    "            snprintf(buffer_main, sizeof(buffer_main), \"\\tnegl\\t%s\\n\", increment_reg->bit_32);\n"
    "            inst_list = add_inst(inst_list, buffer_main);\n"
    "        }\n",
    "        if (target_uses_qword)\n"
    "        {\n"
    "            Register_t *_dru[] = {increment_reg};\n"
    "            inst_list = add_inst_du(inst_list, ctx, _dru, 1, _dru, 1, \"\\tnegq\\t%0\\n\");\n"
    "        }\n"
    "        else\n"
    "        {\n"
    "            Register_t *_dru[] = {increment_reg};\n"
    "            inst_list = add_inst_du(inst_list, ctx, _dru, 1, _dru, 1, \"\\tnegl\\t%0\\n\");\n"
    "        }\n"
)

# Lines 1945-1950: spill increment_reg to stack (movq/movl)
src = replace_once(src,
    "        if (target_uses_qword)\n"
    "            snprintf(spill_buf, sizeof(spill_buf), \"\\tmovq\\t%s, -%d(%%rbp)\\n\",\n"
    "                increment_reg->bit_64, inc_spill->offset);\n"
    "        else\n"
    "            snprintf(spill_buf, sizeof(spill_buf), \"\\tmovl\\t%s, -%d(%%rbp)\\n\",\n"
    "                increment_reg->bit_32, inc_spill->offset);\n"
    "        inst_list = add_inst(inst_list, spill_buf);\n",
    "        if (target_uses_qword)\n"
    "        {\n"
    "            char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovq\\t%%0, -%%d(%%%%rbp)\\n\", inc_spill->offset);\n"
    "            Register_t *_u[] = {increment_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl);\n"
    "        }\n"
    "        else\n"
    "        {\n"
    "            char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\tmovl\\t%%0, -%%d(%%%%rbp)\\n\", inc_spill->offset);\n"
    "            Register_t *_u[] = {increment_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl);\n"
    "        }\n"
)

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 14: incdec - addq/addl to (addr_reg) - is_reference branch (lines 1991-1995)
# ─────────────────────────────────────────────────────────────────────────────

# The is_reference branch in EXPR_VAR_ID: addq/addl value_reg64/32, (addr_reg)
src = replace_once(src,
    "                    if (target_uses_qword)\n"
    "                        snprintf(buffer, sizeof(buffer), \"\\taddq\\t%s, (%s)\\n\", value_reg64, addr_reg->bit_64);\n"
    "                    else\n"
    "                        snprintf(buffer, sizeof(buffer), \"\\taddl\\t%s, (%s)\\n\", value_reg32, addr_reg->bit_64);\n"
    "                    inst_list = add_inst(inst_list, buffer);\n"
    "                    free_reg(get_reg_stack(), addr_reg);\n"
    "                }\n"
    "            }\n"
    "            else if (var_node->is_static)\n",
    "                    if (target_uses_qword)\n"
    "                    {\n"
    "                        if (increment_reg != NULL)\n"
    "                        { Register_t *_u[] = {increment_reg, addr_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 2, \"\\taddq\\t%0, (%1)\\n\"); }\n"
    "                        else\n"
    "                        { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\taddq\\t%%%%rax, (%s)\\n\", addr_reg->bit_64); Register_t *_u[] = {addr_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "                    }\n"
    "                    else\n"
    "                    {\n"
    "                        if (increment_reg != NULL)\n"
    "                        { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\taddl\\t%%0, (%s)\\n\", addr_reg->bit_64); Register_t *_u[] = {increment_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "                        else\n"
    "                        { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\taddl\\t%%%%eax, (%s)\\n\", addr_reg->bit_64); Register_t *_u[] = {addr_reg}; inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "                    }\n"
    "                    free_reg(get_reg_stack(), addr_reg);\n"
    "                }\n"
    "            }\n"
    "            else if (var_node->is_static)\n"
)

# Lines 2004-2006: addq/addl increment_reg, label(%rip)
src = replace_once(src,
    "                if (target_uses_qword)\n"
    "                    snprintf(buffer, sizeof(buffer), \"\\taddq\\t%s, %s(%%rip)\\n\", increment_reg->bit_64, label);\n"
    "                else\n"
    "                    snprintf(buffer, sizeof(buffer), \"\\taddl\\t%s, %s(%%rip)\\n\", increment_reg->bit_32, label);\n",
    "                if (target_uses_qword)\n"
    "                { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\taddq\\t%%0, %s(%%%%rip)\\n\", label); Register_t *_u[] = {increment_reg}; snprintf(buffer, sizeof(buffer), \"\\taddq\\t%s, %s(%%rip)\\n\", increment_reg->bit_64, label); inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
    "                else\n"
    "                { char _tmpl[64]; snprintf(_tmpl, sizeof(_tmpl), \"\\taddl\\t%%0, %s(%%%%rip)\\n\", label); Register_t *_u[] = {increment_reg}; snprintf(buffer, sizeof(buffer), \"\\taddl\\t%s, %s(%%rip)\\n\", increment_reg->bit_32, label); inst_list = add_inst_du(inst_list, ctx, NULL, 0, _u, 1, _tmpl); }\n"
)

# Wait, the is_static branch doesn't have its own add_inst - the add_inst is after the if-else.
# Let me re-read the code around line 1999-2006.
# From the source, lines 1999-2039:
# else if (var_node->is_static) { snprintf(...) }
# else if (scope_depth > 0) { ... snprintf() }
# else { snprintf() }
# if (!var_node->is_reference) inst_list = add_inst(inst_list, buffer);
# 
# So for is_static, scope_depth, else - the shared add_inst is at line ~2039.
# These only have snprintf, no individual add_inst.
# We need to add add_inst_du inside each and prevent the shared one.
# 
# But the instruction says to migrate these. Let me rethink.
# The is_reference branch already has its own add_inst (at line 1995).
# The is_static, scope_depth, and else branches build buffer and then fall through to:
#   if (!var_node->is_reference) inst_list = add_inst(inst_list, buffer);
# 
# So strategy:
# - For is_static: add add_inst_du inside, keep snprintf (because the fallthrough also emits via add_inst)
#   Wait, actually the fallthrough `if (!var_node->is_reference)` will still fire for non-reference cases
#   So for is_static - it's fine: the snprintf builds the buffer, then add_inst fires (since is_reference would be false for static).
#   But we want to replace add_inst with add_inst_du. So we need to:
#   - Add add_inst_du inside the is_static branch 
#   - Use a flag to skip the shared add_inst
# 
# Actually, looking more carefully, the pattern is similar to the setlength descriptor setup!
# Let me re-read the instructions for this section...
#
# The instructions say:
# Lines 2004: `addq increment_reg, label(%rip)` → USE=increment_reg, label is literal
# Lines 2006: `addl increment_reg->bit_32, label(%rip)` → USE=increment_reg
# 
# These are inside the is_static branch. The add_inst that fires them is at line ~2039.
# My previous replacement above was wrong - I added add_inst_du but also kept the snprintf
# and didn't prevent the shared add_inst. Let me revert that and do it properly.
# 
# Actually wait - I already did the replacement above in a way that adds both the new snprintf 
# (for buffer) and the add_inst_du. But then the shared `if (!var_node->is_reference) add_inst`
# would fire again! That's wrong.
#
# Let me redo this properly with a flag approach like setlength.
# But first let me check: did the replacement above work?
# I replaced the snprintf-only lines with snprintf+add_inst_du. But the old buffer still gets
# overwritten... Actually wait, in the is_static branch the snprintf is storing into buffer,
# and the add_inst at the end fires for is_static (since is_reference=false for static vars).
# 
# So my replacement above is wrong. The approach should be:
# 1. Add add_inst_du inside is_static branch
# 2. Add a flag to prevent the shared add_inst from firing

# UNDO the bad replacement above and redo it properly.
# First let me check what the current state looks like...

print("Checking is_static branch...")
idx = src.find("else if (var_node->is_static)")
if idx == -1:
    print("ERROR: is_static not found!")
else:
    print(f"Found at position {idx}")
    print(src[idx:idx+500])

