/*
 * GpcType.c
 * First-class type system implementation for the Gwinn Pascal Compiler
 */

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include "GpcType.h"
#include "type_tags.h"

// --- Constructor Implementations ---

GpcType* create_primitive_type(int primitive_tag) {
    GpcType *type = (GpcType *)calloc(1, sizeof(GpcType));
    assert(type != NULL);
    type->kind = TYPE_KIND_PRIMITIVE;
    type->info.primitive_type_tag = primitive_tag;
    // Size will be determined later in semcheck
    return type;
}

GpcType* create_pointer_type(GpcType *points_to) {
    GpcType *type = (GpcType *)calloc(1, sizeof(GpcType));
    assert(type != NULL);
    type->kind = TYPE_KIND_POINTER;
    type->info.points_to = points_to; // Takes ownership of points_to
    return type;
}

GpcType* create_procedure_type(ListNode_t *params, GpcType *return_type) {
    GpcType *type = (GpcType *)calloc(1, sizeof(GpcType));
    assert(type != NULL);
    type->kind = TYPE_KIND_PROCEDURE;
    type->info.proc_info.params = params; // Takes ownership
    type->info.proc_info.return_type = return_type; // Takes ownership
    return type;
}

GpcType* create_array_type(GpcType *element_type, int start_index, int end_index) {
    GpcType *type = (GpcType *)calloc(1, sizeof(GpcType));
    assert(type != NULL);
    type->kind = TYPE_KIND_ARRAY;
    type->info.array_info.element_type = element_type; // Takes ownership
    type->info.array_info.start_index = start_index;
    type->info.array_info.end_index = end_index;
    return type;
}

GpcType* create_record_type(struct RecordType *record_info) {
    GpcType *type = (GpcType *)calloc(1, sizeof(GpcType));
    assert(type != NULL);
    type->kind = TYPE_KIND_RECORD;
    type->info.record_info = record_info; // Record is owned by the AST
    return type;
}

// --- Destructor Implementation ---

void destroy_gpc_type(GpcType *type) {
    if (type == NULL) return;

    switch (type->kind) {
        case TYPE_KIND_POINTER:
            destroy_gpc_type(type->info.points_to);
            break;
        case TYPE_KIND_PROCEDURE:
            // The list itself contains Tree_t*, which are owned by the AST
            // and should not be freed here. Just free the list nodes.
            DestroyList(type->info.proc_info.params);
            destroy_gpc_type(type->info.proc_info.return_type);
            break;
        case TYPE_KIND_ARRAY:
            destroy_gpc_type(type->info.array_info.element_type);
            break;
        case TYPE_KIND_PRIMITIVE:
            // Nothing to free
            break;
        case TYPE_KIND_RECORD:
            // record_info is owned by the AST, not by us
            break;
    }
    free(type);
}

// --- Utility Implementations ---

int are_types_compatible_for_assignment(GpcType *lhs_type, GpcType *rhs_type) {
    // TODO in Phase 3: Implement full compatibility logic.
    // For now, a basic check is enough.
    if (lhs_type == NULL || rhs_type == NULL) return 0;
    if (lhs_type->kind != rhs_type->kind) return 0;
    if (lhs_type->kind == TYPE_KIND_PRIMITIVE) {
        return lhs_type->info.primitive_type_tag == rhs_type->info.primitive_type_tag;
    }
    // This will be expanded for procedure signature comparison.
    return 1;
}

const char* gpc_type_to_string(GpcType *type) {
    static char buffer[256];
    if (type == NULL) {
        return "NULL";
    }
    
    switch (type->kind) {
        case TYPE_KIND_PRIMITIVE:
            switch (type->info.primitive_type_tag) {
                case INT_TYPE: return "integer";
                case REAL_TYPE: return "real";
                case LONGINT_TYPE: return "longint";
                case STRING_TYPE: return "string";
                case CHAR_TYPE: return "char";
                case BOOL: return "boolean";
                case POINTER_TYPE: return "pointer";
                case SET_TYPE: return "set";
                case ENUM_TYPE: return "enum";
                case FILE_TYPE: return "file";
                default:
                    snprintf(buffer, sizeof(buffer), "primitive(%d)", type->info.primitive_type_tag);
                    return buffer;
            }
        case TYPE_KIND_POINTER:
            snprintf(buffer, sizeof(buffer), "^%s", gpc_type_to_string(type->info.points_to));
            return buffer;
        case TYPE_KIND_ARRAY:
            snprintf(buffer, sizeof(buffer), "array[%d..%d] of %s",
                type->info.array_info.start_index,
                type->info.array_info.end_index,
                gpc_type_to_string(type->info.array_info.element_type));
            return buffer;
        case TYPE_KIND_RECORD:
            return "record";
        case TYPE_KIND_PROCEDURE:
            if (type->info.proc_info.return_type == NULL) {
                return "procedure";
            } else {
                snprintf(buffer, sizeof(buffer), "function: %s",
                    gpc_type_to_string(type->info.proc_info.return_type));
                return buffer;
            }
        default:
            return "unknown";
    }
}
