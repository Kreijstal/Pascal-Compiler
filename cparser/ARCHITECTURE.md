# cparser Library Architecture

## Overview

The cparser library is a parser combinator framework for building recursive descent parsers in C. It uses memoization (packrat parsing) to handle left-recursion and improve performance.

## Component Diagram

```
┌─────────────────────────────────────────────────────────────┐
│                     Parser Application                       │
│              (e.g., Pascal Parser, Calculator)               │
└─────────────────────────┬───────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────┐
│                   Combinator Library                         │
│  ┌────────────┬──────────────┬────────────┬──────────────┐ │
│  │   Basic    │  Sequential  │  Choice    │  Repetition  │ │
│  │ match()    │   seq()      │  multi()   │   many()     │ │
│  │ satisfy()  │   between()  │  optional()│   sep_by()   │ │
│  └────────────┴──────────────┴────────────┴──────────────┘ │
└─────────────────────────┬───────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────┐
│                     Parser Engine                            │
│  ┌──────────────────────────────────────────────────────┐  │
│  │              parse(input, combinator)                │  │
│  │  • Position tracking (line/column)                   │  │
│  │  • Memoization lookup/store                          │  │
│  │  • Backtracking management                           │  │
│  │  • Error propagation                                 │  │
│  └──────────────────────────────────────────────────────┘  │
└─────────────┬──────────────────────┬────────────────────────┘
              │                      │
              ▼                      ▼
┌──────────────────────┐  ┌──────────────────────────────────┐
│  Memoization Table   │  │      AST Construction            │
│  ┌────────────────┐  │  │  ┌────────────────────────────┐ │
│  │ Hash Table     │  │  │  │  ast_t nodes               │ │
│  │ Key: (pos,comb)│  │  │  │  • typ: tag_t              │ │
│  │ Val: result    │  │  │  │  • child/next: tree struct │ │
│  └────────────────┘  │  │  │  • sym: symbol info        │ │
│                      │  │  │  • line/col: position      │ │
└──────────────────────┘  │  └────────────────────────────┘ │
                          └──────────────────────────────────┘
```

## Data Flow

### Parsing Flow

```
Input String
    │
    ▼
┌─────────────────┐
│  Preprocessor   │ (if applicable)
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  input_t        │
│  • buffer       │
│  • position     │
│  • memo table   │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  combinator_t   │◄─────┐
│  • type         │      │ Recursive
│  • fn()         │      │ Calls
│  • args         │──────┘
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  ParseResult    │
│  • is_success   │
│  • ast or error │
└─────────────────┘
```

### Memoization Flow

```
parse(input, combinator)
    │
    ├──► memo_table_lookup(combinator_id, position)
    │        │
    │        ├──► Hit: replay saved result
    │        │         └──► Return cached ParseResult
    │        │
    │        └──► Miss: continue parsing
    │                  │
    │                  ▼
    │             Execute combinator function
    │                  │
    │                  ▼
    │             memo_table_store(result)
    │                  │
    └──────────────────┴──► Return ParseResult
```

## Key Structures

### combinator_t
The core abstraction for all parsers:
```c
struct combinator_t {
    parser_type_t type;     // P_MATCH, COMB_SEQ, etc.
    comb_fn fn;             // Function pointer to parser logic
    void* args;             // Arguments for this combinator
    void* extra_to_free;    // Additional memory to free
    char* name;             // For debugging
    size_t memo_id;         // Unique ID for memoization
};
```

### input_t
Tracks parsing state:
```c
struct input_t {
    char* buffer;           // Input text
    int length;             // Buffer length
    int start;              // Current position
    int line, col;          // Position tracking
    memo_table_t* memo;     // Memoization cache
};
```

### ParseResult
Result of a parse attempt:
```c
struct ParseResult {
    bool is_success;
    union {
        ast_t* ast;         // On success
        ParseError* error;  // On failure
    } value;
};
```

### ast_t
Abstract syntax tree node:
```c
struct ast_t {
    tag_t typ;              // Node type (grammar-specific)
    ast_t* child;           // First child
    ast_t* next;            // Next sibling
    sym_t* sym;             // Symbol info (optional)
    int line, col;          // Source position
};
```

## Critical Paths

### Performance-Critical Operations
1. **memo_table_lookup**: O(1) average, called for every parse attempt
2. **clone_parse_result**: Deep copy for memoization, expensive
3. **AST node allocation**: ~3.9 allocations per line of input
4. **Backtracking**: Can be exponential in pathological cases

### Memory-Critical Operations
1. **Memo table growth**: Proportional to input size × combinator count
2. **AST construction**: Tree structure with many small allocations
3. **Error cloning**: During backtracking, errors are cloned repeatedly
4. **Combinator creation**: Dynamic combinators leak if not tracked

## Extension Points

### Adding New Combinators
1. Define parser_type_t enum value
2. Implement comb_fn function
3. Create constructor function
4. Register in combinator system

Example:
```c
ParseResult my_combinator_fn(input_t* in, void* args, char* name) {
    // Implementation
}

combinator_t* my_combinator(/* args */) {
    combinator_t* c = new_combinator();
    c->type = COMB_MY_COMBINATOR;
    c->fn = my_combinator_fn;
    c->args = /* ... */;
    return c;
}
```

### Custom AST Node Types
Define tag_t constants for grammar-specific node types:
```c
#define PASCAL_T_PROGRAM 1000
#define PASCAL_T_VAR_DECL 1001
// etc.
```

### Error Recovery
Implement custom error handling:
```c
ParseResult with_recovery(input_t* in, combinator_t* parser) {
    ParseResult res = parse(in, parser);
    if (!res.is_success) {
        // Custom recovery logic
        // Update in->start to skip past error
        // Return partial result
    }
    return res;
}
```

## Known Limitations

1. **Left Recursion**: Handled via memoization but with performance cost
2. **Ambiguity**: `multi()` takes first successful alternative
3. **Error Messages**: Basic, lack rich context
4. **Memory**: No pooling, each node individually allocated
5. **Parallelism**: Single-threaded design

## Related Documentation

- `REFACTORING_ROADMAP.md`: Planned improvements and architecture evolution
- `parser.h`: Public API documentation
- `tests.c`: Usage examples and test cases
