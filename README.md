# lori

A lua5.1 interpreter written in rust. This is still very much a WIP (a lot has to be done between now and
when it will be feature-complete).

## What needs doing

- Migrating the parser and the lexer away from nom (and towards lalrpop)
- Writing Garbage collector abstraction (Reference counting might work for now, but it will leak when cyclic references occur)
- Write Tree-Visiting Interpreter
- Implement coroutines
- Implement Lua Tables
- Implement Lua Functions
- Implement Standard Library (far off)
- Implement C API (far off)

## How things are structured

    main.rs : Contains simple execution stub for interpreter, will eventually parse args
    ast_types.rs : contains the various types of nodes in the AST
    parse/ : contains everything related to lexing/parsing
    interpreter/ : contains everything related to actually evaluating lua code
