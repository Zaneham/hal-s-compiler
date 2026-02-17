# HAL/S Compiler

> **Work in Progress** - Lexer and parser functional, codegen not yet implemented.

A compiler for HAL/S (High-order Assembly Language/Shuttle), written in OCaml.

## What is HAL/S?

HAL/S was developed by Intermetrics for NASA in the early 1970s for real-time aerospace applications. It powered the Space Shuttle's Primary Avionics Software System (~420,000 lines) across all 135 missions from 1981 to 2011. The language has built-in support for vectors, matrices, real-time scheduling, and deterministic execution — no dynamic memory allocation, no garbage collection.

## Current Status

- **Lexer** (ocamllex) - complete
- **Parser** (menhir LR(1)) - complete, single-line format
- **Multi-line E/M/S format** - not yet implemented
- **Semantic analysis** - not yet implemented
- **Code generation** - not yet implemented

## Building

```bash
opam install . --deps-only
dune build
dune exec halsc -- examples/hello.hal -ast
```

### Requirements

- OCaml 4.14+
- Dune 3.0+
- Menhir

## Language Example

```text
PROGRAM GUIDANCE_LOOP;

  DECLARE POSITION VECTOR(3);
  DECLARE VELOCITY VECTOR(3);
  DECLARE DELTA_V VECTOR(3);
  DECLARE DT SCALAR CONSTANT INITIAL(0.04);

  /* State integration - runs at 25 Hz */
  POSITION = POSITION + VELOCITY * DT;
  VELOCITY = VELOCITY + DELTA_V;

  IF POSITION.3 < 0.0 THEN
    CALL ABORT_GUIDANCE;
  END;

  SCHEDULE GUIDANCE_LOOP IN DT PRIORITY(10);

CLOSE GUIDANCE_LOOP;
```

Notable syntax: exponents on the E-line above, subscripts on the S-line below, spaces for multiplication, `.1`/`.2`/`.3` for vector components, `SCHEDULE`/`WAIT` as first-class statements.

## The Specification

Based on the HAL/S-FC Compiler System Specification (IR-95-6, Intermetrics, 1977), obtained from the Wichita State University Libraries Special Collections and University Archives.

## Specification Documents

- HAL/S Language Specification, IR 61-7 (Intermetrics, 1976)
- HAL/S-FC Compiler System Specification, IR-95-6 (Intermetrics, 1977)
- HAL/S-FC Compiler Functional Specification, IR 59-4 (Intermetrics, 1974)
- HAL/S Programmer's Guide, NASA (various editions)

## Related Projects

- [hals-lsp](https://github.com/Zaneham/hals-lsp) - HAL/S Language Server for VS Code
- [coral66-compiler](../coral66-compiler) - CORAL 66 compiler
- [jovial-compiler](https://github.com/Zaneham/jovial-compiler) - JOVIAL J73 compiler

## Acknowledgements

The HAL/S-FC Compiler System Specification was obtained from the **Wichita State University Libraries, Special Collections and University Archives**.

## License

Apache 2.0
