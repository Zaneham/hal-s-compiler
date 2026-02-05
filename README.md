# HAL/S Compiler

> **Work in Progress** - Lexer and parser functional, codegen coming soon. NASA had a 40-year head start and a bigger budget.

A modern compiler for HAL/S (High-order Assembly Language/Shuttle), written in OCaml. Because the Space Shuttle's programming language deserves to live on.


## What is HAL/S?

HAL/S was developed by Intermetrics for NASA in the early 1970s, because someone looked at the Space Shuttle and thought "this needs its own programming language." They were right. You don't fly a spacecraft with FORTRAN. Well, you could, but you wouldn't enjoy it.

The language was designed specifically for real-time aerospace applications, with built-in support for vectors, matrices, and scheduling. It's what happens when you ask MIT engineers to make a language for keeping astronauts alive. Fixed-point arithmetic, deterministic execution, and absolutely no garbage collection.

**Where it ran:**

| System | What It Did | Flight Heritage |
|--------|-------------|-----------------|
| Primary Avionics Software System | Guidance, navigation, control | 135 missions |
| Backup Flight System | Independent redundant GNC | Never needed, thankfully |
| Systems Management | Vehicle health monitoring | Every Shuttle flight |
| Payload Operations | Experiment control, RMS arm | Hubble, ISS construction |

The software ran on the IBM AP-101S computers - five of them, because four-way redundancy wasn't paranoid enough. The Primary system was about 420,000 lines of HAL/S. The Backup was written by a completely separate team in a completely separate building to avoid common-mode failures. Aerospace engineering is a lifestyle.

## Features

This compiler aims to implement:

- **Full HAL/S Language Support** - Per the HAL/S Language Specification (IR 61-7)
- **Vector/Matrix Operations** - First-class 3-vectors and matrices, because aerospace
- **Real-Time Constructs** - SCHEDULE, WAIT, SIGNAL, and friends
- **COMPOOL Support** - Shared data pools between compilation units
- **Modern Diagnostics** - Error messages that tell you what went wrong

## Building

```bash
# Install OCaml dependencies
opam install . --deps-only

# Build the compiler
dune build

# Parse some flight software
dune exec halsc -- examples/hello.hal -ast

# Question your life choices
```

### Requirements

- OCaml 4.14 or later
- Dune 3.0+
- Menhir (parser generator)
- A tolerance for languages older than most programmers

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

  /* Check altitude constraint */
  IF POSITION.3 < 0.0 THEN
    CALL ABORT_GUIDANCE;
  END;

  /* Schedule next cycle */
  SCHEDULE GUIDANCE_LOOP IN DT PRIORITY(10);

CLOSE GUIDANCE_LOOP;
```

HAL/S has some interesting syntax choices:
- Exponents are written on the line above (E-line)
- Subscripts are written on the line below (S-line)
- Vector components use `.1`, `.2`, `.3`
- `SCHEDULE` and `WAIT` are first-class statements
- Everything is deterministic because floating debris is bad

This compiler currently handles single-line format. Multi-line E/M/S format support is planned.

## The Specification

This compiler is based on the HAL/S-FC Compiler System Specification (IR-95-6, January 1977), a 251-page document that describes how Intermetrics built the original compiler.

The specification includes:
- Phase I: Lexical and syntactic analysis, HALMAT intermediate representation
- Phase II: Code generation for the AP-101S
- Runtime library interfaces for vector/matrix operations
- Real-time executive integration

We obtained this document from the Wichita State University Libraries Special Collections and University Archives. If you're researching aerospace computing history, their collection is worth exploring.

## Why OCaml?

Because writing a compiler for NASA's flight software language in a French functional language felt appropriately international. Also the type system is excellent for compiler construction and menhir generates better parsers than I could write by hand.

## Why Does This Exist?

1. Historical preservation of aerospace computing heritage
2. The HAL/S language design is genuinely interesting
3. Real-time language features that modern languages still struggle with
4. Someone should be able to read Shuttle flight software without hunting for 1980s IBM documentation
5. Educational value for compiler construction and language design
6. It's a fascinating piece of engineering history

## Related Projects

If you find dead languages interesting:

| Project | What It Is | Historical Context |
|---------|-----------|-------------------|
| [coral66-compiler](../coral66-compiler) | CORAL 66 native compiler | British nuclear reactors |
| [jovial-compiler](https://github.com/Zaneham/jovial-compiler) | JOVIAL J73 compiler | US Air Force, B-2 bomber |
| [coral66-lsp](https://github.com/Zaneham/coral66-lsp) | CORAL 66 Language Server | IDE support for reactor code |

## Fun Facts

- The language has three input lines per statement: E-line (exponents), M-line (main), and S-line (subscripts). This was designed for mathematical notation on 1970s line printers. It looks exactly as unhinged as you'd expect.
- HAL/S has no dynamic memory allocation. Every variable's lifetime is known at compile time. This is a feature, not a limitation - you don't want malloc failing at Mach 25.
- The Backup Flight System was written by Rockwell instead of IBM to ensure no common bugs. Two teams, two buildings, one specification. They found a specification ambiguity once. It was a whole thing.
- Vector cross products use the `CROSS` operator. `V1 CROSS V2` is valid HAL/S. The mathematicians won that argument.
- IBM charged NASA by the line of code. The HAL/S compiler team at Intermetrics wrote tools to pack more statements per line. Software economics hasn't changed much.
- The AP-101S had 424KB of memory for all five computers combined. Your browser tab is using more RAM than the entire Space Shuttle avionics suite.

## Acknowledgements

The HAL/S-FC Compiler System Specification was obtained from the **Wichita State University Libraries, Special Collections and University Archives**. Their preservation of aerospace computing documents makes projects like this possible.

Thanks also to the original Intermetrics team who designed a language that flew 135 missions without a software-caused failure. That's a safety record most modern software can only dream of.

## Specification Documents

- HAL/S Language Specification, IR 61-7 (Intermetrics, 1976)
- HAL/S-FC Compiler System Specification, IR-95-6 (Intermetrics, 1977)
- HAL/S-FC Compiler Functional Specification, IR 59-4 (Intermetrics, 1974)
- HAL/S Programmer's Guide, NASA (various editions)

If you have access to HAL/S documentation not widely available, I would be unreasonably grateful.

## Contact

Found a bug? Want to discuss aerospace computing history? Have opinions about real-time language design?

**zanehambly@gmail.com**

Based in New Zealand, where it's already tomorrow and the closest thing to a space program is really good weather satellites.

## License

Apache 2.0

---

