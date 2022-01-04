Coursework-Compiler
===================

A minimal if-while-functions compiler, featuring static type deduction, an IR layer, optimizations and an NASM codegen.
Created for ELTE's MSc Compilers course.

```rust
fn main() {
    let a = read_int() + 1;
    let b: bool = read_bool();
    let c = 0;

    if a > 20 && b {
        write(a);

        while a > 0 {
            a -= 1;
            c += a;
        }
    }

    write(c);
}
```

---

Usage
-----

Compiling an ``example.lang`` file:

```bash
mkdir out
cd out

# Compile the program
coursework-compiler example.lang -o example.nasm

# Build with nasm, ELF64 target
nasm -f elf64 example.nasm -o example.o

# Build the IO library with rustc
rustc ../assets/io.rs --print native-static-libs -o libio.a
# This will give a list of libraries like: ``-l:gcc_s -lutil -lrt -lpthread -lm -ldl -lc``

# Using that list, link the executable
ld example.o libio.a  -lgcc_s -lutil -lrt -lpthread -lm -ldl -lc  -o example
# ld <NASM output> <Library> <Static libs> -o <output-file>
```

The executables currently target 64-bit ELF on Linux.
Tested and working with Rust target `x86_64-unknown-linux-gnu`.

Sometimes the linker isn't able to find `gcc_s`, in that case try linking using `-l:libgcc_s.so.1` or similar.

You can also emit various artifacts of the compiler using ``--emit=``. Currently AST, IR, and IR-opt can be emitted.

To disable optimizations use the ``--no-opt`` flag.

---

Building the compiler
---------------------

Building requires nightly Rust, because of the `rustc_index` library.

Building the compiler is done using ``cargo build``.
Tests can be run via ``cargo test``.
The executable is located at ``target/<build-type>/coursework-compiler.exe``.
