# Custom Compiler for the Juice Language

## Overview
This project features a custom compiler designed and implemented for **Juice Lang**, a small, self-developed programming language. The compiler translates high-level source code into performant machine code.

## Features
- Lexical analysis and tokenization
- Syntactic analysis and parsing
- Semantic analysis and type checking
- Intermediate code generation
- Machine code generation

## Requirements
- [Ocaml Language](https://ocaml.org/install)
- [Dune Build System](https://github.com/ocaml/dune)

## Installation
1. Clone the repository:
    ```bash
    git clone https://github.com/zachary0249/juice_lang.git
    ```
2. Navigate to the project directory:
    ```bash
    cd juice_lang
    ```


## Usage
There are different commands:
- lexer
- parser
- codegen
```bash
dune exec -- -c <command> -f <sourcefile>.jce
```

**Note that this project is a work in progress**
