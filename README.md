# Harmonica
Harmonica Programming Language

## Overview
This started as a course project at Columbia University and I'm now using it to experiment with language features just for fun.

Harmonica is a mixture of Go-Python language with support for functional programming and concurrency.

## Instructions

### Installation under Ubuntu

Currently Ubuntu is the only supported system. We are doing some clang-to-llvm bindings which requires the proper clang version that's not available on macOS/Windows, so getting it to work on other platforms would be very cumbersome.

Compiling this project requires clang-3.4, llvm-3.4, and opam 1.2. These are not the default for most Ubuntu systems, so you'll need to explicitly specify the version of packages you are installing.

```
sudo add-apt-repository ppa:avsm/ppa
sudo apt-get update
sudo apt-get install ocaml opam
sudo apt-get install -y m4 clang-3.4 llvm-3.4
opam init
opam install llvm.3.4 ocamlfind
eval `opam config env`

make
./testall.sh

```

------------------------------
### Installation under macOS

The cgen branch of this project works with macOS, but other branches do not. cgen translates harmonica programs to C instead of LLVM. In order to run it on macOS, simply do:

```
brew install opam
eval `opam config env`

make
./testall.sh
```
