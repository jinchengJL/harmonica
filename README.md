# Harmonica
Harmonica Programming Language

## Overview 
This is a course project PLT-Fall2016 at Columbia University.

## Goal
We are implementing a mixture of Go-Python language that supports functional programming and concurrency based on OCaml -> LLVM. A functional and parallel programming languange is perfect for scientific computing on large data sets over distributed systems. The goal of the project is for Harmonica to be able to handle large matrix operations / data frame manipulation / signal processing computations efficiently.

## Instructions
------------------------------
### Installation under Ubuntu

Compiling this project requires clang-3.4, llvm-3.4, and opam 1.2. These are not the default for any Ubuntu systems we know of, so it is required to explicitly specify the version of packages you are installing.

```
sudo add-apt-repository ppa:avsm/ppa
sudo apt-get update
sudo apt-get install ocaml opam
sudo apt-get install -y m4 clang-3.4 llvm-3.4
opam init
opam install llvm.3.6 ocamlfind
eval `opam config env`

make
./testall.sh

opam install llvm.3.4 ocamlfind
```

------------------------------
### Installation under macOS

You are welcome to try to install this under macOS. We were not able to run this on Apple laptops due to clang-3.4 not being available.

The following are outdated instructions for macOS.

1. Install Homebrew:

   `ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"`

2. Verify Homebrew is installed correctly:

   `brew doctor`

3. Install opam:

   `brew install opam`

4. Set up opam:

   `opam init`

5. Install llvm:

   `brew install llvm`

   Take note of where brew places the llvm executables. It will show
   you the path to them under the CAVEATS section of the post-install
   terminal output. For me, they were in /usr/local/opt/llvm/bin. Also
   take note of the llvm version installed. For me, it was 3.6.2.

6. Have opam set up your enviroment:

   `eval 'opam config env'`

7. Install the OCaml llvm library:

   `opam install llvm.3.6`

   Ensure that the version of llvm you install here matches the
   version you installed via brew. Brew installed llvm version 3.6.2,
   so I install llvm.3.6 with opam.

   IF YOU HAVE PROBLEMS ON THIS STEP, it's probably because you are
   missing some external dependencies. Ensure that libffi is installed
   on your machine. It can be installed with

   `brew install libffi`

   If, after this, opam install llvm.3.6 is still not working, try
   running

   `opam list --external --required-by=llvm.3.6`

   This will list all of the external dependencies required by
   llvm.3.6. Install all the dependencies listed by this command.

   IF THE PREVIOUS STEPS DO NOT SOLVE THE ISSUE, it may be a problem
   with using your system's default version of llvm. Install a
   different version of llvm and opam install llvm with that version
   by running:
```
   brew install homebrew/versions/llvm37
   opam install llvm.3.7
```
   Where the number at the end of both commands is a version different 
   from the one your system currently has.

8. Create a symbolic link to the lli command:

   sudo ln -s /usr/local/opt/llvm/bin/lli /usr/bin/lli

   Create the symlink from wherever brew installs the llvm executables
   and place it in your bin. From step 5, I know that brew installed
   the lli executable in the folder, /usr/local/opt/llvm/bin/, so this
   is where I symlink to. Brew might install the lli executables in a
   different location for you, so make sure you symlink to the right
   directory.

   IF YOU GET OPERATION NOT PERMITTED ERROR, then this is probably a
   result of OSX's System Integrity Protection. 

   One way to get around this is to reboot your machine into recovery
   mode (by holding cmd-r when restarting). Open a terminal from
   recovery mode by going to Utilities -> Terminal, and enter the
   following commands:
```
   csrutil disable
   reboot
```
   After your machine has restarted, try the `ln....` command again,
   and it should succeed.

   IMPORTANT: the prevous step disables System Integrity Protection,
   which can leave your machine vulnerable. It's highly advisable to
   reenable System Integrity Protection when you are done by 
   rebooting your machine into recovery mode and entering the following
   command in the terminal:
```
   csrutil enable
   reboot
```
   Another solution is to update your path, e.g.,
```
   export PATH=$PATH:/usr/local/opt/llvm/bin
```
   A third solution is to modify the definition of LLI in testall.sh to
   point to the absolute path, e.g., `LLI="/usr/local/opt/llvm/bin/lli"`

9. To run and test, navigate to the Harmonica folder. Once there, run

   `make ; ./testall.sh`

   Harmonica should build without any complaints and all tests should
   pass.

   IF RUNNING './testall.sh' FAILS ON SOME TESTS, check to make sure you
   have symlinked the correct executable from your llvm installation.
   For example, if the executable is named lli-[version], then the 
   previous step should have looked something like:

   `sudo ln -s /usr/local/opt/llvm/bin/lli-3.7 /usr/bin/lli   `

   As before, you may also modify the path to lli in testall.sh

