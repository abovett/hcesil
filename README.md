hcesil
======

hcesil is a interpreter for the CESIL educational low level programming language, written in Haskell. It is a CLI program (i.e. runs in the terminal).

Build instructions
------------------

Assuming you have [ghc](https://www.haskell.org/ghc/) (the  Glasgow Haskell Compiler) installed:

```
ghc -O2 hcesil.hs
```
(The -O2 is optional but will result in a significant speedup with large CESIL programs like maze.cesil)

Usage
-----
```
hcesil [OPTIONS] FILE
Execute CESIL program FILE.

OPTIONS are:
  -c, --countsteps   Count the steps executed and report at program completion.
  -h, --help         Display this help text and exit.
  -m MAX, --maxsteps MAX
                     Halt the program after MAX steps have been executed.
  -t, --trace        Display instructions and status as the program runs.
  -v, --version      Display the program version and exit.
```

Compatibility
-------------

hcesil has been tested under Debian GNU/Linux, but should work on any platform with a terminal and a Haskell compiler.

CESIL language specification
----------------------------

You will find more information about CESIL here: https://en.wikipedia.org/wiki/CESIL . There is also a description of a slightly different dialect, as implemented by the "Visual CESIL" implementation, here:  https://web.archive.org/web/20210912192137/http://www.obelisk.me.uk/cesil/

The main differences between Visual CESIL the dialect and the "standard" one described on the WikiPedia page are:
- Comments start with `*` not `(`
- `*` is not required at the end of the file
- Positive numbers and zero do not need to be signed

hcesil versions up to V1.2.0 support the Visual CESIL dialect. Version 1.3.0 and upwards support either variant.


Example CESIL programs
----------------------

The cesil subdirectory contains some sample CESIL progams which can be used to test and demonstrate hcesil.
