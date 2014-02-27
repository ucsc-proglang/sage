The Sage Programming Language
=============================

http://sage.soe.ucsc.edu/ 
https://github.com/ucsc-proglang/sage

A prototype implementation of a language with:

 - Executable refinement types
 - First-class types (aka full dependent types a la Cayenne)
 - Dynamic types (aka gradual typing)

with an implementation supported by:

 - An off-the-shelf theorem prover (in this case, hard-coded to Simplify)
 - A compile-time interpreter
 - A database of run-time failures (every run-time failure occurs "at most once")

Disclaimer: This software is a very rough prototype! It is provided for
scientific and pedagogical purposes.

*Note*: The Simplify binaries included here are not part of the Sage materials.
They are distributed for your convenience under the Java Programming Toolkit software license, 
available at http://www.hpl.hp.com/downloads/crl/jtk/agreement.html


Building and "installing"
-------------------------

Try this

```bash
    $ make
```

If all goes well, you should have a sage executable.

Two copies of the binary executable of the Simplify theorem prover are
included. For `sage` to run correctly (without the `-nosimplify` flag), an
executable named `Simplify` must exist.

On Linux, the following should work:

```bash
  $ ln -s Simplify-1.5.4.linux Simplify
```

On Windows, this is better:

```
  cp Simplify-1.5.4.exe.win Simplify.exe
```


Usage
-----

You can experiment with Sage via its interactive toplevel by running `sage` with no arguments.

```ocaml
$ ./sage
creating new database default.db.
     Welcome to Sage

# let x = 3;;
Binding for: x
Type: (Refine Int (fn (x:Int) => (inteq x 3)))
Evaluation: 3

# let add_one (x:Int) = x + 1;;
Binding for: add_one
Type: (x:Int -> (Refine Int (fn (z:Int) => (inteq z (add x 1)))))
Evaluation: (fn (x:Int) => (add x 1))

# add_one 3;;
Type: (Refine Int (fn (z:Int) => (inteq z (add 3 1))))
Evaluation: 4
```

The fun never ends.

To type check and run a file of sage code, use `sage <filename>`.
A number of example programs are in the `tests` subdirectory.
For example, try `sage tests/polylist.f`

Running `sage -help` shows the command-line options, many of which are
are for debugging purposes.


Other interesting projects
--------------------------

If you are interested in exciting modern developments in refinement types, be sure to check these other projects out!

 - [Liquid Haskell](https://github.com/ucsd-progsys/liquidhaskell) adds refinement types to Haskell.
 - [F7](http://research.microsoft.com/en-us/projects/f7/) adds refinement types to F#.
 - [Ynot](http://ynot.cs.harvard.edu/) adds stateful programming capabilities to Coq.
 - [HCC](http://pauillac.inria.fr/~naxu/research/hcc.html) adds hybrid contract checking to OCaml.

A bit more tangential, but still related, you may also enjoy following the progress of dependently typed
programming via
[Agda](http://wiki.portal.chalmers.se/agda/pmwiki.php),
[Epigram](http://code.google.com/p/epigram/),
[Coq](http://coq.inria.fr/),
[Ur](http://www.impredicative.com/ur/),
etc.


Acknowledgments
---------------

The example code from Benjamin Pierce's _Types and Programming Languages_ was used with permission as a starting
point for this prototype.

