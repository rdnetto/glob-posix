# Glob-Posix
[![Build Status](https://travis-ci.org/rdnetto/glob-posix.svg?branch=master)](https://travis-ci.org/rdnetto/glob-posix)

Glob-Posix is a Haskell library that provides bindings for the [glob(3)](https://linux.die.net/man/3/glob) C function on POSIX systems. It is significantly faster than the available alternatives at time of writing. GNU extensions are also supported.

## Benchmarks
Glob-Posix isn't just fast, it's orders of magitude faster than the competing implementations:

[![Complete benchmark](https://rdnetto.github.io/glob-posix/bench-1.png)](https://rdnetto.github.io/glob-posix/bench-1.html)
(Click on the graph for an interactive version.)

You can find the source for these benchmarks [here](https://github.com/rdnetto/glob-posix/blob/master/bench/Bench.hs). To run the benchmarks, use:

    stack bench --benchmark-arguments '--output=bench.html'

## Reasons to use this library
* You need a fast, powerful implementation of `glob(3)` on a POSIX OS

## Reasons to not use this library
* You need to write programs portable to non-POSIX operating systems. e.g. Windows
* You need support for virtual file-systems - see [System.IO.HVFS](https://hackage.haskell.org/package/MissingH/docs/System-IO-HVFS.html).

