# cl-transit: Transit format for Common Lisp

*À tout Seigneur tout Honneur !*

## About Transit 

The Transit format has been introduced by [Cognitect](https://www.cognitect.com/) and defined [here](https://github.com/cognitect/transit-format).

## Runing tests.

The library is able to process all the tests of the official [test-suite](https://github.com/cognitect/transit-format/blob/master/examples/0.8/simple/README.md). It has been developped and tested with [SBCL](https://www.sbcl.org/) but should work on other CL implementations.  

Prerequesites:

* install SBCL (`homebrew`, `apt-get` ...)
* install [quicklisp](https://www.quicklisp.org/beta/)
* untill [this commit](https://github.com/Zulu-Inuoe/jzon/commit/d6428d6602752d44d5b08e9c0a51d31f92aee2ab)  has made it to quicklisp (monthly), you need manually clone JZON to your local quicklisp directory e.g.,

```shell
$ mkdir -p ~/quicklisp/local-projects
$ cd ~/quicklisp/local-projects
$ git clone https://github.com/Zulu-Inuoe/jzon
```
then

* clone this repository
* clone [Transit test-suite](https://github.com/cognitect/transit-format.git) next to it
* run `make test`

```
sbcl --non-interactive \
		--load run-tests.lisp
This is SBCL 2.3.4, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
To load "cl-transit-tests":
  Load 1 ASDF system:
    cl-transit-tests
; Loading "cl-transit-tests"
..................................................
[package cl-transit-tests]..

Running test suite TEST-EXAMPLAR
 Running test JSON<->MP .................................................................
 Running test JSON<->VERBOSE .................................................................
 Running test DECODE-MARSHALABLE ................................................................................................................
 Running test ROUND-TRIP-MARSHALABE ................................................................................................................
 Running test ONE-URI .....
 Running test URIS .....
 Running test ONE-UUID .....
 Running test UUIDS .....
 Running test ONE-DATE .....
 Running test SET-EMPTY .....
 Running test SET-SIMPLE .....
 Running test SET-NESTED .....
 Running test SET-MIXED .....
 Running test MAPS-UNROCOGNIZED-KEYS .....
 Did 404 checks.
    Pass: 404 (100%)
    Skip: 0 ( 0%)
    Fail: 0 ( 0%)
```

## Default Type Mapping

`clt` is short for `cl-transit` and `lt` short for [local-time](https://github.com/dlowe-net/local-time)

|Transit type|Write accepts|Read returns|
|------------|-------------|------------|
|null        |'null        |'null       |
|string      |string       |string      |
|boolean     |boolean      |boolean     |
|integer     |fixnum       |integer      |
|decimal     |float        |float
|keyword     |keyword      |keyword     |
|symbol      |symbol       |symbol      |
|big integer |integer      |integer      |
|big decimal |long-float   |long-float   |
|time        |lt:timestamp    |lt:timestamp or clt:tr-timestamp|
|uri         |quri:uri        |quri:uri            |
|uuid        |uuid:uuid       |uuid:uuid           |
|char        |character    |character           |
|array       |vector       |vector              |
|list        |list         |list            |
|set         |fset:set or clt:tr-set         |clt:tr-set          |
|map         |hash-table   |hash-table       |
|link        |clt:tr-link        |clt:tr-link|
|ratio       |ratio        |ratio               |


## Contributing

Happy to take contributions, and any kind of improvments (performance, coding style ...)

## Copyright and License

Copyright © 2023 Jan Sulmont

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

