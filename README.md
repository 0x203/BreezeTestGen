# BreezeTestGen

[![Build Status](https://travis-ci.org/0x203/BreezeTestGen.svg?branch=master)](https://travis-ci.org/0x203/BreezeTestGen)

Generator of Verification Tests for Breeze Netlists.

This project is written in [Scala](http://www.scala-lang.org/) Version 2.11.8.


Usage
----

### First Installation

You'll need Java 8 and [SBT](http://www.scala-sbt.org/).

1. Clone this repository and `cd` into it.
2. Run `sbt "run --help"` for installation and usage information.

### Example

Here is a example for generating tests for a circuit in the `.breeze` format. Generated tests will be stored into the specified file `generated_tests.json`: 

    sbt "run ./path/to/circuit.breeze generated_test.json"

### Configuration

See `src/main/resources/reference.conf` as reference for configurable values.
Create an `application.conf` in the same location with the same format for your own configuration or use Java system properties.

For further information about the format consult the documentation of [typesafe/config](https://github.com/typesafehub/config).
