# Pact-Time Library

A minimal time library for usage with the [Pact Smart Contract
Language](https://github.com/kadena-io/pact/).

The focus of this library is on minimality, performance, and binary level
stability. Time is represented as 64-bit integral value that counts
micro-seconds since the modified Julian date epoch (MJD). The implementation
ignores leap seconds.

While the library can parse date-time values with time zones, internally all
date-times are represented as UTC and formatting only supports UTC. Only
the default English language locale is supported.

Detail about supported formats can be found in the [Pact Language
Reference](https://pact-language.readthedocs.io/en/stable/pact-reference.html#time-formats).

