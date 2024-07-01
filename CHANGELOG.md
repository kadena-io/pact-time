# Revision history for pact-time

## 0.3.0.1 -- 2024-07-01

*   Update documentation links to the Pact language specification.

## 0.3.0.0 -- 2024-06-30

Breaking changes:

*   Replaced dependency on the vector-spaces package by internal public
    `numeric` library.

    Most notably class method `zeroV` is now just called `zero`. Otherwise, most
    numeric operators remain identical and are now (re-)exported by the main
    module `Pact.Time`.

Other changes:

*   Support for builds with GHC-9.10.1.

## 0.2.0.2 -- 2023-05-11

*   Support mtl-2.3

## 0.2.0.1 -- 2022-10-25

*   Relax upper bound on base
*   Support GHC versions 9.2 and 9.4

## 0.2.0.0 -- 2021-05-07

*   Move all modules from `Data.Time` to `Pact.Time`.

## 0.1.0.0 -- 2021-05-06

*   First version. Released on an unsuspecting world.
