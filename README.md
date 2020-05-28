# Bankroll: A COIN wrapper

Bankroll is a wrapper for COIN-OR CLP.

It also comes with a simple executable frontend.

## Building

Bankroll depends on Stack and the Haskell platform, and CLP.

On Ubuntu, these can be installed with:

    apt-get install haskell-stack
    apt-get install coinor-clp coinor-libclp-dev
    apt-get install coinor-cbc coinor-libcbc-dev

Then to build, just run:

    stack build

Ratl can then be run:

    stack exec bankroll examples/lemonade.mps

