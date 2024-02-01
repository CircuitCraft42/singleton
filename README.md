# Singleton for Haskell

This library is a prototype implementation for my top-level mutable state proposal.
It introduces `Singleton`s, which are types that have a single canonical value that persists for the lifetype of the program.
My declaring a `newtype` that wraps an `IORef` or `MVar`, you get a unique mutable cell which works perfectly for top-level mutable state.
