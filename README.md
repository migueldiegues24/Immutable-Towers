# Immutable Towers 🏰

## ⚙️ Executable

You can compile and run the program using Cabal’s `build` and `run` commands:

```bash
cabal run --verbose=0
```

## 💻 Interpreter

To open the Haskell interpreter (GHCi) with the project loaded, use Cabal’s `repl` command:

```bash
cabal repl
```

## 🧪 Tests

The project uses the [HUnit](https://hackage.haskell.org/package/HUnit) library for unit testing.  

Run the tests with Cabal’s `test` command, and use the `--enable-coverage` flag to generate a coverage report:

```bash
cabal test --enable-coverage
```

You can also run the documentation examples as tests using the [doctest](https://hackage.haskell.org/package/doctest) library.  
To install the executable, use the following command:

```bash
cabal install doctest
```

Then run doctest with:

```bash
cabal repl --build-depends=QuickCheck,doctest --with-ghc=doctest --verbose=0
```

## 📖 Documentation

The project documentation can be generated using [Haddock](https://haskell-haddock.readthedocs.io/):

```bash
cabal haddock
```
