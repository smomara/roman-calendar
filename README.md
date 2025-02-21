# Roman Calendar

A Haskell library for computing liturgical celebrations and
season in the Roman Catholic calendar.

## Installation

Clone the repository:

```sh
git clone https://github.com/smomara/roman-calendar.git
cd roman-calendar
```

### With Nix

```sh
nix build
```

### With Cabal

```sh
cabal build
```

## Usage

The library provides a simple interface to compute liturgical information
for a given date. Here's an example:

```haskell
import Data.Time.Calendar (fromGregorian)
import Data.Time.Calendar.RomanCatholic

main :: IO ()
main = do
    let date = fromGregorian 2025 4 20 -- Easter Day
    print $ celebrationInfo date
```

This will output the celebration information for Easter Day,
including the liturgical season, color, and any optional memorials.

## Development

Open a Nix development shell, which includes `ghc`, `cabal`, `hls`,
and the necessary packages:

```sh
nix develop
```

Contributions are welcome! Submit a PR.
(I'm sure I messed up and/or missed a few things - the norms can be confusing).
