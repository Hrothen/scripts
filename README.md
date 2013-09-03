This repo is a collection of haskell scripts for everyday use. It grows as I think of things to do.

## ginit.hs

Creates a repo with a dummy README.md, a copy of the MIT license, and a basic .gitignore. Takes any arguments and passes them to git init.

## mktwit.hs

Takes a file and cuts it into phrases of at most 140 characters without cutting any words up. Useful for tweeting entire books at people you hate.

## convertPNG.hs

When finished, this script will convert one or more png files into csv files suitable for use with [Quickfort](https://github.com/joelpt/quickfort). Uses a user modifiable config file to pick colors.