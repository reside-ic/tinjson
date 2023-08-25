# tinyjson

<!-- badges: start -->
[![Project Status: Concept – Minimal or no implementation has been done yet, or the repository is only intended to be a limited example, demo, or proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
[![R build status](https://github.com/reside-ic/tinyjson/workflows/R-CMD-check/badge.svg)](https://github.com/reside-ic/tinyjson/actions)
[![codecov.io](https://codecov.io/github/reside-ic/tinyjson/coverage.svg?branch=main)](https://codecov.io/github/reside-ic/tinyjson?branch=main)
<!-- badges: end -->

This package solves a very specific goal - read json into R with almost no deserialisation, into some intermediate format that does not suffer from the information loss typically encountered with arrays (are they scalar or are they a vector of length 1?).

Unlike `jsonlite::serialzeJSON` which aims for a lossless `R -> JSON -> R` roundtrip, we aim for a lossless `JSON -> R -> JSON` roundtrip, so that R can safely ammend existing JSON that might be consumed by other applications without breaking it.

The package is not intended to be fast (it is written in pure R) nor provide good errors (use jsonlite first to ensure the json is valid) nor provide good output formatting (use jsonlite's pretty formatter).

## Installation

To install `tinyjson`:

```r
remotes::install_github("reside-ic/tinyjson", upgrade = FALSE)
```

## License

MIT © Imperial College of Science, Technology and Medicine
