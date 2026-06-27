
<!-- README.md is generated from README.Rmd. Please edit that file -->

# easieRnmt <img src="man/figures/logo.png" align="right" height="120" alt="" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/thieled/easieRnmt/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/thieled/easieRnmt/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/thieled/easieRnmt/graph/badge.svg)](https://app.codecov.io/gh/thieled/easieRnmt)
<!-- badges: end -->

The goal of easieRnmt is to provide a user-friendly R wrapper around the
[`'EasyNMT'`](https://github.com/UKPLab/EasyNMT) python library, which
provides “Easy to use, state-of-the-art Neural Machine Translation for
100+ languages” - on a local machine.

## Installation

You can install the development version of easieRnmt from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("thieled/easieRnmt")
```

From version 0.0.3 onwards, the package’s python backend that runs the
`'EasyNMT'` library is managed by `'uv'` via `'reticulate'`.[^1] This
function initializes this backend, automatically installs the correct
pytorch version - supporting CUDA (Nvidia GPU) integration if this is
available on your machine. It also includes a workaround of the
`'fasttext'` dependency conflict which is occuring on Windows machines.

``` r
easieRnmt::initialize_easynmt()
```

Note that the package requires a C++ compiler (e.g. g++). If you are a
Windows user, please make sure to install a RTools version that matches
your R version, from
[here](https://cran.r-project.org/bin/windows/Rtools/).

## Example

The package easieRnmt completely takes care of preprocessing your text
data - from sentence tokenization, careful cleaning, emoji-replacement,
language detection, and handling ambiguous cases.

To avoid compatibility conflicts with the fasttext python library in
Windows, it uses the fastText R package for language detection.

It supports efficient batch-processing, and takes care that only
language-homogeneous batches are processed – as the models assume that
languages is consistent within batches.

Finally, it glues all translated sentences back together to the input
format, sorts the translations as the input, and returns either a
data.table (including the cleaned text and additional information) or
the string only.

``` r

# Minimal example
sentences = c('Dies ist ein Satz in Deutsch. Und noch ein Satz.',   # This is a German sentence
              'Esta es una oración en español.', # This is a Spanish sentence
              "هذه جملة باللغة العربية!!!")       # This is an Arabic sentence

library(easieRnmt)

# Translate
res <- easieRnmt::translate(sentences,
                     model = 'opus-mt',
                     targ_lang = "en",
                     return_string = T)
#> Running fastText language detection...
#> Tokenizing texts after language detection...
#> Tokenizing texts into sentences and chunks...
#> Processing language: ar
#> Processing language: de
#> Processing language: es
# Print results
print(res)
#> [1] "This is a sentence in German. And another sentence."
#> [2] "This is a sentence in Spanish."                     
#> [3] "That's a sentence in Arabic!"
```

[^1]: Thanks to [JBGruber](https://github.com/JBGruber), who provided
    this python backend setup for my other package
    [sentiner](https://github.com/thieled/sentiner).
