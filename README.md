# tmhelp

<!-- badges: start -->

<!-- badges: end -->

A focused chat app for quick, authoritative answers from tidymodels
documentation.

`tmhelp` launches an interactive chat interface that finds and
summarizes relevant excerpts from tidymodels documentation using both
semantic and keyword-based search methods (via
[ragnar](https://github.com/tidyverse/ragnar)). Chat responses include
links to official sources and are designed to help users quickly
accomplish specific tidymodels tasks.

## Screenshot

![Screenshot of tmhelp app](man/figures/app-screenshot.png)

## Usage

``` r
# Launch with a blank chat
tmhelp::ask()

# Start with a specific question
tmhelp::ask("How do I make a two column layout?")
```

## Installation

``` r
remotes::install_github("topepo/tmhelp")
```

## Requirements

-   An OpenAI API key must be set in `Sys.getenv("OPENAI_API_KEY")`.
-   A typical question costs approximately 1 to 2 cents with the current
    flagship `gpt-4.1` model.
