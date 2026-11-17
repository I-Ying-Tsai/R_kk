# R_kk

[`rbuddy/`](rbuddy/) — R Buddy, an AI teaching-assistant add-in for RStudio. While you write problems, it suggests analyses, answers questions, and can insert R code or proofs into your script. It works with local Ollama models, Gemini, or Claude.

Install:

```r
install.packages(c("shiny", "httr2", "jsonlite", "rstudioapi", "callr", "later", "commonmark", "httpuv", "remotes"))
remotes::install_github("I-Ying-Tsai/R_kk", subdir = "rbuddy")
```

See [rbuddy/README.md](rbuddy/README.md) for details.
