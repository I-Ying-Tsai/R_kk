# Settings are remembered between sessions (API keys are never written to disk).

config_path <- function() file.path(tools::R_user_dir("rbuddy", "config"), "config.json")

default_config <- function() {
  list(
    backend = if (nzchar(Sys.getenv("ANTHROPIC_API_KEY"))) "anthropic" else "ollama",
    anthropic_model = "claude-opus-5-5",
    effort = "medium",
    suggest_model = "",          # "" = same model as chat
    ollama_url = "http://localhost:11434",
    ollama_model = "",
    num_ctx = 16384,
    ollama_think = TRUE,
    gemini_model = "",
    openai_url = "http://localhost:1234/v1",
    openai_model = "",
    auto_suggest = TRUE,
    auto_insert = TRUE,
    code_comments = FALSE,       # R code without comments
    term_model = "",             # small Ollama model for the 名詞／翻譯 button; "" = main brain
    language = "繁體中文"
  )
}

load_config <- function() {
  cfg <- default_config()
  p <- config_path()
  if (file.exists(p)) {
    saved <- tryCatch(jsonlite::read_json(p, simplifyVector = TRUE), error = function(e) list())
    saved <- saved[intersect(names(saved), names(cfg))]
    cfg[names(saved)] <- saved
  }
  cfg
}

save_config <- function(cfg) {
  p <- config_path()
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  cfg <- cfg[intersect(names(cfg), names(default_config()))]
  try(writeLines(jsonlite::toJSON(cfg, auto_unbox = TRUE, pretty = TRUE), p), silent = TRUE)
}
