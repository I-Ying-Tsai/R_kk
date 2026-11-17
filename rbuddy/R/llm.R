# ---------------------------------------------------------------------------
# LLM backends: Claude (Anthropic Messages API), Ollama, OpenAI-compatible.
# All calls are plain HTTP via httr2 (R has no official Anthropic SDK).
# ---------------------------------------------------------------------------

anthropic_models <- c("claude-opus-5-5", "claude-sonnet-5-5", "claude-haiku-5-5",
                      "claude-fable-5-2")

# Models that accept the server-side refusal fallback (`fallbacks: "default"`).
fallback_models <- c("claude-opus-5-5", "claude-opus-5", "claude-fable-5-2",
                     "claude-fable-5-1", "claude-sonnet-5-5")

to_json <- function(x) {
  jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null", digits = NA)
}

http_post <- function(url, body, headers = list(), timeout = 900) {
  req <- httr2::request(url)
  req <- httr2::req_headers(req, !!!headers, `content-type` = "application/json")
  req <- httr2::req_body_raw(req, to_json(body), type = "application/json")
  req <- httr2::req_timeout(req, timeout)
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  req <- httr2::req_retry(req, max_tries = 3,
    is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 500, 502, 503, 529))
  httr2::req_perform(req)
}

api_error <- function(resp, who) {
  status <- httr2::resp_status(resp)
  detail <- tryCatch({
    b <- httr2::resp_body_json(resp)
    if (is.null(names(b)) && length(b)) b <- b[[1]]   # Gemini wraps errors in a list
    b$error$message %||% b$error %||% b$message %||% ""
  }, error = function(e) tryCatch(httr2::resp_body_string(resp), error = function(e) ""))
  stop(sprintf("%s 錯誤 (HTTP %d)：%s", who, status, paste(detail, collapse = " ")),
       call. = FALSE)
}

#' Call the configured model.
#' @param messages list of list(role, content). For Anthropic, assistant
#'   `content` may be the raw content-block list returned earlier.
#' @return list(text, raw, model, note)
llm_call <- function(cfg, system, messages, max_tokens = 16000, effort = NULL,
                     json = FALSE) {
  res <- switch(cfg$backend,
    anthropic = call_anthropic(cfg, system, messages, max_tokens,
                               effort %||% cfg$effort),
    ollama = call_ollama(cfg, system, messages, json),
    openai = call_openai(cfg, system, messages, max_tokens),
    gemini = call_openai(gemini_cfg(cfg), system, messages, max_tokens, who = "Gemini"),
    stop("未知的 backend: ", cfg$backend, call. = FALSE)
  )
  # Reasoning models (deepseek-r1, qwen3, ...) inline their thoughts; hide them.
  res$text <- trimws(gsub("<think>[\\s\\S]*?</think>", "", res$text %||% "", perl = TRUE))
  # Small models sometimes wrap the reply in stray <proof>/<answer> tags.
  res$text <- trimws(gsub("</?(proof|answer)>", "", res$text, perl = TRUE))
  res
}

call_anthropic <- function(cfg, system, messages, max_tokens, effort) {
  key <- Sys.getenv("ANTHROPIC_API_KEY")
  if (!nzchar(key)) {
    stop("找不到 ANTHROPIC_API_KEY。請在 ~/.Renviron 加入一行 ANTHROPIC_API_KEY=你的key ",
         "然後重開 R；或在 ⚙ 設定裡貼上（只存在記憶體）。", call. = FALSE)
  }
  model <- cfg$anthropic_model
  body <- list(
    model = model,
    max_tokens = max_tokens,
    system = system,
    messages = messages,
    output_config = list(effort = effort),
    cache_control = list(type = "ephemeral")
  )
  headers <- list(`x-api-key` = key, `anthropic-version` = "2026-10-21")
  if (model %in% fallback_models) {
    # If a safety classifier declines, the API re-runs on a fallback model.
    body$fallbacks <- "default"
    headers$`anthropic-beta` <- "server-side-fallback-2026-07-01"
  }
  resp <- http_post("https://api.anthropic.com/v1/messages", body, headers)
  if (httr2::resp_status(resp) >= 400) api_error(resp, "Anthropic API")
  out <- httr2::resp_body_json(resp, simplifyVector = FALSE)

  texts <- vapply(Filter(function(b) identical(b$type, "text"), out$content),
                  function(b) b$text %||% "", "")
  note <- NULL
  if (identical(out$stop_reason, "refusal")) {
    note <- "模型婉拒了這個請求。"
  } else if (identical(out$stop_reason, "max_tokens")) {
    note <- "回答因長度上限被截斷。"
  }
  list(text = paste(texts, collapse = ""), raw = out$content,
       model = out$model %||% model, note = note)
}

call_ollama <- function(cfg, system, messages, json = FALSE) {
  if (!nzchar(cfg$ollama_model %||% "")) {
    stop("請先在 ⚙ 設定裡選一個 Ollama 模型（例如先在終端機執行 `ollama pull qwen3.5:9b`）。",
         call. = FALSE)
  }
  msgs <- c(list(list(role = "system", content = system)), messages)
  body <- list(model = cfg$ollama_model, messages = msgs, stream = FALSE,
               options = list(num_ctx = as.integer(cfg$num_ctx %||% 8192)))
  if (json) {
    body$format <- "json"
    body$think <- FALSE   # quick suggestions don't need the model's thinking pass
  } else {
    body$think <- !isFALSE(cfg$ollama_think)
  }
  url <- paste0(sub("/+$", "", cfg$ollama_url), "/api/chat")
  post <- function(body) tryCatch(http_post(url, body),
    error = function(e) stop("連不到 Ollama（", cfg$ollama_url, "）。請確認已執行 `ollama serve`。",
                             call. = FALSE))
  resp <- post(body)
  if (httr2::resp_status(resp) >= 400 && !is.null(body$think)) {
    body$think <- NULL    # models without a thinking mode reject the field
    resp <- post(body)
  }
  if (httr2::resp_status(resp) >= 400) api_error(resp, "Ollama")
  out <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  text <- out$message$content %||% ""
  note <- NULL
  # A long thinking pass can use up the whole context window before the model
  # writes its answer, leaving the reply empty. Ask again without thinking.
  if (!nzchar(trimws(text)) && !json && isTRUE(body$think)) {
    body$think <- FALSE
    resp <- post(body)
    if (httr2::resp_status(resp) >= 400) api_error(resp, "Ollama")
    out <- httr2::resp_body_json(resp, simplifyVector = FALSE)
    text <- out$message$content %||% ""
    note <- "模型思考太久、沒寫出答案，已改用不思考模式重答（品質可能較差）。"
  }
  if (identical(out$done_reason, "length")) {
    note <- c(note, "回答因長度上限被截斷：可以按「新對話」，或在 ⚙ 調大 context 長度。")
  }
  list(text = text, raw = NULL, model = cfg$ollama_model,
       note = if (length(note)) paste(note, collapse = " ") else NULL)
}

gemini_base <- "https://generativelanguage.googleapis.com/v1beta/openai"

# Gemini speaks the OpenAI chat-completions protocol at gemini_base.
gemini_cfg <- function(cfg) {
  key <- Sys.getenv("GEMINI_API_KEY")
  if (!nzchar(key)) {
    stop("找不到 Gemini API key。請到 Google AI Studio（aistudio.google.com）建立 key，",
         "在 ~/.Renviron 加入 GEMINI_API_KEY=你的key 後重開 R；或在 ⚙ 設定裡貼上（只存在記憶體）。",
         call. = FALSE)
  }
  modifyList(cfg, list(openai_url = gemini_base, openai_model = cfg$gemini_model %||% "",
                       openai_key = key))
}

call_openai <- function(cfg, system, messages, max_tokens, who = "OpenAI 相容伺服器") {
  if (!nzchar(cfg$openai_model %||% "")) {
    stop("請先在 ⚙ 設定裡選一個模型（可以按「重新抓模型清單」）。", call. = FALSE)
  }
  msgs <- c(list(list(role = "system", content = system)), messages)
  body <- list(model = cfg$openai_model, messages = msgs, max_tokens = max_tokens)
  key <- cfg$openai_key %||% Sys.getenv("RBUDDY_OPENAI_KEY")
  headers <- if (nzchar(key)) list(Authorization = paste("Bearer", key)) else list()
  url <- paste0(sub("/+$", "", cfg$openai_url), "/chat/completions")
  resp <- tryCatch(http_post(url, body, headers),
    error = function(e) stop("連不到 ", cfg$openai_url, "：", conditionMessage(e), call. = FALSE))
  if (httr2::resp_status(resp) >= 400) api_error(resp, who)
  out <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  choice <- out$choices[[1]]
  note <- if (identical(choice$finish_reason, "length")) "回答因長度上限被截斷。" else NULL
  list(text = choice$message$content %||% "", raw = NULL,
       model = cfg$openai_model, note = note)
}

list_local_models <- function(cfg) {
  tryCatch({
    if (cfg$backend == "ollama") {
      url <- paste0(sub("/+$", "", cfg$ollama_url), "/api/tags")
      out <- httr2::resp_body_json(httr2::req_perform(httr2::req_timeout(httr2::request(url), 5)))
      vapply(out$models, function(m) m$name, "")
    } else if (cfg$backend %in% c("openai", "gemini")) {
      if (cfg$backend == "gemini") cfg <- gemini_cfg(cfg)
      url <- paste0(sub("/+$", "", cfg$openai_url), "/models")
      req <- httr2::req_timeout(httr2::request(url), 5)
      key <- cfg$openai_key %||% Sys.getenv("RBUDDY_OPENAI_KEY")
      if (nzchar(key)) req <- httr2::req_headers(req, Authorization = paste("Bearer", key))
      out <- httr2::resp_body_json(httr2::req_perform(req))
      ids <- sub("^models/", "", vapply(out$data, function(m) m$id, ""))
      if (cfg$backend == "gemini") {
        ids <- ids[grepl("^gemini", ids) &
                   !grepl("embedding|tts|image|live|audio|native", ids)]
        ids <- sort(ids)
      }
      ids
    } else character()
  }, error = function(e) character())
}

# Pull the first {...} object out of a reply and parse it.
parse_json_reply <- function(text) {
  s <- regexpr("\\{", text)
  e <- max(gregexpr("\\}", text)[[1]])
  if (s < 0 || e < s) stop("模型沒有回傳 JSON。", call. = FALSE)
  jsonlite::fromJSON(substr(text, s, e), simplifyVector = FALSE)
}
