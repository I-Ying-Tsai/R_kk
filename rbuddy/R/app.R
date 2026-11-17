# ---------------------------------------------------------------------------
# The Shiny app shown in RStudio's Viewer pane. Runs in a background R process.
# ---------------------------------------------------------------------------

run_app <- function(dir, port) {
  shiny::runApp(rbuddy_app(dir), port = port, host = "127.0.0.1",
                launch.browser = FALSE, quiet = TRUE)
}

rbuddy_app <- function(dir) {
  shiny::addResourcePath("rbuddy", system.file("www", package = "rbuddy"))
  shiny::shinyApp(ui = app_ui, server = function(input, output, session) {
    app_server(input, output, session, dir)
  })
}

# ---- UI --------------------------------------------------------------------

app_ui <- function(req) {
  cfg <- load_config()
  tags <- shiny::tags
  shiny::fluidPage(
    title = "R Buddy",
    tags$head(
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$link(rel = "stylesheet", href = "rbuddy/rbuddy.css"),
      tags$script(src = "rbuddy/rbuddy.js"),
      tags$script(src = "https://cdn.jsdelivr.net/npm/mathjax@3.2.2/es5/tex-mml-chtml.js",
                  async = NA, id = "MathJax-script")
    ),
    shiny::div(class = "rb-header",
      shiny::span(class = "rb-title", "R Buddy"),
      shiny::uiOutput("status", inline = TRUE),
      shiny::actionLink("toggle_settings", "⚙", class = "rb-gear", title = "設定")
    ),
    shiny::conditionalPanel("input.toggle_settings % 2 == 1", settings_ui(cfg)),
    shiny::div(class = "rb-card",
      shiny::div(class = "rb-card-h",
        shiny::span("💡 你可能需要"),
        shiny::actionLink("refresh_suggest", "重新分析", class = "rb-link")),
      shiny::uiOutput("suggestions")
    ),
    shiny::div(id = "chat_wrap", class = "rb-chat", shiny::uiOutput("chat")),
    shiny::div(class = "rb-input",
      tags$textarea(id = "question", rows = 3,
                    placeholder = "問問題…（Ctrl+Enter 送出）"),
      shiny::div(class = "rb-input-btns",
        tags$button(class = "btn btn-default btn-sm", onclick = "rbNew()", "新對話"),
        tags$button(class = "btn btn-default btn-sm", onclick = "rbSend('term_msg')",
                    title = "用小模型快速翻譯或解釋名詞，不寫程式、不做證明", "名詞／翻譯"),
        tags$button(class = "btn btn-primary btn-sm", onclick = "rbSend()", "送出")
      )
    )
  )
}

settings_ui <- function(cfg) {
  cp <- shiny::conditionalPanel
  shiny::div(class = "rb-settings",
    shiny::radioButtons("backend", "大腦", selected = cfg$backend, choices = c(
      "Claude（Anthropic API）" = "anthropic",
      "Ollama（本機開源模型）" = "ollama",
      "Gemini（Google，有免費額度，需要網路）" = "gemini",
      "OpenAI 相容伺服器（LM Studio、vLLM、llama.cpp…）" = "openai")),
    cp("input.backend == 'anthropic'",
      shiny::selectizeInput("anthropic_model", "模型", choices = unique(c(anthropic_models, cfg$anthropic_model)),
                            selected = cfg$anthropic_model, options = list(create = TRUE)),
      shiny::selectInput("effort", "思考深度（effort）", c("low", "medium", "high", "xhigh", "max"),
                         selected = cfg$effort),
      shiny::selectizeInput("suggest_model", "自動建議用的模型",
                            choices = c("跟聊天相同" = "", anthropic_models),
                            selected = cfg$suggest_model, options = list(create = TRUE)),
      shiny::passwordInput("anthropic_key", "API key（留空＝用環境變數 ANTHROPIC_API_KEY；不會存檔）")
    ),
    cp("input.backend == 'ollama'",
      shiny::textInput("ollama_url", "Ollama 網址", cfg$ollama_url),
      shiny::selectizeInput("ollama_model", "模型", choices = cfg$ollama_model,
                            selected = cfg$ollama_model, options = list(create = TRUE)),
      shiny::numericInput("num_ctx", "Context 長度（num_ctx；8 GB 顯卡建議 16384）", cfg$num_ctx,
                          min = 2048, step = 1024),
      shiny::checkboxInput("ollama_think", "先思考再回答（比較準，但比較慢）", cfg$ollama_think %||% TRUE)
    ),
    cp("input.backend == 'gemini'",
      shiny::selectizeInput("gemini_model", "模型（貼上 key 後按「重新抓模型清單」）",
                            choices = cfg$gemini_model, selected = cfg$gemini_model,
                            options = list(create = TRUE)),
      shiny::passwordInput("gemini_key", "API key（留空＝用環境變數 GEMINI_API_KEY；不會存檔）")
    ),
    cp("input.backend == 'openai'",
      shiny::textInput("openai_url", "Base URL", cfg$openai_url),
      shiny::selectizeInput("openai_model", "模型", choices = cfg$openai_model,
                            selected = cfg$openai_model, options = list(create = TRUE)),
      shiny::passwordInput("openai_key", "API key（選填，不會存檔）")
    ),
    cp("input.backend != 'anthropic'",
       shiny::actionLink("refresh_models", "↻ 重新抓模型清單", class = "rb-link")),
    shiny::textInput("term_model", "「名詞／翻譯」用的小模型（Ollama；留空＝用上面的大腦）", cfg$term_model),
    shiny::checkboxInput("auto_suggest", "打字停下來後自動建議分析", cfg$auto_suggest),
    shiny::checkboxInput("auto_insert", "回答後自動把程式碼／證明寫進腳本", cfg$auto_insert),
    shiny::checkboxInput("code_comments", "程式碼加註解（按「新對話」後生效）", cfg$code_comments %||% FALSE),
    shiny::textInput("language", "回答語言", cfg$language)
  )
}

# ---- server ----------------------------------------------------------------

app_server <- function(input, output, session, dir) {
  ctx_file <- file.path(dir, "context.json")
  inbox_file <- file.path(dir, "inbox.json")
  act_dir <- file.path(dir, "actions")
  isolate <- shiny::isolate

  state <- shiny::reactiveValues(
    msgs = list(),           # display + API history
    system = NULL,           # frozen per conversation
    last_sent = NULL,        # editor contents sent with the last message
    last_console = 0,        # id of the newest console entry already sent
    busy = FALSE,
    sugg = NULL, sugg_status = "在左邊的腳本寫下題目，停一下我就會提供建議。",
    sugg_busy = FALSE, sugg_for = NULL, sugg_console = 0,
    job = NULL, sugg_job = NULL  # running model calls (child processes)
  )

  # -- editor context written by the main R session --------------------------
  read_ctx <- function() {
    tryCatch(jsonlite::read_json(ctx_file, simplifyVector = TRUE),
             error = function(e) list(available = FALSE))
  }
  ctx <- shiny::reactivePoll(700, session,
    checkFunc = function() { fi <- file.info(ctx_file); paste(fi$size, fi$mtime) },
    valueFunc = read_ctx)

  # -- settings ---------------------------------------------------------------
  cfg <- shiny::reactive({
    shiny::req(input$backend)
    list(
      backend = input$backend, anthropic_model = input$anthropic_model,
      effort = input$effort, suggest_model = input$suggest_model %||% "",
      ollama_url = input$ollama_url, ollama_model = input$ollama_model %||% "",
      num_ctx = input$num_ctx %||% 16384,
      ollama_think = !isFALSE(input$ollama_think),
      gemini_model = input$gemini_model %||% "",
      openai_url = input$openai_url, openai_model = input$openai_model %||% "",
      auto_suggest = isTRUE(input$auto_suggest), auto_insert = isTRUE(input$auto_insert),
      code_comments = isTRUE(input$code_comments),
      term_model = trimws(input$term_model %||% ""),
      language = if (nzchar(input$language %||% "")) input$language else "繁體中文"
    )
  })
  cfg_saved <- shiny::debounce(cfg, 1500)
  shiny::observe(save_config(cfg_saved()))

  shiny::observeEvent(input$anthropic_key, {
    if (nzchar(input$anthropic_key)) Sys.setenv(ANTHROPIC_API_KEY = input$anthropic_key)
  })
  shiny::observeEvent(input$gemini_key, {
    if (nzchar(input$gemini_key)) {
      Sys.setenv(GEMINI_API_KEY = input$gemini_key)
      refresh_models()
    }
  })
  shiny::observeEvent(input$openai_key, {
    if (nzchar(input$openai_key)) Sys.setenv(RBUDDY_OPENAI_KEY = input$openai_key)
  })

  refresh_models <- function() {
    c0 <- isolate(cfg())
    if (c0$backend == "anthropic") return()
    models <- list_local_models(c0)
    id <- paste0(c0$backend, "_model")
    current <- c0[[id]] %||% ""
    if (!nzchar(current) && length(models)) {
      # Gemini: default to the newest stable Flash model (good, fast, free tier).
      flash <- sort(models[grepl("flash", models) & !grepl("lite|preview|exp", models)],
                    decreasing = TRUE)
      current <- if (c0$backend == "gemini" && length(flash)) flash[1] else models[1]
    }
    shiny::updateSelectizeInput(session, id, choices = unique(c(models, current)),
                                selected = current)
  }
  shiny::observeEvent(input$backend, refresh_models())
  shiny::observeEvent(input$refresh_models, refresh_models())

  # -- actions back to the main session --------------------------------------
  act_counter <- 0
  send_action <- function(type, text = "", kind = "code", edits = NULL) {
    act_counter <<- act_counter + 1
    f <- file.path(act_dir, sprintf("%s-%05d.json", format(Sys.time(), "%Y%m%d%H%M%S"), act_counter))
    tmp <- paste0(f, ".tmp")
    a <- list(type = type, text = text, kind = kind)
    if (!is.null(edits)) a$edits <- edits
    writeLines(to_json(a), tmp, useBytes = TRUE)
    file.rename(tmp, f)
  }

  # Send every code / proof / edit block of a reply to the editor, in order.
  insert_blocks <- function(segs) {
    n <- 0
    for (s in segs) {
      if (s$type %in% c("code", "proof") && nzchar(trimws(s$text))) {
        send_action("insert", s$text, s$type)
        n <- n + 1
      } else if (s$type == "edit") {
        e <- parse_edits(s$text)
        if (length(e)) { send_action("edit", edits = e); n <- n + 1 }
      }
    }
    n
  }

  # Results of edits (e.g. "original text not found") come back from the main session.
  notice_file <- file.path(dir, "notice.json")
  notice <- shiny::reactivePoll(600, session,
    checkFunc = function() { fi <- file.info(notice_file); paste(fi$size, fi$mtime) },
    valueFunc = function() if (file.exists(notice_file)) {
      tryCatch(jsonlite::read_json(notice_file, simplifyVector = TRUE), error = function(e) NULL)
    })
  shiny::observeEvent(notice(), {
    nt <- notice()
    if (!is.null(nt$msg)) {
      shiny::showNotification(paste(nt$msg, collapse = "\n"), duration = if (isTRUE(nt$ok)) 2 else 8,
                              type = if (isTRUE(nt$ok)) "message" else "warning", session = session)
    }
  }, ignoreInit = TRUE)

  # -- model calls run in a child process so the panel stays responsive and can stop them
  run_llm <- function(slot, args, done) {
    p <- callr::r_bg(function(args) do.call(rbuddy:::llm_call, args), list(args = args))
    state[[slot]] <- p
    poll <- function() isolate({
      if (!identical(state[[slot]], p)) return(invisible())   # stopped
      if (p$is_alive()) return(later::later(poll, 0.3))
      state[[slot]] <- NULL
      res <- tryCatch(p$get_result(), error = function(e) list(error = conditionMessage(e$parent %||% e)))
      done(res)
    })
    later::later(poll, 0.3)
  }
  stop_suggest <- function() {
    p <- isolate(state$sugg_job)
    if (is.null(p)) return()
    p$kill(); state$sugg_job <- NULL; state$sugg_busy <- FALSE; state$sugg_status <- ""
  }
  shiny::observeEvent(input$stop, {
    p <- isolate(state$job)
    if (is.null(p)) return()
    p$kill(); state$job <- NULL
    msgs <- isolate(state$msgs)
    msgs[[length(msgs)]]$api <- FALSE   # drop the unanswered question from the history
    msgs[[length(msgs) + 1]] <- list(role = "error", text = "已停止。", api = FALSE)
    state$msgs <- msgs
    state$busy <- FALSE
  })

  # -- chat -------------------------------------------------------------------
  api_messages <- function(msgs, backend) {
    keep <- Filter(function(m) isTRUE(m$api), msgs)
    lapply(keep, function(m) {
      if (m$role == "assistant") {
        if (backend == "anthropic" && !is.null(m$raw)) {
          list(role = "assistant", content = m$raw)       # replay unchanged
        } else {
          list(role = "assistant", content = if (nzchar(m$text)) m$text else "(空白)")
        }
      } else list(role = "user", content = m$content)
    })
  }

  push_msg <- function(m) {
    msgs <- isolate(state$msgs)
    msgs[[length(msgs) + 1]] <- m
    state$msgs <- msgs
  }

  ask <- function(question, display = question) {
    if (isolate(state$busy)) {
      shiny::showNotification("上一個問題還在回答，可以先按「停止」。", type = "warning", session = session)
      return()
    }
    stop_suggest()
    c0 <- isolate(cfg())
    if (is.null(isolate(state$system))) state$system <- chat_system_prompt(c0$language, c0$code_comments)
    cx <- isolate(ctx())
    content <- paste0(build_context(cx, isolate(state$last_sent), isolate(state$last_console)),
                      "\n\n", question)
    push_msg(list(role = "user", display = display, content = content, api = TRUE))
    state$busy <- TRUE
    history <- api_messages(isolate(state$msgs), c0$backend)
    system <- isolate(state$system)

    run_llm("job", list(cfg = c0, system = system, messages = history), function(res) {
      msgs <- state$msgs
      if (!is.null(res$error)) {
        msgs[[length(msgs)]]$api <- FALSE   # drop the failed turn from API history
        msgs[[length(msgs) + 1]] <- list(role = "error", text = res$error, api = FALSE)
        state$last_sent <- NULL
        state$last_console <- 0
      } else {
        segs <- lapply(parse_segments(res$text), function(s) {
          if (s$type == "code") {
            s$text <- paste(strip_line_numbers(strsplit(s$text, "\n", fixed = TRUE)[[1]]), collapse = "\n")
            if (!isTRUE(c0$code_comments)) s$text <- strip_r_comments(s$text)
          }
          s
        })
        inserted <- if (c0$auto_insert) insert_blocks(segs) else 0
        msgs[[length(msgs) + 1]] <- list(role = "assistant", text = res$text, raw = res$raw,
                                         segs = segs, model = res$model, note = res$note,
                                         inserted = inserted, api = TRUE)
        state$last_sent <- cx$contents
        state$last_console <- console_last_id(cx$console)
      }
      state$msgs <- msgs
      state$busy <- FALSE
    })
  }

  shiny::observeEvent(input$send_msg, {
    q <- trimws(input$send_msg$text %||% "")
    if (nzchar(q)) ask(q)
  })

  # Quick term / translation questions: small model, no script context, not added to the chat history.
  term_ask <- function(q) {
    if (isolate(state$busy)) return()
    stop_suggest()
    c0 <- isolate(cfg())
    tc <- if (nzchar(c0$term_model)) {
      modifyList(c0, list(backend = "ollama", ollama_model = c0$term_model, ollama_think = FALSE, num_ctx = 4096))
    } else c0
    push_msg(list(role = "user", display = paste0("📖 ", q), api = FALSE))
    state$busy <- TRUE
    run_llm("job", list(cfg = tc, system = term_system_prompt(c0$language),
                        messages = list(list(role = "user", content = term_request(q, c0$language))),
                        max_tokens = 2000, effort = "low"), function(res) {
      push_msg(if (!is.null(res$error)) list(role = "error", text = res$error, api = FALSE)
               else list(role = "assistant", text = res$text, segs = parse_segments(res$text),
                         model = res$model, note = res$note, api = FALSE))
      state$busy <- FALSE
    })
  }
  shiny::observeEvent(input$term_msg, {
    q <- trimws(input$term_msg$text %||% "")
    if (nzchar(q)) term_ask(q)
  })

  shiny::observeEvent(input$new_chat, {
    if (isolate(state$busy)) return()
    state$msgs <- list(); state$system <- NULL; state$last_sent <- NULL; state$last_console <- 0
  })

  shiny::observeEvent(input$block_action, {
    a <- input$block_action
    m <- isolate(state$msgs)[[a$m]]
    s <- m$segs[[a$b]]
    if (is.null(s)) return()
    if (a$act == "apply" && s$type == "edit") {
      send_action("edit", edits = parse_edits(s$text))
    } else if (a$act == "insert") {
      send_action("insert", s$text, if (s$type == "proof") "proof" else "code")
      shiny::showNotification("已寫入腳本", duration = 1.5, session = session)
    } else if (a$act == "run") {
      send_action("run", s$text)
      shiny::showNotification("已送到 Console 執行", duration = 1.5, session = session)
    }
  })

  # Questions sent from the "Ask about selection" add-in
  inbox <- shiny::reactivePoll(800, session,
    checkFunc = function() { fi <- file.info(inbox_file); paste(fi$size, fi$mtime) },
    valueFunc = function() if (file.exists(inbox_file)) {
      tryCatch(jsonlite::read_json(inbox_file, simplifyVector = TRUE), error = function(e) NULL)
    })
  shiny::observeEvent(inbox(), {
    q <- inbox()
    if (!is.null(q$text)) {
      unlink(inbox_file)
      # Give the main session a moment to write the latest selection.
      later::later(function() isolate(ask(q$text)), 0.8)
    }
  }, ignoreInit = TRUE)

  # -- suggestions ----------------------------------------------------------------
  run_suggest <- function() {
    if (isolate(state$busy) || isolate(state$sugg_busy)) return()
    cx <- isolate(ctx())
    txt <- cx$contents %||% ""
    if (!isTRUE(cx$available) || nchar(trimws(txt)) < 15) {
      state$sugg_status <- "在左邊的腳本寫下題目，停一下我就會提供建議。"
      return()
    }
    c0 <- isolate(cfg())
    if (c0$backend == "anthropic" && nzchar(c0$suggest_model)) c0$anthropic_model <- c0$suggest_model
    state$sugg_busy <- TRUE
    state$sugg_status <- "分析題目中…"
    run_llm("sugg_job", list(cfg = c0, system = suggest_system_prompt(c0$language),
                             messages = list(list(role = "user", content = build_context(cx))),
                             max_tokens = 4000, effort = "low", json = TRUE), function(res) {
      if (is.null(res$error)) {
        res <- tryCatch(parse_json_reply(res$text), error = function(e) list(error = conditionMessage(e)))
      }
      if (!is.null(res$error)) {
        state$sugg_status <- paste("建議失敗：", res$error)
      } else {
        state$sugg <- res
        state$sugg_status <- ""
      }
      state$sugg_for <- txt
      state$sugg_console <- console_last_id(cx$console)
      state$sugg_busy <- FALSE
    })
  }

  watched <- shiny::reactive(list(contents = ctx()$contents %||% "",
                                   console = console_last_id(ctx()$console)))
  watched_settled <- shiny::debounce(watched, 5000)
  shiny::observeEvent(watched_settled(), {
    if (!isTRUE(isolate(cfg())$auto_suggest)) return()
    new <- watched_settled()$contents
    old <- isolate(state$sugg_for)
    # Only re-run when the script changed meaningfully or new results came in.
    if (is.null(old) || abs(nchar(new) - nchar(old)) >= 40 ||
        (!identical(new, old) && nchar(old) < 15) ||
        watched_settled()$console > isolate(state$sugg_console)) {
      run_suggest()
    }
  })
  shiny::observeEvent(input$refresh_suggest, run_suggest())

  shiny::observeEvent(input$suggest_click, {
    s <- isolate(state$sugg)$suggestions[[input$suggest_click$i]]
    if (!is.null(s)) ask(s$ask %||% s$title, display = paste0("💡 ", s$title, "\n", s$ask %||% ""))
  })

  # -- outputs -----------------------------------------------------------------
  output$status <- shiny::renderUI({
    c0 <- cfg(); cx <- ctx()
    brain <- switch(c0$backend %||% "anthropic",
      anthropic = c0$anthropic_model, ollama = c0$ollama_model, openai = c0$openai_model,
      gemini = c0$gemini_model)
    file <- if (isTRUE(cx$available)) {
      p <- cx$path %||% ""
      sprintf("📄 %s · 第 %s 行", if (nzchar(p)) basename(p) else "未存檔", cx$cursor_row %||% "?")
    } else "📄 讀不到編輯器"
    n <- length(console_entries(cx$console))
    if (n) file <- paste0(file, " · 🖥 Console ", n, " 筆")
    shiny::span(class = "rb-status", paste0(brain %||% "", "  ·  ", file))
  })

  output$suggestions <- shiny::renderUI({
    s <- state$sugg
    status <- state$sugg_status
    items <- NULL
    if (!is.null(s$suggestions) && length(s$suggestions)) {
      items <- lapply(seq_along(s$suggestions), function(i) {
        x <- s$suggestions[[i]]
        shiny::tags$button(class = "rb-sugg", onclick = sprintf("rbAsk(%d)", i),
          shiny::div(class = "rb-sugg-t", x$title %||% ""),
          shiny::div(class = "rb-sugg-w", x$why %||% ""))
      })
    }
    shiny::tagList(
      if (!is.null(s$topic)) shiny::div(class = "rb-topic", s$topic),
      if (nzchar(status)) shiny::div(class = "rb-muted", status),
      shiny::div(class = "rb-sugg-list", items)
    )
  })

  output$chat <- shiny::renderUI({
    msgs <- state$msgs
    if (!length(msgs)) {
      return(shiny::div(class = "rb-empty",
        "我看得到你左邊打開的腳本、游標位置、選取的文字和 R 環境裡的資料。",
        shiny::tags$br(), "直接問我，例如：「這題要用什麼檢定？」「幫我證明這個估計量不偏」。"))
    }
    shiny::tagList(
      lapply(seq_along(msgs), function(i) render_msg(msgs[[i]], i)),
      if (state$busy) shiny::div(class = "rb-msg rb-thinking", "思考中… ",
        shiny::tags$button(class = "rb-btn", onclick = "rbStop()", title = "Esc", "■ 停止"))
    )
  })

  shiny::observe({
    state$msgs; state$busy; state$sugg
    session$sendCustomMessage("rb_rendered", list())
  })
  invisible(state)
}

# ---- message rendering -----------------------------------------------------

block_buttons <- function(i, j, type) {
  b <- function(label, act) {
    shiny::tags$button(class = "rb-btn", onclick = sprintf("rbAct(%d,%d,'%s')", i, j, act), label)
  }
  copy <- shiny::tags$button(class = "rb-btn", onclick = "rbCopy(this)", "複製")
  switch(type,
    code = shiny::tagList(b("插入腳本", "insert"), b("執行", "run"), copy),
    proof = shiny::tagList(b("插入腳本", "insert"), copy),
    edit = b("套用修改", "apply"),
    copy)
}

render_msg <- function(m, i) {
  tags <- shiny::tags
  if (m$role == "user") {
    return(shiny::div(class = "rb-msg rb-user", m$display))
  }
  if (m$role == "error") {
    return(shiny::div(class = "rb-msg rb-error", "⚠ ", m$text))
  }
  segs <- m$segs
  if (!length(segs)) {
    segs <- list(list(type = "text", text = "*（模型沒有產生回答。可以再問一次，或按「新對話」清掉太長的對話。）*"))
  }
  body <- lapply(seq_along(segs), function(j) {
    s <- segs[[j]]
    if (s$type == "text") return(shiny::HTML(md_to_html(s$text)))
    label <- switch(s$type, code = "R", proof = "證明", edit = "修改腳本", s$lang)
    inner <- if (s$type == "proof") shiny::div(class = "rb-proof-body", shiny::HTML(md_to_html(s$text)))
             else if (s$type == "edit") render_edit(s$text)
             else shiny::HTML(sprintf("<pre><code>%s</code></pre>", htmltools::htmlEscape(s$text)))
    shiny::div(class = paste("rb-block", paste0("rb-", s$type)),
      shiny::div(class = "rb-block-h", shiny::span(class = "rb-lang", label),
                 shiny::span(class = "rb-btns", block_buttons(i, j, s$type))),
      inner,
      tags$textarea(class = "rb-raw", s$text)
    )
  })
  foot <- c(m$model,
            if (isTRUE(m$inserted > 0)) sprintf("✓ 已寫入／修改腳本（%d 段）", m$inserted),
            m$note)
  shiny::div(class = "rb-msg rb-assistant", body,
             shiny::div(class = "rb-foot", paste(foot, collapse = " · ")))
}

render_edit <- function(text) {
  edits <- parse_edits(text)
  if (!length(edits)) {
    return(shiny::HTML(sprintf("<pre><code>%s</code></pre>", htmltools::htmlEscape(text))))
  }
  esc <- htmltools::htmlEscape
  html <- vapply(edits, function(e) {
    old <- strsplit(e$old, "\n", fixed = TRUE)[[1]]
    new <- strsplit(e$new, "\n", fixed = TRUE)[[1]]
    del <- paste0('<span class="rb-del">- ', esc(old), "</span>", collapse = "\n")
    add <- if (length(new)) paste0('<span class="rb-add">+ ', esc(new), "</span>", collapse = "\n")
           else '<span class="rb-muted">（刪除）</span>'
    paste0(del, "\n", add)
  }, "")
  shiny::HTML(sprintf('<pre class="rb-diff"><code>%s</code></pre>', paste(html, collapse = "\n\n")))
}
