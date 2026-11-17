# ---------------------------------------------------------------------------
# Main R session side.
#
# The chat app runs in a background R process (so your console stays free).
# This file runs in YOUR session and acts as a bridge:
#   * every ~0.6 s (only while the console is idle) it writes what is in the
#     active editor + a summary of the Global Environment to context.json
#   * it picks up "actions" the app drops in actions/ (insert code, run code)
#     and performs them with rstudioapi.
# ---------------------------------------------------------------------------

.rb <- new.env(parent = emptyenv())

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#' Start R Buddy
#'
#' Opens the assistant in the RStudio Viewer pane (or a browser outside RStudio).
#' @param port Port for the local app (random if NULL).
#' @param viewer Open in RStudio's Viewer pane if TRUE, else in a web browser.
#' @export
rbuddy_start <- function(port = NULL, viewer = TRUE) {
  if (rb_alive()) {
    message("R Buddy 已在執行中：", .rb$url)
    open_ui(.rb$url, viewer)
    return(invisible(.rb$url))
  }
  dir <- file.path(tempdir(), paste0("rbuddy-", Sys.getpid()))
  unlink(dir, recursive = TRUE)
  dir.create(file.path(dir, "actions"), recursive = TRUE, showWarnings = FALSE)
  port <- port %||% httpuv::randomPort()
  log <- file.path(dir, "app.log")

  .rb$dir <- dir
  .rb$last_json <- NULL
  start_console_capture()
  write_context()

  proc <- callr::r_bg(
    function(dir, port) rbuddy:::run_app(dir, port),
    args = list(dir = dir, port = port),
    stdout = log, stderr = "2>&1", supervise = TRUE
  )
  .rb$proc <- proc
  .rb$url <- sprintf("http://127.0.0.1:%d", port)
  .rb$running <- TRUE

  if (!wait_for_port(port, proc, timeout = 40)) {
    msg <- if (file.exists(log)) paste(readLines(log, warn = FALSE), collapse = "\n") else ""
    rbuddy_stop(quiet = TRUE)
    stop("R Buddy 無法啟動：\n", msg, call. = FALSE)
  }
  schedule_poll()
  open_ui(.rb$url, viewer)
  message("R Buddy 已啟動：", .rb$url)
  invisible(.rb$url)
}

#' Stop R Buddy
#' @param quiet Suppress the message.
#' @export
rbuddy_stop <- function(quiet = FALSE) {
  .rb$running <- FALSE
  stop_console_capture()
  if (!is.null(.rb$proc) && .rb$proc$is_alive()) .rb$proc$kill()
  .rb$proc <- NULL
  if (!is.null(.rb$dir)) unlink(.rb$dir, recursive = TRUE)
  if (!quiet) message("R Buddy 已關閉。")
  invisible()
}

#' Ask R Buddy about the current selection
#' @export
rbuddy_ask_selection <- function() {
  if (!rb_alive()) rbuddy_start()
  q <- if (rstudioapi::isAvailable()) {
    rstudioapi::showPrompt("R Buddy", "想問什麼？（會附上你選取的內容）",
                           "請解釋並檢查我選取的部分")
  } else readline("想問 R Buddy 什麼？ ")
  if (is.null(q) || !nzchar(trimws(q))) return(invisible())
  write_context()
  write_json_atomic(list(id = format(Sys.time(), "%Y%m%d%H%M%OS6"), text = q),
                    file.path(.rb$dir, "inbox.json"))
  invisible()
}

# ---- internals -------------------------------------------------------------

rb_alive <- function() {
  isTRUE(.rb$running) && !is.null(.rb$proc) && .rb$proc$is_alive()
}

open_ui <- function(url, viewer = TRUE) {
  if (viewer && rstudioapi::isAvailable()) rstudioapi::viewer(url)
  else utils::browseURL(url)
}

wait_for_port <- function(port, proc, timeout = 40) {
  t0 <- Sys.time()
  while (difftime(Sys.time(), t0, units = "secs") < timeout) {
    if (!proc$is_alive()) return(FALSE)
    ok <- tryCatch({
      con <- suppressWarnings(socketConnection("127.0.0.1", port, open = "r+",
                                               blocking = TRUE, timeout = 1))
      close(con)
      TRUE
    }, error = function(e) FALSE)
    if (ok) return(TRUE)
    Sys.sleep(0.3)
  }
  FALSE
}

write_json_atomic <- function(x, path) {
  tmp <- paste0(path, ".tmp", Sys.getpid())
  writeLines(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", digits = NA),
             tmp, useBytes = TRUE)
  file.rename(tmp, path)
}

schedule_poll <- function() later::later(poll_once, 0.6)

poll_once <- function() {
  if (!isTRUE(.rb$running)) return(invisible())
  if (is.null(.rb$proc) || !.rb$proc$is_alive()) {
    .rb$running <- FALSE
    message("R Buddy 的背景程式已結束。用 rbuddy::rbuddy_start() 重新開啟。")
    return(invisible())
  }
  tryCatch(write_context(), error = function(e) NULL)
  tryCatch(process_actions(),
           error = function(e) message("R Buddy 寫入腳本失敗：", conditionMessage(e)))
  schedule_poll()
}

collect_context <- function() {
  ctx <- list(available = FALSE)
  if (rstudioapi::isAvailable()) {
    ed <- tryCatch(rstudioapi::getSourceEditorContext(), error = function(e) NULL)
    if (!is.null(ed)) {
      sel <- ed$selection[[1]]
      ctx$available <- TRUE
      ctx$path <- ed$path %||% ""
      ctx$contents <- paste(ed$contents, collapse = "\n")
      ctx$cursor_row <- unname(sel$range$start[["row"]])
      ctx$selection <- sel$text %||% ""
    }
  }
  ctx$env <- env_summary()
  ctx$console <- .rb$console %||% list()
  err <- geterrmessage()
  ctx$last_error <- if (nzchar(trimws(err))) trimws(err) else ""
  ctx
}

write_context <- function() {
  if (is.null(.rb$dir)) return(invisible())
  ctx <- collect_context()
  js <- jsonlite::toJSON(ctx, auto_unbox = TRUE, null = "null")
  if (!identical(as.character(js), .rb$last_json)) {
    .rb$last_json <- as.character(js)
    path <- file.path(.rb$dir, "context.json")
    tmp <- paste0(path, ".tmp")
    writeLines(js, tmp, useBytes = TRUE)
    file.rename(tmp, path)
  }
  invisible()
}

env_summary <- function(envir = globalenv(), max_objs = 40) {
  nms <- ls(envir)
  if (!length(nms)) return(character())
  extra <- length(nms) - max_objs
  nms <- utils::head(nms, max_objs)
  out <- vapply(nms, function(n) tryCatch({
    obj <- get(n, envir = envir)
    cls <- paste(class(obj), collapse = "/")
    if (is.data.frame(obj)) {
      cols <- utils::head(names(obj), 30)
      types <- vapply(obj[cols], function(x) class(x)[1], "")
      more <- if (ncol(obj) > 30) sprintf(" …(+%d 欄)", ncol(obj) - 30) else ""
      sprintf("%s: %s [%d 列 x %d 欄]; 欄位: %s%s", n, cls, nrow(obj), ncol(obj),
              paste0(cols, "<", types, ">", collapse = ", "), more)
    } else if (is.function(obj)) {
      sprintf("%s: function(%s)", n, paste(names(formals(obj)), collapse = ", "))
    } else if (is.atomic(obj) && length(obj) <= 8) {
      val <- substr(paste(format(obj), collapse = " "), 1, 120)
      sprintf("%s: %s = %s", n, cls, val)
    } else if (is.atomic(obj)) {
      sprintf("%s: %s, 長度 %d, 前幾個值: %s", n, cls, length(obj),
              substr(paste(format(utils::head(obj, 6)), collapse = " "), 1, 120))
    } else {
      sprintf("%s: %s, 長度 %d", n, cls, length(obj))
    }
  }, error = function(e) sprintf("%s: (無法讀取)", n)), "")
  if (extra > 0) out <- c(out, sprintf("…還有 %d 個物件", extra))
  unname(out)
}

process_actions <- function() {
  adir <- file.path(.rb$dir, "actions")
  files <- sort(list.files(adir, pattern = "\\.json$", full.names = TRUE))
  notes <- character(); ok <- TRUE
  for (f in files) {
    a <- tryCatch(jsonlite::read_json(f, simplifyVector = FALSE), error = function(e) NULL)
    unlink(f)
    if (is.null(a)) next
    res <- tryCatch(apply_action(a), error = function(e) list(ok = FALSE, msg = conditionMessage(e)))
    if (is.list(res) && !is.null(res$msg)) {
      notes <- c(notes, res$msg)
      ok <- ok && isTRUE(res$ok)
    }
  }
  notes <- unique(notes)
  if (length(notes)) {
    write_json_atomic(list(id = format(Sys.time(), "%H%M%OS6"), ok = ok, msg = notes),
                      file.path(.rb$dir, "notice.json"))
  }
}

apply_action <- function(a) {
  if (!rstudioapi::isAvailable()) {
    message("R Buddy（不在 RStudio 中，無法寫入腳本）：\n", a$text)
    return(invisible())
  }
  switch(a$type,
    insert = insert_into_editor(a$text, a$kind %||% "code"),
    edit = apply_edits(a$edits),
    run = rstudioapi::sendToConsole(a$text, execute = TRUE, focus = FALSE),
    invisible()
  )
}

doc_kind <- function(path) {
  ext <- tolower(tools::file_ext(path %||% ""))
  if (ext %in% c("rmd", "qmd", "md", "rmarkdown")) "md" else "r"
}

format_for_doc <- function(text, kind, doc) {
  text <- sub("\n+$", "", text)
  if (doc == "r") {
    if (kind == "code") return(text)
    lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
    return(paste0("# ", lines, collapse = "\n"))
  }
  # R Markdown / Quarto
  if (kind == "code") return(paste0("```{r}\n", text, "\n```"))
  text
}

insert_into_editor <- function(text, kind) {
  ed <- tryCatch(rstudioapi::getSourceEditorContext(), error = function(e) NULL)
  if (is.null(ed)) {
    rstudioapi::documentNew(text, type = "r")
    return(invisible())
  }
  formatted <- format_for_doc(text, kind, doc_kind(ed$path))
  contents <- ed$contents
  row <- unname(ed$selection[[1]]$range$end[["row"]])
  row <- max(1, min(row, length(contents)))
  line <- if (length(contents)) contents[row] else ""
  col <- nchar(line, type = "chars") + 1
  block <- paste0("\n", formatted, "\n")
  rstudioapi::insertText(rstudioapi::document_position(row, col), block, id = ed$id)
  # Park the cursor at the end of what we inserted, so the next insert follows it.
  new_lines <- strsplit(block, "\n", fixed = TRUE)[[1]]
  end_row <- row + length(new_lines) - 1
  rstudioapi::setCursorPosition(rstudioapi::document_position(end_row + 1, 1), id = ed$id)
  invisible()
}

# ---- console capture ---------------------------------------------------------
# R calls task callbacks after every top-level command it finishes, passing the
# expression and its value. We keep the last few commands and, for those whose
# result was printed (visible), the same text the Console showed.

console_max_entries <- 10
console_max_chars <- 6000      # all entries together
console_entry_chars <- 3000    # one printed result

start_console_capture <- function() {
  stop_console_capture()
  .rb$console <- list()
  .rb$console_id <- 0
  addTaskCallback(console_callback, name = "rbuddy_console")
  invisible()
}

stop_console_capture <- function() {
  if ("rbuddy_console" %in% getTaskCallbackNames()) removeTaskCallback("rbuddy_console")
  invisible()
}

console_callback <- function(expr, value, ok, visible) {
  tryCatch(record_console(expr, value, visible), error = function(e) NULL)
  TRUE  # keep the callback registered
}

record_console <- function(expr, value, visible) {
  if (!isTRUE(.rb$running)) return()
  cmd <- paste(deparse(expr, width.cutoff = 120L), collapse = "\n")
  if (grepl("^rbuddy(::|:::)", cmd)) return()   # don't log R Buddy's own calls
  if (nchar(cmd) > 600) cmd <- paste0(substr(cmd, 1, 600), " …")
  out <- ""
  # print(x) returns x invisibly, but the Console did show it.
  shown <- isTRUE(visible) || (is.call(expr) && identical(expr[[1]], as.name("print")))
  if (shown) out <- printed_output(value)
  .rb$console_id <- .rb$console_id + 1
  entry <- list(id = .rb$console_id, cmd = cmd, out = out)
  log <- c(.rb$console, list(entry))
  log <- utils::tail(log, console_max_entries)
  # Drop the oldest entries until the whole log fits the size budget.
  while (length(log) > 1 &&
         sum(vapply(log, function(e) nchar(e$cmd) + nchar(e$out), 0)) > console_max_chars) {
    log <- log[-1]
  }
  .rb$console <- log
}

printed_output <- function(value) {
  if (inherits(value, c("ggplot", "gg", "trellis", "htmlwidget", "recordedplot"))) {
    return("[圖形，顯示在 Plots 面板]")
  }
  old <- options(max.print = 400, width = 100)
  on.exit(options(old))
  txt <- tryCatch(paste(utils::capture.output(print(value)), collapse = "\n"),
                  error = function(e) "[無法擷取輸出]")
  if (nchar(txt) > console_entry_chars) {
    txt <- paste0(substr(txt, 1, console_entry_chars), "\n…（輸出太長，已截斷）")
  }
  txt
}

# ---- editing existing code -----------------------------------------------------
# The model quotes the ORIGINAL lines verbatim; we only touch the script when we
# find exactly those lines (first ignoring trailing spaces, then all spacing).

trim_blank_ends <- function(x) {
  while (length(x) && !nzchar(trimws(x[1]))) x <- x[-1]
  while (length(x) && !nzchar(trimws(x[length(x)]))) x <- x[-length(x)]
  x
}

strip_line_numbers <- function(x) {
  pat <- "^\\s*\\d+\\| ?"
  body <- x[nzchar(trimws(x))]
  if (length(body) && all(grepl(pat, body))) sub(pat, "", x) else x
}

#' @return list(start, end) of the matching lines, or NULL
find_lines <- function(lines, old, near = 1) {
  old <- trim_blank_ends(strip_line_numbers(old))
  k <- length(old); n <- length(lines)
  if (k == 0 || k > n) return(NULL)
  norms <- list(function(x) sub("\\s+$", "", x),
                function(x) gsub("\\s+", " ", trimws(x)))
  for (norm in norms) {
    L <- norm(lines); O <- norm(old)
    hits <- which(vapply(seq_len(n - k + 1), function(i) all(L[i:(i + k - 1)] == O), TRUE))
    if (length(hits)) {
      s <- hits[which.min(abs(hits - near))]
      return(list(start = s, end = s + k - 1))
    }
  }
  NULL
}

#' Range + replacement text that turns lines[start:end] into `new`
#' (or removes them, newline included, when `new` is empty).
edit_range <- function(lines, start, end, new) {
  n <- length(lines)
  new <- sub("\n+$", "", new)
  if (nzchar(trimws(new))) {
    return(list(from = c(start, 1), to = c(end, nchar(lines[end]) + 1), text = new))
  }
  if (end < n) return(list(from = c(start, 1), to = c(end + 1, 1), text = ""))
  if (start > 1) return(list(from = c(start - 1, nchar(lines[start - 1]) + 1),
                             to = c(end, nchar(lines[end]) + 1), text = ""))
  list(from = c(1, 1), to = c(end, nchar(lines[end]) + 1), text = "")
}

apply_edits <- function(edits) {
  done <- 0; missing <- 0
  for (e in edits) {
    ed <- rstudioapi::getSourceEditorContext()
    if (is.null(ed)) return(list(ok = FALSE, msg = "沒有開著的腳本可以修改。"))
    old <- strsplit(e$old %||% "", "\n", fixed = TRUE)[[1]]
    near <- unname(ed$selection[[1]]$range$start[["row"]])
    hit <- find_lines(ed$contents, old, near)
    if (is.null(hit)) { missing <- missing + 1; next }
    new <- paste(strip_line_numbers(strsplit(e$new %||% "", "\n", fixed = TRUE)[[1]]), collapse = "\n")
    r <- edit_range(ed$contents, hit$start, hit$end, new)
    rng <- rstudioapi::document_range(rstudioapi::document_position(r$from[1], r$from[2]),
                                      rstudioapi::document_position(r$to[1], r$to[2]))
    rstudioapi::modifyRange(rng, r$text, id = ed$id)
    done <- done + 1
  }
  if (missing) {
    return(list(ok = FALSE, msg = sprintf(
      "已修改 %d 處；有 %d 處在腳本裡找不到原文，沒有更動（可能腳本已經改過）。", done, missing)))
  }
  list(ok = TRUE, msg = sprintf("已修改腳本 %d 處（Ctrl+Z 可復原）", done))
}
