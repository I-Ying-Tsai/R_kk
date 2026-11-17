# ---------------------------------------------------------------------------
# Prompts and context formatting.
# ---------------------------------------------------------------------------

chat_system_prompt <- function(language, comments = FALSE) {
  comment_rule <- if (isTRUE(comments)) {
    "只在不明顯的地方加一行簡短註解，不要逐行解釋。"
  } else {
    "不要寫任何註解（使用者看得懂 R 語法，不懂會自己問），也不要用 cat() 或 print() 印出說明文字。"
  }
  paste0(
"你是 R Buddy，一位內建在 RStudio 裡、坐在使用者旁邊的統計／資料分析／數學助教。
使用者正在寫作業、考古題或分析報告。每則使用者訊息前面都附有：
- <editor>：使用者目前在 RStudio 編輯器開著的檔案（含行號）與游標所在行
- <selection>：使用者選取的文字（如果有）
- <r_environment>：R Global Environment 中的物件摘要（資料框的欄位與型別等）
- <console>：使用者最近在 Console 執行的指令，以及 Console 印出的結果（例如 summary、anova 表、檢定結果）。解讀結果時請引用裡面實際的數字，不要自己編
- <last_error>：R 最近一次的錯誤訊息（可能是很久以前的，僅供參考）

回答規則：
1. 一律使用", language, "回答，簡潔、直接，不要客套。
2. 要「新增」到使用者腳本的 R 程式碼，放在 ```r 程式碼區塊裡（會插在使用者游標下方）。程式碼必須能在使用者目前的環境直接執行：沿用環境裡已存在的物件名稱，必要的 library() 寫在最前面。程式碼要精簡，只寫完成題目必要的指令。", comment_rule, "不要重寫使用者已經寫好的部分，只寫接下來需要的部分。
2b. 要「修改或刪除」使用者腳本裡已經存在的程式碼（例如修正錯誤），用 ```edit 區塊，格式如下，會直接替換腳本裡的原文：
```edit
<<<<<<< ORIGINAL
（從腳本中逐字複製要被取代的那幾行，不要包含行號和「|」）
=======
（改好的新內容；如果是要刪除，這裡留空）
>>>>>>> UPDATED
```
   一個 edit 區塊裡可以放好幾組。ORIGINAL 必須和腳本內容一字不差，只包含需要改的那幾行。用了 edit 區塊就不要再另外貼一份完整的程式碼。
   只做使用者要求的事：使用者請你修正時，只給 edit 區塊，不要順便加其他分析的程式碼。
3. 數學證明或推導，放在 ```proof 區塊裡，用 Markdown 加 LaTeX（行內 $...$，獨立一行 $$...$$）。步驟清楚、每一步說明理由，最後寫出結論。
4. 程式碼區塊和 proof 區塊之外的文字只顯示給使用者看，不會寫進腳本；用來說明思路、解讀結果、提醒注意事項。
5. 做統計檢定或建模時，提醒需要檢查的前提假設（常態性、變異數同質、獨立性、樣本數等），並提供檢查用的程式碼。
6. 題目資訊不足時，先說明你的假設再作答；真的無法判斷時才反問。如果題目引用課本的表格、習題或資料（例如「Verify Table 2.10」、「Exercise 2.26」），而內容不在腳本裡，不要猜題目、不要寫範例程式碼，只請使用者把題目或資料貼進腳本。
7. 使用者只是問名詞或概念（例如「什麼是 leverage」）時，只用文字和數學式解釋，不要給任何程式碼區塊；使用者要你寫程式時才寫。
8. R 的模型公式裡要做算術（加權相加、平方、相乘）必須包在 I() 裡，例如 lm(y ~ I(4*x1 + 2*x2 + x3) - 1)；沒有截距用 - 1。
9. 每個程式碼區塊和 proof 區塊都要用 ``` 單獨一行結束，不要把一個區塊放在另一個區塊裡。")
}

suggest_system_prompt <- function(language) {
  paste0(
"你是統計／資料分析／數學助教。根據使用者正在寫的題目或腳本，判斷他接下來最可能需要做哪些分析、檢定、圖表或推導。
用", language, "撰寫。只輸出一個 JSON 物件，不要有其他文字，格式：
{\"topic\": \"一句話描述這題在問什麼\",
 \"suggestions\": [
   {\"title\": \"簡短標題（15 字內）\",
    \"why\": \"為什麼適合這題（一句話）\",
    \"ask\": \"使用者點這個建議時，要送給助教的完整請求（例如：請幫我對 df 的 score 依 group 做單因子 ANOVA，並檢查常態與變異數同質）\"}
 ]}
建議 2 到 5 個，依相關程度排序；如果已經做完的分析就不要再建議，改建議下一步。")
}

#' Build the context header that precedes every user message.
#' @param prev_contents editor contents sent with the previous message (to
#'   avoid resending an unchanged script).
build_context <- function(ctx, prev_contents = NULL, prev_console_id = 0) {
  parts <- character()
  if (isTRUE(ctx$available)) {
    path <- ctx$path %||% ""
    fname <- if (nzchar(path)) basename(path) else "未存檔的腳本"
    contents <- ctx$contents %||% ""
    if (!is.null(prev_contents) && identical(prev_contents, contents)) {
      body <- "（內容與上一則訊息相同，沒有變動）"
    } else {
      lines <- strsplit(contents, "\n", fixed = TRUE)[[1]]
      body <- if (length(lines)) {
        paste(sprintf("%4d| %s", seq_along(lines), lines), collapse = "\n")
      } else "（空白檔案）"
    }
    parts <- c(parts, sprintf('<editor file="%s" cursor_line="%s">\n%s\n</editor>',
                              fname, ctx$cursor_row %||% "?", body))
    sel <- ctx$selection %||% ""
    if (nzchar(sel)) parts <- c(parts, sprintf("<selection>\n%s\n</selection>", sel))
  } else {
    parts <- c(parts, "<editor>（目前讀不到 RStudio 編輯器）</editor>")
  }
  parts <- c(parts, format_console(ctx$console, prev_console_id))
  env <- unlist(ctx$env)
  parts <- c(parts, sprintf("<r_environment>\n%s\n</r_environment>",
                            if (length(env)) paste(env, collapse = "\n") else "（空的）"))
  err <- ctx$last_error %||% ""
  if (nzchar(err)) parts <- c(parts, sprintf("<last_error>\n%s\n</last_error>", err))
  paste(parts, collapse = "\n")
}

# ---- parsing replies into text / code / proof segments ---------------------

parse_segments <- function(text) {
  lines <- strsplit(text %||% "", "\n", fixed = TRUE)[[1]]
  segs <- list()
  buf <- character()
  in_block <- FALSE
  lang <- ""
  flush <- function(type, lang = "") {
    if (type == "text" && !nzchar(trimws(paste(buf, collapse = "\n")))) return()
    segs[[length(segs) + 1]] <<- list(type = type, lang = lang,
                                      text = paste(buf, collapse = "\n"))
  }
  for (ln in lines) {
    if (!in_block && grepl("^\\s*```", ln)) {
      flush("text")
      buf <- character()
      in_block <- TRUE
      info <- tolower(trimws(gsub("[`{}]", "", ln)))
      lang <- strsplit(info, "[ ,]")[[1]][1] %||% ""
      if (is.na(lang)) lang <- ""
      next
    }
    # A new opening fence inside a block means the model forgot to close the
    # previous one (e.g. ```r nested inside ```proof): close it and start anew.
    if (in_block && grepl("^\\s*```\\s*[{]?[A-Za-z]", ln)) {
      flush(classify_block(lang, buf), lang)
      buf <- character()
      info <- tolower(trimws(gsub("[`{}]", "", ln)))
      lang <- strsplit(info, "[ ,]")[[1]][1] %||% ""
      if (is.na(lang)) lang <- ""
      next
    }
    if (in_block && grepl("^\\s*```\\s*$", ln)) {
      flush(classify_block(lang, buf), lang)
      buf <- character()
      in_block <- FALSE
      next
    }
    buf <- c(buf, ln)
  }
  if (length(buf)) { if (in_block) flush(classify_block(lang, buf), lang) else flush("text") }
  segs
}

block_type <- function(lang) {
  if (lang %in% c("edit", "diff", "replace", "patch")) "edit"
  else if (lang %in% c("r", "", "rscript", "splus", "s")) "code"
  else if (lang %in% c("proof", "latex", "math", "tex", "derivation")) "proof"
  else "other"
}

# ---- markdown + math -------------------------------------------------------

md_to_html <- function(text) {
  # 1. protect inline code so `df$x` is never mistaken for math
  codes <- character()
  m <- gregexpr("`[^`\n]+`", text, perl = TRUE)
  found <- regmatches(text, m)[[1]]
  if (length(found)) {
    codes <- found
    regmatches(text, m) <- list(sprintf("RBCODE%dZZ", seq_along(found)))
  }
  # 2. protect math from the markdown parser
  maths <- character()
  pats <- c("\\$\\$[\\s\\S]+?\\$\\$", "\\\\\\[[\\s\\S]+?\\\\\\]",
            "\\\\\\([\\s\\S]+?\\\\\\)", "(?<![\\\\$])\\$[^$\n]+?\\$")
  for (p in pats) {
    m <- gregexpr(p, text, perl = TRUE)
    found <- regmatches(text, m)[[1]]
    if (length(found)) {
      ids <- length(maths) + seq_along(found)
      maths <- c(maths, found)
      regmatches(text, m) <- list(sprintf("RBMATH%dZZ", ids))
    }
  }
  for (k in seq_along(codes)) text <- sub(sprintf("RBCODE%dZZ", k), codes[k], text, fixed = TRUE)
  html <- commonmark::markdown_html(text, extensions = TRUE)
  for (k in seq_along(maths)) {
    html <- gsub(sprintf("RBMATH%dZZ", k), htmltools::htmlEscape(maths[k]), html, fixed = TRUE)
  }
  html
}

# Console entries arrive as a data frame (id, cmd, out) after the JSON round trip.
console_entries <- function(console) {
  if (is.null(console) || length(console) == 0) return(list())
  if (is.data.frame(console)) {
    return(lapply(seq_len(nrow(console)), function(i) as.list(console[i, , drop = FALSE])))
  }
  console
}

console_last_id <- function(console) {
  e <- console_entries(console)
  if (length(e)) max(vapply(e, function(x) as.numeric(x$id), 0)) else 0
}

format_console <- function(console, prev_id = 0) {
  e <- console_entries(console)
  if (!length(e)) return(character())
  new <- Filter(function(x) as.numeric(x$id) > (prev_id %||% 0), e)
  if (!length(new)) return("<console>（上一則訊息之後沒有新的 Console 輸出）</console>")
  lines <- vapply(new, function(x) {
    cmd <- paste0("> ", gsub("\n", "\n+ ", x$cmd, fixed = TRUE))
    if (nzchar(x$out %||% "")) paste0(cmd, "\n", x$out) else cmd
  }, "")
  sprintf("<console>\n%s\n</console>", paste(lines, collapse = "\n"))
}

# A block counts as an edit if it is labelled so, or if it contains the
# <<<<<<< / ======= / >>>>>>> markers (small models sometimes label it ```r).
classify_block <- function(lang, lines) {
  type <- block_type(lang)
  if (type != "edit" && any(grepl("^<{5,}", lines)) && any(grepl("^>{5,}", lines))) type <- "edit"
  type
}

#' Split an edit block into list(list(old = , new = ), ...)
parse_edits <- function(text) {
  lines <- strsplit(text %||% "", "\n", fixed = TRUE)[[1]]
  edits <- list()
  mode <- "none"
  old <- new <- character()
  for (ln in lines) {
    if (grepl("^<{5,}", ln)) { mode <- "old"; old <- new <- character(); next }
    if (mode == "old" && grepl("^={5,}\\s*$", ln)) { mode <- "new"; next }
    if (mode == "new" && grepl("^>{5,}", ln)) {
      edits[[length(edits) + 1]] <- list(old = paste(old, collapse = "\n"),
                                         new = paste(new, collapse = "\n"))
      mode <- "none"; next
    }
    if (mode == "old") old <- c(old, ln)
    if (mode == "new") new <- c(new, ln)
  }
  Filter(function(e) nzchar(trimws(e$old)), edits)
}

#' Remove comments from R code. Uses R's own parser so a `#` inside a string
#' is kept; if the code doesn't parse, only whole-line comments are dropped.
strip_r_comments <- function(code) {
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  pd <- tryCatch({
    p <- parse(text = code, keep.source = TRUE)
    utils::getParseData(p)
  }, error = function(e) NULL)
  if (!is.null(pd) && nrow(pd)) {
    com <- pd[pd$token == "COMMENT", , drop = FALSE]
    drop <- integer()
    # Right-to-left so earlier column positions stay valid.
    for (i in rev(seq_len(nrow(com)))) {
      r <- com$line1[i]
      before <- substr(lines[r], 1, com$col1[i] - 1)
      if (!nzchar(trimws(before))) drop <- c(drop, r)       # comment-only line
      lines[r] <- sub("\\s+$", "", before)
    }
    if (length(drop)) lines <- lines[-unique(drop)]
  } else {
    lines <- lines[!grepl("^\\s*#", lines)]
  }
  # Collapse runs of blank lines and trim blank ends.
  out <- paste(lines, collapse = "\n")
  out <- gsub("\n{3,}", "\n\n", out)
  sub("^\n+", "", sub("\n+$", "", out))
}

# The small "名詞" model only translates and explains terms.
term_system_prompt <- function(language) {
  paste0(
"你是統計學（線性模型、迴歸、變異數分析）的名詞小幫手，只做兩件事：
1. 解釋名詞：用", language, "寫 2～5 句，第一句就是定義，附上英文原名；需要時給一個數學式（用 $...$）。
2. 翻譯：輸入是完整的英文句子或題目（不是單一名詞或問句）時，只把它翻成", language, "，統計名詞後面括號附英文原文；不要解釋、不要判斷對錯、不要解題。
不要寫程式碼、不要做證明、不要延伸講檢定流程，也不要說明你在遵守哪條規則。
縮寫或名詞在不同課本可能有不同意思；不確定是什麼的縮寫時，直接說不確定並列出常見的可能，不要自己編造全名。")
}

# An English sentence (not a single term) is sent as an explicit translation request.
term_request <- function(q, language) {
  words <- strsplit(trimws(q), "\\s+")[[1]]
  if (length(words) >= 5 && !grepl("[^\\x01-\\x7F]", q, perl = TRUE)) {
    paste0("把下面這段英文完整翻成", language, "，只輸出翻譯：\n\n", q)
  } else q
}
