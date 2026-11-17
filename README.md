# R Buddy：RStudio 裡的 AI 助教

在 RStudio 右邊的 Viewer 面板開一個小助教，左邊照常寫你的腳本。

## 它會做什麼

- **💡 你可能需要**：你在腳本寫題目、停下來約 5 秒，或在 Console 跑出新結果後，它會列出接下來可能需要的分析、檢定或推導。點一下就開始做。
- **聊天框**：問問題時，它看得到：
  - 你**目前正在看的那個腳本分頁**（含沒存檔的修改）、游標在第幾行、選取的文字
  - **最近在 Console 執行的指令和印出的結果**（`summary`、`anova` 表、檢定結果等），所以可以直接問「這個 p 值代表什麼？」
  - Global Environment 裡的物件（資料框的欄位與型別）和最近一次的錯誤訊息
- **寫進腳本**：回答裡的 R 程式碼和證明會插在游標下方。`.R` 檔的證明會變成 `#` 開頭的行；`.Rmd`/`.qmd` 會變成 chunk 和數學式。每段也有「插入腳本／執行／複製」按鈕。
- **修改你寫錯的程式碼**：請它修正時，它會直接把錯的那幾行換掉（面板上用紅色刪除線／綠色標出改了什麼）。只有在腳本裡找到一字不差的原文才會改，找不到就不動並提醒你。改錯了按 **Ctrl+Z** 復原。
- **名詞／翻譯按鈕**：只想知道一個名詞是什麼、或看不懂一句英文題目時，按「名詞／翻譯」而不是「送出」。它用小模型、不看腳本，1～3 秒就回答；不寫程式、不做證明，也不會算進聊天紀錄。
- **程式碼不寫註解**：預設會把程式碼裡的註解刪掉，只留必要的指令。想要註解可以在 ⚙ 打開。

## 安裝

```r
install.packages(c("shiny", "httr2", "jsonlite", "rstudioapi", "callr", "later", "commonmark", "httpuv", "remotes"))
remotes::install_github("I-Ying-Tsai/R_kk", subdir = "rbuddy")   # 從 GitHub 安裝
# 或從本機資料夾安裝：install.packages("~/Downloads/R_kk/rbuddy", repos = NULL, type = "source")
```

**更新 R Buddy 之後**一定要：Addins → R Buddy - Close → **Session → Restart R** → Addins → R Buddy - Open，新版本才會生效。

## 選大腦（⚙ 設定）

### Ollama：本機開源模型（免費、離線，目前的預設）

這台電腦（RTX 4060，8 GB）已經下載好：

| 模型 | 用途 | 速度 |
|---|---|---|
| `qwen3.5:9b` | 「送出」：寫程式、證明、解讀結果 | 一題約 15～90 秒 |
| `qwen3.5:4b` | 「名詞／翻譯」按鈕 | 1～3 秒 |

- **換模型**：「送出」用的模型在 ⚙ → Ollama →「模型」；「名詞／翻譯」用的模型在 ⚙ 下方的「名詞／翻譯用的小模型」，留空就跟「送出」用同一個大腦（例如用 Gemini 時）。
- 顯示卡記憶體一次只放得下一個模型，所以在「送出」和「名詞／翻譯」之間切換時，第一個問題會多等幾秒載入。
- **先思考再回答**（只影響「送出」）：比較準但比較慢，簡單的問題可以關掉。
- **Context 長度**：建議 16384。調到 32768 會有一部分跑到 CPU，速度會慢很多。
- 想下載其他模型：在終端機執行 `ollama pull 模型名稱`，再到 ⚙ 按「↻ 重新抓模型清單」。

### Gemini：Google（有免費額度，需要網路）

1. 到 Google AI Studio（aistudio.google.com）建立 API key。
2. 在 `~/.Renviron` 加一行 `GEMINI_API_KEY=你的key` 後重開 R；或直接貼在 ⚙ 設定裡（只存在記憶體）。
3. ⚙ → Gemini → 按「↻ 重新抓模型清單」，會自動選最新的穩定版 Flash 模型。

免費額度每分鐘、每天都有次數上限，用完會出現錯誤，隔天重置。依 Google 條款，免費方案的內容可能被用來改進他們的產品。

### Claude：Anthropic API（要付費）

在 `~/.Renviron` 加一行 `ANTHROPIC_API_KEY=sk-ant-...` 後重開 R。預設模型是 `claude-opus-5-5`，可以在 ⚙ 換模型、調思考深度。自動建議每次都會呼叫一次 API，想省錢可以另外挑比較便宜的「自動建議用的模型」，或關掉自動建議。

### 其他：OpenAI 相容伺服器

LM Studio、vLLM、llama.cpp 等，填 Base URL 和模型名稱即可。

## 使用

1. **Addins → R Buddy - Open**（或 `rbuddy::rbuddy_start()`）。
2. 只問名詞或翻譯一句英文：打好後按「**名詞／翻譯**」。
3. 問選取的那段：**Addins → R Buddy - Ask about selection**。
4. 綁快捷鍵：**Tools → Modify Keyboard Shortcuts**，搜尋 “R Buddy”。
5. 關閉：**Addins → R Buddy - Close**（或 `rbuddy::rbuddy_stop()`）。
6. 想太久或問錯了：按「思考中…」旁邊的「**■ 停止**」（或按 Esc）。停掉的問題不會留在對話紀錄裡。
7. 換一份作業、或對話太長時，按「**新對話**」。

## 讓它答得更好的小技巧

- **題目要打進腳本**：它只看得到文字。題目引用課本的表格或習題（例如「Verify Table 2.10」、「Exercise 2.26」）時，要把表格或題目內容貼進腳本，否則它會請你提供，不會亂猜。
- **照你們課本的定義**：各本書的門檻不一樣（例如高槓桿 $h_{ii} > 2p/n$ 還是 $3p/n$），把定義寫在腳本最上面，它就會照著用：
  ```r
  # 本課定義：Outlier |studentized residual| > 2；Extreme point h_ii > 2p/n；Influential Cook's D > 4/n
  ```
- **小模型會亂猜縮寫**：「名詞／翻譯」遇到課本自己定義的縮寫（例如 SSLF）有時會編一個錯的全名。對它的解釋有疑問時，改用「送出」問大模型，或翻課本。
- **一定要自己核對**：9B 的小模型證明的主體多半是對的，但細節、前提說明、程式碼裡的欄位名稱常常出錯，而且有些錯不會報錯、只會算出錯的數字。能互相驗證的就驗證（例如 t² 應該等於 F）。

## 限制

- **Console**：只記得開著 R Buddy 之後最近 10 個指令；`cat()`、`message()` 印出的文字和警告訊息抓不到；用 `source()` 跑整個檔案時只會記到 `source()` 這一行。
- 只看得到、也只寫得進**目前正在看的那個分頁**。
- 看不到圖片和 Plots 面板。

## 疑難排解

| 狀況 | 原因／解法 |
|---|---|
| 回答是空的，或註明「已改用不思考模式重答」 | 模型想太久把 context 用完。按「新對話」，或確認 ⚙ 的 Context 長度是 16384。 |
| 「找不到原文，沒有更動」 | 要修改的那幾行已經被你改過了，或模型抄錯原文。可以再請它修一次。 |
| 回答很慢 | 按「■ 停止」；只是問名詞就改用「名詞／翻譯」按鈕，其他問題可以關掉「先思考再回答」。 |
| 改了設定或更新後沒變化 | 重開 R（見上面的「安裝」）。程式碼註解的開關要按「新對話」才生效。 |
| 打不開、或關掉後沒反應 | Console 執行 `rbuddy::rbuddy_stop()` 再 `rbuddy::rbuddy_start()`。 |

## 隱私

- 你的腳本、Console 結果和環境摘要會送給你選的模型。用 Ollama 的話，資料不會離開你的電腦。
- 設定記在 `tools::R_user_dir("rbuddy", "config")`；API key 不會存到硬碟。

## 程式結構（給維護的人）

| 檔案 | 內容 |
|---|---|
| `R/start.R` | 在你的 R session 裡跑：讀編輯器和環境、記錄 Console、把程式碼插入或修改到腳本 |
| `R/app.R` | Viewer 面板（Shiny），跑在背景 R 程序 |
| `R/llm.R` | 呼叫 Claude、Ollama、Gemini、OpenAI 相容伺服器 |
| `R/prompts.R` | 給模型的指示、組合腳本／Console 內容、解析回答（程式碼／證明／修改區塊） |
| `R/config.R` | 設定的預設值與存取 |
| `inst/www/` | 面板的 CSS 和 JavaScript |
| `inst/rstudio/addins.dcf` | RStudio Addins 選單 |

兩個 R 程序之間透過暫存資料夾裡的 JSON 檔溝通：主程序每 0.6 秒寫 `context.json`（編輯器、環境、Console），面板把要做的事寫進 `actions/`（插入、修改、執行），結果寫回 `notice.json`。
