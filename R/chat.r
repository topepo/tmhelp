#' Launch an Interactive tidymodels Documentation Chat App
#'
#' Starts an interactive chat interface for asking questions about tidymodels
#' documentation, powered by an OpenAI-based assistant and a hybrid search (RAG)
#' retrieval system over an embedded tidymodels knowledge store.
#'
#' This app combines semantic and text-based search, returning authoritative
#' excerpts from tidymodels documentation.
#'
#' @param question A character string with the user's question (optional). If
#'   not provided, app opens with a blank chat.
#' @param client An `ellmer::Chat` object. Defaults to openai 'gpt-4.1'. Note
#'   that if a different chat provider is used for chat, an `OPENAI_API_KEY`
#'   must still be set for embedding vector search.
#' @param interactive Logical; whether to launch the interactive Shiny app
#'   (default `TRUE`). If `FALSE`, returns chat response directly if `question`
#'   is provided, otherwise, the `client` is returned with the retrieval tool
#'   registered and system prompt set.
#'
#' @return Invisibly returns the `client` object for further use or inspection.
#'
#' @seealso [ellmer::chat_openai], [ragnar::ragnar_retrieve]
#' @importFrom rlang .data
#' @export
#' @examples
#' if (interactive() && nzchar(Sys.getenv("OPENAI_API_KEY"))) {
#'   tmhelp::ask("How can I make a two column layout?")
#' }
ask <- function(
  question = NULL,
  client = ellmer::chat_openai(model = "gpt-4.1"),
  interactive = TRUE
) {
  # Early check for OpenAI API Key
  if (!nzchar(Sys.getenv("OPENAI_API_KEY"))) {
    stop(
      "No OpenAI API key found in Sys.getenv('OPENAI_API_KEY').",
      call. = FALSE
    )
  }

  # Validate user input
  if (!is.null(question)) {
    if (!is.character(question) || length(question) != 1 || is.na(question)) {
      stop("question must be a single, non-NA character string.")
    }
  }

  # Set up the system prompt
  client$set_system_prompt(glue::trim(
    "
    You are an expert in Tidymodels documentation. You are concise.
    Always perform a search of the Tidymodels knowledge store for each user request.
    Every response must cite links to official documentation sources.
    If the request is ambiguous, search first, then ask a clarifying question.
    If docs are unavailable or search fails, inform the user and do NOT answer the question.

    Always give answers that include a minimal fully self-contained tm document.

    To display tm code blocks, use oversized markdown fences, like this:

    ````` markdown
    PROSE HERE
    ```{r}
    CODE HERE
    ```
    ```{python}
    CODE HERE
    ```
    `````
    "
  ))

  # Connect to the tidymodels knowledge store
  store <- tm_ragnar_store()

  retrieved_ids <- integer()
  rag_retrieve_tm_excerpts <- function(text) {
    # Retrieve relevant chunks using hybrid (vector/BM25) search,
    # excluding previously returned IDs in this session.
    chunks <- dplyr::tbl(store) |>
      dplyr::filter(!.data$id %in% retrieved_ids) |>
      ragnar::ragnar_retrieve(text, top_k = 10)
    retrieved_ids <<- unique(c(retrieved_ids, chunks$id))
    stringi::stri_c(
      "<excerpt>",
      chunks$text,
      "</excerpt>",
      sep = "\n",
      collapse = "\n"
    )
  }

  retrieve_tool <- ellmer::tool(
    rag_retrieve_tm_excerpts,
    glue::trim(
      "
      Use this tool to retrieve the most relevant excerpts from the Tidymodels
      knowledge store for a given text input. This function:
      - uses both vector (semantic) similarity and BM25 text search.
      - never returns the same excerpt twice in the same session; it always excludes recently retrieved IDs.
      - returns the results as plain text wrapped in <excerpt> tags.
      "
    ),
    text = ellmer::type_string()
  )
  client$register_tool(retrieve_tool)

  # Pre-set turns with a tool call if a short question is provided
  # (for both interactive and non-interactive modes)
  if (!is.null(question) && nchar(question) < 500) {
    initial_tool_request <- ellmer::ContentToolRequest(
      id = "init",
      name = "rag_retrieve_tm_excerpts",
      arguments = list(text = question),
      tool = retrieve_tool
    )
    client$add_turn(
      ellmer::Turn("user", contents = list(ellmer::ContentText(question))),
      ellmer::Turn("assistant", contents = list(initial_tool_request))
    )
    # next turn is the initial tool result
    question <-
      asNamespace("ellmer")$invoke_tool(initial_tool_request)
    initial_tool_request <- NULL
  }

  if (!interactive) {
    return(if (is.null(question)) client else client$chat(question))
  }

  initial_stream <-
    if (is.null(question)) NULL else client$stream_async(question)

  ui <- tmhelp_chat_ui(client)

  server <- function(input, output, session) {
    tmhelp_chat_server(
      "chat",
      client,
      initial_stream = initial_stream
    )
    shiny::observeEvent(input$close_btn, {
      shiny::stopApp()
    })
  }

  tryCatch(shiny::runGadget(ui, server), interrupt = function(cnd) NULL)
  invisible(client)
}


#' Shiny UI for Tidymodels Help Chat
#' @noRd
tmhelp_chat_ui <- function(client) {
  bslib::page_fillable(
    style = "display: flex; flex-direction: column; height: 100vh; padding: 0.5rem;",
    shiny::h1(
      "Tidymodels Help",
      style = "margin-bottom: 0.5rem; text-align: center;"
    ),
    shinychat::chat_mod_ui(
      "chat",
      client = client,
      height = "100%"
    ),
    shiny::actionButton(
      "close_btn",
      label = "",
      class = "btn-close",
      style = "position: fixed; top: 6px; right: 6px;"
    )
  )
}

#' Shiny Server for Tidymodels Help Chat (with Initial Stream)
#' @noRd
tmhelp_chat_server <- function(
  id,
  client,
  initial_stream = NULL
) {
  initial_stream # force

  append_stream_task <- shiny::ExtendedTask$new(
    function(client, ui_id, stream) {
      promises::then(
        promises::promise_resolve(stream),
        function(stream) {
          shinychat::chat_append(ui_id, stream)
        }
      )
    }
  )

  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(
      input$chat_user_input,
      {
        if (is.null(input$chat_user_input)) {
          stream <- initial_stream
          initial_stream <<- NULL
        } else {
          stream <- client$stream_async(input$chat_user_input)
        }
        append_stream_task$invoke(client, "chat", stream)
      },
      ignoreNULL = is.null(initial_stream)
    )

    shiny::reactive({
      if (append_stream_task$status() == "success") {
        client$last_turn()
      }
    })
  })
}
