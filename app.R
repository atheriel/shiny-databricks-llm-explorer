library(shiny)
library(shinychat)
library(ellmer)

workspace <- Sys.getenv("DATABRICKS_HOST")

models <- tibble::tibble(
  title = c(
    "DBRX Instruct",
    "Mixtral-8x7B Instruct",
    "Meta Llama 3.1 70B Instruct",
    "Meta Llama 3.1 405B Instruct"
  ),
  model = c(
    "databricks-dbrx-instruct",
    "databricks-mixtral-8x7b-instruct",
    "databricks-meta-llama-3-1-70b-instruct",
    "databricks-meta-llama-3-1-405b-instruct"
  ),
  description = c(
    "a state-of-the-art mixture of experts language model trained by Databricks",
    "a state-of-the-art 8x7B parameter sparse mixture of experts language model trained by Mistral AI",
    "a state-of-the-art 70B parameter dense language model trained and released by Meta",
    "a state-of-the-art 405B parameter dense language model trained and released by Meta"
  ),
  link = c(
    "https://docs.databricks.com/en/machine-learning/foundation-models/supported-models.html#dbrx-instruct",
    "https://docs.databricks.com/en/machine-learning/foundation-models/supported-models.html#mixtral-8x7b-instruct",
    "https://docs.databricks.com/en/machine-learning/foundation-models/supported-models.html#meta-llama-31-70b-instruct",
    "https://docs.databricks.com/en/machine-learning/foundation-models/supported-models.html#meta-llama-31-405b-instruct"
  )
)

# Module UI generator for individual chats.
chat_panel_ui <- function(id, initial_model = NULL) {
  ns <- NS(id)
  bslib::card(
    id = id,
    bslib::card_header(
      bslib::popover(
        actionButton(
          ns("show_compare"),
          label = bsicons::bs_icon("gear", title = "Chat settings"),
          class = "btn-light btn-sm m-0 py-0 border-0"
        ),
        selectInput(
          ns("model"),
          "Model",
          choices = structure(models$model, names = models$title),
          selected = initial_model
        ),
        textInput(
          ns("system_prompt"),
          "System prompt",
          value = "You are a helpful assistant."
        ),
        numericInput(
          ns("max_tokens"),
          "Per-response token limit",
          value = 500
        ),
        title = "Chat settings",
        placement = "bottom"
      ),
      uiOutput(ns("model_title"), inline = TRUE),
      div(
        class = "btn-toolbar",
        role = "toolbar",
        bslib::tooltip(
          actionButton(
            ns("remove_chat"),
            label = bsicons::bs_icon("dash-circle", title = "Remove chat"),
            class = "btn-light btn-sm m-0 py-0 border-0",
            disabled = TRUE
          ),
          "Remove chat",
          placement = "bottom"
        ),
        bslib::tooltip(
          actionButton(
            ns("add_chat"),
            label = bsicons::bs_icon("plus-circle", title = "Add chat"),
            class = "btn-light btn-sm m-0 py-0 border-0"
          ),
          "Add chat",
          placement = "bottom"
        )
      ),
      class = "d-flex justify-content-between bg-light"
    ),
    chat_ui(ns("chat"), width = "auto"),
    style = "width: min(700px,100%); margin: auto"
  )
}

# Module server generator for individual chats.
chat_panel_server <- function(id, add_panel) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    chat <- NULL

    details <- reactive({
      if (!is.null(input$model)) {
        models[models$model == input$model,]
      } else {
        NULL
      }
    })

    output$model_title <- renderUI(paste("Model:", details()$title))

    bindEvent(
      observe({
        chat <<- elmer::chat_databricks(model = input$model)
        chat_append(ns("chat"), glue::glue(
          "👋 Hi, I'm an assistant backed by [{details()$title}]({details()$link}),
          {details()$description}.

          How can I help you?"
        ))
      }),
      details,
      ignoreNULL = TRUE,
      once = TRUE
    )

    observeEvent(input$chat_user_input, {
      chat_append(ns("chat"), chat$stream_async(input$chat_user_input))
    })

    observeEvent(input$system_prompt, {
      chat$set_system_prompt(input$system_prompt)
    })

    observeEvent(input$model, {
      chat_append(ns("chat"), glue::glue("Switching to the `{input$model}` model."))
      # There's no "set_model()" API for a chat. So instead, swap out the whole
      # object, passing the existing turns and system prompt back in.
      chat <<- elmer::chat_databricks(
        model = input$model,
        system_prompt = input$system_prompt,
        turns = chat$get_turns()
      )
    }, ignoreInit = TRUE)

    bindEvent(
      observe({
        idx <- add_panel()
        add_panel(idx + 1L)
      }),
      input$add_chat
    )
  })
}

ui <- bslib::page_fillable(
  div(
    h2("Databricks LLM Explorer"),
    markdown(glue::glue(
      "Similar to the [Databricks AI Playground](https://docs.databricks.com/en/large-language-models/ai-playground.html),
      you can use this Shiny app to test, prompt, and explore models in a
      chat-like interface. Models are served directly from [your private
      Databricks workspace]({workspace}) {bsicons::bs_icon('box-arrow-up-right')}."
    )),
    class = "px-5 text-center"
  ),
  bslib::layout_column_wrap(
    id = "chats",
    chat_panel_ui("chat1"),
    width = "500px"
  ),
  title = "Databricks LLM Explorer",
  lang = "en"
)

server <- function(input, output, session) {
  add_panel <- reactiveVal(1L)
  chat_panel_server("chat1", add_panel)

  bindEvent(
    observe({
      idx <- add_panel()
      id <- paste0("chat", idx)
      insertUI(
        "#chats",
        where = "beforeEnd",
        div(
          class = "bslib-grid-item bslib-gap-spacing html-fill-container",
          chat_panel_ui(id, initial_model = sample(models$model, 1))
        )
      )
      chat_panel_server(id, add_panel)
    }),
    add_panel(),
    ignoreInit = TRUE
  )
}

shinyApp(ui, server)
