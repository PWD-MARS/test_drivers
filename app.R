library(shiny)
library(pool)
library(odbc)
library(tibble)

odbc_args <- list (
  drv = odbc(),
  dsn = "Jon_sandbox",
  uid = Sys.getenv("shiny_uid"),
  pwd = Sys.getenv("pwd")
)

rpost_args <- list(
  drv = RPostgres::Postgres(),
  host = "PWDMARSDBS1",
  port = 5434,
  dbname = "Jon_sandbox",
  user= Sys.getenv("shiny_uid"),
  password = Sys.getenv("pwd")
)

# conn <- do.call(dbConnect, rpost_args)
# test <- dbGetQuery(conn, "SELECT * FROM public.tbl_liner_tests")

source("test_wr.R")

write_q <- function(text_block, platform, driver, length) {
  df <- tribble(
    ~type_text, ~type_varchar_default, ~type_varchar, ~platform, ~driver, ~requested_length,
    text_block, text_block, text_block, platform, driver, length
  )
  fields <- names(df)
  query <- sprintf("INSERT INTO %s (%s) VALUES (%s) RETURNING test_id",
                   "tbl_test_drivers",
                   paste(fields, collapse = ", "),
                   paste("'", df, "'", collapse = ", ", sep = "") 
                   )
}

get_q <- function(test_id) {
  query <- sprintf("SELECT * FROM tbl_test_drivers WHERE test_id = %s",
                   test_id)
}

test <- NULL

  
# Define UI for application that draws a histogram
ui <- fluidPage(
  absolutePanel(tags$h1("Test Drivers"),
                selectInput("platform",
                            label = "Platform",
                            choices = c("Windows",
                                        "Linux",
                                        "Posit Connect",
                                        "Workbench")),
                selectInput(inputId = "driver",
                            label = "Driver",
                            choices = c("ODBC",
                                        "RPostgres")),
                numericInput(inputId = "length",
                            label = "Text Length",
                            value = 257),
                actionButton(inputId = "submit",
                             label = "Submit"),
                actionButton(inputId = "test",
                             label = "Test"),
                numericInput(inputId = "text_length",
                             label = "Text Length",
                             value = ""),
                numericInput(inputId = "varchar_length",
                             label = "Varchar Length",
                             value = ""),
                numericInput(inputId = "varchar_1000_length",
                             label = "Varchar 1000 Length",
                             value = ""),
                textAreaInput(inputId = "text",
                              label = "text"),
                textAreaInput(inputId = "varchar_d",
                              label = "Varchar Default"),
                textAreaInput(inputId = "varchar_1000",
                              label = "Varchar 1000")
                ),
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  config <- reactive({
    info <- tibble(
      "platform" = input$platform,
      "driver" = input$driver,
      "length" = input$length
    )
  })
  
  observeEvent(input$submit, {
    text_block <- strrep("Y", config()$length)
    if (config()$driver == "ODBC") {
      query <- write_q(text_block, config()$platform, config()$driver, config()$length)
      conn <- do.call(dbConnect, odbc_args)
      test_id <- dbGetQuery(conn, query)
      glimpse(test_id)
      get_query <- get_q(test_id)
      results <- dbGetQuery(conn = conn, statement = get_query)
      dbDisconnect(conn)

      #updateTextAreaInput(inputId = text, value = )
      test <<- results
    }
  })
  
  observeEvent(input$test, {
    updateNumericInput(inputId = "text", value = stringr::str_length(input$text))
  })
   
} 

# Run the application 
shinyApp(ui = ui, server = server)
