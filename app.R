library(shiny)
library(pool)
library(odbc)
library(tibble)

# odbc_args <- list (
#   drv = odbc(),
#   dsn = "mars14_datav2",
#   uid = Sys.getenv("shiny_uid"),
#   pwd = Sys.getenv("shiny_pwd")
# )

odbc_args <- list(odbc(),
          Driver = "PostgreSQL Unicode",
          Server = "PWDMARSDBS1.pwd.phila.local", 
          port = 5434,
          Database = "Jon_sandbox",
          uid = Sys.getenv("shiny_uid"),
          pwd= Sys.getenv("shiny_pwd"))

# rpost_args <- list(
#   drv = RPostgres::Postgres(),
#   host = "PWDMARSDBS1",
#   port = 5434,
#   dbname = "mars_data",
#   user= Sys.getenv("shiny_uid"),
#   password = Sys.getenv("pwd")
# )

rpost_args <- list(RPostgres::Postgres(),
                      host = "PWDMARSDBS1.pwd.phila.local",
                      port = 5434,
                      dbname = "Jon_sandbox",
                      user = Sys.getenv("shiny_uid"),
                      password = Sys.getenv("shiny_pwd"))

# conn <- do.call(dbConnect, odbc_args)
# test <- dbGetQuery(conn, "SELECT * FROM public.tbl_liner_tests")

test_wr <- function(conn_args, text, platform, driver, length) {
  query <- write_q(text, platform, driver, length)
  conn <- do.call(dbConnect, conn_args)
  test_id <- dbGetQuery(conn, query)
  glimpse(test_id)
  get_query <- get_q(test_id)
  results <- dbGetQuery(conn, get_query)
  dbDisconnect(conn)
  results
}

test_update <- function(conn_args, test_id, char_nums) {
  fields <- names(char_nums)
  values <- char_nums
  query <- sprintf("UPDATE tbl_test_drivers SET %s WHERE %s",
                   paste(fields, values, sep = " = ", collapse = ", "),
                   paste("test_id = ", "'", test_id, "'", sep = ""))
  conn <- do.call(dbConnect, conn_args)
  dbGetQuery(conn, query)
  dbDisconnect
}

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
    test_id <- NULL
    if (config()$driver == "ODBC") {
      test <- test_wr(odbc_args, text_block, config()$platform, config()$driver, config()$length)
      updateTextAreaInput(inputId = "text", value = test$type_text)
      updateTextAreaInput(inputId = "varchar_d", value = test$type_varchar_default)
      updateTextAreaInput(inputId = "varchar_1000", value = test$type_varchar)
      test_id <<- test$test_id

    }
    else {
      test <- test_wr(rpost_args, text_block, config()$platform, config()$driver, config()$length)
      updateTextAreaInput(inputId = "text", value = test$type_text)
      updateTextAreaInput(inputId = "varchar_d", value = test$type_varchar_default)
      updateTextAreaInput(inputId = "varchar_1000", value = test$type_varchar)
      test_id <<- test$test_id
    }
  })
  
  observeEvent(input$test, {
    updateNumericInput(inputId = "text_length", value = stringr::str_length(input$text))
    updateNumericInput(inputId = "varchar_length", value = stringr::str_length(input$varchar_d))
    updateNumericInput(inputId = "varchar_1000_length", value = stringr::str_length(input$varchar_1000))
    char_counts <- tibble(
      actual_text_length = stringr::str_length(input$text),
      actual_varchar_d_length = stringr::str_length(input$varchar_d),
      actual_varchar_1000 = stringr::str_length(input$varchar_1000)
    )
    test_update(odbc_args, test_id, char_counts)
  })
   
} 

# Run the application 
shinyApp(ui = ui, server = server)
