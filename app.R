library(shiny)
library(pool)
library(odbc)
library(tibble)

odbc_args <- list (
  drv = odbc(),
  dsn = "mars14_datav2",
  uid = Sys.getenv("shiny_uid"),
  pwd = Sys.getenv("shiny_pwd")
)

# odbc_args <- list(odbc(),
#           Driver = "PostgreSQL Unicode",
#           Server = "PWDMARSDBS1.pwd.phila.local", 
#           port = 5434,
#           Database = "Jon_sandbox",
#           uid = Sys.getenv("shiny_uid"),
#           pwd= Sys.getenv("shiny_pwd"))

rpost_args <- list(
  drv = RPostgres::Postgres(),
  host = "PWDMARSDBS1",
  port = 5434,
  dbname = "mars_data",
  user= Sys.getenv("shiny_uid"),
  password = Sys.getenv("pwd")
)

# rpost_args <- list(RPostgres::Postgres(),
#                       host = "PWDMARSDBS1.pwd.phila.local",
#                       port = 5434,
#                       dbname = "Jon_sandbox",
#                       user = Sys.getenv("shiny_uid"),
#                       password = Sys.getenv("shiny_pwd"))

# conn <- do.call(dbConnect, odbc_args)
# test <- dbGetQuery(conn, "SELECT * FROM public.tbl_liner_tests")
  
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
    # Platfrom data
    info <- tibble(
      "platform" = input$platform,
      "driver" = input$driver,
      "length" = input$length
    )
  })
  
  observeEvent(input$submit, {
    # Create a text box based on the legnth
    text_block <- strrep("Y", config()$length)
    test_id <- NULL
    if (config()$driver == "ODBC") {
      # Write to values to db and return test_id for ODBC
      test <- test_wr(odbc_args, text_block, config()$platform, config()$driver, config()$length)
      updateTextAreaInput(inputId = "text", value = test$type_text)
      updateTextAreaInput(inputId = "varchar_d", value = test$type_varchar_default)
      updateTextAreaInput(inputId = "varchar_1000", value = test$type_varchar)
      # Lazy use of super alignment
      test_id <<- test$test_id

    }
    else {
      # Write to values to db and return test_id for ODBC
      test <- test_wr(rpost_args, text_block, config()$platform, config()$driver, config()$length)
      updateTextAreaInput(inputId = "text", value = test$type_text)
      updateTextAreaInput(inputId = "varchar_d", value = test$type_varchar_default)
      updateTextAreaInput(inputId = "varchar_1000", value = test$type_varchar)
      test_id <<- test$test_id
    }
  })
  
  observeEvent(input$test, {
    # Update lengths
    updateNumericInput(inputId = "text_length", value = stringr::str_length(input$text))
    updateNumericInput(inputId = "varchar_length", value = stringr::str_length(input$varchar_d))
    updateNumericInput(inputId = "varchar_1000_length", value = stringr::str_length(input$varchar_1000))
    # Ready actual lengths to write DB
    char_counts <- tibble(
      actual_text_length = stringr::str_length(input$text),
      actual_varchar_d_length = stringr::str_length(input$varchar_d),
      actual_varchar_1000 = stringr::str_length(input$varchar_1000)
    )
    # Update records in DB
    if (config()$driver == "ODBC") {
    test_update(odbc_args, test_id, char_counts)
    } else {
      test_update(rpost_args, test_id, char_counts)
    }
  })
   
} 

# Run the application 
shinyApp(ui = ui, server = server)
