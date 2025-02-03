library(shiny)
library(pool)
library(odbc)

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

conn <- do.call(dbConnect, rpost_args)



test <- dbGetQuery(conn, "SELECT * FROM public.tbl_liner")
  
# Define UI for application that draws a histogram
ui <- fluidPage(
  absolutePanel(tags$h1("Test Drivers"),
                selectInput("platform",
                            label = "Platform",
                            choices = c("Local-Windows",
                                        "Local-Linux",
                                        "Posit Connect",
                                        "Workbench")))

   
)

# Define server logic required to draw a histogram
server <- function(input, output) {

   
}

# Run the application 
shinyApp(ui = ui, server = server)
