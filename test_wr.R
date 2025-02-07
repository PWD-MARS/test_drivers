test_wr <- function(text, platform, driver, length){
  query <- write_q(text, platform, driver, length)
  conn <- do.call(dbConnect, odbc_args)
  test_id <- dbGetQuery(conn, query)
  dbDisconnect(conn)
  glimpse(test_id)
  get_query <- get_q(test_id)
  conny <- do.call(dbConnect, odbc_args)
  results <- dbGetQuery(conn = conny, statement = get_query)
  dbDisconnect(conny)
  glimpse(data_from_db)
}