test_wr <- function(connection, text, platform, driver, length){
  query <- write_q(text, platform, driver, length)
  test_id <- dbGetQuery(connection, query)
  glimpse(test_id)
  get_query <- get_q(test_id)
  results <- dbGetQuery(connection, get_query)
  glimpse(results)
  results
}