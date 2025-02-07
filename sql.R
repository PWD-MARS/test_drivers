# Write to DB
test_wr <- function(conn_args, text, platform, driver, length) {
  # Query
  query <- write_q(text, platform, driver, length)
  # Connection to DB
  conn <- do.call(dbConnect, conn_args)
  # Send to DB and return test_id
  test_id <- dbGetQuery(conn, query)
  # Return query to updateTextAreaInput
  get_query <- get_q(test_id)
  results <- dbGetQuery(conn, get_query)
  # Disconnect from DB
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