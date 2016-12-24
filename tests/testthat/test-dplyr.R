context("dplyr backend")

suppressPackageStartupMessages({
  library(DBI)
  library(dplyr)
})

ss <- src_sqlserver("TEST", database = "DBItest")

df1 <- data_frame(a = c(1, 2), b = c("a", "b"))
df2 <- data_frame(a = letters, b = 1:26)
df3 <- data_frame(a = letters, c = rev(letters))
df4 <- data_frame(a = rep_len(letters, 200), b = 1:200)

test_that("src creation works", {
  expect_s3_class(ss, "src_sqlserver")
  expect_s4_class(con_acquire(ss), "SQLServerConnection")
  expect_named(ss$info, c("username", "host", "port", "dbname", "db.version"))
})

# Remove tables from last run of tests. This prevents tests failing in case
# previous invocation of tests failed before df could be dropped
dbExecute(con_acquire(ss), "DROP TABLE IF EXISTS DF1")
dbExecute(con_acquire(ss), "DROP TABLE IF EXISTS DF2")
dbExecute(con_acquire(ss), "DROP TABLE IF EXISTS DF3")
dbExecute(con_acquire(ss), "DROP TABLE IF EXISTS DF4")

test_that("copy_to works", {
  expect_error(copy_to(ss, df1, temporary = FALSE), NA)
  expect_error(copy_to(ss, df2, temporary = FALSE), NA)
  expect_error(copy_to(ss, df3, temporary = FALSE), NA)
  expect_error(copy_to(ss, df4, temporary = FALSE), NA)
  dbExecute(con_acquire(ss), "DROP TABLE IF EXISTS DF1")
  expect_error(copy_to(ss, df1, "df1", temporary = FALSE), NA)
  expect_error(copy_to(ss, df1, random_table_name(temp = TRUE)), NA)
  expect_error(copy_to(ss, df1, random_table_name(temp = TRUE),
    types = c("FLOAT", "NVARCHAR(2)")), NA)
  expect_error(copy_to(ss, df1, random_table_name(temp = TRUE),
    indexes = list("a")), NA)
  expect_error(copy_to(ss, df1, random_table_name(temp = TRUE),
    unique_indexes = list("a")), NA)
})

df1_tbl <- tbl(ss, "df1")
df2_tbl <- tbl(ss, "df2")
df3_tbl <- tbl(ss, "df3")
df4_tbl <- tbl(ss, "df4")

test_that("tbl creation works", {
  expect_s3_class(df1_tbl, "tbl_sqlserver")
})

test_that("collect works", {
  expect_equal(df1_tbl %>% collect(), df1)
})

test_that("select verb works", {
  expect_equal(select(df1_tbl, a) %>% collect(),
    data_frame(a = c(1, 2)))
})

test_that("filter verb works", {
  expect_equal(filter(df1_tbl, a > 1) %>% collect(),
    data_frame(a = 2, b = "b"))
})

test_that("arrange verb works", {
  expect_equal(arrange(df1_tbl, -a) %>% collect(),
    data_frame(a = c(2, 1), b = c("b", "a")))
  expect_equal(arrange(df4_tbl, a) %>% collect() %>% nrow(), 200)
})

test_that("mutate verb works", {
  expect_equal(mutate(df1_tbl, c = as.character(a) %+% b) %>% collect(),
    data_frame(a = c(1, 2), b = c("a", "b"), c = c("1a", "2b")))
})

test_that("summarise verb works", {
  expect_equal(summarise(df1_tbl, a_ave = mean(a)) %>% collect(),
    data_frame(a_ave = 1.5))
})

test_that("explain works", {
  expect_message(explain(df1_tbl))
})

test_that("setops works", {
  expect_error(setdiff(df1_tbl, df1_tbl), NA)
  expect_error(intersect(df1_tbl, df1_tbl), NA)
  expect_error(union(df1_tbl, df1_tbl), NA)
})

test_that("join works", {
  expect_error(df2_tbl %>% left_join(df3_tbl), NA)
  expect_error(df3_tbl %>% right_join(df2_tbl), NA)
  expect_error(df2_tbl %>% full_join(df3_tbl), NA)
  expect_error(df2_tbl %>% semi_join(df3_tbl), NA)
  expect_error(df2_tbl %>% anti_join(df3_tbl), NA)
})

# Remove tables from last run of tests. This prevents tests failing in case
# previous invocation of tests failed before df could be dropped
dbExecute(con_acquire(ss), "DROP TABLE IF EXISTS DF1")
dbExecute(con_acquire(ss), "DROP TABLE IF EXISTS DF2")
dbExecute(con_acquire(ss), "DROP TABLE IF EXISTS DF3")
dbExecute(con_acquire(ss), "DROP TABLE IF EXISTS DF4")
