context("dplyr backend")

suppressPackageStartupMessages({
  library(DBI)
  library(dplyr)
})

ss <- src_sqlserver("TEST", database = "DBItest")
df1 <- data_frame(a = c(1, 2), b = c("a", "b"))

test_that("src creation works", {
  expect_s3_class(ss, "src_sqlserver")
  expect_s4_class(ss$con, "SQLServerConnection")
  expect_named(ss$info, c("username", "host", "port", "dbname", "db.version"))
})

# Remove tables from last run of tests. This prevents tests failing in case
# previous invocation of tests failed before df could be dropped
dbExecute(ss$con, "DROP TABLE IF EXISTS DF")
dbExecute(ss$con, "DROP TABLE IF EXISTS DF1")

test_that("copy_to works", {
  expect_error(copy_to(ss, df1, temporary = FALSE), NA)
  dbExecute(ss$con, "DROP TABLE IF EXISTS DF1")
  expect_error(copy_to(ss, df1, "df1", temporary = FALSE), NA)
  expect_error(copy_to(ss, df1, random_table_name(temp = TRUE)), NA)
  expect_error(copy_to(ss, df1, random_table_name(temp = TRUE),
    types = c("FLOAT", "NVARCHAR(2)")), NA)
  expect_error(copy_to(ss, df1, random_table_name(temp = TRUE),
    indexes = list("a")), NA)
  expect_error(copy_to(ss, df1, random_table_name(temp = TRUE),
    unique_indexes = list("a")), NA)
})

df_tbl <- tbl(ss, "df1")

test_that("tbl creation works", {
  expect_s3_class(df_tbl, "tbl_sqlserver")
})

test_that("collect works", {
  expect_equal(df_tbl %>% collect(), df1)
})

test_that("select verb works", {
  expect_equal(select(df_tbl, a) %>% collect(),
    data_frame(a = c(1, 2)))
})

test_that("filter verb works", {
  expect_equal(filter(df_tbl, a > 1) %>% collect(),
    data_frame(a = 2, b = "b"))
})

test_that("arrange verb works", {
  expect_equal(arrange(df_tbl, -a) %>% collect(),
    data_frame(a = c(2, 1), b = c("b", "a")))
})

test_that("mutate verb works", {
  expect_equal(mutate(df_tbl, c = as.character(a) %+% b) %>% collect(),
    data_frame(a = c(1, 2), b = c("a", "b"), c = c("1a", "2b")))
})

test_that("summarise verb works", {
  expect_equal(summarise(df_tbl, a_ave = mean(a)) %>% collect(),
    data_frame(a_ave = 1.5))
})

test_that("explain works", {
  expect_message(explain(df_tbl))
})

test_that("setops works", {
  expect_error(setdiff(df_tbl, df_tbl), NA)
  expect_error(intersect(df_tbl, df_tbl), NA)
  expect_error(union(df_tbl, df_tbl), NA)
})

test_that("join works", {
  flights <- tbl(ss, "flights")
  airlines <- tbl(ss, "airlines")
  planes <- tbl(ss, "planes")
  expect_error(flights %>% left_join(airlines), NA)
  expect_error(airlines %>% right_join(flights), NA)
  expect_error(flights %>% full_join(planes), NA)
  expect_error(flights %>% semi_join(planes), NA)
  expect_error(flights %>% anti_join(planes), NA)
})
