## code to prepare `DATASET` dataset goes here

table_1 <-
  c(14, -104, -97, -59, -161, 93, 454, -341, 54, 137, 473, 45, 193, 22)

table_2 <-
  read.csv(
    'data-raw/table_2.csv',
    header = F,
    row.names = NULL
    )

table_2 <- as.matrix(table_2)


table_8 <-
  read.csv(
    'data-raw/table_8.csv',
    header = F,
    row.names = NULL
  )

table_8 <- as.matrix(table_8)

usethis::use_data(table_1, overwrite = TRUE)
usethis::use_data(table_2, overwrite = TRUE)
usethis::use_data(table_8, overwrite = TRUE)
