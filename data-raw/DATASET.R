## code to prepare `DATASET` dataset goes here

as_received <-
  c(14, -104, -97, -59, -161, 93, 454, -341, 54, 137, 473, 45, 193, 22)

table_2 <-
  read.csv(
    'table_2.csv',
    header = F,
    row.names = NULL
    )

table_8 <-
  read.csv(
    'table_8.csv',
    header = F,
    row.names = NULL
  )

usethis::use_data(as_received, overwrite = TRUE)
usethis::use_data(table_2, overwrite = TRUE)
usethis::use_data(table_8, overwrite = TRUE)
