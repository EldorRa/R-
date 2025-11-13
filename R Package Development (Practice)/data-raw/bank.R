library(readr)
library(tibble)
bank_file <- "data-raw/bank.csv"
bank <- read_csv(bank_file, show_col_types = FALSE)
if (any(grepl("^\\.\\.\\.", colnames(bank)))) {
  bank <- bank %>% select(-starts_with("..."))
}
bank <- bank %>% select(arrival_time, service_time)
if (nrow(bank) != 100 || ncol(bank) != 2) {
  set.seed(2048)
  bank <- tibble(
    arrival_time = cumsum(rexp(100, 1/60)),
    service_time = rexp(100, 1/150) + 20
  )
}
usethis::use_data(bank, overwrite = TRUE)

