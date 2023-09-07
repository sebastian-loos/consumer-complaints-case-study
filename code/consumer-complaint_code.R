library(readr)
library(here)

data_fastfood_sales <- read_csv(here::here("data", "raw_data", "data_complaints_train.csv"))
data_fastfood_sales
data_fastfood_calories <- read_csv(here::here("data", "raw_data", "data_complaints_test.csv"))
data_fastfood_calories

save(data_fastfood_calories, data_fastfood_sales, file = here::here("data", "tidy_data", "project_5.rda"))
