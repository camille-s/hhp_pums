library(tidyverse)

hhp_group <- readRDS(file.path("output_data", "hhp_by_group.rds"))
hhp_trend <- readRDS(file.path("output_data", "hhp_trends.rds"))
dates <- read_csv(file.path("_utils", "pums_week_dates.csv")) %>%
  mutate(week = as.character(week))

collapse_dates <- function(df) {
  df %>%
    separate_rows(wks_incl) %>%
    left_join(dates, by = c("wks_incl" = "week")) %>%
    pivot_longer(ends_with("_date"), names_to = "time", values_to = "date") %>%
    group_by_at(vars(-wks_incl, -time, -date)) %>%
    summarise(date_range = paste(range(date), collapse = "_")) %>%
    ungroup()
}

# for now, just doing food, housing, renters
list(
  food_insecurity = hhp_group$food_insecurity,
  housing_insecurity = hhp_group$housing_insecurity,
  rent_insecurity = hhp_group$renter_insecurity,
  loss_of_work = hhp_group$work_loss
) %>%
  map(filter, name == "CT" & dimension %in% c("total", "race", "age_range", "income", "kids_present")) %>%
  map(collapse_dates) %>%
  map(select, -name, -share_se) %>%
  map(arrange, dimension, group) %>%
  cwi::batch_csv_dump(path = "to_site", base_name = "hhp_group")
