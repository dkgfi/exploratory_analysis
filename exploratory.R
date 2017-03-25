#' open sourced data from various countries (usa, japan, etc)
#' The Netherlands, Ethiopia, Ghana, Kenya, Nigeria, South Africa

library(tidyverse)

# set working directory to the folder containing 
setwd("Documents/code/gfi/")

# read in sample datasets
sampleData <- map(dir('trade_samples_by_country/', full.names = TRUE), read_csv)

# give it naaaaaaaaames
names(sampleData) <- gsub("\\.csv", "", dir("trade_samples_by_country/"))

# save individual dataframes
list2env(sampleData,envir=.GlobalEnv)

#' createa  subset of data for anlaysis:
#'     Focus on exported (Ethiopia), 
#'     AND ONLY where the currency is in EUROs, 
#'     AND where quantity is not equal to 0
#'     
#' I realize that it's not generally a good idea to throw out cases, but
#' there was no time to look for fixes.
exports <- ethiopia %>%
  mutate(per_unit_price = value / quantity_1) %>%
  filter(grepl('f', is_import)) %>%
  filter(grepl('EUR', currency)) %>%
  filter(quantity_1 != 0 & value != 0)

#' let's ensure that there is only one distinct unit_id for each
#' commodity id
unitIsDistinct <- exports %>%
  group_by(commodity_id) %>%
  summarise(distinct_unit_1_ids = n_distinct(unit_1_id))

exports %>% 
  count(commodity_id, unit_1_id) %>%
  count(commodity_id) %>% 
  xtabs(~ nn, data = .)

#' we're going to compare `per_unit_price` to it's corresponding
#' median per_unit_price, on a `commodity_id` level, so aggregate
#' the median, average, and standard deviation. We're going to
#' exclude commodities with only one record.
commMedian <- exports %>%
  group_by(commodity_id) %>%
  summarise(med_per_unit_price = median(per_unit_price),
            avg_per_unit_price = mean(per_unit_price, na.rm=T),
            sd_per_unit_price = sd(per_unit_price, na.rm=T),
            base = n()) %>%
  ungroup() %>%
  filter(base > 1)
  
#' Join in the commodity level aggregates to the `exports` dataset.
#' Then we can compare each record's `per_unit_price` to the median
#' price. We're going to calculate an upper and lower bound based 
#' on `nSds` (the number of standard deviations that the record is
#' from the median)

nSds <- 2

exports2 <- exports %>%
  inner_join(., commMedian, by='commodity_id') %>%
  mutate(diff_from_median = per_unit_price - med_per_unit_price,
         upper_bound= med_per_unit_price + nSds*sd_per_unit_price,
         lower_bound = med_per_unit_price - nSds*sd_per_unit_price,
         is_outside_band =ifelse(per_unit_price > upper_bound |
                              per_unit_price < lower_bound, 1, 0)
           )

#' is_outside_band:
#'     for each unique commodity_id, we calculated the median per-value
#'     unit cost for export transactions in ethiopia. We took this value
#'     to be the expected per unit cost, and compared it to the
#'     per_unit_cost of each transaction. `is_outside_band` is a binary
#'     flag indicating whether a given transaction is +/- 1 standard
#'     deviation from the median.
