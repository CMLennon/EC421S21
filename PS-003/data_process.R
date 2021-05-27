library(pacman)
p_load(stats, tidyverse, data.table, lubridate, magrittr, forecast, seasonal)

gas <- read_csv("NatGas.csv") %>% as.data.table()
oil <- read_csv("HeatingOil.csv") %>% as.data.table()
prop <- read_csv("PropanePrices.csv") %>% as.data.table()

names(gas) = c('month_year', 'price_gas')
names(oil) = c('month_year', 'price_oil')
names(prop) = c('month_year', 'price_prop')

oil <- oil[!is.na(price_oil)]
gas <- gas[!is.na(price_gas)]
prop <- prop[!is.na(price_prop)]

oil = oil[, month_year := month_year %>% mdy() %>% floor_date("month")]
gas = gas[, month_year := month_year %>% mdy() %>% floor_date("month")]
prop =prop[, month_year := month_year %>% mdy() %>% floor_date("month")]

oil = oil[,list(price_oil = mean(price_oil)), by = month_year]
gas = gas[,list(price_gas = mean(price_gas)), by = month_year]
prop = prop[,list(price_prop = mean(price_prop)), by = month_year]

oil[oil[,.(month_year = seq.Date(min(month_year), max(month_year), by = "day"))], on = .(month_year)]
gas[gas[,.(month_year = seq.Date(min(month_year), max(month_year), by = "day"))], on = .(month_year)]
prop[prop[,.(month_year = seq.Date(min(month_year), max(month_year), by = "day"))], on = .(month_year)]

oil = na.interp(oil)
gas = na.interp(gas)
prop = na.interp(prop)


price_df <- merge(gas, oil, by = "month_year", all = F)
price_df <- merge(price_df, prop, by = 'month_year', all = F)
price_df[, `:=`(
  month = month(month_year),
  year = year(month_year),
  t_month = 1:.N
)]
price_df = price_df[, t := year + (month-1)/12] %>% filter(year > 2000)

price_df = price_df %>% mutate(price_oil = price_oil + price_gas*.0223443247836392)

write_csv(price_df, "ps03_data.csv")
