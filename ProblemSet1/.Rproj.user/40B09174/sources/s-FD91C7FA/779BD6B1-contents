# Setup ----------------------------------------------------------------------------------
# Load packages, read full data
library(pacman)
p_load(tidyverse, readxl, haven, data.table, broom, magrittr, here, ipumsr)
ddi <- read_ipums_ddi("PS1_data/usa_00005.xml")
data <- read_ipums_micro(ddi)

#softmax function to find percentage weighting scheme with no distributional assumptions
data %<>% mutate(perwt_nrm = (PERWT - min(PERWT))/max(PERWT),perwt_sftmx= exp(perwt_nrm)/sum(exp(perwt_nrm))) %>% filter(AGE > 23)

# Load data ------------------------------------------------------------------------------
# Load ACS data
acs_raw = data
names(acs_raw) = tolower(names(acs_raw))
# Grab variable labels
var_desc = lapply(acs_raw, function(x) attributes(x)$label)
# Load ERS data
ers_raw = here("PS1_data/ers-rural-urban-2013.xls") %>% read_xls(sheet = 1)


# Data work: Prep ERS data ---------------------------------------------------------------
# Grab and name desired columns
ers_clean = transmute(
  ers_raw,
  fips = FIPS,
  i_urban = 1 * (RUCC_2013 <= 5)
)
setDT(ers_clean)

# Data work: upsample rural weights
setDT(acs_raw)
acs_raw[, `:=`(fips = paste0(str_pad(statefip, 2, "left", 0), str_pad(countyfip, 3, "left", 0)))]

acs_raw = merge(
  x = ers_clean,
  y = acs_raw,
  by = "fips",
  all.x = F,
  all.y = T
)

# Fix missingness from small areas (assuming rural)
acs_raw[is.na(i_urban), i_urban := .5]

acs_raw[, `:=`(perwt_sftmx = perwt_sftmx+(i_urban*-sd(acs_raw$perwt_sftmx)))] 

acs_raw[i_urban == .5, i_urban := 0]

# Data work: Clean ACS data --------------------------------------------------------------
# Convert data into data table
 

# Subset to individuals with non-zero commute times
# Change depart and arrive time to minutes past midnight
acs_raw[, `:=`(
  time_depart = str_pad(departs, 4, "left", 0),
  time_arrive = str_pad(arrives, 4, "left", 0)
)]
acs_raw[, `:=`(
  time_depart =
    as.numeric(str_sub(time_depart, 1, 2)) * 60 + as.numeric(str_sub(time_depart, 3, 4)),
  time_arrive =
    as.numeric(str_sub(time_arrive, 1, 2)) * 60 + as.numeric(str_sub(time_arrive, 3, 4))
)]
# Drop observations that arrive before they depart
# NOTE: Includes people right around midnight
acs_raw %<>% .[time_arrive - time_depart > 0]
# Indicators for race and sex
acs_clean = acs_raw[, .(
  fips = paste0(str_pad(statefip, 2, "left", 0), str_pad(countyfip, 3, "left", 0)),
  age = age,
  i_urban = i_urban,
  i_citizen = 1*(citizen %in% c(0,1,2)),
  i_noenglish = 1*(speakeng %in% c(1,6)),
  i_only_english = 1*(speakeng == 3),
  i_drive_to_work = 1*(tranwork %in% c(10:15, 20, 38)),
  i_asian = 1 * (race %in% 4:6),
  i_black = 1 * (race == 2),
  i_indigenous = 1 * (race == 3),
  i_white = 1 * (race == 1),
  i_female = 1 * (sex == 2),
  i_male = 1 * (sex == 1),
  i_grad_college = 1 * (educd %in% c(101,114,115,116)),
  i_grad_highschool = 1*(educd >= 62),
  i_married = 1 * (marst %in% c(1:2)),
  i_married_mult = 1 * (marrno %in% c(2:6)),
  personal_income = ifelse(inctot != 9999999, inctot, NA) / 1e4,
  i_health_insurance = 1 * (hcovany == 2),
  i_internet = fcase(
    cinethh %in% c(1,2), 1,
    cinethh == 3, 0
  ),
  time_depart,
  time_arrive,
  time_commuting = time_arrive - time_depart,
  weights = perwt_sftmx
)]
# Restrict to individuals with positive income (for logs in assignment)
acs_clean %<>% .[personal_income > 0]

# Data work: Join, sample, and clean -----------------------------------------------------
acs_sub = sample_n(acs_clean, size = 8000, weight = weights)
# Add state names
acs_sub[, fips := fips %>% str_sub(1, 2)]
setnames(acs_sub, old = "fips", new = "state_fips")
fips_xwalk = maps::state.fips %>% transmute(
  state_fips = fips %>% str_pad(2, "left", 0),
  state = abb
) %>% unique() %>% setDT()
acs_sub = merge(
  x = fips_xwalk,
  y = acs_sub,
  by = "state_fips",
  all.x = F,
  all.y = T
)
acs_sub[, state_fips := NULL]

#save data
readr::write_rds(
  acs_sub,
  "ps-001-data.rds"
)
readr::write_csv(
  acs_sub,
  "ps-001-data.csv"
)

# Assignment -----------------------------------------------------------------------------
# Linear: Income
lm(
  time_commuting ~ personal_income,
  data = acs_sub
) %>% tidy()
lm(
  time_depart ~ personal_income,
  data = acs_sub
) %>% tidy()
# Log-linear
lm(
  log(time_commuting) ~ personal_income,
  data = acs_sub
) %>% tidy()
# Log-log
lm(
  log(time_commuting) ~ log(personal_income),
  data = acs_sub
) %>% tidy()
# Indicators
lm(
  time_depart ~ i_white + i_female,
  data = acs_sub
) %>% tidy()
# Adding a control: How does it change the interpretation?
lm(	
  time_depart ~ i_white + i_female + i_urban,
  data = acs_sub
) %>% tidy()
# Add interaction
lm(	
  time_depart ~ i_white * i_female,
  data = acs_sub
) %>% tidy()
lm(	
  time_depart ~ i_female * i_married,
  data = acs_sub
) %>% tidy()
# Subset to child-bearing age
lm(	
  time_depart ~ i_female * i_married,
  data = acs_sub %>% filter(between(age, 12, 51))
) %>% tidy()
# Indicator as outcome
lm(
  i_health_insurance ~ personal_income,
  data = acs_sub
) %>% tidy()
lm(
  i_health_insurance ~ I(personal_income < 3),
  data = acs_sub
) %>% tidy()
# Add ethnicity
lm(
  i_internet ~ I(personal_income < 3) + i_black,
  data = acs_sub
) %>% tidy()
# Interaction
lm(
  i_internet ~ I(personal_income < 3) * i_urban + i_citizen,
  data = acs_sub
) %>% tidy()
# Omitted variables?
#interact i_citizen w/ no english
lm(
  i_internet ~ I(personal_income < 3) * i_urban + i_black + I(age > 50) + i_citizen*i_noenglish,
  data = acs_sub
) %>% tidy()
