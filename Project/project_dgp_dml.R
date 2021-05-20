# Setup ----------------------------------------------------------------------------------
# Load packages, read full data
library(pacman)
p_load(tidymodels, tidyverse, readxl, haven, data.table, broom, magrittr, here, ipumsr)
ddi <- read_ipums_ddi("usa_00010.xml")
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
ers_raw = here("ers-rural-urban-2013.xls") %>% read_xls(sheet = 1)


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

doubleml <- function(data) {
  
  ### STEP 1: split X, W and Y into 2 random sets
  data = sample_n(data, 30000)
  data %<>% mutate(educ = as.numeric(educ), sei = as.numeric(sei), hwsei = as.numeric(hwsei), presgl = as.numeric(presgl), prent = as.numeric(prent), incwage = as.numeric(incwage))
  split <- initial_split(data, prop = .5)
  
  dat1 = training(split)
  dat2 = testing(split)
  
  dat_out = rbind(dat1, dat2)
  
  ### STEP 2a: use a SuperLearner to train a model for E[X|W] on set 1 and predict X on set 2 using this model. Do the same but training on set 2 and predicting on set 1
  dmlrecipe1 = recipe(educ ~ sei + hwsei + presgl + prent, data = dat1)
  dmlrecipe2 = recipe(educ ~ sei + hwsei + presgl + prent, data = dat2)
  
  tune_spec <- rand_forest(
    mtry = 3,
    trees = 1000,
    min_n = tune()
  ) %>%
    set_mode("regression") %>%
    set_engine("ranger")
  
  dml1wf = workflow() %>% add_recipe(dmlrecipe1) %>% add_model(tune_spec)
  dml2wf = workflow() %>% add_recipe(dmlrecipe2) %>% add_model(tune_spec)
  
  dat1folds = vfold_cv(dat1, 5)
  dat2folds = vfold_cv(dat2, 5)
  
  tune_res1 = tune_grid(
    dml1wf,
    resamples = dat1folds,
    grid = 5
  ) %>% select_best(maximize = FALSE)
  
  tune_res2 = tune_grid(
    dml2wf,
    resamples = dat2folds,
    grid = 5
  ) %>% select_best(maximize = FALSE)
  
  tune_best1 = dml1wf %>% finalize_workflow(tune_res1) %>% fit(dat1)
  tune_best2 = dml2wf%>% finalize_workflow(tune_res1) %>% fit(dat2)

  
  predict1 = predict(tune_best2, dat1 %>% data.table())
  predict2 = predict(tune_best1, dat2 %>% data.table())
  dat_out$educ_hat = rbind(predict1, predict2)
  ### STEP 2b: get the residuals X - X_hat on set 2 and on set 1
  dat_out$educ_resid = rbind(dat1$educ - predict1, dat2$educ - predict2)
  
  ### STEP 3a: use a SuperLearner to train a model for E[Y|W] on set 1 and predict Y on set 2 using this model. Do the same but training on set 2 and predicting on set 1
  dmlrecipe1 = recipe(incwage ~ sei + hwsei + presgl + prent, data = dat1)
  dmlrecipe2 = recipe(incwage ~ sei + hwsei + presgl + prent, data = dat2)
  
  tune_spec <- rand_forest(
    mtry = 3,
    trees = 100,
    min_n = tune()
  ) %>%
    set_mode("regression") %>%
    set_engine("ranger")
  
  dml1wf = workflow() %>% add_recipe(dmlrecipe1) %>% add_model(tune_spec)
  dml2wf = workflow() %>% add_recipe(dmlrecipe2) %>% add_model(tune_spec)
  
  dat1folds = vfold_cv(dat1, 5)
  dat2folds = vfold_cv(dat2, 5)
  
  tune_res1 = tune_grid(
    dml1wf,
    resamples = dat1folds,
    grid = 5
  ) %>% select_best(maximize = FALSE)
  
  tune_res2 = tune_grid(
    dml2wf,
    resamples = dat2folds,
    grid = 5
  ) %>% select_best(maximize = FALSE)
  
  tune_best1 = dml1wf %>% finalize_workflow(tune_res1) %>% fit(dat1)
  tune_best2 = dml2wf%>% finalize_workflow(tune_res1) %>% fit(dat2)
  
  predict1 = predict(tune_best2, dat1)
  predict2 = predict(tune_best1, dat2)
  dat_out$inc_hat = rbind(predict1, predict2)
  
  ### STEP 3b: get the residuals Y - Y_hat on set 2 and on set 1
  dat_out$inc_resid = rbind(dat1$incwage - predict1, dat2$incwage - predict2)
  
  ### STEP 4: regress (Y - Y_hat) on (X - X_hat) and return predictions
  reg1 = lm(inc_resid ~ educ_resid, data= dat_out)
  dat_out$ability = reg1$fitted.values
  
  
  return(dat_out)
}

acs_raw = doubleml(acs_raw)

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
  i_drive_to_work = 1*(tranwork %in% c(10:15, 20, 38)),
  i_physical_disability = diffphys == 2,
  socioeconomic_index = hwsei,
  poverty_pct = poverty,
  i_female = 1 * (sex == 2),
  i_male = 1 * (sex == 1),
  i_grad_college = 1 * (educd %in% c(101,114,115,116)),
  i_grad_highschool = 1*(educd >= 62),
  i_married = 1 * (marst %in% c(1:2)),
  i_married_mult = 1 * (marrno %in% c(2:6)),
  personal_income = ifelse(inctot != 9999999, inctot, NA) / 1e4,
  personal_nonwage_income = ifelse(inctot != 9999999, inctot, NA) / 1e4 - ifelse(incwage != 9999999, inctot, NA) / 1e4,
  i_health_insurance = 1 * (hcovany == 2),
  i_moved_in_last_year = 1 * (migrate1d >22),
  i_moved_out_of_state_in_last_year = 1* (migrate1d %in% c(30, 31, 32, 40)),
  lived_abroad_in_last_year = 1* (migrate1d == 40),
  time_depart,
  time_arrive,
  time_commuting = time_arrive - time_depart,
  education = educ,
  ability = ability,
  weights = perwt_sftmx
)]
# Restrict to individuals with positive income (for logs in assignment)
acs_clean %<>% .[personal_income > 0 & poverty_pct > 0]

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
  "proj-data.rds"
)
readr::write_csv(
  acs_sub,
  "proj-data.csv"
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