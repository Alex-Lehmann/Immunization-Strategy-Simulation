library(tidyverse)
library(cancensus)

# Load original data and discard unneeded columns
marginalization = read_csv("ref/data/csd_marg.csv", col_types=cols()) %>%
  select(CSDUID, deprivation_q_CSDUID, ethniccon_q_CSDUID) %>%
  drop_na() %>%
  rename(Deprivation = deprivation_q_CSDUID, EthnicCon = ethniccon_q_CSDUID)

# Get age groups from CensusMapper
query_csd = marginalization$CSDUID
query_vectors = c("v_CA16_4","v_CA16_64","v_CA16_82","v_CA16_100","v_CA16_118","v_CA16_136","v_CA16_154","v_CA16_172", # Age group populations
                  "v_CA16_190","v_CA16_208","v_CA16_226","v_CA16_247","v_CA16_265","v_CA16_283","v_CA16_301","v_CA16_319")
csdData = get_census(dataset="CA16",
                     regions=list(CSD=query_csd),
                     vectors=query_vectors) %>%
  mutate(CSDUID = as.numeric(GeoUID),
         under20 = `v_CA16_4: 0 to 14 years` + `v_CA16_64: 15 to 19 years`,
         `20s` = `v_CA16_82: 20 to 24 years` + `v_CA16_100: 25 to 29 years`,
         `30s` = `v_CA16_118: 30 to 34 years` + `v_CA16_136: 35 to 39 years`,
         `40s` = `v_CA16_154: 40 to 44 years` + `v_CA16_172: 45 to 49 years`,
         `50s` = `v_CA16_190: 50 to 54 years` + `v_CA16_208: 55 to 59 years`,
         `60s` = `v_CA16_226: 60 to 64 years` + `v_CA16_247: 65 to 69 years`,
         `70s` = `v_CA16_265: 70 to 74 years` + `v_CA16_283: 75 to 79 years`,
         over80 = `v_CA16_301: 80 to 84 years` + `v_CA16_319: 85 years and over`) %>%
  select(CSDUID, Name=`Region Name`, under20:over80)

# Merge with marginalization data
marginalization = full_join(marginalization, csdData)

# Compute population (in 100,000s) of each quintile for later
deprivationPops = marginalization %>%
  group_by(Deprivation) %>%
  summarize(across(under20:over80, sum), .groups="drop") %>%
  transmute(Pop = rowSums(across(under20:over80))) %>%
  pull(Pop)

# Compute proportion of age group population in each CSD
marginalization = marginalization %>%
  mutate(across(under20:over80, function(x){x/sum(x)}))

# Find infection risk weights by marginalization level
covidRates = read_csv("ref/data/covid_marg.csv", col_types=cols()) %>%
  mutate(DepQ1 = DepQ1*deprivationPops[1],
         DepQ2 = DepQ2*deprivationPops[2],
         DepQ3 = DepQ3*deprivationPops[3],
         DepQ4 = DepQ4*deprivationPops[4],
         DepQ5 = DepQ5*deprivationPops[5]) %>%
  summarize(across(everything(), sum)) %>%
  mutate(DepSum = rowSums(across(starts_with("Dep"), sum)),
         across(starts_with("DepQ"), function(x){x / DepSum})) %>%
  select(starts_with("DepQ"))

depMerge = tibble(Deprivation = c(1,2,3,4,5),
                  DeprivationModifier = c(covidRates$DepQ1,
                                          covidRates$DepQ2,
                                          covidRates$DepQ3,
                                          covidRates$DepQ4,
                                          covidRates$DepQ5))
marginalization = left_join(marginalization, depMerge, by="Deprivation")

write.csv(marginalization, "ref/data/marginalization.csv", row.names=FALSE)
