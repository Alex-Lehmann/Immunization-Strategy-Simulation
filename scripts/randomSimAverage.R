# Quick script to get average results of a simulation with no vaccinations for reference
# purposes

library(tidyverse)
library(lubridate)
library(snow)
source("fn/sim.R")

# Initalize computing cluster
nCores = 10
c1 = makeCluster(nCores, "SOCK")

# Function to run simulations
runSim = function(nSims){
  library(tidyverse)
  library(lubridate)
  source("fn/sim.R")
  
  overallResults = NULL
  for (i in 1:nSims){
    
    agents = sim_make_agents(scaleFactor=100)
    simResults = tibble(Iteration = 0,
                        Cases_Under20 = 0,
                        Cases_20s = 0,
                        Cases_30s = 0,
                        Cases_40s = 0,
                        Cases_50s = 0,
                        Cases_60s = 0,
                        Cases_70s = 0,
                        Cases_Over80 = 0,
                        
                        Deaths_Under20 = 0,
                        Deaths_20s = 0,
                        Deaths_30s = 0,
                        Deaths_40s = 0,
                        Deaths_50s = 0,
                        Deaths_60s = 0,
                        Deaths_70s = 0,
                        Deaths_Over80 = 0)
    
    for (j in 1:39){
      print(paste0("Simulation #", i, ", iteration #", j))
      agents = sim_iter(0, 0, 0, "One Dose", agents, 100)
      simResults = sim_results(agents, simResults, j, 100)
    }
    
    summaryResults = transmute(simResults,
                               Iteration = Iteration,
                               Cases = rowSums(across(starts_with("Cases"))),
                               Deaths = rowSums(across(starts_with("Deaths"))))
    
    overallResults = rbind(overallResults, summaryResults)
  }
  
  return(overallResults)
}

# Run simulations
clusterResults = bind_rows(clusterCall(c1, runSim, nSims=10)) %>%
  mutate(Date = as_date("2021-01-01") + (7*Iteration))

# Close cluster
stopCluster(c1)

# Save CSV files for cases and deaths
clusterResults %>%
  select(Date, Cases) %>%
  group_by(Date) %>%
  summarize(Reference = mean(Cases)) %>%
  write.csv("ref/data/refCases.csv", row.names=FALSE)
clusterResults %>%
  select(Date, Deaths) %>%
  group_by(Date) %>%
  summarize(Reference = mean(Deaths)) %>%
  write.csv("ref/data/refDeaths.csv", row.names=FALSE)
