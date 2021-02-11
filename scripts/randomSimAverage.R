setwd("App")

library(tidyverse)
library(lubridate)
source("fn/sim.R")

nSims = 10000
overallResults = tibble(`Total Cases`=0, `Total Deaths`=0, `Total Vaccinations`=0)

# Run simulation nSims times
for (i in 1:nSims){
  print(paste0("Simulation #", i))
  
  agents = sim_make_agents(scaleFactor=100)
  simResults = tibble(Iteration = 0,
                      `Total Cases` = 0,
                      `Total Deaths` = 0,
                      `Total Vaccinations` = 0)
  for (j in 1:39){
    agents = sim_iter(0, agents, 100)
    simResults = rbind(simResults, sim_results(agents, j, 100))
  }
  
  simEnd = slice_tail(simResults, n=1)
  simCases = simEnd$`Total Cases`
  simDeaths = simEnd$`Total Deaths`
  simVax = simEnd$`Total Vaccinations`
  
  overallResults = rbind(overallResults, c(simCases, simDeaths, simVax))
}

averageValue = overallResults %>%
  summarize(AvgCases = mean(`Total Cases`), AvgDeaths = mean(`Total Deaths`), AvgVax = mean(`Total Vaccinations`))