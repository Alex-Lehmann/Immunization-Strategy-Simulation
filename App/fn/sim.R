library(tidyverse)
library(lubridate)
source("fn/custom_priority.R")

###########################################################################################
# Known constants #########################################################################
reference = tibble(AgeGroup =   c("under20", "20s"  , "30s"  , "40s"  , "50s"  , "60s"  , "70s" , "over80"),
                   Population = c(3028125  , 1772750, 1711035, 1813875, 2053490, 1591725, 926075, 594645),
                   Risk = c(2/34355, 8/54757, 17/41002, 51/37089, 196/38353, 554/24446, 1200/13176, 4454/18227))

confCases = read_csv("ref/data/conposcovidloc.csv", col_types=cols()) %>%
  mutate(Accurate_Episode_Date = as_date(Accurate_Episode_Date))

ageData = list(list(label="under20", infectRisk=0.1308),
               list(label="20s", infectRisk=0.2083),
               list(label="30s", infectRisk=0.1572),
               list(label="40s", infectRisk=0.1419),
               list(label="50s", infectRisk=0.1473),
               list(label="60s", infectRisk=0.0939),
               list(label="70s", infectRisk=0.0506),
               list(label="over80", infectRisk=0.0700))

marginalization = read_csv("ref/data/marginalization.csv", col_types=cols())

###########################################################################################
# Simulation initialization ###############################################################

sim_make_agents = function(strategy="random", priorityOrder=NULL, scaleFactor=1){
  
  # Find individuals with active infections or natural immunity
  caseStatus = confCases %>%
    filter(Outcome1 == "Resolved",
           Accurate_Episode_Date > as_date("2020-07-07"),
           Accurate_Episode_Date < as_date("2021-01-01")) %>%
    group_by(Week = week(Accurate_Episode_Date), Age_Group) %>%
    summarize(Cases = n(), .groups="drop") %>%
    pivot_wider(names_from = Age_Group, values_from = Cases) %>%
    replace_na(list(`70s` = 0, `80s` = 0, `90s` = 0)) %>%
    mutate(over80 = `80s` + `90s`) %>%
    rename(under20 = `<20`) %>%
    select(-c(`NA`, UNKNOWN, `80s`, `90s`)) %>%
    mutate(Immunity = Week - 27)
  
  # Generate simulated population
  agents = NULL
  for (ageGroup in ageData){
    age = ageGroup$label
    infectRisk = ageGroup$infectRisk
    
    constants = filter(reference, AgeGroup == age)
    
    # Number of new agents in age group
    nAgents = round(constants$Population / scaleFactor)
    
    # Initial states
    stateCounts = round(pull(caseStatus, age) / scaleFactor)
    states = sample(c(rep(0, nAgents - sum(stateCounts)), rep(1:26, stateCounts)))
    
    # Geographic location and marginalization levels
    margCSDs = marginalization %>%
      mutate(Deprivation = replace(Deprivation, Deprivation == 1, "Lowest Deprivation"),
             Deprivation = replace(Deprivation, Deprivation == 2, "Low Deprivation"),
             Deprivation = replace(Deprivation, Deprivation == 3, "Moderate Deprivation"),
             Deprivation = replace(Deprivation, Deprivation == 4, "High Deprivation"),
             Deprivation = replace(Deprivation, Deprivation == 5, "Highest Deprivation"),
             
             EthnicCon = replace(EthnicCon, EthnicCon == 1, "Lowest Diversity"),
             EthnicCon = replace(EthnicCon, EthnicCon == 2, "Low Diversity"),
             EthnicCon = replace(EthnicCon, EthnicCon == 3, "Moderate Diversity"),
             EthnicCon = replace(EthnicCon, EthnicCon == 4, "High Diversity"),
             EthnicCon = replace(EthnicCon, EthnicCon == 5, "Highest Diversity")) %>%
      slice_sample(n=nAgents, weight_by=marginalization[[age]], replace=TRUE) %>%
      select(Deprivation, EthnicCon, DeprivationModifier, EthnicModifier)
    
    # Generate new agents
    newAgents = tibble(UID = rep.int(0, nAgents),
                       AgeGroup = rep.int(age, nAgents),
                       State = states,
                       Risk = rep(constants$Risk, nAgents),
                       Infections = rep(0, nAgents),
                       Ticket = rep.int(0, nAgents),
                       Vax = rep.int(0, nAgents),
                       InfectionWeight = rep(infectRisk, nAgents),
                       Deprivation = margCSDs$Deprivation,
                       EthnicCon = margCSDs$EthnicCon,
                       DeprivationModifier = margCSDs$DeprivationModifier,
                       EthnicModifier = margCSDs$EthnicModifier) %>%
      mutate(InfectionWeight = InfectionWeight * DeprivationModifier * EthnicModifier)
    
    # Add new agents to simulation
    agents = rbind(agents, newAgents)
  }
  totalAgents = nrow(agents)
  agents$UID = 1:totalAgents
  
  # Determine priority order
  switch(strategy,
    ageDesc = {agents = arrange(agents, desc(row_number()))},
    ageAsc = {agents = arrange(agents, row_number())},
    ethDesc = {agents = agents %>% slice_sample(n=totalAgents) %>% arrange(desc(EthnicCon))},
    ethAsc = {agents = agents %>% slice_sample(n=totalAgents) %>% arrange(EthnicCon)},
    depDesc = {agents = agents %>% slice_sample(n=totalAgents) %>% arrange(desc(Deprivation))},
    depAsc = {agents = agents %>% slice_sample(n=totalAgents) %>% ararnge(Deprivation)},
    custom = {agents = custom_priority(agents, priorityOrder)},
    {agents = slice_sample(agents, n=totalAgents)}
  )
  agents$Ticket = 1:totalAgents
  
  return(agents)
}

###########################################################################################
# Simulation iterations ###################################################################
# Agent states:
#   * 0:  base state
#   *27: new case
#   *26; first week
#   *25: second week
#   *1-24: post-infection immunity
#   *-2: dead

# Vaccination states:
#   *0: unvaccinated
#   *1: new vaccination
#   *2-4: waiting for second dose (partial immunity)
#   *5: new second dose (partial immunity)
#   *6: fully vaccinated

sim_iter = function(doses, vaxEff, vaxPartialEff, strategy, simState, scaleFactor=1){
  
  # Kill off fatal cases
  atRisk = simState %>% # Grab infected agents
    filter(State > 24) %>%
    select(UID, Risk)
  nAtRisk = nrow(atRisk)
  
  toKill = atRisk %>% # Binomial experiment for each agent's possible demise
    mutate(Outcome = rbinom(nAtRisk, 1, Risk)) %>%
    filter(Outcome == 1) %>%
    pull(UID)
  
  simState = mutate(simState, State = ifelse(UID %in% toKill, -2, State)) # Update agents
  
  # Spread virus
  nContagious = simState %>% # Get number of contagious agents
    filter(State > 24) %>%
    summarize(n()) %>%
    pull(`n()`)
  nExposable = simState %>% # Get number of exposable persons
    filter(State == 0) %>%
    summarize(n()) %>%
    pull(`n()`)
  
  nExposed = rpois(1, round(0.5 * nContagious * 1.05)) # Simulate exposures; divide by two since cases last two weeks
  if (nExposed > nExposable) {nExposed = nExposable}
  toExpose = simState %>%
    filter(State == 0) %>%
    slice_sample(n=nExposed, weight_by=InfectionWeight) %>%
    pull(UID)
  
  simState = mutate(simState, State = ifelse(UID %in% toExpose & (Vax == 0 |  # Update agents
                                                                  Vax %in% 1:5 & rbinom(1, 1, vaxPartialEff/100) == 0 |
                                                                  Vax == 6 & rbinom(1, 1, vaxEff/100) == 0),
                                             27, State))
  simState = mutate(simState, Infections = ifelse(State == 27, Infections + 1, Infections)) # Update infection count
  
  # Vaccinate uninfected agents
  if (doses != 0){
    # Get number of doses to be distributed this week
    nDoses = NULL
    if (strategy == "One Dose") { nDoses = rpois(1, round(doses / scaleFactor)) } # One-shot strategy
    else { nDoses = rpois(1, round(doses / (2*scaleFactor))) } # Two-dose strategy
    
    # First doses
    toFirstDose = simState %>% # Grab highest-priority uninfected persons without previous vaccination
      filter(State %in% 0:24, Vax == 0) %>%
      arrange(Ticket) %>%
      slice_head(n=nDoses) %>%
      pull(UID)
    simState = mutate(simState, Vax = ifelse(UID %in% toFirstDose, 1, Vax)) # Update agents
    
    # Second doses
    if (strategy=="Two Doses"){
      toSecondDose = simState %>% # Grab highest-priority uninfected persons waiting for second dose
        filter(State %in% 0:24, Vax == 4) %>%
        arrange(Ticket) %>%
        slice_head(n=nDoses) %>%
        pull(UID)
      simState = mutate(simState, Vax = ifelse(UID %in% toSecondDose, 5, Vax))
    }
  }
  
  # Progress time
  simState = mutate(simState, State = ifelse(State > 0, State - 1, State),
                              Vax = ifelse(Vax %in% c(1:3, 5) & strategy=="Two Doses", Vax + 1, Vax))
  
  return(simState)
}

###########################################################################################
# Results update ##########################################################################

sim_results = function(state, results, iter, scaleFactor=1){
  
  # Get current totals per age group from simulation state
  ageResults = state %>%
    group_by(AgeGroup) %>%
    summarize(Cases = sum(Infections),
              Deaths = sum(State == -2),
              FullVax = sum(Vax == 6),
              PartVax = sum(Vax %in% 1:5),
              Active = sum(State %in% c(25, 26)),
              Immune = sum(State %in% 1:24),
              .groups="drop") %>%
    arrange(factor(AgeGroup, levels=c("under20", "20s", "30s", "40s", "50s", "60s", "70s", "over80")))
  
  # Get current totals per marginalization level from simulation state
  depResults = state %>%
    group_by(Deprivation) %>%
    summarize(Cases = sum(Infections),
              Deaths = sum(State == -2),
              FullVax = sum(Vax == 6),
              PartVax = sum(Vax %in% 1:5),
              Active = sum(State %in% c(25, 26)),
              Immune = sum(State%in% 1:24),
              .groups="drop")
  ethResults = state %>%
    group_by(EthnicCon) %>%
    summarize(Cases = sum(Infections),
              Deaths = sum(State == -2),
              FullVax = sum(Vax == 6),
              PartVax = sum(Vax %in% 1:5),
              Active = sum(State %in% c(25, 26)),
              Immune = sum(State%in% 1:24),
              .groups="drop")
  
  newTotals = c(ageResults$Cases, ageResults$Deaths, ageResults$FullVax, ageResults$PartVax, ageResults$Active, ageResults$Immune,
                depResults$Cases, depResults$Deaths, depResults$FullVax, depResults$PartVax, depResults$Active, depResults$Immune,
                ethResults$Cases, ethResults$Deaths, ethResults$FullVax, ethResults$PartVax, ethResults$Active, ethResults$Immune) * scaleFactor
  
  # Update simulation results
  results = rbind(results, c(iter, newTotals))
  return(results)
}

###########################################################################################
# Results helper functions ################################################################

sim_activeByAge = function(state, ages){
  state %>%
    filter(AgeGroup == ages) %>%
    summarize(Active = sum(State %in% c(26, 25))) %>%
    pull(Active)
}

sim_immuneByAge = function(state, ages){
  state %>%
    filter(AgeGroup == ages) %>%
    summarize(Immune = sum(State %in% 1:24)) %>%
    pull(Immune)
}

sim_activeByDeprivation = function(state, depLevel){
  state %>%
    filter(Deprivation == depLevel) %>%
    summarize(Active = sum(State %in% c(26, 25))) %>%
    pull(Active)
}

sim_immuneByDeprivation = function(state, depLevel){
  state %>%
    filter(Deprivation == depLevel) %>%
    summarize(Active = sum(State %in% 1:24)) %>%
    pull(Active)
}

sim_activeByEthnicCon = function(state, conLevel){
  state %>%
    filter(EthnicCon == conLevel) %>%
    summarize(Active = sum(State %in% c(26, 25))) %>%
    pull(Active)
}

sim_immuneByEthnicCon = function(state, conLevel){
  state %>%
    filter(EthnicCon == conLevel) %>%
    summarize(Active = sum(State %in% 1:24)) %>%
    pull(Active)
}

###########################################################################################
