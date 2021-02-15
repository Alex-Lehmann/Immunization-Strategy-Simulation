###########################################################################################
# Known constants #########################################################################
reference = tibble(AgeGroup =   c("under20", "20s"  , "30s"  , "40s"  , "50s"  , "60s"  , "70s" , "over80"),
                   Population = c(3028125  , 1772750, 1711035, 1813875, 2053490, 1591725, 926075, 594645),
                   Risk = c(2/34355, 8/54757, 17/41002, 51/37089, 196/38353, 554/24446, 1200/13176, 4454/18227))

confCases = read_csv("ref/data/conposcovidloc.csv", col_types=cols()) %>%
  select(Accurate_Episode_Date, Age_Group, Outcome1) %>%
  mutate(Accurate_Episode_Date = as_date(Accurate_Episode_Date))

vaxEff = 0.95
###########################################################################################
# Simulation initialization ###############################################################
sim_make_agents = function(strategy="random", scaleFactor=1){
  
  # Find individuals with active infections or natural immunity
  caseStatus = confCases %>%
    filter(Outcome1 == "Resolved",
           Accurate_Episode_Date > as_date("2020-07-07"),
           Accurate_Episode_Date < as_date("2021-01-01")) %>%
    group_by(Week = week(Accurate_Episode_Date), Age_Group) %>%
    summarize(Cases = n()) %>%
    pivot_wider(names_from = Age_Group, values_from = Cases) %>%
    replace_na(list(`70s` = 0, `80s` = 0, `90s` = 0)) %>%
    mutate(over80 = `80s` + `90s`) %>%
    rename(under20 = `<20`) %>%
    select(-c(`NA`, UNKNOWN, `80s`, `90s`)) %>%
    mutate(Immunity = Week - 27)
  
  # Generate simulated population
  agents = NULL
  for (age in c("under20", "20s", "30s", "40s", "50s", "60s", "70s", "over80")){
    constants = filter(reference, AgeGroup == age)
    
    # Generate new agents
    nAgents = round(constants$Population / scaleFactor)
    stateCounts = round(pull(caseStatus, age) / scaleFactor)
    states = sample(c(rep(0, nAgents - sum(stateCounts)), rep(1:26, stateCounts)))
    newAgents = tibble(UID = rep.int(0, nAgents),
                       AgeGroup = rep.int(age, nAgents),
                       State = states,
                       Risk = rep(constants$Risk, nAgents),
                       Infections = rep(0, nAgents),
                       Ticket = rep.int(0, nAgents))
    
    # Add new agents to simulation
    agents = rbind(agents, newAgents)
  }
  totalAgents = nrow(agents)
  agents$UID = 1:totalAgents
  
  # Determine priority order
  switch(strategy,
    risk = {agents = arrange(agents, desc(row_number()))},
    random = {agents = slice_sample(agents, n=totalAgents)}
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
#   *-1: vaccinated
#   *-2: dead
sim_iter = function(doses=82800, simState, scaleFactor=1){
  doses = round(doses / scaleFactor)
  
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
    filter(State %in% c(0, -1)) %>%
    summarize(n()) %>%
    pull(`n()`)
  
  nExposed = rpois(1, 0.5 * nContagious * 1.05) # Simulate exposures; divide by two since cases last two weeks
  if (nExposed > nExposable) {nExposed = nExposable}
  toExpose = simState %>%
    filter(State %in% c(0,-1)) %>%
    pull(UID) %>%
    sample(nExposed)
  
  simState = mutate(simState, State = ifelse(UID %in% toExpose & (State == 0 | (State == -1 & rbinom(1, 1, vaxEff) == 0)), 27, State)) # Update agents
  simState = mutate(simState, Infections = ifelse(State == 27, Infections + 1, Infections))
  
  # Progress cases
  simState = mutate(simState, State = ifelse(State > 0, State - 1, State))
  
  # Vaccinate uninfected agents
  if (doses != 0){
    toVax = simState %>% # Grab highest-priority uninfected persons
      filter(State %in% 0:24) %>%
      arrange(Ticket) %>%
      slice_head(n=rpois(1, doses)) %>%
      pull(UID)
    
    simState = mutate(simState, State = ifelse(UID %in% toVax, -1, State)) # Update agents
  }
  
  return(simState)
}

###########################################################################################
# Results update ##########################################################################
sim_results = function(state, results, iter, scaleFactor=1){
  
  # Get current totals per age group from simulation state
  cases = c(sim_casesByAge(state, "under20"),
            sim_casesByAge(state, "20s"),
            sim_casesByAge(state, "30s"),
            sim_casesByAge(state, "40s"),
            sim_casesByAge(state, "50s"),
            sim_casesByAge(state, "60s"),
            sim_casesByAge(state, "70s"),
            sim_casesByAge(state, "over80")) * scaleFactor
  
  deaths = c(sim_deathsByAge(state, "under20"),
             sim_deathsByAge(state, "20s"),
             sim_deathsByAge(state, "30s"),
             sim_deathsByAge(state, "40s"),
             sim_deathsByAge(state, "50s"),
             sim_deathsByAge(state, "60s"),
             sim_deathsByAge(state, "70s"),
             sim_deathsByAge(state, "over80")) * scaleFactor
  
  vax = c(sim_vaxByAge(state, "under20"),
          sim_vaxByAge(state, "20s"),
          sim_vaxByAge(state, "30s"),
          sim_vaxByAge(state, "40s"),
          sim_vaxByAge(state, "50s"),
          sim_vaxByAge(state, "60s"),
          sim_vaxByAge(state, "70s"),
          sim_vaxByAge(state, "over80")) * scaleFactor
  
  # Update simulation results
  results = rbind(results, c(iter, cases, deaths, vax))
  
  return(results)
}

###########################################################################################
# Results helper functions ################################################################
sim_casesByAge = function(state, ages){
  state %>%
    filter(AgeGroup == ages) %>%
    summarize(Cases = sum(Infections)) %>%
    pull(Cases)
}

sim_deathsByAge = function(state, ages){
  state %>%
    filter(AgeGroup == ages) %>%
    summarize(Deaths = sum(State == -2)) %>%
    pull(Deaths)
}

sim_vaxByAge = function(state, ages){
  state %>%
    filter(AgeGroup == ages) %>%
    summarize(Vax = sum(State == -1)) %>%
    pull(Vax)
}
