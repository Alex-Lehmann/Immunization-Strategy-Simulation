###########################################################################################
# Known constants #########################################################################
reference = tibble(AgeGroup =   c("under20", "20s"  , "30s"  , "40s"  , "50s"  , "60s"  , "70s" , "over80"),
                   Population = c(3028125  , 1772750, 1711035, 1813875, 2053490, 1591725, 926075, 594645),
                   Risk = c(2/34355, 8/54757, 17/41002, 51/37089, 196/38353, 554/24446, 1200/13176, 4454/18227))

###########################################################################################
###########################################################################################
sim_make_agents = function(cases=4800, strategy="random", scaleFactor=1){
  
  # Generate simulated population
  agents = NULL
  for (age in c("under20", "20s", "30s", "40s", "50s", "60s", "70s", "over80")){
    constants = filter(reference, AgeGroup == age)
    
    # Generate new agents
    nAgents = round(constants$Population / scaleFactor)
    newAgents = tibble(UID = rep.int(0, nAgents),
                       AgeGroup = rep.int(age, nAgents),
                       State = rep(-3, nAgents),
                       Risk = rep(constants$Risk, nAgents),
                       Infections = rep(0, nAgents),
                       Ticket = rep.int(0, nAgents))
    
    # Add new agents to simulation
    agents = rbind(agents, newAgents)
  }
  totalAgents = nrow(agents)
  agents$UID = 1:totalAgents
  
  # Randomly distribute COVID cases
  nCases = round(cases / scaleFactor)
  nWeek1 = rbinom(1, nCases, 0.5)
  nWeek2 = nCases - nWeek1
  agents$State = sample(c(rep(0, totalAgents - nCases), rep(28, nWeek1), rep(27, nWeek2)))
  agents = mutate(agents, Infections = ifelse(State %in% c(2, 1), 1, 0))
  
  # Determine priority order
  switch(strategy,
    risk = {agents = arrange(agents, desc(row_number()))},
    random = {agents = slice_sample(agents, n=totalAgents)}
  )
  agents$Ticket = 1:totalAgents
  
  return(agents)
}

###########################################################################################
###########################################################################################
# Agent states:
#   * 0:  base state
#   *29: new case
#   *28; first week
#   *27: second week
#   *1-26: post-infection immunity
#   *-1: vaccinated
#   *-2: dead
sim_iter = function(doses=82800, simState, scaleFactor=1){
  doses = round(doses / scaleFactor)
  
  # Vaccinate uninfected agents
  toVax = simState %>% # Grab highest-priority uninfected persons
    filter(State %in% 0:26) %>%
    arrange(Ticket) %>%
    slice_head(n=doses) %>%
    pull(UID)
  
  simState = mutate(simState, State = ifelse(UID %in% toVax, -1, State)) # Update agents
  
  # Kill off fatal cases
  atRisk = simState %>% # Grab infected agents
    filter(State > 26) %>%
    select(UID, Risk)
  nAtRisk = nrow(atRisk)
  
  toKill = atRisk %>% # Binomial experiment for each agent's possible demise
    mutate(Outcome = rbinom(nAtRisk, 1, Risk)) %>%
    filter(Outcome == 1) %>%
    pull(UID)
  
  simState = mutate(simState, State = ifelse(UID %in% toKill, -2, State)) # Update agents
  
  # Spread virus
  nContagious = simState %>% # Get number of contagious agents
    filter(State > 26) %>%
    summarize(n()) %>%
    pull(`n()`)
  nExposable = simState %>% # Get number of exposable persons
    filter(State %in% -1:26) %>%
    summarize(n()) %>%
    pull(`n()`)
  
  nExposed = round((nContagious * 1.05) / 2) # Simulate exposures; divide by two since cases last two weeks
  if (nExposed > nExposable) {nExposed = nExposable}
  toExpose = simState %>%
    filter(State == 0) %>%
    pull(UID) %>%
    sample(nExposed)
  
  simState = mutate(simState, State = ifelse(UID %in% toExpose & State == 0 , 29, State)) # Update agents
  simState = mutate(simState, Infections = ifelse(State == 29, Infections + 1, Infections))
  
  # Progress cases
  simState = mutate(simState, State = ifelse(State > 0, State - 1, State))
  
  return(simState)
}

###########################################################################################
###########################################################################################
sim_results = function(finalState, iter, scaleFactor=1){
  # Get cumulative statistics from simulation state
  nCases = sum(finalState$Infections) * scaleFactor
  nDeaths = sum(finalState$State == -2) * scaleFactor
  nVax = sum(finalState$State == -1) * scaleFactor
  
  # Output for results
  results = c(iter, nCases, nDeaths, nVax)
  
  return(results)
}