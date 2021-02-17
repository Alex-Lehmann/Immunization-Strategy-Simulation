###########################################################################################
# Known constants #########################################################################
reference = tibble(AgeGroup =   c("under20", "20s"  , "30s"  , "40s"  , "50s"  , "60s"  , "70s" , "over80"),
                   Population = c(3028125  , 1772750, 1711035, 1813875, 2053490, 1591725, 926075, 594645),
                   Risk = c(2/34355, 8/54757, 17/41002, 51/37089, 196/38353, 554/24446, 1200/13176, 4454/18227))

confCases = read_csv("ref/data/conposcovidloc.csv", col_types=cols()) %>%
  select(Accurate_Episode_Date, Age_Group, Outcome1) %>%
  mutate(Accurate_Episode_Date = as_date(Accurate_Episode_Date))

vaxEff = 0.95
vaxPartialEff = 0.54
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
                       Ticket = rep.int(0, nAgents),
                       Vax = rep.int(0, nAgents))
    
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
#   *-2: dead

# Vaccination states:
#   *0: unvaccinated
#   *1: new vaccination
#   *2-4: waiting for second dose (partial immunity)
#   *5: new second dose (partial immunity)
#   *6: fully vaccinated
sim_iter = function(doses, strategy, simState, scaleFactor=1){
  
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
    pull(UID) %>%
    sample(nExposed)
  
  simState = mutate(simState, State = ifelse(UID %in% toExpose & (Vax == 0 |  # Update agents
                                                                  Vax %in% 1:5 & rbinom(1, 1, vaxPartialEff) == 0 |
                                                                  Vax == 6 & rbinom(1, 1, vaxEff) == 0),
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
  
  fullVax = c(sim_fullVaxByAge(state, "under20"),
              sim_fullVaxByAge(state, "20s"),
              sim_fullVaxByAge(state, "30s"),
              sim_fullVaxByAge(state, "40s"),
              sim_fullVaxByAge(state, "50s"),
              sim_fullVaxByAge(state, "60s"),
              sim_fullVaxByAge(state, "70s"),
              sim_fullVaxByAge(state, "over80")) * scaleFactor
  
  partialVax = c(sim_partialVaxByAge(state, "under20"),
                 sim_partialVaxByAge(state, "20s"),
                 sim_partialVaxByAge(state, "30s"),
                 sim_partialVaxByAge(state, "40s"),
                 sim_partialVaxByAge(state, "50s"),
                 sim_partialVaxByAge(state, "60s"),
                 sim_partialVaxByAge(state, "70s"),
                 sim_partialVaxByAge(state, "over80")) * scaleFactor
  
  active = c(sim_activeByAge(state, "under20"),
             sim_activeByAge(state, "20s"),
             sim_activeByAge(state, "30s"),
             sim_activeByAge(state, "40s"),
             sim_activeByAge(state, "50s"),
             sim_activeByAge(state, "60s"),
             sim_activeByAge(state, "70s"),
             sim_activeByAge(state, "over80")) * scaleFactor
  
  immune = c(sim_immuneByAge(state, "under20"),
             sim_immuneByAge(state, "20s"),
             sim_immuneByAge(state, "30s"),
             sim_immuneByAge(state, "40s"),
             sim_immuneByAge(state, "50s"),
             sim_immuneByAge(state, "60s"),
             sim_immuneByAge(state, "70s"),
             sim_immuneByAge(state, "over80")) * scaleFactor
  
  # Update simulation results
  results = rbind(results, c(iter, cases, deaths, fullVax, partialVax, active, immune))
  
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

sim_fullVaxByAge = function(state, ages){
  state %>%
    filter(AgeGroup == ages) %>%
    summarize(Vax = sum(Vax == 6)) %>%
    pull(Vax)
}

sim_partialVaxByAge = function(state, ages){
  state %>%
    filter(AgeGroup == ages) %>%
    summarize(Vax = sum(Vax %in% 1:5)) %>%
    pull(Vax)
}

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
