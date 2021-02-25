library(tidyverse)

# Vectors of options for each rank category
ageOptions = c("under20", "20s", "30s", "40s", "50s", "60s", "70s", "over80")
deprivationOptions = c("lowestdeprivation", "lowdeprivation", "moderatedeprivation", "highdeprivation", "highestdeprivation")
ethnicOptions = c("lowestdiversity", "lowdiversity", "moderatediversity", "highdiversity", "highestdiversity")

custom_priority = function(agents, order){
  
  # Sort priority agents
  priority = NULL
  for (trait in order){
    attribute = trait %>% # For string matching
      str_to_lower() %>%
      str_replace_all("\\s", "")
    
    if (attribute %in% ageOptions){
      toAdd = filter(agents, AgeGroup == attribute)
      priority = rbind(priority, slice_sample(toAdd, n=nrow(toAdd)))
      agents = filter(agents, AgeGroup != attribute)
      
    } else if (attribute %in% deprivationOptions){
      toAdd = filter(agents, Deprivation == trait)
      priority = rbind(priority, slice_sample(toAdd, n=nrow(toAdd)))
      agents = filter(agents, Deprivation != trait)
      
    } else if (attribute %in% ethnicOptions){
      toAdd = filter(agents, EthnicCon == trait)
      priority = rbind(priority, slice_sample(toAdd, n=nrow(toAdd)))
      agents = filter(agents, EthnicCon != trait)
    }
  }
  
  # Randomly sort remaining agents
  general = slice_sample(agents, n=nrow(agents))
  
  return(rbind(priority, general))
}
