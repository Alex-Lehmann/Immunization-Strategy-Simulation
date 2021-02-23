library(tidyverse)

custom_priority = function(agents, order){
  
  # Translate rank list output to agents language
  order = order %>%
    str_replace_all("\\s", "") %>%
    str_to_lower()
  
  # Split agents into priority and non-priority groups and impose ordering
  priority = filter(agents, AgeGroup %in% order)
  regular = filter(agents, !(AgeGroup %in% order))
  
  # Set ordering in each group
    priority = priority %>%
      slice_sample(n=nrow(priority)) %>%
      arrange(factor(AgeGroup, levels=order))
    regular = slice_sample(regular, n=nrow(regular))
  
  # Output custom-ordered agents list
  orderedAgents = rbind(priority, regular)
  return(orderedAgents)
}
