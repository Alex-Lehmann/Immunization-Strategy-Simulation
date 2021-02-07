library(shiny)

# Load packages
packages = c("sortable", "tidyverse")

source("fn/loadPackages.R")
loadPackages(packages)

source("fn/sim.R")

print("Server ready")
shinyServer(function(input, output, session){
    values = reactiveValues()
    #######################################################################################
    # Simulation ##########################################################################
    observeEvent(input$runSimBn,{
        
        nIter = 39
        results = tibble(Iteration = 0,
                         Total_Cases = 0,
                         Total_Deaths = 0,
                         Total_Vax = 0)
        
        # Generate agents
        print("Generating agents")
        agents = sim_make_agents(input$paramOriginal, input$paramStrategy, input$paramScaling)
        
        # Run simulation
        for (i in 1:nIter){
            print(paste0("Starting iteration ", i))
            agents = sim_iter(input$paramVax, agents, input$paramScaling)
            
            results = rbind(results, sim_results(agents, i, input$paramScaling))
            print(paste0("Iteration ", i, " complete"))
        }
        print("Simulation complete")
        
        # Process summary results and update summary button labels
        results = results %>%
            mutate(New_Cases = c(NA, diff(Total_Cases)),
                   New_Deaths = c(NA, diff(Total_Deaths)),
                   New_Vax = c(NA, diff(Total_Vax)))
        values$simResults = results
            
        simEnd = slice_tail(results, n=1)
        simCases = simEnd$Total_Cases
        simDeaths = simEnd$Total_Deaths
        simVax = simEnd$Total_Vax
        
        updateActionButton(session, "summaryCases", label=HTML(paste0("<h3><b>Total Cases</h3><h4>", format(simCases, big.mark=","), "</h4></b>")))
        updateActionButton(session, "summaryDeaths", label=HTML(paste0("<h3><b>Total Deaths</h3><h4>", format(simDeaths, big.mark=","), "</h4></b>")))
        updateActionButton(session, "summaryVax", label=HTML(paste0("<h3><b>Total Vaccinated</h3><h4>", format(simVax, big.mark=","), "</h4></b>")))
        
        updateTabsetPanel(session, "summaryTabs", "cases")
    })
    
    #######################################################################################
    # Summary plots #######################################################################
    
    # Cases time series
    output$summaryTotalCasesTS = renderPlot({
        values$simResults %>%
            filter(!is.na(Total_Cases)) %>%
            ggplot( aes(x=Iteration, y=Total_Cases)) +
            geom_point(color="#4CAF50", size=4) +
            geom_line(color="#4CAF50", size=2) +
            geom_area(fill="#4CAF50", alpha=0.2) +
            ylab("Cumulative COVID-19 Cases")
    })
    
    output$summaryNewCasesTS = renderPlot({
        values$simResults %>%
            filter(!is.na(New_Cases)) %>%
            ggplot(aes(x=Iteration, y=New_Cases)) +
            geom_point(color="#4CAF50", size=4) +
            geom_line(color="#4CAF50", size=2) +
            ylab("New COVID-19 Cases")
    })
    
    # Deaths time series
    output$summaryTotalDeathsTS = renderPlot({
        values$simResults %>%
            filter(!is.na(Total_Deaths)) %>%
            ggplot(aes(x=Iteration, y=Total_Deaths)) +
            geom_point(color="#DC2824", size=4) +
            geom_line(color="#DC2824", size=2) +
            geom_area(fill="#DC2824", alpha=0.2) +
            ylab("Cumulative COVID-19 Deaths")
    })
    
    output$summaryNewDeathsTS = renderPlot({
        values$simResults %>%
            filter(!is.na(New_Deaths)) %>%
            ggplot(aes(x=Iteration, y=New_Deaths)) +
            geom_point(color="#DC2824", size=4) +
            geom_line(color="#DC2824", size=2) +
            ylab("New COVID-19 Deaths")
    })
    
    # Vacinations time series
    output$summaryTotalVaxTS = renderPlot({
        values$simResults %>%
            filter(!is.na(Total_Vax)) %>%
            ggplot(aes(x=Iteration, y=Total_Vax)) +
            geom_point(color="#428BCA", size=4) +
            geom_line(color="#428BCA", size=2) +
            geom_area(fill="#428BCA", alpha=0.2) +
            ylab("Cumulative COVID-19 Vaccinations")
    })
    
    output$summaryNewVaxsTS = renderPlot({
        values$simResults %>%
            filter(!is.na(New_Vax)) %>%
            ggplot(aes(x=Iteration, y=New_Vax)) +
            geom_point(color="#428BCA", size=4) +
            geom_line(color="#428BCA", size=2) +
            ylab("New COVID-19 Vaccinations")
    })
    
    #######################################################################################
    # Results summary button variable handlers ############################################
    
    # Change to cases
    observeEvent(input$summaryCases,{
        updateTabsetPanel(session, "summaryTabs", selected="cases")
    })
    
    # Change to deaths
    observeEvent(input$summaryDeaths,{
        updateTabsetPanel(session, "summaryTabs", selected="deaths")
    })
    
    # Change to vaccinated
    observeEvent(input$summaryVax,{
        updateTabsetPanel(session, "summaryTabs", selected="vax")
    })
    #######################################################################################
})
