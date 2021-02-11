library(shiny)

shinyServer(function(input, output, session){
    
    values = reactiveValues()
    values$hasRun = FALSE
    #######################################################################################
    # Simulation ##########################################################################
    observeEvent(input$runSimBn,{
        
        # Localize dangerous inputs
        paramVax = input$paramVax
        
        # Validate inputs
        if (is.na(input$paramVax) | input$paramVax < 0){
            updateNumericInput(session, "paramVax", value=0)
            paramVax = 0
        }
        
        # Random seed
        if (!is.na(input$paramSeed)){ set.seed(input$paramSeed) } else { set.seed(NULL) }
        
        # Simulation initial conditions
        progress=0
        nIter = 39
        results = tibble(Iteration = 0,
                         `Total Cases` = 0,
                         `Total Deaths` = 0,
                         `Total Vaccinations` = 0)
        
        # Display busy dialog
        show_modal_spinner(spin="swapping-squares", color="#112446", text="Simulating...")
        
        # Generate agents
        print("Generating agents")
        agents = sim_make_agents(input$paramStrategy, input$paramScaling)
        
        progress = 1/5
        
        # Run simulation
        for (i in 1:nIter){
            progress = progress + 1/50
            update_modal_progress(value=progress, text=paste0("Simulating iteration ", i," of 40..."))
            
            print(paste0("Starting iteration ", i))
            agents = sim_iter(paramVax, agents, input$paramScaling)
            
            results = rbind(results, sim_results(agents, i, input$paramScaling))
            print(paste0("Iteration ", i, " complete"))
        }
        print("Simulation complete")
        
        # Process summary results
        results = results %>%
            mutate(`New Cases` = c(NA, diff(`Total Cases`)),
                   `New Deaths` = c(NA, diff(`Total Deaths`)),
                   `New Vaccinations` = c(NA, diff(`Total Vaccinations`)),
                   Date = as_date("2021-01-01") + (7 * Iteration))
        values$simResults = results
        
        # Update summary buttons
        simEnd = slice_tail(results, n=1)
        simCases = simEnd$`Total Cases`
        simDeaths = simEnd$`Total Deaths`
        simVax = simEnd$`Total Vaccinations`
        
        updateActionButton(session, "summaryCases", label=HTML(paste0("<h3><b>Total Cases</h3><h4>", format(simCases, big.mark=",", scientific=FALSE), "</h4></b>")))
        updateActionButton(session, "summaryDeaths", label=HTML(paste0("<h3><b>Total Deaths</h3><h4>", format(simDeaths, big.mark=",", scientific=FALSE), "</h4></b>")))
        updateActionButton(session, "summaryVax", label=HTML(paste0("<h3><b>Total Vaccinated</h3><h4>", format(simVax, big.mark=",", scientific=FALSE), "</h4></b>")))
        updateTabsetPanel(session, "summaryTabs", "cases")
        
        # Close busy dialog
        remove_modal_spinner()
        values$hasRun = TRUE
    })
    
    #######################################################################################
    # Summary plots #######################################################################
    
    # Cases time series
    output$summaryTotalCasesTS = renderPlotly({
        plot = values$simResults %>%
            filter(!is.na(`Total Cases`)) %>%
            ggplot( aes(x=Date, y=`Total Cases`)) +
            geom_point(color="#4CAF50") +
            geom_line(color="#4CAF50") +
            ylab("Cumulative COVID-19 Cases")
        ggplotly(plot) %>%
            layout(showlegend=TRUE, hovermode="x", spikedistance=-1,
                   xaxis=list(fixedrange=TRUE, showspikes=TRUE, spikemode="across", spikesnap="cursor", spikedash="solid", showline=TRUE, showgrid=TRUE),
                   yaxis=list(fixedrange=TRUE)) %>%
            config(displayModeBar = FALSE)
    })
    
    output$summaryNewCasesTS = renderPlotly({
        plot = values$simResults %>%
            filter(!is.na(`New Cases`)) %>%
            ggplot(aes(x=Date, y=`New Cases`)) +
            geom_point(color="#4CAF50") +
            geom_line(color="#4CAF50") +
            ylab("New COVID-19 Cases")
        ggplotly(plot) %>%
            layout(showlegend=TRUE, hovermode="x", spikedistance=-1,
                   xaxis=list(fixedrange=TRUE, showspikes=TRUE, spikemode="across", spikesnap="cursor", spikedash="solid", showline=TRUE, showgrid=TRUE),
                   yaxis=list(fixedrange=TRUE)) %>%
            config(displayModeBar = FALSE)
    })
    
    # Deaths time series
    output$summaryTotalDeathsTS = renderPlotly({
        plot = values$simResults %>%
            filter(!is.na(`Total Deaths`)) %>%
            ggplot(aes(x=Date, y=`Total Deaths`)) +
            geom_point(color="#DC2824") +
            geom_line(color="#DC2824") +
            ylab("Cumulative COVID-19 Deaths")
        ggplotly(plot) %>%
            layout(showlegend=TRUE, hovermode="x", spikedistance=-1,
                   xaxis=list(fixedrange=TRUE, showspikes=TRUE, spikemode="across", spikesnap="cursor", spikedash="solid", showline=TRUE, showgrid=TRUE),
                   yaxis=list(fixedrange=TRUE)) %>%
            config(displayModeBar = FALSE)
    })
    
    output$summaryNewDeathsTS = renderPlotly({
        plot = values$simResults %>%
            filter(!is.na(`New Deaths`)) %>%
            ggplot(aes(x=Date, y=`New Deaths`)) +
            geom_point(color="#DC2824") +
            geom_line(color="#DC2824") +
            ylab("New COVID-19 Deaths")
        ggplotly(plot) %>%
            layout(showlegend=TRUE, hovermode="x", spikedistance=-1,
                   xaxis=list(fixedrange=TRUE, showspikes=TRUE, spikemode="across", spikesnap="cursor", spikedash="solid", showline=TRUE, showgrid=TRUE),
                   yaxis=list(fixedrange=TRUE)) %>%
            config(displayModeBar = FALSE)
    })
    
    # Vaccinations time series
    output$summaryTotalVaxTS = renderPlotly({
        plot = values$simResults %>%
            filter(!is.na(`Total Vaccinations`)) %>%
            ggplot(aes(x=Date, y=`Total Vaccinations`)) +
            geom_point(color="#428BCA") +
            geom_line(color="#428BCA") +
            ylab("Cumulative COVID-19 Vaccinations")
        ggplotly(plot) %>%
            layout(showlegend=TRUE, hovermode="x", spikedistance=-1,
                   xaxis=list(fixedrange=TRUE, showspikes=TRUE, spikemode="across", spikesnap="cursor", spikedash="solid", showline=TRUE, showgrid=TRUE),
                   yaxis=list(fixedrange=TRUE)) %>%
            config(displayModeBar = FALSE)
    })
    
    output$summaryNewVaxsTS = renderPlotly({
        plot = values$simResults %>%
            filter(!is.na(`New Vaccinations`)) %>%
            ggplot(aes(x=Date, y=`New Vaccinations`)) +
            geom_point(color="#428BCA") +
            geom_line(color="#428BCA") +
            ylab("New COVID-19 Vaccinations")
        ggplotly(plot) %>%
            layout(showlegend=TRUE, hovermode="x", spikedistance=-1,
                   xaxis=list(fixedrange=TRUE, showspikes=TRUE, spikemode="across", spikesnap="cursor", spikedash="solid", showline=TRUE, showgrid=TRUE),
                   yaxis=list(fixedrange=TRUE)) %>%
            config(displayModeBar = FALSE)
    })
    
    #######################################################################################
    # Results summary button variable handlers ############################################
    
    # Change to cases
    observeEvent(input$summaryCases,{ if (values$hasRun) { updateTabsetPanel(session, "summaryTabs", selected="cases") }})
    
    # Change to deaths
    observeEvent(input$summaryDeaths,{ if (values$hasRun) { updateTabsetPanel(session, "summaryTabs", selected="deaths") }})
    
    # Change to vaccinated
    observeEvent(input$summaryVax,{ if (values$hasRun) { updateTabsetPanel(session, "summaryTabs", selected="vax") }})
    
    #######################################################################################
    # Clear random seed button ############################################################
    observeEvent(input$paramSeedBn,{updateNumericInput(session, "paramSeed", value=NA)})
    
    #######################################################################################
    # Help buttons ########################################################################
    
    # Parameters help
    observeEvent(input$vaxHelpBn,{
        updateTabsetPanel(session, "helpTabs", selected="vax")
        updateTabsetPanel(session, "infoTabs", select="help")
    })
    observeEvent(input$strategyHelpBn,{
        updateTabsetPanel(session, "helpTabs", select="strategy")
        updateTabsetPanel(session, "infoTabs", select="help")
    })
    observeEvent(input$simHelpBn,{
        updateTabsetPanel(session, "helpTabs", select="sim")
        updateTabsetPanel(session, "infoTabs", select="help") 
    })
    
    #######################################################################################
})
