library(shiny)

# Base constants
baseCases = 568919
baseDeaths = 21368

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
        nIter = 39
        results = tibble(Iteration = 0,
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
                         Deaths_Over80 = 0,
                         
                         Vax_Under20 = 0,
                         Vax_20s = 0,
                         Vax_30s = 0,
                         Vax_40s = 0,
                         Vax_50s = 0,
                         Vax_60s = 0,
                         Vax_70s = 0,
                         Vax_Over80 = 0)
        
        # Display busy dialog
        show_modal_spinner(spin="swapping-squares", color="#112446", text="Simulating...")
        
        # Generate agents
        print("Generating agents")
        agents = sim_make_agents(input$paramStrategy, input$paramScaling)
        
        # Run simulation
        for (i in 1:nIter){
            update_modal_progress(value=progress, text=paste0("Simulating iteration ", i," of " ,nIter, "..."))
            
            print(paste0("Starting iteration ", i))
            agents = sim_iter(paramVax, agents, input$paramScaling)
            
            results = sim_results(agents, results, i, input$paramScaling)
            print(paste0("Iteration ", i, " complete"))
        }
        print("Simulation complete")
        values$results = mutate(results, Date = as_date("2021-01-01") + (7*Iteration))
        
        # Process summary results
        values$overallCases = values$results %>%
            mutate(`Total Cases` = Cases_Under20 + Cases_20s + Cases_30s + Cases_40s + Cases_50s + Cases_60s + Cases_70s + Cases_Over80,
                   `New Cases` = c(NA, diff(`Total Cases`))) %>%
            select(Iteration, Date, `Total Cases`, `New Cases`)
        values$overallDeaths = values$results %>%
            mutate(`Total Deaths` = Deaths_Under20 + Deaths_20s + Deaths_30s + Deaths_40s + Deaths_50s + Deaths_60s + Deaths_70s + Deaths_Over80,
                   `New Deaths` = c(NA, diff(`Total Deaths`))) %>%
            select(Iteration, Date, `Total Deaths`, `New Deaths`)
        values$overallVax = values$results %>%
            mutate(`Total Vaccinations` = Vax_Under20 + Vax_20s + Vax_30s + Vax_40s + Vax_50s + Vax_60s + Vax_70s + Vax_Over80,
                   `New Vaccinations` = c(NA, diff(`Total Vaccinations`))) %>%
            select(Iteration, Date, `Total Vaccinations`, `New Vaccinations`)
        
        values$simCases = values$overallCases %>%
            slice_tail(n=1) %>%
            pull(`Total Cases`)
        values$simDeaths = values$overallDeaths %>%
            slice_tail(n=1) %>%
            pull(`Total Deaths`)
        values$simVax = values$overallVax %>%
            slice_tail(n=1) %>%
            pull(`Total Vaccinations`)
        
        # Compute user-defined metric
        casesCoef = input$metricCases / 100
        deathsCoef = input$metricDeaths / 100
        values$casesReduction = -(values$simCases - baseCases) / baseCases
        values$deathsReduction = -(values$simDeaths - baseDeaths) / baseDeaths
        values$userMetric = 100 * (casesCoef*values$casesReduction + deathsCoef*values$deathsReduction)
        
        # Update summary buttons
        updateActionButton(session, "summaryCases", label=HTML(paste0("<h3><b>Total Cases</h3><h4>", format(values$simCases, big.mark=",", scientific=FALSE), "</h4></b>")))
        updateActionButton(session, "summaryDeaths", label=HTML(paste0("<h3><b>Total Deaths</h3><h4>", format(values$simDeaths, big.mark=",", scientific=FALSE), "</h4></b>")))
        updateActionButton(session, "summaryVax", label=HTML(paste0("<h3><b>Total Vaccinated</h3><h4>", format(values$simVax, big.mark=",", scientific=FALSE), "</h4></b>")))
        updateActionButton(session, "summaryMetric", label=HTML(paste0("<h3><b>Strategy Effectiveness</h3><h4>", format(round(values$userMetric, 2), scientific=FALSE), "%</h4></b>")))
        updateTabsetPanel(session, "summaryTabs", "cases")
        
        # Close busy dialog
        remove_modal_spinner()
        values$hasRun = TRUE
    })
    
    #######################################################################################
    # Summary displays ####################################################################
    
    # Cases time series
    output$summaryTotalCasesTS = renderPlotly({
        plot = values$overallCases %>%
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
        plot = values$overallCases %>%
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
        plot = values$overallDeaths %>%
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
        plot = values$overallDeaths %>%
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
        plot = values$overallVax %>%
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
        plot = values$overallVax %>%
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
    
    # User-defined metric summary
    output$summaryMetricCases = renderUI({
        fluidRow(
            column(width=6, align="right",
                   HTML("<h4><b>Expected Without Vaccine:<br>
                        Simulation Result:<br>
                        Percent Improvement:</b></h4>")
            ),
            column(width=6, align="left",
                   HTML(paste0("<h4>",
                               format(baseCases, big.mark=","), "<br>",
                               format(values$simCases, big.mark=","), "<br>",
                               format(round(100*values$casesReduction, 2), big.mark=","), "%</h4>"))
            )
        )
    })
    
    output$summaryMetricDeaths = renderUI({
        fluidRow(
            column(width=6, align="right",
                   HTML("<h4><b>Expected Without Vaccine:<br>
                        Simulation Result:<br>
                        Percent Improvement:</b></h4>")
            ),
            column(width=6, align="left",
                   HTML(paste0("<h4>",
                               format(baseDeaths, big.mark=","), "<br>",
                               format(values$simDeaths, big.mark=","), "<br>",
                               format(round(100*values$deathsReduction, 2), big.mark=","), "%</h4>"))
            )
        )
    })
    
    #######################################################################################
    # User-defined metric linked sliders ##################################################
    observeEvent(input$metricCases,{
        updateSliderInput(session, "metricDeaths", value=100-input$metricCases)
    })
    
    observeEvent(input$metricDeaths,{
        updateSliderInput(session, "metricCases", value=100-input$metricDeaths)
    })
    
    #######################################################################################
    # Results summary button variable handlers ############################################
    
    # Change to cases
    observeEvent(input$summaryCases,{ if (values$hasRun) { updateTabsetPanel(session, "summaryTabs", selected="cases") }})
    
    # Change to deaths
    observeEvent(input$summaryDeaths,{ if (values$hasRun) { updateTabsetPanel(session, "summaryTabs", selected="deaths") }})
    
    # Change to vaccinated
    observeEvent(input$summaryVax,{ if (values$hasRun) { updateTabsetPanel(session, "summaryTabs", selected="vax") }})
    
    # Change to user-defined metric
    observeEvent(input$summaryMetric,{ if (values$hasRun) { updateTabsetPanel(session, "summaryTabs", selected="metric") }})
    
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
    observeEvent(input$metricHelpBn,{
        updateTabsetPanel(session, "helpTabs", select="metric")
        updateTabsetPanel(session, "infoTabs", select="help")
    })
    
    #######################################################################################
    
    output$refReproductionRate = renderUI({
        HTML(markdown::markdownToHTML(knit("ref/reproductionRate.Rmd", quiet=TRUE)))
    })
})
