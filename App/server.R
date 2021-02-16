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
                         Vax_Over80 = 0,
                         
                         Active_Under20 = 0,
                         Active_20s = 0,
                         Active_30s = 0,
                         Active_40s = 0,
                         Active_50s = 0,
                         Active_60s = 0,
                         Active_70s = 0,
                         Active_Over80 = 0)
        
        # Display busy dialog
        show_modal_spinner(spin="swapping-squares", color="#112446", text="Simulating...")
        
        # Generate agents and record active cases
        print("Generating agents")
        agents = sim_make_agents(input$paramStrategy, input$paramScaling)
        results = mutate(results,
                         Active_Under20 = sim_activeByAge(agents, "under20")*input$paramScaling,
                         Active_20s = sim_activeByAge(agents, "20s")*input$paramScaling,
                         Active_30s = sim_activeByAge(agents, "30s")*input$paramScaling,
                         Active_40s = sim_activeByAge(agents, "40s")*input$paramScaling,
                         Active_50s = sim_activeByAge(agents, "50s")*input$paramScaling,
                         Active_60s = sim_activeByAge(agents, "60s")*input$paramScaling,
                         Active_70s = sim_activeByAge(agents, "70s")*input$paramScaling,
                         Active_Over80 = sim_activeByAge(agents, "over80")*input$paramScaling)
        
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
        updateTabsetPanel(session, "resultsTabs", "summary")
        
        # Reset epidemiological summary inputs
        updateSliderInput(session, "caseDistributionPlotTime", value=as_date("2021-10-01"))
        updateSelectInput(session, "ageGroupSummarySelect", selected="Under20")
        
        # Close busy dialog
        remove_modal_spinner()
        values$hasRun = TRUE
    })
    
    #######################################################################################
    # Summary displays ####################################################################
    
    # Cases time series
    output$summaryActiveCasesTS = renderPlotly({
        plot = values$results %>%
            transmute(Date = Date,
                      `Active Cases` = Active_Under20 + Active_20s + Active_30s + Active_40s + Active_50s + Active_60s + Active_70s + Active_Over80) %>%
            ggplot(aes(x=Date, y=`Active Cases`)) +
            geom_point(color="#4CAF50") +
            geom_line(color="#4CAF50")
        ggplotly(plot) %>%
            layout(showlegend=TRUE, hovermode="x", spikedistance=-1,
                   xaxis=list(fixedrange=TRUE, showspikes=TRUE, spikemode="across", spikesnap="cursor", spikedash="solid", showline=TRUE, showgrid=TRUE),
                   yaxis=list(fixedrange=TRUE)) %>%
            config(displayModeBar = FALSE)
    })
    
    output$summaryTotalCasesTS = renderPlotly({
        plot = values$overallCases %>%
            filter(!is.na(`Total Cases`)) %>%
            ggplot(aes(x=Date, y=`Total Cases`)) +
            geom_point(color="#4CAF50") +
            geom_line(color="#4CAF50") +
            ylab("Cumulative Cases")
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
            ylab("New Cases")
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
            ylab("Cumulative Deaths")
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
            ylab("New Deaths")
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
            ylab("Cumulative Vaccinations")
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
            ylab("New Vaccinations")
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
    # Epidemiological detail plots ########################################################
    output$activeCasesTSbyAge = renderPlotly({
        plot = values$results %>%
            select(Date, Active_Under20, Active_20s, Active_30s, Active_40s, Active_50s, Active_60s, Active_70s, Active_Over80) %>%
            rename(`Under 20` = Active_Under20, `20s` = Active_20s, `30s` = Active_30s, `40s` = Active_40s,
                   `50s` = Active_50s, `60s` = Active_60s, `70s` = Active_70s, `Over 80` = Active_Over80) %>%
            pivot_longer(!Date, names_to="AgeGroup", values_to="Active Cases") %>%
            mutate(`Age Group` = factor(AgeGroup, levels=c("Under 20", "20s", "30s", "40s", "50s", "60s", "70s", "Over 80"))) %>% # For legend order
            ggplot(aes(x=Date, y=`Active Cases`, color=`Age Group`)) +
            geom_line() +
            ylab("Active Cases")
        
        ggplotly(plot) %>%
            layout(legend=list(orientation="h", y=1.1),
                   xaxis=list(fixedrange=TRUE),
                   yaxis=list(fixedrange=TRUE)) %>%
            config(displayModeBar = FALSE)
    })
    
    output$totalCasesTSbyAge = renderPlotly({
        plot = values$results %>%
            select(Date, Cases_Under20, Cases_20s, Cases_30s, Cases_40s, Cases_50s, Cases_60s, Cases_70s, Cases_Over80) %>%
            rename(`Under 20` = Cases_Under20, `20s` = Cases_20s, `30s` = Cases_30s, `40s` = Cases_40s, `50s` = Cases_50s, `60s` = Cases_60s, `70s` = Cases_70s, `Over 80` = Cases_Over80) %>%
            pivot_longer(!Date, names_to="AgeGroup", values_to="Total Cases") %>%
            mutate(`Age Group` = factor(AgeGroup, levels=c("Under 20", "20s", "30s", "40s", "50s", "60s", "70s", "Over 80"))) %>% # For legend order
            ggplot(aes(x=Date, y=`Total Cases`, color=`Age Group`)) +
            geom_line() +
            ylab("Cumulative Cases")
        
        ggplotly(plot) %>%
            layout(legend=list(orientation="h", y=1.2),
                   xaxis=list(fixedrange=TRUE),
                   yaxis=list(fixedrange=TRUE)) %>%
            config(displayModeBar = FALSE)
    })
    
    output$newCasesTSbyAge = renderPlotly({
        plot = values$results %>%
            select(Date, Cases_Under20, Cases_20s, Cases_30s, Cases_40s, Cases_50s, Cases_60s, Cases_70s, Cases_Over80) %>%
            mutate_at(vars(-Date), function(x){c(NA, diff(x))}) %>%
            rename(`Under 20` = Cases_Under20, `20s` = Cases_20s, `30s` = Cases_30s, `40s` = Cases_40s, `50s` = Cases_50s, `60s` = Cases_60s, `70s` = Cases_70s, `Over 80` = Cases_Over80) %>%
            pivot_longer(!Date, names_to="AgeGroup", values_to="New Cases") %>%
            mutate(`Age Group` = factor(AgeGroup, levels=c("Under 20", "20s", "30s", "40s", "50s", "60s", "70s", "Over 80"))) %>% # For legend order
            ggplot(aes(x=Date, y=`New Cases`, color=`Age Group`)) +
            geom_line() +
            ylab("New Cases")
        
        ggplotly(plot) %>%
            layout(legend=list(orientation="h", y=1.2),
                   xaxis=list(fixedrange=TRUE),
                   yaxis=list(fixedrange=TRUE)) %>%
            config(displayModeBar = FALSE)
    })
    
    output$casesTable = renderDataTable({
        
        toTable = NULL
        if (input$caseTableSelect == "New"){ # Difference cases columns if new cases data is requested
            toTable = values$results %>%
                mutate(across(starts_with("Cases_"), function(x){ c(NA, diff(x)) })) %>%
                filter(Iteration > 0) %>%
                select("Date", paste0("Cases_", c("Under20", "20s", "30s", "40s", "50s", "60s", "70s", "Over80")))
        } else { # Otherwise, grab requested columns
            toTable = values$results %>%
                select("Date", paste0(input$caseTableSelect, c("Under20", "20s", "30s", "40s", "50s", "60s", "70s", "Over80")))
        }
        toTable = toTable %>%
            mutate(Total = rowSums(across(!matches("Date")))) %>%
            relocate(Total, .after=Date)
        colnames(toTable) = colnames(toTable) %>%
            str_extract("(?<=_).*|Date|Total") %>%
            str_replace_all(pattern="r", replacement="r ")
        
        toTable
    }, options=list(pageLength=8, bFilter=FALSE), filter="none")
    
    output$caseOutcomePlot = renderPlotly({
        
        # Get data for user-selected week
        selectedWeek = values$results %>%
            filter(Date == input$caseOutcomePlotTime)
        
        # Get resolved cases for each age group
        resolvedUnder20 = selectedWeek$Cases_Under20 - selectedWeek$Deaths_Under20
        resolved20s = selectedWeek$Cases_20s - selectedWeek$Deaths_20s
        resolved30s = selectedWeek$Cases_30s - selectedWeek$Deaths_30s
        resolved40s = selectedWeek$Cases_40s - selectedWeek$Deaths_40s
        resolved50s = selectedWeek$Cases_50s - selectedWeek$Deaths_50s
        resolved60s = selectedWeek$Cases_60s - selectedWeek$Deaths_60s
        resolved70s = selectedWeek$Cases_70s - selectedWeek$Deaths_70s
        resolvedOver80 = selectedWeek$Cases_Over80 - selectedWeek$Deaths_Over80
        
        # Create tibble and plot
        plot = tibble(AgeGroup = rep(c("Under 20", "20s", "30s", "40s", "50s", "60s", "70s", "Over 80"), each=2),
                      Outcome = rep(c("Recovered", "Fatal"), 8),
                      Cases = c(resolvedUnder20, selectedWeek$Deaths_Under20,
                                resolved20s, selectedWeek$Deaths_20s,
                                resolved30s, selectedWeek$Deaths_30s,
                                resolved40s, selectedWeek$Deaths_40s,
                                resolved50s, selectedWeek$Deaths_50s,
                                resolved60s, selectedWeek$Deaths_60s,
                                resolved70s, selectedWeek$Deaths_70s,
                                resolvedOver80, selectedWeek$Deaths_Over80)) %>%
            mutate(`Age Group` = factor(AgeGroup, levels=c("Under 20", "20s", "30s", "40s", "50s", "60s", "70s", "Over 80"))) %>% # For x-axis sorting
            ggplot(aes(x=`Age Group`, y=Cases, fill=Outcome)) +
            geom_bar(stat="identity", position="stack") +
            ylab("Cumulative Count") +
            scale_fill_manual(values=c("#DC2824", "#4CAF50")) +
            theme(legend.position="none")
        
        ggplotly(plot) %>%
            config(displayModeBar = FALSE)
    })
    
    output$reproductionPlot = renderPlotly({
        plot = values$results %>%
            mutate(Cases = Cases_Under20 + Cases_20s + Cases_30s + Cases_40s + Cases_50s + Cases_60s + Cases_70s + Cases_Over80,
                   Cases = c(NA, diff(Cases)),
                   Deaths = Deaths_Under20 + Deaths_20s + Deaths_30s + Deaths_40s + Deaths_50s + Deaths_60s + Deaths_70s + Deaths_Over80,
                   Deaths = c(NA, diff(Deaths)),
                   Active = Active_Under20 + Active_20s + Active_30s + Active_40s + Active_50s + Active_60s + Active_70s + Active_Over80) %>%
            transmute(Date = Date,
                      `Reproduction Rate` = round(2*Cases / (lag(Active) - Deaths), 2)) %>%
            ggplot(aes(x=Date, y=`Reproduction Rate`)) +
            geom_point(color="orange") +
            geom_line(color="orange") +
            geom_hline(aes(yintercept=1), alpha=0.5)
        
        ggplotly(plot) %>%
            layout(showlegend=TRUE, hovermode="x", spikedistance=-1,
                   xaxis=list(fixedrange=TRUE, showspikes=TRUE, spikemode="across", spikesnap="cursor", spikedash="solid", showline=TRUE, showgrid=TRUE),
                   yaxis=list(fixedrange=TRUE)) %>%
            config(displayModeBar = FALSE)
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
})
