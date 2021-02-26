library(shiny)

# Color palette
casesColor = "#FFB000"
deathsColor = "#DC267F"
vaxColor = "#648FFF"
partVaxColor = "#324880"

ptol8 = c("#332288", "#117733", "#44AA99", "#88CCEE", "#DDCC77", "#CC6677", "#AA4499", "#882255")
ptol5 = c("#332288", "#44AA99", "#DDCC77", "#CC6677", "#882255")

# Base constants
refCases = read_csv("ref/data/refCases.csv", col_types=cols())
baseCases = refCases %>%
    filter(Date == as_date("2021-10-01")) %>%
    summarize(Cases = mean(Reference)) %>%
    pull(Cases)

refDeaths = read_csv("ref/data/refDeaths.csv", col_types=cols())
baseDeaths = refDeaths %>%
    filter(Date == as_date("2021-10-01")) %>%
    summarize(Deaths = mean(Reference)) %>%
    pull(Deaths)

shinyServer(function(input, output, session){
    
    values = reactiveValues()
    values$hasRun = FALSE
    values$trialsWarning = FALSE
    #######################################################################################
    # Simulation ##########################################################################
    observeEvent(input$runSimBn,{
        
        # Localize dangerous inputs
        values$paramDoses = input$paramDoses
        values$metricCases = input$metricCases
        values$metricDeaths = input$metricDeaths
        
        # Validate inputs
        if (is.na(input$paramVax) | input$paramVax < 0){
            updateNumericInput(session, "paramVax", value=0)
        }
        if (is.na(input$paramTrials) | input$paramTrials < 1){
            updateNumericInput(session, "paramTrials", 1)
        }
        
        # Random seed
        if (!is.na(input$paramSeed)){ set.seed(input$paramSeed) } else { set.seed(NULL) }
        
        overallResults = NULL
        for (i in 1:input$paramTrials){
            print(paste0("Beginning trial #", i))
            
            # Simulation initial conditions
            nIter = 39
            results = tibble(Iteration = 0,
                             Cases_Under20 = 0, Cases_20s = 0, Cases_30s = 0, Cases_40s = 0, Cases_50s = 0, Cases_60s = 0, Cases_70s = 0, Cases_Over80 = 0,
                             Deaths_Under20 = 0, Deaths_20s = 0, Deaths_30s = 0, Deaths_40s = 0, Deaths_50s = 0, Deaths_60s = 0, Deaths_70s = 0, Deaths_Over80 = 0,
                             FullVax_Under20 = 0, FullVax_20s = 0, FullVax_30s = 0, FullVax_40s = 0, FullVax_50s = 0, FullVax_60s = 0, FullVax_70s = 0, FullVax_Over80 = 0,
                             PartialVax_Under20 = 0, PartialVax_20s = 0, PartialVax_30s = 0, PartialVax_40s = 0,  PartialVax_50s = 0, PartialVax_60s = 0, PartialVax_70s = 0, PartialVax_Over80 = 0,
                             Active_Under20 = 0, Active_20s = 0, Active_30s = 0, Active_40s = 0, Active_50s = 0, Active_60s = 0, Active_70s = 0, Active_Over80 = 0,
                             Immune_Under20 = 0, Immune_20s = 0, Immune_30s = 0, Immune_40s = 0, Immune_50s = 0, Immune_60s = 0, Immune_70s = 0, Immune_Over80 = 0,
                             
                             Cases_DeprQ1 = 0, Cases_DeprQ2 = 0, Cases_DeprQ3 = 0, Cases_DeprQ4 = 0, Cases_DeprQ5 = 0,
                             Deaths_DeprQ1 = 0, Deaths_DeprQ2 = 0, Deaths_DeprQ3 = 0, Deaths_DeprQ4 = 0, Deaths_DeprQ5 = 0,
                             FullVax_DeprQ1 = 0, FullVax_DeprQ2 = 0, FullVax_DeprQ3 = 0, FullVax_DeprQ4 = 0, FullVax_DeprQ5 = 0,
                             PartialVax_DeprQ1 = 0, PartialVax_DeprQ2 = 0, PartialVax_DeprQ3 = 0, PartialVax_DeprQ4 = 0, PartialVax_DeprQ5 = 0,
                             Active_DeprQ1 = 0, Active_DeprQ2 = 0, Active_DeprQ3 = 0, Active_DeprQ4 = 0, Active_DeprQ5 = 0,
                             Immune_DeprQ1 = 0, Immune_DeprQ2 = 0, Immune_DeprQ3 = 0, Immune_DeprQ4 = 0, Immune_DeprQ5 = 0)
            
            # Display busy dialog
            show_modal_spinner(spin="swapping-squares", color="#112446", text="Simulating...")
            
            # Generate agents and record active cases
            print("Generating agents")
            agents = sim_make_agents(input$paramStrategy, input$paramRank, input$paramScaling)
            
            # Initial state
            results = mutate(results,
                             Active_Under20 = sim_activeByAge(agents, "under20")*input$paramScaling,
                             Active_20s = sim_activeByAge(agents, "20s")*input$paramScaling,
                             Active_30s = sim_activeByAge(agents, "30s")*input$paramScaling,
                             Active_40s = sim_activeByAge(agents, "40s")*input$paramScaling,
                             Active_50s = sim_activeByAge(agents, "50s")*input$paramScaling,
                             Active_60s = sim_activeByAge(agents, "60s")*input$paramScaling,
                             Active_70s = sim_activeByAge(agents, "70s")*input$paramScaling,
                             Active_Over80 = sim_activeByAge(agents, "over80")*input$paramScaling,
                             
                             Active_DeprQ1 = sim_activeByDeprivation(agents, "Least Deprivation")*input$paramScaling,
                             Active_DeprQ2 = sim_activeByDeprivation(agents, "Low Deprivation")*input$paramScaling,
                             Active_DeprQ3 = sim_activeByDeprivation(agents, "Moderate Deprivation")*input$paramScaling,
                             Active_DeprQ4 = sim_activeByDeprivation(agents, "High Deprivation")*input$paramScaling,
                             Active_DeprQ5 = sim_activeByDeprivation(agents, "Highest Deprivation")*input$paramScaling,
                             
                             Immune_Under20 = sim_immuneByAge(agents, "under20")*input$paramScaling,
                             Immune_20s = sim_immuneByAge(agents, "20s")*input$paramScaling,
                             Immune_30s = sim_immuneByAge(agents, "30s")*input$paramScaling,
                             Immune_40s = sim_immuneByAge(agents, "40s")*input$paramScaling,
                             Immune_50s = sim_immuneByAge(agents, "50s")*input$paramScaling,
                             Immune_60s = sim_immuneByAge(agents, "60s")*input$paramScaling,
                             Immune_70s = sim_immuneByAge(agents, "70s")*input$paramScaling,
                             Immune_Over80 = sim_immuneByAge(agents, "over80")*input$paramScaling,
                             
                             Immune_DeprQ1 = sim_immuneByDeprivation(agents, "Least Deprivation")*input$paramScaling,
                             Immune_DeprQ2 = sim_immuneByDeprivation(agents, "Low Deprivation")*input$paramScaling,
                             Immune_DeprQ3 = sim_immuneByDeprivation(agents, "Moderate Deprivation")*input$paramScaling,
                             Immune_DeprQ4 = sim_immuneByDeprivation(agents, "High Deprivation")*input$paramScaling,
                             Immune_DeprQ5 = sim_immuneByDeprivation(agents, "Highest Deprivation")*input$paramScaling)
            
            # Run simulation
            for (j in 1:nIter){
                agents = sim_iter(input$paramVax, input$paramFullEff, input$paramPartEff, input$paramDoses, agents, input$paramScaling)
                results = sim_results(agents, results, j, input$paramScaling)
                print(paste0("Iteration ", j, " complete"))
            }
            print(paste0("Trial #", i, " complete"))
            results = mutate(results, Date = as_date("2021-01-01") + (7*Iteration))
            
            # Add trial results to total
            overallResults = rbind(overallResults, results)
        }
        
        # Process overall results
        values$results = overallResults %>%
            group_by(Date) %>%
            summarize(across(.fns=mean), .groups="drop") %>%
            mutate(across(!Date, round))
        
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
            mutate(`Total Fully Vaccinated` = rowSums(across(starts_with("FullVax"))),
                   `New Fully Vaccinated` = c(NA, diff(`Total Fully Vaccinated`)),
                   `Total Partially Vaccinated` = rowSums(across(starts_with("PartialVax"))),
                   `New Partially Vaccinated` = c(NA, diff(`Total Partially Vaccinated`)))
        
        values$simCases = values$overallCases %>%
            slice_tail(n=1) %>%
            pull(`Total Cases`)
        values$simDeaths = values$overallDeaths %>%
            slice_tail(n=1) %>%
            pull(`Total Deaths`)
        if (input$paramDoses == "Two Doses"){
            values$simVax = values$overallVax %>%
                slice_tail(n=1) %>%
                pull(`Total Fully Vaccinated`)
        } else {
            values$simVax = values$overallVax %>%
                slice_tail(n=1) %>%
                pull(`Total Partially Vaccinated`)
        }
        
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
        
        # Set tabs
        updateTabsetPanel(session, "mainTabs", "main")
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
            geom_point(color=casesColor) +
            geom_line(color=casesColor)
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
            geom_point(color=casesColor) +
            geom_line(color=casesColor)
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
            geom_point(color=casesColor) +
            geom_line(color=casesColor)
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
            geom_point(color=deathsColor) +
            geom_line(color=deathsColor)
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
            geom_point(color=deathsColor) +
            geom_line(color=deathsColor)
        ggplotly(plot) %>%
            layout(showlegend=TRUE, hovermode="x", spikedistance=-1,
                   xaxis=list(fixedrange=TRUE, showspikes=TRUE, spikemode="across", spikesnap="cursor", spikedash="solid", showline=TRUE, showgrid=TRUE),
                   yaxis=list(fixedrange=TRUE)) %>%
            config(displayModeBar = FALSE)
    })
    
    # Vaccinations time series
    output$summaryTotalVaxTS = renderPlotly({
        df = values$overallVax
        plot = NULL
        if (values$paramDoses == "Two Doses"){
            plot = df %>%
                filter(!is.na(`Total Fully Vaccinated`)) %>%
                ggplot(aes(x=Date, y=`Total Fully Vaccinated`)) +
                geom_point(color=vaxColor) +
                geom_line(color=vaxColor)
        } else {
            plot = df %>%
                filter(!is.na(`Total Partially Vaccinated`)) %>%
                ggplot(aes(x=Date, y=`Total Partially Vaccinated`)) +
                geom_point(color=vaxColor) +
                geom_line(color=vaxColor)
        }
        ggplotly(plot) %>%
            layout(showlegend=TRUE, hovermode="x", spikedistance=-1,
                   xaxis=list(fixedrange=TRUE, showspikes=TRUE, spikemode="across", spikesnap="cursor", spikedash="solid", showline=TRUE, showgrid=TRUE),
                   yaxis=list(fixedrange=TRUE)) %>%
            config(displayModeBar = FALSE)
    })
    
    output$summaryNewVaxsTS = renderPlotly({
        df = values$overallVax
        plot = NULL
        if (values$paramDoses == "Two Doses"){
            plot = df %>%
                filter(!is.na(`New Fully Vaccinated`)) %>%
                ggplot(aes(x=Date, y=`New Fully Vaccinated`)) +
                geom_point(color=vaxColor) +
                geom_line(color=vaxColor)
        } else {
            plot = df %>%
                filter(!is.na(`New Partially Vaccinated`)) %>%
                ggplot(aes(x=Date, y=`New Partially Vaccinated`)) +
                geom_point(color=vaxColor) +
                geom_line(color=vaxColor)
        }
        ggplotly(plot) %>%
            layout(showlegend=TRUE, hovermode="x", spikedistance=-1,
                   xaxis=list(fixedrange=TRUE, showspikes=TRUE, spikemode="across", spikesnap="cursor", spikedash="solid", showline=TRUE, showgrid=TRUE),
                   yaxis=list(fixedrange=TRUE)) %>%
            config(displayModeBar = FALSE)
    })
    
    # User-defined metric summary
    output$summaryMetricOverall = renderUI({
        fluidRow(
            column(width=6, align="right",
                   HTML("<h4><b>Case Mitigation Weight:<br>
                        Mortality Mitigation Weight:<br>
                        Overall Strategy Effectiveness:</b></h4>")
            ),
            column(width=6, align="left",
                   HTML(paste0("<h4>",
                               round(values$metricCases), "%<br>",
                               round(values$metricDeaths), "%<br>",
                               format(round(values$userMetric, 2)), "%</h4>"))
            )
        )
    })
    
    output$summaryMetricTotal = renderPlotly({
        
        # Compute percent improvements for cases and deaths over time
        casePercent = full_join(refCases, values$overallCases, by="Date") %>%
            transmute(Date = Date,
                      `Case Mitigation` = -(`Total Cases` - Reference) / Reference)
        deathPercent = full_join(refDeaths, values$overallDeaths, by="Date") %>%
            transmute(Date = Date,
                      `Mortality Mitigation` = -(`Total Deaths` - Reference) / Reference)
        
        # Compute user metric over time
        userMetric = full_join(casePercent, deathPercent, by="Date") %>%
            drop_na() %>%
            mutate(Overall = (values$metricCases*`Case Mitigation`) + (values$metricDeaths*`Mortality Mitigation`),
                   `Case Mitigation` = round(100 * `Case Mitigation`, 2),
                   `Mortality Mitigation` = round(100 * `Mortality Mitigation`, 2),
                   Reference = rep(0, 39)) %>%
            pivot_longer(!Date, names_to="Metric", values_to="Percent Improvement")
        
        # Plot
        plot = userMetric %>%
            ggplot(aes(x=Date, y=`Percent Improvement`, color=Metric, linetype=Metric)) +
            geom_line() +
            scale_color_manual(values=c(casesColor, deathsColor, vaxColor, "#000000")) +
            scale_linetype_manual(values=c("solid", "solid", "solid", "dotted")) +
            ylim(-100, 100)
           
        ggplotly(plot) %>%
            layout(legend=list(orientation="h", y=1.1),
                   showlegend=TRUE, hovermode="x", spikedistance=-1,
                   xaxis=list(fixedrange=TRUE, showspikes=TRUE, spikemode="across", spikesnap="cursor", spikedash="solid", showline=TRUE, showgrid=TRUE),
                   yaxis=list(fixedrange=TRUE)) %>%
            config(displayModeBar = FALSE)
    })
    
    output$summaryMetricCases = renderUI({
        fluidRow(
            column(width=6, align="right",
                   HTML("<h4><b>Cases Expected Without Vaccine:<br>
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
    
    output$summaryCasesComparison = renderPlotly({
        plot = full_join(refCases, values$overallCases, by="Date") %>%
            select(Date, Reference, `Total Cases`) %>%
            rename(Simulation = `Total Cases`) %>%
            pivot_longer(!Date, names_to="Data", values_to="Total Cases") %>%
            ggplot(aes(x=Date, y=`Total Cases`, color=Data, linetype=Data)) +
            geom_line() +
            scale_color_manual(values=c("#000000", casesColor)) +
            scale_linetype_manual(values=c("dotted", "solid")) +
            theme(legend.position="top")
        ggplotly(plot) %>%
            layout(legend=list(orientation="h", y=1.1),
                   showlegend=TRUE, hovermode="x", spikedistance=-1,
                   xaxis=list(fixedrange=TRUE, showspikes=TRUE, spikemode="across", spikesnap="cursor", spikedash="solid", showline=TRUE, showgrid=TRUE),
                   yaxis=list(fixedrange=TRUE)) %>%
            config(displayModeBar = FALSE)
    })
    
    output$summaryMetricDeaths = renderUI({
        fluidRow(
            column(width=6, align="right",
                   HTML("<h4><b>Deaths Expected Without Vaccine:<br>
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
    
    output$summaryDeathsComparison = renderPlotly({
        plot = full_join(refDeaths, values$overallDeaths, by="Date") %>%
            select(Date, Reference, `Total Deaths`) %>%
            rename(Simulation = `Total Deaths`) %>%
            pivot_longer(!Date, names_to="Data", values_to="Total Deaths") %>%
            ggplot(aes(x=Date, y=`Total Deaths`, color=Data, linetype=Data)) +
            geom_line() +
            scale_color_manual(values=c("#000000", deathsColor)) +
            scale_linetype_manual(values=c("dotted", "solid")) +
            theme(legend.position="top")
        ggplotly(plot) %>%
            layout(legend=list(orientation="h", y=1.1),
                   showlegend=TRUE, hovermode="x", spikedistance=-1,
                   xaxis=list(fixedrange=TRUE, showspikes=TRUE, spikemode="across", spikesnap="cursor", spikedash="solid", showline=TRUE, showgrid=TRUE),
                   yaxis=list(fixedrange=TRUE)) %>%
            config(displayModeBar = FALSE)
    })
    
    #######################################################################################
    # Epidemiological detail plots ########################################################
    
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
            geom_point(color=casesColor) +
            geom_line(color=casesColor) +
            geom_hline(aes(yintercept=1), alpha=0.5)
        
        ggplotly(plot) %>%
            layout(showlegend=TRUE, hovermode="x", spikedistance=-1,
                   xaxis=list(fixedrange=TRUE, showspikes=TRUE, spikemode="across", spikesnap="cursor", spikedash="solid", showline=TRUE, showgrid=TRUE),
                   yaxis=list(fixedrange=TRUE)) %>%
            config(displayModeBar = FALSE)
    })
    
    # Age group
    output$activeCasesTSbyAge = renderPlotly({
        df = values$results %>%
            select(Date, Active_Under20, Active_20s, Active_30s, Active_40s, Active_50s, Active_60s, Active_70s, Active_Over80) %>%
            rename(`Under 20` = Active_Under20, `20s` = Active_20s, `30s` = Active_30s, `40s` = Active_40s,
                   `50s` = Active_50s, `60s` = Active_60s, `70s` = Active_70s, `Over 80` = Active_Over80) %>%
            pivot_longer(!Date, names_to="AgeGroup", values_to="Active Cases") %>%
            mutate(`Age Group` = factor(AgeGroup, levels=c("Under 20", "20s", "30s", "40s", "50s", "60s", "70s", "Over 80"))) # For legend order
        
        plot = NULL
        if (input$activeCasesTSbyAgeType == "Stacked Area Plot"){
            plot = df %>%
                ggplot(aes(x=Date, y=`Active Cases`, fill=`Age Group`)) +
                geom_area() +
                scale_fill_manual(values=ptol8)
        } else {
            plot = df %>%
                ggplot(aes(x=Date, y=`Active Cases`, color=`Age Group`)) +
                geom_line() +
                scale_color_manual(values=ptol8)
        }
        
        ggplotly(plot) %>%
            layout(legend=list(orientation="h", y=1.1),
                   xaxis=list(fixedrange=TRUE),
                   yaxis=list(fixedrange=TRUE)) %>%
            config(displayModeBar = FALSE)
    })
    
    output$totalCasesTSbyAge = renderPlotly({
        df = values$results %>%
            select(Date, Cases_Under20, Cases_20s, Cases_30s, Cases_40s, Cases_50s, Cases_60s, Cases_70s, Cases_Over80) %>%
            rename(`Under 20` = Cases_Under20, `20s` = Cases_20s, `30s` = Cases_30s, `40s` = Cases_40s, `50s` = Cases_50s, `60s` = Cases_60s, `70s` = Cases_70s, `Over 80` = Cases_Over80) %>%
            pivot_longer(!Date, names_to="AgeGroup", values_to="Total Cases") %>%
            mutate(`Age Group` = factor(AgeGroup, levels=c("Under 20", "20s", "30s", "40s", "50s", "60s", "70s", "Over 80"))) # For legend order
        
        plot = NULL
        if (input$totalCasesTSbyAgeType == "Stacked Area Plot"){
            plot = df %>%
                ggplot(aes(x=Date, y=`Total Cases`, fill=`Age Group`)) +
                geom_area() +
                scale_fill_manual(values=ptol8)
        } else {
            plot = df %>%
                ggplot(aes(x=Date, y=`Total Cases`, color=`Age Group`)) +
                geom_line() +
                scale_color_manual(values=ptol8)
        }
        
        ggplotly(plot) %>%
            layout(legend=list(orientation="h", y=1.2),
                   xaxis=list(fixedrange=TRUE),
                   yaxis=list(fixedrange=TRUE)) %>%
            config(displayModeBar = FALSE)
    })
    
    output$newCasesTSbyAge = renderPlotly({
        df = values$results %>%
            select(Date, Cases_Under20, Cases_20s, Cases_30s, Cases_40s, Cases_50s, Cases_60s, Cases_70s, Cases_Over80) %>%
            mutate_at(vars(-Date), function(x){c(NA, diff(x))}) %>%
            drop_na() %>%
            rename(`Under 20` = Cases_Under20, `20s` = Cases_20s, `30s` = Cases_30s, `40s` = Cases_40s, `50s` = Cases_50s, `60s` = Cases_60s, `70s` = Cases_70s, `Over 80` = Cases_Over80) %>%
            pivot_longer(!Date, names_to="AgeGroup", values_to="New Cases") %>%
            mutate(`Age Group` = factor(AgeGroup, levels=c("Under 20", "20s", "30s", "40s", "50s", "60s", "70s", "Over 80"))) # For legend order
        
        plot = NULL
        if (input$newCasesTSbyAgeType == "Stacked Area Plot"){
            plot = df %>%
                ggplot(aes(x=Date, y=`New Cases`, fill=`Age Group`)) +
                geom_area() +
                scale_fill_manual(values=ptol8)
        } else {
            plot = df %>%
                ggplot(aes(x=Date, y=`New Cases`, color=`Age Group`)) +
                geom_line() +
                scale_color_manual(values=ptol8)
        }
        
        ggplotly(plot) %>%
            layout(legend=list(orientation="h", y=1.2),
                   xaxis=list(fixedrange=TRUE),
                   yaxis=list(fixedrange=TRUE)) %>%
            config(displayModeBar = FALSE)
    })
    
    # Material deprivation
    output$activeCasesTSbyDep = renderPlotly({
        df = values$results %>%
            select(Date = Date, `Lowest Deprivation` = Active_DeprQ1, `Low Deprivation` = Active_DeprQ2,
                   `Moderate Deprivation` = Active_DeprQ3, `High Deprivation` = Active_DeprQ4, `Highest Deprivation` = Active_DeprQ5) %>%
            pivot_longer(!Date, names_to="DeprivationLevel", values_to="Active Cases") %>%
            mutate(`Deprivation Level` = factor(DeprivationLevel, levels=c("Lowest Deprivation", "Low Deprivation", "Moderate Deprivation",
                                                                           "High Deprivation", "Highest Deprivation")))
        
        plot = NULL
        if (input$activeCasesTSbyDepType == "Stacked Area Plot"){
            plot = df %>%
                ggplot(aes(x=Date, y=`Active Cases`, fill=`Deprivation Level`)) +
                geom_area() +
                scale_fill_manual(values=ptol5)
        } else {
            plot = df %>%
                ggplot(aes(x=Date, y=`Active Cases`, color=`Deprivation Level`)) +
                geom_line() +
                scale_color_manual(values=ptol5)
        }
        
        ggplotly(plot) %>%
            layout(legend=list(orientation="h", y=1.1),
                   xaxis=list(fixedrange=TRUE),
                   yaxis=list(fixedrange=TRUE)) %>%
            config(displayModeBar = FALSE)
    })
    
    output$totalCasesTSbyDep = renderPlotly({
        df = values$results %>%
            select(Date = Date, `Lowest Deprivation` = Cases_DeprQ1, `Low Deprivation` = Cases_DeprQ2,
                   `Moderate Deprivation` = Cases_DeprQ3, `High Deprivation` = Cases_DeprQ4, `Highest Deprivation` = Cases_DeprQ5) %>%
            pivot_longer(!Date, names_to="DeprivationLevel", values_to="Total Cases") %>%
            mutate(`Deprivation Level` = factor(DeprivationLevel, levels=c("Lowest Deprivation", "Low Deprivation", "Moderate Deprivation",
                                                                           "High Deprivation", "Highest Deprivation")))
        
        plot = NULL
        if (input$totalCasesTSByDepType == "Stacked Area Plot"){
            plot = df %>%
                ggplot(aes(x=Date, y=`Total Cases`, fill=`Deprivation Level`)) +
                geom_area() +
                scale_fill_manual(values=ptol5)
        } else {
            plot = df %>%
                ggplot(aes(x=Date, y=`Total Cases`, color=`Deprivation Level`)) +
                geom_line() +
                scale_color_manual(values=ptol5)
        }
        
        ggplotly(plot) %>%
            layout(legend=list(orientation="h", y=1.2),
                   xaxis=list(fixedrange=TRUE),
                   yaxis=list(fixedrange=TRUE)) %>%
            config(displayModeBar = FALSE)
    })
    
    output$newCasesTSbyDep = renderPlotly({
        df = values$results %>%
            select(Date = Date, `Lowest Deprivation` = Cases_DeprQ1, `Low Deprivation` = Cases_DeprQ2,
                   `Moderate Deprivation` = Cases_DeprQ3, `High Deprivation` = Cases_DeprQ4, `Highest Deprivation` = Cases_DeprQ5) %>%
            mutate_at(vars(-Date), function(x){c(NA, diff(x))}) %>%
            drop_na() %>%
            pivot_longer(!Date, names_to="DeprivationLevel", values_to="New Cases") %>%
            mutate(`Deprivation Level` = factor(DeprivationLevel, levels=c("Lowest Deprivation", "Low Deprivation", "Moderate Deprivation",
                                                                           "High Deprivation", "Highest Deprivation")))
        
        plot = NULL
        if (input$newCasesTSByDepType == "Stacked Area Plot"){
            plot = df %>%
                ggplot(aes(x=Date, y=`New Cases`, fill=`Deprivation Level`)) +
                geom_area() +
                scale_fill_manual(values=ptol5)
        } else {
            plot = df %>%
                ggplot(aes(x=Date, y=`New Cases`, color=`Deprivation Level`)) +
                geom_line() +
                scale_color_manual(values=ptol5)
        }
        
        ggplotly(plot) %>%
            layout(legend=list(orientation="h", y=1.2),
                   xaxis=list(fixedrange=TRUE),
                   yaxis=list(fixedrange=TRUE)) %>%
            config(displayModeBar = FALSE)
    })
    
    #######################################################################################
    # Case Severity #######################################################################
    
    output$totalDeathsTSbyAge = renderPlotly({
        df = values$results %>%
            select(Date, matches("Deaths.*[[:digit:]]*s$|Deaths.*0$")) %>%
            pivot_longer(!Date, names_to="AgeGroup", values_to="Total Deaths") %>%
            mutate(AgeGroup = str_extract(AgeGroup, "(?<=_).*"),
                   AgeGroup = str_replace_all(AgeGroup, pattern="r", replacement="r "),
                   `Age Group` = factor(AgeGroup, levels=c("Under 20", "20s", "30s", "40s", "50s", "60s", "70s", "Over 80"))) # For legend order
        
        plot = NULL
        if (input$totalDeathsTSbyAgeType == "Stacked Area Plot"){
            plot = df %>%
                ggplot(aes(x=Date, y=`Total Deaths`, fill=`Age Group`)) +
                geom_area() +
                scale_fill_manual(values=ptol8)
        } else {
            plot = df %>%
                ggplot(aes(x=Date, y=`Total Deaths`, color=`Age Group`)) +
                geom_line() +
                scale_color_manual(values=ptol8)
        }
        
        ggplotly(plot) %>%
            layout(legend=list(orientation="h", y=1.2),
                   xaxis=list(fixedrange=TRUE),
                   yaxis=list(fixedrange=TRUE)) %>%
            config(displayModeBar = FALSE)
    })
    
    output$newDeathsTSbyAge = renderPlotly({
        df = values$results %>%
            select(Date, matches("Deaths.*[[:digit:]]*s$|Deaths.*0$")) %>%
            mutate(across(starts_with("Deaths"), function(x){ c(NA, diff(x)) })) %>%
            drop_na() %>%
            pivot_longer(!Date, names_to="AgeGroup", values_to="New Deaths") %>%
            mutate(AgeGroup = str_extract(AgeGroup, "(?<=_).*"),
                   AgeGroup = str_replace_all(AgeGroup, pattern="r", replacement="r "),
                   `Age Group` = factor(AgeGroup, levels=c("Under 20", "20s", "30s", "40s", "50s", "60s", "70s", "Over 80"))) # For legend order
        
        plot = NULL
        if (input$newDeathsTSbyAgeType == "Stacked Area Plot"){
            plot = df %>%
                ggplot(aes(x=Date, y=`New Deaths`, fill=`Age Group`)) +
                geom_area() +
                scale_fill_manual(values=ptol8)
        } else {
            plot = df %>%
                ggplot(aes(x=Date, y=`New Deaths`, color=`Age Group`)) +
                geom_line() +
                scale_color_manual(values=ptol8)
        }
        
        ggplotly(plot) %>%
            layout(legend=list(orientation="h", y=1.2),
                   xaxis=list(fixedrange=TRUE),
                   yaxis=list(fixedrange=TRUE)) %>%
            config(displayModeBar = FALSE)
    })
    
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
            scale_fill_manual(values=c(deathsColor, casesColor)) +
            theme(legend.position="none")
        
        ggplotly(plot) %>%
            config(displayModeBar = FALSE) %>%
            layout(xaxis=list(fixedrange=TRUE),
                   yaxis=list(fixedrange=TRUE))
    })
    
    output$mortalityPlot = renderPlotly({
        
        plot = values$results %>%
            mutate(Active = rowSums(across(matches("Active.*[[:digit:]]*s$|Active.*0$"))),
                   Deaths = c(NA, diff(rowSums(across(matches("Deaths.*[[:digit:]]*s$|Deaths.*0$"))))),
                   `Mortality Rate` = round(Deaths / Active, 4)) %>%
            filter(Iteration > 0) %>%
            ggplot(aes(x=Date, y=`Mortality Rate`)) +
            geom_point(color=deathsColor) +
            geom_line(color=deathsColor)
            
        ggplotly(plot) %>%
            layout(legend=list(orientation="h", y=1.1),
                   showlegend=TRUE, hovermode="x", spikedistance=-1,
                   xaxis=list(fixedrange=TRUE, showspikes=TRUE, spikemode="across", spikesnap="cursor", spikedash="solid", showline=TRUE, showgrid=TRUE),
                   yaxis=list(fixedrange=TRUE)) %>%
            config(displayModeBar = FALSE)
    })
    
    #######################################################################################
    # Vaccination and immunity plots ######################################################
    
    output$totalImmunityTS = renderPlotly({
        
        forStack = values$results %>%
            transmute(Date = Date,
                      `Post-Infection` = rowSums(across(matches("Immune.*[[:digit:]]*s$|Immune.*0$"))),
                      `Fully Vaccinated` = rowSums(across(matches("FullVax.*[[:digit:]]*s$|FullVax.*0$"))),
                      `Partially Vaccinated` = rowSums(across(matches("PartialVax.*[[:digit:]]*s|PartialVax.*0$")))) %>%
            pivot_longer(!Date, names_to="Immunity Type", values_to="Total Immune")
        
        plot = forStack %>%
            ggplot(aes(x=Date, y=`Total Immune`, fill=`Immunity Type`)) +
            geom_area(alpha=0.8) +
            scale_fill_manual(values=c("Post-Infection"=casesColor, "Fully Vaccinated"=vaxColor, "Partially Vaccinated"=partVaxColor))
        
        ggplotly(plot) %>%
            layout(legend=list(orientation="h", y=1.1),
                   showlegend=TRUE, hovermode="x", spikedistance=-1,
                   xaxis=list(fixedrange=TRUE, showspikes=TRUE, spikemode="across", spikesnap="cursor", spikedash="solid", showline=TRUE, showgrid=TRUE),
                   yaxis=list(fixedrange=TRUE)) %>%
            config(displayModeBar = FALSE)
    })
    
    output$immunityProportionTS = renderPlotly({
        
        plot = values$results %>%
            mutate(TotalNatural = rowSums(across(starts_with("Immune"))),
                   TotalFullVax = rowSums(across(starts_with("FullVax"))),
                   TotalPartVax = rowSums(across(starts_with("PartialVax"))),
                   TotalImmune = TotalNatural + TotalFullVax + TotalPartVax) %>%
            transmute(Date = Date,
                      `Post-Infection` = round(TotalNatural / TotalImmune, 4),
                      `Fully Vaccinated` = round(TotalFullVax / TotalImmune, 4),
                      `Partially Vaccinated` = round(TotalPartVax / TotalImmune, 4)) %>%
            pivot_longer(!Date, names_to="Immunity Type", values_to="Proportion") %>%
            ggplot(aes(x=Date, y=Proportion, fill=`Immunity Type`)) +
            geom_area(alpha=0.8) +
            scale_fill_manual(values=c("Post-Infection"=casesColor, "Fully Vaccinated"=vaxColor, "Partially Vaccinated"=partVaxColor))
        
        ggplotly(plot) %>%
            layout(legend=list(orientation="h", y=1.1),
                   showlegend=TRUE, hovermode="x", spikedistance=-1,
                   xaxis=list(fixedrange=TRUE, showspikes=TRUE, spikemode="across", spikesnap="cursor", spikedash="solid", showline=TRUE, showgrid=TRUE),
                   yaxis=list(fixedrange=TRUE)) %>%
            config(displayModeBar = FALSE)
    })
    
    # Age group
    output$totalFullVaxTSbyAge = renderPlotly({
        
        df = values$results %>%
            select(Date, matches("FullVax.*[[:digit:]]*s$|FullVax.*0$")) %>%
            pivot_longer(!Date, names_to="AgeGroup", values_to="Total Vaccinations") %>%
            mutate(AgeGroup = str_extract(AgeGroup, "(?<=_).*"),
                   AgeGroup = str_replace_all(AgeGroup, pattern="r", replacement="r "),
                   `Age Group` = factor(AgeGroup, levels=c("Under 20", "20s", "30s", "40s", "50s", "60s", "70s", "Over 80"))) # For legend order
        
        plot = NULL
        if (input$totalVaxTSbyAgeType == "Stacked Area Plot"){
            plot = df %>%
                ggplot(aes(x=Date, y=`Total Vaccinations`, fill=`Age Group`)) +
                geom_area() +
                scale_fill_manual(values=ptol8)
        } else {
            plot = df %>%
                ggplot(aes(x=Date, y=`Total Vaccinations`, color=`Age Group`)) +
                geom_line() +
                scale_color_manual(values=ptol8)
        }
            
        ggplotly(plot) %>%
            layout(legend=list(orientation="h", y=1.2),
                   xaxis=list(fixedrange=TRUE),
                   yaxis=list(fixedrange=TRUE)) %>%
            config(displayModeBar = FALSE)
    })
    
    output$newFullVaxTSbyAge = renderPlotly({
        
        df = values$results %>%
            select(Date, matches("FullVax.*[[:digit:]]*s$|FullVax.*0$")) %>%
            mutate(across(starts_with("FullVax"), function(x){ c(NA, diff(x)) })) %>%
            filter(Date > as_date("2021-01-01")) %>%
            pivot_longer(!Date, names_to="AgeGroup", values_to="New Vaccinations") %>%
            mutate(AgeGroup = str_extract(AgeGroup, "(?<=_).*"),
                   AgeGroup = str_replace_all(AgeGroup, pattern="r", replacement="r "),
                   `Age Group` = factor(AgeGroup, levels=c("Under 20", "20s", "30s", "40s", "50s", "60s", "70s", "Over 80"))) # For legend order
        
        plot = NULL
        if (input$newVaxTSbyAgeType == "Stacked Area Plot"){
            plot = df %>%
                ggplot(aes(x=Date, y=`New Vaccinations`, fill=`Age Group`)) +
                geom_area() +
                scale_fill_manual(values=ptol8)
        } else {
            plot = df %>%
                ggplot(aes(x=Date, y=`New Vaccinations`, color=`Age Group`)) +
                geom_line() +
                scale_color_manual(values=ptol8)
        }
        
        ggplotly(plot) %>%
            layout(legend=list(orientation="h", y=1.2),
                   xaxis=list(fixedrange=TRUE),
                   yaxis=list(fixedrange=TRUE)) %>%
            config(displayModeBar = FALSE)
    })
    
    #######################################################################################
    # Manual vaccine priority UI elements #################################################
    
    # Reactive rank list
    output$priorityRankList = renderUI({ rank_list("Higher Priority", "paramRank", labels=c(input$paramAgeGroups,input$paramDeprivationGroups)) })
    
    # Event handlers to show modal
    observeEvent(input$paramStrategy,{
        if (input$paramStrategy == "custom"){ showModal(priorityModal()) }
    })
    observeEvent(input$customStrategyBn,{
        showModal(priorityModal())
    })
    
    # Priority group select dialog
    priorityModal = function(){
        modalDialog(title="Select Priority Groups", size="l", footer=modalButton("Close Window"),
                    checkboxGroupInput("paramAgeGroups", "Age group(s):", inline=TRUE,
                                       c("Under 20", "20s", "30s", "40s", "50s", "60s", "70s", "Over 80"),
                                       selected=input$paramAgeGroups),
                    checkboxGroupInput("paramDeprivationGroups", "Material deprivation level(s):", inline=TRUE,
                                       c("Lowest Deprivation", "Low Deprivation", "Moderate Deprivation", "High Deprivation", "Highest Deprivation"),
                                       select=input$paramDeprivationGroups)
        )
    }
    
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
    # Warning modals ######################################################################
    
    observeEvent(input$paramTrials,{
        if (input$paramTrials > 1 & !values$trialsWarning){
            values$trialsWarning = TRUE
            showModal(modalDialog(title="Warning!",
                                  "Running multiple trials in one simulation may result in long computation times."))
        }
    })
    
    #######################################################################################
})
