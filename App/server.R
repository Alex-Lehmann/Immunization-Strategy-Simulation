library(shiny)

# Load packages
packages = c("sortable")

source("fn/loadPackages.R")
loadPackages(packages)

shinyServer(function(input, output, session){
    
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
    
    # Linked sliders for target metric ####################################################
    # Change cases -> deaths
    observeEvent(input$paramCases,{
        updateSliderInput(session, "paramDeaths", value=100-input$paramCases)
    })
    
    # Change deaths -> cases
    observeEvent(input$paramDeaths,{
        updateSliderInput(session, "paramCases", value=100-input$paramDeaths)
    })
    
    # Modal dialog appearance for advanced settings #######################################
    observeEvent(input$advancedSettings,{
        if (input$advancedSettings == 1){ showModal(source("ui/settingsWarningDialog.R"))}
    })
    
    #######################################################################################
    # Modal dialog appearance for manual vaccine priority #################################
    
    # UI element definitions
    output$uiPriorityAgeGroups = renderUI({
        checkboxGroupInput("paramAgeGroups", "High-Risk Age Groups", inline=TRUE,
                           c("Persons Aged 60-64", "Persons Aged 65-69",
                             "Persons Aged 70-74", "Persons Aged 75-79",
                             "Persons Aged 80+"),
                           selected=input$paramAgeGroups)
    })
    
    output$uiPriorityInstitutional = renderUI({
        checkboxGroupInput("paramInstitutional", "Institutional Living Settings", inline=TRUE,
                           c("Long-Term Care (LTC) Homes", "Prisons and Jails"),
                           selected=input$paramInstitutional)
    })
    
    output$uiPriorityRankList = renderUI({
        rank_list("Higher Priority","paramRank", labels=c(input$paramAgeGroups, input$paramInstitutional))
    })
    
    output$uiPriorityRemaining = renderUI({
        selectInput("paramRemaining", "Vaccine Priority Criterion",
                    list("Age (Descending)" = "age",
                         "Random Selection" = "random"),
                    selected=input$paramRemaining)
    })
    
    # Dialog definition
    priorityModal = function(){
        modalDialog(title="Edit Vaccine Priority", size="l", footer=modalButton("Close Window"),
                    helpText(HTML("<h4>1. Select priority groups</h4>")),
                    uiOutput("uiPriorityAgeGroups"),
                    uiOutput("uiPriorityInstitutional"),
                    
                    helpText(HTML("<h4>2. Set priority order</h4>")),
                    uiOutput("uiPriorityRankList"),
                    
                    helpText(HTML("<h4>3. Select criterion for remaining persons</h4>")),
                    uiOutput("uiPriorityRemaining")
        )
    }
    
    # Event handler to display dialog
    observeEvent(input$manualPriority | input$manualPriorityBn,{
        if (input$manualPriority){
            showModal(priorityModal())
        }
    })
    
    #######################################################################################
    # Mockup values #######################################################################
    output$summaryCasesLabel = renderText({paste0("<h3><b>Total Cases</h3><h4>", format(10246556, big.mark=","), "</h4></b>")})
    output$summaryDeathsLabel = renderText({paste0("<h3><b>Total Deaths</h3><h4>", format(204931, big.mark=","), "</h4></b>")})
    output$summaryVaxLabel = renderText({paste0("<h3><b>Total Vaccinated</h3><h4>", format(14745040, big.mark=","), "</h4></b>")})
    output$phuCasesLabel = renderText({paste0("<h3><b>Local Cases</h3><h4>", format(679054, big.mark=","), "</h4></b>")})
    output$phuDeathsLabel = renderText({paste0("<h3><b>Local Deaths</h3><h4>", format(20372, big.mark=","), "</h4></b>")})
    output$phuVaxLabel = renderText({paste0("<h3><b>Local Vaccinated</h3><h4>", format(988681, big.mark=","), "</h4></b>")})
})
