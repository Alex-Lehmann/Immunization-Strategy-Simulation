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
    
    # Change to user-defined
    observeEvent(input$summaryUser,{
        updateTabsetPanel(session, "summaryTabs", selected="user")
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
    # Results button labels ###############################################################
    simCases = 10135814
    simDeaths = round(simCases*0.02)
    simVax = 14524135
    simUser = 54.78
    
    output$summaryCasesLabel = renderText({paste0("<h3><b>Total Cases</h3><h4>", format(simCases, big.mark=","), "</h4></b>")})
    output$summaryDeathsLabel = renderText({paste0("<h3><b>Total Deaths</h3><h4>", format(simDeaths, big.mark=","), "</h4></b>")})
    output$summaryVaxLabel = renderText({paste0("<h3><b>Total Vaccinated</h3><h4>", format(simVax, big.mark=","), "</h4></b>")})
    output$summaryUserLabel = renderText({paste0("<h3><b>User Metric</h3><h4>", simUser, "%</h4></b>")})
})
