library(shiny)

shinyUI(fluidPage(
    navbarPage("Ontario COVID-19 Immunization Strategy Simulation",
               # Dashboard page
               tabPanel("Dashboard",
                        # Main body
                        sidebarLayout(position="right",
                                      
                                      # Information side panel
                                      sidebarPanel(width=3, style = "position:fixed; width:inherit;",
                                                   titlePanel("Information"),
                                                   
                                                   tabsetPanel(type="tabs",
                                                               
                                                               tabPanel("About",
                                                                        HTML("<br>This app displays the results of different COVID-19 vaccination strategies in Ontario under a variety of simulated conditions. This allows users to quickly and easily review different vaccination strategies under different initial conditions to better inform decision making in the pandemic response.")
                                                               ),
                                                               tabPanel("Help",
                                                                        
                                                               )
                                                   )
                                      ),
                                      
                                      # Active main panel
                                      mainPanel(width=9,
                                                verticalLayout(
                                                    
                                                    # Results display
                                                    wellPanel(
                                                        titlePanel("Simulation Results"),
                                                        
                                                        # Summary buttons
                                                        fluidRow(
                                                          column(width=3, align="center",
                                                                 actionButton("summaryCases", htmlOutput("summaryCasesLabel"), width="100%",
                                                                              style="border-radius:6px; background-color:#4CAF50; box-shadow:0 8px 16px 0 rgba(0,0,0,0.2), 0 6px 20px 0 rgba(0,0,0,0.19)")
                                                          ),
                                                          column(width=3, align="center",
                                                                 actionButton("summaryDeaths", htmlOutput("summaryDeathsLabel"), width="100%",
                                                                              style="border-radius:6px; background-color:#DC2824; box-shadow:0 8px 16px 0 rgba(0,0,0,0.2), 0 6px 20px 0 rgba(0,0,0,0.19)")
                                                          ),
                                                          column(width=3, align="center",
                                                                 actionButton("summaryVax", htmlOutput("summaryVaxLabel"), width="100%",
                                                                              style="border-radius:6px; background-color:#428BCA; box-shadow:0 8px 16px 0 rgba(0,0,0,0.2), 0 6px 20px 0 rgba(0,0,0,0.19)")
                                                          ),
                                                          column(width=3, align="center",
                                                                 actionButton("summaryUser", htmlOutput("summaryUserLabel"), width="100%",
                                                                              style="border-radius:6px; box-shadow:0 8px 16px 0 rgba(0,0,0,0.2), 0 6px 20px 0 rgba(0,0,0,0.19)")
                                                          )
                                                        ),
                                                        
                                                        # Plot and histogram displays
                                                        tabsetPanel(type="hidden", id="summaryTabs",
                                                                    tabPanelBody(NULL, value="cases",
                                                                                 titlePanel(HTML("<b>Case Summary</b>")),
                                                                                 HTML("<br>"),
                                                                                 fluidRow(
                                                                                   column(width=6, align="center",
                                                                                          
                                                                                   ),
                                                                                   column(width=6, align="center",
                                                                                          
                                                                                   )
                                                                                 )
                                                                    ),
                                                                    tabPanelBody(NULL, value="deaths",
                                                                                 titlePanel(HTML("<b>Mortality Summary</b>")),
                                                                                 HTML("<br>"),
                                                                                 fluidRow(
                                                                                   column(width=6, align="center",
                                                                                   ),
                                                                                   column(width=6, align="center",
                                                                                          
                                                                                   )
                                                                                 )
                                                                    ),
                                                                    tabPanelBody(NULL, value="vax",
                                                                                 titlePanel(HTML("<b>Vaccination Summary</b>")),
                                                                                 HTML("<br>"),
                                                                                 fluidRow(
                                                                                   column(width=6, align="center",
                                                                                          
                                                                                   ),
                                                                                   column(width=6, align="center",
                                                                                          
                                                                                   )
                                                                                 )
                                                                    ),
                                                                    tabPanelBody(NULL, value="user",
                                                                                 titlePanel(HTML("<b>User-Defined Metric Summary</b>")),
                                                                                 HTML("<br>"),
                                                                                 fluidRow(
                                                                                   column(width=6, align="center",
                                                                                     
                                                                                   ),
                                                                                   column(width=6, align="center",
                                                                                     
                                                                                   )
                                                                                 )
                                                                    )
                                                        )
                                                    ),
                                                    
                                                    # Settings panel
                                                    wellPanel(
                                                        titlePanel("Simulation Settings"),
                                                        
                                                        fluidRow(
                                                            
                                                            # Outcome priorities settings
                                                            column(width=3,
                                                                   helpText(HTML("<h4>Outcome Priorities</h4>")),
                                                                   sliderInput("paramCases", "Minimize Cases", ticks=FALSE,
                                                                               min=0, max=100, value=50),
                                                                   HTML("<br>"),
                                                                   sliderInput("paramDeaths", "Minimize Deaths", ticks=FALSE,
                                                                               min=0, max=100, value=50)
                                                            ),
                                                            
                                                            # Initial case load
                                                            column(width=3,
                                                                   helpText(HTML("<h4>Initial Case Load</h4>")),
                                                                   numericInput("paramOriginal", "Original Strain",
                                                                                value=4800, min=0),
                                                            ),
                                                            
                                                            # Vaccine efficacy settings
                                                            column(width=3,
                                                                   helpText(HTML("<h4>Provincial Vaccine Availability</h4>")),
                                                                   numericInput("paramVax", "Vaccine Doses Per Week",
                                                                                value=82800, step=100)
                                                            ),
                                                            
                                                            # Vaccination strategy settings
                                                            column(width=3,
                                                                   helpText(HTML("<h4>Distribution Strategy</h4>")),
                                                                   selectInput("paramStrategy", "Preset",
                                                                               list("Highest-Risk First",
                                                                                    "Random",
                                                                                    "No Vaccination"))
                                                            )
                                                        ),
                                                        
                                                        # Run simulation button
                                                        HTML("<br>"),
                                                        actionButton("runSimBn", HTML("<b>Simulate With Current Settings</b>"), icon("refresh"), width="100%",
                                                                     style="background-color:#428BCA;")
                                                    )
                                                )
                                      )
                        )
               ),
               
               # Reference page
               tabPanel("Reference",
                 
               )
    )
    
))