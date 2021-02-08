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
                                                          column(width=4, align="center",
                                                                 actionButton("summaryCases", HTML("<h3><b>Total Cases</h3><h4>-</h4></b>"), width="100%",
                                                                              style="border-radius:6px; background-color:#4CAF50; box-shadow:0 8px 16px 0 rgba(0,0,0,0.2), 0 6px 20px 0 rgba(0,0,0,0.19)")
                                                          ),
                                                          column(width=4, align="center",
                                                                 actionButton("summaryDeaths", HTML("<h3><b>Total Deaths</h3><h4>-</h4></b>"), width="100%",
                                                                              style="border-radius:6px; background-color:#DC2824; box-shadow:0 8px 16px 0 rgba(0,0,0,0.2), 0 6px 20px 0 rgba(0,0,0,0.19)")
                                                          ),
                                                          column(width=4, align="center",
                                                                 actionButton("summaryVax", HTML("<h3><b>Total Vaccinated</h3><h4>-</h4></b>"), width="100%",
                                                                              style="border-radius:6px; background-color:#428BCA; box-shadow:0 8px 16px 0 rgba(0,0,0,0.2), 0 6px 20px 0 rgba(0,0,0,0.19)")
                                                          )
                                                        ),
                                                        
                                                        # Plot and histogram displays
                                                        tabsetPanel(type="hidden", id="summaryTabs",
                                                                    tabPanelBody(NULL, value="start"),
                                                                    tabPanelBody(NULL, value="cases",
                                                                                 titlePanel(HTML("<b>Case Summary</b>")),
                                                                                 HTML("<br>"),
                                                                                 fluidRow(
                                                                                   column(width=6, align="center",
                                                                                          HTML("<h2>Cumulative Cases</h2>"),
                                                                                          plotOutput("summaryTotalCasesTS")
                                                                                   ),
                                                                                   column(width=6, align="center",
                                                                                          HTML("<h2>New Cases</h2>"),
                                                                                          plotOutput("summaryNewCasesTS")
                                                                                   )
                                                                                 )
                                                                    ),
                                                                    tabPanelBody(NULL, value="deaths",
                                                                                 titlePanel(HTML("<b>Mortality Summary</b>")),
                                                                                 HTML("<br>"),
                                                                                 fluidRow(
                                                                                   column(width=6, align="center",
                                                                                          HTML("<h2>Cumulative Deaths</h2>"),
                                                                                          plotOutput("summaryTotalDeathsTS")
                                                                                   ),
                                                                                   column(width=6, align="center",
                                                                                          HTML("<h2>New Deaths</h2>"),
                                                                                          plotOutput("summaryNewDeathsTS")
                                                                                   )
                                                                                 )
                                                                    ),
                                                                    tabPanelBody(NULL, value="vax",
                                                                                 titlePanel(HTML("<b>Vaccination Summary</b>")),
                                                                                 HTML("<br>"),
                                                                                 fluidRow(
                                                                                   column(width=6, align="center",
                                                                                          HTML("<h2>Cumulative Vaccinations</h2>"),
                                                                                          plotOutput("summaryTotalVaxTS")
                                                                                   ),
                                                                                   column(width=6, align="center",
                                                                                          HTML("<h2>New Vaccinations</h2>"),
                                                                                          plotOutput("summaryNewVaxsTS")
                                                                                   )
                                                                                 )
                                                                    )
                                                        )
                                                    ),
                                                    
                                                    # Settings panel
                                                    wellPanel(
                                                        titlePanel("Simulation Settings"),
                                                        
                                                        fluidRow(
                                                            
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
                                                                               list("Highest-Risk First" = "risk",
                                                                                    "Random" = "random"))
                                                            ),
                                                            column(width=3,
                                                                   helpText(HTML("<h4>Simulation Performance</h4>")),
                                                                   numericInput("paramScaling", "Agent Scaling Factor",
                                                                                value=10, min=1, max=20, step=1)
                                                            )
                                                        ),
                                                        
                                                        # Run simulation button
                                                        HTML("<br>"),
                                                        actionButton("runSimBn", HTML("<b>Simulate With Current Settings</b>"), icon("refresh"), width="100%")
                                                    )
                                                )
                                      )
                        )
               )
    )
))