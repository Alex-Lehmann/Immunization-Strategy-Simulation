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
                                                   
                                                   tabsetPanel(type="tabs", id="infoTabs",
                                                               
                                                               tabPanel("About", value="about",
                                                                        HTML("<br>This app displays the results of different COVID-19 vaccination strategies in Ontario under a variety of simulated conditions. This allows users to quickly and easily review different vaccination strategies under different initial conditions to better inform decision making in the pandemic response.")
                                                               ),
                                                               tabPanel("Help", value="help",
                                                                        tabsetPanel(type="hidden", id='helpTabs',
                                                                                    tabPanelBody(NULL, value="start",
                                                                                                 HTML("<br>Click the help buttons on the dashboard to display additional information about the simulation elements.")
                                                                                    ),
                                                                                    tabPanelBody(NULL, value="vax",
                                                                                                 HTML("<h3>Provincial Vaccine Availability</h3><br>
                                                                                                      <h4>Vaccine Doses Per Week</h4>
                                                                                                      <p>This parameter controls the mean number of vaccine doses administered per week. Passing a value of 0 results in a simulation with no vaccinations.")
                                                                                    ),
                                                                                    tabPanelBody(NULL, value="strategy",
                                                                                                 HTML("<h3>Distribution Strategy</h3><br>
                                                                                                      <h4>Presets</h4>
                                                                                                      <p>Users can select pre-built vaccine distribution strategies from this menu. The currently-supported presets are:
                                                                                                      <ul>
                                                                                                        <li><b>Highest-Risk First:</b> Vaccine distribution will prioritize individuals in age groups with higher COVID-19 mortality rates.</li>
                                                                                                        <li><b>Random:</b> Vaccines are distributed entirely randomly with no priority given to any group.</li>
                                                                                                      </ul>")
                                                                                    ),
                                                                                    tabPanelBody(NULL, value="sim",
                                                                                                 HTML("<h3>Simulation Settings</h3><br>
                                                                                                      <h4>Seed Value</h4>
                                                                                                      <p>Users may pass a seed value to the simulation to ensure reproducible results. Pass an empty value to run the simulation without a seed value.
                                                                                                      <h4>Agent Scaling Factor</h4>
                                                                                                      <p>This parameter allows users to increase the speed of the simulation procedure at the expense of some accuracy. Increase this value to increase computation speed, to a maximum of 20."))
                                                                        )
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
                                                            
                                                            # Vaccine efficacy settings
                                                            column(width=4,
                                                                   fluidRow(
                                                                       column(width=10,
                                                                              helpText(HTML("<h4>Provincial Vaccine Availability</h4>")) 
                                                                       ),
                                                                       column(width=2,
                                                                              actionButton("vaxHelpBn", NULL, icon("question"),
                                                                                           style="border-radius:100%")
                                                                       )
                                                                   ),
                                                                   
                                                                   # Number of doses input
                                                                   numericInput("paramVax", "Vaccine Doses Per Week",
                                                                                value=82800, step=100)
                                                            ),
                                                            
                                                            # Vaccination strategy settings
                                                            column(width=4,
                                                                   fluidRow(
                                                                     column(width=10, align="left",
                                                                            helpText(HTML("<h4>Distribution Strategy</h4>"))
                                                                     ),
                                                                     column(width=2,
                                                                            actionButton("strategyHelpBn", NULL, icon("question"),
                                                                                         style="border-radius:100%")
                                                                     )
                                                                   ),
                                                                   
                                                                   # Preset strategy input
                                                                   selectInput("paramStrategy", "Presets",
                                                                               list("Highest-Risk First" = "risk",
                                                                                    "Random" = "random"))
                                                            ),
                                                            
                                                            # Simulation settings
                                                            column(width=4,
                                                                   fluidRow(
                                                                     column(width=10,
                                                                            helpText(HTML("<h4>Simulation Settings</h4>"))
                                                                     ),
                                                                     column(width=2, align="left",
                                                                            actionButton("simHelpBn", NULL, icon("question"),
                                                                                         style="border-radius:100%")
                                                                     )
                                                                   ),
                                                                   
                                                                   numericInput("paramSeed", "Seed Value",
                                                                                value=7, min=0, step=1),
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