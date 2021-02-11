library(shiny)

# Load packages
packages = c("tidyverse", "lubridate", "shinybusy", "shinyWidgets", "plotly")

source("fn/loadPackages.R")
loadPackages(packages)
source("fn/sim.R")

shinyUI(fluidPage(
    # Global page settings
    chooseSliderSkin("Shiny", "#112446"),
    
    navbarPage("Ontario COVID-19 Immunization Strategy Simulation",
               # Dashboard page
               tabPanel("Dashboard",
                        # Main body
                        sidebarLayout(position="right",
                                      
                                      # Information side panel
                                      sidebarPanel(width=3, style = "position:fixed; width:inherit;",
                                                   titlePanel("Information"),
                                                   tabsetPanel(type="tabs", id="infoTabs",
                                                               
                                                               # General information
                                                               tabPanel("About", value="about",
                                                                        HTML("<br><p>This app displays the results of different COVID-19 vaccination strategies in Ontario under a variety of simulated conditions. This allows users to quickly and easily review different vaccination strategies under different initial conditions to better inform decision making in the pandemic response.
                                                                              <p>Click on the help buttons for details about dashboard elements. Detailed and reference information is available on the Data and Assumptions page.
                                                                              <h3>Contact</h3>"),
                                                                        # LinkedIn badge
                                                                        fluidRow(width=12, align="center",
                                                                          HTML("<script type='text/javascript' src='https://platform.linkedin.com/badges/js/profile.js' async defer></script>
                                                                               <div class='LI-profile-badge' data-version='v1' data-size='large' data-locale='en_US' data-type='vertical' data-theme='light' data-vanity='alex-lehmann-ds'><a class='LI-simple-link' href='https://ca.linkedin.com/in/alex-lehmann-ds?trk=profile-badge'>Alex Lehmann</a></div>"),
                                                                          HTML("<br><b>Email: </b><a href='mailto:alex.lehmann@cmail.carleton.ca'>alex.lehmann@cmail.carleton.ca</a>")
                                                                        ),
                                                                        # Contact information
                                                                        HTML("<h3>Source</h3>
                                                                             <a href='https://github.com/Alex-Lehmann/Immunization-Strategy-Simulation'>View the full source code on GitHub.</a>")
                                                               ),
                                                               
                                                               # Help tabs
                                                               tabPanel("Help", value="help",
                                                                        tabsetPanel(type="hidden", id='helpTabs',
                                                                                    tabPanelBody(NULL, value="start",
                                                                                                 HTML("<br>Click the help buttons on the dashboard to display additional information about the simulation elements.")
                                                                                    ),
                                                                                    tabPanelBody(NULL, value="vax",
                                                                                                 HTML("<h3>Provincial Vaccine Availability</h3>
                                                                                                      <h4>Vaccine Doses Per Week</h4>
                                                                                                      <p>This parameter controls the mean number of vaccine doses administered per week. Passing a value of 0 results in a simulation with no vaccinations.")
                                                                                    ),
                                                                                    tabPanelBody(NULL, value="strategy",
                                                                                                 HTML("<h3>Distribution Strategy</h3>
                                                                                                      <h4>Presets</h4>
                                                                                                      <p>Select pre-built vaccine distribution strategies from this menu. The currently-supported presets are:
                                                                                                      <ul>
                                                                                                        <li><b>Highest-Risk First:</b> Vaccine distribution will prioritize individuals in age groups with higher COVID-19 mortality rates.</li>
                                                                                                        <li><b>Random:</b> Vaccines are distributed randomly with no priority given to any group.</li>
                                                                                                      </ul>")
                                                                                    ),
                                                                                    tabPanelBody(NULL, value="sim",
                                                                                                 HTML("<h3>Simulation Settings</h3>
                                                                                                      <h4>Seed Value</h4>
                                                                                                      <p>Users may pass a seed value to the simulation to ensure reproducible results. Pass an empty value to run the simulation without a seed value.
                                                                                                      <h4>Agent Scaling Factor</h4>
                                                                                                      <p>This parameter allows users to increase the speed of the simulation procedure at the expense of some accuracy. Increase this value to decrease computation time.")
                                                                                    ),
                                                                                    tabPanelBody(NULL, value="metric",
                                                                                                 HTML("<h3>User-Defined Metric</h3>
                                                                                                      <h4>Reduce Cases</h4>
                                                                                                      <p>This parameter determines the amount of weight given to reducing the total number of cases when comparing the currently-selected strategy against no vaccinations. Increasing this value causes the metric to favour a reduction in cases rather than a reduction in deaths.
                                                                                                      <h4>Reduce Deaths</h4>
                                                                                                      <p>This parameter determines the amount of weight given to reducing the total number of deaths when comparing the currently-selected strategy against no vaccinations. Increasing this value causes the metric to favour a reduction in deaths rather than a reduction in cases.")
                                                                                    )
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
                                                          column(width=3, align="center",
                                                                 actionButton("summaryCases", HTML("<h3><b>Total Cases</h3><h4>-</h4></b>"), width="100%",
                                                                              style="border-radius:6px; background-color:#4CAF50; box-shadow:0 8px 16px 0 rgba(0,0,0,0.2), 0 6px 20px 0 rgba(0,0,0,0.19)")
                                                          ),
                                                          column(width=3, align="center",
                                                                 actionButton("summaryDeaths", HTML("<h3><b>Total Deaths</h3><h4>-</h4></b>"), width="100%",
                                                                              style="border-radius:6px; background-color:#DC2824; box-shadow:0 8px 16px 0 rgba(0,0,0,0.2), 0 6px 20px 0 rgba(0,0,0,0.19)")
                                                          ),
                                                          column(width=3, align="center",
                                                                 actionButton("summaryVax", HTML("<h3><b>Total Vaccinated</h3><h4>-</h4></b>"), width="100%",
                                                                              style="border-radius:6px; background-color:#428BCA; box-shadow:0 8px 16px 0 rgba(0,0,0,0.2), 0 6px 20px 0 rgba(0,0,0,0.19)")
                                                          ),
                                                          column(width=3, align="center",
                                                                 actionButton("summaryMetric", HTML("<h3><b>User Metric</h3><h4>-</h4></b>"), width="100%",
                                                                              style="border-radius:6px; box-shadow:0 8px 16px 0 rgba(0,0,0,0.2), 0 6px 20px 0 rgba(0,0,0,0.19)")
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
                                                                                          plotlyOutput("summaryTotalCasesTS")
                                                                                   ),
                                                                                   column(width=6, align="center",
                                                                                          HTML("<h2>New Cases</h2>"),
                                                                                          plotlyOutput("summaryNewCasesTS")
                                                                                   )
                                                                                 )
                                                                    ),
                                                                    tabPanelBody(NULL, value="deaths",
                                                                                 titlePanel(HTML("<b>Mortality Summary</b>")),
                                                                                 HTML("<br>"),
                                                                                 fluidRow(
                                                                                   column(width=6, align="center",
                                                                                          HTML("<h2>Cumulative Deaths</h2>"),
                                                                                          plotlyOutput("summaryTotalDeathsTS")
                                                                                   ),
                                                                                   column(width=6, align="center",
                                                                                          HTML("<h2>New Deaths</h2>"),
                                                                                          plotlyOutput("summaryNewDeathsTS")
                                                                                   )
                                                                                 )
                                                                    ),
                                                                    tabPanelBody(NULL, value="vax",
                                                                                 titlePanel(HTML("<b>Vaccination Summary</b>")),
                                                                                 HTML("<br>"),
                                                                                 fluidRow(
                                                                                   column(width=6, align="center",
                                                                                          HTML("<h2>Cumulative Vaccinations</h2>"),
                                                                                          plotlyOutput("summaryTotalVaxTS")
                                                                                   ),
                                                                                   column(width=6, align="center",
                                                                                          HTML("<h2>New Vaccinations</h2>"),
                                                                                          plotlyOutput("summaryNewVaxsTS")
                                                                                   )
                                                                                 )
                                                                    ),
                                                                    tabPanelBody(NULL, value="metric",
                                                                                 titlePanel(HTML("<b>User-Defined Metric Summary</b>")),
                                                                                 HTML("<br>"),
                                                                                 fluidRow(
                                                                                   column(width=6, align="center",
                                                                                          HTML("<h2>Case Mitigation</h2>"),
                                                                                          htmlOutput("summaryMetricCases")
                                                                                   ),
                                                                                   column(width=6, align="center",
                                                                                          HTML("<h2>Mortality Mitigation</h2>"),
                                                                                          htmlOutput("summaryMetricDeaths")
                                                                                   )
                                                                                 )
                                                                    )
                                                        )
                                                    ),
                                                    
                                                    # Settings panel
                                                    wellPanel(
                                                        titlePanel("Simulation Settings"),
                                                        
                                                        fluidRow(
                                                            
                                                            # User-defined metric
                                                            column(width=3,
                                                                   fluidRow(
                                                                      column(width=10,
                                                                             helpText(HTML("<h4>User-Defined Metric</h4"))
                                                                      ),
                                                                      column(width=2, align="right",
                                                                             actionButton("metricHelpBn", NULL, icon("question"),
                                                                                          style="border-radius:100%")
                                                                      )
                                                                   ),
                                                                   
                                                                   # Cases emphasis slider
                                                                   sliderInput("metricCases", "Reduce Cases", ticks=FALSE, post="%",
                                                                               min=0, max=100, value=50),
                                                                   
                                                                   # Deaths emphasis slider
                                                                   sliderInput("metricDeaths", "Reduce Deaths", ticks=FALSE, post="%",
                                                                               min=0, max=100, value=50)
                                                            ),
                                                            
                                                            # Vaccine efficacy settings
                                                            column(width=3,
                                                                   fluidRow(
                                                                       column(width=10,
                                                                              helpText(HTML("<h4>Provincial Vaccine Availability</h4>"))
                                                                       ),
                                                                       column(width=2, align="right",
                                                                              actionButton("vaxHelpBn", NULL, icon("question"),
                                                                                           style="border-radius:100%")
                                                                       )
                                                                   ),
                                                                   
                                                                   # Number of doses input
                                                                   numericInput("paramVax", "Vaccine Doses Per Week",
                                                                                value=82800, step=100)
                                                            ),
                                                            
                                                            # Vaccination strategy settings
                                                            column(width=3,
                                                                   fluidRow(
                                                                     column(width=10, align="left",
                                                                            helpText(HTML("<h4>Distribution Strategy</h4>"))
                                                                     ),
                                                                     column(width=2, align="right",
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
                                                            column(width=3,
                                                                   fluidRow(
                                                                     column(width=10,
                                                                            helpText(HTML("<h4>Simulation Settings</h4>"))
                                                                     ),
                                                                     column(width=2, align="right",
                                                                            actionButton("simHelpBn", NULL, icon("question"),
                                                                                         style="border-radius:100%")
                                                                     )
                                                                   ),
                                                                   
                                                                   numericInput("paramSeed", "Seed Value",
                                                                                value=NA, min=0, step=1),
                                                                   sliderInput("paramScaling", "Agent Scaling Factor", ticks=FALSE,
                                                                               value=100, min=1, max=200, step=1)
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