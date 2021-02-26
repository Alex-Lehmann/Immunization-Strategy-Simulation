library(shiny)
library(tidyverse)
library(lubridate)
library(shinybusy)
library(shinyWidgets)
library(plotly)
library(DT)
library(sortable)
source("fn/sim.R")

# Color palette
casesColor = "#FFB000"
deathsColor = "#DC267F"
vaxColor = "#648FFF"

shinyUI(fluidPage(
    # Global page settings
    chooseSliderSkin("Shiny", "#112446"),
    
    # Page
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
                                                                              <p>Click on the help buttons for details about dashboard elements. Detailed information regarding data and model assumptions is available on the Reference page.
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
                                                                                                      <p>This parameter controls the mean number of vaccine doses administered per week. Passing a value of 0 results in a simulation with no vaccinations.<br>
                                                                                                      <h4>Full Efficacy</h4>
                                                                                                      <p>Use this parameter to set the efficacy of the vaccine <i>after two doses</i>. An agent who is exposed to SARS-CoV-2 after two doses of vaccine will be protected from infection according to the selected value of this parameter.<br>
                                                                                                      <h4>Partial Efficacy</h4>
                                                                                                      <p>Use this paramter to set the efficacy of the vaccine <i>after a single dose</i>. An agent who is exposed to SARS-CoV-2 after one dose of vaccine will be protected from infection according to the selected value of this parameter.<br>")
                                                                                    ),
                                                                                    tabPanelBody(NULL, value="strategy",
                                                                                                 HTML("<h3>Distribution Strategy</h3>
                                                                                                      <h4>Vaccination Priority</h4>
                                                                                                      <p>Select vaccine priority strategies from this menu. The currently-supported choices are:
                                                                                                      <ul>
                                                                                                        <li><b>Age (Oldest First):</b> Vaccine distribution will prioritize individuals in older age groups.</li>
                                                                                                        <li><b>Age (Youngest First):</b> Vaccine distribution will prioritize individuals in younger age groups.</li>
                                                                                                        <li><b>Material Deprivation (High to Low):</b> Vaccine distribution will prioritize individuals residing in areas with high material deprivation levels, as determined by the 2016 Ontario Marginalization Index.</li>
                                                                                                        <li><b>Material Deprivation (Low to High):</b> Vaccine distribution will prioritize individuals residing in areas with low material deprivations levels, as determined by the 2016 Ontario Marginalization Index.</li>
                                                                                                        <li><b>Random:</b> Vaccines are distributed randomly with no priority given to any group.</li>
                                                                                                        <li><b>User-Defined:</b> Users may define their own vaccination strategies by selecting their groups of interest and ordering to establish priority. Non-prioritized groups will be vaccinated randomly after the priority groups.</li>
                                                                                                      </ul>
                                                                                                      <h4>Number of Doses Per Patient</h4>
                                                                                                      <p>This choice determines how many doses of vaccine each agent may receive. A one-dose strategy will result in all persons receiving a only single dose while a two-dose strategy will result in all persons receiving a second dose after 4 weeks.<br>")
                                                                                    ),
                                                                                    tabPanelBody(NULL, value="sim",
                                                                                                 HTML("<h3>Simulation Settings</h3>
                                                                                                      <h4>Seed Value</h4>
                                                                                                      <p>Users may pass a seed value to the simulation to ensure reproducible results. Pass an empty value to run the simulation without a seed value.<br>
                                                                                                      <h4>Agent Scaling Factor</h4>
                                                                                                      <p>This parameter allows users to increase the speed of the simulation procedure at the expense of some accuracy. Increase this value to decrease computation time.<br>
                                                                                                      <h4>Number of Trials</h4>
                                                                                                      <p>The number of simulation trials to run. Displayed results will be the mean of the results of each individual trial. Note that this parameter can greatly increase computation time.<br>")
                                                                                    ),
                                                                                    tabPanelBody(NULL, value="metric",
                                                                                                 HTML("<h3>Evaluation Priorities</h3>
                                                                                                      <h4>Reduce Cases</h4>
                                                                                                      <p>This parameter determines the amount of weight given to reducing the total number of cases when comparing the currently-selected strategy against no vaccinations. Increasing this value causes the metric to favour a reduction in cases rather than a reduction in deaths.<br>
                                                                                                      <h4>Reduce Deaths</h4>
                                                                                                      <p>This parameter determines the amount of weight given to reducing the total number of deaths when comparing the currently-selected strategy against no vaccinations. Increasing this value causes the metric to favour a reduction in deaths rather than a reduction in cases.<br>")
                                                                                    )
                                                                        )
                                                               )
                                                   )
                                      ),
                                      
                                      # Active main panel
                                      mainPanel(width=9,
                                                verticalLayout(
                                                    
                                                    # Results display
                                                    tabsetPanel(type="hidden", id="mainTabs",
                                                                tabPanelBody(NULL, value="start"),
                                                                tabPanelBody(NULL, value="main",
                                                                             wellPanel(
                                                                                 titlePanel(HTML("<h1>Simulation Results</h1>")),
                                                                                 
                                                                                 tabsetPanel(type="tabs", id="resultsTabs",
                                                                                             
                                                                                             # High-level summary
                                                                                             tabPanel("Summary", value="summary",
                                                                                                      HTML("<br>"),
                                                                                                      # Summary buttons
                                                                                                      fluidRow(
                                                                                                          column(width=3, align="center",
                                                                                                                 actionButton("summaryCases", HTML("<h3><b>Total Cases</h3><h4>-</h4></b>"), width="100%",
                                                                                                                              style=paste0("border-radius:6px; background-color:", casesColor, "; box-shadow:0 8px 16px 0 rgba(0,0,0,0.2), 0 6px 20px 0 rgba(0,0,0,0.19)"))
                                                                                                          ),
                                                                                                          column(width=3, align="center",
                                                                                                                 actionButton("summaryDeaths", HTML("<h3><b>Total Deaths</h3><h4>-</h4></b>"), width="100%",
                                                                                                                              style=paste0("border-radius:6px; background-color:", deathsColor, "; box-shadow:0 8px 16px 0 rgba(0,0,0,0.2), 0 6px 20px 0 rgba(0,0,0,0.19)"))
                                                                                                          ),
                                                                                                          column(width=3, align="center",
                                                                                                                 actionButton("summaryVax", HTML("<h3><b>Total Vaccinated</h3><h4>-</h4></b>"), width="100%",
                                                                                                                              style=paste0("border-radius:6px; background-color:", vaxColor, "; box-shadow:0 8px 16px 0 rgba(0,0,0,0.2), 0 6px 20px 0 rgba(0,0,0,0.19)"))
                                                                                                          ),
                                                                                                          column(width=3, align="center",
                                                                                                                 actionButton("summaryMetric", HTML("<h3><b>Strategy Effectiveness</h3><h4>-</h4></b>"), width="100%",
                                                                                                                              style="border-radius:6px; box-shadow:0 8px 16px 0 rgba(0,0,0,0.2), 0 6px 20px 0 rgba(0,0,0,0.19)")
                                                                                                          )
                                                                                                      ),
                                                                                                      
                                                                                                      # Plot and histogram displays
                                                                                                      tabsetPanel(type="hidden", id="summaryTabs",
                                                                                                                  tabPanelBody(NULL, value="start"),
                                                                                                                  tabPanelBody(NULL, value="cases",
                                                                                                                               titlePanel(HTML("<b>Case Summary</b>")),
                                                                                                                               HTML("<h2>Active Cases</h2>"),
                                                                                                                               plotlyOutput("summaryActiveCasesTS"),
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
                                                                                                                               titlePanel(HTML("<b>Strategy Effectiveness Summary</b>")),
                                                                                                                               fluidRow(
                                                                                                                                   column(width=6, align="center",
                                                                                                                                          HTML("<h2>Case Mitigation</h2>"),
                                                                                                                                          htmlOutput("summaryMetricCases"),
                                                                                                                                          plotlyOutput("summaryCasesComparison")
                                                                                                                                   ),
                                                                                                                                   column(width=6, align="center",
                                                                                                                                          HTML("<h2>Mortality Mitigation</h2>"),
                                                                                                                                          htmlOutput("summaryMetricDeaths"),
                                                                                                                                          plotlyOutput("summaryDeathsComparison")
                                                                                                                                   )
                                                                                                                               ),
                                                                                                                               HTML("<h2>Strategy Effectiveness</h2>"),
                                                                                                                               htmlOutput("summaryMetricOverall"),
                                                                                                                               plotlyOutput("summaryMetricTotal"),
                                                                                                                  )
                                                                                                      )
                                                                                             ),
                                                                                             
                                                                                             # Detailed epidemiological results
                                                                                             tabPanel("Case Spread", value="cases",
                                                                                                      # Reproduction rate
                                                                                                      titlePanel(HTML("<h1>Reproduction Rate</h1>")),
                                                                                                      plotlyOutput("reproductionPlot"),
                                                                                                      
                                                                                                      titlePanel(HTML("<br><h1>Sub-Populations of Interest</h1>")),
                                                                                                      tabsetPanel(
                                                                                                          tabPanel("Age Groups",
                                                                                                                   titlePanel(HTML("<h2>Time Series of Cases, By Age Group</h2>")),
                                                                                                                   
                                                                                                                   titlePanel(HTML("<h3>Active Cases</h3>")),
                                                                                                                   radioButtons("activeCasesTSbyAgeType", "Display as:", inline=TRUE,
                                                                                                                                c("Stacked Area Plot", "Line Plot")),
                                                                                                                   plotlyOutput("activeCasesTSbyAge"),
                                                                                                                   
                                                                                                                   fluidRow(
                                                                                                                       column(width=6, align="center",
                                                                                                                              titlePanel(HTML("<h3>Cumulative Cases</h3>")),
                                                                                                                              radioButtons("totalCasesTSbyAgeType", "Display as:", inline=TRUE,
                                                                                                                                           c("Stacked Area Plot", "Line Plot")),
                                                                                                                              plotlyOutput("totalCasesTSbyAge")
                                                                                                                       ),
                                                                                                                       column(width=6, align="center",
                                                                                                                              titlePanel(HTML("<h3>New Cases</h3>")),
                                                                                                                              radioButtons("newCasesTSbyAgeType", "Display as:", inline=TRUE,
                                                                                                                                           c("Stacked Area Plot", "Line Plot"), selected="Line Plot"),
                                                                                                                              plotlyOutput("newCasesTSbyAge")
                                                                                                                       )
                                                                                                                   )
                                                                                                          ),
                                                                                                          
                                                                                                          tabPanel("Material Deprivation Levels",
                                                                                                                   titlePanel(HTML("<h2>Time Series of Cases, By Material Deprivation Level</h2>")),
                                                                                                                   
                                                                                                                   titlePanel(HTML("<h3>Active Cases</h3>")),
                                                                                                                   radioButtons("activeCasesTSbyDepType", "Display as:", inline=TRUE,
                                                                                                                                c("Stacked Area Plot", "Line Plot")),
                                                                                                                   plotlyOutput("activeCasesTSbyDep"),
                                                                                                                   
                                                                                                                   fluidRow(
                                                                                                                       column(width=6, align="center",
                                                                                                                              titlePanel(HTML("<h3>Cumulative Cases</h3>")),
                                                                                                                              radioButtons("totalCasesTSByDepType", "Display as:", inline=TRUE,
                                                                                                                                           c("Stacked Area Plot", "Line Plot")),
                                                                                                                              plotlyOutput("totalCasesTSbyDep")
                                                                                                                       ),
                                                                                                                       column(width=6, align="center",
                                                                                                                              titlePanel(HTML("<h3>New Cases</h3>")),
                                                                                                                              radioButtons("newCasesTSByDepType", "Display as:", inline=TRUE,
                                                                                                                                           c("Stacked Area Plot", "Line Plot"), selected="Line Plot"),
                                                                                                                              plotlyOutput("newCasesTSbyDep")
                                                                                                                        ),
                                                                                                                   )
                                                                                                          )
                                                                                                      )
                                                                                             ),
                                                                                             
                                                                                             # Detailed mortality results
                                                                                             tabPanel("Illness Severity", value="severity",
                                                                                                      
                                                                                                      # Mortality rate
                                                                                                      titlePanel(HTML("<h1>Mortality Rate</h1>")),
                                                                                                      plotlyOutput("mortalityPlot"),
                                                                                                      
                                                                                                      # Time series by age group
                                                                                                      titlePanel(HTML("<br><h2>Age Group Results</h2>")),
                                                                                                      titlePanel(HTML("<h2>Time Series of Deaths, By Age Group</h2>")),
                                                                                                      fluidRow(
                                                                                                          column(width=6, align="center",
                                                                                                                 titlePanel(HTML("<h3>Cumulative Deaths</h3>")),
                                                                                                                 radioButtons("totalDeathsTSbyAgeType", "Display as:", inline=TRUE,
                                                                                                                              c("Stacked Area Plot", "Line Plot")),
                                                                                                                 plotlyOutput("totalDeathsTSbyAge")
                                                                                                          ),
                                                                                                          column(width=6, align="center",
                                                                                                                 titlePanel(HTML("<h3>New Deaths</h3>")),
                                                                                                                 radioButtons("newDeathsTSbyAgeType", "Display as:", inline=TRUE,
                                                                                                                              c("Stacked Area Plot", "Line Plot"), selected="Line Plot"),
                                                                                                                 plotlyOutput("newDeathsTSbyAge")
                                                                                                          )
                                                                                                      ),
                                                                                                      
                                                                                                      # Age group comparison
                                                                                                      titlePanel(HTML("<br><h2>Cumulative Outcomes Per Age Group</h2>")),
                                                                                                      plotlyOutput("caseOutcomePlot"),
                                                                                                      HTML("<br>"),
                                                                                                      sliderInput("caseOutcomePlotTime", "Show data through:",
                                                                                                                  min=as_date("2021-01-08"), max=as_date("2021-10-01"), step=7, value=as_date("2021-10-01"))   
                                                                                             ),
                                                                                             
                                                                                             # Detailed vaccinations
                                                                                             tabPanel("Vaccinations and Immunity", value="vax",
                                                                                                      # Time series overall
                                                                                                      titlePanel(HTML("<h2>Time Series of Immunity, Overall")),
                                                                                                      titlePanel(HTML("<h3>Total Immunity")),
                                                                                                      plotlyOutput("totalImmunityTS"),
                                                                                                      
                                                                                                      titlePanel(HTML("<h3>Immunity Types by Proportion</h3>")),
                                                                                                      plotlyOutput("immunityProportionTS"),
                                                                                                      
                                                                                                      titlePanel(HTML("<h2>Time Series of Vaccinations, By Age Group</h2>")),
                                                                                                      fluidRow(
                                                                                                          column(width=6, align="center",
                                                                                                                 titlePanel(HTML("<h3>Cumulative Vaccinations</h3>")),
                                                                                                                 radioButtons("totalVaxTSbyAgeType", "Display as:", inline=TRUE,
                                                                                                                              c("Stacked Area Plot", "Line Plot")),
                                                                                                                 plotlyOutput("totalFullVaxTSbyAge")
                                                                                                          ),
                                                                                                          column(width=6, align="center",
                                                                                                                 titlePanel(HTML("<h3>New Vaccinations</h3>")),
                                                                                                                 radioButtons("newVaxTSbyAgeType", "Display as:", inline=TRUE,
                                                                                                                              c("Stacked Area Plot", "Line Plot"), selected="Line Plot"),
                                                                                                                 plotlyOutput("newFullVaxTSbyAge")
                                                                                                          ),
                                                                                                      )
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
                                                            column(width=3,
                                                                   fluidRow(
                                                                       column(width=10,
                                                                              helpText(HTML("<h4>Vaccine Parameters</h4>"))
                                                                       ),
                                                                       column(width=2, align="right",
                                                                              actionButton("vaxHelpBn", NULL, icon("question"),
                                                                                           style="border-radius:100%")
                                                                       )
                                                                   ),
                                                                   
                                                                   # Number of doses input
                                                                   numericInput("paramVax", "Vaccine Doses Per Week",
                                                                                value=165000, step=100),
                                                                   
                                                                   # Efficacy inputs
                                                                   sliderInput("paramFullEff", "Full Efficacy", ticks=FALSE, post="%",
                                                                               min=0, max=100, value=95),
                                                                   sliderInput("paramPartEff", "Partial Efficacy", ticks=FALSE, post="%",
                                                                               min=0, max=100, value=54)
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
                                                                   
                                                                   # Strategy input
                                                                   selectInput("paramStrategy", "Vaccination Priority",
                                                                               list("Age (Oldest First)" = "ageDesc",
                                                                                    "Age (Youngest First)" = "ageAsc",
                                                                                    "Material Deprivation (High to Low)" = "depDesc",
                                                                                    "Material Deprivation (Low to High)" = "depAsc",
                                                                                    "Random" = "random",
                                                                                    "User-Defined" = "custom")),
                                                                   conditionalPanel("input.paramStrategy == 'custom'",
                                                                                    uiOutput("priorityRankList"),
                                                                                    actionButton("customStrategyBn", "Edit Priority Groups", width="100%"),
                                                                                    HTML("<br><br>")),
                                                                   
                                                                   # Dosage selection
                                                                   radioButtons("paramDoses", "Number of Doses Per Patient", inline=TRUE,
                                                                                c("Two Doses", "One Dose"))
                                                            ),
                                                            
                                                            # User-defined metric
                                                            column(width=3,
                                                                   fluidRow(
                                                                       column(width=10,
                                                                              helpText(HTML("<h4>Evaluation Priorities</h4"))
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
                                                                               value=50, min=1, max=100, step=1),
                                                                   numericInput("paramTrials", "Number of Trials",
                                                                                value=1, min=0, step=1)
                                                            )
                                                        ),
                                                        
                                                        # Run simulation button
                                                        HTML("<br>"),
                                                        actionButton("runSimBn", HTML("<b>Simulate With Current Settings</b>"), icon("refresh"), width="100%")
                                                    )
                                                )
                                      )
                        )
               ),
               
               tabPanel("Reference",
                        wellPanel(
                            tabsetPanel(type="tabs",
                                        tabPanel("COVID-19",
                                                 tabsetPanel(type="pills",
                                                             tabPanel("Reproduction Rate", includeHTML("ref/reproductionRate.html")),
                                                             tabPanel("Mortality", includeHTML("ref/mortality.html")),
                                                             tabPanel("Post-Infection Immunity", includeHTML("ref/postInfectionImmunity.html"))
                                                 )
                                        ),
                                        tabPanel("Vaccination Strategies",
                                                 tabsetPanel(type="pills",
                                                             tabPanel("Risk Groups", includeHTML("ref/riskGroups.html")),
                                                             tabPanel("Target Metric", includeHTML("ref/targetMetric.html"))
                                                 )
                                        )
                            )
                        )
               )
    )
))