library(shiny)

shinyUI(fluidPage(
    navbarPage("Ontario COVID-19 Immunization Strategy Simulation",
               tabPanel("Dashboard",
                        # Main body
                        sidebarLayout(position="right",
                                      
                                      # Information side panel
                                      sidebarPanel(width=3, style = "position:fixed; width:inherit;",
                                                   titlePanel("Information"),
                                                   
                                                   tabsetPanel(type="tabs",
                                                               
                                                               tabPanel("About",
                                                                        HTML("<br>This app displays the results of different COVID-19 vaccination strategies in Ontario under a variety of simulated conditions. This allows users to quickly and easily review different vaccination strategies under different initial conditions to better inform decision making in the pandemic response."),
                                                                        actionButton("citationsBn", "View Citations", width="100%")
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
                                                        
                                                        tabsetPanel(type="tabs",
                                                                    
                                                                    # High-level summary
                                                                    tabPanel("Provincial Summary",
                                                                             HTML("<br>"),
                                                                             
                                                                             # Summary buttons
                                                                             fluidRow(
                                                                                 column(width=4, align="center",
                                                                                        actionButton("summaryCases", htmlOutput("summaryCasesLabel"), width="100%",
                                                                                                     style="border-radius:6px; background-color:#4CAF50; box-shadow:0 8px 16px 0 rgba(0,0,0,0.2), 0 6px 20px 0 rgba(0,0,0,0.19)")
                                                                                 ),
                                                                                 column(width=4, align="center",
                                                                                        actionButton("summaryDeaths", htmlOutput("summaryDeathsLabel"), width="100%",
                                                                                                     style="border-radius:6px; background-color:#DC2824; box-shadow:0 8px 16px 0 rgba(0,0,0,0.2), 0 6px 20px 0 rgba(0,0,0,0.19)")
                                                                                 ),
                                                                                 column(width=4, align="center",
                                                                                        actionButton("summaryVax", htmlOutput("summaryVaxLabel"), width="100%",
                                                                                                     style="border-radius:6px; background-color:#428BCA; box-shadow:0 8px 16px 0 rgba(0,0,0,0.2), 0 6px 20px 0 rgba(0,0,0,0.19)")
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
                                                                                         )
                                                                             )
                                                                    ),
                                                                    
                                                                    # Provincial-level summary of cases and deaths with demographic information
                                                                    tabPanel("Epidemiological Outcome",
                                                                             
                                                                    ),
                                                                    
                                                                    # Detailed summary for individual PHUs
                                                                    tabPanel("Public Health Unit Detail",
                                                                             HTML("<br>"),
                                                                             selectInput("phuDetail", "Select Health Unit",
                                                                                         sort(c("Huron Perth Health Unit",
                                                                                                "The District of Algoma Health Unit",
                                                                                                "Brant County Health Unit",
                                                                                                "Durham Regional Health Unit",
                                                                                                "Grey Bruce Health Unit",
                                                                                                "Haldimand-Norfolk Health Unit",
                                                                                                "Haliburton, Kawartha, Pine Ridge District Health Unit",
                                                                                                "Halton Regional Health Unit",
                                                                                                "City of Hamilton Health Unit",
                                                                                                "Hastings and Prince Edward Counties Health Unit",
                                                                                                "Chatham-Kent Health Unit",
                                                                                                "Kingston, Frontenac and Lennox & Addington Health Unit",
                                                                                                "Leeds, Grenville and Lanark District Health Unit",
                                                                                                "Middlesex-London Health Unit",
                                                                                                "Niagara Regional Area Health Unit",
                                                                                                "North Bay Parry Sound District Health Unit",
                                                                                                "Northwestern Health Unit",
                                                                                                "City of Ottawa Health Unit",
                                                                                                "Peel Regional Health Unit",
                                                                                                "Perth District Health Unit",
                                                                                                "Peterborough County-City Health Unit",
                                                                                                "Porcupine Health Unit",
                                                                                                "Renfrew County and District Health Unit",
                                                                                                "The Eastern Ontario Health Unit",
                                                                                                "Simcoe Muskoka District Health Unit",
                                                                                                "Sudbury and District Health Unit",
                                                                                                "Thunder Bay District Health Unit",
                                                                                                "Timiskaming Health Unit",
                                                                                                "Waterloo Health Unit",
                                                                                                "Wellington-Dufferin-Guelph Health Unit",
                                                                                                "Windsor-Essex County Health Unit",
                                                                                                "York Regional Health Unit",
                                                                                                "Southwestern Public Health",
                                                                                                "City of Toronto Health Unit"))),
                                                                             fluidRow(
                                                                                 column(width=4, align="center",
                                                                                        actionButton("phuCases", htmlOutput("phuCasesLabel"), width="100%",
                                                                                                     style="border-radius:6px; background-color:#4CAF50; box-shadow: 0 8px 16px 0 rgba(0,0,0,0.2), 0 6px 20px 0 rgba(0,0,0,0.19);"),
                                                                                 ),
                                                                                 column(width=4, align="center",
                                                                                        actionButton("phuDeaths", htmlOutput("phuDeathsLabel"), width="100%",
                                                                                                     style="border-radius:6px; background-color:#DC2824; box-shadow: 0 8px 16px 0 rgba(0,0,0,0.2), 0 6px 20px 0 rgba(0,0,0,0.19)"),
                                                                                 ),
                                                                                 column(width=4, align="center",
                                                                                        actionButton("phuVax", htmlOutput("phuVaxLabel"), width="100%",
                                                                                                     style="border-radius:6px; background-color:#428BCA; box-shadow: 0 8px 16px 0 rgba(0,0,0,0.2), 0 6px 20px 0 rgba(0,0,0,0.19)"),
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
                                                                                value=2400, min=0),
                                                                   numericInput("paramUK", "UK (B.1.1.7) Variant",
                                                                                value=2400, min=0),
                                                                   numericInput("paramSA", "South Africa (1.351) Variant",
                                                                                value=0, min=0),
                                                                   numericInput("paramBrazil", "Brazil (P.1) Variant",
                                                                                value=0, min=0)
                                                            ),
                                                            
                                                            # Vaccine efficacy settings
                                                            column(width=3,
                                                                   helpText(HTML("<h4>Provincial Vaccine Availability</h4>")),
                                                                   numericInput("paramModerna", "Moderna Doses Per Week",
                                                                                value=15000, step=1000),
                                                                   numericInput("paramPfizer", "Pfizer-BioNTech Doses Per Week",
                                                                                value=15000, step=1000)
                                                            ),
                                                            
                                                            # Vaccination strategy settings
                                                            column(width=3,
                                                                   helpText(HTML("<h4>Distribution Strategy</h4>")),
                                                                   radioButtons("advancedSettings", NULL, inline=TRUE,
                                                                                list("Preset" = 0, "User-Defined" = 1), selected=0),
                                                                   conditionalPanel("input.advancedSettings == 0",
                                                                                    selectInput("paramStrategy", NULL,
                                                                                                list("Highest-Risk First",
                                                                                                     "One Dose Strategy",
                                                                                                     "Age (Descending)",
                                                                                                     "Random",
                                                                                                     "No Vaccination"))
                                                                   ),
                                                                   conditionalPanel("input.advancedSettings == 1",
                                                                                    radioButtons("paramDoses", "Dosage Strategy", inline=TRUE,
                                                                                                 list("Two Doses" = 2, "One Dose" = 1), selected=2),
                                                                                    conditionalPanel(condition="input.paramDoses == 2",
                                                                                                     numericInput("paramDelay", "Additional Delay Between Doses (Weeks)",
                                                                                                                  value=0, min=0, step=1)
                                                                                    ),
                                                                                    selectInput("paramDistribution", "Health Unit Allocation Criterion",
                                                                                                list("Equal Distribution",
                                                                                                     "New Cases (Absolute)",
                                                                                                     "New Cases (Per Capita)",
                                                                                                     "Population",
                                                                                                     "Population Density",
                                                                                                     "Transmission Rate")),
                                                                                    HTML("<b>Vaccine Priority</b>"),
                                                                                    checkboxInput("manualPriority", "Use manual vaccine priority", value=FALSE),
                                                                                    conditionalPanel("input.manualPriority",
                                                                                                     actionButton("manualPriorityBn", "Edit Vaccine Priority", width="100%"))
                                                                   )
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
               )
    )
    
))