function(){
  tabPanel("Help", value="help",
           tabsetPanel(type="hidden", id='helpTabs',
                       tabPanelBody(NULL, value="start",
                                    HTML("<br>Click the help buttons on the dashboard to display additional information about the simulation elements.")
                       ),
                       tabPanelBody(NULL, value="vax",
                                    helpText(HTML("Provincial Vaccine Availability")),
                                    HTML("<h4>Vaccine Doses Per Week</h4>
                                          This parameter controls the mean number of vaccine doses administered per week. The actual number of doses administered per week has a Poisson distribution with rate equal to this parameter.")
                       )
           )
  )
}