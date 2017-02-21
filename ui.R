shinyUI(fluidPage(
  titlePanel("Exploration of simulation results for False positive dynamic occupancy model"),
  sidebarLayout(
    sidebarPanel(
      p("Select the parameter which you would like to focus on."),
      radioButtons("focal", label = h3("Focal Parameter"), 
                   choices = list("occupancy" = "occ.bias", "pT" = "pT.bias", "pF" = "pF.bias", "colonisation" ="col.bias", "survival"="surv.bias", "initial occupancy"="psi1.bias"), 
                   selected = "occ.bias"),
      br(),
      br(),
      
      p("select parameters to visualise"),
      selectInput("par1", label = h3("Colour variable"), 
                  choices = list( "pT" = "pT.true", "pF" = "pF.true", "colonisation" ="mean.col", "survival"="mean.surv", "initial occupancy"="psi1.true","proportion confirmed" = "prop.conf.true"), 
                  selected = "pF.true"),
 
      br(),
      br(),
      selectInput("par2", label = h3("Panel variable"), 
                  choices = list( "pT" = "pT.true", "pF" = "pF.true", "colonisation" ="mean.col", "survival"="mean.surv", "initial occupancy"="psi1.true","proportion confirmed" = "prop.conf.true"), 
                  selected = "pT.true"),
      br(),
      
      radioButtons("summarystat", label = h4("Summary statistic"),
                   choices = list("Median" = "median","Mean" = "mean"))
    ),
    
    
    mainPanel(
      
      tabsetPanel(
        tabPanel("Bias in parameter", plotOutput("plot"),
                 br(),
                 br(),
                 h4("Summary statistics"),
                 tableOutput("plotsum"),
                h4("Explanatory Notes"),
                p("These figures display the outputs of the simulations described in ", em("Cruickshank et al 2016"), " (in prep). The false-positive dynamic occupancy model described in this paper was applied to datasets generated under a range of parameter values."),
                p("This app allows you to explore the resulting bias (first tab), and precision (width of 95% credible intervals; second tab) for the key parameters."),
                p("First select the parameter of interest to explore (e.g. Occupancy Bias), and select a tab to choose between visualising absolute bias or CI widths. Also select a summary statistic (mean or median). By selecting one or two parameters from the drop down menus, the figures and table will autoupdate to give the average measures of interest averaged across all values for the parameters not selected in the menus"),
                br(),
                p(em("Note")," Currently these visualisations only show 1/4 of the simulations I ran- specifically simulations run on datasets which contain no NA's, and in which the data-generating colonisation and survival rates did not vary between years")),
        tabPanel("95% Credible Interval width", plotOutput("CIwidth"),
                 br(),
                 br(),
                 h4("Summary statistics"),
                 tableOutput("CIsum"))
 
    
      )
    )
  )
))




