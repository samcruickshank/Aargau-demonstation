shinyUI(fluidPage(
  titlePanel("Exploration of simulation results for false-positive dynamic occupancy model"),
  sidebarLayout(
    sidebarPanel(
      p("Select the parameter which you would like to focus on."),
      radioButtons("focal", label = h3("Focal Parameter"), 
                   choices = list("occupancy" = "occ.bias", "pT" = "pT.bias", "pF" = "pF.bias", "colonisation" ="col.bias", "survival"="surv.bias", "initial occupancy"="psi1.bias"), 
                   selected = "occ.bias"),
      br(),

      
      p("select parameters to visualise"),
      selectInput("par1", label = h3("Colour variable"), 
                  choices = list( "pT" = "pT.true", "pF" = "pF.true", "colonisation" ="mean.col", "survival"="mean.surv", "initial occupancy"="psi1.true","proportion confirmed" = "prop.conf.true"), 
                  selected = "pF.true"),
 
      br(),

      selectInput("par2", label = h3("Panel variable"), 
                  choices = list( "pT" = "pT.true", "pF" = "pF.true", "colonisation" ="mean.col", "survival"="mean.surv", "initial occupancy"="psi1.true","proportion confirmed" = "prop.conf.true"), 
                  selected = "pT.true"),
      br(),
      
      radioButtons("summarystat", label = h4("Summary statistic"),
                   choices = list("Median" = "median","Mean" = "mean")),
      
      br(),

      
      

      
      radioButtons("simset", label = h4("Characteristics of underlying simulated data"),
                   choices = list("No NA's, constant colonisation and survival" = 1, "NA's, constant colonisation and survival" = 2,"No NA's, annual variation in colonisation and survival" =3, "NA's,  annual variation in  colonisation and survival" = 4),selected = 1)
      
      ####NA tab
      ####complex thing tab
      
      
      
    ),
    
    
    mainPanel(
      
      tabsetPanel(
        tabPanel("Bias in parameter", plotOutput("plot"),
                 br(),
                 br(),
                 h4("Summary statistics"),
                 tableOutput("plotsum"),
                h4("Explanatory Notes"),
                p("These figures display the outputs of the simulations described in ", em("Cruickshank et al 2019, Conservation Science and Practice"), ". The false-positive dynamic occupancy model described in this paper was applied to datasets generated under a range of parameter values."),
                p("This app allows you to explore the resulting bias (first tab), and precision (width of 95% credible intervals; second tab) for the key parameters."),
                p("First select the parameter of interest to explore (e.g. Occupancy Bias), and select a tab to choose between visualising absolute bias or CI widths. Also select a summary statistic (mean or median). By selecting one or two parameters from the drop down menus, the figures and table will autoupdate to give the average measures of interest averaged across all values for the parameters not selected in the menus"),
                br(),
                p("The final selection parameter allows you to view the results from each of the 4 simulation case studies described in the text. The underlying simulated data either full (no NA's) or has had 10 of the 15 years worth of simulated data converted to NA's before the model is run. Furthermore, the data was either generated with colonisation and survival rates remaining constant through time, or having slight annual variations around the mean value"),
                br(),
                br(),
                p("This app was created using the packages", em("shiny"), "(v1.0.0),", em("ggplot2")," (v2.2.1), and ", em("dplyr"), "(v0.5.0) ")
                
                
                ),
        tabPanel("95% Credible Interval width", plotOutput("CIwidth"),
                 br(),
                 br(),
                 h4("Summary statistics"),
                 tableOutput("CIsum")),
        
        
        tabPanel("Bias in occupancy trend", 
                 br(),
                 br(),
                 plotOutput("timebias"),
                 br(),
                 p("This histograph shows the bias in the trend of occupancy (slope of a linear model of error~year) for all simulations in the selected simulation scenario (grey bars), and the subset of simulations for which the trend bias is significant (p<0.05 in linear model;red)."),
                 br(),
                 tableOutput("timebiastable"),
                 br(),
                 tableOutput("bigbias"),
                 br(),
                 p("Table shows the number of simulations for each combination of false-positive and detection rates, for which substantial bias in occupancy trends were seen (bias in occupancy trend exceeded 0.01"),
                 br())

                
 
    
      )
    )
  )
))




