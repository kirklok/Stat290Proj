library(shiny)

# Define UI for random distribution application 
shinyUI(fluidPage(
    
  # Application title
  titlePanel("Retention analysis"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  sidebarLayout(position = "right",
    sidebarPanel(
      tabsetPanel(type = "tabs", 
        tabPanel("Upload file", 
                br(),
                fileInput('file', 'Choose CSV File',
                accept=c('text/csv', 
                 'text/comma-separated-values,text/plain', 
                 '.csv')),
                checkboxInput('header', 'Header', TRUE),
                radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
                radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"'),
                numericInput("colUID", "Which column does contain user IDs?", 2,
                        min = 1, max = 100),
                numericInput("colTimestamp", "Which column does contain time stamp?", 5,
                        min = 1, max = 100),
                numericInput("colEvents", "Which column does contain events?", 3,
                        min = 1, max = 100)
                ),
        tabPanel("Parameters",      
                br(),
                selectInput("triggerEvent", "Show me people who did:",
                  c("Sign up" = "welcome",
                    "Sign in" = "user_signin")),
                selectInput("followupEvent", "Then came back and did:",
                  c("Sign up" = "welcome",
                    "Sign in" = "user_signin")),
                selectInput("groupBy", "Group cohorts by:",
                  c("Days" = "days",
                    "Weeks" = "weeks",
                    "Months" = "months")),
                dateInput("startDate", "Start date:", value = ""),
                dateInput("endDate", "End date:", value = "")
                )

      )


    ),
    
    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs", 
        tabPanel("Input file", 
          br(),
          tableOutput('contents')),
        tabPanel("Retention Matrix", 
          br(),
          tableOutput('retentionMatrix')),
        tabPanel("Plot1", plotOutput("plot")), 
        tabPanel("Plot2", verbatimTextOutput("summary"))
      )
    )
  )
))