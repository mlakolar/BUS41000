
shinyUI(pageWithSidebar(
    
    # Application title
    headerPanel("Page view statistics for Wikimedia projects"),
    
    # Sidebar with controls to select the subjects and time span
    sidebarPanel(
        helpText(
            p("The graph represents the page view statistics for Wikimedia projects."),
            p("The data is available from December 2007 to the present day. Adjust the slider to amend the time covered."),
            p("Increasing the number of subjects and extending the time period will impact processing time")),
        wellPanel(
            p(strong("Enter page names seperated by commas")),
            textInput(inputId = "subjects", label = " ", value = "Hillary Clinton, Donald Trump"),
            p("For ambiguous names use wiki nomenclature     e.g. Andrew Clark (priest)"), 
            p(strong("Date range (months back from present);")),
            sliderInput(inputId = "obs",
                        label=" ",
                        min = 0, max = 60, step = 1, value = c(0,12))
            
        ), 
        div(class="span6", submitButton("Get Graph")),
        div(class="span6", checkboxInput(inputId = "log", label = "log10 scale", value = FALSE)),
        helpText("Use log scale if compared searches are significantly different"),
        downloadButton('downloadData', 'Download Output as csv')
    ),
    
    # Show the caption a line graph of the dauly rate and summary of results 
    mainPanel(
        h3(textOutput("caption")),
        plotOutput("plot"), 
        tableOutput("view")
    )
))