library(shiny)

shinyUI(pageWithSidebar(

  headerPanel("Height and weight of schoolchildren"),

  sidebarPanel(

    wellPanel(
      selectInput(inputId = "x_var",
                  label = "X variable",
                  choices = c("Age" = "ageYear",
                              "Height (inches)" = "heightIn",
                              "Weight (pounds)" = "weightLb"),
                  selected = "Age"
                 ),

      uiOutput("x_range_slider")
    ),


    wellPanel(
      selectInput(inputId = "y_var",
                  label = "Y variable",
                  choices = c("Age" = "ageYear",
                              "Height (inches)" = "heightIn",
                              "Weight (pounds)" = "weightLb"),
                  selected = "Height (inches)"
                 ),

      uiOutput("y_range_slider")
    ),

    checkboxInput(inputId = "sex",
                  label = "Separate male/female",
                  value = FALSE),

    wellPanel(
      p(strong("Model predictions")),
      checkboxInput(inputId = "mod_linear",    label = "Linear (dot-dash)"),
      checkboxInput(inputId = "mod_quadratic", label = "Quadratic (dashed)"),
      checkboxInput(inputId = "mod_loess",     label = "Locally weighted LOESS (solid)"),
      conditionalPanel(
        condition = "input.mod_loess == true",
        sliderInput(inputId = "mod_loess_span", label = "Smoothing (alpha)",
          min = 0.15, max = 1, step = 0.05, value = 0.75)
      )
    )
  ),

  mainPanel(
    plotOutput(outputId = "main_plot"),

    conditionalPanel("input.mod_linear == true",
      p(strong("Linear model")),
      verbatimTextOutput(outputId = "mod_linear_text")
    ),

    conditionalPanel("input.mod_quadratic == true",
      p(strong("Quadratic model")),
      verbatimTextOutput(outputId = "mod_quadratic_text")
    ),

    conditionalPanel("input.mod_loess == true",
      p(strong("LOESS model")),
      conditionalPanel("input.sex == true",
        p("Note: categorical variable ", code("sex"),
          " cannot be used as a predictor in a LOESS model.",
          " (The plot above uses two separate models.)")
      ),
      verbatimTextOutput(outputId = "mod_loess_text")
    )

  )
))
