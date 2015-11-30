library(ggplot2)

hw <- read.csv("heightweight.csv")

# Returns a logical vector of which values in `x` are within the min and max
# values of `range`.
in_range <- function(x, range) {
  x >= min(range) & x <= max(range)
}

shinyServer(function(input, output) {

  limit_data_range <- function() {
    # ------------------------------------------------------------------
    # Because we're using reactiveUI for x_range and y_range, they start
    # out as null, then get resolved after the client and server talk a bit.
    # If they are not yet set, there will be some errors in this function, so
    # do nothing for now (this function will be run again).
    if (is.null(input$x_range) || is.null(input$y_range)) {
      return(NULL)
    }

    # ------------------------------------------------------------------
    # Limit range of data
    # Take a subset of the data, respecting the limited range
    hw_sub <- hw[in_range(hw[[input$x_var]], input$x_range) &
                 in_range(hw[[input$y_var]], input$y_range), ]

    hw_sub
  }



  output$main_plot <- renderPlot({

    # Take a subset of the data, respecting the limited range
    hw_sub <- limit_data_range()

    if (is.null(hw_sub))
      return()

    # Get the x and y values from the non-range-limited data, for convenience
    xdat <- hw[[input$x_var]]
    ydat <- hw[[input$y_var]]


    # ------------------------------------------------------------------
    # Make the base plot

    # If any models are drawn, make the points less prominent
    if (input$mod_linear || input$mod_quadratic || input$mod_loess)
      point_alpha <- 0.5
    else
      point_alpha <- 1


    # Separate by sex, if requested
    if (input$sex) {
      aes_mapping <- aes_string(x = input$x_var, y = input$y_var,
                                colour = "sex", shape = "sex")

      # Use different point geom specification, depending on if we're separating
      # by sex.
      points <- geom_point(solid = FALSE, alpha = point_alpha)

    } else {
      aes_mapping <- aes_string(x = input$x_var, y = input$y_var)

      # Use different point geom specification, depending on if we're separating
      # by sex.
      points <- geom_point(shape = 21, alpha = point_alpha)
    }



    # Base plot
    p <- ggplot(hw_sub, mapping = aes_mapping) +
      points +
      theme_bw() +
      scale_colour_hue(l = 40) +
      scale_shape(solid = FALSE) +
      # # Show the original range
      scale_x_continuous(limits = range(xdat)) +
      scale_y_continuous(limits = range(ydat))

    # ------------------------------------------------------------------
    # If the range has been limited, draw lines displaying the limits

    if (max(input$x_range) != max(xdat)) {
      p <- p + geom_vline(xintercept = max(input$x_range), linetype = "dashed",
                          alpha = 0.3)
    }
    if (min(input$x_range) != min(xdat)) {
      p <- p + geom_vline(xintercept = min(input$x_range), linetype = "dashed",
                          alpha = 0.3)
    }

    if (max(input$y_range) != max(ydat)) {
      p <- p + geom_hline(yintercept = max(input$y_range), linetype = "dashed",
                          alpha = 0.3)
    }
    if (min(input$y_range) != min(ydat)) {
      p <- p + geom_hline(yintercept = min(input$y_range), linetype = "dashed",
                          alpha = 0.3)
    }


    # ------------------------------------------------------------------
    # Add model lines

    if (input$mod_linear) {
      p <- p + geom_smooth(method = lm, se = FALSE, size = 0.75,
                           linetype = "dotdash")
    }
    if (input$mod_quadratic) {
      p <- p + geom_smooth(method = lm, se = FALSE, formula = y ~ x + I(x^2),
                           size = .75, linetype = "dashed")
    }
    if (input$mod_loess) {
      p <- p + geom_smooth(method = loess, se = FALSE, linetype = "solid",
                           span = input$mod_loess_span)
    }

    print(p)
  })


  # ------------------------------------------------------------------
  # Create renderUI sliders for x and y range, because their limits
  # depend on the selected x and y variables.

  output$x_range_slider <- renderUI({
    xmin <- floor(min(hw[[input$x_var]]))
    xmax <- ceiling(max(hw[[input$x_var]]))

    sliderInput(inputId = "x_range",
                label = paste("Limit range"),
                min = xmin, max = xmax, value = c(xmin, xmax))
  })

  output$y_range_slider <- renderUI({
    ymin <- floor(min(hw[[input$y_var]]))
    ymax <- ceiling(max(hw[[input$y_var]]))

    sliderInput(inputId = "y_range",
                label = paste("Limit range"),
                min = ymin, max = ymax, value = c(ymin, ymax))
  })


  # ------------------------------------------------------------------
  # Functions for creating models and printing summaries

  make_model <- function(model_type, formula, ...) {
    # Get the subset of the data limited by the specified range
    hw_sub <- limit_data_range()
    if (is.null(hw_sub))
      return()

    # In order to get the output to print the formula in a nice way, we'll
    # use do.call here with some quoting.
    do.call(model_type, args = list(formula = formula, data = quote(hw_sub), ...))
  }

  output$mod_linear_text <- renderPrint({
    formula <- paste(input$y_var, "~", input$x_var)

    # Use sex as a predictor variable
    if (input$sex) {
      formula <- paste(formula, " * sex", sep = "")
    }

    summary(make_model("lm", formula))
  })

  output$mod_quadratic_text <- renderPrint({
    formula <- paste(input$y_var, " ~ ", "I(", input$x_var, "^2) + ",
                     input$x_var, sep = "")

    # Use sex as a predictor variable
    if (input$sex) {
      formula <- paste(formula, " * sex", sep = "")
    }

    summary(make_model("lm", formula))
  })

  output$mod_loess_text <- renderPrint({
    formula <- paste(input$y_var, "~", input$x_var)
    summary(make_model("loess", formula, span = input$mod_loess_span))
  })


})
