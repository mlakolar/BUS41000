library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("Confidence intervals"),
  sidebarPanel(
    tags$head(tags$script(src="http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML-full", type = 'text/javascript'),
              tags$script( "MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}});", type='text/x-mathjax-config')),
    p("This simulation demonstrates how confidence intervals are derived using a combination of
the standard error, the Central Limit Theorem, and the normal and standard normal distributions."),
    p("First, the central limit theorem states that the means $\\overline{X}$ obtained by repeatedly sampling the same parent distribution using a sufficiently large number of independent observations will be normally distributed.
      "),
    p("Second, the distribution of $\\overline{X}$ can be converted to a standard normal distribution by computing the $Z$-score of each sample mean. This is approximated by subtracting the true population mean $\\mu$ from each sample mean $\\overline{X}$ and dividing by the standard error $SE=\\frac{s}{\\sqrt{n}}$, which is our estimate of the unknown population standard deviation $\\sigma$. Doing so converts the sampling distribution $\\overline{X}$ into a standard normal distribution $Z$ with a mean of 0 and a standard deviation of 1. This distribution can then be used to determine the $(1-\\alpha)\\cdot 100\\%$ confidence interval for the unknown population mean $\\mu$."),
    
    p("You can verify this for yourself by using the controls below to compute arbitrary confidence intervals for samples drawn from any parent distribution."),
    p("The top panel shows the distribution of observations within a random sample. The middle panel shows the standard normal distribution, with the blue region representing the probability that the $Z$-score of the sample mean will be within $Z_{\\alpha/2}$ standard deviations $\\sigma$ of the true population mean $\\mu$. The black dashes at the bottom represent the $Z$-scores of the samples means. The bottom panel represents the confidence intervals for the first 100 samples. Confidence intervals that contain the true population mean $\\mu$ are represented in blue whereas those that do not are represented in red."),
    h4("Simulation parameters:"),
    radioButtons("dist", "Parent distribution:",
                 list("Exponential" = "rexp",
                      "Normal" = "rnorm",
                      "Log-normal" = "rlnorm",
                      "Uniform" = "runif",
                      "Poisson" = "rpois",
                      "Bernoulli" = "rbinom")),
    br(),
    
    sliderInput("nobs", 
                "Number of observations in each sample from parent distribution:", 
                value = 10,
                min = 2, 
                max = 100),
    br(),
    
    sliderInput("nsamples", 
                "Number of samples taken from parent distribution:", 
                value = 100,
                min = 1, 
                max = 1000),
    br(),
    sliderInput("alpha", 
                "Type I error ((\\alpha)):", 
                value = 0.05,
                min = 0.005, 
                max = 1)
    ),
  
  mainPanel(
    plotOutput("plot", height="1200px")
  )
))