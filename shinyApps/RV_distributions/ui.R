library(shiny)
library(shinythemes)
shinyUI(fluidPage(theme=shinytheme("united"),
	headerPanel(
		"Distributions of Random Variables"
	),
	fluidRow(
		column(4,
			wellPanel( radioButtons("disttype","Distribution type:",list("Discrete","Continuous"),selected="Continuous") ),
			wellPanel(	uiOutput("distName") ),
			wellPanel(
				numericInput("n","Sample size:",10000),
				uiOutput("dist1"),
				uiOutput("dist2"),
				uiOutput("dist3")
			),
			wellPanel(
			    uiOutput("NBIN"),
			    uiOutput("plotPDF"),
				#uiOutput("sampDens"),
				#uiOutput("BW"),
				fluidRow(
					column(6, downloadButton("dlCurPlot", "Download Graphic", class="btn-block btn-primary")),
					column(6, downloadButton("dldat", "Download Sample", class="btn-block btn-warning"))
				)
			)
		),
		column(8, plotOutput("plot", width="100%", height="auto"), plotOutput("plotts", width="100%", height="auto"))
	)
))
