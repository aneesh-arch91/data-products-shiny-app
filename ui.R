library(shiny)
library(plotly)

diet <- c(1, 2, 3, 4, "All")
navbarPage(
	theme = bslib::bs_theme(bootswatch = "cerulean"),
	"Developing Data Products: Shiny Application",
	tabPanel("Main",
		sidebarLayout(
			sidebarPanel(
				selectInput("diet", "Diet", diet, selected="All"),
				tags$br(),  # Adds a line break
				sliderInput('time', 'Time Range', value = c(0, 22), min = 0, max = 22, step = 2)
			),
			mainPanel(
				plotlyOutput('myPlot'),
				htmlOutput('lmodel')
			)
		)
	),
	tabPanel("Docs",
		 mainPanel(
			h2("About"),
			p("This is an interactive tool for analysing the ChickWeight data dividing it into diet types and the time range."),
			h2("Instructions"),
			p("Select the diet type from the select menu. Modify the time range by moving both the thumbs on the slider."),
			p("Look at the linear model generated and the coefficients below.")
		 )
	)
)
