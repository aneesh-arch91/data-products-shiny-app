library(UsingR)
library(ggplot2)
library(plotly)
data(ChickWeight)

shinyServer(
	function(input, output) {
		output$myPlot <- renderPlotly({
			chick_avgdiet <- aggregate(list(Weight=ChickWeight$weight),
				by=c(list(Diet=ChickWeight$Diet), list(Time=ChickWeight$Time)),
				FUN=mean)

			if (input$diet == "All")
			{
				plt1 <- ggplot(
					data=chick_avgdiet[((chick_avgdiet$Time >= input$time[1]) & (chick_avgdiet$Time <= input$time[2])),],
					aes(x=Time,y=Weight, fill=as.factor(Diet))
				) +
				geom_point() +
				geom_smooth(method='lm', formula= y~x) +
				ggtitle("ChickWeight data: Time vs Weight") +
				ylab("Weight") +
				xlab("Time") +
				theme(
					  plot.title = element_text(hjust = 0.5)
				) +
				labs(fill = "Diet Type")
				plt2 <- ggplotly(plt1)
				return(plt2)
			}
			else
			{
				plt1 <- ggplot(
					data=chick_avgdiet[((chick_avgdiet$Diet == input$diet) & (chick_avgdiet$Time >= input$time[1] & chick_avgdiet$Time <= input$time[2])),],
					aes(x=Time,y=Weight, fill=as.factor(Diet))
				) +
				geom_point() +
				geom_smooth(method='lm', formula= y~x) +
				ggtitle("ChickWeight data: Time vs Weight") +
				ylab("Weight") +
				xlab("Time") +
				theme(
					plot.title = element_text(hjust = 0.5),
					legend.position="none"
				)
				plt2 <- ggplotly(plt1)
				return(plt2)
			}
		})
		output$lmodel <- renderText({
			chick_avgdiet <- aggregate(list(Weight=ChickWeight$weight),
				by=c(list(Diet=ChickWeight$Diet), list(Time=ChickWeight$Time)),
				FUN=mean)
			time_condition <- (chick_avgdiet$Time >= input$time[1]) & (chick_avgdiet$Time <= input$time[2])
			fit1 <- lm(Weight ~ Time, data=chick_avgdiet[(chick_avgdiet$Diet == 1 & time_condition),])
			fit2 <- lm(Weight ~ Time, data=chick_avgdiet[(chick_avgdiet$Diet == 2 & time_condition),])
			fit3 <- lm(Weight ~ Time, data=chick_avgdiet[(chick_avgdiet$Diet == 3 & time_condition),])
			fit4 <- lm(Weight ~ Time, data=chick_avgdiet[(chick_avgdiet$Diet == 4 & time_condition),])

			models <- list(fit1, fit2, fit3, fit4)

			intercepts <- sapply(models, function(model) summary(model)$coefficients[1,1])
			slopes <- sapply(models, function(model) summary(model)$coefficients[2,1])

			printCoeff <- function(type, intercept, slope) {
				paste0(type, ", ", "Intercept: ", round(intercept, 4), ", Slope: ", round(slope, 4))
			}

			diet <- as.numeric(input$diet)

			if (input$diet == "All")
			{
				paste(printCoeff("Diet 1", intercepts[1], slopes[1]),
					  printCoeff("Diet 2", intercepts[2], slopes[2]),
					  printCoeff("Diet 3", intercepts[3], slopes[3]),
					  printCoeff("Diet 4", intercepts[4], slopes[4]),
					  sep= "<br>"
				)
			}
			else
			{
				printCoeff(paste0("Diet ", diet), intercepts[diet], slopes[diet])
			}
		})
	}
)
