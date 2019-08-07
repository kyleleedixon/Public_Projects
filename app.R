################################
#                              #
# Author: Kyle Dixon           #
# Date Created: August 5 2019  #
# Title: Hilbert-Huang Example #
# Last Edited: August 5 2019   #
#                              #
################################

library(shiny)
library(quantmod)
library(hht)
library(forecast)
library(ggplot2)
library(ggfortify)
library(lubridate)

rm(list = ls())

#Pull S&P 500 data for the last 2 years
prices <- quantmod::getSymbols("^GSPC",
                               src = "yahoo",
                               from = Sys.Date() - years(2),
                               to = Sys.Date(),
                               auto.assign = F)

#Get date index before converting to time series
dates <- as.Date(index(prices), "%Y-%m-%D")
#Convert to time series
prices <- as.ts(prices)
#Decompose the signal into it's imfs
emd <- hht::Sig2IMF(prices[, "GSPC.Close"], seq(1:length(prices[, "GSPC.Close"])))

#Create the UI page
ui <- fluidPage(
    br(),
    sidebarLayout(
        sidebarPanel(
            #Create radio buttons to select how many high frequency imfs to remove
            radioButtons("imfs",
                         "Number of Intrinsic Mode Functions to Remove:",
                         c("1" = 2, "2" = 3, "3" = 4)),
            #Create slider to select how many historical days to look back
            sliderInput("days",
                        "Number of Historical Days Back:",
                        min = 1,
                        max = 50,
                        value = 25)
            ),
        mainPanel(
           plotOutput("plot")
        )
    )
)

server <- function(input, output) {
    output$plot <- renderPlot({
        
        #Loop through the kept imfs and rebuild the transformed signal
        imf <- list()
        for (i in input$imfs:ncol(emd$imf)) {
            tmp <- as.ts(emd[["imf"]][, i], start = 1, end = length(prices[, "GSPC.Close"]))
            imf <- append(imf, list(tmp))
        }
        residue <- as.ts(emd[["residue"]], start = 1, end = length(prices[, "GSPC.Close"]))
        transformed <- Reduce("+", imf) + residue
        
        #Check correlation plots to determine AR or MA forecast
        #plot(diff(log(transformed)))
        #forecast::Acf(diff(log(transformed)))
        #forecast::Pacf(diff(log(transformed)))
        
        #Fit the appropriate AR or MA model based on the correlation plots
        fit <- ar(transformed, order.max = 2)
        #Add the original data back into the fit so the forecast function can find it
        fit$x <- transformed
        
        #Forecast the model based on the correlation plots
        model <- forecast(na.omit(fit), h = 2)
        
        #Plot the results
        autoplot(model, ts.colour = "black", predict.colour = "blue", conf.int.fill = "deepskyblue", predict.linetype = "dashed") + 
            geom_line(aes(x = 1:length(model$x), y = as.numeric(prices[, "GSPC.Close"]), col = "Signal")) +
            geom_line(aes(x = 1:length(model$x), y = as.numeric(transformed), col = "Transformed")) + 
            geom_line(aes(x = 1:length(model$x), y = 1, col = "Predicted"), linetype = "dashed") + 
            xlim(length(model$x) - input$days, length(model$x) + 3) + 
            xlab(paste0("Trading Days since ", Sys.Date() - years(2))) + 
            ylim(min(prices[(length(model$x) - input$days):length(model$x), "GSPC.Close"]) * .975, 
                 max(prices[(length(model$x) - input$days):length(model$x), "GSPC.Close"]) * 1.025) + 
            ylab("USD") +
            scale_colour_manual(name = "", 
                                values = c("Signal" = "red", "Transformed" = "black", "Predicted" = "blue"), 
                                guide = guide_legend(override.aes = list(linetype = c(2, 1, 1)))) +
            ggtitle("S&P 500 Hilbert-Huang Transform & 2-Day Forecast at 95% Confidence") +
            theme(
                plot.title = element_text(face = "bold"),
                axis.title.x = element_text(face = "bold"),
                axis.title.y = element_text(face = "bold"))
    })
}

shinyApp(ui = ui, server = server)
