###############################
#                             #
# Author: Kyle Dixon          #
# Date Created: August 5 2019 #
# Title: EMD Demo             #
# Last Edited: August 5 2019  #
#                             #
##############################

library(quantmod)
library(hht)
library(forecast)
library(lubridate)

rm(list = ls())

#Pull S&P 500 data for the last 2 years
prices <- as.ts(quantmod::getSymbols("^GSPC",
                                     src = "yahoo",
                                     from = Sys.Date() - lubridate::years(2),
                                     to = Sys.Date(),
                                     auto.assign = F))

#Decompose the signal into it's imfs and plot
emd <- hht::Sig2IMF(prices[, "GSPC.Close"], seq(1:length(prices[, "GSPC.Close"])))
hht::PlotIMFs(emd)

#Loop through the kept imfs and rebuild the transformed signal
imf <- list()
for (i in 2:ncol(emd$imf)) {
  tmp <- as.ts(emd[["imf"]][, i], start = 1, end = length(prices[, "GSPC.Close"]))
  imf <- append(imf, list(tmp))
}
residue <- as.ts(emd[["residue"]], start = 1, end = length(prices[, "GSPC.Close"]))
transformed <- Reduce("+", imf) + residue

#Plot transformed and original signal
plot(transformed, ylab = "USD", xlim = c(480, 510), ylim = c(2800, 3050))
lines(prices[, "GSPC.Close"], col = "red")

#Plot transformed and original signal with forecast
plot(forecast(na.omit(ar(transformed, order.max = 2)), h = 2), xlim = c(480, 510), ylim = c(2800, 3050), main = "", xlab = "Time", ylab = "USD")
lines(prices[, "GSPC.Close"], col = "red")
################################################################################################################################################