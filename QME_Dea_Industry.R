library("readxl")

Data <- read.csv2("C:\\Users\\marco\\Documents\\UniversitÓ\\QME\\Dataset\\df_avg_industry.csv", sep = ',')
View(Data)
attach(Data)

### Variable Definition
##  Production System with: 3 inputs, 1 output

X1 <- as.numeric(Data$RD.2019.million)                     
X2 <- as.numeric(Data$Employees)                           
X3 <- as.numeric(Data$Capex.million)                       

X <- matrix(c(X1, X2, X3), ncol=3)               
View(X)
Y <- as.numeric(Data$Net.sales.million)             
View(data.frame(X, Y))

### Efficiency Analysis

## Installing and Loading Packages

install.packages("Benchmarking")
library(Benchmarking)
library(FEAR) # Remember to download it from https://pww.people.clemson.edu/Software/FEAR/fear-download.php

## Testing Returns to Scale and Convexity Assumptions

x <- t(X)      # Transposing the X Matrix to work on FEAR
y <- t(Y)      # Transposing the Y vector to work on FEAR

test.rts(x, y, ORIENTATION = 1, METRIC = 1, NSPLIT = 1, NREP = 1000)

test.convexity(x, y, ORIENTATION = 1, METRIC = 1, NSPLIT = 1, NREP = 1000)

## Seminal Models

detach("package:FEAR", unload = TRUE)

# FDH is our choice in this case

e2 <- dea(X, Y, RTS = "fdh", ORIENTATION = "in", SLACK = TRUE, DUAL = TRUE)
summary(e2)

par(mfrow = c(1, 3))  # Confronting graphically the different approaches
dea.plot(X, Y, RTS = "crs", main = "Constant Returns to Scale", col = 'red') # Efficient Frontier Plot under CRS Technology
dea.plot(X, Y, RTS = "vrs", main = "Variable Returns to Scale", col = 'goldenrod3') # Efficient Frontier Plot under VRS Technology
dea.plot(X, Y, RTS = "fdh", main = "Free Disposal Hull", col = 'green') # Efficient Frontier Plot under FDH Assumption

results2 <- data.frame(Data$Industry, e2$eff, peers(e2), e2$ux, e2$vy, e2$sx, X1, X2, X3, Y)
View(results2)

## Bootstrap and Confidence Intervals

library(FEAR)
BootResults <- boot.sw98(x, y, RTS = 3, ORIENTATION = 1, NREP = 1000)
print(BootResults)

ee <- 1/BootResults$dhat # Efficiency estimates for observations in (x, y) relative to the supported technology
View(ee)

ee1 <- 1/BootResults$dhat.bc # Bias corrected efficiency estimates for observations in (x, y) relative to the supported technology

View(BootResults$conf.int)
CI <- 1/BootResults$conf.int
View(CI)

install.packages("plotrix")
library(plotrix)

par(mfrow = c(1, 2))  # Visualization with 1 row, 2 columns
plotCI(BootResults$dhat.bc[1:36], ui = BootResults$conf.int[1:36, 2], li = BootResults$conf.int[1:36], xlab = c("Industry"), ylab = c("Farrell Eff. Intervals"))
plotCI(ee1[1:36], ui = CI[1:36], li = CI[1:36, -1], xlab = c("Industry"), ylab = c("Shephard Eff. Intervals"))


