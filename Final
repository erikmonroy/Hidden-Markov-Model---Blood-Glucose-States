

######## data creation #########
#function that creates current bg
set.seed(1849)
prevbg <- 0
createbg <- function(prevbg){
  bg = round(rnorm(1, mean = prevbg, sd=2))
  return(bg)
}
#data
bgdata <- seq(0,0,length.out = 250)
bgdata[1] <- sample(80:120, 1)
bgdata[2] <- createbg(bgdata[1])
for (bg in 3:length(bgdata)){
  bgdata[bg] <- createbg(bgdata[bg-1])
}
#plot
bgdata
minutes <- seq(from = 0, to = 5*(length(bgdata)-1), by = 5)
plot(bgdata, ylab = "Blood Glucose (mm/dL)", main = "Blood Glucose Time-Series")
#creating dataframe for later column additions
df <- data.frame(bgdata, minutes)

######## hidden markov model  #########
library(dplyr)

#transmission probabilities → parameters that are altered
stableProb <-c(0.80,0.20)
risingProb <-c(0.20,0.80)
transProb <- matrix(c(stableProb, risingProb), 2)

#adding bg difference and emission probabilities to df
df <- df %>%
  mutate(bgdiff = bgdata - lag(bgdata)) %>%
  mutate(stable.likelihood = dnorm(bgdiff, mean=0, sd=2)) %>%
  mutate(rising.likelihood = dnorm(bgdiff, mean=2, sd=2))

#these are our hidden states
states <-c("Stable","Rising")
df$state <- NA
#arbitrarily set first state
df$state[1] <- "Rising"

#code block for markov processes
for(row in 2:nrow(df)){
  if (df[row-1,6]=="Stable"){
    stable.l <- df[row,]$stable.likelihood * transProb[1,1]
    rising.l <- df[row,]$rising.likelihood * transProb[1,2]
  }
  else {
    stable.l <- df[row,]$stable.likelihood * transProb[2,1]
    rising.l <- df[row,]$rising.likelihood * transProb[2,2]
  }
  if(stable.l > rising.l){
    currentstate <- states[1]
  } else {
    currentstate <- states[2]
  }
  df$state[row] <- currentstate
}
