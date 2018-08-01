library(stringr)
library(zoo)
library(tidyverse)
library(XML)
library(HapEstXXR)

#powerset fxn automatically excludes empty set
power <- powerset(c(1,3,5,7,9))
max_length <- max(sapply(power,length))
powerdf <- sapply(power, function(x){
  c(x, rep(NA, max_length - length(x)))
}) %>%
  t()
powerdf <- as.data.frame(powerdf)

##can remove singleton sets as well as [7,9]
#powerdf <- filter(powerdf, !is.na(powerdf$V2)) %>%
#  filter(powerdf$V1 != 7) #this is sufficiently restrictive w/five events

#generate all possible {ref, sup} pairs-- ref <= sup
refsup <- data.frame()
counter <- 1
for (ref in 4:10){
  for (sup in 4:10){
    if(ref <= sup){
     refsup[counter,1] <- ref
     refsup[counter,2] <- sup
     counter <- counter + 1
    }
  }
}


#smoosh everything into one
bigdf <- merge(powerdf, refsup, by.x = NULL, by.y = NULL)
colnames(bigdf) <- c("Event1", "Event2", "Event3", "Event4", "Event5", "Ref", "Sup")

#check truth of impf and prog given a state, ref, and sup
bigdf$PBin1 <- NA
bigdf$PBin2 <- NA
bigdf$IBin1 <- NA
bigdf$IBin2 <- NA

#individually check prog and impf bins
for(counter in 1:nrow(bigdf)){
  ifelse(any(bigdf[counter, 1:5] <= 0.5*bigdf[counter, 6], na.rm = T),
         bigdf[counter, 8] <- 1, bigdf[counter, 8] <-  0)
}

for(counter in 1:nrow(bigdf)){
  ifelse(any(bigdf[counter, 1:5] %in% setdiff(seq(0.5*bigdf[counter, 6], bigdf[counter, 6], by = 0.5), 0.5*bigdf[counter, 6]), na.rm = T),
         bigdf[counter, 9] <- 1, bigdf[counter, 9] <-  0)
}

for(counter in 1:nrow(bigdf)){
  ifelse(any(bigdf[counter, 1:5] <= 0.5*bigdf[counter, 7], na.rm = T),
         bigdf[counter, 10] <- 1, bigdf[counter, 10] <-  0)
}

for(counter in 1:nrow(bigdf)){
  ifelse(any(bigdf[counter, 1:5] %in% setdiff(seq(0.5*bigdf[counter, 7], bigdf[counter, 7], by = 0.5), 0.5*bigdf[counter, 6]), na.rm = T),
         bigdf[counter, 11] <- 1, bigdf[counter, 11] <-  0)
}

#truth of prog and impf
bigdf$Prog <- ifelse(bigdf$PBin1 == 1 & bigdf$PBin2 == 1, 1, 0)
bigdf$Impf <- ifelse(bigdf$IBin1 == 1 & bigdf$IBin2 == 1, 1, 0)

bigdf <- select(bigdf, -c(PBin1, PBin2, IBin1, IBin2))

sumtable <- group_by(bigdf, Prog, Impf) %>%
  summarise(Count = n())