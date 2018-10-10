library(stringr)
prog <- read.csv("prog-tokens201810081330.csv", header=FALSE)
impf <- read.csv("impf-tokens201810081330.csv", header=FALSE)

timesP <- str_match_all(prog$V1, "[\\.-]([0-9ehpxm\\.-]*)\\.pos$")
timesI <- str_match_all(impf$V1, "[\\.-]([0-9ehpxm\\.-]*)\\.pos$")

times2P <- unlist(lapply(timesP, "[", ,2))
times2I <- unlist(lapply(timesI, "[", ,2))

impf$V3 <- 0
impf$V3 <- times2I

unique(prog$V1)

table(prog[prog$V3=="BEP",]$V4)
table(impf$V3)
