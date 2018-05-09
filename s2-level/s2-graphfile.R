library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(jsonlite)

#location of this folder on your computer
setwd("/Users/rebeccajarvis/Documents/prog-impf-shift/s2-level/")
#setwd("~/git/prog-impf-shift/s2-level/")

datalist <- data.frame()

for (filename in c("prog-5-s2.json", "impf-5-s2.json", "null-5-s2.json"))
{
  templist <- cbind(filename, fromJSON(filename, flatten=TRUE))
  datalist <- rbind(datalist, templist)
}
rm(templist)
rm(filename)

#ugly but oh well
#removing states I don't really want to plot--[0.7, 0.9] and one-event states, b/c they have to be described by null
datalist <- filter(datalist, y != 0)

#I think some of this is unneccessary, but I'll just leave it
datalist$x <- substring(datalist$x, 3)
datalist$x <- gsub('.{1}$', '', datalist$x)
datalist <- rename(datalist, worldstate = x)
datalist <- rename(datalist, freq = y)
datalist$stageno <- factor(datalist$sub,levels = c("emer1", "emer2", "cat", "exp1", "exp2"))

datalist$utterance <- ifelse(grepl(pattern = "prog", x = datalist$filename), "PROG",
                             ifelse(grepl(pattern = "impf", x = datalist$filename), "IMPF", "NULL"))
datalist <- subset(datalist, select = -c(model, sub, filename))


#I'll filter out nulls for now, since they're pretty uninteresting
datalist <- filter(datalist, utterance != "NULL")

#optional further filtering to just plot select world states (can comment out)
#datalist <- filter(datalist, worldstate %in% c("0.1, 0.3", "0.3, 0.7", "0.1, 0.3, 0.5, 0.7, 0.9"))





#graphs




####################

#graph of each worldstate through all states (pretty hard to read)
fullgraph <- ggplot(data=datalist, aes(x = stageno, y = freq, colour = worldstate, group = interaction(worldstate, utterance))) + geom_line(aes(linetype = utterance)) + theme_bw() #+ geom_point()

#ggsave("s2-allstates.jpg", plot = fullgraph, device = NULL, path = NULL,
#       scale = 1, width = 10, height = 6,
#       dpi = 300, limitsize = TRUE)


####################


#just plotting a couple states
datalist1 <- filter(datalist, worldstate %in% c("0.1, 0.3", "0.3, 0.7", "0.1, 0.3, 0.5, 0.7, 0.9"))
smallgraph <- ggplot(data=datalist1, aes(x = stageno, y = freq, colour = worldstate, group = interaction(worldstate, utterance))) + geom_line(aes(linetype = utterance)) + theme_bw() #+ geom_point()

#ggsave("s2-smallgraph.jpg", plot = smallgraph, device = NULL, path = NULL,
#       scale = 1, width = 10, height = 6,
#       dpi = 300, limitsize = TRUE)


####################


#graph colored by "category"
multistate <- c("0.1, 0.3, 0.5, 0.7, 0.9", "0.1, 0.3, 0.5, 0.7", "0.1, 0.3, 0.5, 0.9", "0.1, 0.3, 0.7, 0.9", "0.1, 0.5, 0.7, 0.9", "0.3, 0.5, 0.7, 0.9", "0.1, 0.3, 0.5", "0.1, 0.3, 0.7", "0.1, 0.5, 0.7", "0.1, 0.3, 0.9", "0.1, 0.5, 0.9", "0.1, 0.7, 0.9", "0.3, 0.5, 0.7", "0.3, 0.5, 0.9", "0.3, 0.7, 0.9", "0.5, 0.7, 0.9")
twospread <- c("0.1, 0.7", "0.1, 0.9", "0.3, 0.7", "0.3, 0.9", "0.5, 0.7", "0.5, 0.9")
twonarrow <- c("0.1, 0.3", "0.3, 0.5", "0.1, 0.5")

datalist$statetype <- ifelse(datalist$worldstate %in% multistate, "multistate",
                             ifelse(datalist$worldstate %in% twospread, "twospread", "twonarrow"))
catgraph <- ggplot(data=datalist, aes(x = stageno, y = freq, colour = statetype)) + geom_line(aes(linetype = utterance, group = interaction(worldstate, utterance))) + theme_bw() #+ geom_point()

#ggsave("s2-allstates-bycat.jpg", plot = catgraph, device = NULL, path = NULL,
#       scale = 1, width = 8, height = 6,
#       dpi = 300, limitsize = TRUE)


#these lines average across all the states in a "category"
datalist <- group_by(datalist, statetype, stageno, utterance)
summary <- summarise(datalist, meanfreq = mean(freq))
sumgraph <- ggplot(data=summary, aes(x = stageno, y = meanfreq, colour = statetype, group = interaction(statetype, utterance))) + geom_line(aes(linetype = utterance)) + theme_bw() #+ geom_point()
#ggsave("s2-summary", plot = sumgraph, device = NULL, path = NULL,
#       scale = 1, width = 8, height = 6,
#       dpi = 300, limitsize = TRUE)


####################

# a faceted plot with all the data
ggplot(data=datalist,aes(x=stageno,y=freq,color=utterance,group=utterance)) +
  geom_line(alpha=0.5) +
  geom_point() +
  facet_wrap(~worldstate) +
  scale_color_manual(values=c("red", "blue")) +
  theme_bw()
#ggsave("all-plot.png",height=7,width=11)

####################

#other sort of averaging
datalist$statesort <- ifelse(grepl("0.1, 0.3", datalist$worldstate), "both1and3", 
                             ifelse(!grepl("0.1", datalist$worldstate), "not1", NA))
datalist2 <- filter(datalist, !is.na(statesort))
datalist2 <- group_by(datalist2, statesort, stageno, utterance)
sum2 <- summarise(datalist2, meanfreq = mean(freq))

graph2 <- ggplot(data=sum2, aes(x = stageno, y = meanfreq, colour = statesort, group = interaction(statesort, utterance))) + geom_line(aes(linetype = utterance)) + theme_bw() + scale_color_manual(values=c("red", "blue"))
#ggsave("statesort.png",height=7,width=11)

####################

datalist$ev1 <- ifelse(grepl("0.1", datalist$worldstate), 1, NA)
datalist$ev3 <- ifelse(grepl("0.3", datalist$worldstate), 3, NA)
datalist$ev5 <- ifelse(grepl("0.5", datalist$worldstate), 5, NA)
datalist$ev7 <- ifelse(grepl("0.7", datalist$worldstate), 7, NA)
datalist$ev9 <- ifelse(grepl("0.9", datalist$worldstate), 9, NA)

#not elegant but works
ev1data <- filter(datalist, !is.na(ev1)) %>%
  group_by(stageno, utterance) %>%
  summarise(meanfreq = mean(freq)) %>%
  mutate(event = "1")

ev3data <- filter(datalist, !is.na(ev3)) %>%
  group_by(stageno, utterance) %>%
  summarise(meanfreq = mean(freq)) %>%
  mutate(event = "3")

ev5data <- filter(datalist, !is.na(ev5)) %>%
  group_by(stageno, utterance) %>%
  summarise(meanfreq = mean(freq)) %>%
  mutate(event = "5")

ev7data <- filter(datalist, !is.na(ev7)) %>%
  group_by(stageno, utterance) %>%
  summarise(meanfreq = mean(freq)) %>%
  mutate(event = "7")

ev9data <- filter(datalist, !is.na(ev9)) %>%
  group_by(stageno, utterance) %>%
  summarise(meanfreq = mean(freq)) %>%
  mutate(event = "9")

eventdata <- rbind(ev1data, ev3data, ev5data, ev7data, ev9data)
rm(ev1data)
rm(ev3data)
rm(ev5data)
rm(ev7data)
rm(ev9data)

evgraph <- ggplot(data=eventdata, aes(x = stageno, y = meanfreq, colour = event, group = interaction(event, utterance))) +
  geom_line(aes(linetype = utterance)) +
  theme_bw()

#ggsave("evgraph.png",height=7,width=11)

