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

#and now plotting in a different way
#ehh, I don't think this tells very much b/c they always add up to ~1
#I'm not going to bother doing a category version of this one
#list2 <- datalist #just copying this so I don't mess it up/can plot graphs in any order
#list2 <- spread(list2, key = utterance, value = freq)
#list2 <- rename(list2, impffreq = IMPF, progfreq = PROG)
#listgraph <- ggplot(data=list2, aes(x = impffreq, y = progfreq, colour = statetype)) + geom_point(aes(colour = worldstate, shape = as.factor(stageno))) + theme_bw()

####################

# a faceted plot with all the data
ggplot(data=datalist,aes(x=stageno,y=freq,color=utterance,group=utterance)) +
  geom_line(alpha=0.5) +
  geom_point() +
  facet_wrap(~worldstate) +
  scale_color_manual(values=c("red", "blue")) +
  theme_bw()
#ggsave("all-plot.png",height=7,width=11)