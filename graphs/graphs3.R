library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(jsonlite)
library(striprtf)
library(gtable)
library(grid)
library(gridExtra)
library(lattice)

setwd("/Users/rebeccajarvis/Documents/prog-impf-shift/graphs/")
#setwd("~/git/prog-impf-shift/graphs/")

#getting and processing L1 data




r <- read_rtf("datafiles/l1wrapper-17state.rtf")
r <- unlist(strsplit(r, "\\}\\}"))

data <- as.data.frame(cbind(r))
data$r <- substring(data$r, 3)
data <- as.data.frame(str_split_fixed(data$r, ",", n = Inf))
data <- data[-52,]

#clean up
data$V1 <- gsub(".*:","",data$V1)
data$V2 <- gsub(".*:","",data$V2)
data$V3 <- gsub(".*:","",data$V3)
data$V8 <- gsub(".*:","",data$V8)

#rename to reasonable things
data <- data %>%
  rename(Utterance = V1) %>%
  rename(Stage = V2) %>%
  rename(Prob9 = V3) %>%
  rename(Prob7 = V4) %>%
  rename(Prob5 = V5) %>%
  rename(Prob3 = V6) %>%
  rename(Prob1 = V7)

#convert types
data$Prob9 <- as.numeric(as.character(substring(data$Prob9, 2)))
data$Prob7 <- as.numeric(as.character(data$Prob7))
data$Prob5 <- as.numeric(as.character(data$Prob5))
data$Prob3 <- as.numeric(as.character(data$Prob3))
data$Prob1 <- as.numeric(as.character(substr(data$Prob1,1,nchar(as.character(data$Prob1))-1)))

data <- data[,-c(8:12)]

#getting rid of pesky quotation marks
data$Utterance <- toupper(substring(data$Utterance,2, nchar(data$Utterance) - 1))
data$Stage <- substring(data$Stage,2, nchar(data$Stage) - 1)

#filtering out null utterance
data <- filter(data, Utterance != "")

#tidying and renaming in preparation to copy/paste old code
data <- gather(data, key = "Event", value = "Probability", Prob9, Prob7, Prob5, Prob3, Prob1)
data$Event <- as.numeric(substring(data$Event, 5))
data$Event <- paste0("T", data$Event)

#will be confused if there are more states (probably not the best way but oh well)
data <- filter(data, Stage %in% c("emer9", "emer7", "emer5", "emer3", "cat", "exp3", "exp5", "exp7", "exp9"))
data$Stage <- ifelse(data$Stage == "emer9", "9p:1i",
                     ifelse(data$Stage == "emer7", "7p:1i",
                            ifelse(data$Stage == "emer5", "5p:1i", 
                                   ifelse(data$Stage == "emer3", "3p:1i",
                                          ifelse(data$Stage == "cat", "1p:1i",
                                                 ifelse(data$Stage == "exp3", "1p:3i",
                                                        ifelse(data$Stage == "exp5", "1p:5i",
                                                               ifelse(data$Stage == "exp7", "1p:7i",
                                                                      ifelse(data$Stage == "exp9", "1p:9i", NA)))))))))


#this doesn't work now for whatever reason. go with the lien below
#data$Stage <- factor(data$Stage, labels = c("9p:1i", "7p:1i", "5p:1i", "3p:1i", "1p:1i", "1p:3i", "1p:5i", "1p:7i", "1p:9i"))

data$Stage <- ordered(data$Stage, levels = c("9p:1i", "7p:1i", "5p:1i", "3p:1i", "1p:1i", "1p:3i", "1p:5i", "1p:7i", "1p:9i"))

graphl1 <- ggplot(data, aes(fill = Utterance,  y=Probability, x=Event)) +
  geom_bar(stat="identity", position = "dodge") +
  facet_wrap(~Stage, nrow = 1) +
  theme_bw()





############





#now dealing with S2 data


datalist <- data.frame()

for (filename in c("datafiles/prog-11-s2.json", "datafiles/impf-11-s2.json", "datafiles/null-11-s2.json", "datafiles/null-11add-s2.json", "datafiles/prog-11add-s2.json", "datafiles/impf-11add-s2.json"))
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
datalist$stageno <- factor(datalist$sub,levels = c("emer1", "emer9", "emer8", "emer7", "emer6", "emer2", "emer3", "emer4", "emer5", "cat", "exp1", "exp2", "exp3", "exp4", "exp6", "exp7", "exp8", "exp9", "exp5"))

datalist$utterance <- ifelse(grepl(pattern = "prog", x = datalist$filename), "PROG",
                             ifelse(grepl(pattern = "impf", x = datalist$filename), "IMPF", "NULL"))
datalist <- subset(datalist, select = -c(model, sub, filename))


#I'll filter out nulls for now, since they're pretty uninteresting
datalist <- filter(datalist, utterance != "NULL")



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



eventdata$stageno <- factor(eventdata$stageno, labels = c("10p:1i", "9p:1i", "8p:1i", "7p:1i", "6p:1i", "5p:1i", "4p:1i", "3p:1i", "2p:1i", "1p:1i", "1p:2i", "1p:3i", "1p:4i", "1p:5i", "1p:6i", "1p:7i", "1p:8i", "1p:9i", "1p:10i"))

#narrowing our collection of stages to odd ones only
eventdata <- filter(eventdata, stageno %in% c("9p:1i", "7p:1i", "5p:1i", "3p:1i", "1p:1i", "1p:3i", "1p:5i", "1p:7i", "1p:9i"))



#putting the following instead of the second line here gives you a non-stacked plot: geom_bar(stat="identity", width=.5, position = "dodge")
evbar <- ggplot(data=eventdata, aes(x=event,y=meanfreq,fill=utterance)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("red", "blue")) +
  facet_wrap(~stageno, nrow = 1) +
  theme_bw()



##############
#display ugh

##current cheat solution
#combograph <- grid.arrange(graphl1, evbar, nrow=2)
#combogrob <- arrangeGrob(graphl1, evbar, nrow=2)
#ggsave("fig-l1_s2-combo_attempt_1.png", combogrob, height=7,width=11)


eventdata$event <- paste("T", eventdata$event, sep = "")
eventdata <- rename(eventdata, Stage = stageno) %>%
  rename(Utterance = utterance) %>%
  rename(Event = event) %>%
  rename(S2_Frequency = meanfreq)
data <- rename(data, L1_Frequency = Probability)


bigdata <- merge(data, eventdata)

bigdata <- gather(bigdata, L1_Frequency, S2_Frequency, key = "Actor", value = "Probability")
bigdata$Actor <- substring(bigdata$Actor, 1, 2)
bigdata$actoralpha <- as.factor(ifelse(bigdata$Actor == "S2", 0.5, 1))

#this is REALLY cheap, but I think it'll work
#magically multiply all S2 probabilities by 0.4
#in the graph:
###add a new scale on the right that's spread out (so 0.4 on the left corresponds to 1 on the right)
bigdata$probscalar <- ifelse(bigdata$Actor == "S2", 0.4, 1)
bigdata <- mutate(bigdata, Probability = Probability * probscalar)

bigdata$Utt_Actor <- paste(bigdata$Utterance, bigdata$Actor, sep = "_")
bigdata$Utt_Actor <- ordered(bigdata$Utt_Actor, levels = c("IMPF_L1", "PROG_L1", "IMPF_S2", "PROG_S2"))
bigdata$Utt_Actor <- ordered(bigdata$Utt_Actor, labels = c("IMPF_L1", "PROG_L1", "IMPF_S2    ", "PROG_S2"))



bigbar <- ggplot(data=bigdata, aes(x=Event,y=Probability,fill=Utt_Actor)) +
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_manual(name = "Utterance and Actor", values = c("red", "blue", "#ffaaaa", "#aaacff")) +
  facet_wrap(~Stage, nrow = 2) +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 0.4, 0.1), "L1 Probability", sec.axis=sec_axis(~.*1/0.4, name="S2 Probability") )

#ggsave("fig-l1_s2-combo_attempt_2.png", bigbar, height=7,width=12)


bigl1 <- filter(bigdata, Actor == "L1")
bigs2 <- filter(bigdata, Actor == "S2")

l1graph <- ggplot(data = bigl1, aes(x=Event,y=Probability,fill=Utt_Actor), colour = c("red", "blue")) +
  geom_bar(stat="identity", position = "dodge", width = 0.5) +
  #scale_fill_manual(name = "Utterance and Actor", values = c("red", "blue")) +
  facet_wrap(~Stage, nrow = 2) +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 0.4, 0.1), "L1 Probability", sec.axis=sec_axis(~.*1/0.4, name="S2 Probability") )

allgraph <- l1graph +
  geom_bar(data = bigs2, aes(x=Event, y = Probability, fill = Utt_Actor),stat="identity", alpha = 0.5, width = 1)



allgraph <- ggplot(data = bigs2, aes(x=Event,y=Probability,fill=Utt_Actor)) +
  geom_bar(stat="identity", width = 0.9, alpha = 1, color="darkgray") + #, show.legend=F) +
  scale_fill_manual(name = "Utterance:", values = c("blue", "#b5c1ff","red", "#ffb9b9")) +
  facet_grid(.~Stage) +
  xlab("event index") +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 0.4, 0.1), name = expression("L"[1]*" probability"), sec.axis=sec_axis(~.*1/0.4, name= expression("S"[2]*" probability") )) + 
  geom_bar(data = bigl1, aes(x=Event, y = Probability, fill = Utt_Actor),stat="identity", position = "dodge", width = 0.5, colour = "black", show.legend=T) +
  theme(legend.position="bottom")
allgraph

#ggsave("fig-l1_s2-combo_attempt_6.png", allgraph, height=2.1,width=9.6)


### trim to just seven stages

bigs2t = bigs2[bigs2$Stage!="9p:1i"&bigs2$Stage!="1p:9i",]
bigs2t$Stage = factor(bigs2t$Stage, labels=c("stage 1", "stage 2", "stage 3", "stage 4", "stage 5", "stage 6", "stage 7"))
bigs2t$Event = factor(bigs2t$Event, labels=c("t1", "t3", "t5", "t7", "t9"))

bigl1t = bigl1[bigl1$Stage!="9p:1i"&bigl1$Stage!="1p:9i",]
bigl1t$Stage = factor(bigl1t$Stage, labels=c("stage 1", "stage 2", "stage 3", "stage 4", "stage 5", "stage 6", "stage 7"))
bigl1t$Event = factor(bigl1t$Event, labels=c("t1", "t3", "t5", "t7", "t9"))

allgraph <- ggplot(data = bigs2t, aes(x=Event,y=Probability,fill=Utt_Actor)) +
  geom_bar(stat="identity", width = 0.9, alpha = 1, color="darkgray") + #, show.legend=F) +
  scale_fill_manual(name = "Utterance:", values = c("blue", "#b5c1ff","red", "#ffb9b9")) +
  facet_grid(.~Stage) +
  xlab("event index") +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 0.4, 0.1), name = expression("L"[1]*" probability"), sec.axis=sec_axis(~.*1/0.4, name= expression("S"[2]*" probability") )) + 
  geom_bar(data = bigl1t, aes(x=Event, y = Probability, fill = Utt_Actor),stat="identity", position = "dodge", width = 0.5, colour = "black", show.legend=T) +
  theme(legend.position="bottom")
allgraph

#ggsave("SuB-figure.png", allgraph, height=3.1,width=9.6)