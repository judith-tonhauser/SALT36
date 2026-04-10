# "stop" experiment
# graphs

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load required packages
library(tidyverse)

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

theme_set(theme_bw())

# load helper functions
source('../../../helpers.R')

# load clean data
d = read_csv("../data/cd.csv")
nrow(d) #500

names(d)
table(d$utterance,d$qud,d$item)
length(unique(d$participantID)) #500

# calculate means by condition

table(d$utterance)

means = d %>%
  group_by(qud,verb,environment,prior,response_type) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(-c(CILow, CIHigh))
means

# plot for know
k = means[means$verb=="know",]
ggplot(data=k,aes(x=response_type,y=Mean,fill=prior))+
  geom_bar(stat = "identity",width = 0.3,position = position_dodge()) +
  geom_errorbar(aes(x=response_type, ymin=YMin, ymax=YMax), width=0.2, colour="black", alpha=1, size=1) +
  facet_grid(environment ~ qud) 

# plot for stop
s = means[means$verb=="stop",]
ggplot(data=s,aes(x=response_type,y=Mean,fill=prior))+
  geom_bar(stat = "identity",width = 0.3,position = position_dodge(1)) +
  geom_errorbar(aes(x=response_type, ymin=YMin, ymax=YMax,), width=0.2, colour="black", alpha=1, size=1,position=position_dodge(1)) +
  facet_grid(environment ~ qud) 

# calculate means by verb

means_verb = d %>%
  filter(response_type=="CC"|response_type=="PRE") %>%
  group_by(verb,environment) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(-c(CILow, CIHigh))
means_verb

# calculate means by prior

means_prior = d %>%
  filter(response_type=="CC"|response_type=="PRE") %>%
  group_by(prior,environment) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(-c(CILow, CIHigh))
means_prior

# calculate means by environment

means_environment = d %>%
  filter(response_type=="CC"|response_type=="PRE") %>%
  group_by(environment,verb) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(-c(CILow, CIHigh))
means_environment

# script ends here




ggplot(data=means.by.qud, aes(x=content, y=Mean)) +
  geom_bar(stat = "identity",width = 0.3, alpha = .7) +
  geom_errorbar(aes(x=content, ymin=YMin, ymax=YMax), width=0.2, colour="black", alpha=1, size=1) +
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  #scale_x_discrete(labels=c("state.CC" = "Charley speaks \n Spanish", "state.BEL" = "Cole believes that \n Charley speaks Spanish")) +
  #scale_x_discrete(labels=c("state.CC" = "CC", "state.BEL" = "BEL")) +
  facet_grid(. ~ qud) +
  #theme(axis.title.x=element_blank()) +
  ylab("Mean inference rating") +
  xlab("Inferences") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) 
ggsave("../graphs/mean-rating-neg-stop.pdf",height=2,width=2)

# mean rating (barplot) for PAST and notNOW for all utterances by QUD ----

# load clean data
d = read_csv("../data/cd.csv")
nrow(d) #500

table(d$utterance)

means.PAST.utt.qud = d %>%
  group_by(utterance,qud) %>%
  summarize(Mean = mean(responsePAST), CILow = ci.low(responsePAST), CIHigh = ci.high(responsePAST)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(-c(CILow, CIHigh)) %>%
  mutate(content = "PAST")
means.PAST.utt.qud

means.notNOW.utt.qud = d %>%
  group_by(utterance,qud) %>%
  summarize(Mean = mean(responseNotNOW), CILow = ci.low(responseNotNOW), CIHigh = ci.high(responseNotNOW)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(-c(CILow, CIHigh)) %>%
  mutate(content = "notNOW")
means.notNOW.utt.qud

# bind the data
means.by.qud = rbind(means.PAST.utt.qud,means.notNOW.utt.qud)
means.by.qud

# change the utterance names
table(d$utterance)
means.by.qud = means.by.qud %>%
  mutate(utterance = recode(utterance, "simple-pos-past" = "Charley smoked", 
                            "simple-pos-now" = "Charley smokes",
                            "stop-neg" = "Charley didn't stop smoking",
                            "stop-pos" = "Charley stopped smoking",
                            "simple-pos-past" = "Charley smoked",
                            "simple-neg-past" = "Charley didn't smoke",
                            "simple-neg-now" = "Charley doesn't smoke",
                            "not-not-now" = "There's no way that Charley doesn't smoke"))

# order the utterances by mean projection strength of CC
tmp = d %>%
  mutate(utterance = recode(utterance, "simple-pos-past" = "Charley smoked", 
                            "simple-pos-now" = "Charley smokes",
                            "stop-neg" = "Charley didn't stop smoking",
                            "stop-pos" = "Charley stopped smoking",
                            "simple-pos-past" = "Charley smoked",
                            "simple-neg-past" = "Charley didn't smoke",
                            "simple-neg-now" = "Charley doesn't smoke",
                            "not-not-now" = "There's no way that Charley doesn't smoke")) %>%
  group_by(utterance) %>%
  summarize(Mean = mean(responsePAST))
tmp
means.by.qud$utterance = factor(means.by.qud$utterance, levels = tmp$utterance[order(tmp$Mean)], ordered = TRUE)

# library for nested facets
library(ggh4x)

ggplot(data=means.by.qud, aes(x=content, y=Mean)) +
  geom_bar(stat = "identity",width = 0.3, alpha = .7) +
  geom_errorbar(aes(x=content, ymin=YMin, ymax=YMax), width=0.2, colour="black", alpha=1, size=1) +
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  #scale_x_discrete(labels=c("state.CC" = "Charley speaks \n Spanish", "state.BEL" = "Cole believes that \n Charley speaks Spanish")) +
  #scale_x_discrete(labels=c("state.CC" = "CC", "state.BEL" = "BEL")) +
  facet_nested(. ~ utterance + qud) +
  #theme(axis.title.x=element_blank()) +
  ylab("Mean inference rating") +
  xlab("Inferences") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) 
ggsave("../graphs/mean-rating-by-utt-and-qud.pdf",height=2,width=16)

# mean rating (points, violins) for PAST and notNOW for all utterances by QUD ----

# load clean data
d = read_csv("../data/cd.csv")
nrow(d) #500

# transform the data into long format
d = d %>%
  select(-c(responseNOW)) %>%
  gather(responseTo, rating, responsePAST:responseNotNOW) %>%
  mutate(content = recode(responseTo, "responsePAST" = "PAST", "responseNotNOW" = "notNOW"))
nrow(d) #1000

means.PAST.utt.qud = d %>%
  group_by(utterance,qud) %>%
  summarize(Mean = mean(responsePAST), CILow = ci.low(responsePAST), CIHigh = ci.high(responsePAST)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(-c(CILow, CIHigh)) %>%
  mutate(content = "PAST")
means.PAST.utt.qud

means.notNOW.utt.qud = d %>%
  group_by(utterance,qud) %>%
  summarize(Mean = mean(responseNotNOW), CILow = ci.low(responseNotNOW), CIHigh = ci.high(responseNotNOW)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(-c(CILow, CIHigh)) %>%
  mutate(content = "notNOW")
means.notNOW.utt.qud

# bind the data
means.by.qud = rbind(means.PAST.utt.qud,means.notNOW.utt.qud)
means.by.qud

# change the utterance names
table(d$utterance)
means.by.qud = means.by.qud %>%
  mutate(utterance = recode(utterance, "simple-pos-past" = "Charley smoked", 
                            "simple-pos-now" = "Charley smokes",
                            "stop-neg" = "Charley didn't stop smoking",
                            "stop-pos" = "Charley stopped smoking",
                            "simple-pos-past" = "Charley smoked",
                            "simple-neg-past" = "Charley didn't smoke",
                            "simple-neg-now" = "Charley doesn't smoke",
                            "not-not-now" = "There's no way that Charley doesn't smoke"))

# order the utterances by mean projection strength of CC
tmp = d %>%
  mutate(utterance = recode(utterance, "simple-pos-past" = "Charley smoked", 
                            "simple-pos-now" = "Charley smokes",
                            "stop-neg" = "Charley didn't stop smoking",
                            "stop-pos" = "Charley stopped smoking",
                            "simple-pos-past" = "Charley smoked",
                            "simple-neg-past" = "Charley didn't smoke",
                            "simple-neg-now" = "Charley doesn't smoke",
                            "not-not-now" = "There's no way that Charley doesn't smoke")) %>%
  group_by(utterance) %>%
  summarize(Mean = mean(responsePAST))
tmp

d = d %>%
  mutate(utterance = recode(utterance, "simple-pos-past" = "Charley smoked", 
                            "simple-pos-now" = "Charley smokes",
                            "stop-neg" = "Charley didn't stop smoking",
                            "stop-pos" = "Charley stopped smoking",
                            "simple-pos-past" = "Charley smoked",
                            "simple-neg-past" = "Charley didn't smoke",
                            "simple-neg-now" = "Charley doesn't smoke",
                            "not-not-now" = "There's no way that Charley doesn't smoke"))

means.by.qud$utterance = factor(means.by.qud$utterance, levels = tmp$utterance[order(tmp$Mean)], ordered = TRUE)
d$utterance = factor(d$utterance, levels = tmp$utterance[order(tmp$Mean)], ordered = TRUE)

str(means.by.qud$utterance)
str(d$utterance)

# library for nested facets
library(ggh4x)

# all utterances
ggplot() +
  geom_violin(data=d, aes(x=content, y=rating), color="gray") +
  geom_point(data=means.by.qud, aes(x=content, y=Mean)) +
  geom_errorbar(data=means.by.qud, aes(x=content, ymin=YMin, ymax=YMax), width=0.2, colour="black", alpha=1, size=.5) +
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  #scale_x_discrete(labels=c("state.CC" = "Charley speaks \n Spanish", "state.BEL" = "Cole believes that \n Charley speaks Spanish")) +
  #scale_x_discrete(labels=c("state.CC" = "CC", "state.BEL" = "BEL")) +
  facet_nested(. ~ utterance + qud) +
  #theme(axis.title.x=element_blank()) +
  ylab("Mean inference rating") +
  xlab("Inferences") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) 
ggsave("../graphs/mean-rating-by-utt-and-qud.pdf",height=2,width=16)

# stop utterances
ggplot() +
  geom_violin(data=d[d$utterance == "Charley stopped smoking" | d$utterance == "Charley didn't stop smoking",], aes(x=content, y=rating), color="gray") +
  geom_point(data=means.by.qud[means.by.qud$utterance == "Charley stopped smoking" | means.by.qud$utterance == "Charley didn't stop smoking",], aes(x=content, y=Mean)) +
  geom_errorbar(data=means.by.qud[means.by.qud$utterance == "Charley stopped smoking" | means.by.qud$utterance == "Charley didn't stop smoking",], aes(x=content, ymin=YMin, ymax=YMax), width=0.2, colour="black", alpha=1, size=.5) +
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  #scale_x_discrete(labels=c("state.CC" = "Charley speaks \n Spanish", "state.BEL" = "Cole believes that \n Charley speaks Spanish")) +
  #scale_x_discrete(labels=c("state.CC" = "CC", "state.BEL" = "BEL")) +
  facet_nested(. ~ utterance + qud) +
  #theme(axis.title.x=element_blank()) +
  ylab("Mean inference rating") +
  xlab("Inferences") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) 
ggsave("../graphs/mean-rating-by-utt-and-qud-stop-utts.pdf",height=3,width=6)

# doesn't smoke/smoked utterances (notNOW and PAST at-issue entailments)
ggplot() +
  geom_violin(data=d[d$utterance == "Charley doesn't smoke" | d$utterance == "Charley smoked",], aes(x=content, y=rating), color="gray") +
  geom_point(data=means.by.qud[means.by.qud$utterance == "Charley doesn't smoke" | means.by.qud$utterance == "Charley smoked",], aes(x=content, y=Mean)) +
  geom_errorbar(data=means.by.qud[means.by.qud$utterance == "Charley doesn't smoke" | means.by.qud$utterance == "Charley smoked",], aes(x=content, ymin=YMin, ymax=YMax), width=0.2, colour="black", alpha=1, size=.5) +
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  #scale_x_discrete(labels=c("state.CC" = "Charley speaks \n Spanish", "state.BEL" = "Cole believes that \n Charley speaks Spanish")) +
  #scale_x_discrete(labels=c("state.CC" = "CC", "state.BEL" = "BEL")) +
  facet_nested(. ~ utterance + qud) +
  #theme(axis.title.x=element_blank()) +
  ylab("Mean inference rating") +
  xlab("Inferences") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) 
ggsave("../graphs/mean-rating-by-utt-and-qud-at-issue-ent.pdf",height=3,width=6)

# there's no way/didn't smoke, notNOW and PAST at-issue under negation)
ggplot() +
  geom_violin(data=d[d$utterance == "There's no way that Charley doesn't smoke" | d$utterance == "Charley didn't smoke",], aes(x=content, y=rating), color="gray") +
  geom_point(data=means.by.qud[means.by.qud$utterance == "There's no way that Charley doesn't smoke" | means.by.qud$utterance == "Charley didn't smoke",], aes(x=content, y=Mean)) +
  geom_errorbar(data=means.by.qud[means.by.qud$utterance == "There's no way that Charley doesn't smoke" | means.by.qud$utterance == "Charley didn't smoke",], aes(x=content, ymin=YMin, ymax=YMax), width=0.2, colour="black", alpha=1, size=.5) +
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  #scale_x_discrete(labels=c("state.CC" = "Charley speaks \n Spanish", "state.BEL" = "Cole believes that \n Charley speaks Spanish")) +
  #scale_x_discrete(labels=c("state.CC" = "CC", "state.BEL" = "BEL")) +
  facet_nested(. ~ utterance + qud) +
  #theme(axis.title.x=element_blank()) +
  ylab("Mean inference rating") +
  xlab("Inferences") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) 
ggsave("../graphs/mean-rating-by-utt-and-qud-negated-at-issue-ent.pdf",height=3,width=6)


# mean rating (points, violins) for PAST and notNOW separately ----

# load clean data
d = read_csv("../data/cd.csv")
nrow(d) #500

means.PAST.utt.qud = d %>%
  group_by(utterance,qud) %>%
  summarize(Mean = mean(responsePAST), CILow = ci.low(responsePAST), CIHigh = ci.high(responsePAST)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(-c(CILow, CIHigh)) %>%
  mutate(content = "PAST")
means.PAST.utt.qud

means.notNOW.utt.qud = d %>%
  group_by(utterance,qud) %>%
  summarize(Mean = mean(responseNotNOW), CILow = ci.low(responseNotNOW), CIHigh = ci.high(responseNotNOW)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(-c(CILow, CIHigh)) %>%
  mutate(content = "notNOW")
means.notNOW.utt.qud

# bind the data
means.by.qud = rbind(means.PAST.utt.qud,means.notNOW.utt.qud)
means.by.qud

# change the utterance names
table(d$utterance)
means.by.qud = means.by.qud %>%
  mutate(utterance = recode(utterance, "simple-pos-past" = "Charley smoked", 
                            "simple-pos-now" = "Charley smokes",
                            "stop-neg" = "Charley hasn't stopped smoking",
                            "stop-pos" = "Charley has stopped smoking",
                            "simple-pos-past" = "Charley smoked",
                            "simple-neg-past" = "Charley didn't smoke",
                            "simple-neg-now" = "Charley doesn't smoke",
                            "not-not-now" = "There's no way that Charley doesn't smoke"))

# order the utterances by mean projection strength of CC
tmp = d %>%
  mutate(utterance = recode(utterance, "simple-pos-past" = "Charley smoked", 
                            "simple-pos-now" = "Charley smokes",
                            "stop-neg" = "Charley hasn't stopped smoking",
                            "stop-pos" = "Charley has stopped smoking",
                            "simple-pos-past" = "Charley smoked",
                            "simple-neg-past" = "Charley didn't smoke",
                            "simple-neg-now" = "Charley doesn't smoke",
                            "not-not-now" = "There's no way that Charley doesn't smoke")) %>%
  group_by(utterance) %>%
  summarize(Mean = mean(responsePAST))
tmp

# transform the data into long format
d = d %>%
  select(-c(responseNOW)) %>%
  gather(responseTo, rating, responsePAST:responseNotNOW) %>%
  mutate(content = recode(responseTo, "responsePAST" = "PAST", "responseNotNOW" = "notNOW"))
nrow(d) #1000

d = d %>%
  mutate(utterance = recode(utterance, "simple-pos-past" = "Charley smoked", 
                            "simple-pos-now" = "Charley smokes",
                            "stop-neg" = "Charley hasn't stopped smoking",
                            "stop-pos" = "Charley has stopped smoking",
                            "simple-pos-past" = "Charley smoked",
                            "simple-neg-past" = "Charley didn't smoke",
                            "simple-neg-now" = "Charley doesn't smoke",
                            "not-not-now" = "There's no way that Charley doesn't smoke"))

means.by.qud$utterance = factor(means.by.qud$utterance, levels = tmp$utterance[order(tmp$Mean)], ordered = TRUE)
d$utterance = factor(d$utterance, levels = tmp$utterance[order(tmp$Mean)], ordered = TRUE)

str(means.by.qud$utterance)
str(d$utterance)

# library for nested facets
library(ggh4x)

# inference to PAST
ggplot() +
  geom_violin(data=d[d$content == "PAST",], aes(x=utterance, y=rating), color="gray") +
  geom_point(data=means.by.qud[means.by.qud$content == "PAST",], aes(x=utterance, y=Mean)) +
  geom_errorbar(data=means.by.qud[means.by.qud$content == "PAST",], aes(x=utterance, ymin=YMin, ymax=YMax), width=0.2, colour="black", alpha=1, size=.5) +
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  #scale_x_discrete(labels=c("state.CC" = "Charley speaks \n Spanish", "state.BEL" = "Cole believes that \n Charley speaks Spanish")) +
  #scale_x_discrete(labels=c("state.CC" = "CC", "state.BEL" = "BEL")) +
  facet_nested(. ~ qud) +
  #theme(axis.title.x=element_blank()) +
  ylab("Mean inference rating for PAST") +
  xlab("Utterance") +
  coord_flip() +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) 
ggsave("../graphs/mean-rating-by-utt-and-qud-PAST.pdf",height=3,width=8)

# inference to notNOW
ggplot() +
  geom_violin(data=d[d$content == "notNOW",], aes(x=utterance, y=rating), color="gray") +
  geom_point(data=means.by.qud[means.by.qud$content == "notNOW",], aes(x=utterance, y=Mean)) +
  geom_errorbar(data=means.by.qud[means.by.qud$content == "notNOW",], aes(x=utterance, ymin=YMin, ymax=YMax), width=0.2, colour="black", alpha=1, size=.5) +
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  #scale_x_discrete(labels=c("state.CC" = "Charley speaks \n Spanish", "state.BEL" = "Cole believes that \n Charley speaks Spanish")) +
  #scale_x_discrete(labels=c("state.CC" = "CC", "state.BEL" = "BEL")) +
  facet_nested(. ~ qud) +
  #theme(axis.title.x=element_blank()) +
  ylab("Mean inference rating for notNOW") +
  xlab("Utterance") +
  coord_flip() +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) 
ggsave("../graphs/mean-rating-by-utt-and-qud-notNOW.pdf",height=3,width=8)

# mean rating for PAST and notNOW for all utterances by QUD and item ----

# load clean data
d = read_csv("../data/cd.csv")
nrow(d) #500

table(d$utterance)

means.PAST.utt.qud = d %>%
  group_by(utterance,qud,item) %>%
  summarize(Mean = mean(responsePAST), CILow = ci.low(responsePAST), CIHigh = ci.high(responsePAST)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(-c(CILow, CIHigh)) %>%
  mutate(content = "PAST")
means.PAST.utt.qud

means.notNOW.utt.qud = d %>%
  group_by(utterance,qud,item) %>%
  summarize(Mean = mean(responseNotNOW), CILow = ci.low(responseNotNOW), CIHigh = ci.high(responseNotNOW)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(-c(CILow, CIHigh)) %>%
  mutate(content = "notNOW")
means.notNOW.utt.qud

# bind the data
means.by.qud = rbind(means.PAST.utt.qud,means.notNOW.utt.qud)
means.by.qud

# change the utterance names
table(d$utterance)
means.by.qud = means.by.qud %>%
  mutate(utterance = recode(utterance, "simple-pos-past" = "Charley smoked", 
                            "simple-pos-now" = "Charley smokes",
                            "stop-neg" = "Charley didn't stop smoking",
                            "stop-pos" = "Charley stopped smoking",
                            "simple-pos-past" = "Charley smoked",
                            "simple-neg-past" = "Charley didn't smoke",
                            "simple-neg-now" = "Charley doesn't smoke",
                            "not-not-now" = "There's no way that Charley doesn't smoke"))

# order the utterances by mean projection strength of CC
tmp = d %>%
  mutate(utterance = recode(utterance, "simple-pos-past" = "Charley smoked", 
                            "simple-pos-now" = "Charley smokes",
                            "stop-neg" = "Charley didn't stop smoking",
                            "stop-pos" = "Charley stopped smoking",
                            "simple-pos-past" = "Charley smoked",
                            "simple-neg-past" = "Charley didn't smoke",
                            "simple-neg-now" = "Charley doesn't smoke",
                            "not-not-now" = "There's no way that Charley doesn't smoke")) %>%
  group_by(utterance) %>%
  summarize(Mean = mean(responsePAST))
tmp
means.by.qud$utterance = factor(means.by.qud$utterance, levels = tmp$utterance[order(tmp$Mean)], ordered = TRUE)

# library for nested facets
library(ggh4x)

ggplot(data=means.by.qud, aes(x=content, y=Mean)) +
  geom_bar(stat = "identity",width = 0.3, alpha = .7) +
  geom_errorbar(aes(x=content, ymin=YMin, ymax=YMax), width=0.2, colour="black", alpha=1, size=1) +
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  #scale_x_discrete(labels=c("state.CC" = "Charley speaks \n Spanish", "state.BEL" = "Cole believes that \n Charley speaks Spanish")) +
  #scale_x_discrete(labels=c("state.CC" = "CC", "state.BEL" = "BEL")) +
  facet_nested(item ~ utterance + qud) +
  #theme(axis.title.x=element_blank()) +
  ylab("Mean inference rating") +
  xlab("Inferences") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) 
ggsave("../graphs/mean-rating-by-utt-and-qud-and-item.pdf",height=4,width=16)



