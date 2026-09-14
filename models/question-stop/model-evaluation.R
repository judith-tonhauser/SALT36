# question model "stop"

# load required libraries
library(jsonlite)
library(tidyverse)
library(rwebppl)
library(ggh4x) # nested facets

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
theme_set(theme_bw())

source('../../helpers.R')

# read in the  model 
model <- read_file("model.wppl")
model

# evaluate the model
eval_webppl <- function(command) {
  webppl(paste(model,command,sep="\n"))
}

# define the utterances
interrogatives = c("did-cole-stop?", 
               "did-cole-pre?",
               "does-cole-notPost?")
interrogatives

# define the goals
goals = c("PRE","notPOST")
goals

# define priors
priors = c("higher", "lower")
priors

# pragmatic listener ----

# input to lookupState: interrogative,goalBias,prior

#### call PL ----

PL = data.frame(interrogative = character(), qudBias = character(), prior = character(),
                PRE = numeric(), notPOST = numeric(), prob = numeric())
PL

for (q in interrogatives) {
  print(q)
  for (g in goals) {
    print(g)
    for (p in priors) {
      print(p)
      PL_tmp = eval_webppl(paste("lookupState('",q,"','",g,"','",p,"')",sep=""))
      for (i in 1:nrow(PL_tmp)) {
        PL = PL %>% 
          add_row(interrogative = q, qudBias = g, prior = p, 
                  PRE = PL_tmp$PRE[i],
                  notPOST = PL_tmp$notPOST[i],
                  prob = PL_tmp$prob[i])
      }
    }
  }
}


PL
#view(PL)
nrow(PL) #48

write_csv(PL, file="data/PL.csv")

#### predictions reported in (9) in SALT paper ----

# read model data
PL = read_csv("data/PL.csv")
nrow(PL) #48
#view(PL)

# make long format to be able to aggregate state.PRE and state.notPOST
PL2 = PL %>% pivot_longer(
  cols = PRE:notPOST,
  names_to = c("state"),
  values_to = c("trueFalse")) 
PL2
nrow(PL2) #96
#view(PL2)

# keep rows where PRE=1 or notPOST=1, sum up identical conditions 
# (created by making long format), take mean across priors and qudBias
means = PL2 %>%
  filter(trueFalse == 1) %>%
  select(-c(trueFalse)) %>%
  droplevels() %>%
  group_by(interrogative,qudBias,state,prior) %>%
  summarize(prior.sum = sum(prob)) %>%
  group_by(interrogative,state) %>%
  summarize(prob = mean(prior.sum))
means
# 3 did-cole-stop?     PRE     0.612

# keep rows where PRE=1 or notPOST=1, sum up identical conditions 
# (created by making long format), take mean across qudBias
means.byPrior = PL2 %>%
  filter(trueFalse == 1) %>%
  select(-c(trueFalse)) %>%
  droplevels() %>%
  group_by(interrogative,qudBias,state,prior) %>%
  summarize(prior.sum = sum(prob)) %>%
  group_by(interrogative,prior,state) %>%
  summarize(prob = mean(prior.sum))
means.byPrior
# 5 did-cole-stop?     higher PRE     0.760
# 7 did-cole-stop?     lower  PRE     0.464

# speaker ----

# define the PAs
pas = c("[{notPOST: 1, PRE: 1}, {notPOST: 0, PRE: 1}, {notPOST: 1, PRE: 0}, {notPOST: 0, PRE: 0}]",
        "[{notPOST: 1, PRE: 1}, {notPOST: 1, PRE: 0}]", # notPOST:1
        "[{notPOST: 1, PRE: 1}, {notPOST: 0, PRE: 1}]", # PRE:1
        "[{notPOST: 0, PRE: 1}, {notPOST: 0, PRE: 0}]", # notPOST:0
        "[{notPOST: 1, PRE: 0}, {notPOST: 0, PRE: 0}]" # PRE:0
)
pas

# input to speaker: goal, pa

S = data.frame(goal = character(), pa = character(), 
               utterance = character(), prob = numeric())
S

for (g in goals) {
  for (p in pas) {
    S_tmp = eval_webppl(paste("speaker('",g,"',",p,")",sep=""))
    for (i in 1:nrow(S_tmp)) {
      S = S %>% 
        add_row(goal=g, pa=p,
                utterance = S_tmp$support[i], prob = S_tmp$prob[i])
    }
  }
}
S
#view(S)

# save raw speaker data
write_csv(S, file="data/S_raw.csv")

#### clean up the speaker data ----

# read raw speaker
S = read_csv(file="data/S_raw.csv")

# speaker plots ----

# plot probability of utterance by state and QUD ----
S_agg = aggregate(prob~goal*utterance*pa,data=S,FUN=mean)
S_agg

# plot utterance probability with PA = worldState

worldState <- "[{notPOST: 1, PRE: 1}, {notPOST: 0, PRE: 1}, {notPOST: 1, PRE: 0}, {notPOST: 0, PRE: 0}]"
worldState

ggplot(S_agg[S_agg$pa == worldState,], aes(x=utterance, y=prob)) +
  geom_bar(stat="identity") +
  facet_grid(. ~ goal, scales = "free_y") +
  ylab("probability") +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  coord_flip()
ggsave("graphs/utterance-probability-by-state-and-QUD-paworldState.pdf",width=4,height=3)

# plot utterance probability with PA = CC:1

PRE1 <- "[{notPOST: 1, PRE: 1}, {notPOST: 0, PRE: 1}]"
PRE1

ggplot(S_agg[S_agg$pa == PRE1,], aes(x=utterance, y=prob)) +
  geom_bar(stat="identity") +
  facet_grid(. ~ goal, scales = "free_y") +
  ylab("probability") +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  coord_flip()
ggsave("graphs/utterance-probability-by-state-and-QUD-paCC1.pdf",width=4,height=3)


#### plot probability of q-stop by PA, state and QUD ----

# read cleaned up speaker
S = read_csv(file="data/S_raw.csv")
#view(S)

# notPOST? QUD
S_agg = aggregate(prob~pa,data=S[S$goal == "notPOST" & S$utterance == "did-cole-stop?",],FUN=mean)
S_agg

# S_agg$PA = paste(S_agg$PRE, S_agg$notPOST, sep="-")
# S_agg$combined = paste(S_agg$PRE, S_agg$notPOST, S_agg$state, sep="-")
# S_agg
# S_agg = S_agg %>%
#   select(c(PA,state,prob))

ggplot(S_agg, aes(x=prob, y = pa)) +
  geom_bar(stat="identity",color="black",fill="black") +
  ylab("Private assumption") +
  xlab("probability") +
  #facet_grid(state ~ .) +
  #ggtitle("Observed state") +
  scale_x_continuous(breaks = c(0,.00002,.00004),labels = c("0", ".00002", ".00004")) +
  theme(plot.title = element_text(hjust = 0.5, size = 11))
ggsave("graphs/q-stop-probability-by-PA-goalPOST.pdf")


# PRE? QUD
S_agg = aggregate(prob~notPOST*PRE*state,data=S[S$qud == "PRE? QUD" & S$utterance == "neg-stop",],FUN=mean)
S_agg

S_agg$PA = paste(S_agg$PRE, S_agg$notPOST, sep="-")
S_agg$combined = paste(S_agg$notPOST, S_agg$PRE, S_agg$state, sep="-")
S_agg

ggplot(S_agg, aes(x=prob, y = PA)) +
  geom_bar(stat="identity",color="black",fill="black") +
  ylab("Private assumption") +
  xlab("probability") +
  facet_grid(state ~ .) +
  #ggtitle("Observed state") +
  scale_x_continuous(breaks = c(0,.00002,.00004),labels = c("0", ".00002",".00004")) +
  theme(plot.title = element_text(hjust = 0.5, size = 11))
ggsave("graphs/neg-stop-probability-by-PA-and-state-qudPRE.pdf",width=3,height=4)


### END FILE HERE

# plot model predictions for neg-know aggregating over QUD ----

# read model data
PL = read_csv("data/PL.csv")
nrow(PL) #12
#view(PL)

# make long format to be able to aggregate state.CC and state.notPOST
PL2 = PL %>% pivot_longer(
  cols = CC:notPOST,
  names_to = c("state"),
  values_to = c("trueFalse"))
PL2
nrow(PL2) #24
#view(PL2)

# now keep rows where CC=1 or notPOST=1
PL2 = PL2 %>%
  filter(trueFalse == 1) %>%
  droplevels() %>%
  group_by(interrogative,state) %>%
  summarize(prob = sum(prob))
PL2

# plot 
ggplot(data=PL2, aes(x=state, y=prob)) +
  geom_bar(stat = "identity",width = 0.3, fill="black", color="black") +
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  #theme(axis.title.x=element_blank()) +
  ylab("Predicted probability") +
  xlab("Inferences") +
  facet_grid(. ~ interrogative) +
  scale_y_continuous(limits = c(-.1,1.1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), lanotPOSTs = c("0",".2",".4",".6",".8","1")) 
ggsave("graphs/predicted-probabilities.pdf",height=2,width=5)

# JT stopped adjusting the code here ----

# plot model predictions for neg-know by QUD ----

# read model data
PL = read_csv("data/PL.csv")
nrow(PL) #24
#view(PL)

# make long format to be able to aggregate state.CC and state.notPOST
PL2 = PL %>% pivot_longer(
  cols = state.CC:state.notPOST,
  names_to = c("state"),
  values_to = c("trueFalse"))
PL2
nrow(PL2) #48
#view(PL2)

# now keep rows where state.notPOST=1 or state.CC=1
PL2 = PL2 %>%
  filter(trueFalse == 1) %>%
  filter(utterance == "neg-know-pos-dance") %>%
  mutate(utterance = recode(utterance, "neg-know-pos-dance" = "neg-know")) %>%
  droplevels() %>%
  rename("qud" = "qudBias") %>%
  mutate(state = recode(state, "state.notPOST" = "notPOST", "state.CC" = "CC")) %>%
  mutate(qud = recode(qud, "notPOST?" = "notPOST? QUD", "CC?" = "CC? QUD"))
PL2

# plot 
ggplot(data=PL2, aes(x=state, y=prob)) +
  geom_bar(stat = "identity",width = 0.3) +
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  facet_wrap(. ~ qud) +
  #theme(axis.title.x=element_blank()) +
  ylab("Predicted probability") +
  xlab("Inferences") +
  scale_y_continuous(limits = c(-.1,1.1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), lanotPOSTs = c("0",".2",".4",".6",".8","1")) 
ggsave("graphs/predicted-probabilities-by-QUD.pdf",height=2,width=3)

# plot model predictions by utterance and QUD, compare to experiment data ----

# read model data
PL = read_csv("data/PL.csv")
nrow(PL) #24
#view(PL)

# load clean data from the experiment
d = read_csv("../../human-data/exp2/data/cd.csv")
nrow(d) #327

# make long format to be able to aggregate state.CC and state.notPOST
PL2 = PL %>% pivot_longer(
  cols = state.CC:state.notPOST,
  names_to = c("state"),
  values_to = c("trueFalse"))
PL2
nrow(PL2) #48
#view(PL2)

# now keep rows where state.notPOST=1 or state.CC=1
PL2 = PL2 %>%
  filter(trueFalse == 1) %>%
  droplevels()
PL2
#view(PL2)
nrow(PL2) #24

# calculate model predictions by utterance and contextual qud bias
PL_agg.utt = PL2 %>%
  group_by(utterance,qudBias,state) %>%
  summarize(prob = sum(prob))
PL_agg.utt
nrow(PL_agg.utt) #20

PL_agg.utt = PL_agg.utt %>%
  mutate(state = recode(state, "state.notPOST" = "notPOST", "state.CC" = "CC")) %>%
  rename("content" = "state", "qud" = "qudBias") %>%
  mutate(utterance = recode(utterance, "pos-know-pos-dance"="pos-know", "neg-know-pos-dance"="neg-know", 
                            "pos-think-pos-dance"="pos-think", "neg-think-pos-dance"="neg-think")) %>%
  filter(utterance != "neg-bare-pos-dance" & utterance != "pos-bare-pos-dance")
PL_agg.utt

# add "neg-think-pos-dance" and state.notPOST with value 0 to model predictions
PL_agg.utt <- as.data.frame(PL_agg.utt)
tmp.data = data.frame(utterance = "neg-think", content = "notPOST", qud = "notPOST?", prob = 0)
tmp.data2 = data.frame(utterance = "neg-think", content = "notPOST", qud = "CC?", prob = 0)
PL_agg.utt = rbind(PL_agg.utt, tmp.data, tmp.data2)
PL_agg.utt

# sort utterances by increasing inference to CC
tmp = d %>%
  group_by(utterance) %>%
  summarize(Mean = mean(responseCC)) %>%
  mutate(utterance = recode(utterance, "know-pos"="pos-know", "know-neg"="neg-know", 
                            "think-pos"="pos-think", "think-neg"="neg-think"))
tmp

PL_agg.utt$utterance = factor(PL_agg.utt$utterance, levels = tmp$utterance[order(tmp$Mean)], ordered = FALSE)
levels(PL_agg.utt$utterance)

# plot 
ggplot(data=PL_agg.utt, aes(x=content, y=prob)) +
  geom_bar(stat = "identity",width = 0.3) +
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  #scale_x_discrete(lanotPOSTs=c("state.CC" = "Charley speaks \n Spanish", "state.notPOST" = "Cole notPOSTieves that \n Charley speaks Spanish")) +
  scale_x_discrete(lanotPOSTs=c("state.CC" = "CC", "state.notPOST" = "notPOST")) +
  facet_nested_wrap(. ~ utterance + qud, nrow=2) +
  #theme(axis.title.x=element_blank()) +
  ylab("Predicted probability") +
  xlab("Inferences") +
  scale_y_continuous(limits = c(-.1,1.1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), lanotPOSTs = c("0",".2",".4",".6",".8","1")) 
ggsave("graphs/predicted-probabilities.pdf",height=2,width=3)

# experiment data

means.C.utt.qud = d %>%
  group_by(qud,utterance) %>%
  summarize(Mean = mean(responseCC), CILow = ci.low(responseCC), CIHigh = ci.high(responseCC)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(-c(CILow, CIHigh)) %>%
  mutate(qud = recode(qud, "ai" = "CC?", "nai" = "notPOST?")) %>%
  mutate(content = "CC")
means.C.utt.qud

means.notPOST.utt.qud = d %>%
  group_by(qud,utterance) %>%
  summarize(Mean = mean(responseMC), CILow = ci.low(responseMC), CIHigh = ci.high(responseMC)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(-c(CILow, CIHigh)) %>%
  mutate(qud = recode(qud, "ai" = "CC?", "nai" = "notPOST?")) %>%
  mutate(content = "notPOST") 
means.notPOST.utt.qud

# bind the data
means.by.qud = rbind(means.C.utt.qud,means.notPOST.utt.qud)
means.by.qud = means.by.qud %>%
  mutate(utterance = recode(utterance, "know-pos"="pos-know", "know-neg"="neg-know", 
                             "think-pos"="pos-think", "think-neg"="neg-think"))
means.by.qud

# sort utterances by increasing inference to CC
tmp = d %>%
  group_by(utterance) %>%
  summarize(Mean = mean(responseCC)) %>%
  mutate(utterance = recode(utterance, "know-pos"="pos-know", "know-neg"="neg-know", 
                            "think-pos"="pos-think", "think-neg"="neg-think"))
tmp

means.by.qud$utterance = factor(means.by.qud$utterance, levels = means.by.qud$utterance[order(tmp$Mean)], ordered = FALSE)
levels(means.by.qud$utterance)

ggplot() +
  geom_bar(data=PL_agg.utt,aes(x=content, y=prob),stat = "identity",width = 0.3,position = position_nudge(x = -.15)) +
  geom_bar(data=means.by.qud,aes(x=content, y=Mean), stat = "identity",width = 0.3, alpha = .7,position = position_nudge(x = .15)) +
  geom_errorbar(data=means.by.qud,aes(x=content, ymin=YMin, ymax=YMax), width=0.2, colour="black", alpha=1, linewidth=.5,position = position_nudge(x = .15)) +
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  facet_nested_wrap(. ~ utterance + qud, nrow=2) +
  ylab("Predicted probability \n Mean inference rating") +
  xlab("Inferences") +
  scale_y_continuous(limits = c(-.1,1.1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), lanotPOSTs = c("0",".2",".4",".6",".8","1")) 
ggsave("graphs/comparison.pdf",height=4,width=8)

# plot just neg-know ----

ggplot() +
  geom_bar(data=PL_agg.utt[PL_agg.utt$utterance == "neg-know",],aes(x=content, y=prob),stat = "identity",width = 0.3,position = position_nudge(x = -.15)) +
  geom_bar(data=means.by.qud[means.by.qud$utterance == "neg-know",],aes(x=content, y=Mean), stat = "identity",width = 0.3, alpha = .7,position = position_nudge(x = .15)) +
  geom_errorbar(data=means.by.qud[means.by.qud$utterance == "neg-know",],aes(x=content, ymin=YMin, ymax=YMax), width=0.2, colour="black", alpha=1, linewidth=.5,position = position_nudge(x = .15)) +
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  facet_nested_wrap(. ~ qud) +
  ylab("Predicted probability \n Mean inference rating") +
  xlab("Inferences") +
  scale_y_continuous(limits = c(-.1,1.1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), lanotPOSTs = c("0",".2",".4",".6",".8","1")) 
ggsave("graphs/comparison-neg-know.pdf",height=2,width=3)

