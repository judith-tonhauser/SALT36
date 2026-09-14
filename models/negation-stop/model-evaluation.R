# negation stop-model

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
utterances = c("pos-stop", 
               "neg-stop",
               "pos-pre",
               "neg-pre",
               "neg-post",
               "neg-neg-post")
utterances

# define the qudBias (from context)
qudBias = c("PRE?", "POST?")
qudBias

# define priors
priors = c("higher", "lower")
priors

# define the world states
states = c("{PRE: 1, notPOST: 1}",
           "{PRE: 0, notPOST: 1}",
           "{PRE: 1, notPOST: 0}",
           "{PRE: 0, notPOST: 0}")
states

# define the plausible private assumptions
cgs = c("[{PRE: 1, notPOST: 1}, {PRE: 0, notPOST: 1}, {PRE: 1, notPOST: 0}, {PRE: 0, notPOST: 0}]",
        "[{PRE: 1, notPOST: 1}, {PRE: 1, notPOST: 0}]", # notPOST:1
        "[{PRE: 1, notPOST: 1}, {PRE: 0, notPOST: 1}]", # PRE:1
        "[{PRE: 0, notPOST: 1}, {PRE: 0, notPOST: 0}]", # notPOST:0
        "[{PRE: 1, notPOST: 0}, {PRE: 0, notPOST: 0}]" # PRE:0
)
cgs

#define the quds
quds = c("PRE?","POST?")
quds

# pragmatic listener ----

# input to PL: utterance, qudBias, prior

#### call PL ----

PL = data.frame(utterance = character(), qudBias = character(), prior = character(),
                state.PRE = numeric(), state.notPOST = numeric(), prob = numeric())
PL

for (u in utterances) {
  print(u)
  for (q in qudBias) {
    print(q)
    for (p in priors) {
      PL_tmp = eval_webppl(paste("pragmaticListener('",u,"','",q,"','",p,"')",sep=""))
      for (i in 1:nrow(PL_tmp)) {
        PL = PL %>% 
          add_row(utterance = u, qudBias = q, prior = p,
                  state.PRE = PL_tmp$PRE[i],
                  state.notPOST = PL_tmp$notPOST[i],
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
  cols = state.PRE:state.notPOST,
  names_to = c("state"),
  values_to = c("trueFalse"))
PL2
nrow(PL2) #96
#view(PL2)

# keep rows where state.PRE=1 or state.notPOST=1, sum up identical conditions 
# (created by making long format), take mean across priors and qudBias
means = PL2 %>%
  filter(trueFalse == 1) %>%
  select(-c(trueFalse)) %>%
  droplevels() %>%
  group_by(utterance,qudBias,state,prior) %>%
  summarize(prior.sum = sum(prob)) %>%
  group_by(utterance,state) %>%
  summarize(prob = mean(prior.sum))
means
# 5 neg-stop     state.PRE     0.869

# keep rows where state.PRE=1 or state.notPOST=1, sum up identical conditions 
# (created by making long format), take mean across qudBias
means.byPrior = PL2 %>%
  filter(trueFalse == 1) %>%
  select(-c(trueFalse)) %>%
  droplevels() %>%
  group_by(utterance,qudBias,state,prior) %>%
  summarize(prior.sum = sum(prob)) %>%
  group_by(utterance,prior,state) %>%
  summarize(prob = mean(prior.sum))
means.byPrior
# 9 neg-stop     higher state.PRE     0.900 
# 11 neg-stop     lower  state.PRE     0.838 


# run the speaker ----

# input to speaker: state, cg, qud
# we only input those state/cg combinations that are compatible with one another

# create data frame of all possible state/cg combinations
tmp = data.frame(state = character(), cg = character())
tmp

#produce all combinations of s and cg
for (s in states) {
  for (cg in cgs) {
    tmp = tmp %>% 
      add_row(state = s, cg = cg)
  }
}
tmp

# filter out those rows where the state is not in the cg
stateCG = tmp %>%
  filter(mapply(grepl, tmp$state, tmp$cg, fixed=TRUE))
#view(stateCG)
stateCG # these are the 12 combinations shown in the tables

#### call the speaker ----

#(state, cg, qud)

S = data.frame(state = character(), commonGround = character(), qud = character(), utterance = character(), prob = numeric())
S

for (n in 1:nrow(stateCG)) {
  print(stateCG$state[[n]])
  print(stateCG$cg[[n]])
  for (q in quds) {
    S_tmp = eval_webppl(paste("speaker(",stateCG$state[[n]],",",stateCG$cg[[n]],",'",q,"')",sep=""))
    for (i in 1:nrow(S_tmp)) {
      S = S %>% 
        add_row(state = stateCG$state[[n]], commonGround = stateCG$cg[[n]], qud = q, utterance = S_tmp$support[i], prob = S_tmp$prob[i])
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

# create a more compact state column for plotting
table(S$state)
S$state = gsub("\\{","",S$state)
S$state = gsub("\\}","",S$state)
S$state = gsub(" ", "",S$state)
table(S$state)

# code the commonGround column into private assumptions  
S <- S %>% 
  mutate(PRE = case_when(!grepl("PRE: 0", commonGround) ~ "PRE:1",
                        !grepl("PRE: 1", commonGround) ~ "PRE:0",
                        TRUE ~ "PRE:?"))
S <- S %>%
  mutate(notPOST = case_when(!grepl("notPOST: 0", commonGround) ~ "notPOST:1",
                         !grepl("notPOST: 1", commonGround) ~ "notPOST:0",
                         TRUE ~ "notPOST:?"))
table(S$notPOST,S$PRE)

S$pa = paste(S$notPOST,S$PRE,sep=" ")
table(S$pa)

# recode the utterance names
table(S$utterance)
# S = S %>%
#   mutate(utterance = recode(utterance, "neg-bare-pos-dance" = "not c", "pos-bare-pos-dance" = "c",
#                             "pos-know-pos-dance" = "pos-know",
#                             "neg-know-pos-dance" = "neg-know",
#                             "pos-think-pos-dance" = "pos-think",
#                             "neg-think-pos-dance" = "neg-think"))

# recode the QUD names
table(S$qud)
S = S %>%
  mutate(qud = recode(qud, "PRE?" = "PRE? QUD", "POST?" = "POST? QUD"))

# save cleaned up speaker data
write_csv(S, file="data/S.csv")
S

# speaker plots ----

# read cleaned up speaker
S = read_csv(file="data/S.csv")

# plot probability of utterance by state and QUD
S_agg = aggregate(prob~state*qud*utterance*pa,data=S,FUN=mean)
S_agg

# plot utterance probability with PA = worldState

ggplot(S_agg[S_agg$pa == "notPOST:? PRE:?",], aes(x=utterance, y=prob)) +
  geom_bar(stat="identity") +
  facet_grid(qud~state, scales = "free_y") +
  ylab("probability") +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  coord_flip()
ggsave("graphs/utterance-probability-by-state-and-QUD-worldState.pdf",width=15,height=4)

# plot utterance probability with PA = CC:1

ggplot(S_agg[S_agg$pa == "notPOST:? PRE:1",], aes(x=utterance, y=prob)) +
  geom_bar(stat="identity") +
  facet_grid(qud~state, scales = "free_y") +
  ylab("probability") +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  coord_flip()
ggsave("graphs/utterance-probability-by-state-and-QUD-CC1.pdf",width=7,height=4)


#### plot probability of neg-stop by PA, state and QUD ----

# read cleaned up speaker
S = read_csv(file="data/S.csv")

# notPOST? QUD
S_agg = aggregate(prob~notPOST*PRE*state,data=S[S$qud == "POST? QUD" & S$utterance == "neg-stop",],FUN=mean)
S_agg

S_agg$PA = paste(S_agg$PRE, S_agg$notPOST, sep="-")
S_agg$combined = paste(S_agg$PRE, S_agg$notPOST, S_agg$state, sep="-")
S_agg
S_agg = S_agg %>%
  select(c(PA,state,prob))

ggplot(S_agg, aes(x=prob, y = PA)) +
  geom_bar(stat="identity",color="black",fill="black") +
  ylab("Private assumption") +
  xlab("probability") +
  facet_grid(state ~ .) +
  #ggtitle("Observed state") +
  scale_x_continuous(breaks = c(0,.02,.04),labels = c("0", ".02", ".04")) +
  theme(plot.title = element_text(hjust = 0.5, size = 11))
ggsave("graphs/neg-stop-probability-by-PA-and-state-qudPOST.pdf",width=3,height=4)


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

# plot model predictions by utterance and QUD ----

# read model data
PL = read_csv("data/PL.csv")
nrow(PL) #72
#view(PL)

# make long format to be able to aggregate state.CC and state.BEL
PL2 = PL %>% pivot_longer(
  cols = state.PRE:state.notPOST,
  names_to = c("state"),
  values_to = c("trueFalse"))
PL2
nrow(PL2) #192
#view(PL2)

# now keep rows where state.PRE=1 or state.notPOST=1
PL2 = PL2 %>%
  filter(trueFalse == 1) %>%
  droplevels()
PL2
#view(PL2)
nrow(PL2) #96

# calculate model predictions by utterance and contextual qud bias
PL_agg.utt = PL2 %>%
  group_by(utterance,qudBias,state) %>%
  summarize(prob = sum(prob))
PL_agg.utt
nrow(PL_agg.utt) #20

PL_agg.utt = PL_agg.utt %>%
  mutate(state = recode(state, "state.PRE" = "PRE", "state.notPOST" = "notPOST")) %>%
  rename("content" = "state", "qud" = "qudBias")
  #mutate(utterance = recode(utterance, "pos-know-pos-dance"="pos-know", "neg-know-pos-dance"="neg-know", 
  #                          "pos-think-pos-dance"="pos-think", "neg-think-pos-dance"="neg-think")) %>%
  #filter(utterance != "neg-bare-pos-dance" & utterance != "pos-bare-pos-dance")
PL_agg.utt

# add "neg-neg-post" and content=notPOST with value 0 to model predictions
PL_agg.utt <- as.data.frame(PL_agg.utt)
tmp.data = data.frame(utterance = "neg-neg-post", content = "notPOST", qud = "POST?", prob = 0)
tmp.data2 = data.frame(utterance = "neg-neg-post", content = "notPOST", qud = "PRE?", prob = 0)
PL_agg.utt = rbind(PL_agg.utt, tmp.data, tmp.data2)
PL_agg.utt

# add "neg-pre" and content=PRE with value 0 to model predictions
PL_agg.utt <- as.data.frame(PL_agg.utt)
tmp.data = data.frame(utterance = "neg-pre", content = "PRE", qud = "POST?", prob = 0)
tmp.data2 = data.frame(utterance = "neg-pre", content = "PRE", qud = "PRE?", prob = 0)
PL_agg.utt = rbind(PL_agg.utt, tmp.data, tmp.data2)
PL_agg.utt

# plot 
ggplot(data=PL_agg.utt, aes(x=content, y=prob)) +
  geom_bar(stat = "identity",width = 0.3) +
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  #scale_x_discrete(labels=c("state.CC" = "CC", "state.BEL" = "BEL")) +
  facet_nested_wrap(. ~ utterance + qud, nrow=2) +
  #theme(axis.title.x=element_blank()) +
  ylab("Predicted probability") +
  xlab("Inferences") +
  scale_y_continuous(limits = c(-.1,1.1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) 
ggsave("graphs/predicted-probabilities-all-utterances.pdf",height=2,width=3)

# plot of just neg-stop
ggplot() +
  geom_bar(data=PL_agg.utt[PL_agg.utt$utterance == "neg-stop",],aes(x=content, y=prob),stat = "identity",width = 0.3) +
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  facet_nested_wrap(. ~ qud) +
  ylab("Predicted probability \n Mean inference rating") +
  xlab("Inferences") +
  scale_y_continuous(limits = c(-.1,1.1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) 
ggsave("graphs/predicted-probabilities-neg-stop.pdf",height=2,width=3)

# plot of just neg-stop with POST? QUD
ggplot() +
  geom_bar(data=PL_agg.utt[PL_agg.utt$utterance == "neg-stop" & PL_agg.utt$qud=="POST?",],aes(x=content, y=prob),stat = "identity",width = 0.3) +
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  #facet_nested_wrap(. ~ qud) +
  ylab("Predicted probability") +
  xlab("Inferences") +
  scale_y_continuous(limits = c(-.1,1.1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) 
ggsave("graphs/predicted-probabilities-neg-stop-POST.pdf",height=2,width=2)

# plot model predictions by utterance and QUD, compare to experiment data ----

# read model data
PL = read_csv("data/PL.csv")
nrow(PL) #24
#view(PL)

# load clean data from the experiment
d = read_csv("../../human-data/data/cd.csv")
nrow(d) #327

# make long format to be able to aggregate state.CC and state.BEL
PL2 = PL %>% pivot_longer(
  cols = state.CC:state.BEL,
  names_to = c("state"),
  values_to = c("trueFalse"))
PL2
nrow(PL2) #48
#view(PL2)

# now keep rows where state.BEL=1 or state.CC=1
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
  mutate(state = recode(state, "state.BEL" = "BEL", "state.CC" = "CC")) %>%
  rename("content" = "state", "qud" = "qudBias") %>%
  mutate(utterance = recode(utterance, "pos-know-pos-dance"="pos-know", "neg-know-pos-dance"="neg-know", 
                            "pos-think-pos-dance"="pos-think", "neg-think-pos-dance"="neg-think")) %>%
  filter(utterance != "neg-bare-pos-dance" & utterance != "pos-bare-pos-dance")
PL_agg.utt

# add "neg-think-pos-dance" and state.BEL with value 0 to model predictions
PL_agg.utt <- as.data.frame(PL_agg.utt)
tmp.data = data.frame(utterance = "neg-think", content = "BEL", qud = "BEL?", prob = 0)
tmp.data2 = data.frame(utterance = "neg-think", content = "BEL", qud = "CC?", prob = 0)
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
  #scale_x_discrete(labels=c("state.CC" = "Charley speaks \n Spanish", "state.BEL" = "Cole believes that \n Charley speaks Spanish")) +
  scale_x_discrete(labels=c("state.CC" = "CC", "state.BEL" = "BEL")) +
  facet_nested_wrap(. ~ utterance + qud, nrow=2) +
  #theme(axis.title.x=element_blank()) +
  ylab("Predicted probability") +
  xlab("Inferences") +
  scale_y_continuous(limits = c(-.1,1.1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) 
ggsave("graphs/predicted-probabilities.pdf",height=2,width=3)

# experiment data

means.C.utt.qud = d %>%
  group_by(qud,utterance) %>%
  summarize(Mean = mean(responseCC), CILow = ci.low(responseCC), CIHigh = ci.high(responseCC)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(-c(CILow, CIHigh)) %>%
  mutate(qud = recode(qud, "ai" = "CC?", "nai" = "BEL?")) %>%
  mutate(content = "CC")
means.C.utt.qud

means.BEL.utt.qud = d %>%
  group_by(qud,utterance) %>%
  summarize(Mean = mean(responseMC), CILow = ci.low(responseMC), CIHigh = ci.high(responseMC)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(-c(CILow, CIHigh)) %>%
  mutate(qud = recode(qud, "ai" = "CC?", "nai" = "BEL?")) %>%
  mutate(content = "BEL") 
means.BEL.utt.qud

# bind the data
means.by.qud = rbind(means.C.utt.qud,means.BEL.utt.qud)
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
  scale_y_continuous(limits = c(-.1,1.1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) 
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
  scale_y_continuous(limits = c(-.1,1.1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) 
ggsave("graphs/comparison-neg-know.pdf",height=2,width=3)

