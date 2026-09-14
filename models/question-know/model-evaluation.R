# question model know

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
interrogatives = c("does-cole-know?", 
               "does-cole-think?",
               "does-charley-speak?")
interrogatives

# define the goals
goals = c("BEL","CC")
goals

# define priors
priors = c("higher", "lower")
priors

# pragmatic listener ----

# input to lookupState: interrogative,goalBias,prior

#### call PL ----

PL = data.frame(interrogative = character(), qudBias = character(), prior = character(),
                CC = numeric(), BEL = numeric(), prob = numeric())
PL

for (q in interrogatives) {
  for (g in goals) {
    for (p in priors) {
    PL_tmp = eval_webppl(paste("lookupState('",q,"','",g,"','",p,"')",sep=""))
    for (i in 1:nrow(PL_tmp)) {
    PL = PL %>% 
      add_row(interrogative = q, qudBias = g, prior = p, 
              CC = PL_tmp$CC[i],
              BEL = PL_tmp$BEL[i],
              prob = PL_tmp$prob[i])
    }
    }
  }
}

PL
nrow(PL) #48

write_csv(PL, file="data/PL.csv")

#### predictions reported in (9) in SALT paper ----

# read model data
PL = read_csv("data/PL.csv")
nrow(PL) #48
#view(PL)

# make long format to be able to aggregate state.CC and state.BEL
PL2 = PL %>% pivot_longer(
  cols = CC:BEL,
  names_to = c("state"),
  values_to = c("trueFalse")) 
PL2
nrow(PL2) #96
#view(PL2)

# keep rows where CC=1 or BEL=1, sum up identical conditions 
# (created by making long format), take mean across prior and qudBias
means = PL2 %>%
  #filter(interrogative == "does-cole-know?" & state == "CC") %>%
  filter(trueFalse == 1) %>%
  select(-c(trueFalse)) %>%
  droplevels() %>%
  group_by(interrogative,qudBias,state,prior) %>%
  summarize(prior.sum = sum(prob)) %>%
  group_by(interrogative,state) %>%
  summarize(prob = mean(prior.sum))
means
# 4 does-cole-know?     CC    0.912

# keep rows where CC=1 or BEL=1, sum up identical conditions 
# (created by making long format), take mean across qudBias
means.byPrior = PL2 %>%
  #filter(interrogative == "does-cole-know?" & state == "CC") %>%
  filter(trueFalse == 1) %>%
  select(-c(trueFalse)) %>%
  droplevels() %>%
  group_by(interrogative,qudBias,state,prior) %>%
  summarize(prior.sum = sum(prob)) %>%
  group_by(interrogative,prior,state) %>%
  summarize(prob = mean(prior.sum))
means.byPrior
# 6 does-cole-know?     higher CC    0.955
# 8 does-cole-know?     lower  CC    0.869


# speaker ----

# define the PAs
pas = c("[{BEL: 1, CC: 1}, {BEL: 0, CC: 1}, {BEL: 1, CC: 0}, {BEL: 0, CC: 0}]",
        "[{BEL: 1, CC: 1}, {BEL: 1, CC: 0}]", # BEL:1
        "[{BEL: 1, CC: 1}, {BEL: 0, CC: 1}]", # CC:1
        "[{BEL: 0, CC: 1}, {BEL: 0, CC: 0}]", # BEL:0
        "[{BEL: 1, CC: 0}, {BEL: 0, CC: 0}]" # CC:0
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

worldState <- "[{BEL: 1, CC: 1}, {BEL: 0, CC: 1}, {BEL: 1, CC: 0}, {BEL: 0, CC: 0}]"
worldState

ggplot(S_agg[S_agg$pa == worldState,], aes(x=utterance, y=prob)) +
  geom_bar(stat="identity") +
  facet_grid(. ~ goal, scales = "free_y") +
  ylab("probability") +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  coord_flip()
ggsave("graphs/utterance-probability-by-state-and-QUD-paworldState.pdf",width=4,height=3)

# plot utterance probability with PA = CC:1

CC1 <- "[{BEL: 1, CC: 1}, {BEL: 0, CC: 1}]"
CC1

ggplot(S_agg[S_agg$pa == CC1,], aes(x=utterance, y=prob)) +
  geom_bar(stat="identity") +
  facet_grid(. ~ goal, scales = "free_y") +
  ylab("probability") +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1"), limits = c(0,1)) +
  coord_flip()
ggsave("graphs/utterance-probability-by-state-and-QUD-paCC1.pdf",width=4,height=3)


### END FILE HERE ----



#### plot probability of does-know by PA and goal ----

# read cleaned up speaker
S = read_csv(file="data/S.csv")

# BEL goal
S_agg = aggregate(prob~BEL*CC,data=S[S$goal == "BEL" & S$utterance == "does-cole-know?",],FUN=mean)
S_agg

S_agg$PA = paste(S_agg$CC, S_agg$BEL, sep="-")
S_agg$combined = paste(S_agg$CC, S_agg$BEL, S_agg$state, sep="-")
S_agg

ggplot(S_agg, aes(x=prob, y = PA)) +
  geom_bar(stat="identity",color="black",fill="black") +
  ylab("Private assumption") +
  xlab("probability") +
  scale_x_continuous(breaks = c(0,.02,.04),labels = c("0", ".02", ".04")) +
  theme(plot.title = element_text(hjust = 0.5, size = 11))
ggsave("graphs/does-know-probability-by-PA-goalBEL.pdf",width=3,height=4)

# CC goal
S_agg = aggregate(prob~BEL*CC,data=S[S$goal == "CC" & S$utterance == "does-cole-know?",],FUN=mean)
S_agg

S_agg$PA = paste(S_agg$CC, S_agg$BEL, sep="-")
S_agg$combined = paste(S_agg$CC, S_agg$BEL, S_agg$state, sep="-")
S_agg

ggplot(S_agg, aes(x=prob, y = PA)) +
  geom_bar(stat="identity",color="black",fill="black") +
  ylab("Private assumption") +
  xlab("probability") +
  scale_x_continuous(breaks = c(0,.00002,.00004),labels = c("0", ".00002", ".00004")) +
  theme(plot.title = element_text(hjust = 0.5, size = 11))
ggsave("graphs/does-know-probability-by-PA-goalCC.pdf",width=3,height=4)


# plot model predictions for neg-know aggregating over QUD ----

# read model data
PL = read_csv("data/PL.csv")
nrow(PL) #12
#view(PL)

# make long format to be able to aggregate state.CC and state.BEL
PL2 = PL %>% pivot_longer(
  cols = CC:BEL,
  names_to = c("state"),
  values_to = c("trueFalse"))
PL2
nrow(PL2) #24
#view(PL2)

# now keep rows where CC=1 or BEL=1
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
  scale_y_continuous(limits = c(-.1,1.1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) 
ggsave("graphs/predicted-probabilities.pdf",height=2,width=5)

