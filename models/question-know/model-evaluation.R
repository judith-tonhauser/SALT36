# question model know

# load required libraries
library(tidyverse)
library(rwebppl)

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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
