# negation model for know (Section 3.1)

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
utterances = c("pos-know-pos-dance", 
               "pos-think-pos-dance",
               "neg-know-pos-dance", 
               "neg-think-pos-dance",
               "pos-bare-pos-dance",
               "neg-bare-pos-dance")
utterances

# define the qudBias (from context)
qudBias = c("BEL?", "CC?")
qudBias

# define priors
priors = c("higher", "lower")
priors

# pragmatic listener ----

# input to PL: utterance, qudBias

#### call PL ----

PL = data.frame(utterance = character(), qudBias = character(), prior = character(),
                state.CC = numeric(), state.BEL = numeric(), prob = numeric())
PL

for (u in utterances) {
  print(u)
  for (q in qudBias) {
    print(q)
    for (p in priors) {
      print(p)
      PL_tmp = eval_webppl(paste("pragmaticListener('",u,"','",q,"','",p,"')",sep=""))
      for (i in 1:nrow(PL_tmp)) {
        PL = PL %>% 
          add_row(utterance = u, qudBias = q, prior = p,
                  state.CC = PL_tmp$CC[i],
                  state.BEL = PL_tmp$BEL[i],
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

# make long format to be able to aggregate state.CC and state.BEL
PL2 = PL %>% pivot_longer(
  cols = state.CC:state.BEL,
  names_to = c("state"),
  values_to = c("trueFalse")) 
PL2
nrow(PL2) #96
#view(PL2)

# keep rows where CC=1 or BEL=1, sum up identical conditions 
# (created by making long format), take mean across prior and qudBias
means = PL2 %>%
  filter(trueFalse == 1) %>%
  select(-c(trueFalse)) %>%
  droplevels() %>%
  group_by(utterance,qudBias,state,prior) %>%
  summarize(prior.sum = sum(prob)) %>%
  group_by(utterance,state) %>%
  summarize(prob = mean(prior.sum))
means
# 3 neg-know-pos-dance  state.CC  0.845

# keep rows where CC=1 or BEL=1, sum up identical conditions 
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
# 4 neg-know-pos-dance  higher state.CC  0.921 
# 6 neg-know-pos-dance  lower  state.CC  0.768 

# keep rows where CC=1 or BEL=1, sum up identical conditions 
# (created by making long format), here do not sum across qudBias!
means.byPrior.byQudBias = PL2 %>%
  filter(trueFalse == 1) %>%
  select(-c(trueFalse)) %>%
  droplevels() %>%
  group_by(utterance,qudBias,state,prior) %>%
  summarize(prior.sum = sum(prob)) %>%
  group_by(utterance,prior,qudBias,state) %>%
  summarize(prob = mean(prior.sum))
means.byPrior.byQudBias
#6 neg-know-pos-dance  higher BEL?    state.CC  0.981  
#10 neg-know-pos-dance  lower  BEL?    state.CC  0.928  

#8 neg-know-pos-dance  higher CC?     state.CC  0.861 
#12 neg-know-pos-dance  lower  CC?     state.CC  0.608
