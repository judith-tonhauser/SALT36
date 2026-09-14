# negation stop-model

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
