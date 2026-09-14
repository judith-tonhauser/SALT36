# question model "stop"

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
