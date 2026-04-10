# "stop" experiment
# analysis

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load required packages
library(tidyverse)
library(lme4)
library(lmerTest)
library(forcats)

# load helper functions
source('../../../helpers.R')

# Is PAST more projective than notNOW in neg-stop utterances? ----

# load data
d = read_csv("../data/cd.csv")
nrow(d) #20

# set reference levels
d = d %>%
  mutate(qud = fct_relevel(qud, "PAST?"), 
         responseTo = fct_relevel(responseTo, "notNOW"))

m = lmer(rating ~ responseTo*qud + (1|item) + (1|participantID), data=d[d$utterance == "stop-neg",])
summary(m)
# responseToPAST           0.86843    0.02239 187.00043  38.782  < 2e-16 ***
# qudNOW?                 -0.01072    0.02313 187.89498  -0.463 0.643610    
# responseToPAST:qudNOW?   0.03801    0.03271 187.00043   1.162 0.246622  

# Is notNOW more projective in "neg-stop" than "there's no way not"? ----

# load data
d = read_csv("../data/cd.csv")
nrow(d) #500

# transform the data
d = d %>%
  select(-c(responseNOW)) %>%
  gather(responseTo, rating, responsePAST:responseNotNOW) %>%
  mutate(responseTo = recode(responseTo, "responsePAST" = "PAST", "responseNotNOW" = "notNOW"))
nrow(d) #1000
#view(d)

# set reference levels
d = d %>%
  mutate(qud = fct_relevel(qud, "PAST?"), 
         utterance = fct_relevel(utterance, "not-not-now"),
         responseTo = fct_relevel(responseTo, "notNOW"))

m = lmer(rating ~ utterance*qud + (1|item), data=d[d$utterance == "stop-neg" | d$utterance == "not-not-now",])
summary(m)
# utterancestop-neg          -0.021028   0.063020 346.000000  -0.334    0.739    
# qudNOW?                     0.013703   0.067653 346.000000   0.203    0.840    
# utterancestop-neg:qudNOW?  -0.005364   0.091393 346.000000  -0.059    0.953 

# predict projection of PAST and notNOW from QUD for neg-stop ----

# load data
d = read_csv("../data/cd.csv")
nrow(d) #500

# set reference levels
d = d %>%
  mutate(qud = fct_relevel(qud, "PAST?"))

m = lmer(responsePAST ~ qud + (1|item), data=d[d$utterance == "stop-neg",])
summary(m)
# qudNOW?      0.02455    0.02092 93.71103   1.174  0.24353 
# no QUD-sensitivity for PAST

m = lmer(responseNotNOW ~ qud + (1|item), data=d[d$utterance == "stop-neg",])
summary(m)
# qudNOW?     -0.01067    0.02505 94.00000  -0.426    0.671
# no QUD-sensitivity for notNOW


