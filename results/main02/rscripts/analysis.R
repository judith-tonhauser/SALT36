# SALT36 proceedings paper
# "stop"/"know" experiment
# analysis

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load required packages
library(tidyverse)
library(lme4)
library(lmerTest)
library(forcats)
library(emmeans)

# load helper functions
source('../../../helpers.R')

# load data
d = read_csv("../data/cd.csv")
nrow(d) #1216

table(d$qud) # use this qud coding in the analysis

# reduce data to CC and PRE, set reference levels
d2 = d %>%
  filter(response_type=="CC" | response_type=="PRE") %>%
  mutate(qud = fct_relevel(qud, "ai"), 
         verb = fct_relevel(verb, "know"), 
         environment = fct_relevel(environment, "question"),
         prior = fct_relevel(prior, "lower"))

nrow(d2) #608

# fit a model with four-way interaction ----
m = lmer(response ~ verb*prior*environment*qud + (1|item),data=d2)
summary(m)
# reference level: know, question, ai, lower
# simple effects: 
# priorhigher: CC is more projective in higher than lower (in questions, when ai)
# environmentnegation: CC is more projective in negation than question (when ai, at lower)

# save the model
saveRDS(m, file = "lme-model.rds")

# pairwise comparison for each predictor while holding the other one constant ----

# read model
m <- readRDS("lme-model.rds")
m

emmeansOutput <- pairs(emmeans(m, ~ verb*prior*environment*qud), simple = "each")
emmeansOutput

# output the emmeansResults to latex ----
library(xtable)

combined_table <- summary(emmeansOutput, combine = TRUE)
combined_table

emmeansOutputTable <- as.data.frame(combined_table)
emmeansOutputTable

# remove df column
names(emmeansOutputTable)
emmeansOutputTable = emmeansOutputTable %>%
  select(-c(df))

# produce latex document

cat("\\documentclass{article}\n\\begin{document}\n", file = "../../../Appendix2/emmeansOutputTable.tex")

write(print(xtable(emmeansOutputTable, caption = "Simple Effects Analysis"), 
      tabular.environment="tabular",
      floating=FALSE,
      latex.environments=NULL,
      include.rownames=FALSE,
      booktabs=FALSE), append=TRUE,
            file = "../../../Appendix2/emmeansOutputTable.tex")
            
cat("\\end{document}\n", file = "../../../Appendix2/emmeansOutputTable.tex", append = TRUE)

      