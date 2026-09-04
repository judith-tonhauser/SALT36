# SALT36 proceedings paper
# "stop"/"know" experiment
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
nrow(d) #1216 = 608 participants x 2 ratings

names(d)
table(d$utterance,d$qud,d$item)
length(unique(d$participantID)) #608

# Fig 2: omnibus plot ----
# fix x-axis labels for the quds (separately for stop and know)

means_qud_prior_environment = d %>%
  filter(response_type=="CC"|response_type=="PRE") %>%
  group_by(qud,verb,environment,prior) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh) %>%
  select(-c(CILow, CIHigh)) %>%
  mutate(qud2 = case_when(qud=="ai" & verb == "know" ~ "CC?",
                          qud=="nai" & verb == "know" ~ "BEL?",
                          qud=="ai" & verb == "stop" ~ "PRE?",
                          qud=="nai" & verb == "stop" ~ "notPOST?",
                          TRUE ~ "ERROR"))
means_qud_prior_environment

d = d %>%
  mutate(qud2 = case_when(qud=="ai" & verb == "know" ~ "CC?",
                          qud=="nai" & verb == "know" ~ "BEL?",
                          qud=="ai" & verb == "stop" ~ "PRE?",
                          qud=="nai" & verb == "stop" ~ "notPOST?",
                          TRUE ~ "ERROR"))

ggplot(data=means_qud_prior_environment,aes(x=qud2,y=Mean,fill=prior))+
  geom_violin(data=d[d$response_type=="CC"|d$response_type=="PRE",],aes(x=qud2,y=response),position=position_dodge(.8),color="darkgray") +
  geom_errorbar(aes(y=Mean, ymin=YMin, ymax=YMax), width=0.2, colour="black", alpha=1,position=position_dodge(.8)) +
  geom_point(position=position_dodge(.8),size=2) +
  scale_fill_discrete(type=c("#E69F00","#56B4E9")) +
  theme(legend.position="bottom") +
  ylab("Mean inference rating\n") +
  labs(x=NULL) +
  ylim(0,1)+
  facet_grid(environment ~ verb,scales="free_x")
ggsave("../graphs/omnibus.pdf",width=6,height=4)

