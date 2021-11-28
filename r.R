library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)

jobs <- read.csv("jobs.csv",encoding="UTF-8", stringsAsFactors=FALSE)

jobs$cat <- word(jobs$cat, 1)
jobs$min <- as.numeric(word(jobs$salary,1))
jobs$max <- as.numeric(word(jobs$salary,-2))
jobs$age_min <- as.numeric(word(jobs$age,1))
jobs$age_max <- as.numeric(word(jobs$age,-2))

jobs <- filter(jobs,!is.na(age_min))

View(jobs)


cr1 <- group_by(jobs,cat) %>% summarise(age=mean(age_min))
cr2 <- group_by(jobs,cat) %>% summarise(age=mean(age_max))
cr <- rbind(cr1,cr2)

View(cr)
o <- function(x) {
  subset(x, x == max(x) | x == min(x))
}

f <- function(x) {
  r <- quantile(x, probs = c(0, 25, 50, 75, 100))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}
ggplot(cr, aes(x=cat, y=age)) + 
  stat_summary(fun.data=f, geom="boxplot") + 
  stat_summary(fun.y = o, geom="point") +
  stat_boxplot(geom='errorbar',coef=10)+
  geom_text(aes(label=cat), position=position_dodge(width=0.2), vjust=-0.50)+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())

# ggplot(data=cr, mapping=aes(x=cat,y=max_salary,fill=cat))+
#   geom_bar(position = 'dodge', stat='identity') +
#   geom_text(aes(label=cat), position=position_dodge(width=0.2), vjust=-0.50)+
# 
#   theme(axis.text.x=element_blank(),
#         axis.ticks.x=element_blank())

# ggplot(data=jobs, aes(x=cat,fill=cat))+
#   geom_bar() +
#   geom_text(aes(label=cat), stat='count',position=position_dodge(width=0.2), vjust=-0.50)+
#   theme(axis.text.x=element_blank(),
#         axis.ticks.x=element_blank())
