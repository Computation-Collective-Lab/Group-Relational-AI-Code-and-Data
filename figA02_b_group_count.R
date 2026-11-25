library(dplyr)
library(ggplot2)
conditions <- c('control','personal','relational-static')
pal <- setNames(c('#8b8b83','#1496BB','#E85125'), conditions)

d <- read.csv('./csv/RoomData.csv') %>%
  filter(Condition %in% conditions, GroupSize >= 1) %>%
  group_by(SessionID, Condition, RoundID) %>%
  summarise(groups = n_distinct(RoomID), .groups='drop') %>%
  group_by(SessionID) %>%
  mutate(delta = groups - groups[RoundID == 1][1]) %>% ungroup() %>%
  mutate(Condition=factor(Condition, levels=conditions), t=RoundID-1,
         individual=as.integer(Condition=='personal'), relational=as.integer(Condition=='relational-static'))

fit <- lm(delta ~ 0 + t + t:individual + t:relational, data=d)

print(summary(fit)); print(confint(fit))

print(ggplot(d,aes(RoundID,delta,colour=Condition)) +
  geom_hline(yintercept=0,linetype=2) +
  stat_summary(fun=mean,geom='line',position=position_dodge(.2)) +
  stat_summary(fun=mean,geom='point',position=position_dodge(.2)) +
  stat_summary(fun.data=mean_se,geom='errorbar',width=0,position=position_dodge(.2)) +
  scale_colour_manual(values=pal,labels=c('None','Individual','Relational')) +
  scale_x_continuous(breaks=1:5) + labs(x='Round',y='Δ Number of groups',colour='Message suggestions:') +
  theme_classic() + theme(legend.position='bottom'))
