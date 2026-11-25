library(dplyr)
library(ggplot2)
keys <- c('SessionID','Condition','ParticipantID')
d <- read.csv('./csv/ParticipantData.csv') %>% 
  filter(Condition %in% c('personal','relational-static'))
a <- d %>% 
  filter(RoundID==1) %>% transmute(SessionID,Condition,ParticipantID,
  rate=ifelse(TotalMessageCount>0,
              (TotalCopiedSuggestion+TotalEditedSuggestion)/TotalMessageCount,NA_real_), initial=OpinionDifference)

b <- d %>% 
  filter(RoundID==5) %>% select(all_of(keys), final=OpinionDifference)

stopifnot(!anyDuplicated(a[keys]),!anyDuplicated(b[keys]))
p <- inner_join(a,b,by=keys) %>% 
  mutate(delta=final-initial,Condition=factor(Condition,levels=c('personal','relational-static'),
                                              labels=c('Individual assistance','Relational assistance')))
print(p %>% count(Condition))
q <- p %>% filter(is.finite(rate),is.finite(delta))
stats <- q %>% 
  group_by(Condition) %>% summarise(n=n(),r=cor(rate,delta,method='spearman'),
                                    p=cor.test(rate,delta,method='spearman',exact=FALSE)$p.value,.groups='drop')
print(stats)

print(ggplot(q,aes(rate,delta,colour=Condition)) + 
        geom_hline(yintercept=0,linetype=2,colour='grey60') + 
        geom_point(alpha=.65,size=1.5) + facet_wrap(~Condition) +
        geom_text(data=stats,aes(x=.05,y=Inf,label=sprintf('r = %.3f',r)),inherit.aes=FALSE,hjust=0,vjust=1.5) +
 scale_colour_manual(values=c('#1496BB','#E85125')) + scale_x_continuous(breaks=seq(0,1,.2)) + 
   labs(x='AI suggestion use rate',y='Change in egocentric stance distance') + 
   theme_classic() + theme(legend.position='none'))
