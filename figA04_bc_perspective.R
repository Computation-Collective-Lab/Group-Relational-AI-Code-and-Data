library(dplyr)
library(tidyr)
library(ggplot2)

conditions <- c('control','personal','relational-static')
d <- read.csv('./csv/PerspectiveScores.csv') %>% 
  filter(Condition %in% conditions,RoundID %in% 1:4) %>%
  pivot_longer(c(TOXICITY,RESPECT_EXPERIMENTAL),names_to='metric',values_to='score')

units <- bind_rows(mutate(d,source='All'),mutate(filter(d,tolower(MessageType)=='human'),source='Self-written')) %>%
  filter(!(Condition=='control' & source=='Self-written'),is.finite(score)) %>%
  group_by(GameID,PID,Condition,metric,source) %>% summarise(score=mean(score),.groups='drop')

for (metric_id in c('TOXICITY','RESPECT_EXPERIMENTAL')) {
  u <- filter(units,metric==metric_id)
  baseline <- filter(u,Condition=='control',source=='All')$score
  p <- u %>% 
    group_by(Condition,source) %>% summarise(mean=mean(score),se=sd(score)/sqrt(n()),n=n(),
      p=if(first(Condition)=='control') NA_real_ else t.test(score,baseline,var.equal=TRUE)$p.value,.groups='drop') %>%
    mutate(x=match(Condition,conditions)+ifelse(Condition=='control',0,ifelse(source=='All',-.2,.2)),
           fill=case_when(Condition=='control'|source=='Self-written'~'Self-written',
                          Condition=='personal'~'AI-assisted (individual)',TRUE~'AI-assisted (relational)'))
  print(p)
  # Significance labels are computed, not copied from the manuscript.
  ann <- filter(p,!is.na(p)) %>% mutate(label=ifelse(p<.001,
                                                     'p < 0.001',sprintf('p = %.3f',p))) %>% arrange(x)
  gap <- max(p$mean+p$se)*.09
  ann$y <- max(p$mean+p$se)+gap*seq_len(nrow(ann))
  print(ggplot(p,aes(x,mean,fill=fill)) + 
          geom_col(width=.38) + 
          geom_errorbar(aes(ymin=mean-se,ymax=mean+se),width=0) +
    geom_segment(data=ann,aes(x=1,xend=x,y=y,yend=y),inherit.aes=FALSE) +
    geom_segment(data=ann,aes(x=1,xend=1,y=y,yend=y-gap*.25),inherit.aes=FALSE) +
    geom_segment(data=ann,aes(x=x,xend=x,y=y,yend=y-gap*.25),inherit.aes=FALSE) +
    geom_text(data=ann,aes(x=(1+x)/2,y=y+gap*.35,label=label),inherit.aes=FALSE,size=3) +
    scale_x_continuous(breaks=1:3,labels=c('None','Individual','Relational')) +
    scale_fill_manual(values=c('Self-written'='#8b8b83','AI-assisted (individual)'='#1496BB','AI-assisted (relational)'='#E85125')) +
    labs(x='Message suggestion condition',y=if(metric_id=='TOXICITY') 'Toxicity' else 'Respectfulness',fill=NULL) + theme_classic() + theme(legend.position='bottom'))
}
