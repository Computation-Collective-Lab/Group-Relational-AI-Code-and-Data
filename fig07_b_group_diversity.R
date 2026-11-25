library(dplyr)
library(ggplot2)
library(lmerTest)
library(emmeans)
d <- read.csv('./csv/RoomData.csv') %>% 
  filter(Condition %in% c('control','personal','relational-static')) %>%
  rename(distance=WithinDistanceAcrossRoomParticipant, volume=NumMsgs, sentiment=VADERMean)
keys <- c('SessionID','Condition','RoomID','RoundID')
stopifnot(!anyDuplicated(d[keys]))
future <- d %>% 
  transmute(SessionID,Condition,RoomID,RoundID=RoundID-1,next_distance=distance,next_size=GroupSize)

m <- inner_join(d,future,by=keys) %>% filter(RoundID<=4,GroupSize>=2,next_size>=2) %>%
  filter(if_all(all_of(c('distance','next_distance','volume','sentiment','GroupSize')),is.finite))

fit <- lmer(next_distance ~ distance + GroupSize + RoundID + volume*sentiment + (1|SessionID/RoomID),data=m)

print(nobs(fit)); print(summary(fit)); print(confint(fit,method='Wald'))
# Fixed-effect prediction intervals for mean change, not individual prediction intervals.

ref <- median(m$distance)

pr <- as.data.frame(confint(emmeans(fit,~volume*sentiment,at=list(volume=seq(0,60,by=1),sentiment=c(0,.3,.6),distance=ref,GroupSize=median(m$GroupSize),RoundID=median(m$RoundID))),level=.95)) %>%
  mutate(change=emmean-ref,lo=lower.CL-ref,hi=upper.CL-ref, sentiment=factor(sentiment,levels=c(0,.3,.6),labels=c('Low (0.0)','Medium (0.3)','High (0.6)')))

sentiment_colors <- c(
  "Low (0.0)"    = "#C2A0CC",
  "Medium (0.3)" = "#7B3294",
  "High (0.6)"   = "#40004B"
)

print(ggplot(pr, aes(volume, change, colour = sentiment, fill = sentiment))
      + geom_ribbon(aes(ymin = lo, ymax = hi), alpha = .15, colour = NA)
      + geom_line(linewidth = 1)
      + scale_colour_manual(values = sentiment_colors)
      + scale_fill_manual(values = sentiment_colors)
      + labs(
        x = "Message volume",
        y = expression(atop("Change in within-group",
                            "stance distance (" * D[t+1] - D[t] * ")")),
        colour = "Message\nsentiment",
        fill = "Message\nsentiment"
      )
      + scale_x_continuous(breaks = seq(0, 60, 20))
      + scale_y_continuous(breaks = seq(-1, 1, .5))
      + theme_classic())