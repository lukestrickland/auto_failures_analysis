library(dplyr)
library(ggplot2)
library(lme4)
library(car)
library(lsmeans)
source("R/0-analysis_functions.R")

load("img/cleandats.RData")

cleandats <- cleandats %>% mutate(C = toupper(S)==R)

theme_set(theme_simple())

accs <-
  cleandats %>% group_by(s, S, cond, failtrial, sess) %>% 
  filter(!is.na(R)) %>% summarise(acc = mean(toupper(as.character(S)) == R)) %>%
  arrange(s) %>% arrange(failtrial)

#Check any bad accs
# None except on fail trials (which is expected)
accs[accs$acc<0.6 & accs$failtrial=="fail",]
accs[accs$acc<0.6 & accs$failtrial=="nonf",]

mean_accs <- accs %>% group_by(S, cond, failtrial, sess) %>%
  summarise(meanacc=mean(acc))

searr= se2(accs, facs= c("S", "cond", "failtrial", "sess"), dvnam="acc", sfac="s")
se_accs <- as.data.frame.table(searr)
colnames(se_accs)[colnames(se_accs)=="Freq"] <- "seacc"

plot.df <- full_join(mean_accs, se_accs)

plot.df$S <- factor(plot.df$S, levels=c("n", "c"),
                    labels=c("Non-conflict", "Conflict"))

plot.df$failtrial <- factor(plot.df$failtrial, levels=c("nonf", "fail"),
                    labels=c("Automation Success", "Automation Failure"))

ggplot(plot.df, aes(failtrial, 1-meanacc)) +geom_point(aes(col=cond), size=3)  +
  geom_line(aes(y=1-meanacc, group=interaction(cond, S), col=cond), linetype=2)+ 
  facet_grid(sess~S) + geom_errorbar(aes(
    ymax = 1-meanacc + seacc,
    ymin = 1-meanacc - seacc,
    colour = cond
  )) +ylab ("Error Rate") +xlab("")

#Fit probit model to accuracies

acc_glmer_top <-
  glmer(C ~ S * cond * sess * failtrial + (1 |s),
        data = cleandats,
        family = binomial(link = "probit"))

ss <- getME(acc_glmer_top,c("theta","fixef"))
acc_glmer_top2 <- update(acc_glmer_top,
                             start = ss,
                             control = glmerControl(optCtrl = list(maxfun = 2e4)))


ss2 <- getME(acc_glmer_top2,c("theta","fixef"))
acc_glmer_top3 <- update(acc_glmer_top2,
                             start = ss2,
                             control = glmerControl(optCtrl = list(maxfun = 2e5)))

save(acc_glmer_top3, file = "img/acc_model.RData")

Anova(acc_glmer_top3)

summary(
  contrast(lsmeans(acc_glmer_top3, ~cond|failtrial*sess), 
           method="pairwise")) 



RTs <-cleandats %>% group_by(s, S, cond, failtrial, sess) %>% filter(toupper(S)==R) %>% 
  summarise(RT=mean(RT))%>% arrange(failtrial)

mean_RTs <- RTs %>% group_by(S, cond, failtrial,sess) %>% summarise(meanRT=mean(RT))

searr= se2(RTs, facs= c("S", "cond", "failtrial", "sess"), dvnam="RT", sfac="s")
se_RTs <- as.data.frame.table(searr)
colnames(se_RTs)[colnames(se_RTs)=="Freq"] <- "seRT"



plot.df <- full_join(mean_RTs, se_RTs)

plot.df$S <- factor(plot.df$S, levels=c("n", "c"),
                    labels=c("Non-conflict", "Conflict"))

plot.df$failtrial <- factor(plot.df$failtrial, levels=c("nonf", "fail"),
                            labels=c("Automation Success", "Automation Failure"))

ggplot(plot.df, aes(failtrial, meanRT)) +geom_point(aes(col=cond), size=3)  + 
  geom_line(aes(y=meanRT, group=interaction(cond, S), col=cond), linetype=2)+ 
  facet_grid(sess~S) + geom_errorbar(aes(
    ymax = meanRT + seRT,
    ymin = meanRT - seRT,
    colour = cond
  ))

RT_model <- lmer(RT ~ S * cond * sess * failtrial + (1 |s),
     data = RTs)

Anova(RT_model)

summary(
  contrast(lsmeans(RT_model, ~cond|failtrial), 
           method="pairwise")) 

summary(
  contrast(lsmeans(RT_model, ~sess*failtrial),
           interaction=TRUE, 
           method="pairwise")) 


summary(
  contrast(lsmeans(RT_model, ~cond|sess),
           method="pairwise")) 

summary(
  contrast(lsmeans(RT_model, ~cond*sess),
           interaction=TRUE, 
           method="pairwise")) 
