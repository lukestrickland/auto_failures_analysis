source("dmc/dmc.R")
source("dmc/dmc_extras.R")
load_model("LBA","lba_B.R")
library(gridExtra)
theme_set(theme_simple())

load("samples_data/CA_top_samples.RData")

theme_set(theme_simple())

pp <- h.post.predict.dmc(auto_anoS_samples[!(names(auto_anoS_samples)=="22")], factors=c("S", "cond", "failtrial"),
                         cores=23, save.simulation=TRUE)

ggplot.RA.dmc(pp, xaxis="cond")

ggplot.RT.dmc(pp, xaxis="cond")

#fit for presentation

pp <- h.post.predict.dmc(CA_top_samples[!(names(auto_anoS_samples)=="22")], 
                         factors=c("S", "sess", "cond", "failtrial"),
                         cores=23, save.simulation = TRUE)

rescore_column <- function(df) {
  df$R <- factor(as.character(toupper(substr(df$S,1,1))==df$R))
  new_data <- attr(df, "data")
  new_data$R <- factor(as.character(toupper(substr(new_data$S,1,1))==new_data$R))
#  new_data <- new_data %>% select(C, everything()) 
  #%>% select(-R)
  attr(df, "data") <- new_data
#  df %>% select(reps, C, everything())
  #%>% select(-R)
  df
}

pp1 <- lapply(pp, rescore_column)


test <- GET.fitgglist.dmc(pp1, factors=c("cond", "sess", "failtrial"))

accs <- test$pps %>% filter(R=="TRUE") %>% select(-R)

accs$cond <- factor(accs$cond, levels=c("A", "M"), labels =
                      c("Automation", "Manual"))

accs$failtrial <- factor(accs$failtrial, levels=c("nonf", "fail"), labels =
                      c("Automation Success", "Automation Failure"))

ggplot.RP.dmc(accs, xaxis="cond") +xlab("Condition") +ylab("Accuracy")

corRTs <- test$RTs %>% filter(R=="TRUE") %>% select(-R)

corRTs$cond <- factor(corRTs$cond, levels=c("A", "M"), labels =
                      c("Automation", "Manual"))

corRTs$failtrial <- factor(corRTs$failtrial, levels=c("nonf", "fail"), labels =
                      c("Automation Success", "Automation Failure"))

ggplot.RT.dmc(corRTs, xaxis="cond") +xlab("Condition") +ylab("Correct RT")



errRTs <- test$RTs %>% filter(R=="FALSE") %>% select(-R)

errRTs$cond <- factor(errRTs$cond, levels=c("A", "M"), labels =
                      c("Automation", "Manual"))

errRTs$failtrial <- factor(errRTs$failtrial, levels=c("nonf", "fail"), labels =
                      c("Automation Success", "Automation Failure"))

ggplot.RT.dmc(errRTs, xaxis="cond") +xlab("Condition") +ylab("Error RT")


