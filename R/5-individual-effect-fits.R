

load_model ("LBA", "lba_B.R")

library(dplyr)
library(tidyr)
library(ggplot2)
options(digits=2)

load("samples_data/CA_top_samples.RData")

pp <- h.post.predict.dmc(CA_top_samples, cores=8, save.simulation = TRUE)


pnames <- colnames(CA_top_samples[[1]]$theta)

pickps_excitation_A <- pnames[grepl("*A.nonf.true*|*A.fail.false*", 
                                  pnames)]

pickps_excitation_M<- pnames[grepl("*M.nonf.true*|*M.fail.false*", 
                                   pnames)]

pp_noex <- pickps.h.post.predict.dmc(CA_top_samples, save.simulation = TRUE,
                                     pickps_set= pickps_excitation_A ,
                                     pickps_other= pickps_excitation_M)


pickps_inhibition_A <- pnames[grepl("*A.nonf.false*|*A.fail.true*", 
                                    pnames)]

pickps_inhibition_M<- pnames[grepl("*M.nonf.false*|*M.fail.true*", 
                                   pnames)]

pp_noinh <- pickps.h.post.predict.dmc(CA_top_samples, save.simulation = TRUE,
                                     pickps_set= pickps_inhibition_A,
                                     pickps_other= pickps_inhibition_M)




autoeffects <- function (currentsim) {
  
  autosuccessdiff=NA;autofaildiff=NA; RTdiff=NA
  
  currentsim$C <- toupper(substr(currentsim$S,1,1)) == as.character(currentsim$R)

  autosuccessdiff <- mean(currentsim$C[currentsim$cond=="A" & currentsim$failtrial=="nonf"]) - 
                            mean(currentsim$C[currentsim$cond=="M" & currentsim$failtrial=="nonf"])
  
  autofaildiff <- mean(currentsim$C[currentsim$cond=="M" & currentsim$failtrial=="fail"]) - 
    mean(currentsim$C[currentsim$cond=="A" & currentsim$failtrial=="fail"])
  
  RTdiff <- mean(currentsim$RT[currentsim$C &
                                 currentsim$failtrial=="fail" &
                                 currentsim$cond=="A"]) -
              mean(currentsim$RT[currentsim$C &
                                   currentsim$failtrial=="fail" &
                                   currentsim$cond=="M"])

  out <- c(
    autosuccessdiff,
    autofaildiff,
    RTdiff
  )
  names(out) <- c(
    "autosuccessdiff","autofaildiff",
    "RTdiff")
  out
  
  
}


#The model provides a good account of individual differences
# in automation effects

PPS<- list(pp, pp_noex, pp_noinh)

ind_effects <- get.subj.effects.m(PPS, autoeffects, c("Full", "NoEx", "NoInh"))


ggplot(ind_effects %>% filter(effect=="autosuccessdiff"), aes(data, mean)) + geom_point(size=0.3) + geom_abline(slope=1, intercept=0) +
  facet_grid(model~effect) + geom_errorbar(aes(ymax = upper, ymin = lower), alpha=0.2) +ylab("Model") + xlab("Data") +
  geom_hline(yintercept=0, linetype=2)+ theme(text = element_text(size = 20))


ggplot(ind_effects %>% filter(effect=="autofaildiff"), aes(data, mean)) + geom_point(size=0.3) + geom_abline(slope=1, intercept=0) +
  facet_grid(model~effect) + geom_errorbar(aes(ymax = upper, ymin = lower), alpha=0.2) +ylab("Model") + xlab("Data") +
  geom_hline(yintercept=0, linetype=2)+ theme(text = element_text(size = 20))

ggplot(ind_effects %>% filter(effect=="RTdiff"), aes(data, mean)) + geom_point(size=0.3) + geom_abline(slope=1, intercept=0) +
  facet_grid(model~effect) + geom_errorbar(aes(ymax = upper, ymin = lower), alpha=0.2) +ylab("Model") + xlab("Data") +
  geom_hline(yintercept=0, linetype=2)+ theme(text = element_text(size = 20))


