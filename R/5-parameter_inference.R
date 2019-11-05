source("dmc/dmc.R")
source("dmc/dmc_extras.R")
load_model("LBA","lba_B.R")

theme_set(theme_simple())

load("samples_data/CA_top_samples.RData")

msds <- get.msds(CA_top_samples)

Vs <- msds[grep("mean_v", rownames(msds)),]

Vs$Cond <- "Manual" 
Vs$Cond[grep("A", rownames(Vs))] <- "Automation"
Vs$Auto <- "Automation Success"
Vs$Auto[grep("fail", rownames(Vs))] <- "Automation Failure"
Vs$S <- "Conflict"
Vs$S[grep("nn", rownames(Vs))] <- "Non-conflict"
Vs$match <- "Match"
Vs$match[grep("false", rownames(Vs))] <- "Mismatch"

ggplot(Vs, aes(factor(Auto),M)) + 
  geom_point(stat = "identity",aes(col=Cond), size=2.5) +
  geom_errorbar(aes(ymax = M + SD, ymin = M - SD, width = 0.3, col=Cond))+ 
  ylab("Accumulation Rate") + xlab("")+
  geom_line(aes(y=M, group=Cond, col=Cond), linetype=2) +
  facet_grid(S ~ match,scales = "free", space = "free") 

Bs <- msds[grep("B", rownames(msds)),]

Bs$R <- "Non-conflict"
Bs$R[grep("C", rownames(Bs))] <- "Conflict"
Bs$Cond <- "Automation"
Bs$Cond[grep("M", rownames(Bs))] <- "Manual"
Bs$Session <- "Session One"
Bs$Session[grep("2", rownames(Bs))] <- "Session Two"

ggplot(Bs, aes(factor(R),M)) + 
  geom_point(stat = "identity",aes(col=Cond), size=2.5) +
  geom_errorbar(aes(ymax = M + SD, ymin = M - SD, width = 0.3, col=Cond))+ 
  ylab("Threshold") + xlab("Accumulator")+
  geom_line(aes(y=M, group=Cond, col=Cond), linetype=2) +
  facet_grid(.~Session)

