
source("dmc/dmc.R")
load_model("LBA", "lba_B.R")

load("img/cleandats.RData")
cleandats <- cleandats[!colnames(cleandats) %in% "C"]
cleandats <- as.data.frame(cleandats)
cleandats$cond <- factor(cleandats$cond, levels=c("AUTO", "MANUAL"),
                         labels=c("A", "M"))

cleandats$S <- factor(cleandats$S, levels=c("n", "c"),
                         labels=c("nn", "cc"))

cleandats$R <- factor(cleandats$R, levels=c("N", "C"))
cleandats$s<- factor(cleandats$s)




CA_top_model <- model.dmc(
  p.map = list(
    A = "1",B = c("cond", "sess", "R"), t0 = "1", mean_v = c("S", "cond", "failtrial", "M"),
    sd_v = c("M"), st0 = "1"),
  match.map = list(
    M = list(nn = "N", cc="C")
  ),
  factors = list(
    S = c("nn", "cc"), cond = c("A", "M"), sess = c("1", "2"),
    failtrial=c("nonf", "fail")
  ),
  constants = c(st0 = 0, sd_v.false = 1
  ),
  responses = c("N", "C"),type = "norm"
)


CA_top_p.vector  <- c(t0=0.3,A=3,
                                sd_v.true = 1,
               
  B.A.1.N=2, B.M.1.N=2,             
   B.A.2.N=2, B.M.2.N=2, B.A.1.C=2,             
  B.M.1.C=2, B.A.2.C=2,   B.M.2.C=2, 
  
  mean_v.nn.A.nonf.true=1,  mean_v.cc.A.nonf.true=1, 
 mean_v.nn.M.nonf.true=1,  mean_v.cc.M.nonf.true=1,
 mean_v.nn.A.fail.true=1, mean_v.cc.A.fail.true=1, 
 mean_v.nn.M.fail.true=1,  mean_v.cc.M.fail.true=1, 
 mean_v.nn.A.nonf.false=0, mean_v.cc.A.nonf.false=0,
 mean_v.nn.M.nonf.false=0,mean_v.cc.M.nonf.false=0, 
 mean_v.nn.A.fail.false=0, mean_v.cc.A.fail.false=0,
 mean_v.nn.M.fail.false=0, mean_v.cc.M.fail.false=0
 )

check.p.vector(CA_top_p.vector, CA_top_model)

CA_top_p.prior <- prior.p.dmc(
  dists = rep("tnorm", length(CA_top_p.vector)),
  p1=CA_top_p.vector,                           
  p2=c(1,1,1,rep(1, 8), rep(2, 16)),
  lower=c(0.1, 0,0, rep(0, 8), rep(NA, 16)),
  upper=c(5,10, rep(Inf, length(CA_top_p.vector)-2))
)

CA_top_dm <- data.model.dmc(cleandats,
                                   CA_top_model)

CA_top_samples <- h.samples.dmc(nmc = 180,
                                          CA_top_p.prior,
                                          CA_top_dm, thin=20)

save(CA_top_samples, file="CA_top_samples.RData")



source("dmc/dmc.R")
load_model("LBA", "lba_B.R")

load("img/cleandats.RData")
cleandats <- cleandats[!colnames(cleandats) %in% "C"]
cleandats <- as.data.frame(cleandats)
cleandats$cond <- factor(cleandats$cond, levels=c("AUTO", "MANUAL"),
                         labels=c("A", "M"))

cleandats$S <- factor(cleandats$S, levels=c("n", "c"),
                         labels=c("nn", "cc"))

cleandats$R <- factor(cleandats$R, levels=c("N", "C"))
cleandats$s<- factor(cleandats$s)




CA_top_thresholds_model <- model.dmc(
  p.map = list(
    A = "1",B = c("cond", "sess", "S", "failtrial", "R"), t0 = "1", mean_v = c("S", "cond", "failtrial", "M"),
    sd_v = c("M"), st0 = "1"),
  match.map = list(
    M = list(nn = "N", cc="C")
  ),
  factors = list(
    S = c("nn", "cc"), cond = c("A", "M"), sess = c("1", "2"),
    failtrial=c("nonf", "fail")
  ),
  constants = c(st0 = 0, sd_v.false = 1
  ),
  responses = c("N", "C"),type = "norm"
)


CA_top_thresholds_p.vector  <- c(t0=0.3,A=3,
                                sd_v.true = 1,
               
B.A.1.nn.nonf.N=1,        B.M.1.nn.nonf.N=1,       
 B.A.2.nn.nonf.N=1,        B.M.2.nn.nonf.N=1,        B.A.1.cc.nonf.N=1,       
  B.M.1.cc.nonf.N=1,        B.A.2.cc.nonf.N=1,        B.M.2.cc.nonf.N=1,       
B.A.1.nn.fail.N=1,        B.M.1.nn.fail.N=1,        B.A.2.nn.fail.N=1,       
B.M.2.nn.fail.N=1,        B.A.1.cc.fail.N=1,        B.M.1.cc.fail.N=1,       
B.A.2.cc.fail.N=1,        B.M.2.cc.fail.N=1,        B.A.1.nn.nonf.C=1,       
 B.M.1.nn.nonf.C=1,        B.A.2.nn.nonf.C=1,        B.M.2.nn.nonf.C=1,       
 B.A.1.cc.nonf.C=1,        B.M.1.cc.nonf.C=1,        B.A.2.cc.nonf.C=1,       
 B.M.2.cc.nonf.C=1,        B.A.1.nn.fail.C=1,        B.M.1.nn.fail.C=1,       
 B.A.2.nn.fail.C=1,        B.M.2.nn.fail.C=1,        B.A.1.cc.fail.C=1,       
 B.M.1.cc.fail.C=1,        B.A.2.cc.fail.C=1,        B.M.2.cc.fail.C=1, 
  
  mean_v.nn.A.nonf.true=1,  mean_v.cc.A.nonf.true=1, 
 mean_v.nn.M.nonf.true=1,  mean_v.cc.M.nonf.true=1,
 mean_v.nn.A.fail.true=1, mean_v.cc.A.fail.true=1, 
 mean_v.nn.M.fail.true=1,  mean_v.cc.M.fail.true=1, 
 mean_v.nn.A.nonf.false=0, mean_v.cc.A.nonf.false=0,
 mean_v.nn.M.nonf.false=0,mean_v.cc.M.nonf.false=0, 
 mean_v.nn.A.fail.false=0, mean_v.cc.A.fail.false=0,
 mean_v.nn.M.fail.false=0, mean_v.cc.M.fail.false=0
 )

check.p.vector(CA_top_thresholds_p.vector, CA_top_thresholds_model)

CA_top_thresholds_p.prior <- prior.p.dmc(
  dists = rep("tnorm", length(CA_top_thresholds_p.vector)),
  p1=CA_top_thresholds_p.vector,                           
  p2=c(1,1,1,rep(1, 32), rep(2, 16)),
  lower=c(0.1, 0,0, rep(0, 32), rep(NA, 16)),
  upper=c(5,10, rep(Inf, length(CA_top_thresholds_p.vector)-2))
)

CA_top_thresholds_dm <- data.model.dmc(cleandats,
                                   CA_top_thresholds_model)

CA_top_thresholds_samples <- h.samples.dmc(nmc = 180,
                                          CA_top_thresholds_p.prior,
                                          CA_top_thresholds_dm, thin=20)

save(CA_top_thresholds_samples, file="CA_top_thresholds_samples.RData")

#Top with thresholds multiplicative model


source("dmc/dmc.R")
load_model("LBA", "lba_B_autothres.R")

load("img/cleandats.RData")
cleandats <- cleandats[!colnames(cleandats) %in% "C"]
cleandats <- as.data.frame(cleandats)
cleandats$cond <- factor(cleandats$cond, levels=c("AUTO", "MANUAL"),
                         labels=c("A", "M"))

cleandats$S <- factor(cleandats$S, levels=c("n", "c"),
                         labels=c("nn", "cc"))

cleandats$R <- factor(cleandats$R, levels=c("N", "C"))
cleandats$s<- factor(cleandats$s)



tmap <-
  empty.map(list(
         S = c("nn", "cc"), cond = c("A", "M"), sess = c("1", "2"),
            failtrial=c("nonf", "fail"),
             R = c("N", "C")
  ), 
    levels=c(
             "MAN",
             "AnNS","AnNF",
             "AnCS", "AnCF",
              "AcNS","AcNF",
             "AcCS", "AcCF"
             ))

tmap[1:32] <- c(
  "AnNS","AcNS","MAN", "MAN",
  "AnNS","AcNS","MAN", "MAN",
  
  "AnNF","AcNF","MAN", "MAN",
  "AnNF","AcNF","MAN", "MAN",
  
  "AnCS","AcCS","MAN", "MAN",
  "AnCS","AcCS","MAN", "MAN",
  
  "AnCF","AcCF","MAN", "MAN",
  "AnCF","AcCF","MAN", "MAN"
  
)


CA_top_thresholdsmult_model <- model.dmc(
  p.map = list(
    A = "1",B = c("sess", "R"), t0 = "1", mean_v = c("S", "cond", "failtrial", "M"),
    sd_v = c("M"), st0 = "1", tb="TMAP"),
  match.map = list(
    M = list(nn = "N", cc="C"),
    TMAP=tmap
  ),
  factors = list(
    S = c("nn", "cc"), cond = c("A", "M"), sess = c("1", "2"),
    failtrial=c("nonf", "fail")
  ),
  constants = c(st0 = 0, sd_v.false = 1, tb.MAN=1
  ),
  responses = c("N", "C"),type = "norm"
)


CA_top_thresholdsmult_p.vector  <- c(t0=0.3,A=3,
                                sd_v.true = 1,
               
  B.1.N=2,B.2.N=2, B.1.C=2, B.2.C=2, 
          
 tb.AnNS=1,                tb.AnNF=1,                tb.AnCS=1,               
tb.AnCF=1,                tb.AcNS=1,                tb.AcNF=1,               
 tb.AcCS=1,                tb.AcCF=1,   
  
  mean_v.nn.A.nonf.true=1,  mean_v.cc.A.nonf.true=1, 
 mean_v.nn.M.nonf.true=1,  mean_v.cc.M.nonf.true=1,
 mean_v.nn.A.fail.true=1, mean_v.cc.A.fail.true=1, 
 mean_v.nn.M.fail.true=1,  mean_v.cc.M.fail.true=1, 
 mean_v.nn.A.nonf.false=0, mean_v.cc.A.nonf.false=0,
 mean_v.nn.M.nonf.false=0,mean_v.cc.M.nonf.false=0, 
 mean_v.nn.A.fail.false=0, mean_v.cc.A.fail.false=0,
 mean_v.nn.M.fail.false=0, mean_v.cc.M.fail.false=0
 )

check.p.vector(CA_top_thresholdsmult_p.vector, CA_top_thresholdsmult_model)

CA_top_thresholdsmult_p.prior <- prior.p.dmc(
  dists = rep("tnorm", length(CA_top_thresholdsmult_p.vector)),
  p1=CA_top_thresholdsmult_p.vector,                           
  p2=c(1,1,1,rep(1, 12), rep(2, 16)),
  lower=c(0.1, 0,0, rep(0, 12), rep(NA, 16)),
  upper=c(5,10, rep(Inf, length(CA_top_thresholdsmult_p.vector)-2))
)

CA_top_thresholdsmult_dm <- data.model.dmc(cleandats,
                                   CA_top_thresholdsmult_model)

CA_top_thresholdsmult_samples <- h.samples.dmc(nmc = 180,
                                          CA_top_thresholdsmult_p.prior,
                                          CA_top_thresholdsmult_dm, thin=20)

save(CA_top_thresholdsmult_samples, file="CA_top_thresholdsmult_samples.RData")


source("dmc/dmc.R")
load_model("LBA", "lba_B_automation.R")

load("img/cleandats.RData")
cleandats <- cleandats[!colnames(cleandats) %in% "C"]
cleandats <- as.data.frame(cleandats)
cleandats$cond <- factor(cleandats$cond, levels=c("AUTO", "MANUAL"),
                         labels=c("A", "M"))

cleandats$S <- factor(cleandats$S, levels=c("n", "c"),
                         labels=c("nn", "cc"))

cleandats$R <- factor(cleandats$R, levels=c("N", "C"))
cleandats$s<- factor(cleandats$s)




mapauto <-
  empty.map(list(
         S = c("nn", "cc"), cond = c("A", "M"), sess = c("1", "2"),
            failtrial=c("nonf", "fail"),
             R = c("N", "C")
  ), 
    levels=c(
             "man",
             "anT","anF",
             "acT", "acF"))

mapauto[1:32] <- c(
  "anT","acF","man", "man",
  "anT","acF","man", "man",
  
  "acF","anT","man", "man",
  "acF","anT","man", "man",
  
   "anF","acT","man", "man",
   "anF","acT","man", "man",
  
    "acT","anF","man", "man",
   "acT","anF","man", "man"
  
)

#to make it less constrained (basically the equivalent of freely estimated accumulation rates) I would be able to separate
# auto evidence for failures and successes - psychologically implausible?

#A few checks on mapmeanv due to mind-bending twisting of levels

mapauto[
  grepl("M", names(mapauto)) ]

mapauto[
  grepl("nn", names(mapauto)) & grepl("nonf", names(mapauto)) &
    !grepl("M", names(mapauto)) 
  ]

mapauto[
  grepl("cc", names(mapauto)) & grepl("nonf", names(mapauto)) &
    !grepl("M", names(mapauto)) 
  ]

mapauto[
  grepl("nn", names(mapauto)) & grepl("fail", names(mapauto)) &
    !grepl("M", names(mapauto)) 
  ]

mapauto[
  grepl("cc", names(mapauto)) & grepl("fail", names(mapauto)) &
    !grepl("M", names(mapauto)) 
  ]




auto_top_model <- model.dmc(
  p.map = list(
    A = "1",B = c("cond", "sess", "R"), t0 = "1", mean_v = c("S", "M"),
    sd_v = c("M"), st0 = "1", a= c("MAPAUTO")),
  match.map = list(
    M = list(nn = "N", cc="C"),
    MAPAUTO = mapauto
  ),
  factors = list(
    S = c("nn", "cc"), cond = c("A", "M"), sess = c("1", "2"),
    failtrial=c("nonf", "fail")
  ),
  constants = c(st0 = 0, sd_v.false = 1, a.man=0
  ),
  responses = c("N", "C"),type = "norm"
)


auto_top_p.vector  <- c(t0=0.3,A=3,
                                sd_v.true = 1,
               
  B.A.1.N=2, B.M.1.N=2,             
   B.A.2.N=2, B.M.2.N=2, B.A.1.C=2,             
  B.M.1.C=2, B.A.2.C=2,   B.M.2.C=2, 
  
  mean_v.nn.true=0,  mean_v.cc.true=0, 
  mean_v.nn.false=0, mean_v.cc.false=0,
  a.anT=0, a.anF=0, a.acT=0, a.acF=0
 )

check.p.vector(auto_top_p.vector, auto_top_model)

auto_top_p.prior <- prior.p.dmc(
  dists = rep("tnorm", length(auto_top_p.vector)),
  p1=auto_top_p.vector,                           
  p2=c(1,1,1,rep(1, 8), rep(2, 8)),
  lower=c(0.1, 0,0, rep(0, 8), rep(NA, 8)),
  upper=c(5,10, rep(Inf, length(auto_top_p.vector)-2))
)

auto_top_dm <- data.model.dmc(cleandats,
                                   auto_top_model)

auto_top_samples <- h.samples.dmc(nmc = 180,
                                          auto_top_p.prior,
                                          auto_top_dm, thin=20)

save(auto_top_samples, file="auto_top_samples.RData")





source("dmc/dmc.R")
load_model("LBA", "lba_B_automation.R")

load("img/cleandats.RData")
cleandats <- cleandats[!colnames(cleandats) %in% "C"]
cleandats <- as.data.frame(cleandats)
cleandats$cond <- factor(cleandats$cond, levels=c("AUTO", "MANUAL"),
                         labels=c("A", "M"))

cleandats$S <- factor(cleandats$S, levels=c("n", "c"),
                         labels=c("nn", "cc"))

cleandats$R <- factor(cleandats$R, levels=c("N", "C"))
cleandats$s<- factor(cleandats$s)




mapauto <-
  empty.map(list(
         S = c("nn", "cc"), cond = c("A", "M"), sess = c("1", "2"),
            failtrial=c("nonf", "fail"),
             R = c("N", "C")
  ), 
    levels=c(
             "man",
             "anT","anF",
             "acT", "acF"))

mapauto[1:32] <- c(
  "anT","acF","man", "man",
  "anT","acF","man", "man",
  
  "acF","anT","man", "man",
  "acF","anT","man", "man",
  
   "anF","acT","man", "man",
   "anF","acT","man", "man",
  
    "acT","anF","man", "man",
   "acT","anF","man", "man"
  
)

#to make it less constrained (basically the equivalent of freely estimated accumulation rates) I would be able to separate
# auto evidence for failures and successes - psychologically implausible?

#A few checks on mapmeanv due to mind-bending twisting of levels

mapauto[
  grepl("M", names(mapauto)) ]

mapauto[
  grepl("nn", names(mapauto)) & grepl("nonf", names(mapauto)) &
    !grepl("M", names(mapauto)) 
  ]

mapauto[
  grepl("cc", names(mapauto)) & grepl("nonf", names(mapauto)) &
    !grepl("M", names(mapauto)) 
  ]

mapauto[
  grepl("nn", names(mapauto)) & grepl("fail", names(mapauto)) &
    !grepl("M", names(mapauto)) 
  ]

mapauto[
  grepl("cc", names(mapauto)) & grepl("fail", names(mapauto)) &
    !grepl("M", names(mapauto)) 
  ]




auto_noB_model <- model.dmc(
  p.map = list(
    A = "1",B = c("sess", "R"), t0 = "1", mean_v = c("S", "M"),
    sd_v = c("M"), st0 = "1", a= c("MAPAUTO")),
  match.map = list(
    M = list(nn = "N", cc="C"),
    MAPAUTO = mapauto
  ),
  factors = list(
    S = c("nn", "cc"), cond = c("A", "M"), sess = c("1", "2"),
    failtrial=c("nonf", "fail")
  ),
  constants = c(st0 = 0, sd_v.false = 1, a.man=0
  ),
  responses = c("N", "C"),type = "norm"
)


auto_noB_p.vector  <- c(t0=0.3,A=3,
                                sd_v.true = 1,
               
  B.1.N=2,            
   B.2.N=2,B.1.C=2,             
  B.2.C=2,   
  
  mean_v.nn.true=0,  mean_v.cc.true=0, 
  mean_v.nn.false=0, mean_v.cc.false=0,
  a.anT=0, a.anF=0, a.acT=0, a.acF=0
 )

check.p.vector(auto_noB_p.vector, auto_noB_model)

auto_noB_p.prior <- prior.p.dmc(
  dists = rep("tnorm", length(auto_noB_p.vector)),
  p1=auto_noB_p.vector,                           
  p2=c(1,1,1,rep(1, 4), rep(2, 8)),
  lower=c(0.1, 0,0, rep(0, 4), rep(NA, 8)),
  upper=c(5,10, rep(Inf, length(auto_noB_p.vector)-2))
)

auto_noB_dm <- data.model.dmc(cleandats,
                                   auto_noB_model)

auto_noB_samples <- h.samples.dmc(nmc = 180,
                                          auto_noB_p.prior,
                                          auto_noB_dm, thin=20)

save(auto_noB_samples, file="auto_noB_samples.RData")







source("dmc/dmc.R")
load_model("LBA", "lba_B_automation.R")

load("img/cleandats.RData")
cleandats <- cleandats[!colnames(cleandats) %in% "C"]
cleandats <- as.data.frame(cleandats)
cleandats$cond <- factor(cleandats$cond, levels=c("AUTO", "MANUAL"),
                         labels=c("A", "M"))

cleandats$S <- factor(cleandats$S, levels=c("n", "c"),
                         labels=c("nn", "cc"))

cleandats$R <- factor(cleandats$R, levels=c("N", "C"))
cleandats$s<- factor(cleandats$s)




mapauto <-
  empty.map(list(
         S = c("nn", "cc"), cond = c("A", "M"), sess = c("1", "2"),
            failtrial=c("nonf", "fail"),
             R = c("N", "C")
  ), 
    levels=c(
             "man",
             "aT","aF"))

mapauto[1:32] <- c(
  "aT","aF","man", "man",
  "aT","aF","man", "man",
  
  "aF","aT","man", "man",
  "aF","aT","man", "man",
  
   "aF","aT","man", "man",
   "aF","aT","man", "man",
  
    "aT","aF","man", "man",
   "aT","aF","man", "man"
  
)

#to make it less constrained (basically the equivalent of freely estimated accumulation rates) I would be able to separate
# auto evidence for failures and successes - psychologically implausible?

#A few checks on mapmeanv due to mind-bending twisting of levels

mapauto[
  grepl("M", names(mapauto)) ]

mapauto[
  grepl("nn", names(mapauto)) & grepl("nonf", names(mapauto)) &
    !grepl("M", names(mapauto)) 
  ]

mapauto[
  grepl("cc", names(mapauto)) & grepl("nonf", names(mapauto)) &
    !grepl("M", names(mapauto)) 
  ]

mapauto[
  grepl("nn", names(mapauto)) & grepl("fail", names(mapauto)) &
    !grepl("M", names(mapauto)) 
  ]

mapauto[
  grepl("cc", names(mapauto)) & grepl("fail", names(mapauto)) &
    !grepl("M", names(mapauto)) 
  ]




auto_anoS_model <- model.dmc(
  p.map = list(
    A = "1",B = c("cond", "sess", "R"), t0 = "1", mean_v = c("S", "M"),
    sd_v = c("M"), st0 = "1", a= c("MAPAUTO")),
  match.map = list(
    M = list(nn = "N", cc="C"),
    MAPAUTO = mapauto
  ),
  factors = list(
    S = c("nn", "cc"), cond = c("A", "M"), sess = c("1", "2"),
    failtrial=c("nonf", "fail")
  ),
  constants = c(st0 = 0, sd_v.false = 1, a.man=0
  ),
  responses = c("N", "C"),type = "norm"
)


auto_anoS_p.vector  <- c(t0=0.3,A=3,
                                sd_v.true = 1,
               
  B.A.1.N=2, B.M.1.N=2,             
   B.A.2.N=2, B.M.2.N=2, B.A.1.C=2,             
  B.M.1.C=2, B.A.2.C=2,   B.M.2.C=2, 
  
  mean_v.nn.true=0,  mean_v.cc.true=0, 
  mean_v.nn.false=0, mean_v.cc.false=0,
  a.aT=0, a.aF=0, a.aT=0, a.aF=0
 )

check.p.vector(auto_anoS_p.vector, auto_anoS_model)

auto_anoS_p.prior <- prior.p.dmc(
  dists = rep("tnorm", length(auto_anoS_p.vector)),
  p1=auto_anoS_p.vector,                           
  p2=c(1,1,1,rep(1, 8), rep(2, 8)),
  lower=c(0.1, 0,0, rep(0, 8), rep(NA, 8)),
  upper=c(5,10, rep(Inf, length(auto_anoS_p.vector)-2))
)

auto_anoS_dm <- data.model.dmc(cleandats,
                                   auto_anoS_model)

auto_anoS_samples <- h.samples.dmc(nmc = 180,
                                          auto_anoS_p.prior,
                                          auto_anoS_dm, thin=20)

save(auto_anoS_samples, file="auto_anoS_samples.RData")




source("dmc/dmc.R")
load_model("LBA", "lba_B_automation.R")

load("img/cleandats.RData")
cleandats <- cleandats[!colnames(cleandats) %in% "C"]
cleandats <- as.data.frame(cleandats)
cleandats$cond <- factor(cleandats$cond, levels=c("AUTO", "MANUAL"),
                         labels=c("A", "M"))

cleandats$S <- factor(cleandats$S, levels=c("n", "c"),
                         labels=c("nn", "cc"))

cleandats$R <- factor(cleandats$R, levels=c("N", "C"))
cleandats$s<- factor(cleandats$s)




mapauto <-
  empty.map(list(
         S = c("nn", "cc"), cond = c("A", "M"), sess = c("1", "2"),
            failtrial=c("nonf", "fail"),
             R = c("N", "C")
  ), 
    levels=c(
             "man",
             "aT","aF"))

mapauto[1:32] <- c(
  "aT","aF","man", "man",
  "aT","aF","man", "man",
  
  "aF","aT","man", "man",
  "aF","aT","man", "man",
  
   "aF","aT","man", "man",
   "aF","aT","man", "man",
  
    "aT","aF","man", "man",
   "aT","aF","man", "man"
  
)

#to make it less constrained (basically the equivalent of freely estimated accumulation rates) I would be able to separate
# auto evidence for failures and successes - psychologically implausible?

#A few checks on mapmeanv due to mind-bending twisting of levels

mapauto[
  grepl("M", names(mapauto)) ]

mapauto[
  grepl("nn", names(mapauto)) & grepl("nonf", names(mapauto)) &
    !grepl("M", names(mapauto)) 
  ]

mapauto[
  grepl("cc", names(mapauto)) & grepl("nonf", names(mapauto)) &
    !grepl("M", names(mapauto)) 
  ]

mapauto[
  grepl("nn", names(mapauto)) & grepl("fail", names(mapauto)) &
    !grepl("M", names(mapauto)) 
  ]

mapauto[
  grepl("cc", names(mapauto)) & grepl("fail", names(mapauto)) &
    !grepl("M", names(mapauto)) 
  ]




auto_anoSnoB_model <- model.dmc(
  p.map = list(
    A = "1",B = c("sess", "R"), t0 = "1", mean_v = c("S", "M"),
    sd_v = c("M"), st0 = "1", a= c("MAPAUTO")),
  match.map = list(
    M = list(nn = "N", cc="C"),
    MAPAUTO = mapauto
  ),
  factors = list(
    S = c("nn", "cc"), cond = c("A", "M"), sess = c("1", "2"),
    failtrial=c("nonf", "fail")
  ),
  constants = c(st0 = 0, sd_v.false = 1, a.man=0
  ),
  responses = c("N", "C"),type = "norm"
)


auto_anoSnoB_p.vector  <- c(t0=0.3,A=3,
                                sd_v.true = 1,
               
  B.1.N=2,            
   B.2.N=2,B.1.C=2,             
  B.2.C=2,   
  
  mean_v.nn.true=0,  mean_v.cc.true=0, 
  mean_v.nn.false=0, mean_v.cc.false=0,
  a.aT=0, a.aF=0, a.aT=0, a.aF=0
 )

check.p.vector(auto_anoSnoB_p.vector, auto_anoSnoB_model)

auto_anoSnoB_p.prior <- prior.p.dmc(
  dists = rep("tnorm", length(auto_anoSnoB_p.vector)),
  p1=auto_anoSnoB_p.vector,                           
  p2=c(1,1,1,rep(1, 4), rep(2, 8)),
  lower=c(0.1, 0,0, rep(0, 4), rep(NA, 8)),
  upper=c(5,10, rep(Inf, length(auto_anoSnoB_p.vector)-2))
)

auto_anoSnoB_dm <- data.model.dmc(cleandats,
                                   auto_anoSnoB_model)

auto_anoSnoB_samples <- h.samples.dmc(nmc = 180,
                                          auto_anoSnoB_p.prior,
                                          auto_anoSnoB_dm, thin=20)

save(auto_anoSnoB_samples, file="auto_anoSnoB_samples.RData")


