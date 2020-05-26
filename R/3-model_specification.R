#This Script sets up the final reported model set for the paper which are then
# dispatched on a grid system using pbs pro (see grid_dispatch.R)

#It is important that the correct model is loaded with each model specification
# so load_model is included at the top of the start of each one, even though
# sometimes it is redundant because the correct model would already be loaded
# if running the script top to bottom


##Create model-ready data frame and load dmc functions

#load dmc
source("dmc/dmc.R")

create_model_data <- function(file) {
  load(file)
  cleandats <- cleandats[!colnames(cleandats) %in% "C"]
  cleandats <- as.data.frame(cleandats)
  cleandats$cond <- factor(cleandats$cond, levels=c("AUTO", "MANUAL"),
                           labels=c("A", "M"))
  
  cleandats$S <- factor(cleandats$S, levels=c("n", "c"),
                           labels=c("nn", "cc"))
  
  cleandats$R <- factor(cleandats$R, levels=c("N", "C"))
  cleandats$s<- factor(cleandats$s)
  cleandats
}

cleandats <- create_model_data("img/cleandats.RData")


#Full classic LBA model of the experiment
load_model("LBA", "lba_B.R")

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

#A classic LBA model in terms of rates, but allows
# thresholds to be affected by automation via threshold multiplier

load_model("LBA", "lba_B_autothres.R")

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


#Rate constraints
#Such that automation inhibition/excitation has to act 
# symmetrically on success and fail trials

load_model("LBA", "lba_B_automation.R")

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



#Rate constrained model with no effect of automation condition on thresholds

load_model("LBA", "lba_B_automation.R")

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


#Rate constrained model where the effects of automation can't vary
#based on stimulus type
load_model("LBA", "lba_B_automation.R")

mapauto_noS <-
  empty.map(list(
         S = c("nn", "cc"), cond = c("A", "M"), sess = c("1", "2"),
            failtrial=c("nonf", "fail"),
             R = c("N", "C")
  ), 
    levels=c(
             "man",
             "aT","aF"))

mapauto_noS[1:32] <- c(
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

mapauto_noS[
  grepl("M", names(mapauto_noS)) ]

mapauto_noS[
  grepl("nn", names(mapauto_noS)) & grepl("nonf", names(mapauto_noS)) &
    !grepl("M", names(mapauto_noS)) 
  ]

mapauto_noS[
  grepl("cc", names(mapauto_noS)) & grepl("nonf", names(mapauto_noS)) &
    !grepl("M", names(mapauto_noS)) 
  ]

mapauto_noS[
  grepl("nn", names(mapauto_noS)) & grepl("fail", names(mapauto_noS)) &
    !grepl("M", names(mapauto_noS)) 
  ]

mapauto_noS[
  grepl("cc", names(mapauto_noS)) & grepl("fail", names(mapauto_noS)) &
    !grepl("M", names(mapauto_noS)) 
  ]




auto_anoS_model <- model.dmc(
  p.map = list(
    A = "1",B = c("cond", "sess", "R"), t0 = "1", mean_v = c("S", "M"),
    sd_v = c("M"), st0 = "1", a= c("mapauto_noS")),
  match.map = list(
    M = list(nn = "N", cc="C"),
    mapauto_noS = mapauto_noS
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


#Model where the automation effect is forced to come out in excitation
#not inhibition
load_model("LBA", "lba_B_automation.R")

auto_anoS_ex_model <- model.dmc(
  p.map = list(
    A = "1",B = c("cond", "sess", "R"), t0 = "1", mean_v = c("S", "M"),
    sd_v = c("M"), st0 = "1", a= c("MAPAUTO")),
  match.map = list(
    M = list(nn = "N", cc="C"),
    MAPAUTO = mapauto_noS
  ),
  factors = list(
    S = c("nn", "cc"), cond = c("A", "M"), sess = c("1", "2"),
    failtrial=c("nonf", "fail")
  ),
  constants = c(st0 = 0, sd_v.false = 1, a.man=0, a.aF=0
  ),
  responses = c("N", "C"),type = "norm"
)


auto_anoS_ex_p.vector  <- c(t0=0.3,A=3,
                                sd_v.true = 1,
               
  B.A.1.N=2, B.M.1.N=2,             
   B.A.2.N=2, B.M.2.N=2, B.A.1.C=2,             
  B.M.1.C=2, B.A.2.C=2,   B.M.2.C=2, 
  
  mean_v.nn.true=0,  mean_v.cc.true=0, 
  mean_v.nn.false=0, mean_v.cc.false=0,
  a.aT=0
 )

check.p.vector(auto_anoS_ex_p.vector, auto_anoS_ex_model)

auto_anoS_ex_p.prior <- prior.p.dmc(
  dists = rep("tnorm", length(auto_anoS_ex_p.vector)),
  p1=auto_anoS_ex_p.vector,                           
  p2=c(1,1,1,rep(1, 8), rep(2, 5)),
  lower=c(0.1, 0,0, rep(0, 8), rep(NA, 5)),
  upper=c(5,10, rep(Inf, length(auto_anoS_ex_p.vector)-2))
)

auto_anoS_ex_dm <- data.model.dmc(cleandats,
                                   auto_anoS_ex_model)

auto_anoS_ex_samples <- h.samples.dmc(nmc = 180,
                                          auto_anoS_ex_p.prior,
                                          auto_anoS_ex_dm, thin=20)

save(auto_anoS_ex_samples, file="auto_anoS_ex_samples.RData")




load_model("LBA", "lba_B_automation.R")

auto_anoSnoB_model <- model.dmc(
  p.map = list(
    A = "1",B = c("sess", "R"), t0 = "1", mean_v = c("S", "M"),
    sd_v = c("M"), st0 = "1", a= c("MAPAUTO")),
  match.map = list(
    M = list(nn = "N", cc="C"),
    MAPAUTO = mapauto_noS
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



load_model("LBA", "lba_B_automation.R")

tmap <-
  empty.map(list(
         S = c("nn", "cc"), cond = c("A", "M"), sess = c("1", "2"),
            failtrial=c("nonf", "fail"),
             R = c("N", "C")
  ), 
    levels=c(
             "MANC1", "MANN1",
             "AnNS1","AnNF1",
             "AnCS1", "AnCF1",
              "AcNS1","AcNF1",
             "AcCS1", "AcCF1",
             
              "MANC2", "MANN2",
             "AnNS2","AnNF2",
             "AnCS2", "AnCF2",
              "AcNS2","AcNF2",
             "AcCS2", "AcCF2"
             
             
             ))

tmap[1:32] <- c(
  "AnNS1","AcNS1","MANN1", "MANN1",
  "AnNS2","AcNS2","MANN2", "MANN2",
  
  "AnNF1","AcNF1","MANN1", "MANN1",
  "AnNF2","AcNF2","MANN2", "MANN2",
  
  "AnCS1","AcCS1","MANC1", "MANC1",
  "AnCS2","AcCS2","MANC2", "MANC2",
  
  "AnCF1","AcCF1","MANC1", "MANC1",
  "AnCF2","AcCF2","MANC2", "MANC2"
  
)



auto_thresholds_top_model <- model.dmc(
  p.map = list(
    A = "1",B = c("MAPTHRES"), t0 = "1", mean_v = c("S", "M"),
    sd_v = c("M"), st0 = "1", a= c("MAPAUTO")),
  match.map = list(
    M = list(nn = "N", cc="C"),
    MAPAUTO = mapauto,
    MAPTHRES=tmap
  ),
  factors = list(
    S = c("nn", "cc"), cond = c("A", "M"), sess = c("1", "2"),
    failtrial=c("nonf", "fail")
  ),
  constants = c(st0 = 0, sd_v.false = 1, a.man=0
  ),
  responses = c("N", "C"),type = "norm"
)


auto_thresholds_top_p.vector  <- c(t0=0.3,A=3,
                                sd_v.true = 1,
               
  B.MANC1=1,         B.MANN1=1,         
  B.AnNS1=1,         B.AnNF1=1,        
  B.AnCS1=1,         B.AnCF1=1,        
  B.AcNS1=1,         B.AcNF1=1,        
  B.AcCS1=1,        
  B.AcCF1=1,         B.MANC2=1,        
  B.MANN2=1,         B.AnNS2=1,        
  B.AnNF2=1,        
  B.AnCS2=1,         B.AnCF2=1,       
  B.AcNS2=1,         B.AcNF2=1,        
  B.AcCS2=1,         B.AcCF2=1,
  
  mean_v.nn.true=0,  mean_v.cc.true=0, 
  mean_v.nn.false=0, mean_v.cc.false=0,
  a.anT=0, a.anF=0, a.acT=0, a.acF=0
 )

check.p.vector(auto_thresholds_top_p.vector, auto_thresholds_top_model)

auto_thresholds_top_p.prior <- prior.p.dmc(
  dists = rep("tnorm", length(auto_thresholds_top_p.vector)),
  p1=auto_thresholds_top_p.vector,                           
  p2=c(1,1,1,rep(1, 20), rep(2, 8)),
  lower=c(0.1, 0,0, rep(0, 20), rep(NA, 8)),
  upper=c(5,10, rep(Inf, length(auto_thresholds_top_p.vector)-2))
)

auto_thresholds_top_dm <- data.model.dmc(cleandats,
                                   auto_thresholds_top_model)

auto_thresholds_top_samples <- h.samples.dmc(nmc = 180,
                                          auto_thresholds_top_p.prior,
                                          auto_thresholds_top_dm, thin=20)

save(auto_thresholds_top_samples, file="auto_thresholds_top_samples.RData")