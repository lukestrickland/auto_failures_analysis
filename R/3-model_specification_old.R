
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

#Auto success/fail model


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


#auto fail models with different svs




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




auto_topsv_model <- model.dmc(
  p.map = list(
    A = "1",B = c("cond", "sess", "R"), t0 = "1", mean_v = c("S", "M"),
    sd_v = c( "cond", "M"), st0 = "1", a= c("MAPAUTO")),
  match.map = list(
    M = list(nn = "N", cc="C"),
    MAPAUTO = mapauto
  ),
  factors = list(
    S = c("nn", "cc"), cond = c("A", "M"), sess = c("1", "2"),
    failtrial=c("nonf", "fail")
  ),
  constants = c(st0 = 0, sd_v.M.false = 1, sd_v.A.false=1, a.man=0
  ),
  responses = c("N", "C"),type = "norm"
)


auto_topsv_p.vector  <- c(t0=0.3,A=3,
                                sd_v.M.true = 1, sd_v.A.true = 1,
               
  B.A.1.N=2, B.M.1.N=2,             
   B.A.2.N=2, B.M.2.N=2, B.A.1.C=2,             
  B.M.1.C=2, B.A.2.C=2,   B.M.2.C=2, 
  
  mean_v.nn.true=0,  mean_v.cc.true=0, 
  mean_v.nn.false=0, mean_v.cc.false=0,
  a.anT=0, a.anF=0, a.acT=0, a.acF=0
 )

check.p.vector(auto_topsv_p.vector, auto_topsv_model)

auto_topsv_p.prior <- prior.p.dmc(
  dists = rep("tnorm", length(auto_topsv_p.vector)),
  p1=auto_topsv_p.vector,                           
  p2=c(1,1,1,1,rep(1, 8), rep(2, 8)),
  lower=c(0.1, 0,0,0, rep(0, 8), rep(NA, 8)),
  upper=c(5,10, rep(Inf, length(auto_topsv_p.vector)-2))
)

auto_topsv_dm <- data.model.dmc(cleandats,
                                   auto_topsv_model)

auto_topsv_samples <- h.samples.dmc(nmc = 180,
                                          auto_topsv_p.prior,
                                          auto_topsv_dm, thin=20)

save(auto_topsv_samples, file="auto_topsv_samples.RData")






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




auto_topsvf_model <- model.dmc(
  p.map = list(
    A = "1",B = c("cond", "sess", "R"), t0 = "1", mean_v = c("S", "M"),
    sd_v = c( "cond", "M"), st0 = "1", a= c("MAPAUTO")),
  match.map = list(
    M = list(nn = "N", cc="C"),
    MAPAUTO = mapauto
  ),
  factors = list(
    S = c("nn", "cc"), cond = c("A", "M"), sess = c("1", "2"),
    failtrial=c("nonf", "fail")
  ),
  constants = c(st0 = 0, sd_v.M.false = 1, a.man=0
  ),
  responses = c("N", "C"),type = "norm"
)


auto_topsvf_p.vector  <- c(t0=0.3,A=3,
                                sd_v.M.true = 1, sd_v.A.true = 1,
                           sd_v.A.false=1, 
               
  B.A.1.N=2, B.M.1.N=2,             
   B.A.2.N=2, B.M.2.N=2, B.A.1.C=2,             
  B.M.1.C=2, B.A.2.C=2,   B.M.2.C=2, 
  
  mean_v.nn.true=0,  mean_v.cc.true=0, 
  mean_v.nn.false=0, mean_v.cc.false=0,
  a.anT=0, a.anF=0, a.acT=0, a.acF=0
 )

check.p.vector(auto_topsvf_p.vector, auto_topsvf_model)

auto_topsvf_p.prior <- prior.p.dmc(
  dists = rep("tnorm", length(auto_topsvf_p.vector)),
  p1=auto_topsvf_p.vector,                           
  p2=c(1,1,1,1,1,rep(1, 8), rep(2, 8)),
  lower=c(0.1, 0,0,0,0, rep(0, 8), rep(NA, 8)),
  upper=c(5,10, rep(Inf, length(auto_topsvf_p.vector)-2))
)

auto_topsvf_dm <- data.model.dmc(cleandats,
                                   auto_topsvf_model)

auto_topsvf_samples <- h.samples.dmc(nmc = 180,
                                          auto_topsvf_p.prior,
                                          auto_topsvf_dm, thin=20)

save(auto_topsvf_samples, file="auto_topsvf_samples.RData")


test2 <- RUN.dmc(auto_topsvf_samples[[15]], cores=8, max.try=2)











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




auto_topsvft0_model <- model.dmc(
  p.map = list(
    A = "1",B = c("cond", "sess", "R"), t0 = c("cond"), mean_v = c("S", "M"),
    sd_v = c( "cond", "M"), st0 = "1", a= c("MAPAUTO")),
  match.map = list(
    M = list(nn = "N", cc="C"),
    MAPAUTO = mapauto
  ),
  factors = list(
    S = c("nn", "cc"), cond = c("A", "M"), sess = c("1", "2"),
    failtrial=c("nonf", "fail")
  ),
  constants = c(st0 = 0, sd_v.M.false = 1, a.man=0
  ),
  responses = c("N", "C"),type = "norm"
)


auto_topsvft0_p.vector  <- c(t0.M=0.3,t0.A=0.3,A=3,
                                sd_v.M.true = 1, sd_v.A.true = 1,
                           sd_v.A.false=1, 
               
  B.A.1.N=2, B.M.1.N=2,             
   B.A.2.N=2, B.M.2.N=2, B.A.1.C=2,             
  B.M.1.C=2, B.A.2.C=2,   B.M.2.C=2, 
  
  mean_v.nn.true=0,  mean_v.cc.true=0, 
  mean_v.nn.false=0, mean_v.cc.false=0,
  a.anT=0, a.anF=0, a.acT=0, a.acF=0
 )

check.p.vector(auto_topsvft0_p.vector, auto_topsvft0_model)

auto_topsvft0_p.prior <- prior.p.dmc(
  dists = rep("tnorm", length(auto_topsvft0_p.vector)),
  p1=auto_topsvft0_p.vector,                           
  p2=c(1,1,1,1,1,1,rep(1, 8), rep(2, 8)),
  lower=c(0.1, 0.1,0,0,0,0, rep(0, 8), rep(NA, 8)),
  upper=c(5,5,10, rep(Inf, length(auto_topsvft0_p.vector)-3))
)

auto_topsvft0_dm <- data.model.dmc(cleandats,
                                   auto_topsvft0_model)

auto_topsvft0_samples <- h.samples.dmc(nmc = 180,
                                          auto_topsvft0_p.prior,
                                          auto_topsvft0_dm, thin=20)

save(auto_topsvft0_samples, file="auto_topsvft0_samples.RData")


test3 <- RUN.dmc(auto_topsvft0_samples[[15]], cores=8, max.try=1)










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




auto_topsvSft0_model <- model.dmc(
  p.map = list(
    A = "1",B = c("cond", "sess", "R"), t0 = c("cond"), mean_v = c("S", "M"),
    sd_v = c("S", "cond", "M"), st0 = "1", a= c("MAPAUTO")),
  match.map = list(
    M = list(nn = "N", cc="C"),
    MAPAUTO = mapauto
  ),
  factors = list(
    S = c("nn", "cc"), cond = c("A", "M"), sess = c("1", "2"),
    failtrial=c("nonf", "fail")
  ),
  constants = c(st0 = 0, sd_v.cc.M.false = 1, a.man=0
  ),
  responses = c("N", "C"),type = "norm"
)


auto_topsvSft0_p.vector  <- c(t0.M=0.3,t0.A=0.3,A=3,
                                sd_v.cc.M.true = 1, sd_v.cc.A.true = 1,
                           sd_v.cc.A.false=1, 
                           
                               sd_v.nn.M.true = 1, sd_v.nn.A.true = 1,
                           sd_v.nn.A.false=1,  sd_v.nn.M.false=1, 
                           
               
  B.A.1.N=2, B.M.1.N=2,             
   B.A.2.N=2, B.M.2.N=2, B.A.1.C=2,             
  B.M.1.C=2, B.A.2.C=2,   B.M.2.C=2, 
  
  mean_v.nn.true=0,  mean_v.cc.true=0, 
  mean_v.nn.false=0, mean_v.cc.false=0,
  a.anT=0, a.anF=0, a.acT=0, a.acF=0
 )

check.p.vector(auto_topsvSft0_p.vector, auto_topsvSft0_model)

auto_topsvSft0_p.prior <- prior.p.dmc(
  dists = rep("tnorm", length(auto_topsvSft0_p.vector)),
  p1=auto_topsvSft0_p.vector,                           
  p2=c(1,1,1,1,1,1,1,1,1,1,rep(1, 8), rep(2, 8)),
  lower=c(0.1, 0.1,0,0,0,0,0,0,0,0, rep(0, 8), rep(NA, 8)),
  upper=c(5,5,10, rep(Inf, length(auto_topsvSft0_p.vector)-3))
)

auto_topsvSft0_dm <- data.model.dmc(cleandats,
                                   auto_topsvSft0_model)

auto_topsvSft0_samples <- h.samples.dmc(nmc = 180,
                                          auto_topsvSft0_p.prior,
                                          auto_topsvSft0_dm, thin=20)

save(auto_topsvSft0_samples, file="auto_topsvSft0_samples.RData")


test4 <- RUN.dmc(auto_topsvSft0_samples[[15]], cores=8, max.try=1)







source("dmc/dmc.R")
load_model("LBA", "lba_B_autolog.R")

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




auto_log_model <- model.dmc(
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


auto_log_p.vector  <- c(t0=0.3,A=3,
                                sd_v.true = 1,
               
  B.A.1.N=2, B.M.1.N=2,             
   B.A.2.N=2, B.M.2.N=2, B.A.1.C=2,             
  B.M.1.C=2, B.A.2.C=2,   B.M.2.C=2, 
  
  mean_v.nn.true=0,  mean_v.cc.true=0, 
  mean_v.nn.false=0, mean_v.cc.false=0,
  a.anT=0, a.anF=0, a.acT=0, a.acF=0
 )

check.p.vector(auto_log_p.vector, auto_log_model)

auto_log_p.prior <- prior.p.dmc(
  dists = rep("tnorm", length(auto_log_p.vector)),
  p1=auto_log_p.vector,                           
  p2=c(1,1,1,rep(1, 8), rep(2, 8)),
  lower=c(0.1, 0,0, rep(0, 8), rep(0, 8)),
  upper=c(5,10, rep(Inf, length(auto_log_p.vector)-2))
)

auto_log_dm <- data.model.dmc(cleandats,
                                   auto_log_model)

auto_log_samples <- h.samples.dmc(nmc = 180,
                                          auto_log_p.prior,
                                          auto_log_dm, thin=20)

save(auto_log_samples, file="auto_log_samples.RData")





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




auto_sv_model <- model.dmc(
  p.map = list(
    A = "1",B = c("cond", "sess", "R"), t0 = "1", mean_v = c("S", "M"),
    sd_v = c("1"), st0 = "1", a= c("MAPAUTO")),
  match.map = list(
    M = list(nn = "N", cc="C"),
    MAPAUTO = mapauto
  ),
  factors = list(
    S = c("nn", "cc"), cond = c("A", "M"), sess = c("1", "2"),
    failtrial=c("nonf", "fail")
  ),
  constants = c(st0 = 0, sd_v = 1, a.man=0
  ),
  responses = c("N", "C"),type = "norm"
)


auto_sv_p.vector  <- c(t0=0.3,A=3,
               
  B.A.1.N=2, B.M.1.N=2,             
   B.A.2.N=2, B.M.2.N=2, B.A.1.C=2,             
  B.M.1.C=2, B.A.2.C=2,   B.M.2.C=2, 
  
  mean_v.nn.true=0,  mean_v.cc.true=0, 
  mean_v.nn.false=0, mean_v.cc.false=0,
  a.anT=0, a.anF=0, a.acT=0, a.acF=0
 )

check.p.vector(auto_sv_p.vector, auto_sv_model)

auto_sv_p.prior <- prior.p.dmc(
  dists = rep("tnorm", length(auto_sv_p.vector)),
  p1=auto_sv_p.vector,                           
  p2=c(1,1,rep(1, 8), rep(2, 8)),
  lower=c(0.1, 0, rep(0, 8), rep(NA, 8)),
  upper=c(5,10, rep(Inf, length(auto_sv_p.vector)-2))
)

auto_sv_dm <- data.model.dmc(cleandats,
                                   auto_sv_model)

auto_sv_samples <- h.samples.dmc(nmc = 180,
                                          auto_sv_p.prior,
                                          auto_sv_dm, thin=20)

save(auto_sv_samples, file="auto_sv_samples.RData")







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




auto_sv_nothres_model <- model.dmc(
  p.map = list(
    A = "1",B = c("sess", "R"), t0 = "1", mean_v = c("S", "M"),
    sd_v = c("1"), st0 = "1", a= c("MAPAUTO")),
  match.map = list(
    M = list(nn = "N", cc="C"),
    MAPAUTO = mapauto
  ),
  factors = list(
    S = c("nn", "cc"), cond = c("A", "M"), sess = c("1", "2"),
    failtrial=c("nonf", "fail")
  ),
  constants = c(st0 = 0, sd_v = 1, a.man=0
  ),
  responses = c("N", "C"),type = "norm"
)


auto_sv_nothres_p.vector  <- c(t0=0.3,A=3,
               
  B.1.N=2,B.2.N =2, B.1.C=2,  B.2.C=2,
  mean_v.nn.true=0,  mean_v.cc.true=0, 
  mean_v.nn.false=0, mean_v.cc.false=0,
  a.anT=0, a.anF=0, a.acT=0, a.acF=0
 )

check.p.vector(auto_sv_nothres_p.vector, auto_sv_nothres_model)

auto_sv_nothres_p.prior <- prior.p.dmc(
  dists = rep("tnorm", length(auto_sv_nothres_p.vector)),
  p1=auto_sv_nothres_p.vector,                           
  p2=c(1,1,rep(1, 4), rep(2, 8)),
  lower=c(0.1, 0, rep(0, 4), rep(NA, 8)),
  upper=c(5,10, rep(Inf, length(auto_sv_nothres_p.vector)-2))
)

auto_sv_nothres_dm <- data.model.dmc(cleandats,
                                   auto_sv_nothres_model)

auto_sv_nothres_samples <- h.samples.dmc(nmc = 180,
                                          auto_sv_nothres_p.prior,
                                          auto_sv_nothres_dm, thin=20)

save(auto_sv_nothres_samples, file="auto_sv_nothres_samples.RData")





test <- samples.dmc(p.prior=auto_top_p.prior, auto_top_dm[[15]], nmc=180, thin=5)







source("dmc/dmc.R")
load_model("LBA", "lba_B_automult.R")

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


automult_top_model <- model.dmc(
  p.map = list(
    A = "1",B = c("cond", "sess", "R"), t0 = "1", mean_v = c("S", "M"),
    sd_v = c("M"), st0 = "1", a= c("MAPauto")),
  match.map = list(
    M = list(nn = "N", cc="C"),
    MAPauto = mapauto
  ),
  factors = list(
    S = c("nn", "cc"), cond = c("A", "M"), sess = c("1", "2"),
    failtrial=c("nonf", "fail")
  ),
  constants = c(st0 = 0, sd_v.false = 1, a.man=1
  ),
  responses = c("N", "C"),type = "norm"
)


automult_top_p.vector  <- c(t0=0.3,A=3,
                                sd_v.true = 1,
               
  B.A.1.N=2, B.M.1.N=2,             
   B.A.2.N=2, B.M.2.N=2, B.A.1.C=2,             
  B.M.1.C=2, B.A.2.C=2,   B.M.2.C=2, 
  
  mean_v.nn.true=0,  mean_v.cc.true=0, 
  mean_v.nn.false=0, mean_v.cc.false=0,
  a.anT=1, a.anF=1, a.acT=1, a.acF=1
 )

check.p.vector(automult_top_p.vector, automult_top_model)

automult_top_p.prior <- prior.p.dmc(
  dists = rep("tnorm", length(automult_top_p.vector)),
  p1=automult_top_p.vector,                           
  p2=c(1,1,1,rep(1, 8), rep(2, 4), rep(0.5, 4)),
  lower=c(0.1, 0,0, rep(0, 8), rep(0, 8)),
  upper=c(5,10, rep(Inf, length(automult_top_p.vector)-2))
)

automult_top_dm <- data.model.dmc(cleandats,
                                   automult_top_model)

automult_top_samples <- h.samples.dmc(nmc = 180,
                                          automult_top_p.prior,
                                          automult_top_dm, thin=20)

save(automult_top_samples, file="automult_top_samples.RData")







source("dmc/dmc.R")
load_model("LBA", "lba_B_automult.R")

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


automultsv_top_model <- model.dmc(
  p.map = list(
    A = "1",B = c("cond", "sess", "R"), t0 = "1", mean_v = c("S", "M"),
    sd_v = c("cond", "M"), st0 = "1", a= c("MAPauto")),
  match.map = list(
    M = list(nn = "N", cc="C"),
    MAPauto = mapauto
  ),
  factors = list(
    S = c("nn", "cc"), cond = c("A", "M"), sess = c("1", "2"),
    failtrial=c("nonf", "fail")
  ),
  constants = c(st0 = 0, sd_v.M.false = 1, a.man=1
  ),
  responses = c("N", "C"),type = "norm"
)


automultsv_top_p.vector  <- c(t0=0.3,A=3,
  sd_v.A.true = 1, sd_v.M.true = 1,
  sd_v.A.false = 1, 
  B.A.1.N=2, B.M.1.N=2,             
   B.A.2.N=2, B.M.2.N=2, B.A.1.C=2,             
  B.M.1.C=2, B.A.2.C=2,   B.M.2.C=2, 
  
  mean_v.nn.true=0,  mean_v.cc.true=0, 
  mean_v.nn.false=0, mean_v.cc.false=0,
  a.anT=1, a.anF=1, a.acT=1, a.acF=1
 )

check.p.vector(automultsv_top_p.vector, automultsv_top_model)

automultsv_top_p.prior <- prior.p.dmc(
  dists = rep("tnorm", length(automultsv_top_p.vector)),
  p1=automultsv_top_p.vector,                           
  p2=c(1,1,1,1,1,rep(1, 8), rep(2, 4), rep(0.5, 4)),
  lower=c(0.1, 0,0,0,0, rep(0, 8), rep(0, 8)),
  upper=c(5,10, rep(Inf, length(automultsv_top_p.vector)-2))
)

automultsv_top_dm <- data.model.dmc(cleandats,
                                   automultsv_top_model)

automultsv_top_samples <- h.samples.dmc(nmc = 180,
                                          automultsv_top_p.prior,
                                          automultsv_top_dm, thin=20)

save(automultsv_top_samples, file="automultsv_top_samples.RData")




source("dmc/dmc.R")
load_model("LBA", "lba_B_automult.R")

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


automult_sv_inh_top_model <- model.dmc(
  p.map = list(
    A = "1",B = c("cond", "sess", "R"), t0 = "1", mean_v = c("S", "M"),
    sd_v = c("cond", "M"), st0 = "1", a= c("MAPauto")),
  match.map = list(
    M = list(nn = "N", cc="C"),
    MAPauto = mapauto
  ),
  factors = list(
    S = c("nn", "cc"), cond = c("A", "M"), sess = c("1", "2"),
    failtrial=c("nonf", "fail")
  ),
  constants = c(st0 = 0, sd_v.M.false = 1, a.man=1, a.anT=1, a.acT=1
  ),
  responses = c("N", "C"),type = "norm"
)


automult_sv_inh_top_p.vector  <- c(t0=0.3,A=3,
  sd_v.A.true = 1, sd_v.M.true = 1,
  sd_v.A.false = 1, 
  B.A.1.N=2, B.M.1.N=2,             
   B.A.2.N=2, B.M.2.N=2, B.A.1.C=2,             
  B.M.1.C=2, B.A.2.C=2,   B.M.2.C=2, 
  
  mean_v.nn.true=0,  mean_v.cc.true=0, 
  mean_v.nn.false=0, mean_v.cc.false=0,
  a.anF=1,  a.acF=1
 )

check.p.vector(automult_sv_inh_top_p.vector, automult_sv_inh_top_model)

automult_sv_inh_top_p.prior <- prior.p.dmc(
  dists = rep("tnorm", length(automult_sv_inh_top_p.vector)),
  p1=automult_sv_inh_top_p.vector,                           
  p2=c(1,1,1,1,1,rep(1, 8), rep(2, 4), rep(0.5, 2)),
  lower=c(0.1, 0,0,0,0, rep(0, 8), rep(0, 6)),
  upper=c(5,10, rep(Inf, length(automult_sv_inh_top_p.vector)-2))
)

automult_sv_inh_top_dm <- data.model.dmc(cleandats,
                                   automult_sv_inh_top_model)

automult_sv_inh_top_samples <- h.samples.dmc(nmc = 180,
                                          automult_sv_inh_top_p.prior,
                                          automult_sv_inh_top_dm, thin=20)

save(automult_sv_inh_top_samples, file="automult_sv_inh_top_samples.RData")



source("dmc/dmc.R")
load_model("LBA", "lba_B_automult.R")

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


automult_sv_inh_nothres_top_model <- model.dmc(
  p.map = list(
    A = "1",B = c("sess", "R"), t0 = "1", mean_v = c("S", "M"),
    sd_v = c("cond", "M"), st0 = "1", a= c("MAPauto")),
  match.map = list(
    M = list(nn = "N", cc="C"),
    MAPauto = mapauto
  ),
  factors = list(
    S = c("nn", "cc"), cond = c("A", "M"), sess = c("1", "2"),
    failtrial=c("nonf", "fail")
  ),
  constants = c(st0 = 0, sd_v.M.false = 1, a.man=1, a.anT=1, a.acT=1
  ),
  responses = c("N", "C"),type = "norm"
)


automult_sv_inh_nothres_top_p.vector  <- c(t0=0.3,A=3,
  sd_v.A.true = 1, sd_v.M.true = 1,
  sd_v.A.false = 1, 
  B.1.N=2,        B.2.N=2, 
  B.1.C=2,         B.2.C=2,   
  mean_v.nn.true=0,  mean_v.cc.true=0, 
  mean_v.nn.false=0, mean_v.cc.false=0,
  a.anF=1,  a.acF=1
 )

check.p.vector(automult_sv_inh_nothres_top_p.vector, automult_sv_inh_nothres_top_model)

automult_sv_inh_nothres_top_p.prior <- prior.p.dmc(
  dists = rep("tnorm", length(automult_sv_inh_nothres_top_p.vector)),
  p1=automult_sv_inh_nothres_top_p.vector,                           
  p2=c(1,1,1,1,1,rep(1, 4), rep(2, 4), rep(0.5, 2)),
  lower=c(0.1, 0,0,0,0, rep(0, 4), rep(0, 6)),
  upper=c(5,10, rep(Inf, length(automult_sv_inh_nothres_top_p.vector)-2))
)

automult_sv_inh_nothres_top_dm <- data.model.dmc(cleandats,
                                   automult_sv_inh_nothres_top_model)

automult_sv_inh_nothres_top_samples <- h.samples.dmc(nmc = 180,
                                          automult_sv_inh_nothres_top_p.prior,
                                          automult_sv_inh_nothres_top_dm, thin=20)

save(automult_sv_inh_nothres_top_samples, file="automult_sv_inh_nothres_top_samples.RData")











source("dmc/dmc.R")
load_model("LBA", "lba_B_automult.R")

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


automultsvS_top_model <- model.dmc(
  p.map = list(
    A = "1",B = c("cond", "sess", "R"), t0 = "1", mean_v = c("S", "M"),
    sd_v = c("cond", "S", "M"), st0 = "1", a= c("MAPauto")),
  match.map = list(
    M = list(nn = "N", cc="C"),
    MAPauto = mapauto
  ),
  factors = list(
    S = c("nn", "cc"), cond = c("A", "M"), sess = c("1", "2"),
    failtrial=c("nonf", "fail")
  ),
  constants = c(st0 = 0, sd_v.M.cc.false = 1, a.man=1
  ),
  responses = c("N", "C"),type = "norm"
)


automultsvS_top_p.vector  <- c(t0=0.3,A=3,
  sd_v.A.nn.true=1,  sd_v.M.nn.true=1, 
  sd_v.A.cc.true=1,  sd_v.M.cc.true=1,  
  sd_v.A.nn.false=1, sd_v.M.nn.false=1, sd_v.A.cc.false=1,
  
  B.A.1.N=2, B.M.1.N=2,             
   B.A.2.N=2, B.M.2.N=2, B.A.1.C=2,             
  B.M.1.C=2, B.A.2.C=2,   B.M.2.C=2, 
  
  mean_v.nn.true=0,  mean_v.cc.true=0, 
  mean_v.nn.false=0, mean_v.cc.false=0,
  a.anT=1, a.anF=1, a.acT=1, a.acF=1
 )

check.p.vector(automultsvS_top_p.vector, automultsvS_top_model)

automultsvS_top_p.prior <- prior.p.dmc(
  dists = rep("tnorm", length(automultsvS_top_p.vector)),
  p1=automultsvS_top_p.vector,                           
  p2=c(1,1,rep(1,7),rep(1, 8), rep(2, 4), rep(0.5, 4)),
  lower=c(0.1, 0,rep(0,7), rep(0, 8), rep(0, 8)),
  upper=c(5,10, rep(Inf, length(automultsvS_top_p.vector)-2))
)

automultsvS_top_dm <- data.model.dmc(cleandats,
                                   automultsvS_top_model)

automultsvS_top_samples <- h.samples.dmc(nmc = 180,
                                          automultsvS_top_p.prior,
                                          automultsvS_top_dm, thin=20)

save(automultsvS_top_samples, file="automultsvS_top_samples.RData")


##Auto mult with no thresholds





source("dmc/dmc.R")
load_model("LBA", "lba_B_automult.R")

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


automultsvS_nothres_top_model <- model.dmc(
  p.map = list(
    A = "1",B = c("sess", "R"), t0 = "1", mean_v = c("S", "M"),
    sd_v = c("cond", "S", "M"), st0 = "1", a= c("MAPauto")),
  match.map = list(
    M = list(nn = "N", cc="C"),
    MAPauto = mapauto
  ),
  factors = list(
    S = c("nn", "cc"), cond = c("A", "M"), sess = c("1", "2"),
    failtrial=c("nonf", "fail")
  ),
  constants = c(st0 = 0, sd_v.M.cc.false = 1, a.man=1
  ),
  responses = c("N", "C"),type = "norm"
)


automultsvS_nothres_top_p.vector  <- c(t0=0.3,A=3,
  sd_v.A.nn.true=1,  sd_v.M.nn.true=1, 
  sd_v.A.cc.true=1,  sd_v.M.cc.true=1,  
  sd_v.A.nn.false=1, sd_v.M.nn.false=1, sd_v.A.cc.false=1,
  
  B.1.N=2,  B.2.N=2,    B.1.C=2,     B.2.C=2, 
  mean_v.nn.true=0,  mean_v.cc.true=0, 
  mean_v.nn.false=0, mean_v.cc.false=0,
  a.anT=1, a.anF=1, a.acT=1, a.acF=1
 )

check.p.vector(automultsvS_nothres_top_p.vector, automultsvS_nothres_top_model)

automultsvS_nothres_top_p.prior <- prior.p.dmc(
  dists = rep("tnorm", length(automultsvS_nothres_top_p.vector)),
  p1=automultsvS_nothres_top_p.vector,                           
  p2=c(1,1,rep(1,7),rep(1, 4), rep(2, 4), rep(0.5, 4)),
  lower=c(0.1, 0,rep(0,7), rep(0, 4), rep(0, 8)),
  upper=c(5,10, rep(Inf, length(automultsvS_nothres_top_p.vector)-2))
)

automultsvS_nothres_top_dm <- data.model.dmc(cleandats,
                                   automultsvS_nothres_top_model)

automultsvS_nothres_top_samples <- h.samples.dmc(nmc = 180,
                                          automultsvS_nothres_top_p.prior,
                                          automultsvS_nothres_top_dm, thin=20)

save(automultsvS_nothres_top_samples, file="automultsvS_nothres_top_samples.RData")









source("dmc/dmc.R")
load_model("LBA", "lba_B_automult.R")

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
             "anTt","anFf","anTT","anFF",
             "acTt", "acFf","acTT", "acFF"))

mapauto[1:32] <- c(
  "anTt","acFf","man", "man",
  "anTT","acFF","man", "man",
  
  "acFf","anTt","man", "man",
  "acFF","anTT","man", "man",
  
   "anFf","acTt","man", "man",
   "anFF","acTT","man", "man",
  
    "acTt","anFf","man", "man",
   "acTT","anFF","man", "man"
  
)


automultsvSmD_top_model <- model.dmc(
  p.map = list(
    A = "1",B = c("cond", "sess", "R"), t0 = "1", mean_v = c("S", "sess", "M"),
    sd_v = c("cond", "S", "M"), st0 = "1", a= c("MAPauto")),
  match.map = list(
    M = list(nn = "N", cc="C"),
    MAPauto = mapauto
  ),
  factors = list(
    S = c("nn", "cc"), cond = c("A", "M"), sess = c("1", "2"),
    failtrial=c("nonf", "fail")
  ),
  constants = c(st0 = 0, sd_v.M.cc.false = 1, a.man=1
  ),
  responses = c("N", "C"),type = "norm"
)


automultsvSmD_top_p.vector  <- c(t0=0.3,A=3,
                                 
  sd_v.A.nn.true=1,  sd_v.M.nn.true=1, 
  sd_v.A.cc.true=1,  sd_v.M.cc.true=1,  
  sd_v.A.nn.false=1, sd_v.M.nn.false=1, sd_v.A.cc.false=1,
  
  B.A.1.N=2, B.M.1.N=2,             
   B.A.2.N=2, B.M.2.N=2, B.A.1.C=2,             
  B.M.1.C=2, B.A.2.C=2,   B.M.2.C=2, 
  
  mean_v.nn.1.true=0,  mean_v.cc.1.true=0, 
  mean_v.nn.1.false=0, mean_v.cc.1.false=0,
  
  mean_v.nn.2.true=0,  mean_v.cc.2.true=0, 
  mean_v.nn.2.false=0, mean_v.cc.2.false=0,

  
  
  a.anTt=1, a.anFf=1, a.acTt=1, a.acFf=1,
  a.anTT=1, a.anFF=1, a.acTT=1, a.acFF=1
  
 )

check.p.vector(automultsvSmD_top_p.vector, automultsvSmD_top_model)

automultsvSmD_top_p.prior <- prior.p.dmc(
  dists = rep("tnorm", length(automultsvSmD_top_p.vector)),
  p1=automultsvSmD_top_p.vector,                           
  p2=c(1,1,rep(1,7),rep(1, 8), rep(2, 8), rep(0.5, 8)),
  lower=c(0.1, 0,rep(0,7), rep(0, 8), rep(0, 16)),
  upper=c(5,10, rep(Inf, length(automultsvSmD_top_p.vector)-2))
)

automultsvSmD_top_dm <- data.model.dmc(cleandats,
                                   automultsvSmD_top_model)

automultsvSmD_top_samples <- h.samples.dmc(nmc = 180,
                                          automultsvSmD_top_p.prior,
                                          automultsvSmD_top_dm, thin=20)

save(automultsvSmD_top_samples, file="automultsvSmD_top_samples.RData")







source("dmc/dmc.R")
load_model("LBA", "lba_B_automult.R")

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
             "FanT","FanF","NanT","NanF",
             "FacT", "FacF", "NacT", "NacF"))

mapauto[1:32] <- c(
  "NanT","NacF","man", "man",
  "NanT","NacF","man", "man",
  
  "FacF","FanT","man", "man",
  "FacF","FanT","man", "man",
  
   "NanF","NacT","man", "man",
   "NanF","NacT","man", "man",
  
    "FacT","FanF","man", "man",
   "FacT","FanF","man", "man"
  
)



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


automult_fail_top_model <- model.dmc(
  p.map = list(
    A = "1",B = c("cond", "sess", "R"), t0 = "1", mean_v = c("S", "M"),
    sd_v = c("M"), st0 = "1", a= c("MAPauto")),
  match.map = list(
    M = list(nn = "N", cc="C"),
    MAPauto = mapauto
  ),
  factors = list(
    S = c("nn", "cc"), cond = c("A", "M"), sess = c("1", "2"),
    failtrial=c("nonf", "fail")
  ),
  constants = c(st0 = 0, sd_v.false = 1, a.man=1
  ),
  responses = c("N", "C"),type = "norm"
)


automult_fail_top_p.vector  <- c(t0=0.3,A=3,
                                sd_v.true = 1,
               
  B.A.1.N=2, B.M.1.N=2,             
   B.A.2.N=2, B.M.2.N=2, B.A.1.C=2,             
  B.M.1.C=2, B.A.2.C=2,   B.M.2.C=2, 
  
  mean_v.nn.true=0,  mean_v.cc.true=0, 
  mean_v.nn.false=0, mean_v.cc.false=0,
  a.FanT=1, a.FanF=1, a.FacT=1, a.FacF=1,
  a.NanT=1, a.NanF=1, a.NacT=1, a.NacF=1
 )

check.p.vector(automult_fail_top_p.vector, automult_fail_top_model)

automult_fail_top_p.prior <- prior.p.dmc(
  dists = rep("tnorm", length(automult_fail_top_p.vector)),
  p1=automult_fail_top_p.vector,                           
  p2=c(1,1,1,rep(1, 8), rep(2, 4), rep(0.5, 8)),
  lower=c(0.1, 0,0, rep(0, 8), rep(0, 12)),
  upper=c(5,10, rep(Inf, length(automult_fail_top_p.vector)-2))
)

automult_fail_top_dm <- data.model.dmc(cleandats,
                                   automult_fail_top_model)

automult_fail_top_samples <- h.samples.dmc(nmc = 180,
                                          automult_fail_top_p.prior,
                                          automult_fail_top_dm, thin=20)

save(automult_fail_top_samples, file="automult_fail_top_samples.RData")



##NEW 2020



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




CA_bound_top_model <- model.dmc(
  p.map = list(
    A = "1",B = c("cond", "sess", "R"), t0 = "1", mean_v = c("S", "cond", "failtrial", "M"),
    sd_v = c("cond", "M"), st0 = "1"),
  match.map = list(
    M = list(nn = "N", cc="C")
  ),
  factors = list(
    S = c("nn", "cc"), cond = c("A", "M"), sess = c("1", "2"),
    failtrial=c("nonf", "fail")
  ),
  constants = c(st0 = 0, sd_v.A.false = 1
  ),
  responses = c("N", "C"),type = "norm"
)


CA_bound_top_p.vector  <- c(t0=0.3,A=3,
                                sd_v.A.true = 1,
                            sd_v.M.true = 1,
                            sd_v.M.false = 1,
               
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

check.p.vector(CA_bound_top_p.vector, CA_bound_top_model)

CA_bound_top_p.prior <- prior.p.dmc(
  dists = rep("tnorm", length(CA_bound_top_p.vector)),
  p1=CA_bound_top_p.vector,                           
  p2=c(1,1,1,1,1,rep(1, 8), rep(2, 16)),
  lower=c(0.1, 0,0,0,0, rep(0, 8), rep(0, 16)),
  upper=c(5,10, rep(Inf, length(CA_bound_top_p.vector)-2))
)

CA_bound_top_dm <- data.model.dmc(cleandats,
                                   CA_bound_top_model)

CA_bound_top_samples <- h.samples.dmc(nmc = 180,
                                          CA_bound_top_p.prior,
                                          CA_bound_top_dm, thin=20)

save(CA_bound_top_samples, file="CA_bound_top_samples.RData")


#CHUCKED - JUST SV TRUE
source("dmc/dmc.R")
load_model("LBA", "lba_B_automult.R")

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


automultsvS_top_model <- model.dmc(
  p.map = list(
    A = "1",B = c("cond", "sess", "R"), t0 = "1", mean_v = c("S", "M"),
    sd_v = c("cond", "S", "M"), st0 = "1", a= c("MAPauto")),
  match.map = list(
    M = list(nn = "N", cc="C"),
    MAPauto = mapauto
  ),
  factors = list(
    S = c("nn", "cc"), cond = c("A", "M"), sess = c("1", "2"),
    failtrial=c("nonf", "fail")
  ),
  constants = c(st0 = 0, sd_v.M.cc.false = 1, 
                
                sd_v.A.nn.false=1, sd_v.M.nn.false=1, sd_v.A.cc.false=1,
                a.man=1
  ),
  responses = c("N", "C"),type = "norm"
)


automultsvS_top_p.vector  <- c(t0=0.3,A=3,
  sd_v.A.nn.true=1,  sd_v.M.nn.true=1, 
  sd_v.A.cc.true=1,  sd_v.M.cc.true=1,  
  
  
  B.A.1.N=2,  B.A.2.N=2,    B.A.1.C=2,     B.A.2.C=2, 
  B.M.1.N=2,  B.M.2.N=2,    B.M.1.C=2,     B.M.2.C=2, 
  
  mean_v.nn.true=0,  mean_v.cc.true=0, 
  mean_v.nn.false=0, mean_v.cc.false=0,
  a.anT=1, a.anF=1, a.acT=1, a.acF=1
 )

check.p.vector(automultsvS_top_p.vector, automultsvS_top_model)

automultsvS_top_p.prior <- prior.p.dmc(
  dists = rep("tnorm", length(automultsvS_top_p.vector)),
  p1=automultsvS_top_p.vector,                           
  p2=c(1,1,rep(1,8),rep(1, 4), rep(2, 4), rep(0.5, 4)),
  lower=c(0.1, 0,rep(0,8), rep(0, 4), rep(0, 8)),
  upper=c(5,10, rep(Inf, length(automultsvS_top_p.vector)-2))
)

automultsvS_top_dm <- data.model.dmc(cleandats,
                                   automultsvS_top_model)

automultsvS_top_samples <- h.samples.dmc(nmc = 180,
                                          automultsvS_top_p.prior,
                                          automultsvS_top_dm, thin=20)

save(automultsvS_top_samples, file="automultsvS_top_samples.RData")





source("dmc/dmc.R")
load_model("LBA", "lba_B_automult.R")

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


automultsvS_nothres_top_model <- model.dmc(
  p.map = list(
    A = "1",B = c("sess", "R"), t0 = "1", mean_v = c("S", "M"),
    sd_v = c("cond", "S", "M"), st0 = "1", a= c("MAPauto")),
  match.map = list(
    M = list(nn = "N", cc="C"),
    MAPauto = mapauto
  ),
  factors = list(
    S = c("nn", "cc"), cond = c("A", "M"), sess = c("1", "2"),
    failtrial=c("nonf", "fail")
  ),
  constants = c(st0 = 0, sd_v.M.cc.false = 1, 
                
                sd_v.A.nn.false=1, sd_v.M.nn.false=1, sd_v.A.cc.false=1,
                a.man=1
  ),
  responses = c("N", "C"),type = "norm"
)


automultsvS_nothres_top_p.vector  <- c(t0=0.3,A=3,
  sd_v.A.nn.true=1,  sd_v.M.nn.true=1, 
  sd_v.A.cc.true=1,  sd_v.M.cc.true=1,  
  
  
  B.1.N=2,  B.2.N=2,    B.1.C=2,     B.2.C=2, 
  mean_v.nn.true=0,  mean_v.cc.true=0, 
  mean_v.nn.false=0, mean_v.cc.false=0,
  a.anT=1, a.anF=1, a.acT=1, a.acF=1
 )

check.p.vector(automultsvS_nothres_top_p.vector, automultsvS_nothres_top_model)

automultsvS_nothres_top_p.prior <- prior.p.dmc(
  dists = rep("tnorm", length(automultsvS_nothres_top_p.vector)),
  p1=automultsvS_nothres_top_p.vector,                           
  p2=c(1,1,rep(1,4),rep(1, 4), rep(2, 4), rep(0.5, 4)),
  lower=c(0.1, 0,rep(0,4), rep(0, 4), rep(0, 8)),
  upper=c(5,10, rep(Inf, length(automultsvS_nothres_top_p.vector)-2))
)

automultsvS_nothres_top_dm <- data.model.dmc(cleandats,
                                   automultsvS_nothres_top_model)

automultsvS_nothres_top_samples <- h.samples.dmc(nmc = 180,
                                          automultsvS_nothres_top_p.prior,
                                          automultsvS_nothres_top_dm, thin=20)

save(automultsvS_nothres_top_samples, file="automultsvS_nothres_top_samples.RData")







source("dmc/dmc.R")
load_model("LBA", "lba_B_automult.R")

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


automultsvS_nothres_inh_top_model <- model.dmc(
  p.map = list(
    A = "1",B = c("sess", "R"), t0 = "1", mean_v = c("S", "M"),
    sd_v = c("cond", "S", "M"), st0 = "1", a= c("MAPauto")),
  match.map = list(
    M = list(nn = "N", cc="C"),
    MAPauto = mapauto
  ),
  factors = list(
    S = c("nn", "cc"), cond = c("A", "M"), sess = c("1", "2"),
    failtrial=c("nonf", "fail")
  ),
  constants = c(st0 = 0, sd_v.M.cc.false = 1, 
                
                sd_v.A.nn.false=1, sd_v.M.nn.false=1, sd_v.A.cc.false=1,
                a.man=1, a.anT=1, a.acT=1
  ),
  responses = c("N", "C"),type = "norm"
)


automultsvS_nothres_inh_top_p.vector  <- c(t0=0.3,A=3,
  sd_v.A.nn.true=1,  sd_v.M.nn.true=1, 
  sd_v.A.cc.true=1,  sd_v.M.cc.true=1,  
  
  
  B.1.N=2,  B.2.N=2,    B.1.C=2,     B.2.C=2, 
  mean_v.nn.true=0,  mean_v.cc.true=0, 
  mean_v.nn.false=0, mean_v.cc.false=0,
   a.anF=1,  a.acF=1
 )

check.p.vector(automultsvS_nothres_inh_top_p.vector, automultsvS_nothres_inh_top_model)

automultsvS_nothres_inh_top_p.prior <- prior.p.dmc(
  dists = rep("tnorm", length(automultsvS_nothres_inh_top_p.vector)),
  p1=automultsvS_nothres_inh_top_p.vector,                           
  p2=c(1,1,rep(1,4),rep(1, 4), rep(2, 4), rep(0.5, 2)),
  lower=c(0.1, 0,rep(0,4), rep(0, 4), rep(0, 6)),
  upper=c(5,10, rep(Inf, length(automultsvS_nothres_inh_top_p.vector)-2))
)

automultsvS_nothres_inh_top_dm <- data.model.dmc(cleandats,
                                   automultsvS_nothres_inh_top_model)

automultsvS_nothres_inh_top_samples <- h.samples.dmc(nmc = 180,
                                          automultsvS_nothres_inh_top_p.prior,
                                          automultsvS_nothres_inh_top_dm, thin=20)

save(automultsvS_nothres_inh_top_samples, file="automultsvS_nothres_inh_top_samples.RData")

