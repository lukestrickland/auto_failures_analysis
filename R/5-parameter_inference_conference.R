source("dmc/dmc.R")
source("dmc/dmc_extras.R")
load_model("LBA","lba_B.R")

theme_set(theme_simple())

get.creds <- function(samples) {
  av.thetas <-fixedeffects.meanthetas(samples)[[1]]
  msds <- cbind(apply(av.thetas, 2, mean), apply(av.thetas, 2, quantile, probs=0.025),
                apply(av.thetas, 2, quantile, probs=0.975)
                )
  colnames(msds) <- c("M", "LCI", "HCI")
  msds <- data.frame(msds)
  msds
}

load("samples_data/CA_top_samples.RData")

msds <- get.creds(auto_anoS_samples[!(names(auto_anoS_samples)=="22")])

Vs <- msds[grep("mean_v", rownames(msds)),]


Vs$Stimulus <- "Conflict"
Vs$Stimulus[grep("nn", rownames(Vs))] <- "Non-conflict"
Vs$match <- "Match"
Vs$match[grep("false", rownames(Vs))] <- "Mismatch"

ggplot(Vs, aes(factor(match),M)) + 
  geom_point(stat = "identity",aes(shape=Stimulus), size=2.5) +
  geom_errorbar(aes(ymax = HCI, ymin = LCI, width = 0.3))+ 
  ylab("Accumulation Rate \n (no automation input)") + xlab("")+
  geom_line(aes(y=M, group=Stimulus ), linetype=2) +xlab("Accumulator")

quantity_nn_NF <- function (thetas) ((thetas[,"mean_v.nn.A.nonf.true",, drop=F] + thetas[,"mean_v.nn.A.nonf.false",, drop=F]) - 
                               (thetas[,"mean_v.nn.M.nonf.true",, drop=F] + thetas[,"mean_v.nn.M.nonf.false",, drop=F]))

zandp(CA_top_samples, quantity_nn_NF)


quantity_cc_NF <- function (thetas) ((thetas[,"mean_v.cc.A.nonf.true",, drop=F] + thetas[,"mean_v.cc.A.nonf.false",, drop=F]) - 
                               (thetas[,"mean_v.cc.M.nonf.true",, drop=F] + thetas[,"mean_v.cc.M.nonf.false",, drop=F]))

zandp(CA_top_samples, quantity_cc_NF)


quantity_nn_F <- function (thetas) ((thetas[,"mean_v.nn.A.fail.true",, drop=F] + thetas[,"mean_v.nn.A.fail.false",, drop=F]) - 
                               (thetas[,"mean_v.nn.M.fail.true",, drop=F] + thetas[,"mean_v.nn.M.fail.false",, drop=F]))

zandp(CA_top_samples, quantity_nn_F)


quantity_cc_F <- function (thetas) ((thetas[,"mean_v.cc.A.fail.true",, drop=F] + thetas[,"mean_v.cc.A.fail.false",, drop=F]) - 
                               (thetas[,"mean_v.cc.M.fail.true",, drop=F] + thetas[,"mean_v.cc.M.fail.false",, drop=F]))

zandp(CA_top_samples, quantity_cc_F)

#quantity is much lower in the auto condition on non-failure trials, not much lower on failure trials


quality_nn_NF <- function (thetas) ((thetas[,"mean_v.nn.A.nonf.true",, drop=F] - thetas[,"mean_v.nn.A.nonf.false",, drop=F]) - 
                               (thetas[,"mean_v.nn.M.nonf.true",, drop=F] - thetas[,"mean_v.nn.M.nonf.false",, drop=F]))

zandp(CA_top_samples, quality_nn_NF)


quality_cc_NF <- function (thetas) ((thetas[,"mean_v.cc.A.nonf.true",, drop=F] - thetas[,"mean_v.cc.A.nonf.false",, drop=F]) - 
                               (thetas[,"mean_v.cc.M.nonf.true",, drop=F] - thetas[,"mean_v.cc.M.nonf.false",, drop=F]))

zandp(CA_top_samples, quality_cc_NF)


quality_nn_F <- function (thetas) ((thetas[,"mean_v.nn.A.fail.true",, drop=F] - thetas[,"mean_v.nn.A.fail.false",, drop=F]) - 
                               (thetas[,"mean_v.nn.M.fail.true",, drop=F] - thetas[,"mean_v.nn.M.fail.false",, drop=F]))

zandp(CA_top_samples, quality_nn_F)


quality_cc_F <- function (thetas) ((thetas[,"mean_v.cc.A.fail.true",, drop=F] - thetas[,"mean_v.cc.A.fail.false",, drop=F]) - 
                               (thetas[,"mean_v.cc.M.fail.true",, drop=F] - thetas[,"mean_v.cc.M.fail.false",, drop=F]))

zandp(CA_top_samples, quality_cc_F)


####Check whether automation inputs are approximately

test <- function (thetas) ((thetas[,"mean_v.nn.A.nonf.true",, drop=F] + thetas[,"mean_v.nn.A.nonf.false",, drop=F]) - 
                               (thetas[,"mean_v.nn.A.fail.true",, drop=F] + thetas[,"mean_v.nn.A.fail.false",, drop=F]))

zandp(CA_top_samples, test)
mean.sd(CA_top_samples, test)

test <- function (thetas) ((thetas[,"mean_v.cc.A.nonf.true",, drop=F] + thetas[,"mean_v.cc.A.nonf.false",, drop=F]) - 
                               (thetas[,"mean_v.cc.A.fail.true",, drop=F] + thetas[,"mean_v.cc.A.fail.false",, drop=F]))

zandp(CA_top_samples, test)
mean.sd(CA_top_samples, test)


test <- function (thetas) ((thetas[,"mean_v.cc.A.nonf.true",, drop=F] + thetas[,"mean_v.cc.A.nonf.false",, drop=F]) - 
                               (thetas[,"mean_v.cc.A.fail.true",, drop=F] + thetas[,"mean_v.cc.A.fail.false",, drop=F]))

zandp(CA_top_samples, test)

mean.sd(CA_top_samples, test)

mean.sd(CA_top_samples, function (thetas) thetas[,"mean_v.cc.A.nonf.true",, drop=F] + thetas[,"mean_v.cc.A.nonf.false",, drop=F])

mean.sd(CA_top_samples, function (thetas) thetas[,"mean_v.cc.A.fail.true",, drop=F] + thetas[,"mean_v.cc.A.fail.false",, drop=F])



test <- function (thetas) ((thetas[,"mean_v.cc.M.nonf.true",, drop=F] - thetas[,"mean_v.cc.M.nonf.false",, drop=F]) - 
                               (thetas[,"mean_v.cc.A.nonf.true",, drop=F] - thetas[,"mean_v.cc.A.fail.false",, drop=F]))

zandp(CA_top_samples, test)
mean.sd(CA_top_samples, test)

test <- function (thetas) ((thetas[,"mean_v.nn.M.nonf.true",, drop=F] - thetas[,"mean_v.nn.M.nonf.false",, drop=F]) - 
                               (thetas[,"mean_v.nn.A.nonf.true",, drop=F] - thetas[,"mean_v.nn.A.fail.false",, drop=F]))

zandp(CA_top_samples, test)
mean.sd(CA_top_samples, test)




mean.sd(CA_top_samples, function (thetas) thetas[,"mean_v.cc.M.nonf.true",, drop=F] - thetas[,"mean_v.cc.M.nonf.false",, drop=F])
mean.sd(CA_top_samples, function (thetas) thetas[,"mean_v.cc.A.nonf.true",, drop=F] - thetas[,"mean_v.cc.A.fail.false",, drop=F])



#the amount automation false chips away at your rate
#False rates, nonfailure trials
mean.sd(CA_top_samples,  function(thetas)
  thetas[,"mean_v.cc.M.nonf.false",, drop=F] - thetas[,"mean_v.cc.A.nonf.false",, drop=F])

#True rates, failure trials
mean.sd(CA_top_samples,  function(thetas)
  thetas[,"mean_v.cc.M.nonf.true",, drop=F] - thetas[,"mean_v.cc.A.fail.true",, drop=F])


mean.sd(CA_top_samples,  function(thetas)
  thetas[,"mean_v.nn.M.nonf.false",, drop=F] - thetas[,"mean_v.nn.A.nonf.false",, drop=F])

#True rates, failure trials
mean.sd(CA_top_samples,  function(thetas)
  thetas[,"mean_v.nn.M.nonf.true",, drop=F] - thetas[,"mean_v.nn.A.fail.true",, drop=F])




#Conclusion: automation inhibits your false accumulators more on non-failure trials,
#than it does your true accumulators on failure trials




test <- function (thetas) ((thetas[,"mean_v.cc.M.nonf.false",, drop=F] - thetas[,"mean_v.cc.A.fail.false",, drop=F]) - 
                               (thetas[,"mean_v.cc.A.fail.true",, drop=F] - thetas[,"mean_v.cc.A.fail.false",, drop=F]))

zandp(CA_top_samples, test)
mean.sd(CA_top_samples, test)



#On fail trials, manual correct rate > auto
mean.sd(CA_top_samples, 
        function (thetas)
          thetas[,"mean_v.cc.M.fail.true",, drop=F] - 
           thetas[,"mean_v.cc.A.fail.true",, drop=F])

#On non-fail trials,manual error rate > auto
mean.sd(CA_top_samples, 
        function (thetas)
          thetas[,"mean_v.cc.M.nonf.false",, drop=F] - 
           thetas[,"mean_v.cc.A.nonf.false",, drop=F])

#On fail trials, manual error rate < auto
mean.sd(CA_top_samples, 
        function (thetas)
          thetas[,"mean_v.cc.M.fail.false",, drop=F] - 
           thetas[,"mean_v.cc.A.fail.false",, drop=F])

#On nonfail trials, auto correct rate and manual nsd
mean.sd(CA_top_samples, 
        function (thetas)
          thetas[,"mean_v.cc.M.nonf.true",, drop=F] - 
           thetas[,"mean_v.cc.A.nonf.true",, drop=F])





Bs <- msds[grep("B", rownames(msds)),]

Bs$R <- "Non-conflict"
Bs$R[grep("C", rownames(Bs))] <- "Conflict"
Bs$Cond <- "Automation"
Bs$Cond[grep("M", rownames(Bs))] <- "Manual"
Bs$Session <- "Session One"
Bs$Session[grep("2", rownames(Bs))] <- "Session Two"

ggplot(Bs, aes(factor(R),M)) + 
  geom_point(stat = "identity",aes(col=Cond), size=2.5) +
  geom_errorbar(aes(ymax = HCI, ymin = LCI, width = 0.3, col=Cond))+ 
  ylab("Threshold") + xlab("Accumulator")+
  geom_line(aes(y=M, group=Cond, col=Cond), linetype=2) +
  facet_grid(.~Session)


#Automation input

as <- msds[grep("a\\.", rownames(msds)),]

as$input <- "Excitation"
as$input[2] <- "Inhibition"
as[2,1:3] <- as[2,1:3] *-1

ggplot(as, aes(factor(input),M)) + 
  geom_point(stat = "identity", size=2.5) +
  geom_errorbar(aes(ymax = HCI, ymin = LCI, width = 0.3)) +xlab ("Automation Input") +ylab("")+
  ylim(-0.1, 0.7) + geom_hline(aes(yintercept=0), linetype=2)

+ 
  ylab("Threshold") + xlab("Accumulator")+
  geom_line(aes(y=M, group=Cond, col=Cond), linetype=2) +
  facet_grid(.~Session)








