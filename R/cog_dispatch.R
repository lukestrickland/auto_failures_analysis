
rm(list=ls())
source("dmc/dmc.R")
load_model("LBA","lba_B_autothres.R")

load("~/auto_failures_analysis/CA_top_thresholdsmult_samples.RData")

CA_top_thresholdsmult_samples <- h.RUN.dmc(CA_top_thresholdsmult_samples,
                                           cores = length(CA_top_thresholdsmult_samples))
save(CA_top_thresholdsmult_samples, file= "CA_top_thresholdsmult_samples.RData")

rm(list=ls())
source("dmc/dmc.R")
load_model("LBA","lba_B.R")

load("~/auto_failures_analysis/CA_top_thresholds_samples.RData")

CA_top_thresholds_samples <- h.RUN.dmc(CA_top_thresholds_samples,
                                           cores = length(CA_top_thresholds_samples))

save(CA_top_thresholds_samples, file= "CA_top_thresholds_samples.RData")
