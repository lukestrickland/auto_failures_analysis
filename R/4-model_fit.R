source("dmc/dmc.R")
source("dmc/dmc_extras.R")
load_model("LBA","lba_B.R")
library(gridExtra)
theme_set(theme_simple())

load("samples_data/CA_top_samples.RData")

theme_set(theme_simple())

pp <- h.post.predict.dmc(CA_top_samples, factors=c("S", "cond", "failtrial"),
                         cores=12, gglist=TRUE)

ggplot.RA.dmc(pp, xaxis="cond")

ggplot.RT.dmc(pp, xaxis="cond")
