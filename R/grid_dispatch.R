
source("dmc/dmc.R")


load_model("LBA", "lba_B.R")
run.grid.dmc("CA_top_samples",model.dir ="LBA",
             model.file="lba_B.R",user="ljs392",
             n.add=60, wall.hours = 300,
             GB = 2, max.try=5)

load_model("LBA", "lba_B_autothres.R")
run.grid.dmc("CA_top_thresholdsmult_samples",model.dir ="LBA",
             model.file="lba_B_autothres.R",user="ljs392",
             n.add=60, wall.hours = 300,
             GB = 2, max.try=5)

load_model("LBA","lba_B_automation.R")
run.grid.dmc("auto_top_samples",model.dir ="LBA",
             model.file="lba_B_automation.R",user="ljs392",
             n.add=60, wall.hours = 300,
             GB = 2, max.try=5)

load_model("LBA","lba_B_automation.R")
run.grid.dmc("auto_noB_samples",model.dir ="LBA",
             model.file="lba_B_automation.R",user="ljs392",
             n.add=60, wall.hours = 300,
             GB = 2, max.try=5)

load_model("LBA","lba_B_automation.R")
run.grid.dmc("auto_anoS_samples",model.dir ="LBA",
             model.file="lba_B_automation.R",user="ljs392",
             n.add=60, wall.hours = 300,
             GB = 2, max.try=5)

load_model("LBA","lba_B_automation.R")
run.grid.dmc("auto_anoSnoB_samples",model.dir ="LBA",
             model.file="lba_B_automation.R",user="ljs392",
             n.add=60, wall.hours = 300,
             GB = 2, max.try=5)

load_model("LBA","lba_B_automation.R")
run.grid.dmc("auto_anoS_ex_samples",model.dir ="LBA",
             model.file="lba_B_automation.R",user="ljs392",
             n.add=60, wall.hours = 300,
             GB = 2, max.try=5)

