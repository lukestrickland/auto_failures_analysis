#setwd("~/grid_collection")

collect.grid <- function(dname,froot="s",verbose=TRUE,v_D_samples=NA) {
  if (any(is.na(v_D_samples))) load(paste("v_D_samples.RData",sep=""))
  for (i in 1:length(v_D_samples)) {
    onam <- paste(froot,i,sep=".")
    fnam <- paste(dname,"/results.",onam,".RData",sep="")
    tmp <- try(load(fnam),silent=TRUE)
    if (class(tmp)=="try-error") bad <- TRUE else {
      v_D_samples[[i]] <- get(tmp)
      bad <- FALSE
    }
    if (bad) attr(v_D_samples[[i]],"auto") <- "GRID FAIL"  
    if (verbose) {
      if (bad) cat("Read fail\n") else 
        cat(paste(onam,":",attr(v_D_samples[[i]],"auto"),"\n")) 
    } else {
      if (bad) cat("F") else cat(".")
    }
  }
  cat("\n")
  auto <- unlist(lapply(v_D_samples,function(x){
    y <- attr(x,"auto"); if (is.null(y)) y<-NA; y
  }))
  attr(v_D_samples,"auto") <- auto
  v_D_samples
}

auto_test <- collect.grid(dname="auto_top_samples", v_D_samples= auto_top_samples)


source("dmc/dmc.R")
load_model("LBA","lba_B_automation.R")


p22 <- auto_test[["22"]]

p22_fit <- RUN.dmc(p22, max.try=1, cores=12)

p22_fit2 <- RUN.dmc(p22_fit, max.try=3)




auto_test <- auto_test[!(names(auto_test) %in% "22")]

auto_test <- collect.grid(dname="auto_top_samples", v_D_samples= auto_top_samples)
auto_test[['22']]$theta[,,2]

pdf(width=100, length=100)
pairs.dmc(auto_test[[1]])
dev.off()



collect.grid <- function(dname,froot="s",verbose=TRUE,auto_top_samples=NA) {
  if (any(is.na(auto_top_samples))) load(paste("auto_top_samples.RData",sep=""))
  for (i in 1:length(auto_top_samples)) {
    onam <- paste(froot,i,sep=".")
    fnam <- paste(dname,"/results.",onam,".RData",sep="")
    tmp <- try(load(fnam),silent=TRUE)
    if (class(tmp)=="try-error") bad <- TRUE else {
      auto_top_samples[[i]] <- get(tmp)
      bad <- FALSE
    }
    if (bad) attr(auto_top_samples[[i]],"auto") <- "GRID FAIL"  
    if (verbose) {
      if (bad) cat("Read fail\n") else 
        cat(paste(onam,":",attr(auto_top_samples[[i]],"auto"),"\n")) 
    } else {
      if (bad) cat("F") else cat(".")
    }
  }
  cat("\n")
  auto <- unlist(lapply(auto_top_samples,function(x){
    y <- attr(x,"auto"); if (is.null(y)) y<-NA; y
  }))
  attr(auto_top_samples,"auto") <- auto
  auto_top_samples
}


