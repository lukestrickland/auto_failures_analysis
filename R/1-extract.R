###################################################################################################
################################# ATC-LAB DATA EXTRACTION ROUTINE #################################
###################################################################################################

### clear workspace ###
rm(list = ls())

#requires tidyverse
source('R/0-extract_functions.R')

xml_files <- dir("XML_logs")

for(i in 1:length(xml_files)){
    splitstrings <- strsplit(xml_files[i], split="_")[[1]]
    ppt <- gsub("ppt", "", splitstrings[1])
    sess <- gsub("sess", "", splitstrings[2])
    block <- gsub("block", "", splitstrings[3])
    cond <- gsub("\\.xml\\.log", "", splitstrings[5])
    current_df <- extract_one(ppt=ppt, sess=sess, block= block,  cond=cond)
    print(i)
    if(i==1) full_df <- current_df else full_df <- rbind(full_df, current_df)
}

dats <- full_df[, c("ppt", "sess", "block", "cond", "stimulus",
                    "failtrial",
                    "response", "rkey", "RT", "score", 
                    "cumulative_score")]

#Get responses from Rkey
dats$rkey[!dats$rkey %in% c("F", "J")] <- NA

#convert response key to responses
dats$R <- NA
key_set <- c(1,1, 2, 2)

for (i in unique(dats$ppt)) {
  counterbalance <- (as.numeric(i) %% 4) + 1
  cb_key <- key_set[counterbalance]
  if (cb_key==1) {
    dats$R[dats$rkey=="F" & dats$ppt==i] <- "C"
    dats$R[dats$rkey=="J"& dats$ppt==i] <- "N" }
  else {
    dats$R[dats$rkey=="F"& dats$ppt==i] <- "N"
    dats$R[dats$rkey=="J"& dats$ppt==i] <- "C"     
  }
  
}

dats$stimulus <- factor(dats$stimulus, levels = c("nonconflict", "conflict"),
                        labels=c("n", "c"))

save(dats, file="img/dats_raw.RData")

#DATA EXCLUSIONS - NON RESPONSES AND <200MS
100* length(dats$RT[is.na(dats$RT)])/length(dats$RT) 
cleandats <- dats[!is.na(dats$RT),]

oldlen <- length(cleandats$RT)
cleandats <-  cleandats[cleandats$RT>200,]

(oldlen -length(cleandats$RT))/length(cleandats$RT) *100

###CLEAN UP DATA FRAME

cleandats$cond <- factor(cleandats$cond)
cleandats$failtrial <- factor(cleandats$failtrial,
                              levels=c("FALSE", "TRUE"),
                              labels=c("nonf", "fail"))
cleandats$sess <- factor(cleandats$sess)
cleandats$block <- factor(cleandats$block, levels=c("1", "2"),
                          labels= c("one", "two"))

colnames(cleandats)[colnames(cleandats)=='ppt'] <- 's'
colnames(cleandats)[colnames(cleandats)=='stimulus'] <- 'S'
cleandats <- cleandats[,c("s", "sess", "block", "cond",
                          "failtrial", "S", "R", "RT")]

cleandats$RT <- cleandats$RT/1000

save(cleandats, file="img/cleandats.RData")

