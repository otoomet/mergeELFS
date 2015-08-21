### Note: we consistently (try to) use utf-8 encoding

### This is the main function which merges all the ELFS waves pulling out the necessary
### variables.  It is mostly based on corresponding wave specific subroutines
mergeELFS <-function(nrows=0,
                     writeR=TRUE, writeTsv=TRUE, writeDta=TRUE,
                     cleanUpResults=TRUE,
                     print.level=2,
                     fromScratch=TRUE) {
   ## nrows        how many rows to read from the tables (-1 = all).  Useful for testing
   ## print.level  how much tracing info to print (higher number = more)
   ## fromScratch  extract the variables from the individual waves
reEncodeResults<-TRUE
generateLaTeX<-FALSE #(do not even try, unless using Deep Blue or equivalent)

# if fromScratch==TRUE, the data is regenerated from scratch
if(fromScratch) {
   source(paste(scriptDir, "merge1990s.R", sep=""))
   doAll1995.2000q2(nrows, print.level)
    source(paste(scriptDir, "merge2000s.R", sep=""))
   mergePost2000(nrows, print.level)
}
   if(print.level > 0) {
       cat("loading individual dataframes from disk \n")
    }
   load("dataOf1995-2000q2All.Rdat")
   load("dataOf2000q3-2010All.Rdat")
   if(print.level > 0)
       cat("\ndataframes loaded")
   allVars <- unique(c(names(fullData1995.2000q2), names(d2000q3.2010)))
   if(length(d <- setdiff(allVars, names(fullData1995.2000q2))) > 0) {
      cat("Warning: 1995-2000q2 does not include the following vars:\n")
      print(d)
   }
   if(length(d <- setdiff(allVars, names(d2000q3.2010))) > 0) {
      cat("Warning: d2000q3.2010 does not include the following vars:\n")
      print(d)
   }
   panel <-rbind(fullData1995.2000q2,d2000q3.2010)
   if(print.level > 0) {
      cat("\nsingle dataframe formed")
   }
if(cleanUpResults) {
                           # introduce NA-s, factorize, etc
   panel$nWorkers[panel$nWorkers > 95] <- NA
   panel$partTimeHours[panel$partTimeHours==999]<-NA
                    panel$wage[panel$wage==999999]<-NA
                    panel$wage[panel$wage==999998]<-NA
                    panel$immigrYear[panel$immigrYear==9999]<-NA
   panel$nWorkers[panel$nWorkers == 9] <- NA
                                        # don't know, present 1998-
                    panel$aasta<-as.POSIXlt(panel$date)$year+1900
                    panel$kuu<-as.POSIXlt(panel$date)$mon+1
                    panel$townOrCountryWork[panel$townOrCountryWork==999]<-NA
                    panel$townOrCountryWork[panel$townOrCountryWork==9]<-NA
                    panel$partOrFullTime[panel$partOrFullTime==9]<-NA
   panel$howFoundJob[panel$howFoundJob > 95] <- NA
   panel$howFoundJob <- factor(panel$howFoundJob)
}
if(reEncodeResults) {
   if(print.level > 0) {
      cat("\nre-encoding the results")
   }
   panel$id <- factor(panel$id)
   panel$idHousehold <- factor(panel$idHousehold)
   panel$idPerson <- factor(panel$idPerson)
   panel$maritalStatus[panel$maritalStatus > 8] <- NA
   panel$maritalStatus<-factor(panel$maritalStatus)
panel$kuu<-factor(panel$kuu)
panel$residenceCounty <- factor(panel$residenceCounty)
panel$residenceMuni <- factor(municipality2008(panel$residenceMuni))
panel$nWorkers<-factor(panel$nWorkers)
panel$townOrCountryWork<-factor(panel$townOrCountryWork)
panel$sex<-factor(panel$sex, exclude=NULL)
   panel$edu <- factor(panel$edu, levels=c("highSchool", "<=basic", "college"))
panel$estLevel <- normLanguage(panel$estLevel)
panel$rusLevel <- normLanguage(panel$rusLevel)
panel$engLevel <- normLanguage(panel$engLevel)
   panel$FILevel <- normLanguage(panel$FILevel)
panel$ownership[panel$ownership > 90] <- NA
panel$ownership<-factor(panel$ownership)
                                        # don't know -> NA
panel$occupation[panel$occupation > 90] <- NA
panel$occupation <-factor(panel$occupation)
panel$sidejob <- convertSidejob(panel$sidejob, panel$retrospective)
   panel$workCountry <- factor(panel$workCountry)
panel$workCounty <- factor(panel$workCounty)
panel$workMuni <- factor(municipality2008(panel$workMuni))
panel$workForceStatus <-factor(panel$workForceStatus)
panel$activityStatus <-factor(panel$activityStatus)
panel$industry[panel$industry == ""] <- NA
panel$industry <- factor(trim(panel$industry))
   panel$mainSearchMethod[panel$mainSearchMethod == 99] <- NA
   panel$mainSearchMethod <- factor(panel$mainSearchMethod)
   panel$UIBenefits[panel$UIBenefits > 90000] <- NA
   panel$UAllowance[panel$UAllowance > 9000] <- NA
   panel$prevIndustry[panel$prevIndustry %in% c("", "X")] <- NA
   # X means nonresponse, according to 1998 docs.
   panel$prevIndustry <- factor(panel$prevIndustry)
   panel$prevOccupation[panel$prevOccupation > 90] <- NA
   panel$prevOccupation <- factor(panel$prevOccupation)
}
if(print.level > 0) {
   cat("\nsaving\n")
}
if(writeR){
   save(panel, file="ETU_panel.Rdat")
}
if(writeTsv) {
   writeTable(panel)
}
if(writeDta) {
   writeDta(panel)
}
if(print.level > 0)
    cat("\n")
invisible(panel)
}
