                           # Functions here take data from ETUs from 1995 until 2000Q2  and save it either to file fromETU2000q1q2.tsv for
                           # standalone use or return it as a dataframe (function doAll2000q1q2) for use in other scripts.

doAll1995.2000q2 <- function(nrows=25,
                             print.level=3) {
   ## nrows        how many rows to read from the tables (-1 = all).  Useful for testing
   ## print.level  how much tracing info to print (higher number = more)
   doDateCurrent<- TRUE
   source("ETUCommon.R")   
   source("Parameters.R")
   source("etuUtils.R")
   doEtu <- function(etuName) {
      ##
      ##       The parameters starting with "ES_" form a compatability layer 
      ##       for different ETUs. "ES_P" means that last two digits are substracted,
      ##       for example C04A0000->C04A00
      ##
      etu <- paste("ETU", etuName, sep="")
      if(print.level > 0)
          cat("\n", etu)
      ES_addToYear<- switch(etuName,
                            "1995"=, "1997" = 1900,
                            0)
                           # some ETUs have years as YYYY,
                           # some YY, in case of YY set this to 1900
      ## Survey stuff
      ES_quarter <- switch(etuName,
                           "2000_1j2" = "kvartal",
                           "")
      ES_surveyDay <- switch(etuName,
                             "1995" = "a03b",
                             "1997" = "a03balg",
                             "1998" =, "1999" = "a03aalg",
                             "1999" = "a03a",
                             "a02aalg")
      ES_surveyMonth <- switch(etuName,
                             "1995" = "a03a",
                             "1997" = "a03aalg",
                               "1998" =, "1999" = "a03balg",
                             "a02balg")
      ## individual background
      ES_yearOfFirstJob <- switch(etuName,
                                  "1995" = "b01a",
                                  "c01a")
      ES_monthOfFirstJob<- switch(etuName,
                                  "1995" = "b01b",
                                  "c01b")
      ES_unionMember <- "m10"
      ES_C_activity <-
          switch(etuName,
                 "1995" = "l0300",
                 "1997" = "b0400",
                 "b09")
      ES_maritalStatus0 <- switch(etuName,
                                  "1995" = "h01",
                                  "1997" = "h010",
                                  "1998" =, "1999" =, "2000_1j2" = "h01")
      ES_P_maritalStatus <- switch(etuName,
                                   "1995" = "h04",
                                   "1997" = "h01",
                                   "1998" =, "1999" =, "2000_1j2" = "h04")
      ES_maritalCol <- switch(etuName,
                              "1995" = c("00", "01", "02", "03", "04"),
                              "1997" = c("0", "1", "2"),
                              "1998" = c("00", "01"),
                              "1999" = c("00", "01", "02"),
                              c("01", "02"))
      ES_P_maritalChangeYear <- switch(etuName,
                                       "1995" = "h03a",
                                       "1997" = "h04a",
                                       "1998" =, "1999" =, "2000_1j2" = "h03a")
      ES_P_maritalChangeMonth <- switch(etuName,
                                        "1995" = "h03b",
                                        "1997" = "h04b",
                                        "1998" =, "1999" =, "2000_1j2" = "h03b")
      ES_maritalChangeCol <- switch(etuName,
                                    "1997" = c("0000", "0001", "0002"),
                                    ES_maritalCol)
                           # in LFS1997 the columns are marked in a different way -> initial status is 'h010' and later statuses are
                           # 'h011' etc.  Hence we have to start status columns from 1, change columns from 0
      ## residence
      ES_residenceStartCounty <- switch(etuName,
                                        "1995" = "i01ekood",
                                        "1997" = "i01lk0",
                                        "i01mk")
      ES_residenceStartMuni <- switch(etuName,
                                      "1995" = "i01ekood",
                                      "I01LKG")
                           # residence in January previous year
      ES_colResidenceMobility <- switch(etuName,
                                        "1995" = c("00", "01", "02", "03", "04"),
                                        "1997" = c("1", "2", "3", "4"),
                                        "1998" =, "1999" = c("00", "01", "02"),
                                        c("01", "02", "03"))
      ES_P_residenceMoveCounty <- switch(etuName,
                                         "1995" = "i05e00",
                                         "1997" = "i01lk",
                                         "i04mk")
      ES_P_residenceMoveMuni <- switch(etuName,
                                       "1995" = "i05e00",
                                       "i04lkg")
                           # which county moved to
      ES_P_residenceMoveYear <- switch(etuName,
                                       "1995" = "i03a00",
                                       "i03a")
      ES_P_residenceMoveMonth <- switch(etuName,
                                        "1995"= "i03b00",
                                        "i03b")
      ## Main job stuff
      ES_employmentSpells<- switch(etuName,
                                      "1995" = c("0000", "0001", "0002", "0003", "0004", "0005", "0006", "0007", "0008", "0009",
                                                 "0010", "0011"),
                                      "1997" =, "1998" =, "1999" = c("0000", "0001", "0002", "0003", "0004", "0005"),
                                      ## "1999" = c("0000", "0001", "0002", "0003", "0004", "0005", "0006", "0007", "0008", "0009"),
                           # more values for some variables ?
                                      c("01", "02", "03", "04"))
      ES_P_howFoundJob <- switch(etuName,
                                 "1995" = "c03",
                                 "c06")
                           # how did you find this job
      ES_P_yearOfStartOfJob<- switch(etuName,
                                     "1995" = "c04a",
                                     "c07a")
      ES_P_monthOfStartOfJob<- switch(etuName,
                                      "1995" = "c04b",
                                      "c07b")
      ES_P_yearOfEndOfJob<- switch(etuName,
                                   "1995" = "c28a",
                                   "c25a")
      ES_P_monthOfEndOfJob<- switch(etuName,
                                    "1995" = "c28b",
                                    "c25b")
      ES_P_employeeStatus <- switch(etuName,
                                    "1995" = "c15",
                                    "c12")
      ES_P_occupation<- switch(etuName,
                               "1995" = "c17b",
                               "c14b")
      ES_P_partOrFullTime<- switch(etuName,
                                   "1995" = "c20",
                                   "c17")
      ES_P_partTimeHours<- switch(etuName,
                                  "1995" = "c22b",
                                  "c19b")
                           # NB <- accurate if previous only part time (not both!) 
      ES_P_partTimeReason<- switch(etuName,
                                   "1995" = "c22a",
                                   "c19a")
      ES_P_industry<- switch(etuName,
                             "1995" = "c02k",
                             "1997" = "c05k",
                             "1998" = "c05",
                             "1999" = "c05a",
                             "c05a")
      ES_P_nWorkers<- switch(etuName,
                                "1995" = "c06",
                                "c09a")
                           # groups 1-8:
                           # [1,10], [11,19], [20,49], [50, 99], [100, 199], [200, 499], [500, 999], [1000, Inf)
                           # 9 - don't know
      ES_P_publicSector<- switch(etuName,
                                   "1995" = "c08",
                                   "c11")
      ES_P9_workCity<- switch(etuName,
                              "1997" =, "1998" =, "1999" = "c08l",
                              "c08lk")
      ES_P_workCounty<- switch(etuName,
                               "1995" = "c05e",
                               "1997" =, "1998" =, "1999" = "c08m",
                               "c08mk")
      ES_P_workMuni <- switch(etuName,
                              "1995" = "c05e",
                              "1997" = "c08l",
                              "c08lkg")
      ES_P9_workCountry<- switch(etuName,
                                 "1995" = "c05v",
                                 "1997" =, "1998" =, "1999" = "c08r",
                                 "c08rk")
      ## other jobs
      ES_P_sidejob <- "c20"
                           # presence of side jobs
      ES_employmentSpells9 <- switch(etuName,
                           # some waves have different codes for different
                           # variables 
                                        "1997" = c("0009", "0010", "0011", "0012", "0013", "0014"),
                                        "1998" =, "1999" = c("0006", "0007", "0008", "0009", "0010", "0011"),
                                        c("01", "02", "03", "04"))
      ES_USpells<- switch(etuName,
                                        "1995" = c("000", "001", "002", "003", "004", "005", "006"),
                                        "1997" = c("000", "001", "002", "003"),
                                        "1998" =, "1999" = c("000", "001", "002"),
                                        c("01", "02", "03"))
      ES_NSpells<- switch(etuName,
                                   "1995" = c("000", "001", "002", "003", "004", "005", "006"),
                                   "1997" = c("000", "001", "002"),
                                   "1998" =, "1999" = c("000", "001", "002"),
                                   c("01", "02", "03"))
      ES_P_UnempStart_Year <- switch(etuName,
                                     "1995" = "d02a",
                                     "d02a")
      ES_P_UnempStart_Month<- switch(etuName,
                                     "1995" = "d02b",
                                     "d02b")
      ES_P_endDatesUnemp_Year<- switch(etuName,
                                       "1995" =, "1997" = "d05a0",
                                       "1998" =, "1999" = "d07a0",
                                       "d07a")
      ES_P_endDatesUnemp_Month<-switch(etuName,
                                       "1995" =, "1997" = "d05b0",
                                       "1998" =, "1999" = "d07b0",
                                       "d07b")
      ES_S_prevIndustry <- switch(etuName,
                                  "1995" = "",
                                  "1997" =, "1998" = "k07kood",
                                  "k07a")
      ES_S_prevOccupation <- switch(etuName,
                                    "1995" = "",
                                    "k08kood")
      ##
      ES_P_startDatesInactive_Year<- switch(etuName,
                                            "1995" = "e02a",
                                            "1997" = "e02a",
                                            "e02a")
      ES_P_startDatesInactive_Month<- switch(etuName,
                                            "1995" = "e02b",
                                             "1997" = "e02b",
                                            "e02b")
      ES_P_endDatesInactive_Year<- switch(etuName,
                                          "1995" =, "1997" = "e06a0",
                                          "1998" =, "1999" = "e05a0",
                                          "e05a")
      ES_P_endDatesInactive_Month<- switch(etuName,
                                           "1995" =, "1997" = "e06b0",
                                          "1998" =, "1999" = "e05b0",
                                           "e05b")
      ES_P_salaryAtDate1 <- switch(etuName,
                                   "1995" = "c18b",
                                   "1997" = "c16a",
                                   "c16b")
      ES_P_salaryAtDate2 <- switch(etuName,
                                   "1995" = "c18c",
                                   "1997" = "c16b",
                                   "c16d")
      ES_P_salaryAtDate3 <- switch(etuName,
                                   "1995" = "c18d",
                                   "1997" = "c16c",
                                   "c16f")
      ES_P_salaryAtDate4 <- switch(etuName,
                                   "1995" = "c18e",
                                   "1997" = "c16d",
                                   "")
      ES_P_salaryLast <- switch(etuName,
                                "1995" = "c18f",
                                "")
      ES_P_ownerType <- "c07"
                           # 1995 has two variables for ownership.  This one distinguished b/w public/private (+ kolkhozes etc)
      ES_P_ownership<- switch(etuName,
                              "1995" = "c09",
                              "c10")
                           #ES_wageAtDate4<-"C16D0000"
      ES_columnsOfHHMembers<- switch(etuName,
                                     "1995" = c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09",
                                                "10","11","12","13","14"),
                                     "1997" =, "1998" =, "1999" = c("00", "01", "02", "03",
                                                         "04", "05", "06", "07", "08", "09", "10"),
                                     c("01", "02", "03", "04", "05", "06", "07", "08", "09",
                                       "10","11","12","13","14"))
      HHMember0 <- switch(etuName,
                          "1995" =,
                          "1997" =, "1998" =, "1999" = 1,
                          0)
                           # to be subtracted from 1 to get the first member suffix
      ## LFS1995 includes birthday for everyone, not grouped to households (ES_xxx), the other by household members (ES_P_xxx)
      ES_P_HHBirthYear <- switch(etuName,
                                 "1995" = "l02a",
                           # Note that LFS1995 only samples household "heads"
                                 "1997" = "b03a",
                                 "b04c")
      ES_P_HHBirthMonth <- switch(etuName,
                                  "1995" = "l02b",
                                  "1997" = "b03b",
                                  "")
      ES_P_HHBirthDay <- switch(etuName,
                                "1995" = "l02c",
                                "1997" = "b03c",
                                "")
      ES_male<- switch(etuName,
                       "1995" = "l02d00",
                       "1997" = "m01d",
                       "b04d")
      ES_ethnicity<- switch(etuName,
                            "1995" = "o03kood",
                            "1997" = "m06kood",
                            "b06k")
      ES_P_ethnicity<- switch(etuName,
                              "1995" = "o03kood",
                              "1997" = "m06kood",
                              "1998" =, "1999" = "b06k00",
                              "b06k")
      LFSLangLevel_HomeLang1 <- switch(etuName,
                                       "1995" = "o04kkoo0",
                                       "1997" = "m07ak",
                                       "m05ak")
      LFSLangLevel_HomeLang2 <- switch(etuName,
                                       "1995" = "o04kkoo1",
                                       "1997" = "m07bk",
                                       "m05bk")
      LFSLang1 <- switch(etuName,
                         "1995" = "o06koo00",
                         "1997" =, "1998"=, "1999" = "kood0",
                         "kood1")
      LFSLang2 <- switch(etuName,
                         "1995" = "o06koo01",
                         "1997" =, "1998"=, "1999" = "kood1",
                         "kood2")
      LFSLang3 <- switch(etuName,
                         "1995" = "o06koo02",
                         "1997" =, "1998"=, "1999" = "kood2",
                         "kood3")
      LFSLang4 <- switch(etuName,
                         "1995" = "o06koo03",
                         "1997" = "",
                         "1998"=, "1999" = "kood3",
                         "kood4")
      LFSLang5 <- switch(etuName,
                         "1995" =, "1997" = "",
                         "1998"=, "1999" = "kood4",
                         "kood5")
      LFSLangLevel1 <- switch(etuName,
                              "1995" = "o06osk00",
                              "1997" = "tase00",
                              "1998"=, "1999" = "tase0",
                         "tase1")
      LFSLangLevel2 <- switch(etuName,
                              "1995" = "o06osk01",
                              "1997" = "tase01",
                              "1998"=, "1999" = "tase1",
                         "tase2")
      LFSLangLevel3 <- switch(etuName,
                              "1995" = "o06osk02",
                              "1997" = "tase02",
                              "1998"=, "1999" = "tase2",
                              "tase3")
      LFSLangLevel4 <- switch(etuName,
                              "1995" = "o06osk03",
                              "1997" = "",
                              "1998"=, "1999" = "tase3",
                              "tase4")
      LFSLangLevel5 <- switch(etuName,
                              "1995" =, "1997" = "",
                              "1998"=, "1999" = "tase4",
                              "tase5")
      ES_immigrYear<- switch(etuName,
                             "1995" = "o02",
                             "1997" = "m03",
                             "b08a")
      ES_HHno<- switch(etuName,
                       "1997" =, "1998" =, "1999" = "a01a",
                       "leibkond")
      ES_respondent <- switch(etuName,
                              "1995" = "a01",
                           # this is actually missing
                              "1997" =, "1998" =, "1999" = "a01b",
                              "kysitlet")
      if(etuName == "1995") {
         date <- c(ISOdate(1989, 10, defaultSalaryDay), ISOdate(1992, 10, defaultSalaryDay),
                   ISOdate(1993, 10, defaultSalaryDay), ISOdate(1994, 10, defaultSalaryDay))
      }
      else if(etuName == "1997") {
         date <- c(ISOdate(1995, 1, defaultSalaryDay), ISOdate(1995, 10, defaultSalaryDay),
                   ISOdate(1996, 10, defaultSalaryDay), ISOdate(1997, 1, defaultSalaryDay))
      }
      else if(etuName == "1998") {
         date <- c(ISOdate(1997, 1, defaultSalaryDay), ISOdate(1997, 10, defaultSalaryDay),
                   ISOdate(1998, 1, defaultSalaryDay), NA)
      }
      else if(etuName == "1999") {
         date <- c(ISOdate(1998, 1, defaultSalaryDay), ISOdate(1998, 10, defaultSalaryDay),
                   ISOdate(1999, 1, defaultSalaryDay), NA)
      }
      else {
         date <- c(ISOdate(1999, 1, defaultSalaryDay), ISOdate(1999, 10, defaultSalaryDay),
                   ISOdate(2000, 1, defaultSalaryDay), NA)
      }
      nDate <- sum(!is.na(date))
                           #Education
                           #
      ES_generalEdu <- switch(etuName,
                              "1995" = "g21",
                              "1997" =, "1998" = "g28",
                              "g17a")
      ES_vocEdu <-  switch(etuName,
                           "1997" = "g30",
                           "1998" = "g30a",
                           "g16a")
      LFSISCED <- "isced"
      ES_columnsOfEducation <- switch(etuName,
                                      "1995" = c("0", "1", "2", "3", "4"),
                                      "1997" =, "1998"=, "1999"= c("0000", "0001", "0002"),
                                      c("01","02","03"))
      ES_P_eduStart_year <-switch(etuName,
                                  "1995" = "g07a000",
                                  "g07a")
      ES_P_eduStart_month <-switch(etuName,
                                   "1995" = "g07b000",
                                   "g07b")
      ES_P_eduEnd_year<- switch(etuName,
                                "1995" = "g15a000",
                                "1997" = "g15a",
                                "g10a")
      ES_P_eduEnd_month<- switch(etuName,
                                 "1995" = "g15b000",
                                 "1997" = "g15b",
                                 "g10b")
      ES_P_finishingStatus<- switch(etuName,
                                    "1995" = "g14000",
                                    "1997" = "g14",
                                    "g09")
      ES_P_typeOfSchool<-"g04"
      ## Survey week data
      ES_surveyJobCols <-
          switch(etuName,
                 "1995" = c("00", "01","02","03","04"),
                 "1997" =,
                 "1998" = c("0000", "0001", "0002", "0003", "0004"),
                 "1999" = c("0000", "0001", "0002", "0003", "0004",
                 "0005", "0006"),
                 c("01", "02", "03", "04", "05", "06", "07")
                 )
      ES_C_surveySalary <- "j16b"
                           # only present 1998 onward
                           # probably net salary
      ES_C_occupation <- switch(etuName,
                                "1995" = "j06b",
                                "1997" =, "1998" =, "1999" = "j06k",
                                "j06ko")
      ES_C_mainJob <- switch(etuName,
                             "1995" = "j15v0000",
                           # 00 - main job, 01 - first sidejob, ...
                             "1997" = "j16",
                             "j1701")
                           # which of the earlier listed jobs is the current main job
      ES_C_surveyWorkHours <- switch(etuName,
                                     "1995" = "j1000",
                                     "j10")
      ES_C_surveyPartReason <- "j31"   
      ES_surveyEmployee <- "j01"
      ES_surveyEnterpreneur <- "j02"
      ES_surveyFarmer <- "j03"
      ES_surveyAbsent <- "j04"
      ES_surveySearching <- switch(etuName,
                                   "1995" = "k01",
                                   "k09")
      ## Technical stuff
      ES_weight <- switch(etuName,
                          "2000_1j2" = "wgt",
                          "wght")
      ##
      ## We stop the ASCII step, needed for low-memory machines.  Man, it is 2010 ;-)
      load(paste("../", etu, ".Rdat", sep=""))
      m <- etu
      rm(etu)
      if(print.level > 0) {
         cat("\n", nrow(m), "rows read")
      }
      names(m) <- tolower(names(m))
      NObs <- nrow(m)
      ##
      ## IDs
      ##
      surveyYear <- as.numeric(substr(etuName, 1, 4))
      defaultSurveyDate <- ISOdate(surveyYear, 5, 3)
                           # in case the survey date is missing
      quarter <-
          if(etuName < "2000")
              rep(2, nrow(m))
          else
              m[[ES_quarter]]
      localHHId <- switch(etuName,
                          "1995" = seq(length=nrow(m)),
                          m[[ES_HHno]]
                          )
                           # used for indexing household members as the quarter may be wrong
      idHousehold<- surveyYear*10000000 + quarter*1000000 + localHHId
      idMember <- switch(etuName,
                         "1995" = rep(1, nrow(m)),
                           # independent sampling, missing codes
                         m[[ES_respondent]])
      idPerson <- 100*idHousehold + idMember
      idObs <- idPerson
      ## Read the municipality data.  It seems to be ordered exactly in the same way as for the main data.
      ## However, we perform a few checks
      if(print.level > 1) {
         cat("\nreading municipality data..")
      }
                           # Need to get municipality data separately
      if(etuName %in% c("1998", "1999")) {
         muniFile <- switch(etuName,
                            "1998" = paste(etuDir, "/municipality/etu98_vald.csv.gz", sep=""),
                            "1999" = paste(etuDir, "/municipality/etu99_vald.csv.gz", sep=""))
         if(file.exists(muniFile)) {
            ## muni <- read.csv(gzfile(muniFile, encoding="cp850"), nrows=nrows)
            muni <- read.csv(gzfile(muniFile), nrows=nrows)
            if(!all(m$ES_HHno == muni$ES_HHno)) {
               stop("data and municipality household codes do not match")
            }
            if(!all(m$ES_respondentNo == muni$ES_respondentNo)) {
               stop("data and municipality respondent codes do not match")
            }
         }
         else
             warning(paste("Municipality data", muniFile, "does not exists"))
      }
      else if(etuName == "2000_1j2") {
                           # for LFS2000 there are 2 separate files
         muniFile1 <- paste(etuDir, "/municipality/etu20001_vald.csv.gz", sep="")
         muniFile2 <- paste(etuDir, "/municipality/etu20002_vald.csv.gz", sep="")
         if(file.exists(muniFile1) & file.exists(muniFile2)) {
            ## muni1 <- read.csv(gzfile(muniFile1, encoding="latin1"), nrows=-1)
            muni1 <- read.csv(gzfile(muniFile1), nrows=-1)
                           # unfortunately, in ETU2000_1j2, the waves have different number of variables
            muni1$C08LI04 <- NA
            muni1$c08lkg04 <- NA
            ## muni2 <- read.csv(gzfile(muniFile2, encoding="latin1"), nrows=-1)
            muni2 <- read.csv(gzfile(muniFile2), nrows=-1)
            muni <- rbind(muni1, muni2)
            rm(muni1, muni2)
            if(!all(m$ES_HHno == muni$ES_HHno)) {
               stop("data and municipality household codes do not match")
            }
            if(!all(m$ES_respondent == muni$ES_respondentNo)) {
               stop("data and municipality respondent codes do not match")
            }
            if(nrows > 0)
                muni <- muni[seq(length=nrows),]
         }
         else
             warning(paste("Municipality data", muniFile,
                           "does not exists"))
      }
      ##
      ## Education
      ##
      if(print.level > 1)
          cat("\nedu")
      edu <- convertEdu(m[[ES_vocEdu]], m[[ES_generalEdu]], name=etuName)
                           # = current highest completed education
      isced <- convertISCED97(m[[ES_vocEdu]], m[[ES_generalEdu]],
                              ISCED=m[[LFSISCED]], name=etuName)
      ## education in the past
      eduStartDates <- eduEndDates <- matrix(ISOdate(2008, 10, 23), nrow(m),
                                                  length(ES_columnsOfEducation))
                           # used for determining student status
      class(eduStartDates) <- class(eduEndDates) <- c("POSIXct", class(eduStartDates))
      eduFinishingStatus <- schoolType <- matrix(0, nrow(m), length(ES_columnsOfEducation))
      colnames(eduStartDates) <- colnames(eduEndDates) <- colnames(eduFinishingStatus) <-
          colnames(schoolType) <- ES_columnsOfEducation
      for(i in ES_columnsOfEducation) {
         eduStartDates[, i] <- ISOdate((ES_addToYear + m[[paste(ES_P_eduStart_year, i, sep="")]]),
                                       m[[paste(ES_P_eduStart_month, i, sep="")]], 1)
         eduEndDates[, i] <- ISOdate((ES_addToYear + m[[paste(ES_P_eduEnd_year,i,sep="")]]),
                                          m[[paste(ES_P_eduEnd_month,i,sep="")]], 1)
                           # not: we assume started education on 1st of the month
         eduFinishingStatus[, i] <- m[[paste(ES_P_finishingStatus, i, sep="")]]
                           # these are the dates of graduation or quitting
         schoolType[, i] <- m[[paste(ES_P_typeOfSchool, i, sep="")]]
      }
      schoolType <- convertSchool(schoolType, etuName)
                           # standardized education code
      preSchoolType <- preSchool(schoolType, etuName)
                           # assumed education before current school
      ##
      ## Marital status
      ##
      marStat <- matrix(0, nrow(m), length(ES_maritalCol) + 1)
      maritalDate <- matrix(ISOdate(2009, 11, 16), nrow(m),
                            length(ES_maritalChangeCol))
      marStat[, 1] <- m[[ES_maritalStatus0]]
      for(i in seq(along=ES_maritalCol)) {
         marStat[, 1 + i] <- m[[paste(ES_P_maritalStatus,
                                      ES_maritalCol[i], sep="")]]
         maritalDate[, i] <- ETUdate(m[[paste(ES_P_maritalChangeYear,
                                              ES_maritalChangeCol[i],
                                              sep="")]],
                                     m[[paste(ES_P_maritalChangeMonth,
                                              ES_maritalChangeCol[i],
                                              sep="")]],
                                     defaultMaritalDay)
      }
      ##
      ## County of residence
      ##
      if(print.level > 2) {
         cat("\nplace of residence")
      }
      moveDate <- matrix(ISOdate(2008, 11, 22), nrow(m), length(ES_colResidenceMobility))
      class(moveDate) <- c(class(ISOdate(2008, 10, 25)), class(moveDate))
      colnames(moveDate) <- ES_colResidenceMobility
      residenceCounty <- residenceMuni <- matrix(0, nrow(m), length(ES_colResidenceMobility) + 1)
      colnames(residenceCounty) <- colnames(residenceMuni) <- c("", ES_colResidenceMobility)
      residenceCounty[,1] <- county(m[[ES_residenceStartCounty]])
      if(etuName >= "1998") {
         residenceMuni[,1] <- muni[[ES_residenceStartMuni]]
      }
      else{
         residenceMuni[,1] <- defactor(m[[ES_residenceStartCounty]])
                           # coded as factor in ETU1995.Rdat
      }         
      for(i in ES_colResidenceMobility) {
         residenceCounty[,i] <- county(m[[paste(ES_P_residenceMoveCounty,
                                                i, sep="")]])
         if(etuName >= "1998") {
            residenceMuni[,i] <- muni[[paste(ES_P_residenceMoveMuni, i,
                                             sep="")]]
         }
         else {
            residenceMuni[,i] <- m[[paste(ES_P_residenceMoveCounty,
                                          i, sep="")]]
         }
         moveDate[,i] <- ISOdate(m[[paste(ES_P_residenceMoveYear, i, sep="")]], m[[paste(ES_P_residenceMoveMonth)]], defaultMoveDay)
      }
      ##
      if(print.level > 2) {
         cat("\nfirst job")
      }
      firstJob<-ISOdate((ES_addToYear+m[[ES_yearOfFirstJob]]), m[[ES_monthOfFirstJob]],defaultDateOfStartingAJob)
      ## ------------------------------------------
      ## ----------- Main Jobs --------------------
      ## ------------------------------------------
      startDates <- endDates <- matrix(ISOdate(2008, 10, 19), nrow(m), length(ES_employmentSpells))
      class(startDates) <- class(endDates) <- c(class(ISOdate(2008, 10, 25)), class(startDates))
      occupation <- matrix("", nrow(m), length(ES_employmentSpells))
      howFoundJob <- partOrFullTime <- partTimeHours <- partTimeReason  <- industry <-
          nWorkers <- publicSector <- cityOfWork <- workCounty <- workMuni <- workCountry <-
              employeeStatus <- 
                  salaryAtDate1 <- salaryAtDate2 <- salaryAtDate3 <-
                      salaryAtDate4 <- salaryLast <- 
                          ownership <-
                              sidejob <- 
                                  matrix(0, nrow(m), length(ES_employmentSpells))
      colnames(howFoundJob) <- colnames(partOrFullTime) <- colnames(partTimeHours) <- colnames(partTimeReason) <-
          colnames(workMuni) <- colnames(workCounty) <-
              colnames(employeeStatus) <- 
                  colnames(salaryAtDate1) <- colnames(salaryAtDate2) <- colnames(salaryAtDate3) <-
                      colnames(salaryAtDate4) <- colnames(salaryLast) <- 
                          colnames(ownership) <- colnames(sidejob) <- 
                              ES_employmentSpells
      for(job in seq(along=ES_employmentSpells)) {
         i <- ES_employmentSpells[job]
         i9 <- ES_employmentSpells9[job]
         howFoundJob[,i] <- m[[paste(ES_P_howFoundJob, i, sep="")]]
         startDates[,job] <- ISOdate((ES_addToYear + m[[paste(ES_P_yearOfStartOfJob,i,sep="")]]),
                                     m[[paste(ES_P_monthOfStartOfJob,i,sep="")]], 1)
         endDates[, job] <- ISOdate((ES_addToYear+m[[paste(ES_P_yearOfEndOfJob,i,sep="")]]), m[[paste(ES_P_monthOfEndOfJob,i,sep="")]],1)
         occupation[, job] <- m[[paste(ES_P_occupation,i,sep="")]]
         partOrFullTime[, i] <- m[[paste(ES_P_partOrFullTime,i,sep="")]]
         partTimeHours[, i] <- m[[paste(ES_P_partTimeHours,i,sep="")]]
         partTimeReason[, i] <- m[[paste(ES_P_partTimeReason,i,sep="")]]      
         employeeStatus[,i] <- m[[paste(ES_P_employeeStatus,i,sep="")]]
         industry[, job] <- m[[paste(ES_P_industry,i,sep="")]]
         nWorkers[, job] <- m[[paste(ES_P_nWorkers,i,sep="")]]
         publicSector[, job] <- m[[paste(ES_P_publicSector,i,sep="")]]
         if(etuName >= "1998") {
            workMuni[, job] <- muni[[paste(ES_P_workMuni, i, sep="")]]
            workCounty[, job] <- county(m[[paste(ES_P_workCounty,i,sep="")]])
            cityOfWork[, job] <- m[[paste(ES_P9_workCity, i9, sep="")]]
         }
         else {
            workMuni[, job] <- defactor(m[[paste(ES_P_workCounty, i, sep="")]])
            workCounty[, job] <- county(workMuni[, job])
            cityOfWork[, job] <- isTown(workMuni[, job])
         }
         if(etuName == "1995") {
            workCountry[, job] <- m[[paste(ES_P9_workCountry, i, sep="")]]
            salaryLast[, i] <- m[[paste(ES_P_salaryLast,i,sep="")]]
         }
         else {
            workCountry[, job] <- m[[paste(ES_P9_workCountry, i9, sep="")]]
         }
         employeeStatus <- convertEmployeeStatus(employeeStatus, etuName)
         salaryAtDate1[, i] <- m[[paste(ES_P_salaryAtDate1, i,sep="")]]
         salaryAtDate2[, i] <- m[[paste(ES_P_salaryAtDate2, i,sep="")]]
         salaryAtDate3[, i] <- m[[paste(ES_P_salaryAtDate3, i,sep="")]]
         if(etuName == "1997") {
            salaryAtDate4[, job] <- m[[paste(ES_P_salaryAtDate4, i,sep="")]]
         }
         if(etuName == "1995") {
            ownership[,i] <- convertOwnership95(m[[paste(ES_P_ownerType, i,sep="")]], m[[paste(ES_P_ownership, i,sep="")]])
         }
         else
             ownership[, i] <- m[[paste(ES_P_ownership, i,sep="")]]
         sidejob[, i] <- m[[paste(ES_P_sidejob, i, sep="")]]      
      }
      if(etuName == "1995") {
         nWorkers <- convertNWorkers95(nWorkers)
      }
      howFoundJob <- convertSearch95(howFoundJob)
      occupation <- convertOccupation(occupation)
      if(etuName <= "1997") {
         industry <- convertIndustry95(industry)
      }
      if(etuName == "1995") {
         prevIndustry <- prevOccupation <- rep(NA, nrow(m))
      }
      else if(etuName == "1997") {
         prevOccupation <- m[[ES_S_prevOccupation]]
         prevIndustry <- convertIndustry95(m[[ES_S_prevIndustry]])
      }
      else {
         prevOccupation <- m[[ES_S_prevOccupation]]
         prevIndustry <- m[[ES_S_prevIndustry]]
      }
      prevOccupation <- convertOccupation(prevOccupation)
      ##
      if(print.level > 0)
          cat("\npublic sector")
      publicSector <-
                           # publicly owned non-profit establishment
          if(etuName == "1995") {
             (publicSector == 2) & (ownership %in% c(1, 2, 7, 8, 9, 10))
          }
          else if(etuName < "1999") {
             (publicSector == 2) & (ownership %in% c(10, 20))
          }
          else {
             (publicSector == 12) & (ownership %in% c(10, 20))
          }
      
      ##
      ##  INFORMATION ABOUT JOBSEEKING/INACTIVITY
      ##
      UnempStart <- endDatesUnemp <-
          matrix(ISOdate(2009, 11, 12), nrow(m),
                 length(ES_USpells))
      startDatesInactive <- endDatesInactive <-
          matrix(ISOdate(2009, 11, 12), nrow(m),
                 length(ES_NSpells))
      colnames(UnempStart) <- colnames(endDatesUnemp) <-
          ES_USpells
      colnames(startDatesInactive) <- colnames(endDatesInactive) <-
          ES_NSpells
      for(i in ES_USpells) {
         UnempStart[, i] <- ISOdate((ES_addToYear+m[[paste(ES_P_UnempStart_Year,i,sep="")]]),
                                    m[[paste(ES_P_UnempStart_Month, i, sep="")]], 1)
         endDatesUnemp[, i] <- ISOdate((ES_addToYear+m[[paste(ES_P_endDatesUnemp_Year,i,sep="")]]),
                                       m[[paste(ES_P_endDatesUnemp_Month, i, sep="")]],1)
      }
      for(i in ES_NSpells) {
         startDatesInactive[, i] <- ISOdate((ES_addToYear+m[[paste(ES_P_startDatesInactive_Year,i,sep="")]]), m[[paste(ES_P_startDatesInactive_Month, i, sep="")]],1)
         endDatesInactive[, i] <- ISOdate((ES_addToYear+m[[paste(ES_P_endDatesInactive_Month,i,sep="")]]), m[[paste(ES_P_endDatesInactive_Month, i, sep="")]],1)
      }
      ## now we precompute in which company the person was working at the dates we
      ## are interested in. whereAtTime() will return a matrix with TRUE-s at the column
      ## representing company where respondend worked at the time. This way we can
      ## multplicate it with other massives and get only the number we are interested in.
      firm <- vector("list", nDate)
      for(dt in seq(length=nDate)) {
         firm[[dt]] <- whereAtTime(date[dt], startDates, endDates)
      }
      ##  in addition to wether respondent has some information about his/her job at the time, we
                           #  check wether they had a wage. We may introduce people with activity status "working"
                           #  as they answer about "autumn of year" but without working place at the Oct 15, what
                           #  we consider to be "autumn of the year" in other places.
                           #  We also assume that if there was no info about the respondents work or inactivity or
                           #  unemployment, she was inactive (student etc)
      workForceStatus <- matrix(0, nrow(startDates), nDate)
      for(dt in seq(length=nDate)) {
         workForceStatus[,dt] <- ifelse((apply(firm[[dt]],1, hasValue)), 1, 
                                        ifelse(anythingAtTime(date[dt], UnempStart, endDatesUnemp), 2,
                                               ifelse(anythingAtTime(date[dt], startDatesInactive, endDatesInactive), 3, 3)))
      }
      ##
      if(print.level > 1) {
         cat("\nbirthday & household ethnicity")
      }
      HHEthnicity <- HHDatesOfBirth <- matrix(0, nrow(m), length(ES_columnsOfHHMembers))
                           # HHDatesOfBirth will be a matrix of the birthdates of
                           # the members of the household. One row for every
                           # household and one column for every member.  
      class(HHDatesOfBirth) <- c("POSIXt", "POSIXct")
      colnames(HHEthnicity) <- colnames(HHDatesOfBirth) <- ES_columnsOfHHMembers
      row.names(HHEthnicity) <- row.names(HHDatesOfBirth) <- localHHId
      if(etuName < "1998") {
                           # 1997 has no ethnicity information for members who are not questioned.
                           # Birthday is present
         if(etuName == "1997") {
            HHEthnicity[] <- NA
                           # no members: ethnicity NA
            MList <- unique(m[[ES_respondent]])
                           # list of all household member ID-s (not household ID-s!)
            for(mem in MList) {
               im <- m[[ES_respondent]] == mem
               HHEthnicity[as.character(localHHId[im]), mem] <- m[[ES_ethnicity]][im]
            }
            HHEthnicity <- HHEthnicity[as.character(localHHId[!duplicated(localHHId)]),]
         }
         else {
            HHEthnicity[] <- NA
                           # no ethnicity information for other HH members in LFS1995
            HHEthnicity <- HHEthnicity[!duplicated(localHHId),]
         }
         HHEthnicity <- HHEthnicity == 15
                           # only distinguish Estonian - non-Estonian (coded 15 before 1998)
         for(i in colnames(HHDatesOfBirth)) {
            ## Next, fill in the birth dates
            bDay <- m[[paste(ES_P_HHBirthDay, i, sep="")]]
            bDay[is.na(bDay)] <- defaultBirthDay
                           # LFS1995 only has bDay for the respondent, not for HH members
            HHDatesOfBirth[,i] <- ISOdate(ES_addToYear +
                                          m[[paste(ES_P_HHBirthYear,i,sep="")]],
                        m[[paste(ES_P_HHBirthMonth, i, sep="")]], bDay)
         }
      }
      else {
         for(i in colnames(HHDatesOfBirth)) {
            HHDatesOfBirth[,i] <- ISOdate(ES_addToYear +
                                          m[[paste(ES_P_HHBirthYear,i,sep="")]],
                                          defaultBirthMonth, defaultBirthDay)
            HHEthnicity[,i] <- m[[paste(ES_P_ethnicity, i, sep="")]]
            HHEthnicity <- HHEthnicity == 1
                           # only distinguish Estonian - non-Estonian (coded 1 since 1998)
         }
      }
                           # now we have the birthdates of all the member.  However
                           # note that this data is only written for the hh 'heads'
      HHDatesOfBirth <- HHDatesOfBirth[!duplicated(as.character(localHHId)),]
                           # now we have birthdates for all household members.
                           # row names can be used for indexing by household id as
                           # HHDatesOfBirth[as.character(localHHId),]
      interEthHH <- apply(HHEthnicity, 1, function(x) any(x[!is.na(x)] != x[1]))
                           # logical, whether households include members of more than one ethnic group
      birthDay <- HHDatesOfBirth[as.character(localHHId),]
                           # matrix of birthdays of all household members
      birthDay <- birthDay[cbind(seq(length=nrow(m)),
                                 match(formatC(idMember - HHMember0, width=2, flag="0"),
                                       colnames(HHDatesOfBirth)))]
                           # birthday of the relevant member only
      ##
      ## LANGUAGES AND ETHNICITY
      ##
      if(print.level > 1) {
         cat("\nlanguage")
      }
      ## Estonian language.  Home language has priority over other languages
      ## Language codes: 1 - Russian, 15 - Estonian, 153 - English, 179 -
      ## Finnish
      lang1 <- m[[LFSLang1]]
      lang1[is.na(lang1)] <- 0
      lang2 <- m[[LFSLang2]]
      lang2[is.na(lang2)] <- 0
      lang3 <- m[[LFSLang3]]
      lang3[is.na(lang3)] <- 0
      if(etuName == "1997") {
         lang4 <- rep(0, nrow(m))
         lang5 <- rep(0, nrow(m))
      }
      else {
         lang4 <- m[[LFSLang4]]
         lang4[is.na(lang4)] <- 0
         lang5 <- m[[LFSLang5]]
         lang5[is.na(lang4)] <- 0
      }
      estLevel<-ifelse(lang1 == 15, m[[LFSLangLevel1]],
                       ifelse(lang2 == 15, m[[LFSLangLevel2]],
                              ifelse(lang3 == 15, m[[LFSLangLevel3]],
                                     ifelse(lang4 == 15,
                                            m[[LFSLangLevel4]], NA))))
      homeLangEE <- switch(etuName,
                           "1995" =, "1997" = 15,
                           1)
      homeLangRU <- switch(etuName,
                           "1995" =,"1997" = 1,
                           2)
                           # how are home languages coded in LFS
      homeLang <- ifelse(!is.na(m[[LFSLangLevel_HomeLang1]]) & (m[[LFSLangLevel_HomeLang1]] == homeLangEE), TRUE,
                         ifelse(!is.na(m[[LFSLangLevel_HomeLang2]]) &
                                (m[[LFSLangLevel_HomeLang2]] == homeLangEE), TRUE, FALSE))
      estLevel[homeLang] <- "home"
      rusLevel<-ifelse(!is.na(lang1) & (lang1 == 1), m[[LFSLangLevel1]],
                       ifelse(!is.na(lang2) & (lang2 == 1), m[[LFSLangLevel2]],
                              ifelse(!is.na(lang3) & (lang3==1), m[[LFSLangLevel3]],
                                     ifelse(!is.na(lang4) &(lang4 == 1), m[[LFSLangLevel4]], NA))))
      homeLang <- ifelse(!is.na(m[[LFSLangLevel_HomeLang1]]) & (m[[LFSLangLevel_HomeLang1]] == homeLangRU), TRUE,
                         ifelse(!is.na(m[[LFSLangLevel_HomeLang2]]) &
                                (m[[LFSLangLevel_HomeLang2]] == homeLangRU), TRUE,
                                FALSE))
      rusLevel[homeLang] <- "home"
      engLevel <- FILevel <- numeric(nrow(m))
      i <- !is.na(lang1) & (lang1 == 153)
      engLevel[i] <- m[[LFSLangLevel1]][i]
      i <- !is.na(lang2) & (lang2 == 153)
      engLevel[i] <- m[[LFSLangLevel2]][i]
      !is.na(lang3) & (lang3 == 153)
      engLevel[i] <- m[[LFSLangLevel3]][i]
      i <- !is.na(lang4) & (lang4 == 153)
      engLevel[i] <- m[[LFSLangLevel4]][i]
      i <- !is.na(lang5) & (lang5 == 153)
      engLevel[i] <- m[[LFSLangLevel5]][i]
      ##
      i <- !is.na(lang1) & (lang1 == 179)
      FILevel[i] <- m[[LFSLangLevel1]][i]
      i <- !is.na(lang2) & (lang2 == 179)
      FILevel[i] <- m[[LFSLangLevel2]][i]
      i <- !is.na(lang3) & (lang3 == 179)
      FILevel[i] <- m[[LFSLangLevel3]][i]
      i <- !is.na(lang4) & (lang4 == 179)
      FILevel[i] <- m[[LFSLangLevel4]][i]
      i <- !is.na(lang5) & (lang5 == 179)
      FILevel[i] <- m[[LFSLangLevel5]][i]
      ##
      ## Ethnicity
      ##
      ethnicity<-m[[ES_ethnicity]]
      ethEE <- switch(etuName,
                      "1995"=, "1997" = 15,
                      1)
                           # which ethnicity code means "estonian"
      nonEst <- ifelse(ethnicity != ethEE, TRUE, 
                       ifelse(is.na(ethnicity), NA, FALSE))
      ##
      ##    PUTING IT ALL TOGETHER
      ##
      waveData <- NULL
      for(dt in seq(length=nDate)) {
         if(print.level > 2)
             cat("\n date", dt)
         HHAges <- difftime(date[dt], HHDatesOfBirth[as.character(localHHId),], units="days")/365.2422
                           # ages of HH members
         age63.64 <- apply(HHAges >= 63 & HHAges < 65, 1, function(x) sum(x, na.rm=TRUE))[as.character(localHHId)]
         age65. <- apply(HHAges >= 65, 1, function(x) sum(x, na.rm=TRUE))[as.character(localHHId)]
         personWeight <-
             if(etuName == "1995")
                 rep(NA, nrow(m))
             else {
                m[[paste(ES_weight, formatC(year(date[dt]) %% 100, width=2, flag="0"), sep="")]]
                           # 2-digit version of 2 last digit of year
             }
         d <- list(id=idObs,
                   idPerson=idPerson, idHousehold=idHousehold, idMember=idMember, 
                   etuName=etuName,
                   wave=1,
                   retrospective= TRUE,
                   sex=m[[ES_male]],
                   birthYear= year(birthDay),
                   age=floor(as.integer(date[dt] - birthDay)/365),
                   nonEst=nonEst,
                   immigrYear=ES_addToYear+m[[ES_immigrYear]],
                   kids0.3 = apply(HHAges > 0 & HHAges < 4, 1, function(x) sum(x, na.rm=TRUE))[as.character(localHHId)],
                   kids4.6 = apply(HHAges >= 4 & HHAges < 7, 1, function(x) sum(x, na.rm=TRUE))[as.character(localHHId)],
                   kids7.17 = apply(HHAges >= 7 & HHAges < 18, 1, function(x) sum(x, na.rm=TRUE))[as.character(localHHId)],
                   age63.64 = age63.64,
                   age65. = age65.,
                   estLevel=estLevel,
                   rusLevel=rusLevel,
                   engLevel=engLevel,
                   FILevel = FILevel,
                   residenceCounty = moveData(date[dt], residenceCounty, moveDate),
                   residenceMuni = moveData(date[dt], residenceMuni, moveDate),
                   wage=matIndex(firm[[dt]], salaryAtDate1),
                   date=date[dt],
                   firstJob=firstJob,
                   ## main job stuff
                   howFoundJob = matIndex(firm[[dt]], howFoundJob),
                   nWorkers= matIndex(firm[[dt]], nWorkers),
                   publicSector= matIndex(firm[[dt]], publicSector),
                   industry = matIndex(firm[[dt]], industry),
                   ownership= matIndex(firm[[dt]], ownership),
                   townOrCountryWork = matIndex(firm[[dt]], cityOfWork),
                   workCounty= matIndex(firm[[dt]], workCounty),
                   workMuni = matIndex(firm[[dt]], workMuni),
                   workCountry = matIndex(firm[[dt]], workCountry),
                   occupation= matIndex(firm[[dt]], occupation),
                   employeeStatus = matIndex(firm[[dt]], employeeStatus),
                   partOrFullTime = matIndex(firm[[dt]], partOrFullTime),
                   partTimeHours = matIndex(firm[[dt]], partTimeHours),
                   partTimeReason = convertPartReason(matIndex(firm[[dt]], partTimeReason), TRUE, 2000, 1),
                   workhoursTotal= NA,
                   experienceInCompany=round((as.integer(date[dt])-matIndex(firm[[dt]], startDates))/secsInYear,1),
                   sidejob = matIndex(firm[[dt]], sidejob),
                   unionMember = NA,
                   ## Unemployment
                   UDuration = NA,
                   UBEligibility = NA,
                   UIBenefits = NA,
                   UAllowance = NA,
                   howSearchJob = NA,
                   mainSearchMethod = NA,
                   reservationWage = NA,
                   reservationWageLevels = NA,
                   searchParttime = NA,
                   prevIndustry = NA,
                   prevOccupation = NA,
                   ## Education
                   edu = retrospectiveEducation(date[dt], edu, eduEndDates, eduFinishingStatus,
                                              schoolType, preSchoolType),
                   isced97 = NA,
                   studying = anythingAtTime(date[dt], eduStartDates, eduEndDates),
                           # whether studying right now
                   ##
                   maritalStatus= moveData(date[dt], marStat, maritalDate),
                   interEthHH = interEthHH[as.character(localHHId)],
                   workForceStatus= workForceStatus[,dt],
                   activityStatus=NA,
                   personWeight= personWeight
                   )
      if(print.level > 0) {
         cat("\n")
      }
      l <- sapply(d, length)
      if(!all(l %in% c(1, nrow(m)))) {
         print(l)
         stop("variable lengths differ in data frame")
      }
      waveData <- rbind(waveData, as.data.frame(d))
   }
      ##
      ##   THE SURVEY WEEK
      ##
      if(doDateCurrent) {
         if(print.level > 1) {
            cat("\nsurvey week")
         }
         cDate <- ISOdate(surveyYear, m[[ES_surveyMonth]], m[[ES_surveyDay]])
         if(print.level > 0) {
            cat("\n", sum(is.na(cDate)), "missing dates")
         }
         cDate[is.na(cDate)] <- defaultSurveyDate
         mainJob <- defactor(m[[ES_C_mainJob]])
                           # which of the former main jobs is the current main job
                           # the columns in the form 1, 2, 3, ...
         mainJobIdx <- cbind(seq(length=nrow(m)), mainJob)
                           # index matrix for extracting the necessary data from main
                           # job matrices
         employeeStatusC <- employeeStatus[mainJobIdx]
                           # employee status at the current main job
         sidejob <- 1 + is.na(m[[paste(ES_C_occupation,
                                       ES_surveyJobCols[2], sep="")]])
                           # is occupation at job 2 NA?
         if(etuName < "1998") {
            wage <- salaryLast[mainJobIdx]
         }
         else {
            wage <- m[[paste(ES_C_surveySalary,
                             ES_surveyJobCols[1], sep="")]]
         }
         ## tenure at the current job.  We cannot use ifelse here because it breaks the class
         if(print.level > 2)
             cat("\ntenure at current job ")
         startDate <- startDates[mainJobIdx]
         experienceInCompany <- difftime(cDate, startDate, units="days")/365.2422
         rm(startDate)
         ##
         if(print.level > 2)
             cat("\nworking hours ")
         currentWorkhours <- matrix(0, nrow(m), length(ES_surveyJobCols))
         colnames(currentWorkhours) <- ES_surveyJobCols
         for(i in ES_surveyJobCols) {
            currentWorkhours[, i] <- m[[paste(ES_C_surveyWorkHours, i, sep="")]]
         }
         partOrFullTime <- 1 + (currentWorkhours[,1] < 35)
                           # first columns should be the main job
                           # 1 - full, 2 - part time
         ##
         age <- difftime(cDate, birthDay, units="days")/365.2422
         HHAges <- difftime(cDate, HHDatesOfBirth[as.character(localHHId),], units="days")/365.2422
                           # ages of HH members
         age63.64 <- apply(HHAges >= 63 & HHAges < 65, 1, function(x) sum(x, na.rm=TRUE))[as.character(localHHId)]
         age65. <- apply(HHAges >= 65, 1, function(x) sum(x, na.rm=TRUE))[as.character(localHHId)]
         statusC <- ifelse((m[[ES_surveyEmployee]] == 1 | m[[ES_surveyEnterpreneur]] == 1 | m[[ES_surveyFarmer]] == 1 |
                            m[[ES_surveyAbsent]] == 1),
                           1,
                           ifelse(m[[ES_surveySearching]] == 1, 2, 3))
                           # Note: you might want to check whether the guy is actually willing to take a job
         if(etuName == "1995")
             activityStatus <- m[[ES_C_activity]]
         else
             activityStatus<-ifelse((m[[ES_C_activity]]<6), m[[ES_C_activity]], 
                                    ifelse(m[[ES_C_activity]]>6, m[[ES_C_activity]]-1, 14))
                           # child of preschool age will be 14, meaning "other".
                           # But we do have some 40-year old kids of preschool age according to the data...
         personWeight <-
             if(etuName == "1995")
                 rep(NA, nrow(m))
             else
                 m[[paste(ES_weight, substr(surveyYear, 3, 4), sep="")]]
         d <- list(id=idObs,
                         idPerson=idPerson, idHousehold=idHousehold,
                   idMember=idMember, 
                         etuName=etuName,
                         wave= 1,
                         retrospective=FALSE,
                         sex=m[[ES_male]],
                   birthYear= year(birthDay),
                         age=age,
                         nonEst=nonEst,
                         immigrYear=ES_addToYear+m[[ES_immigrYear]],
                         kids0.3 = apply(HHAges > 0 & HHAges < 4, 1, function(x) sum(x, na.rm=TRUE))[as.character(localHHId)],
                         kids4.6 = apply(HHAges >= 4 & HHAges < 7, 1, function(x) sum(x, na.rm=TRUE))[as.character(localHHId)],
                         kids7.17 = apply(HHAges >= 7 & HHAges < 18, 1, function(x) sum(x, na.rm=TRUE))[as.character(localHHId)],
                   age63.64 = age63.64,
                   age65. = age65.,
                         estLevel=estLevel,
                         rusLevel=rusLevel,
                         engLevel=engLevel,
                   FILevel = FILevel,
                         residenceCounty = moveData(cDate, residenceCounty, moveDate),
                         residenceMuni = moveData(cDate, residenceMuni, moveDate),
                         wage = wage,
                         date=cDate,
                         ##
                         firstJob=firstJob,
                         howFoundJob = howFoundJob[mainJobIdx],
                         nWorkers = nWorkers[mainJobIdx],
                         publicSector=publicSector[mainJobIdx],
                         industry=industry[mainJobIdx],
                         ownership=ownership[mainJobIdx],
                         townOrCountryWork = cityOfWork[mainJobIdx],
                         workCounty=workCounty[mainJobIdx],
                         workMuni = workMuni[mainJobIdx],
                         workCountry=workCountry[mainJobIdx],
                         occupation= convertOccupation(m[[paste(ES_C_occupation,
                                                       ES_surveyJobCols[1], sep="")]]),
                         employeeStatus = employeeStatusC,
                         partOrFullTime=partOrFullTime,
                         partTimeHours = currentWorkhours[,1],
                           # simply hours at the main job (not necessarily part time)
                         partTimeReason = convertPartReason(m[[ES_C_surveyPartReason]], FALSE, 2000, 1),
                         workhoursTotal = apply(currentWorkhours, 1, function(x) sum(x, na.rm=TRUE)),
                           # total working hours at all jobs
                         experienceInCompany=experienceInCompany,
                         sidejob = sidejob,
                         unionMember = m[[ES_unionMember]] == 1,
                         ## Unemployment
                         UDuration = NA,
                         UBEligibility = NA,
                         UIBenefits = NA,
                         UAllowance = NA,
                   howSearchJob = NA,
                   mainSearchMethod = NA,
                   reservationWage = NA,
                   reservationWageLevels = NA,
                   searchParttime = NA,
                   prevIndustry = prevIndustry,
                   prevOccupation = prevOccupation,
                         ## Education
                         edu = edu,
                   isced97 = isced,
                         studying = apply(eduFinishingStatus, 1, function(x) any(x == 3, na.rm=TRUE)),
                           # any education has status 3: still studying
                         maritalStatus= moveData(cDate, marStat, maritalDate),
                   interEthHH = interEthHH[as.character(localHHId)],
                         workForceStatus = statusC,
                         activityStatus = activityStatus,
                         personWeight= personWeight
                         )
         if(print.level > 0) {
            cat("\n")
         }
         l <- sapply(d, length)
         if(!all(l %in% c(1, nrow(m)))) {
            print(l)
            stop("variable lengths differ in data frame")
         }
         waveData <- rbind(waveData, as.data.frame(d))
         return(waveData)
      }
   }
   fullData1995.2000q2 <-doEtu("1995")
   for(etu in c("1997", "1998", "1999", "2000_1j2")) {
      etuC<-doEtu(etu)
      fullData1995.2000q2 <-rbind(fullData1995.2000q2, etuC)
   }
   fName <- "dataOf1995-2000q2All.Rdat"
   save(fullData1995.2000q2, file=fName)
   invisible(fullData1995.2000q2)
}
