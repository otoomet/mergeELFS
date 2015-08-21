### This file defines some parameters used by scripts
etuDir <- "~/tyyq/andmebaasiq/ETU/"
eraldaCommand <- "~/tyyq/andmebaasiq/eralda_muutujad"
scriptDir<-"~/tyyq/andmebaasiq/ETU/mergeELFS/"
              secsInYear<-31536000
              # parameters of age groups - we will compute how many household members
              # belonged to this age group at given date. It is quite problematic at retrospective
              # studies (as we do not know who has entered or left the household during the time)
              # first agegroup is 0-2 years old (less than 3 years old)
defaultBirthday <- 14
             #From ETU1998 we do not get month of birth anymore. This makes accounting
             #for narrow agegroups of children more or less meaningless. 
defaultBirthMonth <-6 
             defaultDateOfStartingAJob<-1
defaultMoveDay <- 1
                                        # day of month of residential
                                        # mobility
defaultJobseekDay <- 1
                           # DOM when started to look for a job
defaultMaritalDay <- 15
                           # when marital status changes
defaultSalaryDay <- 15
                           # when salary measured
options(stringsAsFactors = FALSE)
                                        # keep strings as strings in data frames
