### LOADING LIBRARY #############
library(readr)
library(naniar) #remove NA data points
data <- read_csv("https://ed-public-download.app.cloud.gov/downloads/Most-Recent-Cohorts-All-Data-Elements.csv")


### DATA PREPROCESSING #############
myvars <- c("UNITID", "INSTNM", "CITY", "STABBR", "INSTURL", "NPCURL", "CONTROL", "LATITUDE", "REGION",
            "LONGITUDE", "CCUGPROF", "ADM_RATE", "SAT_AVG", "UGDS", "COSTT4_A","TUITIONFEE_IN", "LOCALE",
            "TUITIONFEE_OUT", "PFTFAC","C100_4","RET_FT4","GRAD_DEBT_MDN","AGE_ENTRY", "MENONLY", "WOMENONLY",
            "UNEMP_RATE","MD_EARN_WNE_P10","UGDS_MEN","UGDS_WOMEN")
newdata = subset(data, (ICLEVEL == 1 & CCUGPROF >9), select = myvars)
newdata = replace_with_na_all(newdata, condition = (~.x == "NULL"))
newdata$ADM_RATE = as.numeric(newdata$ADM_RATE) * 100
newdata$C100_4 = as.numeric(newdata$C100_4) * 100
newdata$RET_FT4 = as.numeric(newdata$RET_FT4) * 100
newdata$UGDS_MEN = round(as.numeric(newdata$UGDS_MEN)*100)
newdata$UGDS_WOMEN = round(as.numeric(newdata$UGDS_WOMEN)*100)
newdata$AGE_ENTRY = round(as.numeric(newdata$AGE_ENTRY), digits = 1)
newdata$UNEMP_RATE = round(as.numeric(newdata$UNEMP_RATE), digits = 2)
newdata$PFTFAC = as.numeric(newdata$PFTFAC)*100

write_csv(newdata, "cleandata.csv")