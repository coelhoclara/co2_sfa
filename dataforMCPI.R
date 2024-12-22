library(readxl)
library(dplyr)
library(tidyr)
library(tidyverse)
library(scales)

co2emissions <- read_excel("D:\\Phd Projects\\co2_sfa\\co2emissions.xlsx")
pwt <- read_excel("D:\\Phd Projects\\co2_sfa\\pwt1001.xlsx",sheet=3)

pwt <- pwt %>% select(countrycode, country, year, rgdpe, rgdpo, rgdpna, emp, cn, rnna)
pwt <- subset(pwt, year>=1975)

co2emissions_long <- co2emissions %>% pivot_longer( cols="1975":"2022",names_to="year",values_to = "c02emissions")
co2emissions_long <- co2emissions_long[,-3]
co2emissions_long$year <- as.numeric(co2emissions_long$year)

dataforMCPI <- pwt %>% left_join(co2emissions_long, by=c("countrycode"="iso","year"))

country <- dataforMCPI %>% select(countrycode)
country <- unique(country)
country <- rowid_to_column(country, "countryid")

dataforMCPI <- dataforMCPI %>% left_join(country)

containna <- dataforMCPI %>%
  filter_all(any_vars(is.na(.)))

containna <- select(containna, countrycode, countryid)
containna <- unique(containna)

dataforMCPI <- subset(dataforMCPI, !(countryid %in% containna$countryid))
unique(dataforMCPI$countrycode)

#drop oil countries
dataforMCPI <- subset(dataforMCPI, !(countrycode %in% c("ARE","KWT","QAT","VEN")))

#prepare to write data
#keep only numerical variables
dataforMCPI_tofile <- dataforMCPI %>% select(-c("countrycode","country","Country/Region"))

# #scale original variables between 1 and 2 
# dataforMCPI_tofile$emp <- rescale(dataforMCPI_tofile$emp)+1
# dataforMCPI_tofile$rgdpe <- (rescale(dataforMCPI_tofile$rgdpe)+2)
# dataforMCPI_tofile$cn <- (rescale(dataforMCPI_tofile$cn)+2)
# dataforMCPI_tofile$c02emissions <- rescale(dataforMCPI_tofile$c02emissions)+1

#write new variables

dataforMCPI_tofile$l_rscale <-  dataforMCPI_tofile$emp/mean(dataforMCPI_tofile$emp)
dataforMCPI_tofile$y_rscale <- dataforMCPI_tofile$rgdpe/mean(dataforMCPI_tofile$rgdpe)
dataforMCPI_tofile$k_rscale <- dataforMCPI_tofile$cn/mean(dataforMCPI_tofile$cn)
dataforMCPI_tofile$c_rscale <- dataforMCPI_tofile$c02emissions/mean(dataforMCPI_tofile$c02emissions)

dataforMCPI_tofile <- dataforMCPI_tofile %>% group_by(countryid) %>% mutate(lbari = mean(l_rscale),
                                                                            cbari = mean(c_rscale),
                                                                            ybari = mean(y_rscale),
                                                                            kbari = mean(k_rscale))

dataforMCPI_tofile$l <- log(dataforMCPI_tofile$l_rscale)-log(dataforMCPI_tofile$lbari)
dataforMCPI_tofile$y <- log(dataforMCPI_tofile$y_rscale)-log(dataforMCPI_tofile$ybari)
dataforMCPI_tofile$k <- log(dataforMCPI_tofile$k_rscale) - log(dataforMCPI_tofile$kbari)
dataforMCPI_tofile$c <- log(dataforMCPI_tofile$c_rscale) - log(dataforMCPI_tofile$cbari)
dataforMCPI_tofile$t <- dataforMCPI_tofile$year - 1974
dataforMCPI_tofile$l2 <- 0.5*(dataforMCPI_tofile$l)^2
dataforMCPI_tofile$y2 <- 0.5*(dataforMCPI_tofile$y)^2
dataforMCPI_tofile$k2 <- 0.5*(dataforMCPI_tofile$k)^2
dataforMCPI_tofile$t2 <- 0.5*dataforMCPI_tofile$t^2
dataforMCPI_tofile$ly <- dataforMCPI_tofile$l*dataforMCPI_tofile$y
dataforMCPI_tofile$lk <- dataforMCPI_tofile$l*dataforMCPI_tofile$k
dataforMCPI_tofile$lt<- dataforMCPI_tofile$l*dataforMCPI_tofile$t
dataforMCPI_tofile$yk <- dataforMCPI_tofile$y*dataforMCPI_tofile$k
dataforMCPI_tofile$yt <- dataforMCPI_tofile$y*dataforMCPI_tofile$t
dataforMCPI_tofile$kt <- dataforMCPI_tofile$k*dataforMCPI_tofile$t

# dataforMCPI_tofile$lnkbar <- log(mean(dataforMCPI_tofile$cn)) 
# dataforMCPI_tofile$lnlbar <- log(mean(dataforMCPI_tofile$emp)) 
# dataforMCPI_tofile$lnybar <- log(mean(dataforMCPI_tofile$rgdpe)) 
# dataforMCPI_tofile$lncbar <- log(mean(dataforMCPI_tofile$c02emissions)) 
# dataforMCPI_tofile$tbar <- (mean(dataforMCPI_tofile$t)) 
# dataforMCPI_tofile$kt <- dataforMCPI_tofile$log_K - dataforMCPI_tofile$lnkbar 
# dataforMCPI_tofile$lt <- dataforMCPI_tofile$log_L - dataforMCPI_tofile$lnlbar 
# dataforMCPI_tofile$yt <- dataforMCPI_tofile$log_Y - dataforMCPI_tofile$lnybar 
# dataforMCPI_tofile$ct <- dataforMCPI_tofile$log_C - dataforMCPI_tofile$lncbar 
# dataforMCPI_tofile$ttilda <- dataforMCPI_tofile$t - dataforMCPI_tofile$tbar 
# 
# dataforMCPI_tofile$kt2 <- dataforMCPI_tofile$kt*dataforMCPI_tofile$kt
# dataforMCPI_tofile$yt2 <- dataforMCPI_tofile$yt*dataforMCPI_tofile$yt
# dataforMCPI_tofile$lt2 <- dataforMCPI_tofile$lt*dataforMCPI_tofile$lt
# dataforMCPI_tofile$ttilda2 <- dataforMCPI_tofile$ttilda*dataforMCPI_tofile$ttilda
# 
# dataforMCPI_tofile$kl <- dataforMCPI_tofile$kt*dataforMCPI_tofile$lt
# dataforMCPI_tofile$ky <- dataforMCPI_tofile$kt*dataforMCPI_tofile$yt
# dataforMCPI_tofile$ly <- dataforMCPI_tofile$lt*dataforMCPI_tofile$yt
# dataforMCPI_tofile$ktilda <- dataforMCPI_tofile$kt*dataforMCPI_tofile$ttilda
# dataforMCPI_tofile$ltilda <- dataforMCPI_tofile$lt*dataforMCPI_tofile$ttilda
# dataforMCPI_tofile$ytilda <- dataforMCPI_tofile$yt*dataforMCPI_tofile$ttilda




saveRDS(dataforMCPI_tofile,"dataforMCPI_tofile_new.RDS")

write.table(dataforMCPI_tofile,"dataforMCPI.raw", sep=" ", quote = FALSE, row.names = FALSE)                      


#select only one country and write
#Brazil = 25
dataforMCPI_tofile <- dataforMCPI_tofile %>% filter(countryid==25)
write.table(dataforMCPI_tofile,"dataforMCPI_bra.raw", sep=" ", quote = FALSE, row.names = FALSE)                      
