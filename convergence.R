library(dplyr)
library(purrr)
library(sandwich)
library(ConvergenceClubs)
library(Amelia)
library(wrapr)
library(oglmx)
library(readxl)
library(lmtest)


datamcpi <- readRDS("datawithmcpi.RDS") #can use csv file "dataMCPI instead

conv_test <- datamcpi %>% mutate(hit = accmcpi/mean(accmcpi, na.rm = T)) %>% select(countryid, hit, t)
conv_test <- conv_test %>% group_by(t) %>% summarise(Ht = mean((hit-1)^2))
conv_test <- conv_test %>% mutate(logH1Ht = log(first(Ht)/Ht))
conv_test$loglt <- log(conv_test$t+1)

conv_test$lhs <- conv_test$logH1Ht - 2*conv_test$loglt
conv_test$logt <- log(conv_test$t*0.3)


model <- lm(lhs ~ logt, conv_test)
#reject overall convergence
coeftest(model, vcov = vcovHAC(model))

#club convergence 
wb <- read_xlsx("wbankdata.xlsx")
wb2 <- read_xlsx("energyuse.xlsx")
wb <- bind_rows(wb,wb2)
wb <- wb %>% mutate(var_mean = rowMeans(wb[5:20], na.rm=T)) 

codes <- dataforMCPI %>% select(countrycode, countryid)
codes <- dataforMCPI %>% select(countrycode, countryid, country)
codes <- distinct(codes)

wb <- wb %>% left_join(codes, by=c("CountryCode"="countrycode"))
wb <- wb %>% filter(!is.na(countryid))

wbdata_tomerge <- wb %>% select(countryid,SeriesName,var_mean)
wbdata_tomerge <- wbdata_tomerge %>% pivot_wider(id_cols=countryid,names_from = SeriesName, values_from = var_mean)
colnames(wbdata_tomerge) <- c("countryid","ed","es","ei_alt","rd","ur","st","td","ea","ei")


wbdata_tomerge <- wbdata_tomerge %>% mutate_all(~ifelse(is.nan(.), NA, .))
colnames(wbdata_tomerge) <- c("countryid","ed","es","ei_alt","rd","ur","st","td","ea","ei")

mcpidf <- datamcpi %>% select(countryid, year, mcpi, accmcpi)
mcpidf <- mcpidf %>% pivot_wider(id_cols=countryid, names_from = year, values_from = accmcpi, names_prefix = "year")
mcpidf <- mcpidf %>% select(-c(year2019)) %>% ungroup

clubs_mcpi_fix <- findClubs(data.frame(mcpidf),dataCols = 2:45, unit_names = 1, refCol = 45, time_trim = 1/3)

clubs_mcpi <- findClubs(data.frame(mcpidf),dataCols = 2:45, unit_names = 1, refCol = 45, time_trim = 1/3,
                        cstar_method = "incremental",cstar_increment = 0.8)
summary(clubs_mcpi)

mcclubs <- mergeClubs(clubs_mcpi, mergeMethod = "PS", mergeDivergent = T)
summary(mcclubs)


club1 <- as.numeric(clubs_mcpi$club1$unit_names)
club2 <- as.numeric(clubs_mcpi$club2$unit_names)
club3 <- as.numeric(clubs_mcpi$club3$unit_names)
club4 <- as.numeric(clubs_mcpi$club4$unit_names)
club5 <- as.numeric(clubs_mcpi$club5$unit_names)
club6 <- as.numeric(clubs_mcpi$club6$unit_names)

#checked that club1 has the highest carbon productivity, therefore gets higher value
mcpidf <- datamcpi %>% select(countryid, year, mcpi, accmcpi)
mcpidf <- mcpidf %>% mutate(club = case_when(countryid %in% club1 ~ 1, countryid %in% club2 ~ 2,
                                   countryid %in% club3 ~ 3, countryid %in% club4 ~ 4,
                                   countryid %in% club5 ~ 5, countryid %in% club6 ~ 6))
mcpidf <- mcpidf %>% group_by(countryid) %>% mutate(growth = accmcpi/lag(accmcpi, order_by = year ) )

clubs <- mcpidf %>% select(countryid,club) %>% distinct() %>% left_join(codes)

mcpidf <- mcpidf %>% group_by(club) %>% summarise(avgrowth = mean(growth, na.rm = T))

wbdata_imp <- wbdata_tomerge %>% mutate(club = case_when(countryid %in% club1 ~ 1, countryid %in% club2 ~ 2,
                                             countryid %in% club3 ~ 3, countryid %in% club4 ~ 4,
                                             countryid %in% club5 ~ 5, countryid %in% club6 ~ 6))

wbdata_imp <- wbdata_imp %>% filter(!is.na(club))
wbdata_imp$ed <- log(wbdata_imp$ed)

#ordered probit
oprobit <- oprobit.reg(club ~ ed + ei  + rd + ur + st + es+  td + ea, data=wbdata_imp)
summary(oprobit)

margins.oglmx(oprobit)

