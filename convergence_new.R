library(dplyr)
library(purrr)
library(sandwich)
library(ConvergenceClubs)
library(Amelia)
library(wrapr)
library(oglmx)
library(readxl)
library(lmtest)
library(convergEU)


datamcpi <- readRDS("datawithmcpi_minus.RDS") #can use csv file "dataMCPI instead

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

codes <- readRDS("codes.RDS")

wb <- wb %>% left_join(codes, by=c("CountryCode"))
wb <- wb %>% filter(!is.na(countryid))

wbdata_tomerge <- wb %>% select(countryid,SeriesName,var_mean)
wbdata_tomerge <- wbdata_tomerge %>% pivot_wider(id_cols=countryid,names_from = SeriesName, values_from = var_mean)
colnames(wbdata_tomerge) <- c("countryid","ed","es","ei_alt","rd","ur","st","td","ea","ei")


wbdata_tomerge <- wbdata_tomerge %>% mutate_all(~ifelse(is.nan(.), NA, .))
colnames(wbdata_tomerge) <- c("countryid","ed","es","ei_alt","rd","ur","st","td","ea","ei")

mcpidf <- datamcpi %>% select(countryid, Time, mcpi, accmcpi)
mcpidf <- mcpidf %>% pivot_wider(id_cols=countryid, names_from = Time, values_from = accmcpi, names_prefix = "Time")
mcpidf <- mcpidf %>% select(-c(Time2019)) %>% ungroup

clubs_mcpi_fix <- findClubs(data.frame(mcpidf),dataCols = 2:45, unit_names = 1, refCol = 45, time_trim = 1/3)
summary(clubs_mcpi_fix)

clubs_mcpi <- findClubs(data.frame(mcpidf),dataCols = 2:45, unit_names = 1, refCol = 45, time_trim = 1/3,
                        cstar_method = "incremental",cstar_increment = 0.01)
summary(clubs_mcpi)

mcclubs <- mergeClubs(clubs_mcpi, mergeMethod = "PS", mergeDivergent = T, threshold = -3)
summary(mcclubs)


club1 <- as.numeric(mcclubs$club1$unit_names)
club2 <- as.numeric(mcclubs$club2$unit_names)
club3 <- as.numeric(mcclubs$club3$unit_names)
club4 <- as.numeric(mcclubs$club4$unit_names)
club5 <- as.numeric(mcclubs$club5$unit_names)


#checked that club1 has the highest carbon productivity, therefore gets higher value
mcpidf <- datamcpi %>% select(countryid, Time, mcpi, accmcpi)
mcpidf <- mcpidf %>% mutate(club = case_when(countryid %in% club1 ~ 1, countryid %in% club2 ~ 2,
                                             countryid %in% club3 ~ 3, countryid %in% club4 ~ 4,
                                             countryid %in% club5 ~ 5))
mcpidf <- mcpidf %>% group_by(countryid) %>% mutate(growth = accmcpi/lag(accmcpi, order_by = Time ) )

clubs <- mcpidf %>% select(countryid,club) %>% distinct() %>% left_join(codes)

mcpidf <- mcpidf %>% group_by(club) %>% summarise(avgrowth = mean(growth, na.rm = T))

wbdata_imp <- wbdata_tomerge %>% mutate(club = case_when(countryid %in% club1 ~ 5, countryid %in% club2 ~ 4,
                                                         countryid %in% club3 ~ 3, countryid %in% club4 ~ 2,
                                                         countryid %in% club5 ~ 1))

imput.bound <- matrix(c(2,3,4,5,6,7,8,9,10,0,0,0,0,0,0,0,0,0,Inf,100,100,100,100,100,100,100,Inf),nrow=9,ncol=3)

wbdata_imp <- wbdata_tomerge %>% as.data.frame() %>% amelia(m=5, idvars ="countryid", bounds = imput.bound)

wbdata_imp <- as.data.frame(wbdata_imp$imputations[1])
colnames(wbdata_imp) <- c("countryid","ed","es","ei_alt","rd","ur","st","td","ea","ei")

wbdata_imp <- wbdata_imp %>% mutate(club = case_when(countryid %in% club1 ~ 5, countryid %in% club2 ~ 4,
                                                         countryid %in% club3 ~ 3, countryid %in% club4 ~ 2,
                                                         countryid %in% club5 ~ 1))


#colnames(wbdata_imp) <- c("countryid","ed","es","ei_alt","rd","ur","st","td","ea","ei","club")


wbdata_imp <- wbdata_imp %>% filter(!is.na(club))
wbdata_imp$ed <- log(wbdata_imp$ed)

#ordered probit
oprobit <- oprobit.reg(club ~ ed + ei  + rd + ur + st + es+  td + ea, data=wbdata_imp)
summary(oprobit)

margins.oglmx(oprobit)

#beta convergence tests
mcpidf <- datamcpi %>% select(countryid, Time, mcpi, accmcpi)
mcpidf <- mcpidf %>% mutate(club = case_when(countryid %in% club1 ~ 1, countryid %in% club2 ~ 2,
                                             countryid %in% club3 ~ 3, countryid %in% club4 ~ 4,
                                             countryid %in% club5 ~ 5))

club1 <- mcpidf %>% filter(club==1)
club1 <- pivot_wider(club1, id_cols=Time,names_from = countryid, values_from = mcpi,names_prefix = "country")
club1 <- club1 %>% na.exclude()

beta_conv(club1,time_0=1975, time_t=2018,all_within = T, timeName = "Time")                     

club2 <- mcpidf %>% filter(club==2)
club2 <- pivot_wider(club2, id_cols=Time,names_from = countryid, values_from = mcpi,names_prefix = "country")
club2 <- club2 %>% na.exclude()

beta_conv(club2,time_0=1975, time_t=2018,all_within = T, timeName = "Time") 

club3 <- mcpidf %>% filter(club==3)
club3 <- pivot_wider(club3, id_cols=Time,names_from = countryid, values_from = mcpi,names_prefix = "country")
club3 <- club3 %>% na.exclude()

beta_conv(club3,time_0=1975, time_t=2018,all_within = T, timeName = "Time") 

club4 <- mcpidf %>% filter(club==4)
club4 <- pivot_wider(club4, id_cols=Time,names_from = countryid, values_from = mcpi,names_prefix = "country")
club4 <- club4 %>% na.exclude()

beta_conv(club4,time_0=1975, time_t=2018, all_within = T, timeName = "Time") 

club5 <- mcpidf %>% filter(club==5)
club5 <- pivot_wider(club5, id_cols=Time,names_from = countryid, values_from = mcpi,names_prefix = "country")
club5 <- club5 %>% na.exclude()

beta_conv(club5,time_0=1975, time_t=2018,all_within = T, timeName = "Time") 




