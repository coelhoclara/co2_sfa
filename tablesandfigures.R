library(dplyr)
library(readxl)
library(ggplot2)
library(tidyr)
library(texreg)

datamcpi <- readRDS("datawithmcpi.RDS")
datamcpi <- readRDS("datawithmcpi_minus.RDS") #can use csv file "dataMCPI instead


#figure 1
dataforgraph1 <- datamcpi %>% select(countryid,Time,accmcpi)
dataforgraph1$countryid <- as.numeric(dataforgraph1$countryid)
group <- read.csv("list_dev.csv",sep=",")
dataforgraph1 <- dataforgraph1 %>% inner_join(group, by="countryid")


dataforgraph1 <- dataforgraph1 %>% filter(!is.na(accmcpi) & !is.na(group))
dataforgraph1 <- dataforgraph1 %>% group_by(group,Time) %>% summarise(avgmcpi = mean(accmcpi,na.rm = T))

dataforgraph1$group[dataforgraph1$group==1] <-"Developing countries"
dataforgraph1$group[dataforgraph1$group==2] <-"Developed countries"

dataforgraph1 %>% ggplot(aes(x=Time,y=avgmcpi, group = as.factor(group), color=as.factor(group)))+
  geom_line(linewidth=1.5)+ scale_y_continuous(breaks =seq(0.9,2,0.1))+
  theme_minimal() + theme(axis.title = element_blank(), legend.title = element_blank(), legend.position = "bottom",
                          plot.margin = margin(r=20, t=20, l=20)) 

#figure 2
dataforgraph2 <- datamcpi
dataforgraph2 <- dataforgraph2 %>% filter(CountryName=="Australia"|CountryName=="Canada"|CountryName=="Japan"|CountryName=="France"|
                                            CountryName=="United States"| CountryName=="United Kingdom")

dataforgraph2 %>% ggplot(aes(x=Time,y=accmcpi, group = as.factor(CountryName), color=as.factor(CountryName)))+
  geom_line(linewidth=1.5)+ 
  theme_minimal() + theme(axis.title = element_blank(), legend.title = element_blank(), legend.position = "bottom",
                          plot.margin = margin(r=20, t=20, l=20)) 


#figure 3
dataforgraph3 <- datamcpi
dataforgraph3 <- dataforgraph3 %>% filter(CountryName=="Argentina"|CountryName=="Brazil"|CountryName=="China"|CountryName=="India"|
                                            CountryName=="Mexico"| CountryName=="South Africa")

dataforgraph3 %>% ggplot(aes(x=Time,y=accmcpi, group = as.factor(CountryName), color=as.factor(CountryName)))+
  geom_line(linewidth=1.5)+ 
  theme_minimal() + theme(axis.title = element_blank(), legend.title = element_blank(), legend.position = "bottom",
                          plot.margin = margin(r=20, t=20, l=20)) 

#table 1
datamcpi <- datamcpi %>% group_by(countryid) %>% mutate(accmcpi = accumulate(mcpi, ~.x*.y),
                                                        acctecch = accumulate(tecch_rec, ~.x*.y),
                                                        acceffch = accumulate(effch_rec, ~.x*.y))

datatable1 <- datamcpi %>% select(countryid, Time, accmcpi, acctecch, acceffch)
datatable1 <- datatable1 %>% left_join(codes) %>% ungroup()
datatable1 <- datatable1 %>% filter(!is.na(accmcpi) & Time!=2019)
datatable1 <- datatable1 %>% group_by(CountryName) %>% summarise(MCPI = last(accmcpi, order_by=Time),
                                               EFFCH = last(acceffch, order_by=Time),
                                               TECCH = last(acctecch, order_by=Time))
group <- read.csv("list_dev.csv",sep=",")
datatable1 <- datatable1 %>% left_join(group)
write.csv(datatable1, "datatable1.csv")

avg <- datatable1 %>% group_by(group) %>% summarise(avgeffch = mean(EFFCH),
                                                           avgtecch = mean(TECCH),
                                                           avgMCPI = mean(MCPI))

#table 3
clubs <- clubs %>% group_by(club) %>% summarise(members = paste(CountryName,collapse=", "))
write.csv(clubs,"clubs_new.csv")

#table 6
summaryclubs <- mcpidf %>% group_by(club) %>% summarise(avgmcpi = mean(accmcpi, na.rm = T),std = sd(accmcpi, na.rm = T),
                                        minmcpi = min(accmcpi, na.rm = T), maxmcpi = max(accmcpi,na.rm=T),
                                        avggrowth = mean(growth,na.rm=T))
write.csv(summaryclubs,"summaryclubs.csv")  


#table
margins.oglmx(oprobit)
texreg(oprobit_m)

beta <- read.table("D:/estimations/long run - log re const/sfa_mu_u_MAIN_mu_REGRESSION_log_C_minus_LinearEffects_sample.raw",header = T, sep=" ")
beta_m <- beta %>% summarise_all(mean)
beta_sd <- beta %>% summarise_all(sd)

beta_m <- as.data.frame(t(beta_m))
beta_sd <- as.data.frame(t(beta_sd))

beta <- cbind(beta_m,beta_sd)
colnames(beta) <- c("beta","sd")
beta$sig <- beta$beta/beta$sd


mu_predict <- read.table("D:/estimations/long run - log re const/sfa_mu_u_MAIN_mu_REGRESSION_log_C_minus_predict.res",header = T, sep=" ")
sigmau <- mu_predict$pmean_E_sigma_u
mean(log(sigmau^2))
sd(log(sigmau^2))/sqrt(88)
sigmav <- mu_predict$pmean_param_sigma_v 

(mu_predict$pqu10_param_sigma_v - sigmav)/qnorm(0.1)
(mu_predict$pqu90_param_sigma_v - sigmav)/qnorm(0.9)

