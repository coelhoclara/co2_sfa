library(dplyr)
library(readxl)
library(ggplot2)
library(tidyr)
library(texreg)

datamcpi <- readRDS("datawithmcpi.RDS")

#figure 1
dataforgraph1 <- datamcpi %>% select(countryid,year,accmcpi)
dataforgraph1 <- dataforgraph1 %>% left_join(codes)
write.csv(dataforgraph1,"dataforgraph1.csv",dec=",")

dataforgraph1 <- read_xlsx("dataforgraph1.xlsx")
dataforgraph1 <- dataforgraph1 %>% filter(!is.na(accmcpi))
dataforgraph1 <- dataforgraph1 %>% group_by(group,year) %>% summarise(avgmcpi = mean(accmcpi,na.rm = T))

dataforgraph1$group[dataforgraph1$group==1] <-"Developing countries"
dataforgraph1$group[dataforgraph1$group==2] <-"Developed countries"

dataforgraph1 %>% ggplot(aes(x=year,y=avgmcpi, group = as.factor(group), color=as.factor(group)))+
  geom_line(linewidth=1.5)+ scale_y_continuous(breaks =seq(0.9,2,0.1))+
  theme_minimal() + theme(axis.title = element_blank(), legend.title = element_blank(), legend.position = "bottom",
                          plot.margin = margin(r=20, t=20, l=20)) 

#figure 2
dataforgraph2 <- read_xlsx("dataforgraph1.xlsx")
dataforgraph2 <- dataforgraph2 %>% filter(country=="Australia"|country=="Canada"|country=="Japan"|country=="France"|
                                            country=="United States"| country=="United Kingdom")

dataforgraph2 %>% ggplot(aes(x=year,y=accmcpi, group = as.factor(country), color=as.factor(country)))+
  geom_line(linewidth=1.5)+ 
  theme_minimal() + theme(axis.title = element_blank(), legend.title = element_blank(), legend.position = "bottom",
                          plot.margin = margin(r=20, t=20, l=20)) 

dataforgraph2 <- read_xlsx("dataforgraph1.xlsx")
dataforgraph2 <- dataforgraph2 %>% filter(country=="Australia"|country=="Canada"|country=="Japan"|country=="France"|
                                            country=="United States"| country=="United Kingdom")

dataforgraph2 %>% ggplot(aes(x=year,y=accmcpi, group = as.factor(country), color=as.factor(country)))+
  geom_line(linewidth=1.5)+ 
  theme_minimal() + theme(axis.title = element_blank(), legend.title = element_blank(), legend.position = "bottom",
                          plot.margin = margin(r=20, t=20, l=20)) 

#figure 3
dataforgraph3 <- read_xlsx("dataforgraph1.xlsx")
dataforgraph3 <- dataforgraph3 %>% filter(country=="Argentina"|country=="Brazil"|country=="China"|country=="India"|
                                            country=="Mexico"| country=="South Africa")

dataforgraph3 %>% ggplot(aes(x=year,y=accmcpi, group = as.factor(country), color=as.factor(country)))+
  geom_line(linewidth=1.5)+ 
  theme_minimal() + theme(axis.title = element_blank(), legend.title = element_blank(), legend.position = "bottom",
                          plot.margin = margin(r=20, t=20, l=20)) 

#table 1
datamcpi <- datamcpi %>% group_by(countryid) %>% mutate(accmcpi = accumulate(mcpi, ~.x*.y),
                                                        acctecch = accumulate(tecch_rec, ~.x*.y),
                                                        acceffch = accumulate(effch_rec, ~.x*.y))

datatable1 <- datamcpi %>% select(countryid, year, accmcpi, acctecch, acceffch)
datatable1 <- datatable1 %>% left_join(codes) %>% ungroup()
datatable1 <- datatable1 %>% filter(!is.na(accmcpi))
datatable1 <- datatable1 %>% group_by(country) %>% summarise(MCPI = last(accmcpi, order_by=year), 
                                               EFFCH = last(acceffch, order_by=year),
                                               TECCH = last(acctecch, order_by=year))
write.csv(datatable1, "datatable1.csv")

#table 3
clubs <- clubs %>% group_by(club) %>% summarise(members = paste(country,collapse=", "))
write.csv(clubs,"clubs.csv")

#table 6
summaryclubs <- mcpidf %>% group_by(club) %>% summarise(avgmcpi = mean(accmcpi, na.rm = T),std = sd(accmcpi, na.rm = T),
                                        minmcpi = min(accmcpi, na.rm = T), maxmcpi = max(accmcpi,na.rm=T),
                                        avggrowth = mean(growth,na.rm=T))
write.csv(summaryclubs,"summaryclubs.csv")  


#table
margins.oglmx(oprobit)
texreg(oprobit_m)


beta <- read.table("D:/estimations/long run - re const/sfa_mu_u_MAIN_mu_REGRESSION_c_LinearEffects_sample.raw",header = T, sep=",")
beta_m <- beta %>% summarise_all(mean)
beta_sd <- beta %>% summarise_all(sd)

beta_m <- as.data.frame(t(beta_m))
beta_sd <- as.data.frame(t(beta_sd))

beta <- cbind(beta_m,beta_sd)
colnames(beta) <- c("beta","sd")
beta$sig <- beta$beta/beta$sd


mu_predict <- read.table("D:/estimations/long run - re const/sfa_mu_u_MAIN_mu_REGRESSION_c_predict.res",header = T, sep=" ")
sigmau <- mu_predict$pmean_E_sigma_u
mean(log(sigmau^2))
sd(log(sigmau^2))
sigmav <- mu_predict$pmed_sigma_v
mean(log(sigmav^2))

sigmav <- mu_predict$pmean_param_sigma_v
sd(log(sigmav^2))
