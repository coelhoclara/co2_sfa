library(brms)
library(rstan)
library(sfaR)
library(productivity)
library(sandwich)
library(lmtest)
library(dplyr)
library(posterior)

data <- readRDS("dataforMCPI_new.RDS")
sfa_translog_hcost_m <- sfacross(formula = -log_C ~ log_Y + ybari + log_K +ybari + log_L +lbari + t + log_L2 + log_Y2 + 
                                 log_K2 + t2 + log_L_log_Y + log_L_log_K + log_L_t + 
                                 log_Y_log_K + log_Y_t + log_K_t + cbari, muhet~factor(countryid), 
                               udist = 'hnormal', data = data, S = 1,
                               scaling = F, method = 'bfgs')

summary(sfa_translog_hcost_m)


sfa_translog_hcost <- sfacross(formula = -log_C ~ log_Y + log_K + log_L + t + log_L2 + log_Y2 + 
                                 log_K2 + t2 + log_L_log_Y + log_L_log_K + log_L_t + 
                                 log_Y_log_K + log_Y_t + log_K_t + factor(countryid)-1, muhet = ~1, 
                               udist = 'hnormal', data = data, S = 1,
                               scaling = F, method = 'bfgs')
lmtest::coeftest(sfa_translog_hcost, vcov. = sandwich::vcovCL, cluster = ~ countryid + t)

eff <- efficiencies(sfa_translog_hcost)

data <- data %>% bind_cols(eff$teBC)
colnames(data)[46] <- "eff"

data <- data %>% group_by(countryid) %>% mutate(leadeff = lead(eff, order_by = Time))
data$effch <- data$eff/data$leadeff
data <- data %>% group_by(countryid) %>% mutate(acceffch = accumulate(effch, ~.x*.y))

data$tecchp1 <- exp(0.01969 +0.00469 *data$log_K -0.00575*data$log_L+
                      +0.00476*data$log_Y+ 2*-0.00016*data$t)


data <- data %>% group_by(countryid) %>% mutate(tecch = (lead(tecchp1,order_by = Time)*tecchp1)^0.5)
data <- data %>% group_by(countryid) %>% mutate(acctecch = accumulate(tecch, ~.x*.y))

data$mcpi <- data$effch * data$tecch
data <- data %>% group_by(countryid) %>% mutate(accmcpi = accumulate(mcpi, ~.x*.y))

results_ml <- data %>% select(CountryName,CountryCode,Time,effch,acceffch,tecch,acctecch,mcpi,accmcpi)



#not the same
data <- readRDS("dataforMCPI_new.RDS")

sfa_translog_transf <- sfacross(formula = cminus ~ y + k + l + t + l2 + y2 + 
                                  k2 + t2 + ly + lk + lt + 
                                  yk + yt + kt + factor(countryid) -1 , muhet ~1,
                                udist = 'hnormal', data = data, S = 1,
                                scaling = F, method = 'bfgs')

summary(sfa_translog_transf)

eff <- efficiencies(sfa_translog_transf)

data <- data %>% bind_cols(eff$teBC)
colnames(data)[46] <- "eff"

data <- data %>% group_by(countryid) %>% mutate(leadeff = lead(eff, order_by = Time))
data$effch <- data$eff/data$leadeff
data <- data %>% group_by(countryid) %>% mutate(acceffch = accumulate(effch, ~.x*.y))

data$tecchp1 <- exp(0.06055 +0.02076*data$k +0.05356*data$l+
                      -0.00227*data$y+ 2*-0.00147*data$t)

data <- data %>% group_by(countryid) %>% mutate(tecch = (lead(tecchp1,order_by = Time)*tecchp1)^0.5)
data <- data %>% group_by(countryid) %>%arrange(Time) %>% mutate(acctecch = accumulate(tecch, ~.x*.y))

data$mcpi <- data$effch * data$tecch
data <- data %>% group_by(countryid) %>% mutate(accmcpi = accumulate(mcpi, ~.x*.y))

results <- data %>% select(CountryName,CountryCode,Time,effch,acceffch,tecch,acctecch,mcpi,accmcpi)



#until 2013

data <- readRDS("dataforMCPI_new.RDS")

data_short <- data %>% filter(Time <=2013)
data_short$countryid <- relevel(factor(data_short$countryid), ref="1") 
sfa_translog_hcost <- sfacross(formula = -log_C ~ log_Y + log_K + log_L + t + log_L2 + log_Y2 + 
                                 log_K2 + t2 + log_L_log_Y + log_L_log_K + log_L_t + 
                                 log_Y_log_K + log_Y_t + log_K_t + countryid, muhet = ~ 1, 
                               udist = 'hnormal', data = data_short, S = 1,
                               scaling = F, method = 'bfgs')
lmtest::coeftest(sfa_translog_hcost, vcov. = sandwich::vcovCL, cluster = ~ countryid )
summary(sfa_translog_hcost)
eff <- efficiencies(sfa_translog_hcost)
data <- data_short
data <- data %>% bind_cols(eff$teBC)
colnames(data)[47] <- "eff"

data <- data %>% group_by(countryid) %>% mutate(leadeff = lead(eff, order_by = Time))
data$effch <- data$eff/data$leadeff
data <- data %>% group_by(countryid) %>% mutate(acceffch = accumulate(effch, ~.x*.y))

data$tecchp1 <- exp(0.02639 +0.00621 *data$log_K -0.00440*data$log_L+
                      +0.00296*data$log_Y+ 2*-0.00042 *data$t)


data <- data %>% group_by(countryid) %>% mutate(tecch = (lead(tecchp1,order_by = Time)*tecchp1)^0.5)
data <- data %>% group_by(countryid) %>% mutate(acctecch = accumulate(tecch, ~.x*.y))

data$mcpi <- data$effch * data$tecch
data <- data %>% group_by(countryid) %>% mutate(accmcpi = accumulate(mcpi, ~.x*.y))

results_ml <- data %>% select(CountryName,CountryCode,Time,effch,acceffch,tecch,acctecch,mcpi,accmcpi)


##bayesX results

mu_predict <- read.table("D:/estimations/long run - log re const/sfa_mu_u_MAIN_mu_REGRESSION_log_C_minus_predict.res",header = T, sep=" ")
data <- readRDS("dataforMCPI_new.RDS")
data <- bind_cols(data,mu_predict$pmean_E_mu_u)
colnames(data)[47] <- "mu_u"
data$eff_rec <- exp(-data$mu_u)
data <- data %>% group_by(countryid) %>% mutate(leadeff_rec = lead(eff_rec, order_by = Time))
data$effch_rec <- data$eff_rec/data$leadeff_rec
data <- data %>% group_by(countryid) %>% mutate(acceffch_rec =accumulate(effch_rec, ~.x*.y) )
summary(data)

betas <- read.table("D:/estimations/long run - log re const/sfa_mu_u_MAIN_mu_REGRESSION_log_C_minus_LinearEffects_sample.raw",header = T, sep=" ")

data$tecchp1_rec <- exp(mean(betas$b_5)+mean(betas$b_15)*data$log_K+mean(betas$b_12)*data$log_L+
                          +mean(betas$b_14)*data$log_Y+2*mean(betas$b_9)*data$t)
data <- data %>% group_by(countryid) %>% mutate(tecch_rec = (lead(tecchp1_rec,order_by = Time)*tecchp1_rec)^0.5)
data <- data %>% group_by(countryid) %>% mutate(acctecch_rec = accumulate(tecch_rec, ~.x*.y))

data$mcpi <- data$effch_rec * data$tecch_rec
data <- data %>% group_by(countryid) %>% mutate(accmcpi = accumulate(mcpi, ~.x*.y))
summary(data)

results <- data %>% select(CountryName,CountryCode,Time,effch_rec,acceffch_rec,tecch_rec,acctecch_rec,mcpi,accmcpi)

saveRDS(data,"datawithmcpi_minus.RDS")

data_tofile <- data %>% select(countryid,year,l,y,k,c,t,eff_rec,effch_rec,acceffch_rec,
                               tecch_rec,acctecch_rec,mcpi,accmcpi)
data_tofile <- data_tofile %>% left_join(codes)
data_tofile <- data_tofile %>% select(-c(15))
colnames(data_tofile) <- c("countryid","year","l","y","k","c","t","eff","effch","acceffch",
                           "tecch","acctecch","mpci","accmcpi","country")
write.csv(data_tofile,"dataMCPI.csv")

data_tofile <- data %>% select(countryid,year,l,y,k,c,t,eff_rec,effch_rec,acceffch_rec,
                               tecch_rec,acctecch_rec,mcpi,accmcpi)
data_tofile <- data_tofile %>% left_join(codes)
data_tofile <- data_tofile %>% select(-c(15))
colnames(data_tofile) <- c("countryid","year","l","y","k","c","t","eff","effch","acceffch",
                           "tecch","acctecch","mpci","accmcpi","country")

data_tofile <- data %>% select(countryid,Time,log_L,log_Y,log_K,log_C,t,eff_rec,effch_rec,acceffch_rec,
                               tecch_rec,acctecch_rec,mcpi,accmcpi)
data_tofile <- data_tofile %>% left_join(codes)
data_tofile <- data_tofile %>% select(-c(15))
colnames(data_tofile) <- c("countryid","year","l","y","k","c","t","eff","effch","acceffch",
                           "tecch","acctecch","mpci","accmcpi","country")
write.csv(data_tofile,"data3012.csv")
