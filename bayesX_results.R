#read results from BayesX
#get inefficiency to calculate effch
#use coefficientes to calculate tecch
#calculate mcpi
#calculate accumulated growth of effch, tecch and mcpi

mu_predict <- read.table("D:/estimations/short run/sfa_mu_u_MAIN_mu_REGRESSION_c_predict.res",header = T, sep=" ")

exp(-mu_predict$pmean_E_sigma_u)

mu_predict2 <- read.table("D:/estimations/test run/sfa_mu_u_MAIN_mu_REGRESSION_c_predict.res",header = T, sep=" ")

mu_predict2$ineff <- exp(-(mu_predict$y - mu_predict$pmean_param_mu_y + (2/pi)^0.5*mu_predict$pmean_param_sigma_u))
mu_predict2$eff <- 1 - mu_predict2$ineff

data <- readRDS("dataforMCPI_bayesX.RDS")

data <- bind_cols(data,mu_predict2$eff)
colnames(data)[49] <- "eff"

data <- data %>% group_by(countryid) %>% mutate(leadeff = lead(eff, order_by = year))
data$effch_new <- data$eff/data$leadeff

data$tecchp1_n <- exp(0.00535+0.00204*data$k-0.0158895*data$l+
                                    +0.007075*data$y-2*0.00048*data$t)
data <- data %>% group_by(countryid) %>% mutate(tecch_n = (lead(tecchp1_n,order_by = year)*tecchp1_n)^0.5)

data <- data %>% group_by(countryid) %>% mutate(changetecch_n = tecch_n/lag(tecch_n,order_by = year))
data <- data %>% group_by(countryid) %>% mutate(acctecch = prod(changetecch_n, na.rm = T))


mu_predict <- read.table("D:/estimations/long run - const const/sfa_mu_u_MAIN_mu_REGRESSION_c_predict.res",header = T, sep=" ")
data <- bind_cols(data,mu_predict$pmean_E_mu_u)
colnames(data)[55] <- "eff_cc"

data <- data %>% group_by(countryid) %>% mutate(leadeff_cc = lead(eff_cc, order_by = year))
data$effch_cc <- data$eff_cc/data$leadeff_cc

data$tecchp1_cc <- exp(-0.0238359-0.00463729*data$k-0.0188863*data$l+
                        -0.00251016*data$y+2*0.00599712*data$t)
data <- data %>% group_by(countryid) %>% mutate(tecch_cc = (lead(tecchp1_cc,order_by = year)*tecchp1_cc)^0.5)

data <- data %>% group_by(countryid) %>% mutate(changetecch_cc = tecch_cc/lag(tecch_cc,order_by = year))
data <- data %>% group_by(countryid) %>% mutate(acctecch_cc = prod(changetecch_cc, na.rm = T))


mu_predict <- read.table("D:/estimations/long run - re const/sfa_mu_u_MAIN_mu_REGRESSION_c_predict.res",header = T, sep=" ")
data <- bind_cols(data,mu_predict$pmean_E_mu_u)
colnames(data)[62] <- "mu_u"
data$eff_rec <- exp(-data$mu_u)
data <- data %>% group_by(countryid) %>% mutate(leadeff_rec = lead(eff_rec, order_by = year))
data$effch_rec <- data$eff_rec/data$leadeff_rec
data <- data %>% group_by(countryid) %>% mutate(acceffch = prod(effch_rec, na.rm = T))


data$tecchp1_rec <- exp(-0.0320388-0.00670413*data$k-0.00697821*data$l+
                         -0.0135447*data$y+2*0.000820433*data$t)
data <- data %>% group_by(countryid) %>% mutate(tecch_rec = (lead(tecchp1_rec,order_by = year)*tecchp1_rec)^0.5)
data <- data %>% group_by(countryid) %>% mutate(acctecch_rec = prod(tecch_rec, na.rm = T))

data$mcpi <- data$effch_rec * data$tecch_rec
data <- data %>% group_by(countryid) %>% mutate(accmcpi = prod(mcpi, na.rm = T))

saveRDS(data,"datawithmcpi.RDS")
