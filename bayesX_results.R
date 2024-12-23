#read results from BayesX
#get inefficiency to calculate effch
#use coefficientes to calculate tecch
#calculate mcpi
#calculate accumulated growth of effch, tecch and mcpi


data <- readRDS("dataforMCPI_bayesX.RDS")


mu_predict <- read.table("D:/estimations/long run - re const/sfa_mu_u_MAIN_mu_REGRESSION_c_predict.res",header = T, sep=" ")
data <- readRDS("dataforMCPI_bayesX.RDS")
data <- bind_cols(data,mu_predict$pmean_E_mu_u)
colnames(data)[49] <- "mu_u"
data$eff_rec <- exp(-data$mu_u)
data <- data %>% group_by(countryid) %>% mutate(leadeff_rec = lead(eff_rec, order_by = year))
data$effch_rec <- data$eff_rec/data$leadeff_rec
data <- data %>% group_by(countryid) %>% mutate(acceffch_rec =accumulate(effch_rec, ~.x*.y) )

data$tecchp1_rec <- exp(-0.0320388-0.00670413*data$k-0.00697821*data$l+
                         -0.0135447*data$y+2*0.000820433*data$t)
data <- data %>% group_by(countryid) %>% mutate(tecch_rec = (lead(tecchp1_rec,order_by = year)*tecchp1_rec)^0.5)
data <- data %>% group_by(countryid) %>% mutate(acctecch_rec = accumulate(tecch_rec, ~.x*.y))

data$mcpi <- data$effch_rec * data$tecch_rec
data <- data %>% group_by(countryid) %>% mutate(accmcpi = accumulate(mcpi, ~.x*.y))

saveRDS(data,"datawithmcpi.RDS")

data_tofile <- data %>% select(countryid,year,l,y,k,c,t,eff_rec,effch_rec,acceffch_rec,
                               tecch_rec,acctecch_rec,mcpi,accmcpi)
data_tofile <- data_tofile %>% left_join(codes)
data_tofile <- data_tofile %>% select(-c(15))
colnames(data_tofile) <- c("countryid","year","l","y","k","c","t","eff","effch","acceffch",
                           "tecch","acctecch","mpci","accmcpi","country")
write.csv(data_tofile,"dataMCPI.csv")
