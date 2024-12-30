library(pwt10)
library(dplyr)
library(readxl)
library(fastDummies)


data("pwt10.0")
co2 <- read_xlsx("co2emissions_new.xlsx")

co2 <- co2 %>% group_by(CountryCode) %>% filter(!is.na(co2emissions))
data <- pwt10.0 %>% filter(year>=1975)
data <- data %>% group_by(isocode) %>% filter(!is.na(rgdpna)&!is.na(emp)&!is.na(cn))

data <- co2 %>% left_join(data,by=(c("CountryCode"="isocode","Time"="year")))
data <- data %>% select(CountryName,CountryCode,Time,co2emissions,rgdpna,emp,cn)                                

data <- data %>% group_by(CountryCode) %>% filter(!is.na(rgdpna)&!is.na(emp)&!is.na(cn))
data <- data %>% group_by(CountryCode) %>% filter(!first(Time)>1975|!last(Time)<2019)

detectproblem <- data %>% group_by(CountryCode) %>% summarise(c=n())
detectproblem <- detectproblem %>% filter(c==45)

data <- data %>% filter(CountryCode %in% detectproblem$CountryCode )

data <- subset(data, !(CountryCode %in% c("KWT","LBY","IRQ","AGO","AZE","TCD","BRN",
                                          "KAZ","IRN","ARE","ECU","DZA","NGA","RUS","QAT")))

detectproblem <- data %>% group_by(CountryName, CountryCode) %>% summarise(c=n())

data <- subset(data, !(CountryCode %in% c("BWA","COD","COG","GAB","DEU","IDN","KEN","LBN","MUS","MGZ",
                                          "NAM","RWA","SDN","UGA","ZMB","ZWE")))

detectproblem <- data %>% group_by(CountryName, CountryCode) %>% summarise(c=n())

data <- subset(data, !(CountryCode %in% c("YEM","UZB","TKM","TGO","TJK","VCT","SVN","SVK","SLE","SYC","MOZ")))

data$l_rscale <-  data$emp/mean(data$emp)
data$y_rscale <- data$rgdpna/mean(data$rgdpna)
data$k_rscale <- data$cn/mean(data$cn)
data$c_rscale <- data$co2emissions/mean(data$co2emissions)

data <- data %>% group_by(CountryCode) %>% mutate(lbari = mean(l_rscale),
                                                  cbari = mean(c_rscale),
                                                  ybari = mean(y_rscale),
                                                  kbari = mean(k_rscale))

data$l <- log(data$l_rscale)-log(data$lbari)
data$y <- log(data$y_rscale)-log(data$ybari)
data$k <- log(data$k_rscale) - log(data$kbari)
data$c <- log(data$c_rscale) - log(data$cbari)
data$t <- data$Time - 1974
data$l2 <- 0.5*(data$l)^2
data$y2 <- 0.5*(data$y)^2
data$k2 <- 0.5*(data$k)^2
data$t2 <- 0.5*data$t^2
data$ly <- data$l*data$y
data$lk <- data$l*data$k
data$lt<- data$l*data$t
data$yk <- data$y*data$k
data$yt <- data$y*data$t
data$kt <- data$k*data$t

data$cminus <- data$c*-1


country <- data %>% select(CountryCode)
country <- unique(country)
country <- rowid_to_column(country, "countryid")

data <- data %>% left_join(country)

                   

data$log_C <- log(data$c_rscale)
data$log_Y <- log(data$y_rscale)
data$log_K <- log(data$k_rscale)
data$log_L <- log(data$l_rscale)
data$log_L2 <- 0.5*data$log_L^2
data$log_Y2 <- 0.5*data$log_Y^2
data$log_K2 <- 0.5*data$log_K^2
data$log_L_log_Y <- data$log_L*data$log_Y
data$log_L_log_K <- data$log_L*data$log_K
data$log_L_t <- data$log_L*data$t
data$log_Y_log_K <- data$log_Y*data$log_K
data$log_Y_t <- data$log_Y*data$t
data$log_K_t <- data$log_K*data$t

data$log_C_minus <- data$log_C*-1

saveRDS(data,"dataforMCPI_new.RDS")


data <- data %>% relocate(countryid)

codes <- data %>% select("countryid","CountryName","CountryCode") %>% distinct()
saveRDS(codes,"codes.RDS")

data <- data %>% ungroup %>% select(-c("CountryName","CountryCode"))

data <- dummy_cols(data,select_columns = "countryid")

write.table(data,"dataforMCPI_new.raw", sep=" ", quote = FALSE, row.names = FALSE)   

paste(names(data)[45:132], collapse = " + ")
