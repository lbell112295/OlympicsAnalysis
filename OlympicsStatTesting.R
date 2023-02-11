
# to show that the data is not normally distributed use shapiro wilks method
shapiro.test(medals_sig_test$`Medal count`)
#we obtained p value less than .05 indicating the distribution is significantly different 
#from normal distribution
#in this case we will use the wilcox test

#summer olympics
medals_sig_test_summer <- medals_sig_test[medals_sig_test$Season=='Summer',]

median(not_host_brazil$`Medal count`)
median(host_brazil$`Medal count`)

#brazil-2016
not_host_brazil <- medals_sig_test_summer[medals_sig_test_summer$host==0 & medals_sig_test_summer$Region=='Brazil',]
host_brazil <- medals_sig_test_summer[medals_sig_test_summer$host==1 & medals_sig_test_summer$Region=='Brazil',]

Header <- c("Parameter")
BRA_medal_count <- c("BRA medal count",0)
BRA_medal_count[2] <- wilcox.test(host_brazil$`Medal count`, not_host_brazil$`Medal count`, alternative = 'g', exact = FALSE)$p.value

Header <- c("Parameter")
BRA_gold_medal_count <- c("BRA gold medal count",0)
BRA_gold_medal_count[2] <- wilcox.test(host_brazil$`Gold Medal Count`, not_host_brazil$`Gold Medal Count`, alternative = 'g', exact = FALSE)$p.value

#UK-2012
not_host_uk <- medals_sig_test_summer[medals_sig_test_summer$host==0 & medals_sig_test_summer$Region=='UK',]
host_uk <- medals_sig_test_summer[medals_sig_test_summer$host==1 & medals_sig_test_summer$Region=='UK',]

Header <- c("Parameter")
UK_medal_count <- c("UK medal count",0)
UK_medal_count[2] <- wilcox.test(host_uk$`Medal count`, not_host_uk$`Medal count`, alternative = 'g', exact = FALSE)$p.value

Header <- c("Parameter")
UK_gold_medal_count <- c("UK gold medal count",0)
UK_gold_medal_count[2] <- wilcox.test(host_uk$`Gold Medal Count`, not_host_uk$`Gold Medal Count`, alternative = 'g', exact = FALSE)$p.value

#China-2008
not_host_chn <- medals_sig_test_summer[medals_sig_test_summer$host==0 & medals_sig_test_summer$Region=='China',]
host_chn <- medals_sig_test_summer[medals_sig_test_summer$host==1 & medals_sig_test_summer$Region=='China',]

Header <- c("Parameter")
CHN_medal_count <- c("CHN medal count",0)
CHN_medal_count[2] <- wilcox.test(host_chn$`Medal count`, not_host_chn$`Medal count`, alternative = 'g', exact = FALSE)$p.value

Header <- c("Parameter")
CHN_gold_medal_count <- c("CHN gold medal count",0)
CHN_gold_medal_count[2] <- wilcox.test(host_chn$`Gold Medal Count`, not_host_chn$`Gold Medal Count`, alternative = 'g', exact = FALSE)$p.value

#Greece-2004
not_host_gre <- medals_sig_test_summer[medals_sig_test_summer$host==0 & medals_sig_test_summer$Region=='Greece',]
host_gre <- medals_sig_test_summer[medals_sig_test_summer$host==1 & medals_sig_test_summer$Region=='Greece',]

Header <- c("Parameter")
GRC_medal_count <- c("GREECE medal count",0)
GRC_medal_count[2] <- wilcox.test(host_gre$`Medal count`, not_host_gre$`Medal count`, alternative = 'g', exact = FALSE)$p.value

Header <- c("Parameter")
GRC_gold_medal_count <- c("GREECE gold medal count",0)
GRC_gold_medal_count[2] <- wilcox.test(host_gre$`Gold Medal Count`, not_host_gre$`Gold Medal Count`, alternative = 'g', exact = FALSE)$p.value

#Australia-2000
not_host_aus <- medals_sig_test_summer[medals_sig_test_summer$host==0 & medals_sig_test_summer$Region=='Australia',]
host_aus <- medals_sig_test_summer[medals_sig_test_summer$host==1 & medals_sig_test_summer$Region=='Australia',]

Header <- c("Parameter")
AUS_medal_count <- c("AUS medal count",0)
AUS_medal_count[2] <- wilcox.test(host_aus$`Medal count`, not_host_aus$`Medal count`, alternative = 'g', exact = FALSE)$p.value

Header <- c("Parameter")
AUS_gold_medal_count <- c("AUS gold medal count",0)
AUS_gold_medal_count[2] <- wilcox.test(host_aus$`Gold Medal Count`, not_host_aus$`Gold Medal Count`, alternative = 'g', exact = FALSE)$p.value

#winter olympics

medals_sig_test_winter <- medals_sig_test[medals_sig_test$Season=='Winter',]


#Russia-2014
not_host_rus <- medals_sig_test_winter[medals_sig_test_winter$host==0 & medals_sig_test_winter$Region=='Russia',]
host_rus <- medals_sig_test_winter[medals_sig_test_winter$host==1 & medals_sig_test_winter$Region=='Russia',]

Header <- c("Parameter")
RUS_medal_count <- c("RUS medal count",0)
RUS_medal_count[2] <- wilcox.test(host_rus$`Medal count`, not_host_rus$`Medal count`, alternative = 'g', exact = FALSE)$p.value

Header <- c("Parameter")
RUS_gold_medal_count <- c("RUS gold medal count",0)
RUS_gold_medal_count[2] <- wilcox.test(host_rus$`Gold Medal Count`, not_host_rus$`Gold Medal Count`, alternative = 'g', exact = FALSE)$p.value

#Canada-2010
not_host_can <- medals_sig_test_winter[medals_sig_test_winter$host==0 & medals_sig_test_winter$Region=='Canada',]
host_can <- medals_sig_test_winter[medals_sig_test_winter$host==1 & medals_sig_test_winter$Region=='Canada',]

Header <- c("Parameter")
CAN_medal_count <- c("CAN medal count",0)
CAN_medal_count[2] <- wilcox.test(host_can$`Medal count`, not_host_can$`Medal count`, alternative = 'g', exact = FALSE)$p.value

Header <- c("Parameter")
CAN_gold_medal_count <- c("CAN gold medal count",0)
CAN_gold_medal_count[2] <- wilcox.test(host_can$`Gold Medal Count`, not_host_can$`Gold Medal Count`, alternative = 'g', exact = FALSE)$p.value

#Italy-2006
not_host_ita <- medals_sig_test_winter[medals_sig_test_winter$host==0 & medals_sig_test_winter$Region=='Italy',]
host_ita <- medals_sig_test_winter[medals_sig_test_winter$host==1 & medals_sig_test_winter$Region=='Italy',]

Header <- c("Parameter")
ITA_medal_count <- c("ITA medal count",0)
ITA_medal_count[2] <- wilcox.test(host_ita$`Medal count`, not_host_ita$`Medal count`, alternative = 'g', exact = FALSE)$p.value

Header <- c("Parameter")
ITA_gold_medal_count <- c("ITA gold medal count",0)
ITA_gold_medal_count[2] <- wilcox.test(host_ita$`Gold Medal Count`, not_host_ita$`Gold Medal Count`, alternative = 'g', exact = FALSE)$p.value

#USA-2002
not_host_usa <- medals_sig_test_winter[medals_sig_test_winter$host==0 & medals_sig_test_winter$Region=='USA',]
host_usa <- medals_sig_test_winter[medals_sig_test_winter$host==1 & medals_sig_test_winter$Region=='USA',]

Header <- c("Parameter")
USA_medal_count <- c("USA medal count",0)
USA_medal_count[2] <- wilcox.test(host_usa$`Medal count`, not_host_usa$`Medal count`, alternative = 'g', exact = FALSE)$p.value

Header <- c("Parameter")
USA_gold_medal_count <- c("USA gold medal count",0)
USA_gold_medal_count[2] <- wilcox.test(host_usa$`Gold Medal Count`, not_host_usa$`Gold Medal Count`, alternative = 'g', exact = FALSE)$p.value

#Japan-1998
not_host_jpn <- medals_sig_test_winter[medals_sig_test_winter$host==0 & medals_sig_test_winter$Region=='Japan',]
host_jpn <- medals_sig_test_winter[medals_sig_test_winter$host==1 & medals_sig_test_winter$Region=='Japan',]

Header <- c("Parameter")
JPN_medal_count <- c("JPN medal count",0)
JPN_medal_count[2] <- wilcox.test(host_jpn$`Medal count`, not_host_jpn$`Medal count`, alternative = 'g', exact = FALSE)$p.value

Header <- c("Parameter")
JPN_gold_medal_count <- c("JPN gold medal count",0)
JPN_gold_medal_count[2] <- wilcox.test(host_jpn$`Gold Medal Count`, not_host_jpn$`Gold Medal Count`, alternative = 'g', exact = FALSE)$p.value


result = rbind(Header,BRA_medal_count, BRA_gold_medal_count,
               UK_medal_count, UK_gold_medal_count,
               CHN_medal_count, CHN_gold_medal_count,
               GRC_medal_count, GRC_gold_medal_count,
               AUS_medal_count, AUS_gold_medal_count,
               RUS_medal_count, RUS_gold_medal_count,
               CAN_medal_count, CAN_gold_medal_count,
               ITA_medal_count, ITA_gold_medal_count,
               USA_medal_count, USA_gold_medal_count,
               JPN_medal_count, JPN_gold_medal_count)


write.csv(result, file = "T:/desktop/sigresults_olympics.csv")


