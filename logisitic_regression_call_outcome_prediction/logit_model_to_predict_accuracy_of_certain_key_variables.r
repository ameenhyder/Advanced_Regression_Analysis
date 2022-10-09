library(DBI)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RMySQL)
library(data.table)
library(stringr)
library(modelr)
library(scales)
library(knitr)
library(kableExtra)
library(lubridate)
library(RColorBrewer)
library(plotly)
library(shiny)
library(pROC)
library 
install.packages("pROC")


.libPaths()
dbDisconnect()
mydb_archival<- dbConnect(MySQL(), user='mundiale_ai_user', password='#mundiale_ai_user#', dbname='smexplorerdata', host='10.4.2.82',port=3307)
sme <-as.data.frame( dbGetQuery(mydb_archival, "")) ##select query will be added inside the qoutes 


head(sme);
summary(sme);

sme[sme$issale == 'Sale',]$issale = 1;
sme[sme$issale == 'NotSale',]$issale = 0;
sme[sme$m_filler_ll == ",",]$m_filler_ll = 'FC';
sme[sme$m_filler_ll == "",]$m_filler_ll = 'FB';
sme$nConnectsRollUp_OTH3 = as.integer(sme$nConnectsRollUp_OTH3)
sme$sex_b = as.character(sme$sex_b);
sme[sme$on_off == 0,]$on_off = 'off';
sme[sme$on_off == 1,]$on_off = 'on';

sme[sme$sex_b == 0,]$sex_b = 'G';
sme[sme$sex_b == 'notspecified',]$sex_b = 'NS';

sme$c_qt_durc_trfd_3_crm = as.integer(sme$c_qt_durc_trfd_3_crm);
sme$c_tmp_base_3_crm = as.integer(sme$c_tmp_base_3_crm);
sme$issale = as.integer(sme$issale);
str(sme);


log4 <- glm(issale ~ AHT + m_filler_ll + tech_bin + nconnectsrollup_bin + c_qt_durc_trfd_3_crm + on_off, data = sme, family = "binomial")
summary(log4)


##sme2 <- sme %>% filter(on_off == 'off') %>% select(AHT, c_tmp_base_3_crm, m_filler_ll, tech_bin, nconnectsrollup_bin);

predict(log4, sme, type = 'response')


sme$pred4 = predict(log4, sme, type = 'response')
head(sme)

sme[sme$pred4 >= 0.5,]$pred4 = 1;
sme[sme$pred4 < 0.5,]$pred4 = 0;


head(sme)
sme %>% select(issale, pred) %>% filter(issale == 1)

g <- roc(issale ~ pred, data = sme %>% select(issale, AHT, c_tmp_base_3_crm, m_filler_ll, tech_bin, nconnectsrollup_bin, pred)
plot(g)
plot(roc(sme$issale ~ sme$pred))


g2 <- roc(issale ~ pred2, data = sme);
plot(g2);
auc(roc(sme$issale ~ sme$pred2));


g3 <- roc(issale ~ pred3, data = sme);
plot(g3);
auc(roc(sme$issale ~ sme$pred3));

g4 <- roc(issale ~ pred4, data = sme);
plot(g4);
auc(roc(sme$issale ~ sme$pred4));

sme_test <-as.data.frame( dbGetQuery(mydb_archival, "
                                 
                                 select calltime, date(Calltime) as date, on_off, skill, issale, AHT, c_qt_durc_trfd_3_crm
                                 ,c_tmp_base_3_crm, m_filler_ll, case when C_QT_DURC_TRFD_3_CRM in (0,1) then 1 else 2 end qt_cust
                                 , case when C_TECNOLOGIA_RESUMO_CRM in ('2G','3G') then 1 else 2 end tech_bin
                                 , case when C_sexo_crm= '-3' then 1 else 0 end sex_b
                                 , case when cast(nconnectsrollup_oth3 as signed) <= 20 then cast(nconnectsrollup_oth3 as signed) when cast(nconnectsrollup_oth3 as signed) > 50 then 51 else ceiling(( cast(nconnectsrollup_oth3 as signed)/5)) * 5 end nconnectsrollup_bin
                                 , nConnectsRollUp_OTH3
                                 from `mundiale_tfn.smetable_new2` 
                                 where crm_lookup = 1 and leadlist_lookup = 1 and c_offer_type_CRM = 'PRE_CTRL' 
                                 and skill is not null  
                                 and skill <> 'NA'
                                 and tipo_de_ligacao = 'Preditivo'
                                 and nconnects_oth2 <> 'NA'
                                 and date(Calltime) >= '2020-04-07' and date(calltime) <= '2020-04-08';"))


str(sme_test)


sme_test[sme_test$m_filler_ll == ",",]$m_filler_ll = 'FC';
sme_test[sme_test$m_filler_ll == "",]$m_filler_ll = 'FB';
sme_test$nConnectsRollUp_OTH3 = as.integer(sme_test$nConnectsRollUp_OTH3)
sme_test$sex_b = as.character(sme_test$sex_b);
sme_test$on_off = as.character(sme_test$on_off);

sme_test[sme_test$on_off == 0,]$on_off = "off";
sme_test[sme_test$on_off == 1,]$on_off = 'on';

sme_test[sme_test$sex_b == 0,]$sex_b = 'G';
sme_test[sme_test$sex_b == 1,]$sex_b = 'NS';

sme_test$c_qt_durc_trfd_3_crm = as.integer(sme_test$c_qt_durc_trfd_3_crm);
sme_test$c_tmp_base_3_crm = as.integer(sme_test$c_tmp_base_3_crm);
sme_test$issale = as.integer(sme_test$issale);
str(sme_test);


pred_test = predict(log4, sme_test, type = 'response');
pred_test;

sme_test$pred_test = pred_test ;

sme_test[sme_test$pred_test >= 0.5,]$pred_test = 1;
sme_test[sme_test$pred_test < 0.5,]$pred_test = 0;

head(sme_test);


sales <- sme_test %>%  group_by(on_off) %>% summarise(S = sum(pred_test))
call_volume <- sme_test %>%  group_by(on_off) %>% tally()
call_volume;


colnames(sales) = c('on_off', 'isssale') ;
colnames(call_volume) = c('on_off', 'Total_Calls') ;


sales ;
call_volume ;

left_join(sales,call_volume, by = c('on_off' = 'on_off') ;

tot <- cbind(sales, call_volume['Total_Calls'])   ;       
tot;

tot <- tot %>% mutate(((isssale/Total_Calls)*100));
