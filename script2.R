#load libraries
library(data.table)
library(ggplot2)

#Load datasets and converted to r file
commodity <- fread("commodity.csv")
commodity[,id:=as.character(id)]
country <- fread("country.csv")
country[,id:=as.character(id)]
country = country[,.(name=unique(name)),by=c("id")]

#Load large datasets and converted to r file (save storage and improve loading efficiency)
#look at only exported records, only EUR currency
#trade0_20m.csv
trade0_20m <- fread("trade0_20m.csv",colClasses="character",select=c(1:6,8:11)) #only 28 source countries, 236 countries
trade0_20m[,value:=as.numeric(value)];trade0_20m[,quantity_1:=as.numeric(quantity_1)];trade0_20m[,month:=as.Date(month,"%Y-%m-%d")]
trade0_20m[,unit_value:=value/quantity_1] #there are records with zero quantity but non zero value, notes as 'Inf' in unit_value column
save(trade0_20m,file="trade0_20m_abbr.rda")
trade0_20m[is.na(unit_value),unit_value:=0]
trade0_20m_exp = trade0_20m[is_import=='f',]
trade0_20m_exp = trade0_20m_exp[currency=="EUR",c(2,3,5,10,11),with=F]
trade0_20m_exp[,commodity_scrCountry:=paste(commodity_id,src_country_id,sep="_")]
save(trade0_20m_exp,file="trade0_20m_abbr_exp2.rda")

#trade20_40m.csv
trade20_40m <- fread("trade20m_40m.csv",colClasses="character",select=c(1:6,8:11)) 
trade20_40m[,value:=as.numeric(value)];trade20_40m[,quantity_1:=as.numeric(quantity_1)];trade20_40m[,month:=as.Date(month,"%Y-%m-%d")]
trade20_40m[,unit_value:=value/quantity_1] #there are records with zero quantity but non zero value, notes as 'Inf' in unit_value column
trade20_40m[is.na(unit_value),unit_value:=0]
trade20_40m_exp = trade20_40m[is_import=='f',]
trade20_40m_exp = trade20_40m_exp[currency=="EUR",c(2,3,5,10,11),with=F]
trade20_40m_exp[,commodity_scrCountry:=paste(commodity_id,src_country_id,sep="_")]
save(trade20_40m_exp,file="trade20_40m_abbr_exp2.rda")

#trade40_60m.csv
trade40_60m <- fread("trade40m_60m.csv",colClasses="character",select=c(1:6,8:11)) 
trade40_60m[,value:=as.numeric(value)];trade40_60m[,quantity_1:=as.numeric(quantity_1)];trade40_60m[,month:=as.Date(month,"%Y-%m-%d")]
trade40_60m[,unit_value:=value/quantity_1] #there are records with zero quantity but non zero value, notes as 'Inf' in unit_value column
trade40_60m[is.na(unit_value),unit_value:=0]
trade40_60m_exp = trade40_60m[is_import=='f',]
trade40_60m_exp = trade40_60m_exp[currency=="EUR",c(2,3,5,10,11),with=F]
trade40_60m_exp[,commodity_scrCountry:=paste(commodity_id,src_country_id,sep="_")]
save(trade40_60m_exp,file="trade40_60m_abbr_exp2.rda")

#trade60_80m.csv
trade60_80m <- fread("trade60m_80m.csv",colClasses="character",select=c(1:6,8:11)) 
trade60_80m[,value:=as.numeric(value)];trade60_80m[,quantity_1:=as.numeric(quantity_1)];trade60_80m[,month:=as.Date(month,"%Y-%m-%d")]
trade60_80m[,unit_value:=value/quantity_1] #there are records with zero quantity but non zero value, notes as 'Inf' in unit_value column
trade60_80m[is.na(unit_value),unit_value:=0]
trade60_80m_exp = trade60_80m[is_import=='f',]
trade60_80m_exp = trade60_80m_exp[currency=="EUR",c(2,3,5,10,11),with=F]
trade60_80m_exp[,commodity_scrCountry:=paste(commodity_id,src_country_id,sep="_")]
save(trade60_80m_exp,file="trade60_80m_abbr_exp2.rda")

#trade80_105m.csv
trade80_105m <- fread("trade80m_105m.csv",colClasses="character",select=c(1:6,8:11)) 
trade80_105m[,value:=as.numeric(value)];trade80_105m[,quantity_1:=as.numeric(quantity_1)];trade80_105m[,month:=as.Date(month,"%Y-%m-%d")]
trade80_105m[,unit_value:=value/quantity_1] #there are records with zero quantity but non zero value, notes as 'Inf' in unit_value column
trade80_105m[is.na(unit_value),unit_value:=0]
trade80_105m_exp = trade80_105m[is_import=='f',]
trade80_105m_exp = trade80_105m_exp[currency=="EUR",c(2,3,5,10,11),with=F]
trade80_105m_exp[,commodity_scrCountry:=paste(commodity_id,src_country_id,sep="_")]
save(trade80_105m_exp,file="trade80_105m_abbr_exp2.rda")

#load R dataset from trade data
load("trade0_20m_abbr_exp2.rda") #trade0_20m
load("trade20_40m_abbr_exp2.rda") 
load("trade40_60m_abbr_exp2.rda") 
load("trade60_80m_abbr_exp2.rda") 
load("trade80_105m_abbr_exp2.rda") 

#combine into one single trade datatable
alltrade = rbind(trade0_20m_exp,trade20_40m_exp,trade40_60m_exp,trade60_80m_exp,trade80_105m_exp)
save(alltrade,file="trade0_105m_abbr_exp2.rda")

#start with alltrade data. Load it
load("trade0_105m_abbr_exp2.rda")

#data exploratory
#1. could a same commodity has diff representation of unit?
dexp1 = trade0_20m[,.(uqi_unit=length(unique(unit_1_id))),by=c("commodity_id")]
summary(dexp1) #yes, each unique commodity (by commodity_id) has only 1 representation of unit

dexp2 = alltrade[,.(uqi_unit=length(unique(unit_1_id))),by=c("commodity_id")]
summary(dexp2) #yes, each unique commodity (by commodity_id) has only 1 representation of unit

##standardizing 
y = trade0_20m[,c(2:5,8,9,10,11),with=F]
y_stat_noInf = y[!is.infinite(unit_value),.(sd_mean_div =sd(unit_value)/mean(unit_value)),by=c("commodity_scrCountry","month","is_import")]
setorder(y_stat_noInf,sd_mean_div)

#filter out rows where unit_value column has Inf value due to non zero values but zero quantity
my_stat = alltrade[!is.infinite(unit_value),.(sd_mean_div =sd(unit_value)/mean(unit_value)),by=c("commodity_scrCountry","month")]
setorder(my_stat,sd_mean_div) #order from the smallest to largest sd_mean_div

#select the top 20 commodities that shows high variance in unit price exported to other countries 
sel_id = tail(unique(my_stat$commodity_scrCountry),20)
gx = my_stat[commodity_scrCountry %in% sel_id,]
ggplot(gx)+geom_line(aes(x=month,y=sd_mean_div,color=as.factor(commodity_scrCountry)))+scale_y_log10()+scale_x_date(date_labels = "%Y-%m-%d")+ylab("Standard Dev for price per unit (EUR)")+
  ggtitle("Top 20 commodities exported from countries that have relatively high variance (normalized) in unit price")

#select the top 500 commodities that shows high variance in unit price exported to other countries 
sel_id = tail(unique(my_stat$commodity_scrCountry),500)
gx2 = my_stat[commodity_scrCountry %in% sel_id,] 
setorder(gx2,-sd_mean_div) #this gx2 data table tells you the commodity exported from a country at whichever month shows the largest variance in unit price charged among exported countries
write.csv(gx2,file="top500commodities_highVar_for_month.csv")

output = alltrade[commodity_scrCountry %in% sel_id,]
output = output[!is.infinite(unit_value),]
setorder(output,commodity_id,src_country_id,month)
output = merge(output,country,by.x="src_country_id",by.y="id",all.x=T)
output = merge(output,commodity[,c(1,3),with=F],by.x="commodity_id",by.y="id",all.x=T)
write.csv(output,file="top500commodities_highVar_trans.csv")
#output data table gives you the original records (absolute unit prices charged to each exported country).
















