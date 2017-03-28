library('data.table')
library('bit64')
rm(list=ls()) # cleanup

###########################################
# Functions ###############################
###########################################

# Reads each chunk of the trade table and keep just the commodity_ids of interest.
# (Poor man's way of crunching 10GB of data on a laptop w <= 8GB of RAM)
read_trade <- function(csv_list, var_list, commodity_ids, ...) {
  dt <- list()
  for(csv in csv_list){
    cat(paste0('reading csv: ', csv, '\n'))
    tmpdt <- fread(csv, select=var_list, ...) # allowing pass-through parameters to fread
    tmpdt <- tmpdt[commodity_id %in% commodity_ids]
    dt[[length(dt)+1]] <- tmpdt
  }
  return(rbindlist(dt))
}

# Utility function to get the country abbreviations for each country_id
# @trade is the trade table; @country is the country table.
merge_country_codes <- function(trade, country){
  
  # check to see if we need to merge
  if(any(c('src_country_name3', 'country_name3') %in% names(trade))) stop("Stopping merge. Columns already in trade data table.")
  
  # preparing country table for merge
  country <- country[duplicated(id)==F] # each country is duplicated twice... not sure why.
  country[,`:=`(src_country_name3=iso3, country_name3=iso3)] # getting names nicey nice
  
  # merge 3 digit country codes to trade
  trade_m <- merge(trade, country[, .(id, src_country_name3)], all.x=T, all.y=F, by.x='src_country_id', by.y='id')
  trade_m <- merge(trade_m, country[, .(id, country_name3)], all.x=T, all.y=F, by.x='country_id', by.y='id')
  
  return(trade_m)
}


# Calculate standard deviation scaled by mean for each commodity.  
# Very naive in current state: considering all country pairs across all months available.
# Only considering nonzero value and nonzero quantity shipments priced in EUR.
agg_commodity_variance <- function(trade){
  
  # check if trade has any data
  if(dim(trade)[1]==0) warning("Trade has no data :(")
  
  # keeping just standardized data.  Figure out what to do w the rest later
  trade <- trade[unit_1_id==24 & is.na(value)==F & currency=='EUR' & quantity_1>0 & value>1 & is_import=='t']
  
  # adding unit price column
  trade[,unit1price:=value/quantity_1]
  
  # creating agg table at commodity level
  agg <- trade[, .(sd_unit1price=scale(unit1price)[2], 
                   sum_value=sum(value), 
                   hs_desc=unique(hs_description),
                   country_id_cnt=length(unique(country_id)),
                   src_country_id_cnt=length(unique(src_country_id)),
                   src_country_list=list(unique(src_country_name3)),
                   country_list=list(unique(country_name3))
  ), by=commodity_id]
  
  # sort and return variance aggregate table
  if(dim(agg)[1] > 0) agg <- agg[order(-abs(sd_unit1price))]
  return(agg)
}


###########################################
# Analysis ################################
###########################################

#############################
# Paramaterize ##############
#############################

setwd('~/Documents/gfi/public')

csv_list <- c('trade_src_country_id_0_59.csv', 
              'trade_src_country_id_77_110.csv',
              'trade_src_country_id_111_158.csv', 
              'trade_src_country_id_159_203.csv',
              'trade_src_country_id_204_238.csv')

commodity <- fread('commodity.csv')
country <- fread('country.csv')

var_list <- c('month', 'src_country_id', 'country_id', 'commodity_id', 'value', 'quantity_1', 'quantity_2',
              'unit_1_id', 'unit_2_id', 'is_import', 'currency')

#############################
# Run #######################
#############################

agg <- list()
for (commodity_id_seq in list(seq(0, 15000), seq(15001, 30000), seq(30001, 45000), seq(45001, 60000))) {
  
  # read transcripts from disk for commodity_id block
  cat(paste('Processing commodity_id block from:', min(commodity_id_seq), 'to', max(commodity_id_seq), '\n'))
  t <- read_trade(csv_list=csv_list, var_list=var_list, commodity_ids=commodity_id_seq, nrows=-1,
                  colClasses=c(quantity_1='integer64', value='integer64')) 
  
  # merge country names
  t <- merge_country_codes(t, country=country)
  t <- t[is.na(hs_description)==F]  # remove any commodities without a description
  
  # calculate variance stats at commodity level
  cat(paste('Calculating Variance... \n'))
  try({
    agg_tmp <- agg_commodity_variance(t)
    
    # get commodity hs_descriptions
    agg_tmp <- merge(agg_tmp, commodity[, .(hs_description, hs_code, id)], by.x='commodity_id', by.y='id')
    
    agg[[length(agg)+1]] = agg_tmp
  })
  
  rm(t) # hopefully save some memory
  gc() # cleanup
  
}

# combine results across the commodity_id blocks
aggdt <- rbindlist(agg)
aggdt <- aggdt[order(-sd_unit1price)]

# write results out to CSV
write.csv(lapply(data.frame(aggdt), as.character), '../results/scaled_stddev_by_commodity.csv', row.names=F)

