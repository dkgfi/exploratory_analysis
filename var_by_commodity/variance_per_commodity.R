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
agg_commodity_variance <- function(trade, is_import_t_or_f='t'){
  
  # check if trade has any data
  if(dim(trade)[1]==0) warning("Trade has no data :(")
  
  # keeping just standardized data.  Figure out what to do w the rest later
  trade <- trade[unit_1_id==24 & is.na(value)==F & currency=='EUR' & quantity_1>0 & value>1 & is_import==is_import_t_or_f]
  
  # adding unit price column
  trade[,unit1price:=value/quantity_1]
  
  # creating agg table at commodity level
  agg <- trade[, .(sd_unit1price = sd(unit1price)/mean(unit1price), 
                   sum_value = sum(value), 
                   country_id_cnt = length(unique(country_id)),
                   src_country_id_cnt = length(unique(src_country_id)),
                   anomalous_src_country = {
                     commodity_id_mean <- mean(unit1price)
                     list(setdiff(.SD[, mean(unit1price)-commodity_id_mean, by=.(src_country_name3)][order(abs(V1), decreasing=T)][['src_country_name3']][1:3], NA))
                     },
                   anomalous_country = {
                     commodity_id_mean <- mean(unit1price)
                     list(setdiff(.SD[, mean(unit1price)-commodity_id_mean, by=.(country_name3)][order(abs(V1), decreasing=T)][['country_name3']][1:3], NA))
                     },
                   src_country_list_sortby_value = list(.SD[, sum(value), by=src_country_name3][order(V1, decreasing=T)][['src_country_name3']]),
                   country_list_sortby_value = list(.SD[, sum(value), by=country_name3][order(V1, decreasing=T)][['country_name3']])
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

commodity_id_chunks <- list(seq(0, 15000), seq(15001, 30000), seq(30001, 45000), seq(45001, 60000))


#############################
# Run: Chunks ###############
#############################

agg_im <- list()
agg_ex <- list()
t_save <- data.table() # save rows from trade table for most mispriced commodities

for (commodity_id_seq in commodity_id_chunks) {
  
  # read transcripts from disk for commodity_id block
  cat(paste('Processing commodity_id block from:', min(commodity_id_seq), 'to', max(commodity_id_seq), '\n'))
  t <- read_trade(csv_list=csv_list, var_list=var_list, commodity_ids=commodity_id_seq, nrows=-1,
                  colClasses=c(quantity_1='integer64', value='integer64')) 
  
  cat(paste(length(unique(t$commodity_id)), 'unique commodity_ids read for commodity block from', min(commodity_id_seq), 'to', max(commodity_id_seq), '\n'))
  if(dim(t)[1]==0) {cat('No relevant records found. Skipping to next commodity_id block \n'); next}
  
  # merge country names
  t <- merge_country_codes(t, country=country)
  
  # Calculating and saving variance for imports and exports.
  for(t_f in c('t', 'f')){
    cat(paste('Calculating Variance for', switch(t_f, 'f'='Exports', 't'='Imports'), '...\n'))
    if(dim(t[is_import==t_f])[1]==0) next
    
    # calculate variance for each commodity, if possible.  
    agg_tmp <- try(agg_commodity_variance(t, is_import=t_f), silent=T)
    if(class(agg_tmp) == 'try-error') next
    
    # get commodity names and hs codes for readability
    agg_tmp <- merge(agg_tmp, commodity[, .(hs_description, hs_code, id)], by.x='commodity_id', by.y='id')
    
    # remove any commodities without a description
    agg_tmp <- agg_tmp[is.na(hs_description)==F] 
    
    # save the raw records for the top 20 commodities in each chunk
    t_save <- rbind(t_save, t[commodity_id %in% agg_tmp$commodity_id[1:min(20, length(unique(agg_tmp$commodity_id)))]])
    
    # add each results from each chunk to the agg object
    if(t_f == 't'){ 
        agg_im[[length(agg_im)+1]] <- agg_tmp  
    } else if (t_f == 'f') { 
        agg_ex[[length(agg_ex)+1]] <- agg_tmp
    }
    
  }
  
  rm(t) # hopefully save some memory
  gc() # cleanup
  
}


#############################
# Run: Aggregates ###########
#############################

# combine results across the commodity_id blocks
agg_im_dt <- rbindlist(agg_im)
agg_ex_dt <- rbindlist(agg_ex)
agg_im_dt <- agg_im_dt[order(-sd_unit1price)] 
agg_ex_dt <- agg_ex_dt[order(-sd_unit1price)]

# ordering columns for output
colname_order <- { tmp <- c('commodity_id', 'sum_value', 'sd_unit1price', 'country_id_cnt', 'src_country_id_cnt', 'hs_code', 'hs_description')
  c(tmp, setdiff(names(agg_im_dt), tmp))
}
agg_im_dt <- agg_im_dt[,colname_order, with=F]
agg_ex_dt <- agg_ex_dt[,colname_order, with=F]

# saving and prettying-up the raw data from trade table for the most mispriced commodities
t_save <- merge(t_save, commodity[, .(hs_description, hs_code, id)], by.x='commodity_id', by.y='id')
t_save <- t_save[unit_1_id==24 & is.na(value)==F & currency=='EUR' & quantity_1>0 & value>1]
t_save[,unit1price:=value/quantity_1] # adding unit price column

# write results out to CSV
write.csv(lapply(data.frame(agg_im_dt), as.character), '../results/scaled_stddev_by_commodity_imports.csv', row.names=F)
write.csv(lapply(data.frame(agg_ex_dt), as.character), '../results/scaled_stddev_by_commodity_exports.csv', row.names=F)
write.csv(lapply(data.frame(t_save), as.character), '../results/scaled_stddev_by_commodity_raw_data_for_top_commodities_from_trade_table.csv', row.names=F)

