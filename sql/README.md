  ### Summary

 The original trade was delivered to us in a Postgres db dump.  These are the SQL queries we've used to produce various views or CSVs.  `chunk_trade.sql`, `public_tables_to_csv.sql` are the primary SQL queries used to generate the CSV files distributed at the March 25 2017 Data Dive for analysis.  These tables are dumped from the public schema of the GFI trade database.

### Query Details

* `public_tables_to_csv.sql`: Creates one CSV per table in the public schema.
* `chunk_trade.sql`: Breaks up the trade table (~9GB) into smaller chunks (1-2GB each) by `src_country_id` (the country reporting the data).  The numbers in the csv names indicate the range of `src_country_id`s included in the file.  The country names corresponding to these ids can be found in the `country` table.
* `country_commodify_samples_catie.sql`: Produces custom tables used by Catie Schwartz for specific analyses of countries and commodities desired by GFI.



