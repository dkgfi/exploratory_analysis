COPY (select * from gfi.public.trade where src_country_id >= 0 and src_country_id <= 59) TO 'trade_src_country_id_0_59.csv' DELIMITER ',' CSV HEADER;
COPY (select * from gfi.public.trade where src_country_id >= 60 and src_country_id <= 76) TO 'trade_src_country_id_60_76.csv' DELIMITER ',' CSV HEADER;
COPY (select * from gfi.public.trade where src_country_id >= 77 and src_country_id <= 110) TO 'trade_src_country_id_77_110.csv' DELIMITER ',' CSV HEADER;
COPY (select * from gfi.public.trade where src_country_id >= 111 and src_country_id <= 158) TO 'trade_src_country_id_111_158.csv' DELIMITER ',' CSV HEADER;
COPY (select * from gfi.public.trade where src_country_id >= 159 and src_country_id <= 203) TO 'trade_src_country_id_159_203.csv' DELIMITER ',' CSV HEADER;
COPY (select * from gfi.public.trade where src_country_id >= 204 and src_country_id <= 238) TO 'trade_src_country_id_204_238.csv' DELIMITER ',' CSV HEADER;



