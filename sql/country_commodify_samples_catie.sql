
COPY (select * from public. where country_id in (71, 84, 116) and commodity_id=28671 and is_import='t') TO 'coffee_exports_71_84_116.csv' DELIMITER ',' CSV HEADER;
COPY (select * from public. where country_id=163 and commodity_id in (select id from public.commodity where hs_description like '%PETROLEUM%') and is_import='t') TO 'petroleum_cntry186.csv' DELIMITER ',' CSV HEADER;
COPY (select * from public. where country_id=207 and commodity_id in (select id from public.commodity where hs_description like '%PLATINUM%') and is_import='t') TO 'platinum_cntry207.csv' DELIMITER ',' CSV HEADER;
