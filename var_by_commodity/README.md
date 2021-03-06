### Purpose

Identify commodities with most price variance that could be indicative of trade mis-invoicing.

### Method

This script calculates the [coefficient of variation](standard deviation / mean) for unit price across commodities.  For each commodity, for each month, the standard deviation and mean of unit price (`value/quantity_1`) is calculated across all country pairs.

This simple measure controls for the magnitude of the unit price which will be quite large for expensive goods (gold or laptops) but low for cheap goods (cement or bricks).  However, it does not capture any of the nuance in variations across time or any other dimension.  There are certainly more sophisticated methods to measure this (some within this repo), but this method does flag some low hanging fruit commodities with egrigious unit price variation.

### Filters & Caveats

This provides a framework for a for calculating the variance in commodity unit prices across all commodities.  However, in the current state, some naive assumptions and filters are applied.

* Imports into into EU/USA/Japan (`is_import=='t'`) are reported separately from the exports from these countries (`is_import=='f'`).  
* Only goods priced in euros (`currency=='EUR'`).  Can be improved w/ [@margaretmf]'s exchange rate code.
* No restriction on units (however all shipments in EUR are measured in kilograms (`unit_1_id==24`)).
* Only shipments valued at > 1 in quantity and value (`quantity_1>0 & value>1 & is.na(value)==F`) 

#### Assumptions

We are assuming that `id` from the commodity table corresponds to `commodity_id` in the trade table.  The commodity table contains 55618 unique `ids` from 1 to 55620.  The trade tables (as described above) contain roughly the same number of unique `commodity_ids` (48703) ranging from 4 to 54740.  The order of `commodity_id` appears to be related to currency (USD => EUR => JPY => USD again).  Note that each commodity_id can map to the same hs_code (appears to happen up to twice in some cases).

#### Comments on commodity sample

* The full trade table contains `commodity_ids` ranging from **4** to **54745**.
* Restricting the trade table to just `currency=='EUR'` leaves `commodity_ids` ranging from **27406** to **38026**. 
* This leaves ~9k unique `commodity ids` shipped in EUR.
* While commodities priced in USD have more unique commodities shipped (~26k vs the ~9k shipped in EUR), they account for less transactions in the trade table (~13% in USD vs the ~85% in EUR and ~2% in JPY).
* This sample includes only (and all) trades reported European countries (all US reported trade is in USD).  All EU trade (using `src_country_id`) is in `currency=='EUR'`.
* All commodities priced in Euros (`currency=='EUR'`) are also measured in kilograms (`unit_1_id==24`).  The other ~48 unit types are split between USD and JPY.

#### Unit Types by currency

The numbers under each currency represent the total number of transactions in that currency for the corresponding unit type.  Note that all the transactions in Euros are denominated in Kilograms.

| unit_1_id|description           |      EUR|     USD|     JPY|
|---------:|:---------------------|--------:|-------:|-------:|
|         1|Barrels               |        0|   56307|       0|
|         2|Carat                 |        0|     561|     105|
|         3|Content Kilogram      |        0|    2637|       0|
|         5|Square Centimeters    |        0|   10474|       0|
|         6|Content Gram          |        0|     270|       0|
|         7|Content Metric Ton    |        0|   11748|       0|
|         8|Clean Yield Kilogram  |        0|     357|       0|
|         9|Dozen                 |        0|  730396|       0|
|        10|Dozen Pieces          |        0|   26002|       0|
|        11|Dozen Pair            |        0|   68215|       0|
|        12|Doses                 |        0|       9|       0|
|        14|Fiber Meter           |        0|    3956|       0|
|        15|Gigabecquerels        |        0|       6|       0|
|        16|Gross Containers      |        0|     314|       0|
|        17|Gram                  |        0|    5668|    1845|
|        19|Gross                 |        0|   38392|       0|
|        21|Gross Tonnage         |        0|       0|      17|
|        22|Hundreds              |        0|    5205|       0|
|        24|Kilogram              | 73865660| 3985274| 1665234|
|        27|Cubic Kilometers      |        0|    3648|       0|
|        28|Kilogram Total Sugars |        0|     109|       0|
|        29|Liters                |        0|  157776|   13354|
|        30|Linear Meters         |        0|      12|       0|
|        31|Meters                |        0|    7999|     400|
|        32|Square Meters         |        0|  309082|    8139|
|        33|Cubic Meters          |        0|   87671|    5407|
|        34|Megabecquerels        |        0|       2|       0|
|        35|Megawatt Hours        |        0|       1|       0|
|        36|Number                |        0| 3057869|   95188|
|        38|Pieces                |        0|   17234|       0|
|        39|Proof Liter           |        0|   33214|       0|
|        40|Pack                  |        0|    2432|       0|
|        41|Pairs                 |        0|  137122|    1460|
|        42|Square                |        0|     299|       0|
|        43|Set                   |        0|       0|    1681|
|        44|Metric Tons           |        0|  173266|   44519|
|        46|Thousand Meters       |        0|      39|       0|
|        48|Thousand              |        0|   28794|    1256|
|        49|No First Unit Of Qty  |        0| 2261248|       0|




#### Summary of sample

* `src_country_name3`: 3 letter name of reporting country (`src_country_name`) 
* `src_country_id`: `country_id` from trade table for reporting country
* `value_EUR_billions`: value in billions of Euros of trade (imports + exports) from/to the source (reporting) country.
* `transactions`: Number of transactions (imports + exports) from/to the source (reporting) country.

|src_country_name3 | src_country_id| value_EUR_billions| transactions|
|:-----------------|--------------:|------------------:|------------:|
|DEU               |             83|         3998.12285|     10872188|
|GBR               |            236|         2002.70753|      6800244|
|NLD               |            158|         1619.98900|      5645130|
|ITA               |            110|         1420.44516|      6518149|
|BEL               |             22|         1195.50386|      5122876|
|ESP               |            210|         1020.86565|      5795177|
|POL               |            178|          624.02937|      4114391|
|AUT               |             15|          476.52731|      3933372|
|SWE               |            216|          439.12938|      3742856|
|IRL               |            107|          373.61078|      1372819|
|HUN               |            101|          299.53064|      2486760|
|SVK               |            203|          234.47536|      1647069|
|PRT               |            179|          220.77436|      2418736|
|ROU               |            183|          206.43504|      2107082|
|GRC               |             86|          138.75852|      1678024|
|LTU               |            130|           95.89326|      2013411|
|SVN               |            204|           95.65554|      1858776|
|BGR               |             35|           83.04779|      1201875|
|LUX               |            131|           58.21414|      1006037|
|HRV               |             56|           52.99638|      1569800|
|LVA               |            124|           41.67150|      1223162|
|MLT               |            139|           15.30971|       334372|
|CYP               |             59|           12.20828|       403354|

### How to Read Output

* `commodity_id`: commodity_id from trade table
* `sum_value`: sum of `value` (Euros) from trade table across all trade for the given commodity (summed across countries and months)
* `sd_unit1price`: coefficient of variation (sd/mean) of unit price (`value`/`quantity_1`) across all trades for the given commodity (across months and countries)
* `country_id_cnt`: count of unique countries exporting the given commodity
* `src_country_id_cnt`: count of unique countries importing the given commodity.
* `hs_code`: hs_code from commodity table
* `hs_description`: description of good from commodity table
* `anomalous_src_country`: top 3 countries w most unit price deviation.  Measured by the absolute value of the difference between the average unit price (across months) for each a country from the average unitprice across all countries.
* `anomalous_country`: same calculation as `anomalous_src_country`, but for `country_id`.
* `country_list_sortby_value` (not pictured here for brevity): list of all exporting countries sorted from most to least volume (measured by `value`) of the given commodity.
* `src_country_list_sortby_value`: same as `country_list_sortby_value`, but for the reporting source country (EU/Japan/USA).

### Raw Data Sample for Most Price-Variant Commodities

As a convenience, we export the raw records from the `trade` table for the top commodities flagged using this analysis.  We used this as a sanity check when developing and testing this methodology.   We intend for it to be used as a reference when investigating and developing intuitions into why these commodities show so much unit-price variance. 

[coefficient of variation]:https://en.wikipedia.org/wiki/Coefficient_of_variation
[@margaretmf]:https://github.com/margaretmf

### Top 20 Anomalous Commodities

More complete results in CSV in Google Drive (`gfi/results/variance_by_commodity`)

### Imports

(into USA/Japan/EU)


| commodity_id|  sum_value| sd_unit1price| country_id_cnt| src_country_id_cnt|hs_code  |hs_description                                                                                                                                                                                                                                                                                                                                                             |
|------------:|----------:|-------------:|--------------:|------------------:|:--------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|        29753| 5035400473|      76.70329|            133|                 23|22021000 |WATERS, INCL. MINERAL AND AERATED, WITH ADDED SUGAR, SWEETENER OR FLAVOUR, FOR DIRECT CONSUMPTION AS A BEVERAGE                                                                                                                                                                                                                                                            |
|        35854| 1280921711|      72.74004|             98|                 23|84314920 |PARTS OF MACHINERY OF HEADING 8426, 8429 AND 8430 OF CAST IRON OR CAST STEEL, N.E.S.                                                                                                                                                                                                                                                                                       |
|        31719| 2068384205|      70.54883|             66|                 23|39031900 |POLYSTYRENE, IN PRIMARY FORMS (EXCL. EXPANSIBLE)                                                                                                                                                                                                                                                                                                                           |
|        29912|  982619578|      67.80676|             75|                 23|22086011 |VODKA OF AN ALCOHOLIC STRENGTH OF <= 45,4% VOL, IN CONTAINERS HOLDING <= 2 L                                                                                                                                                                                                                                                                                               |
|        32351| 1805959524|      62.98654|             65|                 23|44123210 |PLYWOOD CONSISTING SOLELY OF SHEETS OF WOOD <= 6 MM THICK, WITH AT LEAST ONE OUTER PLY OF ALDER,  ASH, BEECH, BIRCH, CHERRY, CHESTNUT, ELM, HICKORY, HORNBEAM, HORSE CHESTNUT, LIME, MAPLE, OAK, PLANE TREE, POPLAR, ROBINIA, WALNUT OR YELLOW POPLAR (EXCL. SHEETS OF COMPRESSED WOOD, CELLULAR WOOD PANELS, INLAID WOOD AND SHEETS IDENTIFIABLE AS FURNITURE COMPONENTS) |
|        36805| 1657416932|      57.75691|             84|                 23|85362010 |AUTOMATIC CIRCUIT BREAKERS FOR A VOLTAGE <= 1.000 V, FOR A CURRENT <= 63 A                                                                                                                                                                                                                                                                                                 |
|        34824| 1556917427|      57.18036|             75|                 23|73251000 |ARTICLES OF NON-MALLEABLE CAST IRON, N.E.S.                                                                                                                                                                                                                                                                                                                                |
|        35722|  827561607|      55.72480|             67|                 23|84191900 |INSTANTANEOUS OR STORAGE WATER HEATERS, NON-ELECTRIC (EXCL. INSTANTANEOUS GAS WATER HEATERS AND BOILERS OR WATER HEATERS FOR CENTRAL HEATING)                                                                                                                                                                                                                              |
|        29763| 1577023049|      54.45011|             88|                 23|22030009 |BEER MADE FROM MALT, IN CONTAINERS HOLDING <= 10 L (EXCL. IN BOTTLES)                                                                                                                                                                                                                                                                                                      |
|        34099|  921878925|      49.79932|             67|                 23|68109100 |PREFABRICATED STRUCTURAL COMPONENTS FOR BUILDING OR CIVIL ENGINEERING OF CEMENT, CONCRETE OR ARTIFICIAL STONE, WHETHER OR NOT REINFORCED                                                                                                                                                                                                                                   |
|        37394| 2956589740|      48.21043|             82|                 23|90192000 |OZONE THERAPY, OXYGEN THERAPY, AEROSOL THERAPY, ARTIFICIAL RESPIRATION OR OTHER THERAPEUTIC RESPIRATION APPARATUS                                                                                                                                                                                                                                                          |
|        31774| 1610889338|      47.58267|             67|                 23|39089000 |POLYAMIDES, IN PRIMARY FORMS (EXCL. POLYAMIDES-6, -11, -12, -6,6, -6,9, -6,10 AND -6,12)                                                                                                                                                                                                                                                                                   |
|        31013| 2450149746|      47.22209|             56|                 23|29319080 |SEPARATE CHEMICALLY DEFINED ORGANO-INORGANIC COMPOUNDS, N.E.S.                                                                                                                                                                                                                                                                                                             |
|        29749|  727510273|      46.48489|             89|                 23|22011011 |MINERAL WATERS, NATURAL, NOT CONTAINING ADDED SUGAR OR OTHER SWEETENING MATTER NOR FLAVOURED, NOT CARBONATED                                                                                                                                                                                                                                                               |
|        31632|  148054446|      45.93671|             53|                 23|38241000 |PREPARED BINDERS FOR FOUNDRY MOULDS OR CORES                                                                                                                                                                                                                                                                                                                               |
|        31776|  579530060|      44.78800|             50|                 23|39091000 |UREA RESINS AND THIOUREA RESINS, IN PRIMARY FORMS                                                                                                                                                                                                                                                                                                                          |
|        29251|  367477721|      44.58636|             37|                 23|17021100 |LACTOSE IN SOLID FORM AND LACTOSE SYRUP, NOT CONTAINING ADDED FLAVOURING OR COLOURING MATTER, CONTAINING BY WEIGHT >= 99% LACTOSE, EXPRESSED AS ANHYDROUS LACTOSE, CALCULATED ON THE DRY MATTER                                                                                                                                                                            |
|        32323| 1630069929|      44.34621|             50|                 23|44101130 |PARTICLE BOARD OF WOOD, WHETHER OR NOT AGGLOMERATED WITH RESINS OR OTHER ORGANIC BINDING SUBSTANCES, SURFACE-COVERED WITH MELAMINE-IMPREGNATED PAPER (EXCL. ORIENTED STRAND BOARD AND WAFERBOARD, FIBREBOARD AND CELLULAR WOOD PANELS)                                                                                                                                     |
|        29758|  686918164|      44.25425|             61|                 23|22029091 |NON-ALCOHOLIC BEVERAGES CONTAINING < 0,2% FATS DERIVED FROM MILK OR MILK PRODUCTS                                                                                                                                                                                                                                                                                          |
|        29313| 1541786873|      44.00541|             92|                 23|18063290 |CHOCOLATE AND OTHER PREPARATIONS CONTAINING COCOA, IN BLOCKS, SLABS OR BARS OF <= 2 KG (EXCL. FILLED AND WITH ADDED CEREAL, FRUIT OR NUTS)                                                                                                                                                                                                                                 |
 

### Exports 

(from USA/Japan/EU)

| commodity_id|  sum_value| sd_unit1price| country_id_cnt| src_country_id_cnt|hs_code  |hs_description                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
|------------:|----------:|-------------:|--------------:|------------------:|:--------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|        31758| 6519783278|      93.63062|            153|                 22|39072020 |POLYETHER ALCOHOLS, IN PRIMARY FORMS (EXCL. POLYETHYLENE GLYCOLS)                                                                                                                                                                                                                                                                                                                                                                                                                         |
|        32049| 1800687401|      88.49745|            167|                 22|40169957 |ARTICLES OF VULCANISED RUBBER (EXCL. HARD RUBBER), OF A TYPE INTENDED EXCLUSIVELY OR MAINLY FOR USE IN MOTOR VEHICLES OF HEADING 8701 TO 8705, N.E.S. (EXCL. THOSE OF CELLULAR RUBBER, AND RUBBER-TO-METAL BONDED PARTS)                                                                                                                                                                                                                                                                  |
|        32486| 2497845232|      71.67519|            164|                 22|48025700 |UNCOATED PAPER AND PAPERBOARD, OF A KIND USED FOR WRITING, PRINTING OR OTHER GRAPHIC PURPOSES, AND NON-PERFORATED PUNCHCARDS AND PUNCH-TAPE PAPER, IN SQUARE OR RECTANGULAR SHEETS WITH ONE SIDE > 435 MM OR WITH ONE SIDE <= 435 MM AND THE OTHER SIDE > 297 MM IN THE UNFOLDED STATE, NOT CONTAINING FIBRES OBTAINED BY A MECHANICAL OR CHEMI-MECHANICAL PROCESS OR OF WHICH <= 10% BY WEIGHT OF THE TOTAL FIBRE CONTENT CONSISTS OF SUCH FIBRES, AND WEIGHING 40 G TO 150 G/M², N.E.S. |
|        32365| 1741878744|      69.93339|            178|                 23|44152020 |PALLETS AND PALLET COLLARS, OF WOOD                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
|        32553| 3946790318|      65.03863|            165|                 21|48101900 |PAPER AND PAPERBOARD USED FOR WRITING, PRINTING OR OTHER GRAPHIC PURPOSES, NOT CONTAINING FIBRES OBTAINED BY A MECHANICAL OR CHEMI-MECHANICAL PROCESS OR OF WHICH <= 10% BY WEIGHT OF THE TOTAL FIBRE CONTENT CONSISTS OF SUCH FIBRES, COATED ON ONE OR BOTH SIDES WITH KAOLIN OR OTHER INORGANIC SUBSTANCES, IN SQUARE OR RECTANGULAR SHEETS WITH ONE SIDE > 435 MM OR WITH ONE SIDE <= 435 MM AND THE OTHER SIDE > 297 MM IN THE UNFOLDED STATE                                         |
|        35714| 1302178866|      58.68783|            167|                 22|84186100 |HEAT PUMPS (EXCL. AIR CONDITIONING MACHINES OF HEADING 8415)                                                                                                                                                                                                                                                                                                                                                                                                                              |
|        35781|  657397492|      57.69429|            162|                 21|84248110 |AGRICULTURAL OR HORTICULTURAL WATERING APPLIANCES, WHETHER OR NOT HAND-OPERATED                                                                                                                                                                                                                                                                                                                                                                                                           |
|        34150|  622243731|      56.87787|            178|                 23|69089020 |GLAZED FLAGS AND PAVING, HEARTH OR WALL TILES AND MOSAIC CUBES AND THE LIKE, OF COMMON POTTERY (EXCL. DOUBLE TILES OF THE ""SPALTPLATTEN"" TYPE, TILES SPECIALLY ADAPTED AS TABLE MATS, ORNAMENTAL ARTICLES, TILES SPECIFICALLY MANUFACTURED FOR STOVES, TILES AND CUBES AND THE LIKE THE LARGEST SURFACE AREA OF WHICH IS CAPABLE OF BEING ENCLOSED IN A SQUARE OF SIDE OF < 7 CM)                                                                                                       |
|        29762| 5424535920|      54.93372|            204|                 23|22030001 |BEER MADE FROM MALT, IN BOTTLES HOLDING <= 10 L                                                                                                                                                                                                                                                                                                                                                                                                                                           |
|        30519|  442022484|      54.14830|            110|                 19|28352600 |PHOSPHATES OF CALCIUM (EXCL. CALCIUM HYDROGENORTHOPHOSPHATE ""DICALCIUM PHOSPHATE"")                                                                                                                                                                                                                                                                                                                                                                                                      |
|        37259|  887305611|      52.76907|            159|                 20|89079000 |RAFTS, TANKS, COFFER-DAMS, LANDING STAGES, BUOYS, BEACONS AND OTHER FLOATING STRUCTURES (EXCL. INFLATABLE RAFTS, VESSELS OF HEADING 8901 TO 8906 AND FLOATING STRUCTURES FOR BREAKING UP)                                                                                                                                                                                                                                                                                                 |
|        34224|  329506890|      52.50207|            109|                 21|70052935 |FLOAT GLASS AND SURFACE GROUND AND POLISHED GLASS, IN SHEETS, BUT NOT OTHERWISE WORKED, OF A THICKNESS OF > 3,5 MM BUT <= 4,5 MM (EXCL. WIRED GLASS OR GLASS COLOURED THROUGHOUT THE MASS ""BODY TINTED"", OPACIFIED, FLASHED OR MERELY SURFACE GROUND, OR GLASS HAVING AN ABSORBENT, REFLECTING OR NON-REFLECTING LAYER)                                                                                                                                                                 |
|        36914| 1526376399|      51.93581|            174|                 22|85444999 |ELECTRIC CONDUCTORS FOR A VOLTAGE 1.000 V, INSULATED, NOT FITTED WITH CONNECTORS, N.E.S. (EXCL. WINDING WIRE, COAXIAL CONDUCTORS, WIRING SETS FOR VEHICLES, AIRCRAFT OR SHIPS, AND WIRE AND CABLES WITH INDIVIDUAL CONDUCTOR WIRES OF A DIAMETER > 0,51 MM)                                                                                                                                                                                                                               |
|        34648| 1905847799|      51.15498|            119|                 21|72193410 |FLAT-ROLLED PRODUCTS OF STAINLESS STEEL, OF A WIDTH OF >= 600 MM, NOT FURTHER WORKED THAN COLD-ROLLED ""COLD-REDUCED"", OF A THICKNESS OF >= 0,5 MM BUT <= 1 MM, CONTAINING BY WEIGHT >= 2,5% NICKEL                                                                                                                                                                                                                                                                                      |
|        34602|  224921183|      49.84398|            129|                 20|72169110 |SHEETS SHEETS OF IRON OR NON-ALLOY STEEL, COLD-FORMED OR COLD FINISHED, PROFILED ""RIBBED""                                                                                                                                                                                                                                                                                                                                                                                               |
|        36821|  213128811|      47.30604|            150|                 23|85366190 |LAMP HOLDERS FOR A VOLTAGE <= 1.000 V (EXCL. EDISON LAMP HOLDERS)                                                                                                                                                                                                                                                                                                                                                                                                                         |
|        34140|  328186323|      46.38698|            112|                 21|69041000 |BUILDING BRICKS (EXCL. THOSE OF SILICEOUS FOSSIL MEALS OR SIMILAR SILICEOUS EARTHS, AND REFRACTORY BRICKS OF HEADING 6902)                                                                                                                                                                                                                                                                                                                                                                |
|        27563| 2322512418|      46.28740|             65|                 21|02071310 |FRESH OR CHILLED BONELESS CUTS OF FOWLS OF THE SPECIES GALLUS DOMESTICUS                                                                                                                                                                                                                                                                                                                                                                                                                  |
|        35798| 1235313739|      45.96320|            148|                 23|84262000 |TOWER CRANES                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
|        36673| 2447512934|      45.79247|            158|                 23|85234951 |DIGITAL VERSATILE DISCS ""DVD"", RECORDED, FOR REPRODUCING SOUND AND IMAGE OR IMAGE ONLY (EXCL. FOR REPRODUCING REPRESENTATIONS OF INSTRUCTIONS, DATA, SOUND, AND IMAGE RECORDED IN A MACHINE-READABLE BINARY FORM, AND CAPABLE OF BEING MANIPULATED OR PROVIDING INTERACTIVITY TO A USER, BY MEANS OF AN AUTOMATIC DATA-PROCESSING MACHINE)                                                                                                                                              |







