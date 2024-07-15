#####  Load Libraries #####
library(tidycensus)
library(tidyverse)
library(rio)
library(sf)
library(cpaltemplates)
library(cpaltools)
library(googlesheets4)

libDB <- "C:/Users/Michael/CPAL Dropbox/" # Michael Laptop

##### Import Dallas County Zip Codes #####
zipCounty <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1Os5DT5gAb6agPeFTYmzpqtCWWbXnMuHbdToeDB_mxiQ/",
                       sheet = "DallasCoZIPs")

##### Classify Objects and ACS Variables #####
years <- lst(2017, 2018, 2019, 2020, 2021, 2022)

surveys <- lst("acs1", "acs5")

acs_s <- load_variables(2022, "acs5/subject", cache = TRUE)
acs_b <- load_variables(2022, "acs5", cache = TRUE)

acs_var <- c(
  mhi = "B19013_001", #median household income
  tot_pop = "B01001_001", #total population
  his_pop = "B01001I_001", #hispanic population
  wht_pop = "B01001H_001", #white population
  blk_pop = "B01001B_001", #black population
  asn_pop = "B01001D_001", #asian population

  scl_pop = "B15003_001", #population over 25 for educational attainment
  hs_grad = "B15003_017", #high school graduate (over 25yo)
  ba_grad = "B15003_022", #bachelors degree (over 25yo)

  lan_pop = "C16002_001", #population over 5 for language at home
  lan_eng = "C16002_002", #language spoken at home (only english)

  lbr_pop = "B28007_002", #population over 16 in labor force
  unemp = "B28007_009", #labor force unemployment

  lbr_o16 = "B28007_001", # population over 16
  nolbr = "B28007_015", # population over 16 not in labor force

  pop_bp = "S1701_C02_001", #population below poverty
  pop_u18 = "B09001_001", #population under 18
  bp_u18 = "S1701_C02_002", #population under 18 below poverty

  mhi = "B19013_001", #median household income
  thh = "B25003_001", #total housing units
  rohh = "B25003_003", #renter occupied households
  oohh = "B25003_002", #owner occupied households

  pop_u6 = "B17020_003", #population under 6
  pop_6t11 = "B17020_004", #population between 6 and 11
  pop_12t17 = "B17020_005", #population between 12 and 17

  hisbp1_u18 = "B17020I_003", #hispanic population below poverty under 6
  hisbp2_u18 = "B17020I_004", #hispanic population below poverty 6 to 11
  hisbp3_u18 = "B17020I_005", #hispanic population below poverty 12 to 17

  blkbp1_u18 = "B17020B_003", #black population below poverty under 6
  blkbp2_u18 = "B17020B_004", #black population below poverty 6 to 11
  blkbp3_u18 = "B17020B_005", #black population below poverty 12 to 17

  asnbp1_u18 = "B17020D_003", #asian population below poverty under 6
  asnbp2_u18 = "B17020D_004", #asian population below poverty 6 to 11
  asnbp3_u18 = "B17020D_005", #asian population below poverty 12 to 17

  whtbp1_u18 = "B17020H_003", #white population below poverty under 6
  whtbp2_u18 = "B17020H_004", #white population below poverty 6 to 11
  whtbp3_u18 = "B17020H_005", #white population below poverty 12 to 17

  his_u18 = "B18101I_002", #hispanic population under 18
  wht_u18 = "B18101H_002", #white population under 18
  blk_u18 = "B18101B_002", #black population under 18
  asn_u18 = "B18101D_002", #asian population under 18

  fam_u18 = "B09002_001", #total families with children
  fam_nmry = "B09002_008", #families with children (non married-couples)

  wom_rep = "B13012_001", #women of reproductive age
  wom_bir = "B13012_002", #women of reproductive age who have had a birth in the last 12 months
  teen_15t17 = "B01001_030", #teen girls between 15 and 17 years old
  teen_18t19 = "B01001_031" #teen girls between 18 and 19 years old
)

##### Pull Data by Census Tract #####
cpal_tract <- map(
  years,
  ~get_acs(
    geography = "tract",
    state = "TX",
    county = "Dallas County",
    variables = acs_var,
    year = .x,
    survey = "acs5",
    output = "wide",
    geometry = TRUE),
  ) %>%
  map2(years, ~mutate(.x, year = .y)) %>%
  reduce(., rbind) %>%
  mutate(GEOGRAPHY = "Tract",
         SURVEY = "5 Year ACS",
         NAME = str_remove(NAME, ", Dallas County, Texas"))

##### Pull Data by Zip Codes #####
cpal_zcta <- map(
  years,
  ~get_acs(
  geography = "zcta",
  variables = acs_var,
  year = .x,
  survey = "acs5",
  output = "wide",
  geometry = FALSE),
) %>%
  map2(years, ~mutate(.x, year = .y)) %>%
  reduce(., rbind) %>%
  mutate(GEOGRAPHY = "Zip Code",
         SURVEY = "5 Year ACS") %>%
  filter(GEOID %in% zipCounty$ZIP)

zcta_old <- tigris::zctas(year = 2010) %>%
  rename(GEOID = GEOID10) %>%
  select(GEOID, geometry)

zcta_new <- tigris::zctas(year = 2020) %>%
  rename(GEOID = GEOID20) %>%
  select(GEOID, geometry)

cpal_zcta_old <- cpal_zcta %>%
  filter(year < 2020) %>%
  inner_join(zcta_old, .)

cpal_zcta_new <- cpal_zcta %>%
  filter(year >= 2020) %>%
  inner_join(zcta_new, .)

cpal_zcta <- rbind(cpal_zcta_old, cpal_zcta_new)

rm(cpal_zcta_new)
rm(cpal_zcta_old)
rm(zcta_old)
rm(zcta_new)

##### Pull Data by County #####
cpal_county <- map(
  years,
  ~get_acs(
  geography = "county",
  state = "TX",
  variables = acs_var,
  year = .x,
  survey = "acs5",
  output = "wide",
  geometry = TRUE),
) %>%
  map2(years, ~mutate(.x, year = .y)) %>%
  reduce(., rbind) %>%
  mutate(GEOGRAPHY = "County",
         SURVEY = "5 Year ACS") %>%
  filter(NAME %in% c("Dallas County, Texas",
                     "Tarrant County, Texas",
                     "Collin County, Texas",
                     "Denton County, Texas",
                     "Johnson County, Texas",
                     "Ellis County, Texas",
                     "Rockwall County, Texas",
                     "Kaufman County, Texas",
                     "Erath County, Texas",
                     "Hood County, Texas",
                     "Hunt County, Texas",
                     "Navarro County, Texas",
                     "Palo Pinto County, Texas",
                     "Parker County, Texas",
                     "Somervell County, Texas",
                     "Wise County, Texas"))

##### Pull Data by City #####
cpal_city <- map(
  years,
  ~get_acs(
  geography = "place",
  state = "TX",
  variables = acs_var,
  year = .x,
  survey = "acs5",
  output = "wide",
  geometry = TRUE),
) %>%
  map2(years, ~mutate(.x, year = .y)) %>%
  reduce(., rbind) %>%
  mutate(GEOGRAPHY = "City",
         SURVEY = "5 Year ACS") %>%
  .[filter(cpal_county, NAME == "Dallas County, Texas"), ] %>%
  mutate(NAME = str_remove(NAME, " town| city"))

##### Join Data Frames #####
cpalmetrics <- rbind(cpal_city, cpal_county, cpal_zcta, cpal_tract) %>%
  mutate(hisbp_u18E = hisbp1_u18E+hisbp2_u18E+hisbp3_u18E,
         hisbp_u18M =  sqrt(sqrt(hisbp1_u18M)+sqrt(hisbp2_u18M)+sqrt(hisbp3_u18M)),

         blkbp_u18E = blkbp1_u18E+blkbp2_u18E+blkbp3_u18E,
         blkbp_u18M =  sqrt(sqrt(blkbp1_u18M)+sqrt(blkbp2_u18M)+sqrt(blkbp3_u18M)),

         whtbp_u18E = whtbp1_u18E+whtbp2_u18E+whtbp3_u18E,
         whtbp_u18M =  sqrt(sqrt(whtbp1_u18M)+sqrt(whtbp2_u18M)+sqrt(whtbp3_u18M)),

         asnbp_u18E = asnbp1_u18E+asnbp2_u18E+asnbp3_u18E,
         asnbp_u18M =  sqrt(sqrt(asnbp1_u18M)+sqrt(asnbp2_u18M)+sqrt(asnbp3_u18M)),

         hisbp_u18P = round(hisbp_u18E/bp_u18E, digits = 3),
         blkbp_u18P = round(blkbp_u18E/bp_u18E, digits = 3),
         whtbp_u18P = round(whtbp_u18E/bp_u18E, digits = 3),
         asnbp_u18P = round(asnbp_u18E/bp_u18E, digits = 3),


         bp_u18P = round(bp_u18E/pop_u18E, digits = 3),
         pop_bpP = round(pop_bpE/tot_popE, digits = 3),
         rohhP = round(rohhE/thhE, digits = 3),
         oohhP = round(oohhE/thhE, digits = 3),

         his_popP = round(his_popE/tot_popE, digits = 3),
         wht_popP = round(wht_popE/tot_popE, digits = 3),
         blk_popP = round(blk_popE/tot_popE, digits = 3),
         asn_popP = round(asn_popE/tot_popE, digits = 3),

         hs_gradP = round(hs_gradE/scl_popE, digits = 3),
         ba_gradP = round(ba_gradE/scl_popE, digits = 3),

         lan_engP = round(lan_engE/lan_popE, digits = 3),

         unempP = round(unempE/lbr_popE, digits = 3),
         nolbrP = round(nolbrE/lbr_o16E, digits = 3),

         pop_u6P = round(pop_u6E/pop_u18E, digits = 3),
         pop_6t11P = round(pop_6t11E/pop_u18E, digits = 3),
         pop_12t17P = round(pop_12t17E/pop_u18E, digits = 3),

         his_u18P = round(his_u18E/pop_u18E, digits = 3),
         wht_u18P = round(wht_u18E/pop_u18E, digits = 3),
         blk_u18P = round(blk_u18E/pop_u18E, digits = 3),
         asn_u18P = round(asn_u18E/pop_u18E, digits = 3),

         fam_nmryP = round(fam_nmryE/fam_u18E, digits = 3),

         wom_birP = round(wom_birE/wom_repE, digits = 3),
         teen_girlE = teen_15t17E+teen_18t19E,
  ) %>%
  select(-(hisbp1_u18E:whtbp3_u18M),
         -lan_popE,
         -lan_popM,
         -(teen_15t17E:teen_18t19M)
         ) %>%
  rename(YEAR = year)

oldNames <- import("internal-dashboard/data/Dallas Metric Names.csv")

namesMetrics <- names(cpalmetrics) %>%
  as.data.frame(.) %>%
  rename(FieldName = 1) %>%
  left_join(., oldNames) %>%
  mutate(acsNames = str_sub(FieldName, end = -2))

export(namesMetrics, "internal-dashboard/data/Dallas Metric Names.csv")

st_write(cpalmetrics, "internal-dashboard/data/Dallas CPAL Metrics.geojson", delete_dsn = TRUE)
