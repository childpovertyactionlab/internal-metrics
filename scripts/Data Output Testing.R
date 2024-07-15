##### Libraries #####
library(cpaltemplates)
library(cpaltools)
library(bslib)
library(tidyverse)
library(sf)
library(janitor)

#wd <- ""
wd <- "C:/Users/micha/CPAL Dropbox/Analytics/04_Projects/CPAL Internal Dashboard/cpal-dashboard/"

##### Import Data #####
cpalmetrics <- st_read(paste0(wd, "data/Dallas CPAL Metrics.geojson"))

cpalcounty <- cpalmetrics %>%
  st_drop_geometry(.) %>%
  filter(type == "County",
         year == "2021")

names(cpalcounty)

cpalcounty %>%
  filter(year == "2021" & type == "County") %>%
  filter(GEOID == "48113") %>%
  st_drop_geometry(.) %>%
  t() %>%
  as.data.frame(stringsAsFactors = F) %>%
  rownames_to_column("originalName") %>%
  rename(value = 2) %>%
  mutate(metricType = ifelse(str_detect(originalName, "E$"),"Estimate",
                             ifelse(str_detect(originalName, "M$"),"MoE",
                                    ifelse(str_detect(originalName, "P$"),"Percent",
                                           ifelse(str_detect(originalName, "R$"), "Rate",
                                                  "Other")
                                           ))),
         metricName = ifelse(str_detect(originalName, "E$"), str_remove(originalName, "E$"),
                             ifelse(str_detect(originalName, "M$"),str_remove(originalName, "M$"),
                                    ifelse(str_detect(originalName, "P$"),str_remove(originalName, "P$"),
                                           ifelse(str_detect(originalName, "R$"), str_remove(originalName, "R$"),
                                                  originalName)
                                    )))) %>%
  filter(!originalName %in% c("NAME", "GEOID", "year", "type")) %>%
  select(-originalName) %>%
  pivot_wider(names_from = metricType, values_from = value) %>%
  arrange(metricName)


cpalmetrics %>%
  filter(year == "2021" & type == "County") %>%
  filter(GEOID == "48113") %>%
  st_drop_geometry(.) %>%
  t() %>%
  as.data.frame(.)

?rownames_to_column
