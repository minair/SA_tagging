library(RAQSAPI)
library(tidyverse)
library(data.table)
library(lubridate)

# extract O3 monitor information
#aqs_sign_up(email = "mzhong@pa.gov")
aqs_credentials(username = "mzhong@pa.gov",
                key = "aquamouse47")

state_list = c('42', '34', '24', '10', '09', '36','11','51','54', '44') # extract all O3 sites in OTC states
# PA(42), NJ(34), CT(09), MD(24), DE(10)
#NY (36), DC(11), VA(51), wv 54, RI (44)
read_monitors <- function(state){
    monitors <- aqs_monitors_by_state(parameter = "44201",
                     bdate = as.Date("20190601",
                                     format="%Y%m%d"),
                     edate = as.Date("20200602",
                                     format="%Y%m%d"),
                     stateFIPS = state)
    return(monitors)
    }

`%notin%` <- Negate(`%in%`)

monitor_data <- lapply(state_list, read_monitors) %>% 
  bind_rows() %>% 
  unite(aqs, state_code, county_code, site_number, sep = "")%>% 
  filter(cbsa_code %in% c(37980, 35620,12580,25540,35300,35980, 47900, 14860, 45860,35980, 49340, 39300)) %>%
  filter(aqs %notin% c(360715001,360790005,360270007,510610002,511790001)) %>% # drop aqs not in 2015 O3 NAA 
  select(aqs, latitude, longitude, cbsa_name, state_name, county_name, local_site_name) 

monitor_data <- distinct(monitor_data)

#"New York-Newark-Jersey City, NY-NJ-PA"  35620
#"Baltimore-Columbia-Towson, MD"  12580
#'Philadelphia-Wilmington-Atlantic City'
#Hartford-West Hartford-East Hartford, CT 25540
#New Haven-Milford, CT, 35300
#Norwich-New London, CT 35980
#Philadelphia-Camden-Wilmington, PA-NJ-DE-MD 37980
#Washington-Arlington-Alexandria, DC-VA-MD-WV 47900

#write_csv(monitor_data, './processed_data/o3_monitors.csv') # save monitor info

write_csv(monitor_data, './processed_data/NAA_o3_monitors.csv') # save monitors in Philadelphia-Camden-Wilmington   

# -------Read tagging data--------
monitor_o3<- fread('./processed_data/NAA_o3_monitors.csv')

aqs <- monitor_o3$aqs

reg_pattern <- paste0( ".*(", paste(aqs, collapse = "|" ), ").*" ) # create reg pattern

file_list <- list.files("./input", pattern = "*CAMx*", full.names = TRUE)

file_select <- file_list[str_detect(file_list, reg_pattern)]

read_data <- function(file){
  data <- fread(file, colClasses=c(rep('character', 181)))
  return(data)
}

tagging_data <- lapply(file_select, read_data) %>% 
  bind_rows() %>% 
  pivot_longer(as.character(20160408):as.character(20160929), names_to = 'date', values_to = 'o3') %>% 
  mutate(date = ymd(date), o3 = as.numeric(o3)) %>% 
  mutate(Source_state = ifelse(Source_state == " ", Tag, Source_state)) # fill black source_state with tag 


write_csv(tagging_data, './processed_data/NAA_tagging.csv')

aqs2 <- unique(tagging_data$siteid)
#aqs2 tagging data contains 090070007

#aqs contains 90079007
setdiff(aqs, aqs2)

aqs2019 <- aqs_annualsummary_by_box(parameter = "44201",
                                    bdate = as.Date("20190101",
                                                    format = "%Y%m%d"),
                                    edate = as.Date("20191231",
                                                    format = "%Y%m%d"),
                                    minlat = "36.93",
                                    maxlat = "45.77",
                                    minlon = "-83.5",
                                    maxlon = "-67.96"
)

aqs2019.8hr <- aqs2019 %>%
  filter(pollutant_standard == "Ozone 8-hour 2015")

aqs2019.8hr2 <- aqs2019.8hr[,c(1:8,14,35,36,48,50,54)]
aqs2019.8hr2 <- aqs2019.8hr2 %>% unite("Monitor_ID",state_code:site_number,sep = "",remove = FALSE)
#shortened list of header names
headers2019 <- c("Monitor_ID","StateFIPS","CountyFIPS","SiteNumber","ParameterCode","POC","Latitude","Longitude",
                 "Datum","Year","2019_4thMax","2019_4thMax_Datetime","SiteName","State","CBSA")

#applying header names to data frame
colnames(aqs2019.8hr2) <- headers2019

test_data <- lapply(state_list, read_monitors) %>% 
  bind_rows() %>% 
  unite(aqs, state_code, county_code, site_number, sep = "")
