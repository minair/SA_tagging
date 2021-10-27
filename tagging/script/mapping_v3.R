# R scripts to create interactive map. 
#Author: Min Zhong
#Started on Aug.2021 


#------------load libraries-----------------

library(tidyverse)
library(leaflet)
library(data.table)
#library(lubridate)
library(htmltools)
library(leafpop)
library(viridis)
library(hrbrthemes)
library(sf)

#--------------read data---------------------

# read NAA shape files 
US_2015_O3_NAA <- read_sf('./processed_data/US_2015_ozone_NAA_4326.json')

names(US_2015_O3_NAA) # view column names

# list of NAA in OTC region. 
area_list <- c("Washington",
               'Philadelphia-Wilmington-Atlantic City',
               'New York-Northern New Jersey-Long Island',
               'Baltimore',
               'Greater Connecticut')

temp_polygon <- US_2015_O3_NAA %>%
    select(c('area_name', 'classification', 'geometry')) %>% # select useful columns
    filter(area_name %in% area_list)  # select NAA in OTC

#read o3 monitor info and tagging data 
monitors<- fread('./processed_data/NAA_o3_monitors.csv') # monitor info from EPA
site_name <- fread('./processed_data/site_name.csv') # site name

monitors <- monitors %>% 
    left_join(site_name, by = 'aqs') 

# source apportionment data
tagging <- fread('./processed_data/NAA_tagging.csv', select = c(1, 4:8)) 

#read state code
state_list <- fread('./processed_data/all_states.csv')
states <- state_list$Code

#----------calculate average ozone concentration at each monitor--------------

# function to pick top N ozone days for each monitor
find_date_topN <- function(df,N){
    select_date_topN <- df %>%
        filter(Tag == '8hrO3Max') %>% # filter days based 8-hour maximum ozone
        group_by(siteid) %>% 
        arrange(desc(o3), .by_group = TRUE) %>% 
        filter(row_number() %in% c(1:N))
    return(select_date_topN)
}

# function to pick days with O3 Conc. > threshold for each monitor
find_date_exceed <- function(df, threshold){
    select_date_exceed <- df %>% 
        filter(Tag == '8hrO3Max') %>% 
        group_by(siteid) %>% 
        filter(o3 > threshold, .by_group = TRUE)
    return(select_date_exceed)
}

# round function
round_data <- function(x)(round(x, 1)) 

# function to calculate average o3 concentration for each monitor
find_o3_avg <- function(select_date){  
    o3_avg <- select_date %>% 
        group_by(siteid) %>% 
        summarise(avg_o3 = mean(o3))%>% 
        mutate_at(vars(2), round_data) %>% 
        left_join(monitors, by = c('siteid' = 'aqs'))  
    return(o3_avg)
}


select_date_top4 <- find_date_topN(tagging, 4) 
o3_avg_top4 <- find_o3_avg(select_date_top4)
  
select_date_top10 <- find_date_topN(tagging, 10)
o3_avg_top10 <- find_o3_avg(select_date_top10)

select_date_exceed <- find_date_exceed(tagging, 70)
o3_avg_exceed <- find_o3_avg(select_date_exceed)


#-----------create bar charts to show source contribution to ozone--------------------

# function to obtain ozone contribution of states to a monitor
find_tagging_state <- function(iaqs, tagging_df, o3_date){
    select_tagging_state <- tagging_df %>% 
        filter(siteid == iaqs, date %in% o3_date$date) %>% 
        group_by(Source_state, Source_sector) %>% 
        summarise(avg_by_sector = mean(o3), .groups = 'drop')%>% # ozone contribution of sectors in each state
        group_by(Source_state) %>% 
        summarise(o3 = round(sum(avg_by_sector), 2), .groups = 'drop') %>% # ozone contribution of each state
        filter(o3 >= 0.7, Source_state %in% states) %>% 
        arrange(desc(o3)) %>% 
        mutate(class = 'State', o3 = round_data(o3)) %>% 
        rename('tag' = 'Source_state')
    return(select_tagging_state)
  
}

# create notin function
`%notin%` <- Negate(`%in%`) # Negate() is used to negate a function

# function to calculate ozone contribution of each sector
find_tagging_sector <- function(iaqs, tagging_df, o3_date){
    sector_to_exclude <- c('8hrO3Max', '8hrO3NOx','8hrO3VOC', 'BC', 'Biogenic', 'Canada')
    
    select_tagging_sector <- tagging_df %>% 
        filter(siteid == iaqs, date %in% o3_date$date) %>% 
        group_by(Source_sector, Source_state) %>% 
        summarise(avg_by_state = mean(o3), .groups = 'drop') %>% 
        group_by(Source_sector) %>% 
        summarise(o3 = round(sum(avg_by_state), 2), .groups = 'drop') %>% 
        filter(o3 >= 0.7, Source_sector %notin% sector_to_exclude) %>% 
        arrange(desc(o3)) %>% 
        mutate(class = 'Source', o3 = round_data(o3)) %>% 
        rename('tag' = 'Source_sector')
    return(select_tagging_sector)
}

#test_state <- find_tagging_state(90010017, tagging, select_date_top4)
#test_sector <- find_tagging_sector(90010017, tagging, select_date_top4)

#create contribution level factors for coloring and label
create_contrib_level = function(df){
  level <- cut(df, 
               c(0.7, 1.4, 2.8, 5.6, 11.2, 70),
               include.lowest = T, 
               labels = c('lev1', 'lev2', 'lev3', 'lev4', 'lev5'))
}

# function to make horizontal bar charts for each monitor
plot_o3 <- function(iaqs, select_date, o3_avg, tagging_df){

    o3_date <- select_date %>% 
        filter(siteid == iaqs)
  
    select_tagging_state <- find_tagging_state(iaqs, tagging_df, o3_date)
    select_tagging_sector <- find_tagging_sector(iaqs, tagging_df, o3_date)
    select_tagging <- bind_rows(select_tagging_state, select_tagging_sector)

    select_tagging$contrib_level = create_contrib_level(select_tagging$o3)
    
    monitor <- o3_avg %>% 
      filter(siteid == iaqs)

    p <- ggplot(select_tagging,aes(x=reorder(tag, o3), o3, fill = contrib_level))+  # reorder the tag factor based on the value o3
        geom_col() +
        scale_fill_viridis(discrete=TRUE, guide=FALSE, option="C", direction = -1)+
        ylim(0, max(select_tagging$o3)+3)+
        geom_text(aes(label=as.character(o3)), size=4, hjust = -0.1)+
        coord_flip()+ # vertical bar to horizontal bar
        labs(title = paste0("AQS Site ID: ", monitor$siteid, '\n', 
                        'Site Name: ', monitor$SiteName, '\n',
                        'County: ', monitor$county_name, '\n', 
                        'State: ', monitor$state_name, '\n',
                        'Average O3 (ppb): ', monitor$avg_o3),
             x = 'Major Contributors',  
             y="O3 contribution (ppb)") +
        theme_ipsum()+
        theme(plot.title = element_text(size=10), 
            axis.text=element_text(size=8),  
            plot.margin = margin(0, 0.5, 0, 0, "cm"),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            axis.line = element_line(colour = "gray"),
            axis.ticks.x=element_blank(),
            axis.text.x = element_blank(),
            panel.spacing = unit(0, "cm"))+
        facet_wrap( ~ class, nrow = 1, scales = 'free')
  return(p)
}

#plot_o3(90010017,select_date_top4,o3_avg_top4,tagging)

monitor_top4 <- unique(o3_avg_top4$siteid)
monitor_top10 <- unique(o3_avg_top10$siteid)
monitor_exceed <- unique(o3_avg_exceed$siteid)

p_all_top4 <- lapply(monitor_top4, plot_o3, select_date = select_date_top4, o3_avg = o3_avg_top4, tagging_df = tagging)
p_all_top10 <- lapply(monitor_top10, plot_o3, select_date = select_date_top10, o3_avg = o3_avg_top10, tagging_df = tagging)
p_all_exceed <- lapply(monitor_exceed, plot_o3, select_date = select_date_exceed, o3_avg = o3_avg_exceed, tagging_df = tagging)

#p_all_exceed[6]

#-------create O3 level factors for coloring and label---------

create_o3_level <- function(df){
  level <- cut(df, 
               c(0,70,75,80,100), 
               include.lowest = T,
               labels = c('<70', '70-75', '75-80', '>80')) 
  return(level)
}

o3_avg_top4$o3_level <- create_o3_level(o3_avg_top4$avg_o3) 
o3_avg_top10$o3_level <- create_o3_level(o3_avg_top10$avg_o3)  
o3_avg_exceed$o3_level <- create_o3_level(o3_avg_exceed$avg_o3) 

#---------------create map-----------------------

#create Color Palette using colorFactor
create_color_palette = function(level){
    col <- colorFactor(palette = c("#78c679","#fecc5c", "#fd8d3c", "#e31a1c"), level) #color code, https://colorbrewer2.org/#type=sequential&scheme=YlOrRd&n=4 
    return(col)
}

col_top4 <- create_color_palette(o3_avg_top4$o3_level)
col_top10 <- create_color_palette(o3_avg_top10$o3_level)
col_exceed <- create_color_palette(o3_avg_exceed$o3_level)

naa_pal <- colorFactor(
  palette = "Oranges",
  US_2015_O3_NAA$classification) 

# add map title style
map_title_styple <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed;
    left: 50%;
    text-align: center;
    padding-left: 8px; 
    padding-right: 8px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 16px;
  }
"))

title <- tags$div(
  map_title_styple, HTML("Source Apportionment Modeling Analysis of Year 2023")
)  

# create label
create_label <- function(o3_avg){
    label <- sprintf(
          "<strong>%s</strong><br/>%s (ppb)",
          o3_avg$SiteName, o3_avg$avg_o3) %>% 
     lapply(HTML)
    
    return(label)
}

label_top4 <- create_label(o3_avg_top4)
label_top10 <- create_label(o3_avg_top10)
label_exceed <- create_label(o3_avg_exceed)

#create map
m <-leaflet() %>%
  addTiles(attribution = paste0('PA DEP - Min Zhong, Created on ', Sys.Date())) %>%
  addControl(title, position = "topleft", className="map-title") %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>% #CartoDB.DarkMatter, CartoDB.Positron
  setView(lng = -74.42944, lat =40.46218, zoom = 7) %>% #7
  addCircleMarkers(data = o3_avg_top4, lat = ~latitude, lng = ~longitude,
                   color = ~col_top4(o3_level), 
                   fillOpacity = 0.7, 
                   radius = 8,
                   weight = 1, 
                   opacity = 0.7, 
                   group = 'Avg top 4 days',
                   label = label_top4
  ) %>%
  addCircleMarkers(data = o3_avg_top10, lat = ~latitude, lng = ~longitude,
                   color = ~col_top10(o3_level), 
                   fillOpacity = 0.7, 
                   radius = 8,
                   weight = 1, 
                   opacity = 0.7, 
                   group = 'Avg top 10 days',
                   label = label_top10
  ) %>%
  addCircleMarkers(data = o3_avg_exceed, lat = ~latitude, lng = ~longitude,
                   color = ~col_exceed(o3_level), 
                   fillOpacity = 0.7,
                   radius = 8,
                   weight = 1, 
                   opacity = 0.7, 
                   group = 'Avg exceedence days',
                   label = label_exceed
                   
  ) %>%
  addLegend('bottomright', pal = col_top4, values = o3_avg_top4$o3_level,
            title = '2023 Ozone',
            opacity = 1) %>% 
  addPopupGraphs(p_all_top4, width = 400, height = 300, group = 'Avg top 4 days') %>% 
  addPopupGraphs(p_all_top10, width = 400, height = 300, group = 'Avg top 10 days') %>% 
  addPopupGraphs(p_all_exceed, width = 400, height = 300, group = 'Avg exceedence days') %>% 
  addPolygons(data = temp_polygon,
              stroke = TRUE,
              weight = 2,
              color = "black",
              smoothFactor = 0.2,
              fillOpacity = .5,
              fillColor = ~naa_pal(classification),
              group = 'US_2015_O3_NAA',
              popup = as.character(paste0(temp_polygon$area_name,"<br>",
                                          "Classification: ",temp_polygon$classification,"<br>"))) %>% 
  addLayersControl(
    baseGroups = c("Avg top 4 days", "Avg top 10 days", "Avg exceedence days"), # base group be viewed one each time
    options = layersControlOptions(collapsed = FALSE), # if many layers, use collapsed = TRUE
    overlayGroups = c('US_2015_O3_NAA')
  ) %>% 
  hideGroup("Avg top 10 days") %>% 
  hideGroup("Avg exceedence days") %>%
  hideGroup('US_2015_O3_NAA')

m

#saveWidget(m, file="./output/index.html")
