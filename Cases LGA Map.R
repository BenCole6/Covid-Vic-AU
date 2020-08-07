# install.packages("rpud")

library(pacman)

p_load(tidyverse,
       lubridate,
       ggplot2,
       ggmap,
       rvest,
       gganimate,
       plotly,
       transformr,
       gifski,
       png,
       beepr)

source("State Map.R")

Covid_website <- "https://covidlive.com.au/"

for(lga in lga_names) {
  
  lga <- str_replace_all(lga, pattern = " ", replacement = "-")
  
  lga_website <- read_html(paste(Covid_website, 
                                 "vic",
                                 lga,
                                 sep = "/"))
  
  lga_tables <- html_nodes(lga_website,
                           css = "table")
  
  lga_cases_date <- lga_tables[which(str_detect(lga_tables,
                                                regex("cases-by-lga",
                                                      ignore_case = TRUE)))]
  
  lga_cases_date <- as.data.frame(html_table(lga_cases_date))
  
  assign(paste0(lga, "_casesdate"),
         value = lga_cases_date)
  
}

cases_by_LGA_df <- data.frame()

lgacases_dataframes <- ls()[which(str_ends(ls(), "_casesdate"))]

for(lga in lgacases_dataframes) {
  
  lga_name <- str_remove_all(lga, "casesdate")
  lga_name <- str_replace_all(lga_name, pattern =
                                "\\-|\\_", replacement = " ")
  lga_name <- str_trim(lga_name)
  
  if(nrow(get(lga)) >0){
    
    lga_temp <- cbind(get(lga),
                      LGA = lga_name)
    
    cases_by_LGA_df <- rbind(cases_by_LGA_df,
                             lga_temp)
  }
}

## Polygon map from LGA borders for all VIC
#
# VIC_LGA_map <- ggplot() +
#   geom_polygon(data = combined_lga_df,
#                aes(x = long, y = lat,
#                    group = LGA),
#                fill = NA,
#                colour = "grey66",
#                size = 1)


cases_by_LGA_poly <- left_join(combined_lga_df,
                               cases_by_LGA_df,
                               by = "LGA")

cases_by_LGA_poly$DATE <- paste(cases_by_LGA_poly$DATE, "2020")

cases_by_LGA_poly$DATE <- dmy(cases_by_LGA_poly$DATE)

gg_cases_by_LGA_poly <- ggplot(cases_by_LGA_poly,
                               aes(x = long, y = lat,
                                   fill = ACTIVE, group = LGA)) +
  geom_polygon(colour = "grey66",
               size = 1) +
  scale_fill_gradient(low = "grey20", high = "red2",
                      na.value = "grey80") +
  labs(title = "Covid cases per LGA by day",
       subtitle = "{frame_manual}") +
  coord_equal() +
  transition_manual(DATE) +
  theme_void()

animate(gg_cases_by_LGA_poly,
        nframes = length(unique(cases_by_LGA_df$DATE)),
        fps = 4)

anim_save("cases by lga.gif")

ggplot(cases_by_LGA_poly,
       aes(x = long, y = lat,
           fill = ACTIVE, group = LGA)) +
  geom_polygon(colour = "grey85",
               size = 1) +
  scale_fill_gradient(low = "grey66", high = "red2",
                      na.value = "grey66") +
  labs(title = " Active Covid-19 Cases per LGA per day",
       subtitle = "Greater Melbourne Area\n ",
       caption = "Note: active cases and NOT new cases") +
  coord_equal(xlim = c(144.10, 145.75),
              ylim = c(-39.00, -37.25)) +
  transition_manual(DATE) +
  theme_void()

anim_save("cases by lga greater metro.gif",
          frames = length(unique(cases_by_LGA_poly$DATE)),
          fps = 1,
          end_pause = 10)

beep(2)