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
  
  if(nrow(get(lga)) > 0){
    
    lga_temp <- cbind(get(lga),
                      LGA = lga_name)
    
    cases_by_LGA_df <- rbind(cases_by_LGA_df,
                             lga_temp)
  }
}

cases_by_LGA_poly <- left_join(combined_lga_df,
                               cases_by_LGA_df,
                               by = "LGA")

cases_by_LGA_poly$DATE <- paste(cases_by_LGA_poly$DATE, "2020")

cases_by_LGA_poly$DATE <- dmy(cases_by_LGA_poly$DATE)

cases_by_LGA_poly <- filter(cases_by_LGA_poly,
                            !is.na(DATE))

cases_by_LGA_poly$ACTIVE <- replace_na(cases_by_LGA_poly$ACTIVE, 0)

gg_Vic_cases_by_LGA <- ggplot(cases_by_LGA_poly,
                              aes(x = long, y = lat,
                                  fill = ACTIVE, group = LGA)) +
  geom_polygon(colour = "grey85",
               size = 1) +
  scale_fill_gradient(low = "grey75", high = "red2",
                      na.value = "grey75") +
  labs(title = paste(sep = "\n",
                     "Active Covid-19 Cases per LGA per day",
                     "{frame_time}"),
       subtitle = "Victorian Local Government Areas\n\n ",
       caption = paste(sep = "\n",
                       "Note: active cases only",
                       "",
                       "Cases data:  covidlive.com.au",
                       "LGA Shape files:  data.vic.gov.au",
                       "",
                       "github.com/BenCole6/Covid-Vic-AU")) +
  coord_equal() +
  transition_time(DATE) +
  theme_void() +
  theme(text = element_text(family = "Arial Nova",
                            size = 14),
        plot.title = element_text(face = "bold", size = 18))

plot_frames <- length(unique(cases_by_LGA_poly$DATE))
frames_ps <- 3

total_frames <- ((plot_frames %/% frames_ps) + 5) * frames_ps

end_frames <- total_frames - plot_frames

animate(gg_Vic_cases_by_LGA,
        height = 750, width = 750,
        nframes = total_frames,
        fps = frames_ps,
        end_pause = end_frames)

anim_save("Vic Cases by LGA.gif")

gg_MelbMetro_cases_by_LGA <- ggplot(cases_by_LGA_poly,
                                    aes(x = long, y = lat,
                                        fill = ACTIVE, group = LGA)) +
  geom_polygon(colour = "grey85",
               size = 0.75) +
  scale_fill_gradient("Active\nCases",
                      low = "grey75", high = "red2",
                      na.value = "grey75") +
  labs(title = paste(sep = "\n",
                     "Active Covid-19 Cases per LGA per day",
                     "{frame_time}"),
       subtitle = "Greater Melbourne Area\n\n ",
       caption = paste(sep = "\n",
                       "Note: active cases only",
                       "",
                       "Cases data:  covidlive.com.au",
                       "LGA Shape files:  data.vic.gov.au",
                       "",
                       "github.com/BenCole6/Covid-Vic-AU")) +
  coord_equal(xlim = c(144.10, 145.60),
              ylim = c(-38.75, -37.25)) +
  transition_time(DATE) +
  theme_void() +
  theme(text = element_text(family = "Arial Nova",
                            size = 14),
        plot.title = element_text(face = "bold", size = 18))

animate(gg_MelbMetro_cases_by_LGA,
        height = 750, width = 750,
        nframes = total_frames,
        fps = frames_ps,
        end_pause = end_frames)

anim_save("Cases by LGA Greater Metro.gif")

beep(8)
