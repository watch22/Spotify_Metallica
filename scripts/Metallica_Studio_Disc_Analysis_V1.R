pacman::p_load(tidyverse, spotifyr, lubridate, knitr, ggjoy, genius, tidytext, kableExtra, extrafont, ggthemes, ggrepel, magick, webshot)
webshot::install_phantomjs()

findoutlier <- function(x) {
  return(x < quantile(x, .25) - 1.5*IQR(x) | x > quantile(x, .75) + 1.5*IQR(x))
}

#Import Metallica song dataset
metallica_source <- read_csv(here::here("data", "metallica_studio_albums_sql exp.csv"))

#set fonts and colors
plotfont <- "Open Sans Condensed"
bgcol <-  "#171516" ##2B2326
panelcol <- "#F1F1EF"
textcol <- "#ECECEC"
albumcolours <- c("Kill 'Em All" = "#D03028",
                  "Ride The Lightning" = "#3561B0",
                  "Master Of Puppets" = "#F9AD41",
                  "...And Justice For All" = "#8fa36d",
                  "Metallica" = "#000000",
                  "Load" = "#EE9C77",
                  "Reload" = "#9C252D", ##F8C419
                  "St. Anger" = "#F26224", ##F26224 ##D51F37
                  "Death Magnetic" = "#5C4538",
                  "... Hardwired…To Self-Destruct" = "#3CA23A")

#get average tempo
avg_tempo <- mean(metallica$tempo)

#clean up data
metallica <- metallica_source %>% 
  #fix Hardwired track numbers
  mutate(track_number = case_when(grepl("Hardwired",album_name) == TRUE & row_number() %% 2 != 0 ~ track_number + 6,
                                  TRUE ~ track_number)) %>% 
  arrange(album_release_year, track_number) %>% 
  #album lengths/percentage
  group_by(album_name) %>% 
  mutate(running_length = round(cumsum(duration_min),1),
         trk_percent = running_length / max(running_length),
         name_flag = ifelse(trk_percent == 1, album_name, NA),
         #get relative tempo
         tempo_rel = round(tempo/avg_tempo,2)) %>% 
  #add outlier flags for boxplots
  mutate(outlier_energy = ifelse(findoutlier(energy), track_name, NA),
         outlier_valence = ifelse(findoutlier(valence), track_name, NA),
         outlier_danceability = ifelse(findoutlier(danceability), track_name, NA)) %>% 
  ungroup()

#factorise albums
metallica_avg$album_name <- as.factor(metallica_avg$album_name)
metallica$album_name <- as.factor(metallica$album_name)
metallica$album_name <- factor(metallica$album_name, levels(metallica$album_name)[c(4,9,6,1,7,5,8,10,2,3)])
met_albums <- as.factor(metallica$album_name)
met_albums <- factor(met_albums, levels(met_albums)[c(4,9,6,1,7,5,8,10,2,3)])

#get all average values
metallica_avg <- metallica %>% 
  group_by(album_name) %>% 
  summarise(energy_m = round(mean(energy),2),
            danceability_m = round(mean(danceability),2),
            valence_m = round(mean(valence),2),
            tempo_rel_m = round(mean(tempo_rel),2),
            tempo_m = round(mean(tempo),0),
            loudness_m = round(mean(loudness),2))

#Albums by length ----
metallica %>% 
  filter(trk_percent == 1) %>% 
  select(album_name, running_length) %>% 
  rename(Album = album_name,
         `Length (mins)` = running_length) %>% 
  arrange(desc(`Length (mins)`)) %>% 
  mutate(`Length (mins)` = cell_spec(`Length (mins)`, "html",
                            color = case_when(row_number() == 1 ~ textcol,
                                              `Length (mins)` > mean(`Length (mins)`) ~ "#F9AD41", 
                                              TRUE ~ textcol))) %>% 
  kable(format = "html", escape = F) %>% 
  kable_styling(full_width = F, 
                position = "center",
                html_font = plotfont,
                stripe_color = "ECECEC") %>% 
  column_spec(column = 1:2, background = "#2B2326", color = textcol, ) %>% 
  row_spec(row = 0, background = "#2B2326", color = textcol) %>% 
  row_spec(row = 1, background = "#8F1D16", color = textcol, italic = T, bold = T) %>% 
  footnote(general_title = "",
           general = "Average album length is 66.4 minutes
           Albums running over the average length are 
           highlighted in orange") 
  as_image(file = here::here("outputs", "metallica_albums_length.png"),
           width = 5)

#Albums by loudness ----
metallica_avg %>% 
  select(album_name, loudness_m) %>% 
  rename(Album = album_name,
         `Average Loudness (dB)` = loudness_m) %>% 
  arrange(desc(`Average Loudness (dB)`)) %>% 
  kable() %>% 
  kable_styling(full_width = F, 
                position = "center",
                html_font = plotfont,
                stripe_color = textcol)%>% 
  column_spec(column = 1:2, background = "#2B2326", color = textcol) %>% 
  row_spec(row = 0, background = "#2B2326", color = textcol) %>% 
  row_spec(row = 1, background = "#8F1D16", color = textcol, italic = T, bold = T)
    as_image(file = here::here("outputs", "metallica_albums_loudness.png"),
             width = 5)

#Plots ----
#Valence ----
#valence ridge plot
    
valencerangeplot <- metallica %>% 
      ggplot(aes(x = valence, y = factor(album_name, levels(met_albums_valence)), fill = album_name)) + 
  geom_density_ridges(scale = 1.5) + 
  scale_fill_manual(values = albumcolours) +
  scale_x_continuous(breaks = c(seq(0, 1, by = 0.1)),
                     labels = c(seq(0, 1, by = 0.1)),
                     expand = c(0, 0),
                     limits = c(0, 1)) +
      theme_dark(base_family = plotfont) +
      theme(plot.title = element_text(colour = textcol, size = 50, face = "bold"),
            plot.subtitle = element_text(colour = textcol, size = 35, face = "italic"),
            legend.position = "none",
            plot.background = element_rect(fill = bgcol, colour = textcol),
            axis.text = element_text(family = plotfont, colour = textcol, size = 25),
            axis.title = element_blank(),
            axis.line = element_line(colour = textcol),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank(),
            # panel.grid.major.x = element_line(linetype = "dotted", color = bgcol),
            # panel.grid.major.y = element_line(linetype = "dotted", color = bgcol),
            panel.grid.minor.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.background = element_rect(fill = panelcol),
            plot.caption = element_text(family = plotfont, colour = textcol, size = 25, hjust = 0)) + 
      labs(title = "Metallica",
           subtitle = "Albums by emotional range", 
           caption = "Valence is a measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track. Tracks with high valence sound \nmore positive (e.g. happy, cheerful, euphoric), while tracks with low valence sound more negative (e.g. sad, depressed, angry).")
 
valencerangeplot
    
# valence along album length
metallica %>% 
  ggplot(aes(x = trk_percent, y = valence, label = track_name)) +
  geom_smooth(se = F, aes(colour = album_name)) +
  scale_color_manual(values = albumcolours) +
  scale_x_continuous(breaks = c(min(metallica$trk_percent), 1),
                     labels = c("Start", "End")) +
  #facet_wrap(~ album_name, nrow = 5) + 
  theme_dark(base_family = plotfont) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.background = element_rect(fill = bgcol, colour = textcol),
        axis.text = element_text(family = plotfont, colour = textcol, size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_line(colour = textcol),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", color = bgcol),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "#F1F1EF")) + 
  ylab("Valence")

#valence boxplot
metallica %>% 
  ggplot(aes(x = album_name, y = valence, label = track_name))+
  geom_boxplot(aes(colour = album_name), 
               alpha = 0.85) +
  geom_text(aes(label = stringr::str_wrap(outlier_valence, 15), colour = album_name), 
            na.rm = TRUE, 
            hjust = 0.5,
            nudge_y = -0.02,
            angle = 0,
            size = 4,
            family = plotfont,
            check_overlap = TRUE) +
  scale_color_manual(values = albumcolours) +
  scale_y_continuous(breaks = seq(0.2, 1, by = 0.1)) +
  theme_dark() +
  coord_flip() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = bgcol, colour = textcol),
        axis.text = element_text(family = plotfont, colour = textcol, size = 12),
        axis.title.y = element_blank(),
        axis.title.x = element_text(family = plotfont, colour = textcol, size = 12),
        axis.line = element_line(colour = textcol),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", color = bgcol),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "#F1F1EF")) + 
  ylab("Valence")

#Energy ----
#energy ridge plot

metallica %>% ggplot(aes(x = energy, y = album_name, fill = album_name)) + 
  geom_density_ridges_gradient(scale = 2) + 
  scale_fill_manual(values = albumcolours) +
  scale_x_continuous(breaks = c(seq(0.2, 1, by = 0.1)),
                     labels = c(seq(0.2, 1, by = 0.1)),
                     expand = c(0, 0),
                     limits = c(0.25, 1.1)) +
  theme_dark(base_family = plotfont) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        plot.background = element_rect(fill = bgcol, colour = textcol),
        axis.text = element_text(family = plotfont, colour = textcol, size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_line(colour = textcol),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", color = bgcol),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "#F1F1EF"))

#energy vs. time
metallica %>% 
  ggplot(aes(x = trk_percent, y = energy, label = track_name)) +
  geom_smooth(se = F, aes(colour = album_name)) +
  scale_color_manual(values = albumcolours) +
  facet_wrap(~ album_name, nrow = 5) + 
  theme_dark(base_family = plotfont) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.background = element_rect(fill = bgcol, colour = textcol),
        axis.text = element_text(family = plotfont, colour = textcol, size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_line(colour = textcol),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linetype = "dotted", color = bgcol),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "#F1F1EF")) + 
  ylab("Energy")

#energy boxplot
metallica %>% 
ggplot(aes(x = album_name, y = energy, label = track_name))+
  geom_boxplot(aes(colour = album_name), 
               alpha = 0.85) +
  geom_text(aes(label = stringr::str_wrap(outlier_energy, 15), colour = album_name), 
            na.rm = TRUE, 
            hjust = 0.5,
            nudge_y = -0.02,
            angle = 0,
            size = 4,
            family = plotfont,
            check_overlap = TRUE) +
  scale_color_manual(values = albumcolours) +
  scale_y_continuous(breaks = seq(0.2, 1, by = 0.1)) +
  theme_dark() +
  coord_flip() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = bgcol, colour = textcol),
        axis.text = element_text(family = plotfont, colour = textcol, size = 12),
        axis.title.y = element_blank(),
        axis.title.x = element_text(family = plotfont, colour = textcol, size = 12),
        axis.line = element_line(colour = textcol),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", color = bgcol),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "#F1F1EF")) + 
  ylab("Energy")

#Using beeswarm instead of box plot
metallica %>%
  ggplot(aes(y = energy, x = album_name)) + 
  ggbeeswarm::geom_beeswarm(groupOnX = TRUE, aes(color = album_name), size = 1.5) + 
  stat_summary(fun = median, fun.max = median, fun.min = median, mapping = aes(group = 1), geom = "crossbar", width = 0.4) +
  guides(color = "none") +
  geom_text(aes(label = stringr::str_wrap(outlier_energy, 15), colour = album_name), 
            na.rm = TRUE, 
            hjust = 0.5,
            nudge_y = -0.05,
            angle = 0,
            size = 4,
            family = plotfont,
            check_overlap = TRUE) +
  scale_color_manual(values = albumcolours) +
  scale_y_continuous(breaks = seq(0.2, 1, by = 0.1)) +
  theme_dark(base_family = plotfont) +
  coord_flip() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = bgcol, colour = textcol),
        axis.text = element_text(colour = textcol, size = 12),
        axis.title.y = element_blank(),
        axis.title.x = element_text(colour = textcol, size = 12),
        axis.line = element_line(colour = textcol),
        panel.grid.major.x = element_line(linetype = "dotted", color = bgcol),
        panel.grid.major.y = element_line(linetype = "dotted", color = bgcol),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "#F1F1EF")) + 
  ylab("Energy")


#Danceability ----
#danceability ridge plot
metallica %>% ggplot(aes(x = danceability, y = met_albums, fill = ..x..)) + 
  geom_density_ridges_gradient(scale = 0.9) + 
  scale_fill_gradient(low = "white", high = "#8F1D16") + 
  theme_economist() +
  theme(panel.background = element_rect(fill = "#2B2326"), 
        plot.background = element_rect(fill = "#2B2326"),
        axis.text = element_text(colour = "#ECECEC", family = plotfont),
        axis.title.x = element_text(colour = "#ECECEC", family = plotfont),
        axis.title.y = element_blank()) +
  xlim(0,1) +
  theme(legend.position = "none")

# danceability vs. time
metallica %>% 
  ggplot(aes(x = trk_percent, y = danceability, label = track_name)) +
  geom_smooth(se = F, aes(colour = album_name)) +
  facet_wrap(~ album_name, nrow = 5) + 
  theme_clean() +
  theme(legend.position = "none")

#danceability boxplot
metallica %>% 
  ggplot(aes(x = album_name, y = danceability, label = track_name))+
  geom_boxplot(aes(colour = album_name), 
               alpha = 0.85) +
  geom_text(aes(label = stringr::str_wrap(outlier_danceability, 15), colour = album_name), 
            na.rm = TRUE, 
            hjust = 0.5,
            nudge_y = -0.02,
            angle = 0,
            size = 4,
            family = plotfont,
            check_overlap = TRUE) +
  scale_color_manual(values = albumcolours) +
  scale_y_continuous(breaks = seq(0.2, 1, by = 0.1)) +
  theme_dark() +
  coord_flip() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = bgcol, colour = textcol),
        axis.text = element_text(family = plotfont, colour = textcol, size = 12),
        axis.title.y = element_blank(),
        axis.title.x = element_text(family = plotfont, colour = textcol, size = 12),
        axis.line = element_line(colour = textcol),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", color = bgcol),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = "#F1F1EF")) + 
  ylab("Danceability")
#End Plots ----

#mean valence
metallica_avg %>% 
  select(album_name, valence_m) %>% 
  rename(Album = album_name,
         Valence = valence_m) %>% 
  arrange(Valence) %>% 
  kable() %>% 
  kable_styling(full_width = F, 
                position = "center",
                html_font = plotfont,
                stripe_color = "ECECEC") %>% 
  column_spec(column = 1:2, background = bgcol, color = textcol) %>% 
  row_spec(row = 0, background = bgcol, color = textcol) %>% 
  row_spec(row = 1, background = "#3561B0", color = textcol, italic = T, bold = T)

#valence is all over the place -- check danceability, energy and tempo
metallica_avg %>% 
  select(album_name, danceability_m, energy_m, tempo_m) %>% 
  rename(Album = album_name,
         Danceability = danceability_m,
         Energy = energy_m,
         Speed = tempo_m) %>% 
  # arrange(Energy) %>% 
  kable() %>% 
  kable_styling(full_width = T, 
                position = "center",
                html_font = plotfont,
                stripe_color = "ECECEC") %>% 
  column_spec(column = 1:4, background = "#2B2326", color = "#ECECEC") %>% 
  row_spec(row = 0, background = "#2B2326", color = "#ECECEC") %>% 
  row_spec(row = 1, background = "#8F1D16", color = "#ECECEC", italic = T, bold = T)

#get ranges ----
metallica %>% 
  group_by(album_name) %>% 
  summarise(Energy_Range = max(energy) - min(energy),
            Valence_Range = max(valence) - min(valence),
            Danceability_Range = max(danceability) - min(danceability)) %>% 
  arrange(Energy_Range) %>% 
  select(album_name,
         Energy_Range) %>% 
  rename(Album = album_name,
         `Energy (Range)` = Energy_Range) %>% 
  kable() %>% 
  kable_styling(full_width = F, 
                position = "center",
                html_font = plotfont,
                stripe_color = bgcol) %>% 
  column_spec(column = 1:2, background = "#2B2326", color = "#ECECEC") %>% 
  row_spec(row = 0, background = "#2B2326", color = "#ECECEC") %>% 
  row_spec(row = 1, background = "#8F1D16", color = "#ECECEC", italic = T, bold = T)
  
met_albums_valence <- pull(metallica %>% 
                             group_by(album_name) %>% 
                             summarise(Energy_Range = max(energy) - min(energy),
                                       Valence_Range = max(valence) - min(valence),
                                       Danceability_Range = max(danceability) - min(danceability)) %>% 
                             arrange(Valence_Range) %>% 
                             select(album_name))

#Heaviness ----
metallica <- metallica %>% 
  mutate(hb_index = round(danceability * 1/tempo_rel,3),
         hb_index_energy = round(danceability * energy * 1/tempo_rel,3))

metallica %>% 
  arrange(desc(hb_index_energy)) %>% 
    select(track_name, album_name, hb_index_energy) %>%
  slice(1:10) %>% 
    rename(Song = track_name,
           Album = album_name,
           `Heaviness (Variation)` = hb_index_energy) %>% 
    kable() %>% 
    kable_styling(full_width = F, 
                  position = "center",
                  html_font = plotfont,
                  stripe_color = bgcol) %>% 
    column_spec(column = 1:3, background = "#2B2326", color = "#ECECEC") %>% 
    row_spec(row = 0, background = "#2B2326", color = "#ECECEC") %>% 
    row_spec(row = 1, background = "#8F1D16", color = "#ECECEC", italic = T, bold = T)

met_heaviest <- pull(metallica %>% 
  arrange(desc(hb_index)) %>% 
  select(track_name) %>%
  slice(1:10))

met_thrashiest <- metallica %>% 
       arrange(hb_index) %>% 
       select(track_name) %>%
       slice(1:10)

met_heaviest_energy <- pull(metallica %>% 
                       arrange(desc(hb_index_energy)) %>% 
                       select(track_name) %>%
                       slice(1:10))

met_thrash_energy <- pull(metallica %>% 
  arrange(hb_index_energy) %>% 
  select(track_name) %>%
  slice(1:10))

#Heaviness Plot 1 (w/o Energy) -----
heavypointplot <- metallica %>% 
  mutate(hb_index = round((danceability * 1/tempo_rel)^2,3)) %>% 
  arrange(tempo_rel) %>% 
  ggplot(aes(x = tempo_rel, y = danceability, colour = album_name)) +
  annotate(geom = "line", 
           linetype = "dotted",
           colour = bgcol,
           alpha = 0.75, 
           x = c(median(metallica$tempo_rel),median(metallica$tempo_rel)),
           y = c(min(metallica$danceability),max(metallica$danceability))) +
  annotate(geom = "line", 
           linetype = "dotted",
           colour = bgcol,
           alpha = 0.75, 
           x = c(min(metallica$tempo_rel),max(metallica$tempo_rel)),
           y = c(median(metallica$danceability),median(metallica$danceability))) +
  geom_point(aes(colour = album_name)) +
  #scale_size_area(max_size = 4) +
  geom_text_repel(data = metallica %>% filter(track_name %in% c(met_heaviest, met_thrash, "Nothing Else Matters", "Master Of Puppets", "Blackened", "Dyers Eve", "For Whom The Bell Tolls", "The Thing That Should Not Be")), aes(label = stringr::str_wrap(track_name,15)),
            na.rm = TRUE, 
            hjust = 0,
            nudge_y = 0,
            angle = 0,
            size = 4,
            force = 10,
            force_pull = 10,
            alpha = 0.75,
            family = plotfont,
            show.legend = FALSE) +
  scale_color_manual(values = albumcolours) +
  guides(col = guide_legend(nrow = 2,
                            label.hjust = 0)) + 
  theme_dark(base_family = plotfont) +
  theme(plot.title = element_text(colour = textcol, size = 40, face = "bold"),
        plot.subtitle = element_text(colour = textcol, size = 25, face = "italic"),
        legend.position = "top",
        legend.background = element_rect(fill = bgcol, colour = bgcol),
        legend.key = element_rect(fill = bgcol, colour = bgcol),
        legend.title = element_blank(),
        legend.text = element_text(family = plotfont, colour = textcol, size = 15),
        plot.background = element_rect(fill = bgcol, colour = textcol),
        axis.text = element_text(family = plotfont, colour = textcol, size = 15),
        axis.title.y = element_text(family = plotfont, colour = textcol, size = 20),
        axis.title.x = element_text(family = plotfont, colour = textcol, size = 20),
        axis.line = element_line(colour = textcol),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = panelcol),
        plot.caption = element_text(family = plotfont, colour = textcol, size = 15, hjust = 0)) + 
  labs(title = "Metallica",
       subtitle = "Identifying their heaviest songs using Spotify's Audio Features",
       caption = "1. Relative bpm is based on the average bpm of all the songs in Metallica's 10 studio albums (126 bpm) \n2. Danceability describes how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability, \nbeat strength, and overall regularity",
       x = "Relative bpm" ,
       y = "Danceability")

heavypointplot

#Heaviness Plot 1 Zoom -----
heavypointplot_zoom <- metallica %>% 
  filter(tempo_rel < 1 & danceability > 0.446) %>% 
  arrange(tempo_rel) %>% 
  ggplot(aes(x = tempo_rel, y = danceability, colour = album_name)) +
  geom_point(aes(colour = album_name,
                 size = case_when(track_name %in% met_heaviest ~ 3,                                                                            TRUE ~ 1.5))) +
  geom_text_repel(data = metallica %>% filter(tempo_rel < 1 & danceability > 0.446), aes(label = stringr::str_wrap(track_name,15)),
                  size = 4,
                  na.rm = TRUE, 
                  hjust = 0,
                  nudge_y = 0,
                  angle = 0,
                  force = 10,
                  force_pull = 5,
                  alpha = 1,
                  family = plotfont,
                  show.legend = FALSE) +
  scale_color_manual(values = albumcolours) +
  guides(col = guide_legend(nrow = 2,
                            label.hjust = 0),
         size = "none") + 
  theme_dark(base_family = plotfont) +
  theme(plot.title = element_blank(),
        plot.subtitle = element_text(colour = textcol, size = 25, face = "italic"),
        legend.position = "top",
        legend.background = element_rect(fill = bgcol, colour = bgcol),
        legend.key = element_rect(fill = bgcol, colour = bgcol),
        legend.title = element_blank(),
        legend.text = element_text(family = plotfont, colour = textcol, size = 15),
        plot.background = element_rect(fill = bgcol, colour = textcol),
        axis.text = element_text(family = plotfont, colour = textcol, size = 15),
        axis.title.y = element_text(family = plotfont, colour = textcol, size = 20),
        axis.title.x = element_text(family = plotfont, colour = textcol, size = 20),
        axis.line = element_line(colour = textcol),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = panelcol),
        plot.caption = element_blank()) + 
  labs(
       subtitle = "Zooming in on the heaviest tracks according to the index",
       x = "Relative bpm" ,
       y = "Danceability")

heavypointplot_zoom
#Heaviness Plot 2 (w/ Energy) -----
metallica <- metallica %>% 
  mutate(dance_energy = round(danceability * energy,3))

heavypointplot_energy <- metallica %>% 
  arrange(tempo_rel) %>% 
  ggplot(aes(x = tempo_rel, y = dance_energy, colour = album_name)) +
  #ANNOTATION ----
annotate(geom = "line", 
         linetype = "dotted",
         colour = bgcol,
         alpha = 0.75, 
         x = c(median(metallica$tempo_rel),median(metallica$tempo_rel)),
         y = c(min(metallica$dance_energy),max(metallica$dance_energy))) +
  annotate(geom = "line", 
           linetype = "dotted",
           colour = bgcol,
           alpha = 0.75, 
           x = c(min(metallica$tempo_rel),max(metallica$tempo_rel)),
           y = c(median(metallica$dance_energy),median(metallica$dance_energy))) +
#END ANNOTATION -----
  geom_point(aes(colour = album_name)) +
  geom_text_repel(data = metallica %>% filter(track_name %in% c(met_heaviest_energy, met_thrash_energy, "Nothing Else Matters", "Master Of Puppets", "Blackened", "Dyers Eve", "For Whom The Bell Tolls", "The Thing That Should Not Be")), aes(label = stringr::str_wrap(track_name,15)),
                  na.rm = TRUE, 
                  hjust = 0,
                  nudge_y = 0,
                  angle = 0,
                  size = 4,
                  force = 5,
                  force_pull = 10,
                  alpha = 0.75,
                  family = plotfont,
                  show.legend = FALSE) +
  scale_color_manual(values = albumcolours) +
  guides(col = guide_legend(nrow = 2,
                            label.hjust = 0)) + 
  theme_dark(base_family = plotfont) +
  theme(plot.title = element_text(colour = textcol, size = 40, face = "bold"),
        plot.subtitle = element_text(colour = textcol, size = 25, face = "italic"),
        legend.position = "top",
        legend.background = element_rect(fill = bgcol, colour = bgcol),
        legend.key = element_rect(fill = bgcol, colour = bgcol),
        legend.title = element_blank(),
        legend.text = element_text(family = plotfont, colour = textcol, size = 15),
        plot.background = element_rect(fill = bgcol, colour = textcol),
        axis.text = element_text(family = plotfont, colour = textcol, size = 15),
        axis.title.y = element_text(family = plotfont, colour = textcol, size = 20),
        axis.title.x = element_text(family = plotfont, colour = textcol, size = 20),
        axis.line = element_line(colour = textcol),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.background = element_rect(fill = panelcol),
        plot.caption = element_text(family = plotfont, colour = textcol, size = 14, hjust = 0)) + 
  labs(title = "Metallica",
       subtitle = "A different way of measuring heaviness", 
       caption = "Energy is a measure from 0.0 to 1.0 and represents a perceptual measure of intensity and activity. Perceptual features contributing to this attribute include \ndynamic range, perceived loudness, timbre, onset rate, and general entropy.",
       x = "Relative bpm" ,
       y = "Danceability x Energy")

heavypointplot_energy


#Heaviness by Album ----
#Using HB_Index
metallica %>% 
  group_by(album_name) %>% 
  summarise(Heaviness = round(mean(hb_index),2)) %>% 
  arrange(desc(Heaviness)) %>% 
  rename(Album = album_name) %>% 
  kable() %>% 
  kable_styling(full_width = F, 
                position = "center",
                html_font = plotfont,
                stripe_color = bgcol) %>% 
  column_spec(column = 1:2, background = "#2B2326", color = "#ECECEC") %>% 
  row_spec(row = 0, background = "#2B2326", color = "#ECECEC") %>% 
  row_spec(row = 1, background = "#8F1D16", color = "#ECECEC", italic = T, bold = T)

#Using HB_Index (with Energy)
metallica %>% 
  group_by(album_name) %>% 
  summarise(Heaviness = round(mean(hb_index_energy),2)) %>% 
  arrange(desc(Heaviness)) %>% 
  rename(Album = album_name) %>% 
  kable() %>% 
  kable_styling(full_width = F, 
                position = "center",
                html_font = plotfont,
                stripe_color = bgcol) %>% 
  column_spec(column = 1:2, background = "#2B2326", color = "#ECECEC") %>% 
  row_spec(row = 0, background = "#2B2326", color = "#ECECEC") %>% 
  row_spec(row = 1, background = "#8F1D16", color = "#ECECEC", italic = T, bold = T)

#EXPORT PLOTS -----
ggsave(plot = heavypointplot,
       filename = here::here("outputs/Metallica_Heavy_Breakdown_HiRes_V1.png"),
       dpi = 600, scale = 2, width = 15, height = 10, units = "cm")

ggsave(plot = heavypointplot_zoom,
       filename = here::here("outputs/Metallica_Heavy_Breakdown_Zoom_HiRes_V1.png"),
       dpi = 600, scale = 2, width = 15, height = 10, units = "cm")

ggsave(plot = heavypointplot_energy,
       filename = here::here("outputs/Metallica_Heavy_BreakdownHiRes_V2.png"),
       dpi = 600, scale = 2, width = 15, height = 10, units = "cm")

ggsave(plot = valencerangeplot,
       filename = here::here("outputs/Metallica_Emotional Range_HiRes.png"),
       height = 12, width = 21)

 