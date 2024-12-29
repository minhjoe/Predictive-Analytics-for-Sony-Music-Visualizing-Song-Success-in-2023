#Upload file

install.packages("reshape2")
library(reshape2)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggthemes)


spotify_stream <- read.csv("F:/OneDrive - Macquarie University/Desktop/Uni/Cung/Master BA/SEM 1/Data Visual/Assignment 2 _ Visualization/Dataset.csv")
summary(spotify_stream)

spotify_stream <- spotify_stream %>%
  mutate(track_name=as.factor(track_name), artist_name=as.factor(artist_name),
         artist_count=as.numeric(artist_count), released_year=as.numeric(released_year),
         released_month=as.numeric(released_month), released_day=as.numeric(released_day),
         in_spotify_playlists=as.numeric(in_spotify_playlists), in_spotify_charts=as.numeric(in_spotify_charts),
         streams=as.numeric(streams), in_apple_playlists=as.numeric(in_apple_playlists), 
         in_apple_charts=as.numeric(in_apple_charts), in_deezer_playlists= as.numeric(in_deezer_playlists),
         in_deezer_charts=as.numeric(in_deezer_charts), in_shazam_charts=as.numeric(in_shazam_charts),
         bpm=as.numeric(bpm), key=as.factor(key),
         mode=as.factor(mode), danceability=as.numeric(danceability),
         valence=as.numeric(valence), energy=as.numeric(energy),
         acousticness=as.numeric(acousticness), instrumentalness=as.numeric(instrumentalness),
         liveness=as.numeric(liveness), speechiness=as.numeric(speechiness))


# Melt the dataframe to long format for ggplot
df1_melt <- melt(spotify_stream, id.vars = "track_name", measure.vars = c("danceability", "valence", "energy", "acousticness", "instrumentalness", "liveness", "speechiness"))

# Create the boxplot with manually specified colors
theme_set(theme_wsj())

ggplot(df1_melt, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  scale_fill_manual(values = c("danceability" = "#023e8a", "valence" = "#0077b6", "energy" = "#0096c7", "acousticness" = "#48cae4", "instrumentalness" = "#90e0ef", "liveness" = "#ade8f4", "speechiness" = "#caf0f8"), 
                    guide=guide_legend(title="Audio Type", label.position="bottom", nrow=1, keywidth=2)) +
  theme_wsj() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        plot.title = element_text(size = 13, face = "bold"), 
        plot.subtitle = element_text(size = 12.5, face = "italic"),
        axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size=9.8),
        legend.title=element_text(size = 12))+
  scale_x_discrete(name="Audio Features")+
  scale_y_continuous(name="% Value", limits=c(0,100), breaks = seq (0,100,20)) +
  ggtitle("Boxplot of Audio Features.", subtitle="Source: Most Streamed Spotify Songs 2023") +
  theme(legend.position="bottom")

#Top 10 song with most streamed 
spotify_stream_top10 <- spotify_stream %>% top_n(10, streams)
summary(spotify_stream_top10)

df2_melt <- melt(spotify_stream_top10, id.vars = "track_name", measure.vars = c("danceability", "valence", "energy", "acousticness", "instrumentalness", "liveness", "speechiness"))

ggplot(df2_melt, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  scale_fill_manual(values = c("danceability" = "#f72585", "valence" = "#b5179e", "energy" = "#7209b7", "acousticness" = "#480ca8", "instrumentalness" = "#3f37c9", "liveness" = "#4895ef", "speechiness" = "#4cc9f0"), 
                    guide=guide_legend(title="Audio Type", label.position="bottom", nrow=1, keywidth=2)) +
  theme_wsj() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        plot.title = element_text(size = 13, face = "bold"), 
        plot.subtitle = element_text(size = 12.5, face = "italic"),
        axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size=9.8),
        legend.title=element_text(size = 12))+
  scale_x_discrete(name="Audio Features")+
  scale_y_continuous(name="% Value", limits=c(0,100), breaks = seq (0,100,20)) +
  ggtitle("Audio Features of the top 10 Most Streamed Song.", subtitle="Source: Most Streamed Spotify Songs 2023") +
  theme(legend.position="bottom") 
