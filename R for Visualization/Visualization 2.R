
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

ggplot(data=spotify_stream) +
  geom_histogram(mapping=aes(x=bpm), fill="#ade8f4", origin=0, binwidth=10)+
  geom_vline(aes(xintercept=mean(spotify_stream$bpm, na.rm = TRUE)), color='red', linetype='dashed', size=1) +
  annotate("text", label="Mean BPM", x=mean(spotify_stream$bpm, na.rm = TRUE)+ 12, y=7) +
  theme_wsj() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        plot.title = element_text(size = 13, face = "bold"), 
        plot.subtitle = element_text(size = 12.5, face = "italic"),
        axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="BPM", limits = c(60, 210),breaks = seq (60, 220, 20))+
  scale_y_continuous(name="Number of Songs", limits=c(0,140), breaks = seq (0,140,20))  +
  ggtitle("Distribution of BPM in Songs.", subtitle="Source: Most Streamed Spotify Songs 2023")

