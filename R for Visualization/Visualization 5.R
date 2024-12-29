#Upload file

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


#Top 10 artist with most streamed
top_artists <- spotify_stream %>%
  group_by(artist_name) %>%
  summarise(total_streams = sum(streams)) %>%
  arrange(desc(total_streams)) %>%
  head(10)

ggplot(top_artists, aes(x=reorder(artist_name, total_streams), y=total_streams, fill=total_streams)) +
  geom_bar(stat="identity") +
  coord_flip() +
  scale_fill_gradient(low = "lightblue", high = "steelblue", breaks = seq(0, 15000000000, 5000000000), 
                      labels = scales::comma) +
  theme_wsj() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        plot.title = element_text(size = 13, face = "bold"), 
        plot.subtitle = element_text(size = 12.5, face = "italic"),
        axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size=9.8),
        legend.title=element_text(size = 12)) +
  scale_x_discrete(name="Artist")+
  scale_y_continuous(name="Total Streams", limits=c(0,15000000000), breaks=seq(0,15000000000, 5000000000))+
  ggtitle("Top 10 Artists with Most Streams.", subtitle="Source: Most Streamed Spotify Songs 2023") +
  theme(legend.position="bottom")
  

#Top 10 artist with most times in playlist
top_artists_playlist <- spotify_stream %>%
  group_by(artist_name) %>%
  summarise(total_playlist = sum(in_spotify_playlists, in_apple_playlists, in_deezer_playlists)) %>%
  arrange(desc(total_playlist)) %>%
  head(10)

ggplot(top_artists_playlist, aes(x=reorder(artist_name, total_playlist), y=total_playlist, fill=total_playlist)) +
  geom_bar(stat="identity") +
  coord_flip() +
  scale_fill_gradient(low = "#4cc9f0", high = "#f72585", breaks = seq(0, 200000, 50000), 
                    labels = scales::comma) +
  theme_wsj() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        plot.title = element_text(size = 13, face = "bold"), 
        plot.subtitle = element_text(size = 12.5, face = "italic"),
        axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size=9.8),
        legend.title=element_text(size = 12)) +
  scale_x_discrete(name="Artist") +
  scale_y_continuous(name="Total Playlists", limits=c(0,160000), breaks=seq(0,160000, 50000))+
  ggtitle("Top 10 Artists with Most Playlists.", subtitle="Source: Most Streamed Spotify Songs 2023") +
  theme(legend.position="bottom")


# Create the scatter plot with a regression line

# Summarize the data
spotify_stream_summarized <- spotify_stream %>%
  group_by(artist_name) %>%
  summarize(total_song=sum(in_spotify_playlists, in_apple_playlists, in_deezer_playlists), total_streams=sum(streams))


ggplot(spotify_stream_summarized, aes(x = total_song, y = total_streams)) +
  geom_point(alpha=1/5) +
  geom_smooth(se=FALSE) +
  annotate("text", label="The Weeknd", x=163000,y=15100000000, color = "#f72585") +
  annotate("text", label="Taylor Swift", x=128000,y=14500000000, color = "#f72585") +
  annotate("text", label="Ed Sheeran", x=155000,y=13500000000, color = "#f72585") +
  annotate("text", label="Harry Styles", x=115000,y=11200000000, color = "#f72585")+
  annotate("text", label="Bad Bunny", x=50000,y=9700000000, color = "#f72585") +
  annotate("text", label="Olivia Rodrigo", x=44000,y=7100000000, color = "#f72585")+
  annotate("text", label="Eminem", x=111000,y=6000000000, color = "#f72585") +
  annotate("text", label="Acrtic Monkey", x=88000,y=5100000000, color = "#f72585") +
  theme_wsj() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        plot.title = element_text(size = 13, face = "bold"), 
        plot.subtitle = element_text(size = 12.5, face = "italic"),
        axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Total Playlists of Each Artist", limits = c(0, 165000),breaks = seq (0, 165000, 30000)) +
  scale_y_continuous(name="Total Streams of Each Artist", limits = c(0, 16000000000), breaks = seq (0, 16000000000, 3000000000)) +
  ggtitle("Artist Popularity: Total Streams vs Playlist Inclusions.", subtitle="Source: Most Streamed Spotify Songs 2023")

  
  

