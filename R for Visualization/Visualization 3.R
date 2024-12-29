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

#Barchart
ggplot(data=spotify_stream) +
  geom_bar(mapping=aes(x=artist_count, color=mode))

df1 <-spotify_stream %>%
  summarise(Spotify = sum(in_spotify_playlists, na.rm = TRUE),
            Apple = sum(in_apple_playlists, na.rm = TRUE),
            Deezer = sum(in_deezer_playlists, na.rm = TRUE)) %>%
  gather(key = "Streaming Service", value = "Total Count")

ggplot(df1, aes(x = `Streaming Service`, y = `Total Count`, fill = `Streaming Service`)) +
  geom_bar(stat = "identity") +
  theme_wsj() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        plot.title = element_text(size = 13, face = "bold"), 
        plot.subtitle = element_text(size = 12.5, face = "italic"),
        axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
        legend.title=element_text(size = 12)) +
  scale_fill_manual(values=c("Spotify" = "#023e8a", "Apple" = "#caf0f8", "Deezer" = "#48cae4"), 
                             guide=guide_legend(title="Streaming Service", label.position="bottom", nrow=1, keywidth=2)) +
  scale_x_discrete(name="Platfrom_playlist")+
  scale_y_continuous(name="Total Streams Count", limits=c(0,6000000), breaks=seq(0,6000000,1500000))+
  ggtitle("Total Songs in Playlist Across Different Streaming Service.", subtitle="Source: Most Streamed Spotify Songs 2023") +
  theme(legend.position="bottom")


#Scatterplot
ggplot(data=spotify_stream) +
  geom_point(mapping=aes(x=in_spotify_playlists, y=in_deezer_playlists, size=streams, na.rm = TRUE), alpha=1/2 , color="#0077b6") +
  annotate("text", label="Blinding Lights", x=43899,y=3421, color = "#f72585") +
  annotate("text", label="Get Luckey", x=52898, y = 9000, color = "#f72585") +
  geom_hline(yintercept=mean(spotify_stream$in_deezer_playlists, na.rm = TRUE), color="dark gray", linetype="dashed", size=1) +
  annotate("text", label="Mean of Apple Playlists", x=45000, y=mean(spotify_stream$in_deezer_playlists,na.rm = TRUE)+650) +
  geom_vline(xintercept=mean(spotify_stream$in_deezer_playlists,na.rm = TRUE), color="dark grey",linetype="dashed", size=1) +
  annotate("text", label="Mean of Spotify Playlists", x=mean(spotify_stream$in_deezer_playlists,na.rm = TRUE)+7000, y=10000)+
  theme_wsj() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        plot.title = element_text(size = 13, face = "bold"), 
        plot.subtitle = element_text(size = 12.5, face = "italic"),
        axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12),
        legend.title=element_text(size = 12))+
  scale_size_continuous(name="Number of Streams", guide=guide_legend(title="Number of Streams", label.position="bottom", nrow=1, keywidth=2)) +
  scale_x_continuous(name="Number of Spotify Playlists", limits = c(0, 55000),breaks = seq (0, 55000, 10000)) +
  scale_y_continuous(name="Number of Deezer Playlists", limits = c(0, 13000), breaks = seq (0, 13000, 2500)) +
  ggtitle("Song Performance across Spotify and Deezer Playlists.", subtitle="Source: Most Streamed Spotify Songs 2023") +
  theme(legend.position="bottom")



