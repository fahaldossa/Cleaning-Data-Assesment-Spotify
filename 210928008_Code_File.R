#Assessment 2
#210928008

library(tidyr)
library(dplyr)

#FINAL CODE
spotifydf <- read.table("Spotify_Messy_210928008.txt", 
                        sep="\t",# notice how we set the separator
                        header=T,
                        na.strings = "NA") %>%
  pivot_longer(cols = c("rap", "rock", "latin", "r.b"), 
               names_to = "playlist_genre", 
               values_to = "playlist_subgenre")%>% 
  filter(!is.na(playlist_subgenre)) %>% 
  relocate(playlist_genre, .after = playlist_id) %>% 
  relocate(playlist_subgenre, .after = playlist_genre) %>% 
  mutate(playlist_genre = gsub("r.b", "r&b", playlist_genre)) %>%
  rename(track_name=ZZtrack_name89) %>%
  separate_wider_delim("danceability.energy",
                       names=c("danceability",
                               "energy"),
                       delim = '_') %>%
  mutate('track_album_release_date' = gsub('0[.]5-', 'X', track_album_release_date)) %>% 
  mutate('track_album_release_date' = gsub('X', '2020-', track_album_release_date)) %>%
  mutate(mode=as.numeric(gsub("Q", "",mode))) %>%
  mutate('track_artist' = gsub('Fourty', 'Four', track_artist)) %>%
  mutate('instrumentalness' = ifelse(instrumentalness > 1, NA, instrumentalness)) 

#ANNOTED STEP-BY-STEP CODE:
#1.Load the library and dataframe
library(tidyr)
library(dplyr)

#To read in a larger table I used 'read.table', which is what 'read.csv' does under the hood
#Because it is a txt not a csv,  we use sep = "\t"
spotifydf <- read.table("Spotify_Messy_210928008.txt", 
                        sep="\t",# notice how we set the separator
                        header=T,
                        na.strings = "NA")

#2. Making playlist_genre and playlist_subgenre columns
#Since the playlist genre's were in separate columns, pivot_longer function was used to combine them into one column named playlist_genre
#The sub genre's of rap where also put into a new column called playlist_subgenre 
spotifydf <- spotifydf %>% pivot_longer(cols = c("rap", "rock", "latin", "r.b"), names_to = "playlist_genre", values_to = "playlist_subgenre")

#3.Filtering 
#I utilized the filter() function in conjunction with !is.na() to remove rows where the 'playlist_subgenre' column contains missing values (NA)
spotifydf <- spotifydf %>% filter(!is.na(playlist_subgenre))

#4.I rearranged the positions of the 'playlist_genre' and 'playlist_subgenre' columns using the relocate() function, specifying the target position with the .after argument.
spotifydf <- spotifydf %>% 
  relocate(playlist_genre, .after = playlist_id) %>% #Here I used a pipe to rearrange the other column as well
  relocate(playlist_subgenre, .after = playlist_genre)

#5.Renaming r.b to r&b
#The mutate() function from the dplyr package is used to create a new column 'playlist_genre', while simultaneously employing the gsub() function to replace "r.b" with "r&b" within the playlist_genre column
spotifydf <- spotifydf %>% mutate(playlist_genre = gsub("r.b", "r&b", playlist_genre))

#6. Renaming the 'track_name' column
#I corrected the column names using the rename() function, employing the 'new_name = old_name' syntax
spotifydf <- spotifydf %>% rename(track_name=ZZtrack_name89)

#7.Splitting the 'danceability' column from 'energy' using the  'separate_wider_delim' function
spotifydf <- spotifydf %>% separate_wider_delim("danceability.energy",
                                 names=c("danceability",
                                         "energy"),
                                 delim = '_') #because the observations were divided by an underscore

#8. Fixing the date within the 'track_album_release_date' column 
#Mutate will make a new column with the output of a function that we apply to a column
#Since '.' was a special character in the typo '0.5-', gsub() was used to substitute it into 'X'
#gsub() was then used again by using pipe function to substitute the 'X' to its true year '2020-'
spotifydf <- spotifydf %>% 
  mutate('track_album_release_date' = gsub('0[.]5-', 'X', track_album_release_date)) %>% 
  mutate('track_album_release_date' = gsub('X', '2020-', track_album_release_date))

#9.Removing the extra character 'Q' from 'mode' column
#The mutate() function is utilized to generate a new column named 'mode' in conjunction with the gsub() function, which removes 'Q' from the 'mode' column. Additionally, as.numeric() is applied to convert the modified column to a numerical format. 
spotifydf <- spotifydf %>% mutate(mode=as.numeric(gsub("Q", "",mode)))

#10. Correcting the misspelled artists
#The mutate() function was used in conjunction with the gsub() function
spotifydf <- spotifydf %>% 
  mutate('track_artist' = gsub('Fourty', 'Four', track_artist))

#11. Fix the inconsistent values in 'instrumentalness' column
#The mutate() function creates a new 'instrumentalness' column, utilizing ifelse() to replace values exceeding 1 with NA
spotifydf <- spotifydf %>% mutate('instrumentalness' = ifelse(instrumentalness > 1, NA, instrumentalness))

#12.Write to file
#Here I used write.table() to save the dataframe spotifydf as a text file named "Spotify_Clean_210928008.txt"
write.table(spotifydf, "Spotify_Clean_210928008.txt",
                          sep = "\t",
                          col.names = T,
                          row.names = F,
                          quote = F)
