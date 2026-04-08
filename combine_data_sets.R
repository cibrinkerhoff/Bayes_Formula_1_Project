# load packages and combine data sets
library(vroom)
library(tidyverse)

# need raceId and stop from pit_stops.csv
pitstop_df <- vroom("data_sets/pit_stops.csv") %>%
  select(raceId, stop)
head(pitstop_df)

# need raceId, year, and circuitId from races.csv
races_df <- vroom("data_sets/races.csv") %>%
  select(raceId, year, circuitId)
head(races_df)

# need circuitId, circuitRef, and name from circuits.csv
circuits_df <- vroom("data_sets/circuits.csv") %>%
  select(circuitId, circuitRef, name)
head(circuits_df)

# use pitstop_df as base, then join with races_df by raceId
master_df <- pitstop_df %>%
  left_join(races_df, by = "raceId")

# use master_df as base, then join with circuits_df by circuitId
master_df <- master_df %>%
  left_join(circuits_df, by = "circuitId")

track_type <- vroom("f1_track_types.csv")

final_df <- master_df %>%
  left_join(track_type, by="name") %>%
  select(stop,Type)

head(master_df)

unique(master_df)

head(final_df)

write.csv(final_df, "stop_type_df.csv", row.names = TRUE)
