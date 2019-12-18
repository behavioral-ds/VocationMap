## this script is constructing a visualization for Study 2: proffesions clustering.
## The idea is to first compute the mean value per profession over the big10, cluster them into 20 clusters, apply tSNE to visualize them on a 2D plot.

require(readr)
require(cluster)
require(Rtsne)
require(fpc)
require(plotly)
require(stylo)
require(RColorBrewer)
require(dplyr)
source("scripts/utils.R")

myNames <- c("Handle", "Profession", "Openness", "Conscientousness", "Extraversion", "Agreeableness", "Emotional_Range", 
             "Conversation", "Openness to Change", "Hedonism", "Self-enhancement", "Self-transcendence")

twitters <- read_csv("data/all_users_data.csv.xz", col_names = T)
names(twitters) <- myNames
twitters$Profession <- as.factor(twitters$Profession)
sizes <- table(x = twitters$Profession)

#################### start building the professions profiles ##################
## first select professions with at leat 50 individuals
threshold <- 50; nclus <- 20
profs <- names(sizes)[sizes >= threshold]

## new style, using dplyr
df <- twitters %>%
  filter(Profession %in% profs) %>%
  group_by(Profession) %>%  
  add_tally() %>%
  summarize_at(vars(-group_cols(), -"Handle"), median, na.rm = T) %>%
  arrange(Profession)
df$Profession <- as.factor(as.character(df$Profession))

## write to file
write_csv(x = df, path = "data/profession-profiles.csv", quote_escape = F, col_names = T)
