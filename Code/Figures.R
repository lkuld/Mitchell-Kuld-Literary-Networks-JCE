rm(list=ls())
# Creates figures for the paper
# Note: Fig 7 is created in the script regressionsPaper.R

# load libraries

library(ggplot2)
library(dplyr)
library(stringr)
library(ggExtra)
load("Data/regressionData.RData")

# Figure 1

# Fig 1a Number of authors per year
cond <- data$age%in%18:65 & data$year %in% 1725:1975 & !str_detect(data$city,'Unknown')

ggplot(data[cond,], aes(x=year)) +
  geom_bar() +
  theme_minimal() +
  labs(x="Year", y="Number of authors") +
  theme(text = element_text(size=20))
ggsave("Output/Fig1a.eps", width=8, height=6)

# Fig 1b Number of authors in London per year

ggplot(data[cond,] %>% filter(london==1), aes(x=year)) +
  geom_bar() +
  theme_minimal() +
  labs(x="Year", y="Number of authors in London") +
  theme(text = element_text(size=20))
ggsave("Output/Fig1b.eps", width=8, height=6)


# Figure 2

# Fig 2a Mean publications per author per year

data_mean <- data[cond,] %>%
  group_by(year) %>%
  summarise(mean_pubs = mean(output, na.rm=TRUE))

ggplot(data_mean, aes(x=year, y=mean_pubs)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x="Year", y="Mean publications per author") +
  theme(text = element_text(size=20))
ggsave("Output/Fig2a.eps", width=8, height=6)

# Fig 2b Mean Kindler publications per author per year

data_mean_kindler <- data[cond,] %>%
  group_by(year) %>%
  summarise(mean_kindler = mean(kindler_pub, na.rm=TRUE))

ggplot(data_mean_kindler, aes(x=year, y=mean_kindler)) +
  geom_line(colour='grey70') +
  geom_point(colour='grey70') +
  geom_smooth(method='loess', colour='black', se=FALSE, span =.3) +
  theme_minimal() +
  labs(x="Year", y="Mean Kindler publications per author") +
  theme(text = element_text(size=20))
ggsave("Output/Fig2b.eps", width=8, height=6)

# Fig 3
# Fig 3a Publications per author by age

data_age <- data[cond,] %>%
  group_by(age) %>%
  summarise(mean_pubs_age = mean(output, na.rm=TRUE))
ggplot(data_age, aes(x=age, y=mean_pubs_age)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x="Age", y="Mean publications per author") +
  theme(text = element_text(size=20))
ggsave("Output/Fig3a.eps", width=8, height=6)

# Fig 3a Kindler Publications per author by age

data_age_kindler <- data[cond,] %>%
  group_by(age) %>%
  summarise(mean_kindler_age = mean(kindler_pub, na.rm=TRUE))
ggplot(data_age_kindler, aes(x=age, y=mean_kindler_age)) +
  geom_line(colour='grey70') +
  geom_point(colour='grey70') +
  geom_smooth(method='loess', colour='black', se=FALSE, span =.3) +
  theme_minimal() +
  labs(x="Age", y="Mean Kindler publications per author") +
  theme(text = element_text(size=20))
ggsave("Output/Fig3b.eps", width=8, height=6)


# Fig 4 
# Fig 4a Local connections per author by age

data_local <- data[cond,] %>%
  group_by(age) %>%
  summarise(mean_local = mean(location.number, na.rm=TRUE))

ggplot(data_local, aes(x=age, y=mean_local)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x="Age", y="Mean co-located authors") +
  theme(text = element_text(size=20))
ggsave("Output/Fig4a.eps", width=8, height=6)

# Fig 4b Personal connections per author by age
data_personal <- data[cond,] %>%
  group_by(age) %>%
  summarise(mean_personal = mean(buddies.number, na.rm=TRUE))

ggplot(data_personal, aes(x=age, y=mean_personal)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(x="Age", y="Mean social connections") +
  theme(text = element_text(size=20))
ggsave("Output/Fig4b.eps", width=8, height=6)


# Fig 5
cond <- data$age%in%18:65 & data$year %in% 1725:1975 & !str_detect(data$city,'Unknown')
ggplot(data[cond&data$buddies.number>0,], aes(x = buddies.number)) +
  geom_histogram(binwidth = 1, fill='grey20', color = "black", position = 'dodge') +
  labs( x = "Number of Connected Co-Located Authors", y = "Frequency", fill='') +
  theme_minimal() + theme(legend.position = "bottom") 
ggsave("Output/Fig5.eps", width=4, height=3)



## network visualisation ----------


data$circle <- data$buddies.number > 3 
cond <- data$age%in%18:65 & data$year %in% 1725:1975 & !str_detect(data$city,'Unknown')

# define network

networkData <- read.csv("./Data/Connections.csv", stringsAsFactors = FALSE,
                        header = FALSE)

networkData <- networkData[networkData$V1 %in% data$name[cond&data$circle],]

networkData2 <- networkData[,1:2]
for (i in 3:ncol(networkData)) {
  networkData2 <- rbind(networkData2, data.frame(V1 = networkData$V1, V2 = networkData[,i]))
}
networkData2 <- networkData2[networkData2$V2 != "",]


# plot network

g <- graph_from_data_frame(networkData2, directed = FALSE)

# deleted duplicate edges
g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE)


#plot using ggraph

library(ggraph)

ggraph(g, layout = "fr") +
  geom_edge_link(aes(edge_alpha = 0.1), edge_color = "grey50") +
  geom_node_point( color = "grey30", alpha = 0.5) +
  geom_node_text(aes(label = ifelse(name %in% networkData$V1, name, NA)), repel = TRUE, size = 3, color = "black",
                 max.overlaps=500) + 
  theme_void() +
  theme(legend.position = "none")

ggsave("Output/Network.png", width = 12, height = 16)

