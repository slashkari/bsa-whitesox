library(tidyverse)
library(xml2)
library(rvest)
library(scales)

fbs <- read_html("https://www.fueledbysports.com/mlb-payrolls/") %>% html_nodes("td") %>% html_text()

# clean up data
fbs <- fbs[-c(seq(1, 88, by = 3))]
fbs <- fbs[-c(seq(721, 1200))] 

#create data frame
fbs_df <- data.frame(team = fbs[seq(1, 719, by = 2)], payroll = fbs[seq(2, 720, by = 2)], stringsAsFactors = FALSE)

# streamline team names
fbs_df[[1]] <- ifelse(str_detect(fbs_df[[1]], "Arizona"), "Arizona Diamondbacks", fbs_df[[1]])
fbs_df[[1]] <- ifelse(str_detect(fbs_df[[1]], "Boston"), "Boston Red Sox", fbs_df[[1]])
fbs_df[[1]] <- ifelse(str_detect(fbs_df[[1]], "Yankees"), "New York Yankees", fbs_df[[1]])
fbs_df[[1]] <- ifelse(str_detect(fbs_df[[1]], "Washington"), "Washington Nationals", fbs_df[[1]])
fbs_df[[1]] <- ifelse(str_detect(fbs_df[[1]], "Atlanta"), "Atlanta Braves", fbs_df[[1]])
fbs_df[[1]] <- ifelse(str_detect(fbs_df[[1]], "Baltimore"), "Baltimore Orioles", fbs_df[[1]])
fbs_df[[1]] <- ifelse(str_detect(fbs_df[[1]], "Cincinnati"), "Cincinnati Reds", fbs_df[[1]])
fbs_df[[1]] <- ifelse(str_detect(fbs_df[[1]], "Cleveland"), "Cleveland Indians", fbs_df[[1]])
fbs_df[[1]] <- ifelse(str_detect(fbs_df[[1]], "Colorado"), "Colorado Rockies", fbs_df[[1]])
fbs_df[[1]] <- ifelse(str_detect(fbs_df[[1]], "Detroit"), "Detroit Tigers", fbs_df[[1]])
fbs_df[[1]] <- ifelse(str_detect(fbs_df[[1]], "Houston"), "Houston Astros", fbs_df[[1]])
fbs_df[[1]] <- ifelse(str_detect(fbs_df[[1]], "Kansas City"), "Kansas City Royals", fbs_df[[1]])
fbs_df[[1]] <- ifelse(str_detect(fbs_df[[1]], "Angels"), "Los Angeles Angels", fbs_df[[1]])
fbs_df[[1]] <- ifelse(str_detect(fbs_df[[1]], "Dodgers"), "Los Angeles Dodgers", fbs_df[[1]])
fbs_df[[1]] <- ifelse(str_detect(fbs_df[[1]], "Miami"), "Miami Marlins", fbs_df[[1]])
fbs_df[[1]] <- ifelse(str_detect(fbs_df[[1]], "Florida"), "Miami Marlins", fbs_df[[1]])
fbs_df[[1]] <- ifelse(str_detect(fbs_df[[1]], "Milwaukee"), "Milwaukee Brewers", fbs_df[[1]])
fbs_df[[1]] <- ifelse(str_detect(fbs_df[[1]], "Minnesota"), "Minnesota Twins", fbs_df[[1]])
fbs_df[[1]] <- ifelse(str_detect(fbs_df[[1]], "Mets"), "New York Mets", fbs_df[[1]])
fbs_df[[1]] <- ifelse(str_detect(fbs_df[[1]], "Oakland"), "Oakland Athletics", fbs_df[[1]])
fbs_df[[1]] <- ifelse(str_detect(fbs_df[[1]], "Philadelphia"), "Philadelphia Phillies", fbs_df[[1]])
fbs_df[[1]] <- ifelse(str_detect(fbs_df[[1]], "Pittsburgh"), "Pittsburgh Pirates", fbs_df[[1]])
fbs_df[[1]] <- ifelse(str_detect(fbs_df[[1]], "San Diego"), "San Diego Padres", fbs_df[[1]])
fbs_df[[1]] <- ifelse(str_detect(fbs_df[[1]], "San Francisco"), "San Franscisco Giants", fbs_df[[1]])
fbs_df[[1]] <- ifelse(str_detect(fbs_df[[1]], "Seattle"), "Seattle Mariners", fbs_df[[1]])
fbs_df[[1]] <- ifelse(str_detect(fbs_df[[1]], "St. Louis"), "St. Louis Cardinals", fbs_df[[1]])
fbs_df[[1]] <- ifelse(str_detect(fbs_df[[1]], "Tampa Bay"), "Tampa Bay Rays", fbs_df[[1]])
fbs_df[[1]] <- ifelse(str_detect(fbs_df[[1]], "Texas"), "Texas Rangers", fbs_df[[1]])
fbs_df[[1]] <- ifelse(str_detect(fbs_df[[1]], "Toronto"), "Toronto Blue Jays", fbs_df[[1]])
fbs_df[[1]] <- ifelse(str_detect(fbs_df[[1]], "Cubs"), "Chicago Cubs", fbs_df[[1]])
fbs_df[[1]] <- ifelse(str_detect(fbs_df[[1]], "White Sox"), "Chicago White Sox", fbs_df[[1]])
fbs_df[[1]][173] <- "Chicago Cubs"

#clean up payroll info

# trim unnecessary spaces 
fbs_df[[2]] <- str_trim(fbs_df[[2]])

# remove dollar sign and commas
fbs_df[[2]] <- str_replace(fbs_df[[2]], "\\$(\\d{1,3}),(\\d{3}),(\\d{3})", "\\1\\2\\3")

# convert payroll to integers
fbs_df[[2]] <- as.integer(fbs_df[[2]])

# sum payroll for years 2008-2019
team_full <- fbs_df[[1]][1:30]
team_abb <- c("BOS", "CHC", "NYY", "WAS", "HOU", "PHI", "LAA", "NYM", "LAD", "STL", "COL", "SF", "SEA", "MIL", "CIN", "MIN", "ATL", "ARI", "CLE", "TEX", "OAK", "CWS", "DET", "SD", "KC", "TB", "BAL", "TOR", "PIT", "MIA")
team_sum <- data.frame(team = team_abb, payroll = integer(30))

for(i in 1:30) {
  
  invisible(ifelse(fbs_df[[1]] == team_full[i], 
      team_sum[[2]][i] <- sum(fbs_df[fbs_df[[1]] == team_full[i], 2]), fbs_df[[2]]))
}

# add win percentage column 
win_perc <- c(1091/(1091+904), 1016/(1016+966), 1148/(1148+862),
      1016/(1016+962), 977/(977+1016), 999/(999+988),
      1025/(1025+935), 960/(960+999), 1139/(1139+887),
      1111/(1111+906), 920/(920+1034), 1017/(1017+980),
      904/(904+1040), 1006/(1006+965), 929/(929+1024),
      940/(940+1016), 1006/(1006+955), 948/(948+1005),
      1013/(1013+953), 1029/(1029+958), 993/(993+963),
      912/(912+1036), 959/(959+1021), 877/(877+1067),
      920/(920+1055), 1057/(1057+924), 893/(893+1064),
      965/(965+999), 924/(924+1025), 878/(878+1063))
team_sum[[3]] <- win_perc
names(team_sum)[3] <- "win_percentage"

# plot payroll vs. win percentage
ggplot(team_sum, aes(x = win_percentage, y = payroll)) + 
  geom_point(aes(color = payroll)) + 
  geom_text(aes(label = team), size = 2, vjust = 2) + 
  ggtitle("Payroll vs. Win Percentage: 2008-2019") + 
  labs(y = "Payroll (US Dollars)", x = "Win Percentage") +
  scale_color_continuous(name = "Payroll", labels = comma)

ggsave("payroll.png", device = "png")

#---------war distribution among top 5 teams (last three 90 win seasons)---------

five_year_war_dist <- data.frame("team" = c("NYY", "LAD", "BOS", "STL", "TB"), 
  "not_fa" = c(0.8379, 0.7754, 0.8019, 0.7934, 0.8659), 
  "fa" = c(0.1620, 0.2245, 0.1981, 0.2066, 0.1341))

tidydf <- pivot_longer(five_year_war_dist, cols = c("not_fa", "fa"),
   names_to = "variable", values_to = "proportion")

ggplot(tidydf, aes(x = team, y = proportion, fill = variable)) + 
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("WAR Distribution for Last Three 90-win Seasons") +
  scale_fill_discrete(name = "", labels = c("Free Agent", "Homegrown")) +
  ylab("Proportion of Overall Team WAR") + xlab("Team")

ggsave("warlast3.png", device = "png")

#----------barplot of major league extensions given to minor league players------

year <- factor(2010:2020)
frequency <- c(0, 0, 0, 0, 1, 0, 0, 0, 1, 2, 1)
dot <- data.frame("Frequency" = frequency, "Year" = year)

ggplot(dot, aes(x = year, y = frequency)) + geom_col(fill = "green4") + 
  theme_bw() + ggtitle("Contract Extensions Before MLB Debut")

ggsave("contracts.png", device = "png")
