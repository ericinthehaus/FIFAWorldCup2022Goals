library(tidyverse)
library(treemapify)

raw_data <- read.csv("WCGoalsData.csv")

# source = "https://fbref.com/en/comps/1/shooting/World-Cup-Stats"

goal_scorers <- raw_data %>%
  filter(Gls>0) %>%
  select(
    Player, Squad, Gls
  ) 

separate(goal_scorers$Squad)

teams_list <- goal_scorers %>%
  select(Squad) %>%
  unique()

goal_scorers %>%
  ggplot( aes(x= Gls, y=reorder(Player, Gls, sum), group= Squad)) +
  geom_col(position = "stack")

theme_plt <- ggplot(mapping=  aes(area= Gls, fill= reorder(Player, Gls, sum), label= Player )  ) +
  geom_treemap() +
  theme(
    plot.background = element_rect(fill = "gray90") 
       ) +
  labs(
    title  = teams_list[1,],
    fill = "Player"
      ) +
  geom_treemap_text(fontface = "italic", colour = "white",
                    place = "bottom", grow = TRUE)


# treemaps ----

goal_scorers %>%
  filter(Squad== teams_list[1,]) %>%
  ggplot(mapping=  aes(area= Gls, fill= reorder(Player, Gls, sum), label= Player )  ) +
  geom_treemap() +
  theme(
    plot.background = element_rect(fill = "gray0"),
    plot.title = element_text(color= "gray90"),
    legend.position = "none"
  ) +
  labs(
    title  = teams_list[1,],
    fill = "Player"
  ) +
  geom_treemap_text(fontface = "italic", colour = "white",
                    place = "bottom", grow = TRUE)

# function to create treemap ----
make_goal_treemap <- function( num=1) {
  
  team <- teams_list[num,]
  goal_scorers %>%
    filter(Squad== team) %>%
    ggplot(mapping=  aes(area= Gls, fill= reorder(Player, Gls, sum), label= Player )  ) +
    geom_treemap() +
    theme(
      text = element_text(color= "gray90"),
      plot.background = element_rect(fill = "gray0"),
      plot.title = element_text(color= "gray90", size=18),
      legend.position = "none"
    ) +
    labs(
      title  = team,
  #    subtitle = 
      fill = "Player",
      caption = "Data from FBRef.com"
    ) +
    geom_treemap_text(fontface = "italic", colour = "white",
                      place = "bottom", grow = TRUE)
  
}

make_goal_treemap(num = 10)  

y <- 1:10
x <-1

for(x in y) {
  print( make_goal_treemap(num = x) )
} 

make_goal_treemap(num = 32)

