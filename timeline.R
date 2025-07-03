library(tidyverse)

library(timevis)

df <- readxl::read_excel("toya-materialy-spis.xlsx")

glimpse(df)
# three class dark
#1b9e77
#d95f02
#7570b3

d <- df %>% 
  filter(!is.na(id)) %>% 
  select(nr, data, tytuł, program) %>% 
  rename(id = nr, 
         start = data, 
         content = tytuł, 
         group = program) %>% 
  mutate(
         start = as_date(start), 
         style = case_when(
           group == "Nasze Sprawy" ~ "color: #1b9e77; background-color: white; border-color: red",
           group == "Wydarzenia" ~ "color: #d95f02; background-color: white;",
           .default = "color: #7570b3; background-color: white;"
         ))

timevis(d)

dd <- d %>% mutate(content = start)

timevis(dd)



library(vistime)

vistime(d, col.event = "content", col.start = "start", col.group = NA)

gg_vistime(d, )

# ggplot + ggrepel

library(ggrepel)

plants.tb <-
  data.frame(what = c("sowing", "first emergence", "last emergence", "Dualex",
                      "treatment start", "Dualex", "harvest"),
             when = ymd(c("2020-05-01", "2020-05-06", "2020-05-11", "2020-06-21",
                          "2020-06-22", "2020-06-29", "2020-06-30")),
             series = "Experiment 1")


ggplot(plants.tb, aes(x = when, y = series, label = what, colour = what)) +
  geom_line() +
  geom_point() +
  geom_text_repel(direction = "y",
                  point.padding = 0.5,
                  hjust = 0,
                  box.padding = 1,
                  seed = 123) +
  scale_x_date(name = "", date_breaks = "1 months", date_labels = "%d %B",
               expand = expansion(mult = c(0.12, 0.12))) +
  scale_y_discrete(name = "") +
  theme_minimal()
