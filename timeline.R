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




dg <- d
dg$series <- "TOYA"

dg <- dg %>% mutate(
  g = forcats::fct_other(group, keep = c("Nasze Sprawy", "Wydarzenia"), other_level = "Inne")
)

# for paper #### 

Sys.setlocale("LC_TIME", "pl_PL.UTF-8")

ggplot(data = dg, aes(x = start, y = series, label = id, colour = g)) +
  geom_line(colour = "gray") +
  geom_point(size = 3, alpha = 0.7) +
  geom_label_repel(direction = "y",
                  point.padding = 0.5,
                  hjust = 0.5,
                  box.padding = 0.1,
                  max.overlaps = 100,
                  min.segment.length = 0.1,
                  force = 10) +
  scale_x_date(name = "", date_breaks = "1 year", date_labels = "%b %Y", date_minor_breaks = "1 month"
               ) +
  scale_y_discrete(name = "") +
  scale_color_discrete(name = "Program:", ) +
  theme_minimal() +
  theme(legend.position = "bottom") 

ggsave("timeline-toya-te-butelki.png", width = 250, height = 150, units = "mm")


