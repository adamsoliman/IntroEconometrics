# load in packages, and clear environment cat("\f") with # rm(list = ls())
lapply(c("tidyverse", "sf", "ggridges", "tidylog", "patchwork"), 
       library, character.only = TRUE)
theme_set(theme_bw())

setwd("~/Documents/GitHub/IntroEconometrics/Intro materials/")

data <- read.csv("~/Documents/GitHub/IntroEconometrics/Intro materials/Econ 4050 Spring 2026_January 5, 2026_19.33.csv") %>%
  filter(UserLanguage == "EN") %>%
  mutate(major = ifelse(grepl("Econ", Major.), "Economics", Major.)) 

# major -------------------------------------------------------------------
ggplot(data %>% count(Major., name = "count") %>%
         mutate(share = 100 * count / sum(count)), aes(x = reorder(Major., count), y = count)) +
  geom_segment(aes(xend = reorder(Major., count), y = 0, yend = count)) +
  geom_point(size = 3) + coord_flip() + labs(x = "Major", y = "Number of Students") +
  theme(legend.position = "bottom", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        text = element_text(size = 16))
ggsave("figs/major_econ4050.pdf", height=7, width=9, dpi=600, device = cairo_pdf) 

# grade score -------------------------------------------------------------
ggplot(data %>% mutate(count = 1, total = n()) %>% group_by(Expected.Grade) %>% 
         summarise(count = sum(count), total = mean(total)) %>% 
         ungroup() %>% mutate(share = count/total*100) %>% 
         add_row(Expected.Grade = "C", share = 0) %>% add_row(Expected.Grade = "D", share = 0) %>% 
         add_row(Expected.Grade = "F", share = 0), aes(x = share, y = Expected.Grade)) +
  geom_bar(stat = "identity") +
  theme(legend.position = "bottom", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        text = element_text(size = 14)) +
  labs(title = "", x = "Share", y = "Expected Grade")
ggsave("figs/grade_econ4050.pdf", height=6, width=8, dpi=600, device = cairo_pdf) 

# grad school choice ------------------------------------------------------
df_heat <- data %>% count(Plan., Expected.Grade, name = "count") %>%
  group_by(Plan.) %>% mutate(share = count / sum(count))

ggplot(df_heat, aes(x = Plan., y = Expected.Grade, fill = share)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_viridis_c(option = "C",labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Grad School Plans", y = "Expected Grade", fill = "Share") +
  theme_minimal(base_size = 14)
ggsave("figs/gradschool_grade_econ4050.pdf", height=6, width=8, dpi=600, device = cairo_pdf)

# ggplot(data, aes(x = Plan., fill = Expected.Grade)) +
#   geom_bar(position = "dodge") +
#   labs(x = "Grad School Plans", y = "Count", fill = "Expected Grade")

# Class breakdown
df_class <- tibble::tibble(
  Class = c("Junior", "Senior"),
  count = c(16, 21)) %>%
  mutate(share = count / sum(count))

ggplot(df_class, aes(x = "", y = share, fill = Class)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(Class, "\n", count, " (", round(100 * share), "%)")), position = position_stack(vjust = 0.5),
            size = 4) +
  labs(fill = "Class") +
  theme_void(base_size = 14) +
  theme(legend.position = "none")
ggsave("figs/class_econ4050.pdf", height=6, width=8, dpi=600, device = cairo_pdf)

# state -------------------------------------------------------------------
# state <- data %>% mutate(count = 1) %>% group_by(Home.) %>% summarise(count = sum(count))
#   
# allstate <- left_join(get_decennial(geography = "state", variables = "P003001", year = 2010, geometry = TRUE) %>%
#                      rename(stateID = GEOID, pop2010 = value, state = NAME), state) %>% 
#   filter(state != "Hawaii" & state != "Alaska" & state != "Puerto Rico") %>% st_as_sf() 
# 
# ggplot() + geom_sf(data = allstate, aes(fill = count)) +#as.factor(count))) + 
#   theme(panel.grid = element_blank(), panel.background = element_blank(), legend.position = "bottom",
#         axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
#         text = element_text(size = 15)) + scale_fill_viridis(option = "viridis", na.value = "grey") + #scale_fill_discrete(na.value="gray90") +
#   labs(fill = "Number of Students per State", 
#        title = "Qualtrics Question: Which US state or country are you from?") 
# ggsave("figs/statemap_econ4050.pdf", height=5, width=8, dpi=600, device = cairo_pdf)




