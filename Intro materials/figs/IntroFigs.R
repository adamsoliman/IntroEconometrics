# load in packages, and clear environment cat("\f") with # rm(list = ls())
lapply(c("tidyverse", "readxl", "sf", "ggridges", "tidylog", "patchwork"), 
       library, character.only = TRUE)
theme_set(theme_bw())

setwd("/Users/adamsoliman/Documents/GitHub/Econometrics-Slides/Intro materials and keys/")

# major -------------------------------------------------------------------
ggplot(read_excel("qualtrics/class.xlsx") %>%
         mutate(count = 1) %>% group_by(MAJOR) %>% summarise(count = sum(count)) %>%
         mutate(share = count/36*100), aes(x = reorder(MAJOR, -count), y = count)) + 
  geom_bar(stat = "identity") +
  theme(legend.position = "bottom", 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    text = element_text(size = 14), 
    axis.text.x = element_text(angle = 25, hjust = 1), 
    axis.title.x = element_text(margin = margin(t = 10))) +
  labs(x = "Major", y = "Number of Students")
ggsave("figs/major_econ4050.pdf", height=7, width=9, dpi=600, device = cairo_pdf) 

# standing
ggplot(read_excel("qualtrics/class.xlsx") %>%
         mutate(count = 1) %>% group_by(CLASS) %>% summarise(count = sum(count)) %>%
         mutate(share = count / 36 * 100), aes(x = "", y = share, fill = CLASS)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme(legend.position = "bottom", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        text = element_text(size = 14), axis.text.x = element_blank()) +
  geom_text(aes(label = paste0(round(share, 1), "%")),
            position = position_stack(vjust = 0.5),
            color = "black", size = 7, fontface = "bold") +
  scale_fill_manual(values = c("#4C72B0", "#55A868", "#F39C12")) +  # Custom color palette
labs(title = "", x = NULL, y = NULL, fill = "Class Standing")
ggsave("figs/class_econ4050.pdf", height=6, width=6, dpi=600, device = cairo_pdf) 

# grade score -------------------------------------------------------------
ggplot(read_csv("qualtrics/dashboard-export-08-58-pm-2025-01-07.csv") %>%
         mutate(share = as.numeric(Count)/16*100) %>% select(1, 4) %>%
         rename(grade = `Expected Grade - What grade do you think you will get from this class?`) %>%
         add_row(grade = "C", share = 0) %>% add_row(grade = "D", share = 0) %>% 
         add_row(grade = "F", share = 0), aes(x = share, y = grade)) +
  geom_bar(stat = "identity") +
  theme(legend.position = "bottom", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        text = element_text(size = 14)) +
  labs(title = "", x = "Share", y = "Expected Grade")
ggsave("figs/grade_econ4050.pdf", height=6, width=8, dpi=600, device = cairo_pdf) 

# grad school choice ------------------------------------------------------
ggplot(read_csv("qualtrics/dashboard-export-08-57-pm-2025-01-07(1).csv") %>%
         mutate(share = as.numeric(Count)/16*100) %>% select(1, 4) %>%
         rename(plan = `Plan  - Do you plan to go to graduate school?`), aes(x=plan, y=share)) + 
  geom_bar(stat="identity") +
  labs(title = "Qualtrics Question: Do you plan to go to graduate school?",
       x = "", y = "Share") +
  theme(legend.position = "bottom", text = element_text(size = 16)) 
ggsave("figs/gradschool_econ4050.pdf", height=7, width=8, dpi=600, device = cairo_pdf)

# scatter option
ggplot(read_csv("qualtrics/Econ 4050 Spring 2025_January 7, 2025_14.32.csv"), aes(x = Plan, fill = `Expected Grade`)) +
  geom_bar(position = "fill") +  
  ylab("Proportion") +
  labs(x = "Grad School Plans", fill = "Expected Grade") +
  theme(legend.position = "right", 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        text = element_text(size = 14))
ggsave("figs/gradschool_grade_econ4050.pdf", height=6, width=8, dpi=600, device = cairo_pdf)

# state -------------------------------------------------------------------
state <- read_csv("qualtrics/dashboard-export-08-58-pm-2025-01-07(1).csv") %>%
  mutate(count = 1) %>% group_by(state) %>% summarise(count = sum(count))
  
allstate <- left_join(get_decennial(geography = "state", variables = "P003001", year = 2010, geometry = TRUE) %>%
                     rename(stateID = GEOID, pop2010 = value, state = NAME), state) %>% 
  filter(state != "Hawaii" & state != "Alaska" & state != "Puerto Rico") %>% st_as_sf() 

ggplot() + geom_sf(data = allstate, aes(fill = count)) +#as.factor(count))) + 
  theme(panel.grid = element_blank(), panel.background = element_blank(), legend.position = "bottom",
        axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
        text = element_text(size = 15)) + scale_fill_viridis(option = "viridis", na.value = "grey") + #scale_fill_discrete(na.value="gray90") +
  labs(fill = "Number of Students per State", 
       title = "Qualtrics Question: Which US state or country are you from?") 
ggsave("figs/statemap_econ4050.pdf", height=5, width=8, dpi=600, device = cairo_pdf)




