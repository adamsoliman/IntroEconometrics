# load in packages, and clear environment cat("\f") with # rm(list = ls())
lapply(c("tidyverse", "lubridate", "readxl", "sf", "tigris", "did","tidycensus", "ggridges", "viridis", "paletteer", "scales", "readxl", 
         "tidylog", "patchwork", "lfe", "stargazer", "haven", "ivreg", "fixest", "modelsummary", "nngeo", "bacondecomp", "did2s", "ggfixest"), # arcos
       library, character.only = TRUE)
theme_set(theme_bw())

setwd("/Users/adamsoliman/Documents/GitHub/Econometrics-Slides/Introduction Materials")

# major -------------------------------------------------------------------
major <- read_csv("qualtrics/dashboard-export-11-58-pm-2024-08-20.csv") %>%
  mutate(count = 1) %>% group_by(major) %>% summarise(count = sum(count)) %>%
  mutate(share = count/13*100)

ggplot(major, aes(x = major, y = share)) + 
  geom_bar(stat="identity") +
  theme(legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        text = element_text(size = 14)) +
  labs(x = "Major", y = "Share of Students", title = "Qualtrics Question: What is your major?") 
ggsave("figs/major_econ4050.pdf", height=6, width=8, dpi=600, device = cairo_pdf) 

# grad school choice ------------------------------------------------------
gradschool <- read_csv("qualtrics/dashboard-export-11-59-pm-2024-08-20.csv")

ggplot(gradschool, aes(x="", y=Count, fill=GradSchool)) + 
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) + theme_void() + 
  labs(fill = "Decision", title = "Qualtrics Question: Do you plan to go to graduate school?") +
  theme(legend.position = "bottom", text = element_text(size = 16)) 
ggsave("figs/gradschool_econ4050.pdf", height=7, width=8, dpi=600, device = cairo_pdf)

# grade score -------------------------------------------------------------
grade <- read_csv("qualtrics/dashboard-export-11-59-pm-2024-08-20(1).csv")

ggplot(grade, aes(x = ExpectedGrade, y = Count)) + 
  geom_bar(stat="identity") +
  theme(legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        text = element_text(size = 14)) +
  labs(x = "Expected Grade", y = "Number of Students", title = "Qualtrics Question: What grade do you think you will get from this class?") 
ggsave("figs/grade_econ4050.pdf", height=6, width=8, dpi=600, device = cairo_pdf) 

# state -------------------------------------------------------------------
state <- read_csv("qualtrics/dashboard-export-11-59-pm-2024-08-20(2).csv") %>%
  mutate(count = 1) %>% group_by(State_Country) %>% summarise(count = sum(count))
  
allstate <- left_join(get_decennial(geography = "state", variables = "P003001", year = 2010, geometry = TRUE) %>%
                     rename(stateID = GEOID, pop2010 = value, State_Country = NAME), state) %>% 
  filter(State_Country != "Hawaii" & State_Country != "Alaska" & State_Country != "Puerto Rico") %>% st_as_sf() 

ggplot() + geom_sf(data = allstate, aes(fill = as.factor(count))) + 
  theme(panel.grid = element_blank(), panel.background = element_blank(), legend.position = "bottom",
        axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
        text = element_text(size = 15)) + scale_fill_discrete(na.value="gray90") +
  labs(fill = "Number of Students per State", 
       title = "Qualtrics Question: Which US state or country are you from?") 
ggsave("figs/statemap_econ4050.pdf", height=5, width=8, dpi=600, device = cairo_pdf)




