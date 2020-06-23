# IMD lava lamps for 2019 with new data

#install.packages("tidyverse")
library(tidyverse)
#library(svglite)

#### Get the IMD data ####

### IMD data at LSOA level
### This is from https://opendatacommunities.org/resource?uri=http%3A%2F%2Fopendatacommunities.org%2Fdata%2Fsocietal-wellbeing%2Fimd2019%2Findices

imd_data_lsoa <- readr::read_csv("http://opendatacommunities.org/downloads/cube-table?uri=http%3A%2F%2Fopendatacommunities.org%2Fdata%2Fsocietal-wellbeing%2Fimd2019%2Findices")

### IMD data at LA level rank of average rank
### This is from: https://opendatacommunities.org/resource?uri=http%3A%2F%2Fopendatacommunities.org%2Fdata%2Fsocietal-wellbeing%2Fimd2019%2Findicesbyla

imd_data_la <- readr::read_csv("http://opendatacommunities.org/downloads/cube-table?uri=http%3A%2F%2Fopendatacommunities.org%2Fdata%2Fsocietal-wellbeing%2Fimd2019%2Findicesbyla")


#### Political control for councils ####

### Downloaded spreadsheet which is a copy of this google sheet: https://docs.google.com/spreadsheets/d/1vkDwmBKL9P7m99oQXdieJzLFDna2XIhOFDWQ5gVoXq8/edit#gid=0
### This used to use the goooglesheets library to get data direct from the sheet but google changed the API

pol_control <- readr::read_csv("data_sources/pol_control.csv") %>%
  select('ONS_LA_code','2019')


#### Geographical lookups ####

lu_lsoa_la <- readr::read_csv("data_sources/lookup_lsoa_la_2019.csv")
#lu_lsoa_ward <- readr::read_csv("data_sources/lookup_lsoa_ward_2018.csv")
#lu_ward_pcon <- readr::read_csv("data_sources/lookup_ward_westminster_constituency_2018.csv")
lu_la_region <- readr::read_csv("data_sources/lookup_la_region_2019.csv")


#### Process the IMD data ####


imd_data_lsoa_la <- imd_data_lsoa %>%
  filter(Measurement == 'Rank') %>% 
  rename(domain = 'Indices of Deprivation') %>% 
  filter(domain == "a. Index of Multiple Deprivation (IMD)") %>%
  left_join(lu_lsoa_la, by = c('FeatureCode' = 'LSOA11CD')) %>%
  left_join(imd_data_la, by =  c('LAD19CD' = 'FeatureCode')) %>%
  rename(la_domain = 'Indices of Deprivation') %>%
  filter(la_domain == 'a. Index of Multiple Deprivation (IMD)') %>%
  filter(Measurement.y == 'Rank of average rank') %>%
  rename('Rank_of_avg_rank' = 'Value.y') %>%
  left_join(pol_control, by = c('LAD19CD' = 'ONS_LA_code')) %>%
  rename(pol_control_2019 = '2019') %>%
  left_join(lu_la_region, by = c('LAD19CD' = 'LAD19CD')) %>%
  mutate(vigintile = ntile(Value.x, 20)) %>%
  filter(LAD19CD != "E06000053") %>%
  filter(RGN19CD == 'E12000002') %>% 
  select('FeatureCode','LAD19CD','LAD19NM.x','vigintile','pol_control_2019','Rank_of_avg_rank')


#### Prepare the chart ####

## set the colors for the parties
palControl <- c(LAB = '#DC241f',
                LD = '#FAA61A',
                CON = '#0087DC',
                GREEN = '#69b044',
                NOC = '#aaaaaa',
                UKIP = '#70147A',
                OTHER = 'pink',
                NPC = '#333333',
                IND = 'pink')


## add the sort factor to allow sorting by the rank of ranks by local authority

imd_data_lsoa_la$LAD19NM_IMD <- reorder(imd_data_lsoa_la$LAD19NM.x,imd_data_lsoa_la$Rank_of_avg_rank) 


## Make unstyled plot to test

test_violin_plot <- ggplot(imd_data_lsoa_la, aes(LAD19NM_IMD, vigintile, fill = pol_control_2019)) + 
  facet_wrap(~ LAD19NM_IMD, strip.position = 'bottom', scales = 'free_x',ncol = 16) +
  geom_violin(color = '#f7f1d4') +
  scale_fill_manual(values = palControl)

test_violin_plot

## Make the lava lamp plot

eng_imd_pol_lava <- ggplot(imd_data_lsoa_la, aes(LAD19NM_IMD, vigintile, fill = pol_control_2019)) + 
  facet_wrap(~ LAD19NM_IMD, strip.position = 'bottom', scales = 'free_x',ncol = 16) +
  ggtitle("Deprivation Profiles") + 
  geom_violin(linetype = 0) +
  scale_fill_manual(values = palControl) + 
  theme(legend.position = 'none',
        plot.subtitle = element_text(color = '#b1b1b1', size = 18, face='italic'),
        plot.title = element_text(color = "#b1b1b1",size=36),
        strip.background = element_rect(fill = '#f7f1d4'),
        strip.text = element_text(color='#232323',size=8),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x=element_blank(),
        axis.title = element_text(color="#232323",size=12),
        plot.background=element_rect(fill="#f7f1d4"),
        panel.background = element_rect(fill="#f7f1d4"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

eng_imd_pol_lava

ggsave("deprivation_plot.png",eng_imd_pol_lava,width=20,height=20,units="cm")
#ggsave("deprivation_plot.svg",eng_imd_pol_lava,width=24,height=24.15,units="in")



