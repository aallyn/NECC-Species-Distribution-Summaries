#####
## Prepping NOAA NMFS Northeast Fisheries Science Center Spring/Fall Botoom Trawl Data
#####

## Libraries and other necessities
library(here)
library(tidyverse)

# Make sure gmRi package is up to date!!
devtools::install_github("https://github.com/gulfofmaine/gmRi", force = TRUE)
library(gmRi)

## Start work

# load NFMS Trawl Survey

clean_survey<-gmri_survdat_prep(
  survdat_source ="most recent",
  box_location ="cloudstorage"
)
str(clean_survey)

# get just the unique tow information -- we can join this back in later on
survey_tows <- get_survdat_tows(clean_survey)
summary(survey_tows)

# for the species distribution analysis, we can use the make_survdat_occu function to aggregate the  `clean_survey` trawl data up to a species level so that for every unique tow we have a row for each species and then the total biomass and abundance (summed across sex/lengths) and presence/absence.
# load NECC Species List
## AA NOTE: Ah, I finally broke something :) This is always the fun part when you are trying to help someone out and then you get the dreaded "This doesn't work for me" message, which I am all too familiar with getting as my code nearly never works for other people out of the gate. In this case, what is going on here is that the `read.csv` function is basically going to try to find a file called "speciesList_inNECC.csv" in the working directory. This working directory is going to be different for every user. For example, if I run `getwd()`, R returns "/Users/aallyn/GitHub/NECC-Species-Distribution-Summaries", while I am sure if you run it, you'll see something different.

# So, how do we get around this? Two different options and we can start with the easiest that we can use with smaller datasets (less than 100 MB) that can be easily stored within our GitHub repo. When that is the case, we can add the file to our "Data" folder and then leverage the `here` function to read in the file. The `here` function only cares about relative paths and not absolute paths, so it always starts at the same level when you are within the R studio project. So, I make have this project in Users/aallyn/GitHub/DoingStuff/LaLa/NECC-Species-Distribution-Summaries and you might have the project in Users/clovas/GitHub/NECC-Species-Distribution-Summaries. It's smart enough to know that and build the path as necessary. Have a look at `?here` for some more explanation. For bigger files, we would store those on Box and then try to build a path to the file on Box using some of gmRi package functions.
# NECC<-read.csv("speciesList_inNECC.csv", header=TRUE)
NECC <- read.csv(here("Data", "speciesList_inNECC.csv"), header = TRUE) %>%
    rename(comname = Species_comnam) %>%
    mutate(comname = tolower(comname))
head(NECC)

#Now, we can get a tidy occupancy dataset with total biomass/abundance and presence/absence for each species in NECC. I changed this to lowercase "necc_fishes" just as one less thing to type.
necc_fishes<- make_survdat_occu(survdat_clean = clean_survey, species_keep = NECC$comname)
summary(necc_fishes)


# check this worked -- should have the same number of samples (tows) for each species and vice versa
check1 <- necc_fishes %>%
    group_by(comname) %>%
    summarize(n = length(unique(id)))
summary(check1)

check2 <- necc_fishes %>%
    group_by(id) %>%
    summarize(n = length(unique(comname)))
summary(check2)

# as you had in your code, we are also going to need information about each tow (lat, lon, season, etc). this is located in the survey_tows file, which we can join to our necc_fishes occupancy data
necc_fishes <- necc_fishes %>%
    left_join(., survey_tows) %>%
    rename("biomass_kg"="sum_biomass_kg")

# Now, we might think about saving this so that we don't have to run this bit of code every time we work. Let's put it in data and just see how big it is. Looks like it will be okay.
saveRDS(necc_fishes, file = here("Data", "necc_fishes_occu.rds"))
