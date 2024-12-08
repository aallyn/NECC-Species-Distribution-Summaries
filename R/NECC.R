install.packages("gmRi")
library(gmRi)
library(dplyr)
library(here)
#load NFMS Trawl Survey

clean_survey<-gmri_survdat_prep(
  survdat_source ="most recent",
  box_location ="cloudstorage"
)
str(clean_survey)

# load NECC Species List

# NECC<-read.csv("speciesList_inNECC.csv", header=TRUE)
NECC <- read.csv(here("Data", "speciesList_inNECC.csv"))
NECC<-NECC %>%
  rename(comname = Species_comnam)
NECC <- tolower(NECC$comname)
head(NECC)

#Filter trawl by species
NECC_fishes<-clean_survey %>% 
  select("comname", "est_year", "biomass_kg", "decdeg_beglat",  "decdeg_beglon", "season") %>%
  filter(comname %in% NECC)
summary(NECC_fishes)


# center of latitude over time####

dogfish_fall <- NECC_fishes %>%
  filter(comname == "smooth dogfish") %>%
  filter(season == "Fall") %>%
  group_by(est_year) %>%
  summarize(weightedLat = weighted.mean(x = decdeg_beglat, w = biomass_kg), weightedLon = weighted.mean(x = decdeg_beglon, w = biomass_kg))

# This doesn't work for me...I think maybe it would need a `data = ` argument? Or be strung together to the line above with the pipe (%>%) operator.
# ggplot(aes(x=est_year, y=weightedLat))+
#   geom_point()+
#   geom_smooth(method="lm")

#more center of lat practice
NECC_fishes%>%
  filter(comname == "atlantic cod") %>%
  filter(season == "Fall") %>%
  group_by(est_year)%>%
  summarize(weightedLat=weighted.mean(x=decdeg_beglat, w=biomass_kg), weightedLon=weighted.mean(x=decdeg_beglon, w=biomass_kg)) %>%
  ggplot(aes(x=est_year, y=weightedLat))+
  geom_point()+
  geom_smooth(method="lm") 

#center of lat/lon for all species

All_spp_lat<-NECC_fishes %>%
  group_by(comname, est_year, season)%>%
  summarize(weightedLat=weighted.mean(x=decdeg_beglat, w=biomass_kg), 
            weightedLon=weighted.mean(x=decdeg_beglon, w=biomass_kg))
  

#review linear regression models

dog_lm<-lm(weightedLat~est_year, dogfish_fall)
summary(dog_lm)

NECC_fishes%>% 
  filter(comname == "atlantic cod") %>%
  filter(season == "Fall") %>%
  group_by(est_year)%>%
  summarize(weightedLat=weighted.mean(x=decdeg_beglat, w=biomass_kg), weightedLon=weighted.mean(x=decdeg_beglon, w=biomass_kg)) %>%
  ggplot(aes(x=est_year, y=weightedLat))+
  geom_point()+
  geom_smooth(method="lm") 

NECC_fishes%>%
  filter(comname == "atlantic cod") %>%
  filter(season == "Spring") %>%
  group_by(est_year)%>%
  summarize(weightedLat=weighted.mean(x=decdeg_beglat, w=biomass_kg), weightedLon=weighted.mean(x=decdeg_beglon, w=biomass_kg)) %>%
  ggplot(aes(x=est_year, y=weightedLat))+
  geom_point()+
  geom_smooth(method="lm") 

All_spp_lat%>%
  filter(comname =="alewife", season == "Fall")%>%
  ggplot(aes(x=est_year, y=weightedLat)) +
  geom_point()+
  geom_smooth(method="lm")

##functions & looping####
library(tidyverse)
library(tidyr)

All_spp_lat<-NECC_fishes %>%
  group_by(comname, season, est_year)%>%
  summarize(weightedLat=weighted.mean(x=decdeg_beglat, w=biomass_kg), 
            weightedLon=weighted.mean(x=decdeg_beglon, w=biomass_kg))%>%
  nest()

All_spp_lat$data[[1]]

species_mod<-function(df){
  lm(weightedLat~est_year, data=df)
}

All_spp_lat<-All_spp_lat %>%
  mutate(mod=map(data, species_mod))%>%
  group_by(season)

# extract lm outputs
All_spp_lat <- All_spp_lat %>%
  mutate(
    tidy = map(mod, broom::tidy),
    glance = map(mod, broom::glance),
    rsq = glance %>% map_dbl("r.squared"),
    augment = map(mod, broom::augment)
  )

# extract slope (tbd)
# AA here: I am guessing there are quite a few ways of doing this. Here's one option
All_spp_lat <- All_spp_lat %>%
  mutate(
    tidy = map(mod, broom::tidy),
    glance = map(mod, broom::glance),
    rsq = glance %>% map_dbl("r.squared"),
    slope = tidy %>% map_dbl(function(x) x$estimate[2]),
    augment = map(mod, broom::augment)
  )

# AA again: why does this work and what is it doing? We are grabbing the "tidy" column of `All_spp_lat` and then just getting the second value in the estimate column. What else is in there?
All_spp_lat$tidy[[1]]

# AA: So, if we also wanted the std_error, we could add in slope_se = tidy %>% map_dbl(function(x) x$std.error[2]). Good to know.
##unnest to plot by season & species
#subset by season, plot each species over time 

#Fall plots
All_spp_lat%>%
  unnest(data)%>%
  select(comname, season, est_year, weightedLat)%>%
  filter(season == "Fall")%>%
  ggplot(aes(est_year, weightedLat))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~season+comname)
  
#Spring plots
All_spp_lat%>%
  unnest(data)%>%
  select(comname, season, est_year, weightedLat)%>%
  filter(season == "Spring")%>%
  ggplot(aes(est_year, weightedLat))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~season+comname)

##loop to generate individual plots (incomplete)
library(gridExtra)
plotlist<-list()

for (i in length(All_spp_lat)) {
  loop_df <- All_spp_lat %>%
    unnest(data) %>%
    select(comname, season, est_year, weightedLat) %>%
    group_by(comname)

  plotlist[[i]] <- ggplot(loop_df, aes(est_year, weightedLat)) +
    geom_point() +
    geom_smooth(method = "lm")
}

# AA here: The above wasn't working for me and I know it says incomplete. Just for a few thoughts/ideas. On the loop set up, the first thing that jumps out is the "i in length(All_spp_lat)". With loops, I tend to add a `print(i)` line (or whatever I am looping over) inside the loop to make sure it is progressing through as I expect. When we do that here, instead of 1, 2, 3, 4, 5, 6, 7, 8, 9 (which is the length of All_spp_lat or the number of columns) I just get 9.
for (i in length(All_spp_lat)) {
  print(i)
  loop_df <- All_spp_lat %>%
    unnest(data) %>%
    select(comname, season, est_year, weightedLat) %>%
    group_by(comname)

  plotlist[[i]] <- ggplot(loop_df, aes(est_year, weightedLat)) +
    geom_point() +
    geom_smooth(method = "lm")
}

# AA again: We can also see this in the `plotlist` object as only the 9th element is populated. 
##For why this is happening, the "for" bit is saying "for i in 9" and we want it to say something like "for i in 1, 2, 3, 4, 5, 6, 7, 8, 9." 
###To get that, we could write `for(i in 1:length(All_spp_lat)` and confirm that works by typing `1:length(All_spp_lat)` in the console. 

1:length(All_spp_lat)
for (i in 1:length(All_spp_lat)) {
  print(i)
  loop_df <- All_spp_lat %>%
    unnest(data) %>%
    select(comname, season, est_year, weightedLat) %>%
    group_by(comname)

  plotlist[[i]] <- ggplot(loop_df, aes(est_year, weightedLat)) +
    geom_point() +
    geom_smooth(method = "lm")
}

list1 = plotlist[c(1:8)]
do.call(grid.arrange, c(list1, ncol = 4))

# AA again: That gets us the plots, though I am guessing it is pretty clear that they are all identical. 
##I'm not entirely sure what this loop is for, though let's say we wanted to do this to get a plot for each species/season, with 2 species per layout? 
###The first thing we would want to change is that we want to loop over rows and not the columns in `All_spp_lat`. 
##We are also going to want to set up the plotlist storage ahead of time just to help with speed. 
#A bunch of ways to do this, here's one option:

n_spp_seas <- nrow(All_spp_lat)
plotlist <- vector("list", length = n_spp_seas)
names(plotlist) <- paste(All_spp_lat$comname, All_spp_lat$season, sep = "_")
str(plotlist) # 81 list elements, which is what we want

# AA: now, we can run the loop with one small change. We are going to want to grab the species/season data from All_spp_lat instead of using everything. I also made an edit so that the species_season is printed to the title of the plot. Again, not entirely sure this is what you are after, though hopefully helpful either way :)
for (i in 1:n_spp_seas) {
  print(i)
  loop_df <- All_spp_lat[i,] %>%
    unnest(data) %>%
    select(comname, season, est_year, weightedLat) %>%
    group_by(comname)

  plotlist[[i]] <- ggplot(loop_df, aes(est_year, weightedLat)) +
    geom_point() +
    ggtitle(names(plotlist)[i]) +
    geom_smooth(method = "lm")
}

# Plot first four species 
list1 = plotlist[c(1:8)]
do.call(grid.arrange, c(list1, ncol = 4))

#new functions
species_lat_mod<-function(df){
  lm(COGy~est_year, data=df)
}

species_lon_mod<-function(df){
  lm(COGx~est_year, data=df)
}

species_biomass_mod<-function(df){
  lm(biomass_kg~est_year, data=df)
}

cog_mapped<- function(df) {
  COGravity(x=df$decdeg_beglon, y=df$decdeg_beglat, z=NULL, wt=df$biomass_kg)
}

#center of gravity loopz
library(here)
source(here("R", "helper_funcs.R"))
#create some points
x = seq(154,110,length=25) # your longitudes
y = seq(-10,-54,length=25) # your latitudes
z = NULL
wt = runif(25) # your biomasses
#calculate the Centre of Gravity for these points

test<-NECC_fishes%>%
  group_by(comname,est_year,season)%>%
  summarise(COG=COGravity(x=decdeg_beglon, y=decdeg_beglat, z=NULL, wt=biomass_kg))%>%
  unnest_longer(COG)%>%
  pivot_wider(names_from=COG_id, values_from = COG)%>%
  select("comname","est_year", "season", "COGx", "COGy")%>%
  relocate(COGx, .after=COGy)

# AA: NICE!!!! 


test <- test %>%
  group_by(comname, season) %>%
  left_join(unique(All_species %>%
    select("weightedLat", "weightedLon")))

# A few things based on the above bit and trying to bring over other info from "All_spp_lat". I think we will just want to be explicit when doing the joins so that nothing weird happens and we can do that with specifying "by" and the column names. Last thing, in the select, may want to just walways keep comname/season/year handy?
test <- test %>%
  group_by(comname, season) %>%
  left_join(All_spp_lat, by = c("comname", "season"))
test

write.csv(test,"~\\Data\\Center of Gravity.csv", row.names=TRUE)

##linear models, COG
nestedData<-test%>%
  group_by(comname, season)%>%
  nest()

nestedData<-nestedData%>%
  mutate(centerLat=map(data, species_lat_mod))%>%
  mutate(centerLon=map(data, species_lon_mod))%>%
  group_by(season)

##fall plots
nestedData%>%
  unnest(data)%>%
  select(comname, season, est_year, COGy)%>%
  filter(season == "Fall")%>%
  ggplot(aes(est_year, COGy))+
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("1970-2019")+
  ylab("Center of Latitude")+ 
  ggtitle("Fall Trends")+
  theme(
    axis.text.x = element_blank(),
    axis.ticks = element_blank())+
  facet_wrap(~comname)

##spring plots
nestedData%>%
  unnest(data)%>%
  select(comname, season, est_year, COGy)%>%
  filter(season == "Spring")%>%
  ggplot(aes(est_year, COGy))+
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("1970-2019")+
  ylab("Center of Latitude")+ 
  ggtitle("Spring Trends")+
  theme(
    axis.text.x = element_blank(),
    axis.ticks = element_blank())+
  facet_wrap(~comname)

##center of gravity without season
COG_wo_season<-NECC_fishes%>%
  group_by(comname,est_year)%>%
  summarise(COG=COGravity(x=decdeg_beglon, y=decdeg_beglat, z=NULL, wt=biomass_kg))%>%
  unnest_longer(COG)%>%
  pivot_wider(names_from=COG_id, values_from = COG)%>%
  select("comname","est_year", "COGx", "COGy")%>%
  relocate(COGx, .after=COGy)

COG_wo_season<-COG_wo_season%>%
  group_by(comname)%>%
  nest()%>%
  mutate(centerLat=map(data, species_lat_mod))%>%
  mutate(centerLon=map(data, species_lon_mod))

#center of gravity with season
COG_w_season<-nestedData

##extract coefficients from nested models
COG_wo_season<-COG_wo_season%>%
  mutate(tidyLat=map(centerLat,broom::tidy),
         tidyLon=map(centerLon,broom::tidy),
         slopeLat = tidyLat %>% map_dbl(function(x) x$estimate[2]),
         slopeLon = tidyLon %>% map_dbl(function(x) x$estimate[2]))

COG_w_season<-COG_w_season%>%
  mutate(tidyLat=map(centerLat,broom::tidy),
         tidyLon=map(centerLon,broom::tidy),
         slopeLat = tidyLat %>% map_dbl(function(x) x$estimate[2]),
         slopeLon = tidyLon %>% map_dbl(function(x) x$estimate[2]))

##tidy dataset 
clean_w_season<-COG_w_season%>%
  unnest(data)%>%
  select("comname","est_year","season","COGy","COGx","slopeLat", "slopeLon")%>%
  distinct()

##add this line later
# rename("Center of Latitude"="COGy", "Center of Longitude"="COGx", "Slope (lat)"="slopeLat", "Slope (lon)"="slopeLon","Common Name"="comname", "Year"="est_year", "Season"="season")
write.csv(clean_w_season, "withSeason.csv", row.names = FALSE)

clean_wo_season<-COG_wo_season%>%
  unnest(data)%>%
  select("comname","est_year","COGy","COGx","slopeLat","slopeLon")%>%
  distinct()

write.csv(clean_wo_season, "withoutSeason.csv", row.names = FALSE)


##what is happening with tautog/croaker/weakfish/sharpnose (Kathy asked)
tautog<-NECC_fishes%>%
  filter(comname == "tautog")

weakfish<-NECC_fishes%>%
  filter(comname=="weakfish")%>%
  group_by(est_year)%>%
  distinct()

croaker<-NECC_fishes%>%
  filter(comname=="atlantic croaker")%>%
  group_by(est_year)

sharpnose<-NECC_fishes%>%
  filter(comname=="atlantic sharpnose shark")%>%
  group_by(est_year)

write.csv(tautog, "tautog.csv", row.names=FALSE)

tautog_cog<-clean_w_season%>%
  filter(`Common Name`=="tautog",
         Season == "Fall")

##calc distance between seasons####
install.packages(c("sf", "geodist", "geosphere"))
library(sf)
library(geodist)
library(geosphere)

geoTest<-clean_w_season%>%
  select(comname, season, est_year, COGy, COGx)%>%
  pivot_wider(names_from=season, values_from = c(COGx, COGy))%>%
  unite(COGx_Spring, COGy_Spring, col="Spring",sep=",")%>%    ##doesn't work
  unite(COGx_Fall, COGy_Fall, col="Fall",sep=",")%>% 
  group_by(comname,est_year)%>%
  nest()

##cleaner, nested geo data
geoTest<-clean_w_season%>%
  select(comname, season, est_year, COGx, COGy)%>%
  group_by(comname, est_year)%>%
  nest()

#test single species
alewife_geoTest<-clean_w_season%>%
  filter(comname == "alewife",
         est_year == "1970")

alewife_geoTest<-alewife_geoTest%>%
mutate(lat_long=st_as_sf(coords=c("COGx", "COGy"),crs=4326,remove=FALSE)) #nope

alewife_geoTest<-st_as_sf(alewife_geoTest, coords=c("COGx", "COGy"), crs=4326, remove=FALSE)
st_distance(alewife_geoTest$geometry[1], alewife_geoTest$geometry[2])
##hmmmmmmmmmmm that did something...?

#try st_as_sf on tibble
geoTest2<-clean_w_season%>%
  select(comname, season,est_year,COGx, COGy)
geoTest2<-st_as_sf(geoTest2, coords=c("COGx","COGy"), crs=4326, remove=FALSE) #eh

geoTest2<-as_tibble(geoTest2)
geoTest2<-geoTest2%>%
  pivot_wider(names_from=season, values_from=geometry) #nope

#attempting to make functions to map over nested data
join<-function(df){
  st_join(x=c(df$COGx, df$COGy))
}
dist_fun<-function(df){
  distm(df$Spring, df$Fall, fun=distGeo)
}

point_dist<-function(df){
  if(FALSE){
    df<-geoTest$data[[201]]
  }
  temp<-st_as_sf(df,coords=c("COGx","COGy"), crs=4326, remove=FALSE)
  out<-st_distance(temp)[1,2]
  return(out)
}

geoTest<-geoTest%>%
  mutate(dist=map_dbl(data,possibly(point_dist, NA)))

geoTest<-geoTest%>%
  select(comname, est_year, dist)%>%
  group_by(comname)%>%
  nest()

#map lm(est_year~dist)
dist_mod<-function(df){
  temp<-df%>%
    drop_na(dist)
  lm(dist~est_year, data=temp)
}
dist_count<-function(df){
  temp<-df%>%
    drop_na()%>%
    nrow()
}

slope<-function(x) x$estimate[2]

geoTest<-geoTest%>%
  mutate(mod=map(data, possibly(dist_mod, NA)),
         num_obs=map(data, possibly(dist_count, NA)),
         tidy=map(mod, possibly(broom::tidy, NA)),
         slope=map(tidy, possibly(slope, NA))) ##yayyyyyyyy

#cleaned file
season_dist<-geoTest%>%
  select(comname, num_obs, slope)

#map linear model (distance ~ year)

dist_mod<-function(df){
  temp<-df%>%
    drop_na(dist)
  lm(dist~est_year, data=temp)
}
dist_count<-function(df){
  temp<-df%>%
    drop_na()%>%
    nrow()
}
slope<-function(x) x$estimate[2]

season_dist<-season_dist%>%
  mutate(mod=map(data, possibly(dist_mod, NA)),
         num_obs=map(data, possibly(dist_count, NA)),
         tidy=map(mod, possibly(broom::tidy, NA)),
         slope=map(tidy, possibly(slope, NA)))

season_dist_km<-season_dist_km%>%
  mutate(mod=map(data, possibly(dist_mod, NA)),
         num_obs=map(data, possibly(dist_count, NA)),
         tidy=map(mod, possibly(broom::tidy, NA)),
         slope=map(tidy, possibly(slope, NA)))

season_dist_km<-season_dist_km %>% select(comname, num_obs, slope)
  

library(MASS)
write.matrix(season_dist, "seasonal distance.csv", sep=',')
write.matrix(season_dist_km, "Rate_of_Seasonal_Change_kilometers.csv", sep=',')

####pre and post 2010####
pre2010<-clean_w_season%>%
  filter(est_year<2009)%>%
  select(comname, season, est_year, COGx, COGy)%>%
  group_by(comname, season)%>%
  nest()

post2010<-clean_w_season%>%
  filter(est_year>2009)%>%
  select(comname,season,est_year,COGx,COGy)%>%
  group_by(comname, season)%>%
  nest()

write.csv(post2010, "post_2010.csv", row.names = FALSE)
write.csv(pre2010, "pre_2010.csv", row.names=FALSE)

#pre2010 model
pre2010<-pre2010%>%
  mutate(lat_mod=map(data, species_lat_mod),
         tidy_lat=map(lat_mod,broom::tidy),
         slopeLat=tidy_lat%>%map_dbl(function(x) x$estimate[2]))

#post2010 model
post2010<-post2010%>%
  mutate(lat_mod=map(data, species_lat_mod),
         tidy_lat=map(lat_mod,broom::tidy),
         slopeLat=tidy_lat%>%map_dbl(function(x) x$estimate[2]))

####plots 
plot_fun<-function(df){
  temp<-df%>%
    select(comname, season, est_year, COGy)
  out<-ggplot(temp, aes(est_year, COGy)+
                geom_point()+
                geom_smooth(method="lm"))
  print(out)
}

##pre2010
pre2010%>%
  filter(season == "Fall")%>%
  ggplot(aes(est_year, COGy))+
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("1970-2009")+
  ylab("Center of Latitude")+ 
  ggtitle("Fall Trends")+
  theme(
    axis.text.x = element_blank(),
    axis.ticks = element_blank())+
  facet_wrap(~comname)

pre2010%>%
  filter(season == "Spring")%>%
  ggplot(aes(est_year, COGy))+
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("1970-2009")+
  ylab("Center of Latitude")+ 
  ggtitle("Spring Trends")+
  theme(
    axis.text.x = element_blank(),
    axis.ticks = element_blank())+
  facet_wrap(~comname, scales = "free_y") ###andrew suggestion


##post2010
post2010%>%
  filter(season == "Fall")%>%
  ggplot(aes(est_year, COGy))+
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("2010-2019")+
  ylab("Center of Latitude")+ 
  ggtitle("Fall Trends")+
  theme(
    axis.text.x = element_blank(),
    axis.ticks = element_blank())+
  facet_wrap(~comname)

post2010%>%
  filter(season == "Spring")%>%
  ggplot(aes(est_year, COGy))+
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("2010-2019")+
  ylab("Center of Latitude")+ 
  ggtitle("Spring Trends")+
  theme(
    axis.text.x = element_blank(),
    axis.ticks = element_blank())+
  facet_wrap(~comname)



####MAPS/PLOTS####
maps<-clean_w_season%>%
  select(comname, season, est_year, COGx, COGy)%>%
  unite(COGx, COGy, col="coords",sep=",")

clean_wo_season%>%
  filter(comname == "acadian redfish")%>%
  ggplot(aes(COGx, COGy))+
  geom_point()+
  geom_density_2d()+
  xlab("Latitude")+
  ylab("Longitude")+
  ggtitle("acadian redfish")

##plots
