#selects sites for grand canyon or diamond down sampling

library(tidyverse)
library(BalancedSampling) #spatially balanced sampling

#Information about trip
#MUST UPDATE for each trip before running script to select sites
current.year <- 2020
trip.id <- "GC20201025"
#start and end miles of trip
#0.0 - 281.6 for full Grand Canyon, 226.0 - 281.6 for diamond down
trip.start.rm <- 226.0
trip.end.rm <- 281.6
n.nights <- 4 #n sampling nights
n.ef.sites <- 24 #n efish sites per reach (total, for both boats)
n.hoop.sites <- 16 #n hoop sites per reach (total, for both boats)


#file.path <- "//flag-server/Office/Grand Canyon Downstream/Sample_site_selection/"
#working from home now - files in project folder
file.path <- "./data/"
sites <- read.csv(paste0(file.path, "grand_canyon_all_sites.csv"),
                  stringsAsFactors = FALSE)
reaches <-read.csv(paste0(file.path, "sample_reaches.txt"))

#add end rivermile to sites
left <- sites %>%
  filter(RiverSide == "L") %>%
  arrange(id) %>%
  mutate(end.rm = lead(MapLabel))
right <- sites %>%
  filter(RiverSide == "R") %>%
  arrange(id) %>%
  mutate(end.rm = lead(MapLabel))

sites <- bind_rows(left, right)

rm(left, right)

#for diamond downs subset to diamond down only
#if
sites <- sites %>%
  filter(RiverMile_100ths >= trip.start.rm & #below diamond
           RiverMile_100ths < trip.end.rm) %>% #and above pearce ferry rapid
  filter(!is.na(reach))

total.sites = nrow(sites)

reaches <- reaches %>%
  filter(start.rm >= trip.start.rm &
           start.rm < trip.end.rm) %>%
  #recalculate percent length for diamond down only
  mutate(perc = no.sites/total.sites) %>%
  # determine how many sites would be selected in each reach
  # if reach has > 54 sites, select full number of sites
  # if reach has < 54 sites, select 44% of total sites for efishing,
  #                             and 22% of total sites for hoop nets
  mutate(n.ef.sites = ifelse(no.sites >= 54, n.ef.sites,
                                ceiling(no.sites*0.44)),
         n.hoop.sites = ifelse(no.sites >= 54, n.hoop.sites,
                               ceiling(no.sites*0.22)))

#join n sites to site table
sample.sizes <- reaches %>%
  select(reach, n.ef.sites, n.hoop.sites)

sites <- sites %>%
  full_join(sample.sizes)
rm(sample.sizes)

#select reaches to sample #######

#option 1: random reach selection
#selected.r <- sample(reaches$reach, #where to select from
#                     size = n.nights, #sample size: number of nights
#                     replace = FALSE, #sample without replacement
#                     #weight probability of selection according to reach length
#                     prob = reaches$perc)

#option 2: spatially balanced

probability <- (reaches$perc)*n.nights # probabilities must sum to the # reaches you want to sample
sum(probability)
reach.dm <- data.matrix(reaches$reach)
selected.r <- lpm1(probability, reach.dm) +
  #results given are 1-12, not actual reach numbers
  #add number of upstream reaches to get reach numbers
  (min(reaches$reach) - 1)
selected.r

#subset reach dataframe to only selected reaches
selected.reaches <- reaches %>%
  filter(reach %in% selected.r)

#make blank dataframe to hold selected sites
selected <- sites[0,]

selected.reaches$reach #these are the selected reaches to loop along
for (i in seq_along(selected.reaches$reach)) {

  this.reach <- selected.reaches$reach[[i]]
  n.ef <- selected.reaches$n.ef.sites[[i]]
  n.hoop <- selected.reaches$n.hoop.sites[[i]]

  #print the above so I can see reach num and n sites to select
  print(paste("reach", this.reach))
  print(paste(n.ef, "electrofishing sites"))
  print(paste(n.hoop, "hoop nets"))

  #randomly select electrofishing sites
  selected.ef <- sites %>%
    filter(EF == "y" & #filter to sites deemed suitable to electrofish
             reach == this.reach) %>% #filter to current reach
    sample_n(n.ef, #n sites to pick, determined from reach length
             replace = FALSE) %>% #sample without replacement
    mutate(GEAR_CODE = "EL")
  #define vector of selected efish sites, will use to exclude from hoop selection
  ef.selected.sites <- selected.ef$SiteID

  #randomly select hoop net sites
  selected.hoop <- sites %>%
    filter(Hoopnet == "y" & #filter to sites deemed suitable for Hoopnets
           SiteID %in% ef.selected.sites == FALSE & #not already chosen for efishing
           reach == this.reach) %>% #filter to current reach
    sample_n(n.hoop, #n sites to pick, determined from reach length
             replace = FALSE) %>% #sample without replacement
    mutate(GEAR_CODE = "MHB")

  #bind selected sites for this reach to selected dataframe

  selected <- bind_rows(selected, selected.ef)
  selected <- bind_rows(selected, selected.hoop)

}

rm(selected.ef, selected.hoop)

#arrange data frame of selected sites into order needed for printing
selected <- selected %>%
  select(reach, river.mile = RiverMile_100ths, site.id = MapLabel, end.rm, side = RiverSide,
         GEAR_CODE) %>%
  arrange(reach, side, river.mile) %>%
  mutate(day = as.numeric(factor(reach)))


#add contiguous text
selected <- selected %>%
  #add contiguous comments if needed
  mutate(comments = case_when(
    GEAR_CODE == "MHB" ~ paste0(side, "0", day),
    site.id == lag(end.rm) & end.rm == lead(site.id) &
      GEAR_CODE == "EL" &
      GEAR_CODE == lag(GEAR_CODE) &
      GEAR_CODE == lead(GEAR_CODE) ~
        "yellow start & end",
    end.rm == lead(site.id) &
      GEAR_CODE == "EL" &
      GEAR_CODE == lead(GEAR_CODE) ~
        "yellow end",
    site.id == lag(end.rm) &
      GEAR_CODE == "EL" &
      GEAR_CODE == lag(GEAR_CODE) ~
        "yellow start",
    TRUE ~ ""))

#SAVE ########
#commented out to avoid overwriting existing file
#uncomment when making new file for next year
#write.csv(selected, paste0("./output_selected_sites/", trip.id, "sampling_sites.csv"),
     #     row.names = FALSE)

