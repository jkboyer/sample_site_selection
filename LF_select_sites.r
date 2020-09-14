#selects sites for Lees Ferry sampling
#for each trip, edit year, n sites, and nonnative sampling at top as needed

#inputs: LF_sites.csv list of all 250-m sample sites from dam to ferry
#outputs: TRIP.ID_selected_sites.csv selected sites
#         Will require some formatting in excel to print schedule/site sheets

require(tidyverse)

s <- read.csv("./data/LF_sites.csv", stringsAsFactors = FALSE)

#Information about trip
#MUST UPDATE for each trip before running script to select sites
current.year <- 2020
trip.id <- "LF20200921"
nonnative <- TRUE #TRUE or FALSE. Is nonnative sampling happening
n.sites <- 40 #n monitoring sites to sample

#calculate number of sites to pick per section (upper, middle, low)
reaches <- s %>%
  filter(select == "Y" & type == "monitoring") %>%
  mutate(total.sites = length(section)) %>%
  group_by(section, total.sites) %>%
  summarize(n = n()) %>%
  mutate(proportion = n/total.sites,
         #the - 0.05 is to keep rounding up issues from adding an extra site
         n.sites = round((proportion*n.sites - 0.05), 0))

n.per.section <- reaches$n.sites #save as vector of sample sizes
n.upper <- n.per.section[1]
n.middle <- n.per.section[2]
n.lower <- n.per.section[3]

#select monitoring sites
#Throwing errors. Do this in base instead.
#selected <- s %>%
#  filter(section %in% c("upper", "middle", "lower") &
#           select == "Y") %>%
#  group_by(section) %>% #grouping variable to stratify by
#  nest() %>%            #have to nest to do stratified with different n each group
#  mutate(n = n.per.section) %>%  #sample sizes for each group
#  mutate(samples = map2(data, n, sample_n)) %>% #randomly select n sites from each group
#  select(section, samples) %>% #select only needed columns
#  unnest() #remove nesting so data displays as normal dataframe

#split into three sections and select sites
upper <- s %>%
  filter(section == "upper" &  select == "Y") %>%
  sample_n(size = n.upper)
middle <- s %>%
  filter(section == "middle" &  select == "Y")%>%
  sample_n(size = n.middle)
lower <- s %>%
  filter(section == "lower" &  select == "Y")%>%
  sample_n(size = n.lower)

#join selected sites
selected <- bind_rows(upper, middle) %>%
  bind_rows(lower)

rm(upper, middle, lower) #no longer needed, remove

#extract nonnnative sites
nn.sites <- s %>%
  filter(type == "nonnative")

#arrange and format for printing
selected <- selected %>%
 arrange(side, id) %>%
   #add contiguous comments if needed
  mutate(comments = case_when(
    rm_start == lag(rm_end) & rm_end == lead(rm_start) ~
                              paste("yellow start & end", comments),
    rm_end == lead(rm_start) ~ paste("yellow end", comments),
    rm_start == lag(rm_end) ~ paste("yellow start", comments),
    TRUE ~ comments)) %>%
  select(id, type, rm_start, side, site_id, end_site_id, comments)

nn.sites <- nn.sites %>%
  select(id, type, rm_start, side, site_id, end_site_id, comments)


#add nonnative sites IF we are sampling them
if (nonnative == FALSE) {
  selected <- selected
} else {
  selected <- bind_rows(selected, nn.sites)
}

selected <- selected %>%
  arrange(rm_start)

#save file
#in excel, you will still need to:
#            break into days of sampling
#            deal with monitoring sites that overlap nonnative sites
#            make sure sites are orderered correctly in slough and 4 mile bar area
#            assign sites to be sampled on nonnative night to their own day
#            make sure the zigzagging between shores is logical
write.csv(selected,
          paste0("./output_selected_sites/", trip.id, "_sample_sites.csv"),
          row.names = FALSE)

