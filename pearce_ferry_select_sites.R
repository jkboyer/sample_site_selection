#randomly select sampling sites for reach below Pearce Ferry
#stratified by 3 equal length reaches (~5 miles each)
#also selects alternate sites in case site is not hoop-netable (can be problem
#      in inlet as lake level changes)

#inputs: "below_pearce_sites.csv" lall 250m sites Pearce ferry Rapid - Mead
#outputs: "below_pearce_sampling_sites_YYYY.csv" table with sampling sites and alternates
#write.csv lines currently masked with # to avoid overwriting existing tables

tripid = "PF20210421"
n.sites.day <- 17

#load data
sites <- read.csv("./data/below_pearce_sites.csv")

#order sites, add end points to each site
sites <- sites[order(sites$bank, sites$RM),]
sites$name <- as.character(sites$name)
sites$end <- c(tail(sites$name, -1), "")

#remove last RM on each side - these are endpoints only, not start points
sites <- sites[sites$RM <= 296.7,]

#delineate into 3 reaches
total.length <- max(sites$RM) - min(sites$RM) #calculate total length
total.length
reach.length <- total.length/3 #divide into 3 reaches
reach.length
#assign reach number to each site (1 upstream, 2 middle, 3 downstream)
sites$reach <- ifelse(sites$RM <= 281.7 + reach.length, 1,
                      ifelse(sites$RM >= 281.7 + 2*reach.length, 3,
                             2))

#randomly select sampling sites
sp <- split(sites, list(sites$reach)) #split by reach to stratify selection
#randomly select 10 sites in each reach
samples <- lapply(sp, function(x) x[sample(1:nrow(x), n.sites.day, FALSE),]) #10 samples
selected.sites <- do.call(rbind, samples) #make dataframe with selected sites


#merge into one dataframe and export as .csv
selected.sites <- selected.sites[order(selected.sites$reach,
                                       selected.sites$RM),]

selected.sites$notes <- paste0(selected.sites$bank,
                              ifelse(selected.sites$reach == 3, 1, #sample lower reach day 1
                                     ifelse(selected.sites$reach == 1, 2, #uppder reach day 2
                                            3))) #middle reach last

selected.sites$num <- rep(seq(1:n.sites.day), 3)
selected.sites$notes <- paste0(selected.sites$notes,
                               ifelse(selected.sites$num <= 9, "0", ""),
                               selected.sites$num)
selected.sites$num <- NULL


#subset to needed columns
selected.sites <- selected.sites[, c("RM", "name", "end", "reach", "notes")]
selected.sites <- selected.sites[order(selected.sites$RM),]
#order as wanted on sheets
selected.sites <- selected.sites[order(selected.sites$reach,
                                       selected.sites$notes,
                                       selected.sites$RM),]

write.csv(selected.sites,
   file = paste0("./output_selected_sites/", tripid, "_sampling_sites.csv"),
   row.names = FALSE)

