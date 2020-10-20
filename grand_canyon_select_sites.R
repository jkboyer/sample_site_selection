# two files loaded 1) all sites (250 m) and 2) all reaches
# also two approaches - using a spatially balanced sample, the preferred method for selecting reaches
# and two using simple stratified random sample
# and two: 

library (tidyr)
library(dplyr)
library(BalancedSampling) # to select a spatially balanced sample

d<-read.csv("all_sites.csv") #up to date version
dr<-read.csv("sample_reaches.txt", header=TRUE)

#####################################################################
# select spatially balanced sample of reaches
####################################################################
N<-83 # population, how many reaches
n<-24 # number of nights of sampling
df<-dr[4:86,] # subset of reaches btween LF & Pearce Ferry rapid
p<-data.frame(df$perc) # inclusion prob
p<-as.numeric(p[,1]) # changes it to a vector
p1<-p*n # the probabilities must sum to the # reaches (= nights) you want to sample
df2<-data.matrix(df$reach)
s<-lpm1(p1,df2) # actual selection of reaches
s

overview<-subset(dr, reach %in% s)
write.csv(overview, "2020_overview_reaches.csv")

# now to select EF sample sites within reaches, should add a condition where EF= "y", so we
# don't select sites that are not fishable
ss<-d[d$reach %in% c(s) & d$EF=="y",] # sample sites of the subset of random reaches already selected
sp<-split(ss, list(ss$reach)) # split reaches and create a list
# randomly select # ef sites from a list of each reach selected, if more than 54 sites within a reach, only select
# 24, otherwise select 37.5% of the sites for EF
# because typically select 62.5% of all sites 
sam<-lapply(sp, function(x) x[sample(1:nrow(x), 
      ifelse((length(1:nrow(x))> 64), 24, (length(1:nrow(x))*0.375)), FALSE),])
ef.sam2<-lapply(sam, function(x) cbind(x, "numb"=1:nrow(x))) # adds a consecutive number column for the # of samples selected
ef.sam2<-lapply(ef.sam2, function(x) cbind(x, "gear"= 'EF'))
selected.sites <- do.call(rbind, ef.sam2) # make dataframe with selected sites
nrow(selected.sites) # how many sites total 

ef<-select(selected.sites, 4, 9, 11, 12) # subset to a few variables
ef$numb<-as.factor(ef$numb)
ef.sites<-spread(ef, reach, SiteID, fill=NA) # create a wide format table

test<-pivot_wider(ef, names_from=reach, values_from=SiteID)


# setwd("//flag-server/Office/Grand Canyon Downstream/Sample_site_selection/2020")
write.csv(ef.sites, "2020_EF_sample_sites.csv")

# if you have to redo a reach, say to sample the Bright Angel Reach = 23
s23<-d[which(d$reach=='23' & d$EF=='y'),]
s23b<-sample_n(s23, size=40, replace=FALSE)
write.csv(s23b, "Bright_angel_2020.csv")

# code below is if you selected reaches and EF sites, and shut off R without the hoop nets selected
selected.sites<-read.csv("2020_EF_sample_sites.csv")
s<-unique(selected.sites$reach) # extracts the reach number from the "selected.sites" table
ss<-d[d$reach2 %in% c(s),] # sample sites of the subset of random reaches already selected
##

# select hoop nets
hs<-subset(ss, !(OBJECTID %in% selected.sites$OBJECTID)) #subset of sample sites that were not selected for EF
hp<-split(hs, list(hs$reach)) # split reaches and create a list
h.sam<-lapply(hp, function(x) x[sample(1:nrow(x), 
              ifelse((length(1:nrow(x))> 39), 16,     # if more than 39 sample sites select 16
              ifelse(((length(1:nrow(x))*0.25)<3), 3, # sets the minimum as 3 sites per reach
              (length(1:nrow(x))*0.25))), FALSE),])   # otherwise 25% of sites randomly selected
h.sam2<-lapply(h.sam, function(x) cbind(x, "numb"=1:nrow(x))) # adds a consecutive number column for the # of samples selected
h.sam2<-lapply(h.sam2, function(x) cbind(x, "gear"= 'HN'))
hn.selected.sites <- do.call(rbind, h.sam2)       # make dataframe with selected hoop net sites
nrow(hn.selected.sites)
# may need to adjust afterwards for reduced sampling in Marble canyon 8 HN total, 16 HN at 30 mi, and 16 below Nankoweep ~56.3

t1<-select(hn.selected.sites, 4, 9, 11, 12) # subset to a few variables
t2<-spread(t1, reach, SiteID, fill=NA) # create a wide format table

write.csv(t2, "2020_HN_sample_sites.csv") 


######################################################################
# TWO: TOTAL RANDOMIZED SAMPLING
##################################################################### 
#sample sites for both trips based on the number of sampling nights = number of sampling reaches
# this is stratified sampling by reach and then within reach
# the maximum number of samples within a reach is 24
# 
nights<-24 # how many nights of sampling in two trips
area<-sample(dr$no, size=nights, replace=FALSE, prob= dr$perc)#random sampling based on perc of reach
ss<-d[d$reach %in% c(area),] # subset of random reaches selected
sp<-split(ss, list(ss$reach)) # split reaches and create a list
# randomly select # ef sites from a list of each reach selected, if more than 54 sites within a reach, only select
# 24, otherwise select 44% of the sites for EF
# because typically select 66% of all sites, but 66% of those are EF sites (eg 44% of total) and the remainder hoop nets
sam<-lapply(sp, function(x) x[sample(1:nrow(x), ifelse((length(1:nrow(x))> 54), 24, (length(1:nrow(x))*0.44)), FALSE),])
selected.sites <- do.call(rbind, sam) # make dataframe with selected sites
nrow(selected.sites) # how many sites total 



########################################################################################################
# code below for simulations for number of samples per year
#######################################################################################################
nights<-30 # how many nights of sampling in two trips
iterations<-5000
num<-0 # just set initial to zero as a placeholder
result<-vector(mode="numeric", length = iterations) # store the results
# loop code - not the fastest (it takes ~ 30 sec) - but it is what I know how to do :)
for(i in 1:iterations){
  area<-sample(dr$reach, size=nights, replace=FALSE, prob= dr$perc)#random sampling based on perc of reach
  ss<-d[d$reach %in% c(area),] # subset of random reaches selected
  sp<-split(ss, list(ss$reach)) # split reaches and create a list
  # randomly select # ef sites from a list of each reach selected, if more than 54 sites within a reach, only select
  # 24, otherwise select 44% of the sites
  sam<-lapply(sp, function(x) x[sample(1:nrow(x), ifelse((length(1:nrow(x))>54), 24, (length(1:nrow(x))*0.44)), FALSE),])
  selected.sites <- do.call(rbind, sam) #make dataframe with selected sites
  num[i]<-nrow(selected.sites)
  result[i]<-num[i]
}

mean(result)
quantile(result, probs=c(0.025,0.975)) # to get 95% CI

#######################################################################
# a simulation to calculate the max mileage between sample reaches
#######################################################################
# basic code
nights<-30
iterations<-10
b<-sample(dr$start.rm, size=nights, replace=FALSE, prob= dr$perc)
b<-sort(b, decreasing=FALSE)
mx<-max(diff(b))
mx


# loop to determine mean and CI
nights<-30  # how many nights of sampling
num<-0      # set to 0  just as a placeholder for the loop below
iterations<-5000  

dist.result<-vector("numeric", iterations)
for(i in 1:iterations){
  b<-sample(dr$start.rm, size=nights, replace=FALSE, prob= dr$perc) #random sampling based on % of reach
  b<-sort(b, decreasing=FALSE) # need to sort by river mile
  b<-as.vector(b) # not sure if this is necessary
  b<- diff(b) # calculates the difference between each row
  b<-max(b)   # what is the maximum distance
  dist.result[i]<-b
}
mean(dist.result)
quantile(dist.result, probs=c(0.025,0.975)) # to get 95% CI


# loop to determine mean and CI for a single trip, 
# you just change the "diff' command to a lag=2
# as each trip is every other sample (half the samples)
nights<-30  # how many nights of sampling
num<-0      # set to 0  just as a placeholder for the loop below
iterations<-4000  

dist.result<-vector("numeric", iterations)
for(i in 1:iterations){
  b<-sample(dr$start.rm, size=nights, replace=FALSE, prob= dr$perc) #random sampling based on perc of reach
  b<-sort(b, decreasing=FALSE) # need to sort by river mile
  b<-as.vector(b) # not sure if this is necessary
  b<- diff(b, lag=2) # calculates the difference between every other row
  b<-max(b)   # what is the maximum distance
  dist.result[i]<-b
}
mean(dist.result)
quantile(dist.result, probs=c(0.025,0.975)) # to get 95% CI





