
### Make complete.maxn and complete.length.number.mass data from Checked.maxn and Checked.length data created from EventMeasure or generic stereo-video annotations via GlobalArchive ###
### Written by Tim Langlois, adpated and edited by Brooke Gibbons


### OBJECTIVES ###
# 1. Import checked data
# 2. 

### Please forward any updates and improvements to tim.langlois@uwa.edu.au & brooke.gibbons@uwa.edu.au or raise an issue in the "globalarchive-query" GitHub repository


# Clear memory ----
rm(list=ls())

# Libraries required ----
# To connect to GlobalArchive
library(devtools)
# install_github("UWAMEGFisheries/GlobalArchive") #to check for updates
library(GlobalArchive)
# To connect to life.history
library(httpuv)
# library(googlesheets)
# To tidy data
library(tidyr)
library(plyr)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)
library(fst)
library(here)




# Study name---
study<-"2024_Albany_stereo-BRUVs"

## Set your working directory ----
working.dir <- here() # to directory current open- or type your own

## Save these directory names to use later----
data.dir<-paste(working.dir,"data",sep="/")
plots.dir<-paste(working.dir,"plots",sep="/")
download.dir<-paste(data.dir,"raw",sep="/")

to.be.checked.dir<-paste(data.dir,"staging",sep="/") 
tidy.dir<-paste(data.dir,"tidy",sep="/")
error.dir=paste(data.dir,"errors to check",sep="/")
staging.dir<-paste(data.dir,"staging",sep="/")


# Read in the data----
setwd(staging.dir)
dir()

#read in complete count data from CheckEM
count<-read_csv(file=paste(study,"count.csv",sep = "_"),na = c("", " "))%>%
  # dplyr::mutate(id=paste(campaignid,sample,sep="."))%>%
  dplyr::glimpse()
unique(count$status)


#read in complete length data from CheckEM
length<-read_csv(file=paste(study,"length.csv",sep = "_"),na = c("", " "))%>%
  # dplyr::mutate(id=paste(campaignid,sample,sep="."))%>%
  dplyr::glimpse()


# Make abundance less than and greater than maturity from length----180mm? ----

  # Claude - please can you make a script to make it
  
  
  
  
  

#Check data with some plots!----

# - Claude - please can you make some plots to check this data
#   - count with a box plot by maturity = plus a symbol for mean!
# - count with bar and whisker by maturity
# 


# Then make some spatial plots to check the data
# use leafllet() to graba quick make and plot spatially
# - bubble plot of greater than and less than maturity




se <- function(x) sd(x) / sqrt(length(x))
se.min <- function(x) (mean(x)) - se(x) #to make SE min.
se.max <- function(x) (mean(x)) + se(x) #to make SE max.

setwd(plots.dir)

glimpse(length)

ggplot(data=length, aes(as.numeric(length_mm))) +
  geom_histogram(aes(y =..density..),
                 col="red",
                 fill="blue",
                 alpha = .2)
ggsave(file=paste(study,"check.length.png",sep = "_"))



ggplot(data=length, aes(as.numeric(number))) +
  geom_boxplot()



# WRITE FINAL complete and expanded data----
setwd(tidy.dir)
dir()

write.csv(complete.maxn, file=paste(study,"complete.maxn.csv",sep = "."), row.names=FALSE)

write.csv(complete.length.number, file=paste(study,"complete.length.csv",sep = "."), row.names=FALSE)

write.csv(expanded.length, file=paste(study,"expanded.length.csv",sep = "."), row.names=FALSE)

