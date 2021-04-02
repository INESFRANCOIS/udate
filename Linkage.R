# Load libraries 
library(tidyverse)
library(readr)
library(haven)

# Read in the data files 
codes = read.csv('London District codes.csv')
env = read.csv('London ward data environment.csv')
demo = read.delim("London ward data demographics.dat")
health = read_sas('london ward data health.sas7bdat')
socio = read_sav('London ward data socioeconomic.sav')

# Initial observations 
head(codes)
head(env)
head(demo)
head(health)
head(socio) 

# Socio has several rows at the end of the data set which are completely blank. 
# Drop them as there is no info in them 
socio = drop_na(socio)

# Check if all remaning values in socio and env are unique - They are 
# This means env covers more wards than socio
length(unique(socio$Wardcode))
length(unique(env$Wardcode))

# Check the data types 
str(socio)
str(env)

# Convert env Wardcode to a character to make it consisten with socio 
env$Wardcode = as.character(env$Wardcode)

## Joins take overlapping variables as default - hence why no variable name is specified in the joins

# Steps 1-4 in the linkage instructions 
# Joining env with socio on wardcode (full join so all cases are carried through even if there is no match)
temp1 = env %>%
  full_join(socio) %>%
  # Add a new district code variable which takes the first 4 characters of wardcode 
  mutate(districtcode = substr(Wardcode, start=1, stop=4)) %>%
  # Add my id variable which takes district code and population
  mutate(MyID = paste(districtcode, Population2011Census, sep=""))

# Steps 5-8 in the linkage instructions 
temp2 = health %>%
  rowwise() %>%
  # Create new district variable which contains the first word from wardname 
  mutate(District = str_split(Wardname, " - ")[[1]][1]) %>%
  # Join disctrict codes on name to add the code 
  left_join(codes) %>%
  # Trim disctrict code - the original data set had a space before the code and the below line removes that
  mutate(districtcode = str_trim(Districtcode)) %>%
  # Add My ID variable which takes district code and population (Same as above)
  mutate(MyID = paste(districtcode, Population2011Census, sep="")) %>%
  # Join demo on Wardname 
  full_join(demo)

# Join temp1 and 2 - this join you specify which variable to join on because there are multiple overlapping variables 
result = inner_join(temp1, temp2, by="MyID")

# Write out result as csv 
# write.csv(result,'linked.csv')



