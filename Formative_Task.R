# Load libraries 
install.packages("mice")
library(tidyverse, readr, haven)
library(mice, dplyr)


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
result


#---

# Check that pop census.x &.y are identical
result$Population2011Census.y <- as.integer(result$Population2011Census.y) #change from num to int datatype
identical(result[['Population2011Census.x']],result[['Population2011Census.y']]) #same (can filter out one)

# Create new variable for Life Expectancy - mean of male and female exp
result$LifeExpectancy = (result$Malelifeexpectancy + result$Femalelifeexpectancy)/2

# Filter out repeated columns (census, district codes) to keep 15 variables of interest (no fertility rate)
df = result %>% 
        select(Districtcode, Wardcode, Wardname, Population2011Census.x, Crimerate, Openspace, hhSocialRented,
               JobSeekers, Noqual, Carsperhousehold, LifeExpectancy,
               Children, Greaterthan65, nonwhite, NotBorninUK, NotEnglishspeaking)


# MISSING VALUES

duplicated(df$Wardcode) # check if wardcode is ever duplicated, 622 all unique
sum(is.na(df$Wardcode)) # No missing values in wardcode column
# NOTE: issue with Wardcode 00BAGDag - should 'ag' be there?

# Check the count of rows (observations) where there is missing data. 
md.pattern(df, rotate.names = TRUE)
?md.pattern  

# Look at where Na's are
sum(is.na(df)) # overall there are 540 NA's in dataset
 

# Check through columns to find where NA's located
which(is.na(df$hhSocialRented)) # Positions for the 5 NA's
which(is.na(df$JobSeekers))     
which(is.na(df$Noqual)) 
which(is.na(df$Carsperhousehold)) # same 5 positions for all 4 cols (socio dataset)

df %>% slice(123, 127, 188, 333, 574) # e.g. can see which Wards have missing data for these vars
                                      # then decide what to do with them/how to impute
                                      # replace with mean of var for whole district? Values vary a lot

sum(is.na(result$Children))   #104 NA's
which(is.na(result$Children)) 
which(is.na(result$Greaterthan65))
which(is.na(result$nonwhite))
which(is.na(result$NotBorninUK))
which(is.na(result$NotEnglishspeaking)) # same 104 positions for all 5 cols (demo dataset)

df %>% slice(which(is.na(result$NotEnglishspeaking))) # subset of 104 obs where above vars are NA's
                                                      # decide imputation method