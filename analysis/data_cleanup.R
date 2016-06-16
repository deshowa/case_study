### This is the code for the Unit 6 case study####

# The first thing to do is read in both of the files

#install.packages("countrycode")
#install.packages("xlsx")

library(plyr)
library(RCurl)



sessionInfo()

getwd()

# just checking to make sure we are in the right directory for the project, the user should as well

# First, we need to download the data and save in our data directory

gdp_url<- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
educational_url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"

download.file(gdp_url, "analysis/data/getdata-data-gdp.csv")
download.file(educational_url, "analysis/data/getdata-data-EDSTATS_Country.csv")


raw_gdp<- read.csv("analysis/data/getdata-data-gdp.csv",header = TRUE, skip = 3, blank.lines.skip = TRUE) 
                  
#print(raw_gdp)

# I do not want the comments below row 232 so I will simply use the nrows argument and reread the .csv since the file is small

raw_gdp<- read.csv("analysis/data/getdata-data-gdp.csv",header = TRUE, skip = 3, blank.lines.skip = TRUE, nrows = 232) 

#print(raw_gdp)

# good, one problem solved...we got rid of the information below EMU



colnames(raw_gdp)

# These are pretty bad looking column names, definitely not tidy, let's rename them

names(raw_gdp) <-c("country_code","GDP_rank", "null_col","economy","GDP_US_dollars","letter_column" )

colnames(raw_gdp)
# alright, we have some good names here now

# Now, lets' remove that null_col, mainly because it is annoying
# I know that the columns is the 3rd column, so I will just remove it
keepers<- c("country_code","GDP_rank","economy","GDP_US_dollars","letter_column" )

raw_gdp<- raw_gdp[,keepers]

#print(raw_gdp)
#looks gone, let's make sure
colnames(raw_gdp)

# Now let's move on to NA rows
# first, let's count the NA rows
sum(is.na(raw_gdp$country_code))
sum(is.na(raw_gdp$GDP_rank))
sum(is.na(raw_gdp$economy))
sum(is.na(raw_gdp$GDP_US_dollars))
sum(is.na(raw_gdp$letter_column))

## Or could do it this way

sapply(raw_gdp, function(x) length(which(is.na(x))))

# looks like we have NAs in GDP rank, let's take those out

raw_gdp<- na.omit(raw_gdp)

str(raw_gdp)

# make sure we took out all of those country groupings

sapply(raw_gdp, function(x) length(which(is.na(x))))

# looks good

# Let's check the class of the variables in the dataset to see if anything looks weird and needs changing



sapply(raw_gdp, class)

# looks like GDP_US_dollars is a factor, I would like this to be numeric, but I noticed (after many trials) that commas are present in the factor



raw_gdp$GDP_US_dollars<- as.numeric(gsub(",","",raw_gdp$GDP_US_dollars))


#check the results

sapply(raw_gdp, class)

# make sure nothing is null

sapply(raw_gdp, function(x) length(which(is.na(x))))

# check!  We look good here now!

# the only thing left appears to be the letter column.  I am making the decision to leave this in, because I could see
# the column at some point meaning something, but it will not hurt the data to be there
# thank goodness I already have the country code data...The biggest thing to do now is join (merge) the datasets together


# looks like we should do not need to add the country code column to this dataset

### Now, let's move on to the Educational stats data####

raw_educ<- read.csv("analysis/data/getdata-data-EDSTATS_Country.csv",header = TRUE)

sapply(raw_educ, function(x) length(which(is.na(x))))

# looks like alot of my observations contain NAs, I may not want to do the same filtering in this one.  If I do, I may not have any 
# observations left

# let's take a look

#head(raw_educ)
#str(raw_educ)

# just as I suspected, there are 234 observations, if I go out and delete all of the NAs, most of my dataset and potentially some
#matches will disappear.  I can always filter later.  This dataset really looks like it mostly adds dimensionality, not numeric data




# I do need to rename the country code prior to merging, let's do that right now.


colnames(raw_educ)[1]<- "country_code"

colnames(raw_educ)

# ok, now we match, both formats were alright, but I like the more database friendly "-" version

# let's merge the dataset together, I chose all = FALSE in order to perform an inner join


clean_gdp_education_data<- merge(x = raw_gdp, y = raw_educ, by = "country_code", all = FALSE)

sapply(clean_gdp_education_data, class)

head(clean_gdp_education_data)

#clean_gdp_educ$economy
#nrow(clean_gdp_educ)
#nrow(raw_educ)
#nrow(raw_gdp)

write.csv(clean_gdp_education_data,"analysis/data/tidy_gdp_educ_data.csv", row.names = FALSE)


                    




