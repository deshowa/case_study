
## the data_cleanup.R file contains all of the tidying procedures used to get to the analysis phase
## to run the code and re-create the file we can call the source() function

source("analysis/data_cleanup.R")
library(ggplot2)

# Now that we ensured that the data has been created, let's just clear the workspace so we can do the analysis

ls()
rm(list= ls())

# now we can read in the tidy dataset

gdp_education_data<- read.csv("analysis/data/tidy_gdp_educ_data.csv", header = TRUE)

sapply(gdp_education_data, class)

head(gdp_education_data)
ls(gdp_education_data)

# See, I really like this format, because I can differentiate which columns came from the gdp dataset with the "_" 


##QUESTIONS

# question1: How many of the IDs match?
# answer1: Since I merged the data with an inner join, the records that match can be pulled with the nrow() function.  
  # I see that we have 189 matches

matching_cases<- nrow(gdp_education_data)
matching_cases

# question2: What is the 13th country in the dataframe when the data is sorted in ascending order by GDP rank?

# first, I will order the data:

gdp_education_data<- gdp_education_data[order(gdp_education_data$GDP_US_dollars),]

# second, I will pull the 13th country, and make it into a variable because I like creating variables

thirteenth_ranked_country <- gdp_education_data[13,]

# I filtered for St.Kitts, but it is still a dataframe, I want a clean result, so I will just make the variable a vector
class(thirteenth_ranked_country)

as.vector(thirteenth_ranked_country$economy)
# Alright, St.Kitts is the 13th smallest

# question3: What are the average GDP rankings for the "high income:oecd and "high income nonOECD" groups

# first let me filter out the dataset
high_income_groups<- subset(gdp_education_data,Income.Group == "High income: OECD" | Income.Group == "High income: nonOECD")
# check the filtering
str(high_income_groups)
#This looks right, but let me make sure that I am only pulling those 2 groups
as.vector(unique(high_income_groups$Income.Group))
length(as.vector(unique(high_income_groups$Income.Group)))
# great, that looks good


aggregate(high_income_groups$GDP_rank, list(high_income_groups$Income.Group),FUN = mean)
# looks like the mean rank of High income: nonOECD  is 91.9 and the mean rank of the high income: OECD is about 33.0 (32.9 rounded)


#question4: plot the gdp of all countries.  Use ggplot2 to color your plot by income group


ggplot(data = gdp_education_data, aes(x = economy, y = GDP_US_dollars, fill = Income.Group )) + geom_bar(stat = "identity")

# The first graph was nice, but hard to see, so looking at the log still seems to show the relative ranks while making more items visible
#ggplot(data = gdp_education_data, aes(x = economy, y = log(GDP_US_dollars), fill = Income.Group )) + geom_bar(stat = "identity")



# the graph is hard to read, if we look at a scatterplot of ranks versus GDP, it sorts the data automatically

plot(gdp_education_data$GDP_rank, gdp_education_data$GDP_US_dollars)


# Cut the GDP ranking into 5 separate quantile groups, make a table versus income group

# get quantiles
seq(0,1,.2)

# use the handy dandy cut function to create the cut up variable!  
gdp_education_data$GDP_quantile<- cut(gdp_education_data$GDP_rank, quantile(gdp_education_data$GDP_rank, probs = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)))

# now to create a summary table of the variables
quantile_table<-aggregate(gdp_education_data$GDP_rank, list(gdp_education_data$Income.Group, gdp_education_data$GDP_quantile),FUN = length)
quantile_table


## How many countries are lower middle income but among 38 nations with the highest GDP

nrow(highest_GDP_lowest_income_data<- subset(gdp_education_data,Income.Group == "Lower middle income" & GDP_rank <39))

# looks like 5 countries meet the criteria


# now a little rogue analysis

# just want to look at the top and bottom 10
top_10<- subset(gdp_education_data,GDP_rank <11)
plot(top_10$GDP_US_dollar,top_10$economy,  col = top_10$Income.Group)


# now a little rogue analysis,
bottom_10<- subset(gdp_education_data,GDP_rank >180)
plot(bottom_10$GDP_US_dollar,bottom_10$economy,  col = bottom_10$Income.Group)
bottom_10

