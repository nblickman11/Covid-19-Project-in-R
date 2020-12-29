# Part 2
library(ggplot2)

# In America... 
# Percent of Population Tested = 19% of the population was tested (63 mill tests from 331 mill people)
# Diagnosis Rate = 8% of the total tests were Covid Positive (5 mill of 63 mill tests)
# Percent of Population with Covid = 1.5% of the population was Covid Positive (5 mill of 331 mill people) 
# Timeline..
# January - July 2020

# In R you can't delete a row from a DataFrame like this. 
    #worldometer_data = worldometer_data[worldometer_data$Continent != "Diamond Princess",]
#You have to take a subset of the data frame to get the rows you want.

# BE CAREFUL BELOW.  there is a country that does not have a Continent Listed, NA.
# Therefore the mask I create will have one NA along with True and False, which 
# will mess up the values I get later.
# TO FIX: I removed this row from the original DataFrame. I also removed Yemen
# which was messing up my Asia plot

#load in data
#setwd("Desktop")
worldometer_data = read.csv("R Studio/covid project/data/worldometer_data.csv")

# locate row of Yemen and Sudan and others
indic = which(worldometer_data$Country.Region == "Yemen")
indic2 = which(worldometer_data$Country.Region == "Sudan")
indic3 = which(worldometer_data$Country.Region == "Guinea-Bissau")

# take subset of rows
worldometer_data = worldometer_data[-c(indic, indic2, indic3, 157),]

calcs = function(row) {
  total_cases = as.numeric(row["TotalCases"])
  total_tests = as.numeric(row["TotalTests"])
  hypothetical_19_perc_pop_numb = as.numeric(row["Population"]) * .19
  diagnosis_rate = total_cases / total_tests
  
  # if a country tested 19 percent of their population like the U.S did, and
  # based on this countries diagnosis rate, how many covid cases would they have found
  hypo_covid_cases = hypothetical_19_perc_pop_numb * diagnosis_rate
  
  # get the percentage relative to population
  hypo_percent_cases = hypo_covid_cases / as.numeric(row["Population"])
  return(c(diagnosis_rate, hypo_percent_cases))
}

final = apply(worldometer_data, 1, calcs)

#transpose
final = t(final)
#rename columns
colnames(final) = c("Diagnosis Rate", "Percent Cases")

continents = c("North America", "South America", "Africa", "Europe", "Asia", "Australia/Oceania")


# Plot Function
plot_f = function(data, continent){
  # pull out all countries in North America
  mask = worldometer_data["Continent"] == continent
  
  N_Amer_countries = worldometer_data["Country.Region"][mask]
  final = as.data.frame(data)
  countries = worldometer_data["Country.Region"]
  join = as.data.frame(c(final, countries))
  
  # filter join based on the mask, and plot bar graph
  values = join["Percent.Cases"][mask]
  
  #for more room on xlabel ticks on plot
  par(mar=c(9,7,3,3))
  
  values = values*100
  continent = as.character(continent)
  string = paste(continent)
  # plot Covid Cases % for countries in North America
  barplot(values, main = string, 
          ylab = "Percent of Covid Cases",
          names.arg = N_Amer_countries, las=2, 
          col.axis = 'Black', col = 'red', cex.main = 2, cex.lab = 1.5,
          cex.axis = 1.5, cex.names = 1.4)
}

lapply(continents, plot_f, data = final)



