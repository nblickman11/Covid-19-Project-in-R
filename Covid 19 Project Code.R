# Covid 19 R Project
install.packages("gridExtra")
library(gridExtra)

# shape of data frame
dim(usa_county_wise)
# filter data with with regex to contain only months after march (0 deaths mainly before that)
usa_county_wise2 = dplyr::filter(usa_county_wise, grepl("^[4567].*", usa_county_wise$Date))

states = unique(usa_county_wise2$Province_State)   # pull our States/Provinces
states = states[10:20]

# SOO, for loops in R dont have scope, so when get to end of for loop you overwrite your stuff with 
 #the most recent item.  So, use list functions in R instead.
# CONSIDER, when you finish the for loop and want to print say your first "plot[[1]]".  This plot is may use
  # that plot in position one of the list, However, the variables/parameters updated on thed last iteration,
  # which actually changes your first plot according to these variables.

plot_f = function(data, state){
  # get all rows of that state
  all_rows_of_state = data[data$Province_State == state,]
  # get columns dates and deaths of that state and put in dataframe
  timeline_by_day = all_rows_of_state$Date
  deaths_by_day = all_rows_of_state$Deaths
  state_df = cbind(timeline_by_day,deaths_by_day)
  state_df = as.data.frame(state_df)
  
  #Change variable types and recombine in dataframe...
  # our o here is a factor.  This must be turned to character before turned to numeric.
  # Because factors are stored as integers with a table of factor level labels.
  # And we cant take the cumsum of a factor
  o = state_df[, 2]
  death_col = as.numeric(as.character(o))
  o2 = state_df[, 1]
  days_col = (as.character(o2))
  # dataframes take more than one datatype (cbind alone can't)
  bb = cbind.data.frame(days_col,death_col)
  # turn to data table 
  dt = as.data.table(bb)
  
  # Take the running total sum of deaths per day
  death_totals_by_day = dt[,cumsum(death_col), by=days_col]
  # Pull out the total sums
  death_totals_by_day2 = death_totals_by_day[,max(V1), by=days_col]
  
  # will return each plot
  ggplot(death_totals_by_day2, aes(x=death_totals_by_day2$days_col,y=death_totals_by_day2$V1,
                                   group = 1)) + geom_line() + ggtitle(state) + xlab("") + ylab("")
  
}

plots = lapply(states, plot_f, data = usa_county_wise2)

do.call("grid.arrange", 
        c(plots, ncol=4, nrow=3, 
          left = "Deaths per Day (X Axis)", 
          bottom = "April-July by day (Y-Axis"))



