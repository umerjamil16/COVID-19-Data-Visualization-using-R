
 
VISUALIATION OF COVID-19 DATASET

 
# Table of Contents
##### Introduction	
##### R Packages:	
##### Loading Dataset	
##### Data Cleaning	
##### Visualizations:	
###### 	Plot 1: World map: Death cases percentage of infected cases	
###### 	Plot 2: Worldmap: Number of latest confirmed cases	
###### 	Plot 3: Number of COVID-19 Cases (confirmed, deaths, recovered) vs Timeline	
###### 	Plot 4: Current Confirmed Cases vs timeline	
###### 	Plot 5: Accumulative Deaths vs timeline	

# Introduction

This report contain analysis and visualization of COVID-19 dataset. The data source used for this analysis is the 2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository built by the Center for Systems Science and Engineering, Johns Hopkins University.

# R Packages:
Following R packages are required in running this R code. Make sure these packages are installed on your system before proceeding further. An R package can be installed by running following command: install.packages('package_name')

~~~ 
library(magrittr) # pipe operations
library(lubridate) # date operations
library(tidyverse) # ggplot2, tidyr, dplyr...
library(ggforce) # accelerating ggplot2
library(leaflet) # map
require (rworldmap) # worldmap
~~~ 

Loading Dataset
At first, the datasets, which are three CSV files, are downloaded and saved as local files and then are loaded into R.

##### Loading Dataset
###### load data into R

~~~ 
raw.data.confirmed <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv')
raw.data.deaths <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv')
raw.data.recovered <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv')

n.col <- ncol(raw.data.confirmed)
~~~ 

Below we check the time frame of the data.
~~~ 

## get dates from column names
dates <- names(raw.data.confirmed)[5:n.col] %>% substr(2,8) %>% mdy()
range(dates)

min.date <- min(dates)
max.date <- max(dates)

min.date.txt <- min.date %>% format('%d %b %Y')
max.date.txt <- max.date %>% format('%d %b %Y') %>% paste('UTC')
~~~ 

# Data Cleaning

The three datasets are converted from wide to long format and then are aggregated by country. After that, they are merged into one single dataset
	
## Data Cleaning and Transformation
~~~ 
cleanData <- function(data) {
  ## remove some columns
  data <- data %>% select(-c(Province.State, Lat, Long)) %>% rename(country=Country.Region)
  ## convert from wide to long format
  data <- data %>% gather(key=date, value=count, -country)
  ## convert from character to date
  data <- data %>% mutate(date = date %>% substr(2,8) %>% mdy())
  ## aggregate by country
  data <- data %>% group_by(country, date) %>% summarise(count=sum(count, na.rm=T)) %>% as.data.frame()
  return(data)
}
~~~ 


## Clean the three datasets (confirmed, deaths, recovered)
~~~ 
data.confirmed <- raw.data.confirmed %>% cleanData() %>% rename(confirmed=count)
data.deaths <- raw.data.deaths %>% cleanData() %>% rename(deaths=count)
data.recovered <- raw.data.recovered %>% cleanData() %>% rename(recovered=count)
~~~ 

## merge above 3 datasets into one, by country and date
~~~ 
data <- data.confirmed %>% merge(data.deaths, all=T) %>% merge(data.recovered, all=T)

## countries/regions with confirmed cases, excl. cruise ships
#countries <- data %>% pull(country) %>% setdiff('Cruise Ship')
~~~ 

# Print the 'data' list
~~~ 
data
~~~ 






## Visualizations:

### Plot 1: World map: Death cases percentage of infected cases

Here, we first filter the data based on max date, calculate death cases percentage of infected cases and plot the list of countries on the world map. The we also plot sky-blue colored sub bar graphs for few countries. The height of bar indicate how large ratio of death cases is to that of infected cases.
~~~ 
### PLOT 1: Worldmap: Death cases percentage of infected cases

# Filter the data to current date(max.date)
data_max_date <- subset(data, date == max.date)
# Calculate death_ratio
death_ratio <- (data_max_date[4]/data_max_date[3])*100
# Append the list the data_max_date list
data_max_date$prop <- (death_ratio[,1])
# Print the data
data_max_date ## This list contains number of cases on today's date

sPDF <- joinCountryData2Map(data_max_date
                            , joinCode = "NAME"
                            , nameJoinColumn = "country"
                            , verbose = TRUE)
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")

# Plot worldmap
mapCountryData( sPDF, nameColumnToPlot="prop", colourPalette = "diverging", addLegend = TRUE, borderCol = "grey", 
                mapTitle = "Worldmap: Death cases percentage of infected cases",lwd = 1, oceanCol = "steelblue1")

## Plotting sub Bar Graphs
data_filtered_countries <- subset(data_max_date, country == 'US' |country == 'Canada' | country == 'Brazil' | country == 'Australia' | country == 'India' | country == 'China'  | country == 'Russia' )
data_filtered_countries[7,1] = 'United States of America'

df <- merge(x=data_filtered_countries, y=sPDF@data[sPDF@data$ADMIN, c("ADMIN", "LON", "LAT")], by.x="country", by.y="ADMIN", all.x=TRUE)
require(TeachingDemos)

# plot sub bar graphs
for (i in 1:nrow(df)) 
  subplot(barplot(height=as.numeric(as.character(unlist(df[i, 6], use.names=F))), 
                  axes=F, 
                  col="deepskyblue", ylim=range(df[,6])),
          x=df[i, 'LON'], y=df[i, 'LAT'], size=c(.3, .3))


~~~ 

![enter image description here](https://i.ibb.co/6Rftm3K/Picture-1.png)
## Plot 2: Worldmap: Number of latest confirmed cases
Here, we plot the same data on worldmap and add circle markers. The radius of circle is proportional to number of confirmed cases. 
~~~ 
# PLOT 2: Worldmap: Number of latest confirmed cases

## counts for the whole world
data.world <- data %>% group_by(date) %>%
  summarise(country='World',
            confirmed = sum(confirmed, na.rm=T),
            deaths = sum(deaths, na.rm=T),
            recovered = sum(recovered, na.rm=T))
data <- data %>% rbind(data.world)
## current confirmed cases
data <- data %>% mutate(current.confirmed = confirmed - deaths - recovered)

## sort by country and date
data <- data %>% arrange(country, date)

# ## convert from wide to long format, for drawing area plots
data.long <- data %>%
  select(c(country, date, confirmed, current.confirmed, recovered, deaths)) %>%
  gather(key=type, value=count, -c(country, date))

# Plotting worldmap
x <- raw.data.confirmed
x$confirmed <- x[, ncol(x)]
x <- x %>% select(c(Country.Region, Province.State, Lat, Long, confirmed)) %>%
  mutate(txt=paste0(Country.Region, ' - ', Province.State, ': ', confirmed))
m <- leaflet(width=1200, height=800) %>% addTiles()

# circle marker (units in pixels)
m <- m %>% addCircleMarkers(x$Long, x$Lat,
                        radius=2+log2(x$confirmed), stroke=F,
                        color='red', fillOpacity=0.3,
                        popup=x$txt)
# Display world map
m
~~~ 
![enter image description here](https://i.ibb.co/mvptB0Q/Picture-2.png)

## Plot 3: Number of COVID-19 Cases (confirmed, deaths, recovered) vs Timeline

Here we plot the graph for Number of COVID-19 Cases (confirmed, deaths, recovered) worldwide. 
~~~ 
# PLOT 3: Number of cases vs Timline
world.long <- data.long %>% filter(country == 'World')
## cases - area plot
plot3 <- world.long %>% filter(type != 'Total Confirmed') %>%
  ggplot(aes(x=date, y=count)) +
  geom_area(aes(fill=type), alpha=0.5) +
  labs(title=paste0('Numbers of Cases Worldwide - ', max.date.txt)) +
  scale_fill_manual(values=c('red', 'green', 'black', 'blue')) +
  theme(legend.title=element_blank(), legend.position='bottom',
        plot.title = element_text(size=7),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.key.size=unit(0.2, 'cm'),
        legend.text=element_text(size=6),
        axis.text=element_text(size=7),
        axis.text.x=element_text(angle=45, hjust=1))

plot3
~~~ 
![enter image description here](https://i.ibb.co/rsYBkwT/Picture-3.png)

## Plot 4: Current Confirmed Cases vs timeline
Here, we plot he current confirmed cases versus the timeline.
~~~ 
# PLOT 4:  CONFIRMED CASES VS TIMELINE
data.world <- data %>% filter(country=='World')
n <- nrow(data.world)
## current confirmed and daily new confirmed
plot4 <- ggplot(data.world, aes(x=date, y=current.confirmed)) +
  geom_point() + geom_smooth() +
  xlab('') + ylab('Count') + labs(title='Current Confirmed Cases') +
  theme(axis.text.x=element_text(angle=45, hjust=1))

plot4
~~~ 
![enter image description here](https://i.ibb.co/rmhyM1f/Picture-4.png)

## Plot 5: Accumulative Deaths vs timeline
Here, we plot the accumulated number of death cases vs timeline. 
~~~ 
#PLOT 5: Accumalitive deaths
plot4 <- ggplot(data.world, aes(x=date, y=deaths)) +
  geom_point() + geom_smooth() +
  xlab('') + ylab('Count') + labs(title='Accumulative Deaths') +
  theme(axis.text.x=element_text(angle=45, hjust=1))

plot4
~~~ 
![enter image description here](https://i.ibb.co/m4fJj97/Picture-5.png)





