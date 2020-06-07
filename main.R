#Clear all variables, lists
rm(list = ls()) 

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L


## DATA SOURCE 

#Loading R Packages
# Follownig R packages are required in running this R code. 
#Make sure these packages are installed on your system before proceeding further. 
# An R package can be installed by running following command: install.packages('package_name')


library(magrittr) # pipe operations
library(lubridate) # date operations
library(tidyverse) # ggplot2, tidyr, dplyr...
library(gridExtra) # multiple grid-based plots on a page
library(ggforce) # accelerating ggplot2
library(kableExtra) # complex tables
library(leaflet) # map
require (rworldmap) # worldmap

## Loading Dataset

filenames <- c('time_series_covid19_confirmed_global.csv',
               'time_series_covid19_deaths_global.csv',
               'time_series_covid19_recovered_global.csv')

url.path <- paste0('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/',
                   'master/csse_covid_19_data/csse_covid_19_time_series/')

## download files to local
download <- function(filename) {
  url <- file.path(url.path, filename)
  dest <- file.path('./data', filename)
  download.file(url, dest)
}

bin <- lapply(filenames, download)

## load data into R
raw.data.confirmed <- read.csv('./data/time_series_covid19_confirmed_global.csv')
raw.data.deaths <- read.csv('./data/time_series_covid19_deaths_global.csv')
raw.data.recovered <- read.csv('./data/time_series_covid19_recovered_global.csv')

n.col <- ncol(raw.data.confirmed)

## get dates from column names
dates <- names(raw.data.confirmed)[5:n.col] %>% substr(2,8) %>% mdy()
range(dates)

min.date <- min(dates)
max.date <- max(dates)

min.date.txt <- min.date %>% format('%d %b %Y')
max.date.txt <- max.date %>% format('%d %b %Y') %>% paste('UTC')


## Data Cleaning and Transformation
cleanData <- function(data) {
  ## remove some columns
  data %<>% select(-c(Province.State, Lat, Long)) %>% rename(country=Country.Region)
  ## convert from wide to long format
  data %<>% gather(key=date, value=count, -country)
  ## convert from character to date
  data %<>% mutate(date = date %>% substr(2,8) %>% mdy())
  ## aggregate by country
  data %<>% group_by(country, date) %>% summarise(count=sum(count, na.rm=T)) %>% as.data.frame()
  return(data)
}

## Clean the three datasets (confirmed, deaths, recovered)
data.confirmed <- raw.data.confirmed %>% cleanData() %>% rename(confirmed=count)
data.deaths <- raw.data.deaths %>% cleanData() %>% rename(deaths=count)
data.recovered <- raw.data.recovered %>% cleanData() %>% rename(recovered=count)

## merge above 3 datasets into one, by country and date
data <- data.confirmed %>% merge(data.deaths, all=T) %>% merge(data.recovered, all=T)

## countries/regions with confirmed cases, excl. cruise ships
#countries <- data %>% pull(country) %>% setdiff('Cruise Ship')

# Print the 'data' list
data

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


# PLOT 2: Worldmap: Number of latest confirmed cases

# 3.2 worldwide cases
## counts for the whole world
data.world <- data %>% group_by(date) %>%
  summarise(country='World',
            confirmed = sum(confirmed, na.rm=T),
            deaths = sum(deaths, na.rm=T),
            recovered = sum(recovered, na.rm=T))
data %<>% rbind(data.world)
## current confirmed cases
data %<>% mutate(current.confirmed = confirmed - deaths - recovered)

## sort by country and date
data %<>% arrange(country, date)


# ## convert from wide to long format, for drawing area plots
data.long <- data %>%
  select(c(country, date, confirmed, current.confirmed, recovered, deaths)) %>%
  gather(key=type, value=count, -c(country, date))


# Plotting worldmap
x <- raw.data.confirmed
x$confirmed <- x[, ncol(x)]
x %<>% select(c(Country.Region, Province.State, Lat, Long, confirmed)) %>%
  mutate(txt=paste0(Country.Region, ' - ', Province.State, ': ', confirmed))
m <- leaflet(width=1200, height=800) %>% addTiles()

# circle marker (units in pixels)
m %<>% addCircleMarkers(x$Long, x$Lat,
                        radius=2+log2(x$confirmed), stroke=F,
                        color='red', fillOpacity=0.3,
                        popup=x$txt)
# Display world map
m



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


# PLOT 4:  CONFIRMED CASES VS TIMELINE
data.world <- data %>% filter(country=='World')
n <- nrow(data.world)
## current confirmed and daily new confirmed
plot4 <- ggplot(data.world, aes(x=date, y=current.confirmed)) +
  geom_point() + geom_smooth() +
  xlab('') + ylab('Count') + labs(title='Current Confirmed Cases') +
  theme(axis.text.x=element_text(angle=45, hjust=1))

plot4

#PLOT 5: Accumalitive deaths
plot4 <- ggplot(data.world, aes(x=date, y=deaths)) +
  geom_point() + geom_smooth() +
  xlab('') + ylab('Count') + labs(title='Accumulative Deaths') +
  theme(axis.text.x=element_text(angle=45, hjust=1))

plot4
