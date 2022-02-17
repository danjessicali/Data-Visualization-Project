---
title: "Assignment 1: Using ggplot2 for visualization"
author: Dan Li
always_allow_html: yes
output: 
  html_document:
    keep_md: true
    code_folding: hide
---

Winter Olympics Medals over Time
================================




```r
# Reading in the data
library("readr")
athletes_and_events <- read_csv("data/athletes_and_events.csv")
gdp_pop<- read_csv("data/gdp_pop.csv")
noc_regions <- read_csv("data/noc_regions.csv")
```

## Scenario

Imagine you are the data scientist at a respected media outlet -- say the "New York Times". For the Winter Olympics coverage, your editor-in-chief asks you to analyze some data on the history of `Winter Olympics Medals by Year, Country, Event and Gender` and prepare some data visualizations in which you outline the main patterns around which to base the story.

Since there is **no way that all features of the data can be represented** in such a memo, feel free to pick and choose some patterns that would make for a good story -- outlining important patterns and presenting them in a visually pleasing way. 

The full background and text of the story will be researched by a writer of the magazine -- your input should be based on the data and some common sense (i.e. no need to read up on this). 

Provide **polished plots** that are refined enough to include in the magazine with very little further manipulation (already include variable descriptions [if necessary for understanding], titles, source [e.g. "International Olympic Committee"], right color etc.) and are understandable to the average reader of the "New York Times". The design does not need to be NYTimes-like. Just be consistent.

## Data

The main data is provided as an excel sheet, containing the following variables on all participating athletes in all olympics from 1896 to 2016 (sadly, the original source of the data no longer updates beyond that year):

  - `ID`: a unique indentifier of the entry
  - `Name`: name of the athlete
  - `Sex`: sex of the athlete
  - `Age`: age of the athlete
  - `Height`: height of the athlete
  - `Weight`: weight of the athlete
  - `Team`: usually the country team of the athlete, with the exception of political accomodations, e.g. the "Refugee Olympic Athletes" team.
  - `NOC`: national olympic comittee abbreviation.
  - `Games`: year and season of games.
  - `Year`: year of games
  - `Season`: season of games.
  - `City`: host city
  - `Sport`: a grouping of disciplines
  - `Event`: the particular event / competition  
  - `Medal`: the particular event / competition  

For example, an `event` is a competition in a sport or discipline that gives rise to a ranking. Thus `Alpine Skiing` is the discipline, and `Alpine Skiing Women's Downhills` is a particular event.

In addition, you are provided with some additional information about the countries in a separate spreadsheet, including the `IOC Country	Code`, `Population`, and `GDP per capita`.

## Tasks

#### 1. Medal Counts over Time

a) Combine the information in the three spreadsheets `athletes_and_events.csv`, `noc_regions.csv`, and  `gdp_pop.csv`. Note, that the `noc_regions.csv` is the set all NOC regions, while `gdp_pop.csv` only contains a snapshot of the current set of countries. You have to decide what to do with some [countries that competed under different designations in the past (e.g. Germany and Russia)](https://en.wikipedia.org/wiki/All-time_Olympic_Games_medal_table) and some defunct countries and whether and how to combine their totals. Make sure to be clear about your decisions here, so that the editor (and potentially a user of your visualizations) understands what you did.

I am going to see which regions had been changed the NOC code in the past years using the NOC_regions table. Then, I will update the old NOC in the athletes_and_event table using the new NOC code. After updating the NOC code, I will merge the gdp_pop data to the athletes_and_events data. Below is a sample output data:


```r
#find the defunct country using NOC region list
regions <- noc_regions %>% 
  group_by(region) %>%
  filter(n() > 1)

# map region with correct NOC
noc_regions$NOC_old <- noc_regions$NOC
regions_new <- select(noc_regions,c('region','notes','NOC_old'))
regions_new <- merge(regions_new, gdp_pop,by.x ="NOC_old",by.y=c("Code"),all.x=TRUE)


# create new athletes_and_events table
# in the athletes_and_events_new table, convert the old NOC to current NOC using the notes column from noc_regions
athletes_and_events_new <- athletes_and_events
athletes_and_events_new$NOC_new <- athletes_and_events_new$NOC
athletes_and_events_new['NOC_new'][athletes_and_events_new['NOC_new'] == 'ANZ'] <- 'AUS'
athletes_and_events_new['NOC_new'][athletes_and_events_new['NOC_new'] == 'BOH'] <- 'CZE'
athletes_and_events_new['NOC_new'][athletes_and_events_new['NOC_new'] == 'TCH'] <- 'CZE'
athletes_and_events_new['NOC_new'][athletes_and_events_new['NOC_new'] == 'FRG'] <- 'GER'
athletes_and_events_new['NOC_new'][athletes_and_events_new['NOC_new'] == 'GDR'] <- 'GER'
athletes_and_events_new['NOC_new'][athletes_and_events_new['NOC_new'] == 'SAA'] <- 'GER'
athletes_and_events_new['NOC_new'][athletes_and_events_new['NOC_new'] == 'MAL'] <- 'MAS'
athletes_and_events_new['NOC_new'][athletes_and_events_new['NOC_new'] == 'NBO'] <- 'MAS'
athletes_and_events_new['NOC_new'][athletes_and_events_new['NOC_new'] == 'EUN'] <- 'RUS'
athletes_and_events_new['NOC_new'][athletes_and_events_new['NOC_new'] == 'URS'] <- 'RUS'
athletes_and_events_new['NOC_new'][athletes_and_events_new['NOC_new'] == 'SRB'] <- 'SCG'
athletes_and_events_new['NOC_new'][athletes_and_events_new['NOC_new'] == 'YUG'] <- 'SCG'
athletes_and_events_new['NOC_new'][athletes_and_events_new['NOC_new'] == 'UAR'] <- 'SYR'
athletes_and_events_new['NOC_new'][athletes_and_events_new['NOC_new'] == 'TTO'] <- 'TRI'
athletes_and_events_new['NOC_new'][athletes_and_events_new['NOC_new'] == 'WIF'] <- 'TRI'
athletes_and_events_new['NOC_new'][athletes_and_events_new['NOC_new'] == 'VNM'] <- 'VIE'
athletes_and_events_new['NOC_new'][athletes_and_events_new['NOC_new'] == 'YAR'] <- 'YEM'
athletes_and_events_new['NOC_new'][athletes_and_events_new['NOC_new'] == 'VNM'] <- 'YMD'
athletes_and_events_new['NOC_new'][athletes_and_events_new['NOC_new'] == 'RHO'] <- 'ZIM'
athletes_and_events_new['NOC_new'][athletes_and_events_new['NOC_new'] == 'CRT'] <- 'GRE'
athletes_and_events_new['NOC_new'][athletes_and_events_new['NOC_new'] == 'HKG'] <- 'CHN'
athletes_and_events_new['NOC_new'][athletes_and_events_new['NOC_new'] == 'NFL'] <- 'CAN'


# get the current country information for gdp_pop based on NOC_new and Code
athletes_and_events_new <- merge(athletes_and_events_new, gdp_pop, by.x ="NOC_new",by.y=c("Code"),all.x=TRUE)
```

```r
as_table1 <- athletes_and_events_new[1:3,]
datatable(as_table1, class = 'cell-border stripe', colnames = c("NOC_new","ID","Name","Sex","Age","Height","Weight"       ,"Team","NOC" ,"Games","Year","Season","City","Sport","Event","Medal","Country","Population","GDP per Capita"))
```

```{=html}
<div id="htmlwidget-309b072be49bdde74ce9" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-309b072be49bdde74ce9">{"x":{"filter":"none","vertical":false,"data":[["1","2","3"],["AFG","AFG","AFG"],[106372,109113,31285],["Habib Zareef Sayed","Abouwi Ahmad Shah","Mohammad Ebrahimi"],["M","M","M"],[23,null,30],[170,null,160],[58,null,63],["Afghanistan","Afghanistan","Afghanistan"],["AFG","AFG","AFG"],["1960 Summer","1936 Summer","1968 Summer"],[1960,1936,1968],["Summer","Summer","Summer"],["Roma","Berlin","Mexico City"],["Athletics","Hockey","Wrestling"],["Athletics Men's 400 metres","Hockey Men's Hockey","Wrestling Men's Featherweight, Freestyle"],[null,null,null],["Afghanistan","Afghanistan","Afghanistan"],[32526562,32526562,32526562],[594.323081219966,594.323081219966,594.323081219966]],"container":"<table class=\"cell-border stripe\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>NOC_new<\/th>\n      <th>ID<\/th>\n      <th>Name<\/th>\n      <th>Sex<\/th>\n      <th>Age<\/th>\n      <th>Height<\/th>\n      <th>Weight<\/th>\n      <th>Team<\/th>\n      <th>NOC<\/th>\n      <th>Games<\/th>\n      <th>Year<\/th>\n      <th>Season<\/th>\n      <th>City<\/th>\n      <th>Sport<\/th>\n      <th>Event<\/th>\n      <th>Medal<\/th>\n      <th>Country<\/th>\n      <th>Population<\/th>\n      <th>GDP per Capita<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,5,6,7,11,18,19]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

b) Calculate a summary of how many winter games each country competed in, and how many medals of each type the country won. Use that summary to provide a **visual comparison of medal count by country**. 

Feel free to focus on smaller set of countries (say the top 10), highlight the United States or another country of your choice, consider gender of the medal winners etc. to make the visualization interesting. 

Please provide (i) one visualization showing an over time comparison and (ii) one visualization in which a total medal count (across all Winter Olympics) is used. Briefly discuss which visualization you recommend to your editor and why.

**Note:** Currently, the medal data contains information on _each athlete_ competing, including for team events. For example, in 2014 Russia received _4 gold medals for their men's win in Bobsleigh Men's Four_ alone. Since this is usually not how it is done in official medal statistics, try to wrangle the data so that _team events are counted as a single medal_. 

I am going to select the top 10 countries that participated the most # of different winter games.
Based on the calculation below, I'm going to select below countries: 
United Kingdom (GBR), United States (USA), France (FRA), Canada (CAN), Germany (GER), Australia (AUS), Italy (ITA), Japan (JPN), Switzerland (SUI) and Russia (RUS). Below is a sample output of the calculation results, which is by country, sport typs and medal type:



b-1 
Calculate a summary of how many winter games each country competed in. The sample results are below:


```r
# Calculate a summary of how many winter games each country competed in
# this is by country by year
winter <- filter(athletes_and_events_new, Season == "Winter")
winter_games <- aggregate(data = winter,                
                          Year ~ Country,
                          function(Year) length(unique(Year)))

names(winter_games)[names(winter_games) == 'Year'] <- "Number of Games"
winter_games[1:5,]
```

```
##           Country Number of Games
## 1         Albania               3
## 2         Algeria               3
## 3 American Samoa*               1
## 4         Andorra              11
## 5       Argentina              18
```


b-2
Calculate how many medals of each type the country won. The Sample results are below:


```r
# only keep one record for per country & year & sport combination & Medal
winter_new <- select(winter,c('Country','Year','Sport','Medal'))
winter_new <- winter_new %>% distinct(Country, Year, Sport, Medal)

# Calculate how many medals of each type the country won by year and sports
medal_count <- winter_new %>%
  group_by(Country, Medal) %>%
  summarize(n = n())
Medal_count_new <- medal_count %>% drop_na(Medal)
Medal_count_new
```

```
## # A tibble: 104 × 3
## # Groups:   Country [41]
##    Country   Medal      n
##    <chr>     <chr>  <int>
##  1 Australia Bronze     4
##  2 Australia Gold       6
##  3 Australia Silver     3
##  4 Austria   Bronze    50
##  5 Austria   Gold      38
##  6 Austria   Silver    49
##  7 Belarus   Bronze     5
##  8 Belarus   Gold       3
##  9 Belarus   Silver     4
## 10 Belgium   Bronze     3
## # … with 94 more rows
```

(i) one visualization showing an over time comparison


```r
# To show the medal count change over time for the selected ten countries, including gender and sports types.
df_b <- select(winter,c('Country','Year','Sport','Sex', 'Medal')) %>%
  filter(Country %in% c('United Kingdom', 'United States', 'France', 'Canada', 'Germany', 'Australia', 'Italy', 'Japan', 'Switzerland', 'Russia'))

df_b_Medal <- df_b %>% drop_na(Medal) %>%
  group_by(Country, Year, Medal) %>%
  summarize(n = n())

ggplot(data = df_b_Medal, aes(fill = Medal, y=n,x=Year)) +
   geom_col() +
  scale_fill_manual("legend", values = c("Bronze" = "#006400", "Silver" = "#C0C0C0", "Gold" = "#8B4513"))+
  labs(title = "Top 10 Countries Medal Count Over Years",
       y = "Medal Count", x = "Year") + 
  facet_wrap(~ Country,nrow = 5)
```

![](assignment1_winter_olympics_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


(ii) one visualization in which a total medal count (across all Winter Olympics) is used



```r
df_b_Medal_1 <- df_b %>% drop_na(Medal) %>%
  group_by(Country, Medal) %>%
  summarize(n = n())

color_table <- tibble(
  Medal = c("Bronze", "Silver", "Gold"),
  Color = c("#b08d57", "#C0C0C0", "#FFD700")
  )

ggplot(data = df_b_Medal_1, aes(x = Medal, y = n, fill = Medal)) +
   geom_col() +
  scale_fill_manual("legend", values = c("Bronze" = "#006400", "Silver" = "#C0C0C0", "Gold" = "#8B4513")) +
  labs(title = "Top 10 Countries Medal Count Over Years",
       y = "Medal Count") +
  facet_wrap(~Country, nrow = 5)
```

![](assignment1_winter_olympics_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


For the two visualizations above, I will recommend the second graph for two reasons: 
1. It is easy to read the number of different types of medal.
2. It shows a clearer comparison for the medal counts among the ten countries. Although the first visualization contain more information, it is hard to conclude useful insights.

#### 2. Medal Counts adjusted by Population, GDP

There are different ways to calculate "success". Consider the following variants and choose one (and make sure your choice is clear in the visualization):  
  - Just consider gold medals.  
  - Simply add up the number of medals of different types.    
  - Create an index in which medals are valued differently. (gold=3, silver=2, bronze=1).   
  - A reasonable other way that you prefer.
  
Now, adjust the ranking of medal success by (a) GDP per capita and (b) population. You have now three rankings: unadjusted ranking, adjusted by GDP per capita, and adjusted by population.

I am going to use the above ten countries for the winter games only. The method of calculating "success" that I am going to use is only consider "gold" medal, and will times 10000 to the gold medel count in order to adjust for the GDP and population. I am going to sum up the gold medal for each country then calculate the adjusted rank. Below is the calculation for gold medal and the adjusted results.


```r
# Just consider gold medals.  
df_2 <- filter(winter, Medal == "Gold") %>% filter(Country %in% c('United Kingdom', 'United States', 'France', 'Canada', 'Germany', 'Australia', 'Italy', 'Japan', 'Switzerland', 'Russia'))

df_2 <- select(df_2,c('Country','Medal','Population', 'GDP per Capita'))
df_2_new <- select(df_2,c('Country','Population', 'GDP per Capita'))

df_2_Medal <- df_2 %>% drop_na(Medal) %>%
  group_by(Country, Medal) %>%
  summarize(n = n())

df_2_Medal_new <- merge(df_2_Medal, df_2_new, by.x ="Country",by.y=c("Country"),all.x=TRUE)
df_2_Medal_new <- df_2_Medal_new %>% distinct_()

names(df_2_Medal_new)[names(df_2_Medal_new) == 'GDP'] <- "GDP per Capita"
# adjust the ranking of medal success by 
# (a) GDP per capita
df_2_Medal_new$adj_GDP <- df_2_Medal_new$n * 10000 / df_2_Medal_new$GDP
# (b) population
df_2_Medal_new$adj_pop <- df_2_Medal_new$n * 10000 / df_2_Medal_new$Population
df_2_Medal_new
```

```
##           Country Medal   n Population GDP per Capita    adj_GDP     adj_pop
## 1       Australia  Gold   6   23781169      56310.963   1.065512 0.002523005
## 2          Canada  Gold 305   35851774      43248.530  70.522628 0.085072499
## 3          France  Gold  36   66808385      36205.568   9.943222 0.005388545
## 4         Germany  Gold 226   81413145      41313.314  54.703915 0.027759645
## 5           Italy  Gold  57   60802085      29957.804  19.026762 0.009374679
## 6           Japan  Gold  17  126958472      32477.215   5.234439 0.001339021
## 7          Russia  Gold 379  144096812       9092.581 416.823363 0.026301762
## 8     Switzerland  Gold  76    8286976      80945.079   9.389082 0.091710173
## 9  United Kingdom  Gold  42   65138232      43875.970   9.572438 0.006447826
## 10  United States  Gold 166  321418820      56115.718  29.581729 0.005164601
```

Visualize how these rankings differ. Try to highlight a specific pattern (e.g. "South Korea -- specialization reaps benefits" or "The superpowers losing their grip").


```r
ggplot(df_2_Medal_new, aes(x=reorder(Country, -n), y=n)) +
  geom_col() +
  ggtitle("Unadjusted Ranking for Top 10 Countries") + 
  xlab("Country") +
  ylab(" Gold Medal Count")
```

![](assignment1_winter_olympics_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
ggplot(df_2_Medal_new, aes(x=reorder(Country, -adj_GDP), y=adj_GDP)) +
  geom_col() +
  ggtitle("Adjusted Ranking for Top 10 Countries by GDP") + 
  xlab("Country") +
  ylab(" Gold Medal Count adjusted by GDP")
```

![](assignment1_winter_olympics_files/figure-html/unnamed-chunk-10-1.png)<!-- -->


```r
ggplot(df_2_Medal_new, aes(x=reorder(Country, -adj_pop), y=adj_pop)) +
  geom_col() +
  ggtitle("Adjusted Ranking for Top 10 Countries by Population") + 
  xlab("Country") +
  ylab(" Gold Medal Count adjusted by Population")
```

![](assignment1_winter_olympics_files/figure-html/unnamed-chunk-11-1.png)<!-- -->



The above graphs are the un-adjusted ranking, adjusted by GDP per capita, and adjusted by population for the 10 countries in winter Olympics. Canada, Russia and Germany are the three countries that ranked high in these three types of ranking methods. Australia, Japan and United States are the three countires that ranked low in these three types of ranking methods.


#### 3. Host Country Advantage

Until the 2014 Sochi Winter Olympics (our data for Winter Olympics end here), there were 19 host cities. Calculate whether the host nation had an advantage. That is calculate whether the host country did win more medals when the Winter Olympics was in their country compared to other times. 

Note, that the 19 host cities are noted in the data but not the countries they are located in. This happens commonly and often Wikipedia has the [kind of additional data you want for the task](https://en.wikipedia.org/wiki/Winter_Olympic_Games). To save you some time, here is a quick way to get this kind of table from Wikipedia into R:


```r
library(rvest)
library(stringr)
library(tidyverse)
wiki_hosts <- read_html("https://en.wikipedia.org/wiki/List_of_Olympic_Games_host_cities")
hosts <- html_table(html_nodes(wiki_hosts, "table")[[2]], fill=TRUE)[-1]
hosts <- hosts %>% filter(Winter != "") %>%
  select(City, Country, Year)
```


Below are the host countries list:



```r
# Clean the host country data
# Combine Russia and Change South Korea data
hosts['Country'][hosts['Country'] == 'Russia[h]'] <- 'Russia'
hosts['Country'][hosts['Country'] == 'Soviet Union[h]'] <- 'Russia'
hosts['Country'][hosts['Country'] == 'South Korea'] <- 'Korea South'

# Remove the year that did not host game
hosts <- hosts %>% filter(!Year %in% c('1940','1944'))
host_country <- c(distinct(hosts, Country))
host_country
```

```
## $Country
##  [1] "France"        "Switzerland"   "United States" "Germany"      
##  [5] "Norway"        "Italy"         "Austria"       "Japan"        
##  [9] "Yugoslavia"    "Canada"        "Russia"        "Korea South"  
## [13] "China"
```


```r
# Identify the host year information in the full data set
names(hosts)[names(hosts) == 'Year'] <- "Host_Year"
hosts$Flag <- "Yes"
# filter the hosting country and get the medal total count
df_3 <- winter %>% filter(Country %in% hosts$Country)

df_3_new <- select(df_3,c('Country','Year','Medal')) %>% drop_na(Medal) %>%
  group_by(Country, Year) %>%
  summarize(n = n())

df_3_new <- merge(df_3_new, select(hosts,c('Country',"Host_Year",'Flag')), 
                  by.x =c("Country","Year"),
                  by.y=c("Country","Host_Year"),all.x=TRUE)

# remove China, since it is the host country for 2022, and we don't have the data yet.
df_3_new <- df_3_new  %>% filter(!Country %in% c('China'))
```

Provide a visualization of the host country advantage (or absence thereof).


```r
df_3_new %>% 
  mutate(highlight_flag = ifelse(Flag == 'Yes', T, F)) %>% 
  ggplot(aes(x = Year, y = n)) +
    geom_col(aes(fill = highlight_flag))  + 
    ggtitle("The Host Countries Medal Count Change by Year") + 
  scale_fill_discrete( name = "Host Year", labels = c("Host Year", "NA"))  + 
  xlab("Medal Count") +
  ylab("Year") +
  facet_wrap(~ Country,nrow = 5)
```

![](assignment1_winter_olympics_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

From the above graph, we can see the medal count change for the hosting countries over year (the host year is in red color). Overall, not all countries generated large difference in the medal count for being host country. 

For example, France is the host country for 1924, 1968 and 1992.For Germany,there was a decline in medal count for the host year. However, there is no obvious change for the medal count when France is the hosting country. But, for the U.S.A, the host country for 1932, 1960, 1980 and 2002, we can see in 2004 had an increase in medal count comparing to 2000 and 2008. 
Overall,the medal trends are smooth for Japan, Austria, Italy, France and Canada. For United States, Russia, Switzerland and Norway, there exist a "host country effect", as the medal counts are relatively higher than other years.


#### 4. Most successful athletes

a) Now, let's look at the most successful athletes. Provide a visual display of the most successful Winter Olympics athletes of all time.


```r
athletes <- winter %>%  drop_na(Medal) %>%
  group_by(Country, Name, Medal) %>%
  tally() %>%
  spread(Medal, n) %>%
  as.data.frame() %>%
  mutate(Total = rowSums(.[3:5])) %>%
  arrange(desc(Total))

top_10_athletes <- athletes[1:10,]
top_10_athletes
```

```
##    Country                                 Name Bronze Gold Silver Total
## 1   Norway                  Ole Einar Bjrndalen      1    8      4    13
## 2    China                            Yang Yang      2    2      6    10
## 3    Italy                    Stefania Belmondo      5    2      3    10
## 4   Norway                         Marit Bjrgen      1    6      3    10
## 5   Russia             Raisa Petrovna Smetanina      1    4      5    10
## 6  Germany                    Claudia Pechstein      2    5      2     9
## 7  Germany                  Ursula "Uschi" Disl      3    2      4     9
## 8   Sweden                  Edy Sixten Jernberg      2    4      3     9
## 9  Germany    Gunda Niemann-Stirnemann-Kleemann      1    3      4     8
## 10 Germany Karin Enke-Kania (-Busch-, -Richter)      1    3      4     8
```

```r
ggplot(top_10_athletes, aes(x = Total, y = Name, color = Country)) +
  geom_point(size = 4) +
  geom_segment(aes(xend = 0, yend = Name), size = 2) +
  geom_text(aes(label = Total), color = "white", size = 2.5)+
  labs(title = "Top 10 Athletes Total Medal Summary", caption = "Source: Athletes_and_Event.csv") + theme(legend.position = "top", axis.title.y = element_blank())
```

![](assignment1_winter_olympics_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

b) Chose of of the athlete specific dimensions (e.g. gender, height, weight) and visualize an interesting pattern in the data.

I chose age as the athlete specific dimension:


```r
winter_sub <- select(winter,c('Year','Name','Age', 'Medal')) %>% filter(Name %in% top_10_athletes$Name) %>% drop_na(Medal) %>% group_by(Year, Name, Age, Medal) %>%
  summarize(Count = n()) %>%
  arrange(desc(Name))

winter_sub
```

```
## # A tibble: 73 × 5
## # Groups:   Year, Name, Age [42]
##     Year Name                      Age Medal  Count
##    <dbl> <chr>                   <dbl> <chr>  <int>
##  1  1998 "Yang Yang"                20 Silver     3
##  2  1998 "Yang Yang"                21 Silver     1
##  3  2002 "Yang Yang"                24 Bronze     1
##  4  2002 "Yang Yang"                24 Silver     1
##  5  2002 "Yang Yang"                25 Gold       2
##  6  2002 "Yang Yang"                25 Silver     1
##  7  2006 "Yang Yang"                29 Bronze     1
##  8  1992 "Ursula \"Uschi\" Disl"    21 Silver     1
##  9  1994 "Ursula \"Uschi\" Disl"    23 Bronze     1
## 10  1994 "Ursula \"Uschi\" Disl"    23 Silver     1
## # … with 63 more rows
```

```r
ggplot(data = winter_sub, aes(fill = Medal, y=Count,x=Age)) +
   geom_col() +
  scale_fill_manual("legend", values = c("Bronze" = "#006400", "Silver" = "#C0C0C0", "Gold" = "#8B4513"))+
  labs(title = "Top 10 Althetes Medal Count Over Age",
       y = "Medal Count", x = "Age (year)") + 
  facet_wrap(~ Name,nrow = 5)
```

![](assignment1_winter_olympics_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

The above graph shows the medal count changes for the top 10 athletes by age. We can conclude two interesting founding from the above graph:
1. Most of the athletes did not won the Gold medal in their first competition, which make sense that they lack experience in their first game. 
2. Most of the athletes won medals during the age rage of 20-35. Only Raisa Petrovna Smetanina and Ole Einar Bjrndalen won medals in the age range 35-40.

### Interactivity

#### 5. Make two plots interactive

Choose 2 of the plots you created above and add interactivity. One of the plots needs to be written in `plotly` rather than just using the `ggplotly` automation. Briefly describe to the editor why interactivity in these visualization is particularly helpful for a reader.

1) Interactive plot using `ggplotly` to represent the top 10 Athletes total medal summary for winter Olympics (the graph for question 4)



```r
g <- ggplot(top_10_athletes, aes(x = Total, y = Name, color = Country)) +
  geom_point(size = 4) +
  geom_segment(aes(xend = 0, yend = Name), size = 2) +
  geom_text(aes(label = Total), color = "white", size = 2.5)+
  labs(title = "Top 10 Athletes Total Medal Summary", caption = "Source: Athletes_and_Event.csv",  x = "Total Medal Count") +   theme(legend.position = "top", axis.title.y = element_blank())

ggplotly(g, tooltip =c("Name", "Country"))
```

```{=html}
<div id="htmlwidget-d50a2f4e6438ed339f67" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-d50a2f4e6438ed339f67">{"x":{"data":[{"x":[10],"y":[10],"text":"Name: Yang Yang<br />Country: China","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(248,118,109,1)","opacity":1,"size":15.1181102362205,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(248,118,109,1)"}},"hoveron":"points","name":"China","legendgroup":"China","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[9,9,8,8],"y":[1,9,3,4],"text":["Name: Claudia Pechstein<br />Country: Germany","Name: Ursula \"Uschi\" Disl<br />Country: Germany","Name: Gunda Niemann-Stirnemann-Kleemann<br />Country: Germany","Name: Karin Enke-Kania (-Busch-, -Richter)<br />Country: Germany"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(183,159,0,1)","opacity":1,"size":15.1181102362205,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(183,159,0,1)"}},"hoveron":"points","name":"Germany","legendgroup":"Germany","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[10],"y":[8],"text":"Name: Stefania Belmondo<br />Country: Italy","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,186,56,1)","opacity":1,"size":15.1181102362205,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,186,56,1)"}},"hoveron":"points","name":"Italy","legendgroup":"Italy","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[13,10],"y":[6,5],"text":["Name: Ole Einar Bjrndalen<br />Country: Norway","Name: Marit Bjrgen<br />Country: Norway"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,191,196,1)","opacity":1,"size":15.1181102362205,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,191,196,1)"}},"hoveron":"points","name":"Norway","legendgroup":"Norway","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[10],"y":[7],"text":"Name: Raisa Petrovna Smetanina<br />Country: Russia","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(97,156,255,1)","opacity":1,"size":15.1181102362205,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(97,156,255,1)"}},"hoveron":"points","name":"Russia","legendgroup":"Russia","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[9],"y":[2],"text":"Name: Edy Sixten Jernberg<br />Country: Sweden","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(245,100,227,1)","opacity":1,"size":15.1181102362205,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(245,100,227,1)"}},"hoveron":"points","name":"Sweden","legendgroup":"Sweden","showlegend":true,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[10,0],"y":[10,10],"text":"Name: Yang Yang<br />Country: China<br />Name: 10","type":"scatter","mode":"lines","line":{"width":7.55905511811024,"color":"rgba(248,118,109,1)","dash":"solid"},"hoveron":"points","name":"China","legendgroup":"China","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[9,0,null,9,0,null,8,0,null,8,0],"y":[1,1,null,9,9,null,3,3,null,4,4],"text":["Name: Claudia Pechstein<br />Country: Germany<br />Name:  1","Name: Claudia Pechstein<br />Country: Germany<br />Name:  1",null,"Name: Ursula \"Uschi\" Disl<br />Country: Germany<br />Name:  9","Name: Ursula \"Uschi\" Disl<br />Country: Germany<br />Name:  9",null,"Name: Gunda Niemann-Stirnemann-Kleemann<br />Country: Germany<br />Name:  3","Name: Gunda Niemann-Stirnemann-Kleemann<br />Country: Germany<br />Name:  3",null,"Name: Karin Enke-Kania (-Busch-, -Richter)<br />Country: Germany<br />Name:  4","Name: Karin Enke-Kania (-Busch-, -Richter)<br />Country: Germany<br />Name:  4"],"type":"scatter","mode":"lines","line":{"width":7.55905511811024,"color":"rgba(183,159,0,1)","dash":"solid"},"hoveron":"points","name":"Germany","legendgroup":"Germany","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[10,0],"y":[8,8],"text":"Name: Stefania Belmondo<br />Country: Italy<br />Name:  8","type":"scatter","mode":"lines","line":{"width":7.55905511811024,"color":"rgba(0,186,56,1)","dash":"solid"},"hoveron":"points","name":"Italy","legendgroup":"Italy","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[13,0,null,10,0],"y":[6,6,null,5,5],"text":["Name: Ole Einar Bjrndalen<br />Country: Norway<br />Name:  6","Name: Ole Einar Bjrndalen<br />Country: Norway<br />Name:  6",null,"Name: Marit Bjrgen<br />Country: Norway<br />Name:  5","Name: Marit Bjrgen<br />Country: Norway<br />Name:  5"],"type":"scatter","mode":"lines","line":{"width":7.55905511811024,"color":"rgba(0,191,196,1)","dash":"solid"},"hoveron":"points","name":"Norway","legendgroup":"Norway","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[10,0],"y":[7,7],"text":"Name: Raisa Petrovna Smetanina<br />Country: Russia<br />Name:  7","type":"scatter","mode":"lines","line":{"width":7.55905511811024,"color":"rgba(97,156,255,1)","dash":"solid"},"hoveron":"points","name":"Russia","legendgroup":"Russia","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[9,0],"y":[2,2],"text":"Name: Edy Sixten Jernberg<br />Country: Sweden<br />Name:  2","type":"scatter","mode":"lines","line":{"width":7.55905511811024,"color":"rgba(245,100,227,1)","dash":"solid"},"hoveron":"points","name":"Sweden","legendgroup":"Sweden","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[13,10,10,10,10,9,9,9,8,8],"y":[6,10,8,5,7,1,9,2,3,4],"text":[13,10,10,10,10,9,9,9,8,8],"hovertext":["Name: Ole Einar Bjrndalen<br />Country: white","Name: Yang Yang<br />Country: white","Name: Stefania Belmondo<br />Country: white","Name: Marit Bjrgen<br />Country: white","Name: Raisa Petrovna Smetanina<br />Country: white","Name: Claudia Pechstein<br />Country: white","Name: Ursula \"Uschi\" Disl<br />Country: white","Name: Edy Sixten Jernberg<br />Country: white","Name: Gunda Niemann-Stirnemann-Kleemann<br />Country: white","Name: Karin Enke-Kania (-Busch-, -Richter)<br />Country: white"],"textfont":{"size":9.4488188976378,"color":"rgba(255,255,255,1)"},"type":"scatter","mode":"text","hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":43.7625570776256,"r":7.30593607305936,"b":40.1826484018265,"l":221.369863013699},"plot_bgcolor":"rgba(235,235,235,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":{"text":"Top 10 Athletes Total Medal Summary","font":{"color":"rgba(0,0,0,1)","family":"","size":17.5342465753425},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-0.65,13.65],"tickmode":"array","ticktext":["0","5","10"],"tickvals":[0,5,10],"categoryorder":"array","categoryarray":["0","5","10"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"y","title":{"text":"Total Medal Count","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0.4,10.6],"tickmode":"array","ticktext":["Claudia Pechstein","Edy Sixten Jernberg","Gunda Niemann-Stirnemann-Kleemann","Karin Enke-Kania (-Busch-, -Richter)","Marit Bjrgen","Ole Einar Bjrndalen","Raisa Petrovna Smetanina","Stefania Belmondo","Ursula \"Uschi\" Disl","Yang Yang"],"tickvals":[1,2,3,4,5,6,7,8,9,10],"categoryorder":"array","categoryarray":["Claudia Pechstein","Edy Sixten Jernberg","Gunda Niemann-Stirnemann-Kleemann","Karin Enke-Kania (-Busch-, -Richter)","Marit Bjrgen","Ole Einar Bjrndalen","Raisa Petrovna Smetanina","Stefania Belmondo","Ursula \"Uschi\" Disl","Yang Yang"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(255,255,255,1)","gridwidth":0.66417600664176,"zeroline":false,"anchor":"x","title":{"text":"","font":{"color":null,"family":null,"size":0}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":true,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895},"title":{"text":"Country","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"11ab0738eb15e":{"x":{},"y":{},"colour":{},"type":"scatter"},"11ab01d1ecd97":{"x":{},"y":{},"colour":{},"xend":{},"yend":{}},"11ab0514b8370":{"x":{},"y":{},"colour":{},"label":{}}},"cur_data":"11ab0738eb15e","visdat":{"11ab0738eb15e":["function (y) ","x"],"11ab01d1ecd97":["function (y) ","x"],"11ab0514b8370":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

2) Interactive plot using `plotly` that represent the country rank in gold medal count (the interactive plot for question 2).



```r
library(plotly)
fig <- plot_ly(df_2_Medal_new, x = ~Country, y = ~n, type = 'bar',color= ~Country) %>% layout(title="Top 10 Gold Medal Count Country ")
fig
```

```
## Warning in RColorBrewer::brewer.pal(N, "Set2"): n too large, allowed maximum for palette Set2 is 8
## Returning the palette you asked for with that many colors

## Warning in RColorBrewer::brewer.pal(N, "Set2"): n too large, allowed maximum for palette Set2 is 8
## Returning the palette you asked for with that many colors
```

```{=html}
<div id="htmlwidget-6dd67d9ab6d8b1747927" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-6dd67d9ab6d8b1747927">{"x":{"visdat":{"11ab0e2f9c8d":["function () ","plotlyVisDat"]},"cur_data":"11ab0e2f9c8d","attrs":{"11ab0e2f9c8d":{"x":{},"y":{},"color":{},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":"Top 10 Gold Medal Count Country ","xaxis":{"domain":[0,1],"automargin":true,"title":"Country","type":"category","categoryorder":"array","categoryarray":["Australia","Canada","France","Germany","Italy","Japan","Russia","Switzerland","United Kingdom","United States"]},"yaxis":{"domain":[0,1],"automargin":true,"title":"n"},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"x":["Australia"],"y":[6],"type":"bar","name":"Australia","marker":{"color":"rgba(102,194,165,1)","line":{"color":"rgba(102,194,165,1)"}},"textfont":{"color":"rgba(102,194,165,1)"},"error_y":{"color":"rgba(102,194,165,1)"},"error_x":{"color":"rgba(102,194,165,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["Canada"],"y":[305],"type":"bar","name":"Canada","marker":{"color":"rgba(228,156,113,1)","line":{"color":"rgba(228,156,113,1)"}},"textfont":{"color":"rgba(228,156,113,1)"},"error_y":{"color":"rgba(228,156,113,1)"},"error_x":{"color":"rgba(228,156,113,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["France"],"y":[36],"type":"bar","name":"France","marker":{"color":"rgba(201,152,157,1)","line":{"color":"rgba(201,152,157,1)"}},"textfont":{"color":"rgba(201,152,157,1)"},"error_y":{"color":"rgba(201,152,157,1)"},"error_x":{"color":"rgba(201,152,157,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["Germany"],"y":[226],"type":"bar","name":"Germany","marker":{"color":"rgba(175,154,200,1)","line":{"color":"rgba(175,154,200,1)"}},"textfont":{"color":"rgba(175,154,200,1)"},"error_y":{"color":"rgba(175,154,200,1)"},"error_x":{"color":"rgba(175,154,200,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["Italy"],"y":[57],"type":"bar","name":"Italy","marker":{"color":"rgba(226,148,184,1)","line":{"color":"rgba(226,148,184,1)"}},"textfont":{"color":"rgba(226,148,184,1)"},"error_y":{"color":"rgba(226,148,184,1)"},"error_x":{"color":"rgba(226,148,184,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["Japan"],"y":[17],"type":"bar","name":"Japan","marker":{"color":"rgba(176,208,99,1)","line":{"color":"rgba(176,208,99,1)"}},"textfont":{"color":"rgba(176,208,99,1)"},"error_y":{"color":"rgba(176,208,99,1)"},"error_x":{"color":"rgba(176,208,99,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["Russia"],"y":[379],"type":"bar","name":"Russia","marker":{"color":"rgba(227,217,62,1)","line":{"color":"rgba(227,217,62,1)"}},"textfont":{"color":"rgba(227,217,62,1)"},"error_y":{"color":"rgba(227,217,62,1)"},"error_x":{"color":"rgba(227,217,62,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["Switzerland"],"y":[76],"type":"bar","name":"Switzerland","marker":{"color":"rgba(245,207,100,1)","line":{"color":"rgba(245,207,100,1)"}},"textfont":{"color":"rgba(245,207,100,1)"},"error_y":{"color":"rgba(245,207,100,1)"},"error_x":{"color":"rgba(245,207,100,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["United Kingdom"],"y":[42],"type":"bar","name":"United Kingdom","marker":{"color":"rgba(219,192,155,1)","line":{"color":"rgba(219,192,155,1)"}},"textfont":{"color":"rgba(219,192,155,1)"},"error_y":{"color":"rgba(219,192,155,1)"},"error_x":{"color":"rgba(219,192,155,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["United States"],"y":[166],"type":"bar","name":"United States","marker":{"color":"rgba(179,179,179,1)","line":{"color":"rgba(179,179,179,1)"}},"textfont":{"color":"rgba(179,179,179,1)"},"error_y":{"color":"rgba(179,179,179,1)"},"error_x":{"color":"rgba(179,179,179,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```



#### 6. Data Table

Prepare a selected data set and add a `datatable` to the output. Make sure the columns are clearly labelled. Select the appropriate options for the data table (e.g. search bar, sorting, column filters etc.). Suggest to the editor which kind of information you would like to provide in a data table in the online version of the article and why.



I am going to select the information for Freestyle Skiing (Year, Athletes' information, medal type and medal count). I recommend include these information to the online version of the article due to: 

1) People are interested in Freestyle Skiing, and it has relatively less information (started from 1992).

2) There are female and male athletes in this type of sports. The male and female audiences could find their favoriate athletes.

3) Showing the medal count could represent the ability of the athletes.


```r
astable <- select(winter,c('Year','Name','Sex', 'Medal','Sport')) %>% 
  filter(Sport == 'Freestyle Skiing') %>% 
  drop_na(Medal) %>% 
  group_by(Year, Name, Sex, Medal) %>%
  summarize(Count = n()) %>%
  arrange(Year)
astable
```

```
## # A tibble: 102 × 5
## # Groups:   Year, Name, Sex [102]
##     Year Name                                    Sex   Medal  Count
##    <dbl> <chr>                                   <chr> <chr>  <int>
##  1  1992 "Donna L. Weinbrecht"                   F     Gold       1
##  2  1992 "Edgar Alain Grospiron"                 M     Gold       1
##  3  1992 "Nelson Graham Carmichael"              M     Bronze     1
##  4  1992 "Olivier Allamand"                      M     Silver     1
##  5  1992 "Stine Lise Hattestad (-Bratsberg)"     F     Bronze     1
##  6  1992 "Yelizaveta Aleksandrovna Kozhevnikova" F     Silver     1
##  7  1994 "Andreas \"Sonny\" Schnbchler"          M     Gold       1
##  8  1994 "Edgar Alain Grospiron"                 M     Bronze     1
##  9  1994 "Elizabeth Geary \"Liz\" McIntyre"      F     Silver     1
## 10  1994 "Hilde Synnve Lid"                      F     Bronze     1
## # … with 92 more rows
```


```r
library(DT)
datatable(astable, class = 'cell-border stripe', colnames = c("Year","Name","Sex","Medal","Count"))
```

```{=html}
<div id="htmlwidget-7ae247b901ebc13cc469" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-7ae247b901ebc13cc469">{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102"],[1992,1992,1992,1992,1992,1992,1994,1994,1994,1994,1994,1994,1994,1994,1994,1994,1994,1994,1998,1998,1998,1998,1998,1998,1998,1998,1998,1998,1998,1998,2002,2002,2002,2002,2002,2002,2002,2002,2002,2002,2002,2002,2006,2006,2006,2006,2006,2006,2006,2006,2006,2006,2006,2006,2010,2010,2010,2010,2010,2010,2010,2010,2010,2010,2010,2010,2010,2010,2010,2010,2010,2010,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014],["Donna L. Weinbrecht","Edgar Alain Grospiron","Nelson Graham Carmichael","Olivier Allamand","Stine Lise Hattestad (-Bratsberg)","Yelizaveta Aleksandrovna Kozhevnikova","Andreas \"Sonny\" Schnbchler","Edgar Alain Grospiron","Elizabeth Geary \"Liz\" McIntyre","Hilde Synnve Lid","Ingrid Marie Lindgren","Jean-Luc Brassard","Lina Anatolyevna Cheryazova","Lloyd Langlois","Philippe LaRoche","Sergey Borisovich Shupletsov","Stine Lise Hattestad (-Bratsberg)","Yelizaveta Aleksandrovna Kozhevnikova","Colette Brand (-Roth)","Dmitry Vladimirovich Dashchinsky","Eric Bergoust","Janne Petteri Lahtela","Jonathan \"Jonny\" Moseley","Kari Traa","Nicole \"Nikki\" Stone","Sami Jouni Kristian Mustonen","Sbastien Edmond Foucras","Tae Satoya","Tatjana Mittermayer","Xu Nannan","Ale Valenta","Aleksey Gennadyevich Grishin","Alisa Peta Camplin (-Warner)","Deidra Rae Dionne","Janne Petteri Lahtela","Joe Pack","Kari Traa","Richard Nicolas Gay","Shannon Deanne Bahrke","Tae Satoya","Travis Mayer","Veronica Brenner","Alisa Peta Camplin (-Warner)","Dale Begg-Smith","Dmitry Vladimirovich Dashchinsky","Evelyne Leu","Han Xiaopeng","Jennifer Heil","Kari Traa","Li Nina","Mikko Tapani Ronkainen","Sandra Laoura","Toby Soo-Chul (Bong-Seok) Dawson (Kim-)","Vladimir Nikolayevich Lebedev","Aleksey Gennadyevich Grishin","Alexandre \"Alex\" Bilodeau","Andreas Matt","Ashleigh McIvor (-DeMerit)","Audun Grnvold","Bryon Wilson","Dale Begg-Smith","Guo Xinxin","Hannah Angela Kearney","Hedda Helene Berntsen","Jennifer Heil","Jeret Peterson","Li Nina","Liu Zhongqing","Lydia Ierodiaconou-Lassila","Marion Josserand","Michael \"Mike\" Schmid","Shannon Deanne Bahrke","Aleksandr Aleksandrovich Smyshlyayev","Alexandre \"Alex\" Bilodeau","Alla Petrovna Tsuper","Anna Ida Holmlund","Anton Sergeyevich Kushnir","Arnaud Bovolenta","Augustus Richard \"Gus\" Kenworthy","Ayana Onozuka","Chlo Dufour-Lapointe","Dara Howell","David John Morris","David Wise","Devin Logan","Hannah Angela Kearney","Jean-Frdric Chapuis","Jia Zongyang","Jonathan Midol","Joss Christensen","Justine Dufour-Lapointe","Kelsey Serwa","Kevin Rolland","Kim Lamarre","Lydia Ierodiaconou-Lassila","Maddison Michelle \"Maddie\" Bowman","Marie Martinod","Marielle Thompson","Michael \"Mike\" Riddle","Mikal Kingsbury","Nicholas \"Nick\" Goepper","Xu Mengtao"],["F","M","M","M","F","F","M","M","F","F","F","M","F","M","M","M","F","F","F","M","M","M","M","F","F","M","M","F","F","F","M","M","F","F","M","M","F","M","F","F","M","F","F","M","M","F","M","F","F","F","M","F","M","M","M","M","M","F","M","M","M","F","F","F","F","M","F","M","F","F","M","F","M","M","F","F","M","M","M","F","F","F","M","M","F","F","M","M","M","M","F","F","M","F","F","F","F","F","M","M","M","F"],["Gold","Gold","Bronze","Silver","Bronze","Silver","Gold","Bronze","Silver","Bronze","Silver","Gold","Gold","Bronze","Silver","Silver","Gold","Bronze","Bronze","Bronze","Gold","Silver","Gold","Bronze","Gold","Bronze","Silver","Gold","Silver","Silver","Gold","Bronze","Gold","Bronze","Gold","Silver","Gold","Bronze","Silver","Bronze","Silver","Silver","Bronze","Gold","Silver","Gold","Gold","Gold","Silver","Silver","Silver","Bronze","Bronze","Bronze","Gold","Gold","Silver","Gold","Bronze","Bronze","Silver","Bronze","Gold","Silver","Silver","Silver","Silver","Bronze","Gold","Bronze","Gold","Bronze","Bronze","Gold","Gold","Bronze","Gold","Silver","Silver","Bronze","Silver","Gold","Silver","Gold","Silver","Bronze","Gold","Bronze","Bronze","Gold","Gold","Silver","Bronze","Bronze","Bronze","Gold","Silver","Gold","Silver","Silver","Bronze","Silver"],[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]],"container":"<table class=\"cell-border stripe\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>Year<\/th>\n      <th>Name<\/th>\n      <th>Sex<\/th>\n      <th>Medal<\/th>\n      <th>Count<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[1,5]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>
```

## Technical Details

The data comes in a reasonably clean Excel data set. If needed for your visualization, you can add visual drapery like flag icons, icons for sports, icons for medals etc. but your are certainly not obligated to do that. 

Part of the your task will be transforming the dataset into a shape that allows you to plot what you want in `ggplot2`. For some plots, you will necessarily need to be selective in what to include and what to leave out. 

Make sure to use at least three different types of graphs, e.g. line graphs, scatter, histograms, bar chats, dot plots, heat maps etc.

## Submission

Please follow the [instructions](/Exercises/homework_submission_instructions.md) to submit your homework. The homework is due on Wednesday, February 16 at 5pm

## Please stay honest!

Yes, the medal counts of the olympics have surely been analyzed before.  If you do come across something, please no wholesale copying of other ideas. We are trying to practice and evaluate your abilities in using ggplot2 and data visualization not the ability to do internet searches. Also, this is an individually assigned exercise -- please keep your solution to yourself.
