#question1
#what is residence rules for 2010 census?
#Post-marital residence rules specify where a person resides after marriage and, accordingly, influence the structure and size of household units.
#why is it so important?
#Residence rules form a crucial connective link in the census process, taking the data and attributes of each American resident and producing results that can be tabulated by whatever geographic boundaries may be needed. Residence rules are critical to assigning each person to a "correct" address; a second key linkage-between the address and a specific geographic location-is provided by the Census Bureau's Master Address File (MAF) and geographic database systems. Properly understood and executed, residence rules provide structure to the highly complex task of census data collection.
#Sourcelink:https://www.nap.edu/read/11727/chapter/4


#question2
require(tidyverse)
require(gridExtra)
require(usmap)

x <- read_delim("PEP_2018_PEPANNRES_with_ann.csv",
                   col_names=TRUE,delim=",",skip=1)

x<-x[,c("Geography","April 1, 2010 - Census","Population Estimate (as of July 1) - 2018")]
#colnames(x)<-c("states","res2010","pep2018")

x <- rename(x, 
            states=Geography,
            res2010="April 1, 2010 - Census", 
            pep2018="Population Estimate (as of July 1) - 2018")

x<-x[-c(1),]
x
#(a)
#Because there are extra rows. top row that starts with "GEO.id". Row has total residence population of united states.
#rows that has district of columbia and Puerto Rico which doesnt count as 50 states.


#(b)
#according to 2010 census the residential population of the U.S. is 308745538.
total<-sum(as.numeric(x$res2010))
total = total - 308745538
is.element(total,x$res2010)
as.character(total)
#theres one or more rows excluded from 2010 census residental population
for(i in seq(1:length(x$res2010))){
  if(x$res2010[i] == total){
    x<-x[-c(i),]
  }
}


dim(x)

x
#(c)
res2010<-sum(as.numeric(x$res2010))
pep2018<-sum(as.numeric(x$pep2018))
percentagechange<-((pep2018-res2010)/pep2018)
paste(round(100*percentagechange, 2), "%", sep="")

x<-x[,-c(2)]





#question3
require(readxl)
y<-read_xls("ApportionmentPopulation2010.xls")
#y<-select(y,"STATE","(APRIL 1, 2010)","2010 CENSUS")
y<-y[-c(1:10,61:67),c(1,2,4)]
colnames(y)<-c("states","appor2010","reps2010")
#y<-drop_na(y)
#rename(y,states=STATE,appor2010="(APRIL 1, 2010)",reps2010="2010 CENSUS")

y<-as_tibble(y)

#(a)
y$appor2010<- as.numeric(y$appor2010)


minimum<-min(y$appor2010,na.rm = TRUE)
maximum<-max(y$appor2010,na.rm = TRUE)
mean<-mean(y$appor2010,na.rm = TRUE)
median<-median(y$appor2010,na.rm = TRUE)
standarddeviation<-sd(y$appor2010,na.rm = TRUE)
SummaryStatistic<-matrix(c(minimum,maximum,mean,median,standarddeviation),ncol=5,byrow = TRUE)
colnames(SummaryStatistic)<-c("minimum","maximum","mean","median","standarddeviation")
SummaryStatistic <- as.table(SummaryStatistic)

SummaryStatistic

#(b)
y<-as_tibble(y)

y[y$appor2010==max(y$appor2010), ]$states
y[y$appor2010==min(y$appor2010), ]$states


#question4
y$appor2010 <- as.numeric(y$appor2010)

theme.info <- theme(plot.title = element_text(size=15, hjust=0.5),
                    axis.title = element_text(size=12),
                    axis.text = element_text(size=12))

 ggplot(y,aes(appor2010)) + 
  geom_histogram(bins=40) +
  ggtitle("2010 apportionment population") +
  labs(x="residence population") +
  theme.info


#after natural log
y %>%
  mutate(log_appor2010 = log(appor2010)) %>%
      ggplot(aes(x=log_appor2010)) + 
      geom_histogram(bins=40) +
      ggtitle("2010 apportionment population") +
      labs(x="residence population") +
      theme.info
#question5
#Median is the better measure. The highly skewed outlier will pull the mean away from the center. The median better represents the central tendency for the distribution. 

#question6
#(a)
y$reps2010<-as.numeric(y$reps2010)

y %>% ggplot(aes(x=appor2010, y=reps2010)) + geom_point() +
  labs(x="2010 apportionment population", y="number of House members") +
  theme.info

#(b)

y %>% 
  mutate(log_appor2010 = log(appor2010), log_reps2010 = log(reps2010)) %>%
  ggplot(aes(x=log_appor2010, y=log_reps2010)) + geom_point() +
  labs(x="log_2010 apportionment population", y="log_number of House members") +
  theme.info

#The scatterplot before transformation shows a clearer relationship between two variables.
#we can use correlation to represent the graph (a) because there is a clear relationship between the number of house seats and the size of apportionment population. They have positive relation.

#question7
xy <- left_join(x, y)
xy

n<-seq(from=2,to = 60)
denom<-1 / sqrt(n*(n-1))

priorityvalue<-c(t(outer(xy$pep2018, denom)))


dataset<-data.frame(priority_value=priorityvalue,states=rep(xy$states,each=59))

dataset<-dataset[order(dataset$priority_value,decreasing = TRUE),]

dataset<-dataset[1:385,]

freq<-count(dataset,states)

xy<-left_join(xy,freq)
xy$n<-replace_na(xy$n,0)
xy<-xy[order(xy$n,decreasing = TRUE),]
xy

#(a) #three states has the highest number of representative
highest3<-head(xy,3)
highest3
#What fraction of the total number of representatives do these 3 states
#comprise?
sum(highest3$n)/sum(xy$n)
#(b)
sum(xy$n==1)
#5 states have only 1 house of representative

#question8
xy$reps2010<-as.numeric(xy$reps2010)
xy$difference <- as.character(xy$n - xy$reps2010)
freqtable<-table(xy$difference)
freqtable

#question9
#(a)
require(usmap)
usmap<-us_map()


xy$fips<-fips(xy$states)

plot_usmap(regions="states", data = xy, 
           values = "difference") + 
  ggtitle("difference: (estimated 2020 house reps ??? 2010 house reps)")+
  labs(fill="number of difference") +
  theme(legend.position = "right", 
        plot.title = element_text(size=13, hjust=0.5),
        plot.subtitle = element_text(size=12, hjust=0.5))

#(b)
xy[is.na(xy$difference),]
#District Columbia is not included.

#(c)
#According to the data, we can see 37 states' house representative has decreased by 1. 5 states keeps the same. 7 states decreased by 2. Texas increased by 1

#(d)
#i compared it with the predictions results online. It turns out my prediction is similiar to the one i find online. Many states are likely have reduce number of house representitive in 2020
#https://www.fairvote.org/likely_changes_in_us_house_seat_distribution_for_2020

#(e)
#we can mark the reduced/increased house seats number on the states in the map to make it more readable.