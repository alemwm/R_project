clnd_suicide_dt5<- read.csv("cleaned_suicide.csv")
install.packages("corrplot")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("magrittr")
library(dplyr)
library(ggplot2)
library(corrplot)
library(magrittr)
distinct(clnd_suicide_dt5)
#==================================================================
#Summary
#==================================================================
summary(clnd_suicide_dt5)
levels(clnd_suicide_dt5$age)[1]<-"5-14"

#==================================================================
#        TABLES
#==================================================================
#slide No.10
top_cntry<-clnd_suicide_dt5%>%select(country,year,suicides_per_100k,gdp_per_capita)
top_cntry
Arrngd<-top_cntry[order(-top_cntry$suicides_per_100k),]
Arrngd
head(Arrngd,10)
#slide No.13
Age_gr<-group_by(clnd_suicide_dt5,age)%>%summarise(Sucid_tot= sum(suicides_no),mean_rate= mean(suicides_per_100k))
Age_gr

#==================================================================
#       Visualizations
#==================================================================
#Total Number of suicides by year,slide No.11
gr_yr_num<-clnd_suicide_dt5%>%group_by(year)%>%summarize(Tot_suicide=sum(suicides_no))
ggplot(gr_yr_num,aes(x=year,y=Tot_suicide))+geom_line(size=1,color="blue")+ylab("Total Suicides")+geom_point()
#Average Suicide rate by year,slide No.12
gr_yr_rt<-clnd_suicide_dt5%>%group_by(year)%>%summarize(suicid_rt=mean(suicides_per_100k))
ggplot(gr_yr_rt,aes(x=year,y=suicid_rt))+geom_line(size=1,color="blue")+ylab("Suicide Rate")+geom_point()
#Total suicides by age and sex,slide No.14
Age_gr<-clnd_suicide_dt5%>%group_by(age,sex)%>%summarise(Tot_suicides= sum(suicides_no),mean_rate= mean(suicides_per_100k))
ggplot(Age_gr,aes(x=age,y=Tot_suicides, fill=sex))+geom_bar(stat="identity",position="dodge",color="black") + ylab("Total Suicides")
#Average suicid erate by age and sex, slide No.15
gr_age1<-clnd_suicide_dt5%>%group_by(age,sex)%>%summarise(popn=sum(population),mean_rate=mean(suicides_100k))
ggplot(gr_age1,aes(x=age, y=mean_rate, fill=sex))+geom_bar(stat="identity",color="black")
#Total suicides by generation,slide No.17
gr_gener<- clnd_suicide_dt5%>%group_by(generation,year)%>%summarise(mean_rate=mean(suicides_per_100k),Tot_suicides=sum(suicides_no))
ggplot(gr_gener,aes(x=year,y=Tot_suicides,fill=generation))+geom_area()+ylab("Total Suicides")
#======================================================================
#correlation: slide No.19 and corrplot is additional
#======================================================================
cor(clnd_suicide_dt5[,5:8])
mat<-cor(clnd_suicide_dt5[,5:8])
corrplot(mat)
#=======================================================================
# observations Plot,Slide No.20 and another additional plot
#=======================================================================
hist(clnd_suicide_dt5$suicides_per_100k, breaks=10,main="Histogram for Suicide Rate",col="blue",col.main="red",xlab="Suicides Rate",col.lab="red")
lines(density(clnd_suicide_dt5$suicides_per_100k))
hist(yr_gr$mean_Sucid,breaks=10,main="Histogram for Suicide Rate per Year",col="blue",col.main="red",xlab="Suicide Rate",col.lab="red")
lines(density(yr_gr$mean_Sucid$suicides_per_100k))
#======================================================================
#Regression:slides No.21, 22 and 23
#======================================================================
ctry_dt<-clnd_suicide_dt5%>%group_by(country)%>%summarise(mean_rt=mean(suicides_per_100k),mean_gdp=mean(gdp_per_capita))
rg<-lm(mean_rt~mean_gdp,data=ctry_dt)
summary(rg)
confint(rg)
resid(rg)
deviance(rg)
plot(rg)


#======================================================================
# Hypothesis Testing, slide No 24,an additional plot
#=========================================================================
#To change 'year' variable to numeric for this analysis 
clnd_suicide_dt5$year<-as.character((clnd_suicide_dt5$year))
clnd_suicide_dt5$year<-as.numeric()

US_dt<-clnd_suicide_dt5%>%group_by(country,year)%>%summarise(Tot_suicd=sum(suicides_no))%>% filter(country=="United States")
slctd_yr1<-US_dt%>%filter(year>=2007 & year<2012)%>%select(country,year,Tot_suicd)
slctd_yr2<-US_dt%>%filter(year>=2012 & year<=2016)%>%select(country,year,Tot_suicd)
hist(US_dt$Tot_suicd, breaks=10,main="Histogram for US Number of Suicides",col="blue",col.main="red",xlab="Number of Suicides",col.lab="red")
lines(density(US_dt$Tot_suicd))

t.test(slctd_yr1$Tot_suicd,slctd_yr2$Tot_suicd)
wilcox.test(slctd_yr1$Tot_suicd, slctd_yr2$Tot_suicd, alternative = "two.sided")

#To change back the year variable to a factor
clnd_suicide_dt5$year<-as.factor(clnd_suicide_dt5$year)
#==============================================================================
# additioanal verification
#===============================================================================
Sucd_gr<-clnd_suicide_dt5%>%filter(suicides_no==max(suicides_no))%>% select(generation,year,suicides_no)
Sucd_gr
Aruba_pl<-clnd_suicide_dt5%>%group_by(country,year)%>%summarise(mean_rt=mean(suicides_per_100k))%>%filter(country=="Aruba")%>%select(country,year,mean_rt)%>%arrange(year)
Aruba_pl
min_yr<-clnd_suicide_dt5%>%filter(suicides_per_100k==min(suicides_per_100k))%>%select(year,country)
min_yr 
max_yr<-clnd_suicide_dt5%>%filter(suicides_per_100k==max(suicides_per_100k))%>%select(year,generation)
max_yr
 
#The maximum rate occured was in Aruba in 1995.The sex group was Male  and 75+group and the generation was GI generation
#interms of suicides.Interms of no suicides Russian Federation the highest number in 1994.The age group was 35-54,male and generation was Boomers.


levels(clnd_suicide_dt5$age)
rev(levels(clnd_suicide_dt5$age))
rev(levels(clnd_suicide_dt5$age))
sort(levels(clnd_suicide_dt5$age))
