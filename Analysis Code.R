# # # ##------INSTALL PACKAGES--------------
# install.packages("tidyverse")
# install.packages("readxl", type="source")
# install.packages("leaflet",type="source")
# install.packages("rworldmap",type="source")
# install.packages("flexdashboard", type = "source")
# install.packages("ploty",type="source")
# 
#------LOAD DATA------------
library(tidyverse)
library(readxl)
library(leaflet)
library(flexdashboard)
library(gridExtra)
library(broom)
library(plotly)

###### T I D Y I N G  D A T A ######
#------LOAD FILES---------
CCodesfile="~/project/Country-Code.xlsx"
Zomatofile="~/project/zomato.xlsx"
CurrencyFile="~/project/CurrencyRates.xlsx"

CCodes=read_excel(CCodesfile) #importing country code file
zomato=read_excel(Zomatofile) #importing main zomato file
Currency=read_excel(CurrencyFile) #importing currency file

#------ GET RID OF SOME COLUMNS & ADD NEW ONES -------------
zomato=zomato %>% select (-c(Rating_Color,Switch_To_Order_Menu,Address,Locality_Verbose,Currency))
#we remove currency column as it is incorrect and contains too many unique symbols

#add in new columns: Country Names according to Country Code & proper currency names with conversion rates
zomato = zomato %>% left_join(CCodes) 
zomato = zomato %>% left_join(Currency) 

#add in new column: USD Cost (for equal comparison)
zomato = zomato %>% mutate(Avg_Cost_USD = Average_Cost_For_Two*`Conversion Rate (USD)`) 
#Multiplied Currency Column by it's conversion rate

#replacing Cuisines col. with Principal_Cuisines (the first category of cuisines for every country row)
zomato= zomato %>% separate(Cuisines,into=c("Principal_Cuisines"),sep=',') #store "primary" cuisine types into new column and replace old with this

#make a seperate cuisine file for principal cuisine + restaurant
CCFile = zomato %>% select(Country, Principal_Cuisines)

#Make sure rating categories are ordered:
zomato = zomato %>% mutate(Rating_Text=ordered(Rating_Text,levels=c("Excellent","Very Good","Good","Average","Poor")))

#---------REMOVE MISSING VALUES----------
#we notice that there are several "0" rated stores for even expensive places so we decided to remove no rating stores
zomato[zomato == 0] = NA #remove values that have zero
zomato[zomato == "Not rated"] = NA #remove unrated values
zomato = zomato %>% filter(!is.na(Aggregate_Rating))

#------- GRAPHING DATA ----------
#look at distributions of variables

#NONE OF THESE HAVE COLOUR **  **  **
#aggregate rating  dist.
g1=ggplot(zomato,aes(Aggregate_Rating))+geom_histogram()+plot_themes+
  labs(title="Aggregate Rating Distribution",x="Aggregate Rating",y="Frequency") 
g1 #almost a normal distriubtion
ggplotly(g1)

#average cost dist.
g2=ggplot(zomato,aes(Avg_Cost_USD))+geom_histogram() +plot_themes+
  labs(title="Average Cost Distribution",x="Average Cost",y="Frequency")
g2 #skewed to the right
ggplotly(g2)

#country code dist.
g3 = ggplot(zomato,aes(Country_Code))+geom_histogram()+plot_themes+
  labs(title="Country Distribution",x="Country Code",y="Frequency")
g3#terrible
ggplotly(g3)

#votes dist.
g4=ggplot(zomato,aes(Votes))+geom_histogram() +plot_themes+
  labs(title="Votes Distribution",x="Votes",y="Frequency")
g4 #skewed right
ggplotly(g4)

library(RColorBrewer)
display.brewer.all() #check color palettes

#themes for plot
plot_themes=theme(plot.title=element_text(family="Palatino", face="bold.italic", size=15),legend.title = element_text(face = "italic", family = "Palatino"), 
                  legend.text = element_text(face = "bold.italic",family = "Palatino"), 
                  axis.title = element_text(family = "Palatino", face="italic",size = (10)),
                  axis.text.y.left = element_text(family = "Palatino", colour = "darkgrey", size = (10)),
                  axis.text.x.bottom = element_text(angle =90,family = "Palatino", colour = "darkgrey", size = (10)))

getPalette= colorRampPalette(brewer.pal(9,"RdBu"))

#Closer look into Country
country = as.data.frame(count(zomato,Country))
Country_Frequency=ggplot(country,aes(x=reorder(Country,-n),y=n))+geom_bar(aes(fill=Country),stat="identity") + 
  scale_fill_manual(values = getPalette(15)) +plot_themes +labs(title="Country Frequency",y="Country Count",x="Countries",fill="Countries")
ggplotly(Country_Frequency)



#India has most data -> seperate
india.data = zomato %>% filter(Country == "India")
ggplot(india.data,aes(Aggregate_Rating))+geom_histogram(binstart=1.8,binwidth = 0.2) +plot_themes+
  labs(title = "Aggregate Ratings in India", x="Aggregate Rating",y="Frequency")

India.Data_Hist=hist(india.data$Aggregate_Rating, freq = F) 
#normal distribution of ratings within india

#themed display not working here??
#print(India.Data_Hist + plot_themes + labs(title="India Data Distribution",y="Density",x="Aggregate Rating"))

### LOOKING INTO INDIA
#How does price differ by rating categories
India.Cost_Rating=ggplot(india.data,aes(y=Avg_Cost_USD,x=Rating_Text, color=Rating_Text))+geom_boxplot()+ 
                             plot_themes + labs(title="Prices in India",y="Price",x="Rating",color="Ratings")
India.Cost_Rating      


#Price Range vs Ratings
India.PriceR_Rating=ggplotly(
  ggplot(india.data,aes(x=Rating_Text,fill=factor(Price_Range)))+geom_bar(position = "dodge") +
    labs(title = "Prices in India",x="Rating",y="Number of Restaurants",fill="Price Range") +
    plot_themes)

India.PriceR_Rating

#or

ggplotly(ggplot(india.data,aes(factor(Price_Range)))+
           geom_bar(aes(fill=factor(Price_Range)))+facet_wrap(~Rating_Text,scales="free")+plot_themes)


#Price Range vs City
India.PriceR=ggplotly(
  ggplot(india.data,aes(x=factor(Price_Range),fill=City))+geom_bar()+plot_themes+
    labs(title="India Price Ranges",x="Price Range",y="Frequency")
)
India.PriceR

India.Cost_Rating=
  ggplot(india.data,aes(x=log10(Avg_Cost_USD),y=Aggregate_Rating))+
    geom_point(aes(color=Aggregate_Rating))+geom_smooth() + #somewhat log
    plot_themes+labs(title = "Cost per Rating in India",y="Aggregate Rating",x="Average Cost")

 #expensive restaurants are ONLY in new dehli
India.Cost_Rating


#Distribution of data from each City
India.CityDistribution=ggplotly(
  ggplot(india.data,aes(City))+geom_bar(aes(fill=City))+plot_themes+
    labs(title = "India City Distribution",x="Cities",y="Frequency")
)
India.CityDistribution
#most data comes from new dehli, then gurgaon then noida

#Price Variance per City
ggplot(india.data,aes(x=Aggregate_Rating,=Avg_Cost_USD,colour=factor(Price_Range)))+
  geom_point()+facet_wrap(~City)

#what makes good restaurants great?
top = india.data %>% filter(Rating_Text == "Excellent")
India.PrincipalCuisines=ggplotly(ggplot(top,aes(Principal_Cuisines))+geom_bar(aes(fill=Principal_Cuisines))+plot_themes+
                                   labs(title = "Cuisines in India",x="Principal Cuisines",y="Frequency",fill="Cuisines in India")) #NORTH INDIAN FOOD IS BEST?
India.PrincipalCuisines

#No not the best per say bc all rating categories list North as highest:
ggplotly(ggplot(india.data,aes(Principal_Cuisines))+
           geom_bar(aes(fill=Principal_Cuisines))+facet_wrap(~Rating_Text,scales="free")+plot_themes+
           labs(x="Principal Cuisines",y="Frequency",fill="Cuisines in India"))



################ @ OWISHEE ################

#pie chart to visualize Countries
pie <- ggplotly(ggplot(zomato, aes(x = "", fill = factor(Country))) + 
                  geom_bar(width = 1) +
                  theme(axis.line = element_blank(), 
                        plot.title = element_text(hjust=0.5)) + 
                  labs(fill="Country", 
                       x=NULL, 
                       y=NULL, 
                       title="Pie Chart of Countries", 
                       caption="Distribution of Countries"))
pie + coord_polar(theta = "y", start=0)
#visually see how terrifying this limitation is
#the theme is showing up as errors here 

#pie chart for PRICE RANGE INDIA VS AMERICA
IndiaPie_PriceR=ggplot(india.data,aes(x="",fill(Price_Range)))+geom_bar(width=1)+
  theme(axis.line = element_blank(),plot.title =element_text(hjust=0.5)) + 
  labs(fill="Price Range", 
       x=NULL, 
       y=NULL, 
       title="India Price Range", 
       caption="Distribution of Countries")
pie + coord_polar(theta = "y", start=0)
IndiaPie_PriceR

a = ggplot(india.data,aes(x=Rating_Text,y=Avg_Cost_USD))+geom_boxplot()+
  plot_themes+labs(title="Rating VS Cost in India",x="Rating",y="Cost")#looks more like a boxlot 
ggplotly(a)
#does tend to follow a trend but turns out but notice how higher prices got very good
#instead of excellent >> there is a cuttoff price for consumers in india, when exceeded
#it takes away from their utility

#grouped boxplot on india vs the other countries
b = ggplot(zomato,aes(x=Rating_Text,y=Avg_Cost_USD,colour=IndiaYN))+
  geom_boxplot()+plot_themes
ggplotly(b)

rc1=ggplot(zomato %>% filter(Avg_Cost_USD<100),aes(x=Rating_Factor,y=Avg_Cost_USD,colour=IndiaYN))+
  geom_boxplot() #cost 0-100 for a closer look
#indians tend to spend less on food compared to rest of the world 
ggplotly(rc1)

#RATING VS PRINCIPAL CUISINE
ggplot(india.data,aes(x=Rating_Text,fill=Principal_Cuisines))+geom_bar()

ggplot(india.data,aes(x=Rating_Factor,fill=Indian_Food_Served))+geom_bar(position="dodge")
#we see here that indian cuisine in india is not necessarily more favourable 

cost.rating = ggplot(india.data %>% filter(Avg_Cost_USD<50),aes(y=Aggregate_Rating,x=Avg_Cost_USD))+geom_point()+stat_smooth(method="loess")
ggplotly(cost.rating)

########## @ OWISHEE ##################

#-------------MAPPING DATA-----------
## LEAFLET MAP - DYNAMIC
#Check India
leaflet(india.data) %>% addTiles() %>%
  addMarkers(clusterOptions = markerClusterOptions()) %>%
  setView(78.9629,20,zoom=4)

#notice through the maps that majority of the data is actually coming from New Dehli
#let's investigate
leaflet(india.data) %>% addTiles() %>% addCircles(opacity=0.5) %>% setView(77.2090,28.6139,zoom=11)
leaflet(india.data) %>% addTiles() %>% addMarkers(clusterOptions = markerClusterOptions()) %>% setView(77.2090,28.6139,zoom=11)


#----------- REGRESSION ONLY ON INDIA -------
##we need to set a limit so predicted ratings do not go above 5
#might help with some of the skewness of our data
india.data = india.data %>% mutate(Transformed_Rating = log10(Aggregate_Rating/(5-Aggregate_Rating)))

costreg = lm(Aggregate_Rating~log10(Avg_Cost_USD),data=india.data,weights = Votes)
summary(costreg)
#check residual fitted plot
ggplot(data=costreg,aes(y=.resid, x=.fitted))+geom_point()+geom_smooth(se=F)
ggplot(data=costreg,aes(y=.resid, x=.fitted))+geom_point(alpha=0.4)+geom_smooth(se=F)

cityreg = lm(Aggregate_Rating~City,data=india.data,weights = Votes)
summary(cityreg)
#check residual fitted plot
ggplot(data=cityreg,aes(y=.resid, x=.fitted))+geom_point()+geom_smooth(se=F)

localityreg = lm(Aggregate_Rating~Locality,data=india.data,weights = Votes)
summary(localityreg)
#check residual fitted plot
ggplot(data=localityreg,aes(y=.resid, x=.fitted))+geom_point()+geom_smooth(se=F)


cuisinereg = lm(Aggregate_Rating~Principal_Cuisines,data=india.data,weights=Votes)
summary(cuisinereg) #cusine has high p values - not statistically significant

# REGRESSION FOR BOOKING & DELIVERY (we suspect it won't affect much)

#Regression on Booking
bookingreg = lm(Transformed_Rating~Has_Table_Booking, india.data, weights = Votes)
summary(bookingreg)

#Regression on Online Delivery
deliveryreg = lm(Transformed_Rating~Has_Online_Delivery, india.data,weights = Votes)
summary(deliveryreg)

#boxplot - delivery vs residual
library(broom)
augment(deliveryreg) %>% 
  ggplot(aes(x=Has_Online_Delivery,y=.resid))+geom_boxplot() #median is below zero meaning we may have skewed resids

augment(deliveryreg) %>% ggplot(aes(sample=.resid))+stat_qq()+
  stat_qq_line()+facet_wrap(~Has_Online_Delivery)

## ken 

india.data = india.data %>% mutate(Principal_Cuisines=fct_relevel(Principal_Cuisines,"North Indian"))

india.data = india.data %>% mutate(City=fct_relevel(City,"New Delhi"))

drop1(multireg2,test="F")

tidy(multireg2) %>% filter(str_detect(term, "ordered")) %>% filter(p.value<=0.05) %>% arrange(p.value)

##multiple regression
multireg = lm(Transformed_Rating~log10(Avg_Cost_USD)+Has_Table_Booking+Locality+Principal_Cuisines+City,data=india.data,weights = Votes)
summary(multireg)

## ken
library(broom)
a = tidy(multireg) %>% filter(str_detect(term,"Principal")) %>% arrange(estimate) %>% print(n=Inf)
a = as.data.frame(a)

b = tidy(multireg) %>% filter(str_detect(term,"City")) %>% arrange(estimate) %>% print(n=Inf)
b = as.data.frame(b)

reg.loc = tidy(multireg) %>% filter(p.value<0.05) %>% filter(str_detect(term,"Locality")) %>%  arrange(estimate)
reg.cus = tidy(multireg) %>% filter(p.value<0.05) %>% filter(str_detect(term,"Principal")) %>%  arrange(estimate)
reg.city = tidy(multireg) %>% filter(p.value<0.05) %>% filter(str_detect(term,"City")) %>%  arrange(term)
print (reg.loc, n=Inf)

drop1(multireg, test = "F")
ggplot(data=multireg,aes(y=.resid,x=.fitted))+geom_point()+geom_smooth(se=F)

plot(multireg2,2)
plot(multireg2,3)

#how does india fair to the rest of the world?

#--------- PREDICTING WHAT RATING RESTAURANT YOU WILL ATTEND -------
budget = data.frame(Avg_Cost_USD=1)
lograting = predict(multireg,budget)
lograting
rating = (5*(10^lograting))/(1+(10^lograting))
rating

newdata = data.frame(Avg_Cost_USD=10, Locality="Patia",Has_Table_Booking="Yes",Has_Online_Delivery="No")
lgrating = predict(multireg,newdata)
lgrating
rating = (5*(10^lgrating))/(1+(10^lgrating))
rating

#----------INDIA VS THE WORLD-----------
#regression on this
indiacuisine.reg = lm(Transformed_Rating~`Indian Food Served?`,india.data4,weights = Votes)
summary(indiacuisine.reg) #small p value but very low R square
#indian food actually decreases rating slightly 
ggplot(indiacuisine.reg,aes(x=.fitted,y = .resid))+geom_point()+geom_smooth(se=F)
#binary
plot(indiacuisine.reg,1)
plot(indiacuisine.reg,2) #not bad minus top and bottom outliers
ggplot(indiacuisine.reg,aes(sample=.resid))+stat_qq(alpha=0.2)+
  stat_qq_line()+facet_wrap(~`Indian Food Served?`)
#not very normal distribution
#conclusion - insignificant effect


#--------- IDENTIFYING WHATS INDIAN FOOD---------
#CREATING A VECTOR CONTAINING THE INDIAN CUISINES -> IF/ELSE -> NEW COL ADDED TO india.data2
c.df = c("Andhra","Assamese","Bengali","Malwani","Naga","North","South")
india.data2$YN = ifelse(india.data$Principal_Cuisines %in% c.df, "INDIAN","OTHER")
View(india.data %>% select(Principal_Cuisines,YN))
#worked

ggplot(india.data,aes(x=Transformed_Rating,y=Avg_Cost_USD))+geom_point()+geom_smooth()

test = lm(Transformed_Rating~Avg_Cost_USD+Has_Table_Booking+Has_Online_Delivery+Locality+Principal_Cuisines, data=india.data,weights=log(Votes))
summary(test)
drop1(test, test='F')

####exporting india data set####
write.table(india.data,file="/Desktop/india_data.txt",row.names=F,sep="\t")


library(MASS)
fit_select = lm(Transformed_Rating~Avg_Cost_USD+Has_Table_Booking+Locality+Principal_Cuisines+City,data=india.data,weights = Votes)
step = stepAIC(fit_select1, direction = "both")
step$anova # display results
