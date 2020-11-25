sirene<-read.csv('sirene_bretagne.csv')
cmrcouvrt<-read.csv('poi_osm_FR.csv')

head(sirene)

rennes<-sirene[sirene$codepostal==35000 |sirene$codepostal==35200 |sirene$codepostal==35700,]
rennesovrt<-cmrcouvrt[cmrcouvrt$lat<=48.1441 & cmrcouvrt$lat>=48.0813 & cmrcouvrt$lon>=-1.7437 & cmrcouvrt$lon<=-1.6212,]

table(rennesovrt$status)

names(rennes)

str(rennes)
str(cmrcouvrt)

install.packages(leaflet)

library(leaflet)

m <- leaflet() %>% setView(lng = -1.6212, lat = 48.1441, zoom = 11.5)

pal <- colorFactor(c("navy", "red", "green"," yellow"), domain = c("partial", "closed","open","open_adapted"))

leaflet(rennesovrt) %>% addTiles()%>%addCircleMarkers(
  radius = 3,
  color = ~pal(status),
  stroke = FALSE, fillOpacity = 0.5
)%>%addLegend("bottomleft", colors=c("navy", "red", "green"," yellow","grey"),labels=c("partial", "closed","open","open_adapted","unknown"),
              title = "Business Status",
              opacity = 1
)

table(rennesovrt$category)

table(rennesovrt$subcategory)

library(dplyr)
library(ggplot2)

rennesovrt %>%
  group_by(category,status)%>%
  summarise(count=n())%>%ggplot(aes(x=category,y=count,fill=status))+geom_bar(stat="identity",color = "grey40", alpha = .5)+coord_flip()+
  labs(title = "Distribution of Business status by Category in Rennes, FR",
       subtitle = "The percent of amenity business closures are high",
       caption = "Source: Data.gov.fr      25/11/2020      Yaswanthkumar.G")



rennesovrt %>%
  group_by(category)%>%
  summarise(businesses_count=n())%>% ggplot(aes(x=category,y=businesses_count))+geom_bar(stat="identity",fill = "dodgerblue", color = "grey40", alpha = .5)+coord_flip()

library(treemap)
rennesovrt%>%
  group_by(category,subcategory)%>%
  summarise(count=n())%>%treemap(index=c("category","subcategory"),
                                 vSize="count",
                                 type="index",
                                 title = 'Treemap of Business in Rennes, Fr',
                                 bg.labels=0,
                                 fontcolor.labels=c("black","white"),
                                 )
library(treemapify)
rennesovrt%>%
  group_by(category,subcategory)%>%
  summarise(count=n())%>%ggplot(aes(area = count,fill=category, label = subcategory,subgroup=category))+geom_treemap(color = "grey40", alpha = .5)+
  geom_treemap_text(grow = T, reflow = T, colour = "black")+  geom_treemap_subgroup_border(colour = "black", size = 1)+
  labs(title = "Business distribution by Category in Rennes, FR",
       subtitle = "Tile size based on the count of businesses",
       caption = "Source: Data.gov.fr     25/11/2020      Yaswanthkumar.G")

foodrennes<-rennesovrt[rennesovrt$category=='food',]

foodrennes%>%group_by(subcategory,status)%>%summarise(count=n())%>%ggplot(aes(x=subcategory,y=count,fill=status))+geom_bar(stat="identity",color = "grey40", alpha = .5)+coord_flip()+
  labs(title = "Distribution of food Business status by Category",
       subtitle = "chocolate, confectionary, tea, coffee products selling shops\n are either closed or status unknown, some bakeries\n are closed, and most of the super markets are open",
       caption = "Source: Data.gov.fr      25/11/2020      Yaswanthkumar.G")

eatrennes<-rennesovrt[rennesovrt$category=='eat',]

eatrennes%>%group_by(subcategory,status)%>%summarise(count=n())%>%ggplot(aes(x=subcategory,y=count,fill=status))+geom_bar(stat="identity",color = "grey40", alpha = .5)+coord_flip()+
  labs(title = "Distribution of eating Businesses status by subcategory",
       subtitle = "pizza vending machines are open and even some bars too,\n but bars that are open also have tabac",
       caption = "Source: Data.gov.fr      25/11/2020      Yaswanthkumar.G")

eatrennes[eatrennes$subcategory=='fast_food'& eatrennes$status=='partial',]%>%group_by(name)%>%summarise(count=n())%>% 
  ggplot(aes(x=name,y=count))+geom_bar(stat="identity",fill = "dodgerblue", color = "grey40", alpha = .5)+coord_flip()+
  labs(title = "Fast Food businesses that are partially opened",
       subtitle = "Most of the Fast Food businesses that are partially opened\n are big brands",
       caption = "Source: Data.gov.fr      25/11/2020      Yaswanthkumar.G")

amenityrennes<-rennesovrt[rennesovrt$category=='amenity',]

amenityrennes[amenityrennes$status=='closed',]%>%group_by(subcategory)%>%summarise(count=n())%>%ggplot(aes(x=subcategory,y=count))+
  geom_bar(stat="identity",fill = "dodgerblue", color = "grey40", alpha = .5)+coord_flip()+
  labs(title = "Amenity businesses that are closed",
       subtitle = "Most of the Banks and Insurance companies closed their branches\n and moved online",
       caption = "Source: Data.gov.fr      25/11/2020      Yaswanthkumar.G")
