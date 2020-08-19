library(RCurl)
library(dplyr)
library(ggplot2)
x <- getURL("https://raw.githubusercontent.com/TheEconomist/covid-19-excess-deaths-tracker/master/output-data/excess-deaths/ecuador_excess_deaths.csv")
base<- read.csv(text = x)

#funcion 2 
calculo_excess<-function(x,mes){
  
  base_final<-base%>%
    filter(region==x & month>=mes)
  
  total_excess_deaths<-base_final%>%
    mutate(total_excess=sum(excess_deaths))
  #print(y)
  
  #scale_y<-scale_y_continuous(breaks=seq(0,max(base_final$excess_deaths),100))
maximo<-max(base_final$excess_deaths)
minimo<-min(base_final$excess_deaths)
rango<-maximo-minimo
  
  scale_y<-scale_y_continuous(breaks =c(0,ceiling(rango/4), ceiling(rango/2),ceiling(maximo)))
  y_axis = max(base_final$excess_deaths)
  #notas 
  
  
  ggplot(base_final)+geom_area(aes(x=month,y=excess_deaths),fill="red")+geom_line(aes(x=month,y=excess_deaths))+
    labs(title = paste("Excedente de Muertes",x, sep = ":"), caption = "Fuente: The Economist",
         y="Mes de 2020",x="Exceso de Muertes")+
    scale_y+theme_bw()+
    annotate(
      "text",
      x = min(base_final$month)+1,
      y = y_axis,
      label = paste('Total excedente = ', max(total_excess_deaths$total_excess), sep = " " ),
      vjust = 1, size = 4, color = "grey40"
    )
  

  

}



#grafico Ec y GYE

provincia_1<-"Ecuador"
provincia_2<-"Guayas"

base_total<-base%>%
  filter(region%in%c(provincia_1,provincia_2))%>%
  filter(month>=2)

base_1<-base_total%>%
  filter(region==provincia_1)

region_2<-unique(base_2$region)

base_2<-base_total%>%
  filter(region==provincia_2)

#total_excess_deaths<-base_final%>%
 # mutate(total_excess=sum(excess_deaths))
#print(y)

#scale_y<-scale_y_continuous(breaks=seq(0,max(base_final$excess_deaths),100))
maximo<-max(base_total$excess_deaths)
minimo<-min(base_total$excess_deaths)
rango<-maximo-minimo

scale_y<-scale_y_continuous(breaks =c(0,ceiling(rango/4), ceiling(rango/2),ceiling(maximo)))
y_axis = max(base_total$excess_deaths)


labs<-labs(title = paste("Excedente de Muertes, Participacion",region_2, sep = ":"), caption = "Fuente: The Economist",
     x="Mes de 2020",y="Exceso de Muertes")

temas<-theme(
  plot.title = element_text(face = "italic", size = 12),
  legend.position = "left"
)

#notas 


ggplot()+geom_area(data=base_1,aes(x=month,y=excess_deaths),fill="red")+geom_line(data=base_1,aes(x=month,y=excess_deaths))+
  geom_area(data=base_2,aes(x=month,y=excess_deaths),fill="blue")+geom_line(data=base_2,aes(x=month,y=excess_deaths))+
  labs+scale_y+theme_bw()+temas+
  annotate(
    "text",
    x = 3,
    y = y_axis,
    label = paste('Partipación excedente = ',round(sum(base_2$excess_deaths)/sum(base_1$excess_deaths),2)*100,"%" , sep = " " ),
    vjust = 1, size = 4, color = "grey40"
  )+
  annotate(
    "text",
    x = 3,
    y = y_axis-1000,
    label = paste('Total excedente Nacional = ',sum(base_1$excess_deaths) , sep = " " ),
    vjust = 1, size = 4, color = "grey40"
  )
  



