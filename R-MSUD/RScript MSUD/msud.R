Install.packages("tidyverse")
install.packages("metafor")
install.packages("ggplot2")
install.packages("plotly")
install.packages("dplyr")
install.packages("tinytex")


library(tidyverse)
library(metafor)
library(ggplot2)
library(plotly)
library(dplyr)
library(tinytex)

msud1 <-read.csv("C:/Users/uqssaha3/Dropbox/3_MOODSUD/msud1.csv", header=TRUE)
View(msud1)
summary(msud1)
str(msud1)
range(msud1$S_QRTotalScore)
# view 2 vars
View(msud1[,c("unadj_rate", "adj_rate")])

#counting total numbers by class
count(msud1, vars= S_QRTotalScore)
#same
as.data.frame(table(msud1$S_QRTotalScore))
#counting total number
count(msud, "S_QRTotalScore")
count(msud, msud1$unadj_rate, na.rm=TRUE)
count(msud, msud1$adj_rate, na.rm=TRUE)
count(msud, msud1$S_QRTotalScore, na.rm=TRUE)

#count total number of events in a variable (eg.n of adj_rate)
a <- c(msud$adj_rate, na.rm=TRUE)
a
length (which(a>0.0)) # n=444
b <- c(msud$unadj_rate, na.rm=TRUE)
b
length (which(b>0.0)) #n=363

plot (x= msud1$adj_rate, y= msud$year,
      xlab = "adj_rate",
      ylab = "Year",
      xlim = c(0, 15), 
      ylim = c(1980, 2017), 
      main= "rate by year"
)

library(ggplot2)
#scatter plot using various color combinations and style.............not working
ggplot (msud1, aes (x= adj_rate, y= year, colour=Continent)) +
          geom_point()+
          
   geom_smooth()

ggplot(data=msud1, 
       mapping= aes(x= msud$adj_rate, y= masud$year, colour=cut)) +
         geom_point()+
         geom_smooth()

ggplot(data=msud1, 
      mapping= aes(x= adj_rate, y= year, colour=cut)) +
                 geom_point()+
                 geom_smooth()

ggplot(msud1, aes(x=adj_rate, y=year)) + 
   geom_point(mapping = aes (colour = class)) + 
   geom_smooth(method = "lm")

#  scatterplot shows adjusted rate by year with the size of total quality score# not working...
p <- msud1 %>%
   #filter(year==1980-2018) %>%
   ggplot( aes(x=adj_rate, y = S_QRTotalScore, color=Continent)) +
   geom_point() +
   theme_bw()
ggplotly(p)


#BAR chart: how many rates provided by countries: this works

ggplot(msud1, aes(x= country1)) + 
   geom_bar () +
   labs (title = "Studies by country",
             x = "Country",
             y = "Number of rates")+
   coord_flip()+
theme_minimal()

View(msud1)
View(msud1[,c("unadj_rate", "adj_rate", "S_QRAdjustment")])
library(forcats)

# BAR CHART: number of studies by countries
ggplot(msud1, aes(x = fct_infreq(country1)))+
 
   geom_bar (fill = "tomato") +
   labs (title = "Studies by country",
         x = "Country",
         y = "Number of rates")+
   coord_flip()+
theme_minimal()

ggplot(msud1, aes(x = fct_rev(fct_infreq(country1))))+

      geom_bar (fill = "tomato") +
   labs (title = "Studies by country",
         x = "Country",
         y = "Number of rates")+
   coord_flip()+
   theme_minimal()


ggplot(msud1, aes(x = fct_rev(fct_infreq(country1))))+
   geom_bar (fill = "tomato") +
   labs (title = "Rates by country",
         x = "Country",
         y = "Number of rates contributed by country")+
   coord_flip()+
   theme_minimal()

# Number of rates by country width change to .8

ggplot(msud1, aes(x = fct_rev(fct_infreq(country1))))+
   geom_bar (position = "dodge", width = 0.8) +
   labs (title = "Rates by country",
         x = "Country",
         y = "Number of rates contributed by country")+
   coord_flip()+
   theme_minimal()

# Number of rates by country width change to .8, stacked by adjustment not working

   ggplot(msud1, aes(x = fct_rev(fct_infreq(country1)), fill = S_QRAdjustment)) + 
   
   geom_bar (position = "dodge", width = 0.8) +
   labs (title = "Rates by country",
         x = "Country",
         y = "Number of rates contributed by country")+
   coord_flip()+
   theme_minimal()

# Number of rates by country width change to .8, putting frequency on each bar not working

ggplot(msud1, aes(x = fct_rev(fct_infreq(country1, fill = S_QRAdjustment))))+ 
   geom_bar (position = "dodge", width = 0.8) +
   labs (title = "Rates by country",
         x = "Country",
         y = "Number of rates contributed by country")+
   coord_flip()+
   theme_minimal()



# bar chart by number of studies
   msud1 %>%
      count(country1, endnoteID) %>%
      count(country1)%>%
      mutate (freq = n/sum(n)) %>%
      ggplot(aes(x= fct_rev(fct_reorder(country1, n)), y = n)) + 
      geom_col (fill = "tomato") +
      labs (title = "Studies by country",
            x = "Country",
            y = "Number of studies")+
      coord_flip()+
   theme_minimal() 
  
    msud1 %>%
      count(country1, endnoteID) %>%
      count(country1)%>%
      mutate (freq = n/sum(n)) %>%
      ggplot(aes(x= fct_reorder(country1, n), y = n)) + 
      geom_col (fill = "tomato") +
      labs (title = "Studies by country",
            x = "Country",
            y = "Number of studies")+
      coord_flip()+
      theme_minimal() 



   
   





                    