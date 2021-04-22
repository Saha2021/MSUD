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

msud1 <-read.csv("C:/Dropbox/3_MOODSUD/msud1.csv", header=TRUE)
View(msud1)
summary(msud1)
str(msud1)
range(msud1$S_QRTotalScore)
# view 2 vars
View(msud1[,c("unadj_rate", "adj_rate")])

#counting total numbers by class
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

#recode country into bigger groups (developed, emerging and least developed)
ms_recoded <- msud1 %>% 
mutate(recoded = case_when(country1 %in% c("USA", "Germany", "Canada",
                                            "Netherlands", "New Zealand", "France", "Australia", "Spain", 
                                            "Switzerland", "Singapore", "Italy", "UK", "Sweden", "Norway",
                                            "Denmark", "Japan", "Finland" ) ~ developed,
                             
                             country1 == ("South Korea", "Iran", "India", "Hungary", "Greece",
                                         "China", "Brazil", "Ukraine", "Taiwan", "Russia", "Puerto Rico", 
                                         "Hong Kong")  ~ emerging,
                             TRUE ~ least developed)) 
View(ms_recorded)

plot (x= msud1$adj_rate, y= msud1$year,
      xlab = "adj_rate",
      ylab = "Year",
      xlim = c(0, 15), 
      ylim = c(1980, 2017), 
      main= "rate by year"
)
#intere=active plot
library(ggplot2)
#scatter plot using various color combinations and style.............not working
ggplot (msud1, aes (x= adj_rate, y= year, colour=Continent)) +
          geom_point()+
          geom_smooth()

ggplot(data=msud1, 
       mapping= aes(x= msud1$adj_rate, y= masud1$year, colour=Continent)) +
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

# BAR CHART: number of studies by countries.. working
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

# Number of rates by country width change to .8, stacked by adjustment... not working

   ggplot(msud1, aes(x = fct_rev(fct_infreq(country1)), fill = adj_type)) + 
   geom_bar (position = "dodge", width = 0.8) +
   labs (title = "Rates by country",
         x = "Country",
         y = "Number of rates contributed by country")+
   coord_flip()+
   theme_minimal()

# Number of rates by country width change to .8, putting frequency on each bar.... not working

ggplot(msud1, aes(x = fct_rev(fct_infreq(country1, fill = adj_type))))+ 
   geom_bar (position = "dodge", width = 0.8) +
   labs (title = "Rates by country",
         x = "Country",
         y = "Number of rates contributed by country")+
   coord_flip()+
   theme_minimal()


# bar chart by number of studies...working
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
    
    
    
    #FOREST PLOT: 
    # need 4 variables: pair,temporality_type, unadjusted_type,95%CI 
    View(msud1[,c("pair", "temporality_type", "unadj_rate", "adj_rate", "unadj_lower", "unadj_upper")])
    
    #1. risk pair= ALA-DEP or DEP-ALA use filter fn for making risk pair
    #This is alcohol use-dperession or depression-alcohol use 
    ala.dep <-dplyr::filter (msud1, "pair" ("ALA-DEP"|"DEP-ALA")) #...not working
    View(ala.dep)
    
    #2. selectiong temporality var using filter fn
    ala.dep.lt <-dplyr::filter (ala.dep, 'temporality_type' %in%c("LT"))
    View(ala.dep.lt)
    
    #3. selecting unadjusted rate using filter fn
    ala.dep.lt.unadj <-dplyr::filter (ala.dep.lt, !is.na ("unadj_rate") | !is.na(a))
    View(ala.dep.lt.unadj)
    
    #POOLING OF EFFECT SIZES
    #for studies reporting 2X2 data use escl fn to obtain log odds ratio and sampling variance
    dat2 <- escalc (measure="OR", ai=a, bi=b, ci=c, di=d, data=ala.dep.lt.unadj)
    
    # for studies reporting ORs, transform to log OR
    #dat2[,c("endnoteID", "author_year", "country1", "pair", "unadj_rate","unadj_lower", "unadj_upper", "unadj_pvalue", "a", "b", "c", "d", "yi", "vi")]
    dat2$yi <- replmiss(dat2$yi, log(dat2$unadj_rate))
    
    #TRANSFORM P-VALUES TO Z-VALUES and corresponding SES
    dat2$zi <-sign(dat2$yi)*qnorm(dat2$unadj_pvalue/2, lower.tail= FALSE)
    dat2$sei <- dat2$yi/dat2$zi
    
    # CI convert to SEs
    dat2$sei <-replmiss(dat2$sei, with(dat2, (log(unadj_upper)-log(unadj_lower))/(2*1.96)))
    
    #sampling variance vi to check any missing values
    dat2$vi <-replmiss (dat2$ci, dat2$sei^2)
    
    #we can remove unused vars as before
    dat2zi <-dat2$sei <-NULL
    dat2
    
  #FIT RANDOM-EFFECTS MODEL
  res2 <-rma(yi, vi, slab=paste(author_year, ",", country1), data=dat2)
  res2
  
  #back tranform to ORs
  predict(res2, transf=exp, digits=2)
  
  #Now the forest plot
  forest(res2, atransf=exp, xlab="Unadjusted Odds Ratio", cex =0.6, xlim=c(-10,10),
         mlab="", at=log(c(0.01, 1, 5, 10, 25, 50, 100)), psize=1.4, order(res2$yi))
  #par (font=2)
  text(-10, nrow(ala.dep.lt.unadj)+1.5, cex=0.6, "Study", pos=4)
  text(-10, nrow(ala.dep.lt.unadj)+1.5, cex=0.6, "Unadjusted Odds ratio[95% CI]", pos=2)
  text(-10,-1, pos=4,cex=0.6, bquote(paste("Heterogenity (Q=",.(formatC(res2$QE,
       digits=2, format="f")),", df=",.(res2$k-res2$p),",p=",.(formatC(res2$QEp, digits=2, 
       format="f")),";",1^2,"=",.(formatC(res2$I2, digits=1, format="f")), "%)")))

#publication bias: funnel plot

taf >-trimfill(res2)
funnel(taf, cex.lab=0.7, cex.axis=0.7)
regtest(res2, model="rma")

#Forest plot MACRO
unadj_fn <- function(y2,z ){
  dat2 <- escalc(measure="OR", ai=a, bi=b, ci=c, di=d, data=msud1)
  dat2$yi <- replmiss(dat2$yi, log(dat2$unadj_rate))
  dat2$zi <-sign(dat2$yi)*qnorm(dat2$unadj_pvalue/2, lower.tail = FALSE)
  dat2$sei <- dat2$yi/dat2$zi
  dat2$sei <- replmiss(dat2$sei, with(dat2, (log(unadj_upper) - log(unadj_lower))/(2*1.96)))
  dat2$vi <- replmiss(dat2$vi, dat2$sei^2)
  dat2$zi <- dat2$sei <-NULL
  res2 <- rma(yi, vi, slab=paste(author_year,",",country1),   data=dat2)
  taf <- trimfill(res2)
  forest(res2,atransf=exp, xlab="Unadjusted Odds Ratio" )#, cex = 0.6, xlim=c(-10, 10),  mlab="", at=log(c(0.01, 1, 5, 10, 25, 50, z)), psize=1.4,order=order(res2$yi))
  par(font=2)
  text(-10,   nrow(ma1)+1.5, cex = 0.6, "Study",  pos=4)
  text(10,    nrow(ma1)+1.5, cex = 0.6, "Unadjusted Odds Ratio [95% CI]", pos=2)
  text(-10, y2, pos=4,  cex = 0.6, bquote(paste("Heterogeneity (Q = ", .(formatC(res2$QE, digits=2, format="f")), ", df = ", .(res2$k - res2$p),", p = ", .(formatC(res2$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                                .(formatC(res2$I2, digits=1, format="f")), "%)")))
  #Funnel plot
  funnel(taf, legend=TRUE)
  regtest(res2, model="rma") 
}


adj_fn <- function(y2,z ){
  dat2 <- escalc(measure="OR", ai=a, bi=b, ci=c, di=d, data=ma2)
  dat2$yi <- replmiss(dat2$yi, log(dat2$adj_rate))
  dat2$zi <-sign(dat2$yi)*qnorm(dat2$adj_pvalue/2, lower.tail = FALSE)
  dat2$sei <- dat2$yi/dat2$zi
  dat2$sei <- replmiss(dat2$sei, with(dat2, (log(adj_Upper) - log(adj_lower))/(2*1.96)))
  dat2$vi <- replmiss(dat2$vi, dat2$sei^2)
  dat2$zi <- dat2$sei <-NULL
  res2 <- rma(yi, vi, slab=paste(author_year, ",",country1),   data=dat2)
  taf <- trimfill(res2)
  forest(res2,atransf=exp, xlab="Adjusted Odds Ratio" ,cex=0.6, xlim=c(-10, 10), mlab="", at=log(c(0.01, 1, 2, 5,10,50, z)), psize=1.4,order=order(res2$yi))
  par(font=2)
  text(-10,  nrow(ma2)+1.5, cex = 0.6, "Study",  pos=4)
  text(10,   nrow(ma2)+1.5, cex = 0.6, "Adjusted Odds Ratio [95% CI]", pos=2)
  text(-10, y2, pos=4, cex=0.6, bquote(paste("Heterogeneity (Q = ",
                                             .(formatC(res2$QE, digits=2, format="f")), ", df = ", .(res2$k - res2$p),
                                             ", p = ", .(formatC(res2$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                             .(formatC(res2$I2, digits=1, format="f")), "%)")))
  funnel(taf, legend=TRUE)
  regtest(res2, model="rma") 
}

pdf("../test.pdf", width = 11, height = 7)      

# Mood and depression [unadjusted] 
#have to make an indicator var mood_dep_unadj...what is the best way??
msud1 <- msud1 %>% filter(mood_dep_unadj == 1 & temporality_type == "LT")
unadj_fn(-1, 50) 


# Mood and depression [adjusted] 
msud1 <- msud1 %>% filter(mood_dep_adj == 1 & temporality_type == "LT")
adj_fn(-1, 100)


                    