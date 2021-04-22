# Please see my script lines where I am requesting your collegiate input:
#   
# 1.       Line 25-38 which one you suggest for simple count of a num var
# 2.       Line 41—51 where is the problem?
# 3.       Lines 63, 67, 72 not working…
# 4.       Lines 138, 148…putting % on the bar chart
# 5.       Forest plot, line 190, I am not sure how to make this var..ala.dep. if this works rest should work
# 6.       Forest plot macro: 247….. then execute for line 295, 301 (please help how to make the indicator var)

install.packages("tidyverse")
install.packages("metafor")
install.packages("ggplot2")
install.packages("plotly")
# install.packages("dplyr")
# install.packages("tinytex")

library(tidyverse)
library(metafor)
# library(ggplot2)
library(plotly)
# library(dplyr)
library(tinytex)

#### tidyverse already includes ggplot2 and dplyr ####

msud1 <-read.csv("C:/users/milto/Dropbox/3_MOODSUD/msud1.csv", header=TRUE)
#### for SG: msud1 <-read.csv("msud1.csv") ####
View(msud1)
summary(msud1)
str(msud1)
range(msud1$S_QRTotalScore)
# view 2 vars
View(msud1[,c("unadj_rate", "adj_rate")])

#counting total numbers by class
as.data.frame(table(msud1$S_QRTotalScore))

#counting total number
count(msud1, S_QRTotalScore) ###....................NOT
count(msud1, unadj_rate)
count(msud1, adj_rate)

#### If you want to remove the NAs: ####
msud1 %>% 
  filter(!is.na(unadj_rate)) %>% 
  count(unadj_rate)
#### number of rows ####
msud1 %>% 
  filter(!is.na(unadj_rate)) %>% 
  nrow()

#### ... but it's not very interesting for a continuous variable! ####

#count total number of events in a variable (eg.n of adj_rate)
a <- c(msud1$adj_rate, na.rm=TRUE)
a
length (which(a>0.0)) # n=444
b <- c(msud1$unadj_rate, na.rm=TRUE)
b
length (which(b>0.0)) #n=363

#recode country into bigger groups (developed, emerging and least developed) ##....NOT...##
ms_recoded <- msud1 %>% 
  mutate(recoded = case_when(country1 %in% c("USA", "Germany", "Canada",
                                            "Netherlands", "New Zealand", "France", "Australia", "Spain", 
                                            "Switzerland", "Singapore", "Italy", "UK", "Sweden", "Norway",
                                            "Denmark", "Japan", "Finland") ~ "developed",
                             country1 %in% c("South Korea", "Iran", "India", "Hungary", "Greece",
                                         "China", "Brazil", "Ukraine", "Taiwan", "Russia", "Puerto Rico", 
                                         "Hong Kong") ~ "emerging",
                             is.na(country1) ~ "missing", # to be safe
                             TRUE ~ "least developed"),
         .after= country1) 
View(ms_recoded)
#### check results ####
ms_recoded %>% count(recoded)

plot (x= msud1$adj_rate, y= msud1$year,
      xlab = "adj_rate",
      ylab = "Year",
      xlim = c(0, 15), 
      ylim = c(1980, 2017), 
      main= "rate by year"
)

library(ggplot2)
#scatter plot using various color combinations and .... working
ggplot(msud1, aes(y = adj_rate, x = year, colour = Continent)) +
  geom_point() +
  geom_smooth(se = TRUE) + # or FALSE to remove confidence interval
  scale_y_log10()

msud1 %>% 
  filter(!is.na(adj_rate), !is.na(year)) %>% # remove missing data
  ggplot(msud1,aes(y = adj_rate, x = year)) +                       
   geom_point(mapping = aes(colour = Continent)) + 
   geom_smooth(method = "gam")
# alternative
ggplot(msud1, aes(y = adj_rate, x = year)) + 
  geom_point(mapping = aes(colour = Continent), na.rm = TRUE) + 
  geom_smooth(method = "gam", na.rm = TRUE)

# scatterplot shows adjusted rate by year with the size of total quality score# not working...
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

# BAR CHART: number of studies by countries.. FLIP TO THE LEFT HIGH ON DOWN 
ggplot(msud1, aes(x = fct_infreq(country1)))+
 
   geom_bar (fill = "tomato") +
   labs (title = "Studies by country",
         x = "Country",
         y = "Number of rates")+
   coord_flip()+
theme_minimal()

# BAR CHART: number of studies by countries.. FLIP TO THE LEFT HIGH ON TOP 
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

# Number of rates by country width change to .8, NO COLOR

ggplot(msud1, aes(x = fct_rev(fct_infreq(country1))))+
   geom_bar (position = "dodge", width = 0.8) +
   labs (title = "Rates by country",
         x = "Country",
         y = "Number of rates contributed by country")+
   coord_flip()+
   theme_minimal()

# Number of rates by country width change to .8, stacked by adjustment... not working

# reshape data to longer format: gather two columns,
# store names in "rate_type", values in "rate_value"
msud1_long <- msud1 %>% 
  pivot_longer(c(adj_rate, unadj_rate),
               names_to = "rate_type",
               values_to = "rate_value")

# faceted
msud1_long %>% 
  filter(!is.na(rate_value)) %>% 
  ggplot(aes(y = fct_rev(fct_infreq(country1)), fill = rate_type)) + 
   geom_bar() +
   labs(title = "Rates by country",
         y = "Country",
         x = "Number of rates contributed by country") +
  facet_wrap(vars(rate_type)) +
  theme_minimal()

# faceted
msud1_long %>% 
  filter(!is.na(rate_value)) %>% 
  ggplot(aes(y = fct_rev(fct_infreq(country1)), fill = rate_type)) + 
  geom_bar(position = "dodge") + # default is "stack"
  labs(title = "Rates by country",
       y = "Country",
       x = "Number of rates contributed by country") +
  theme_minimal()

# to see data for one country
msud1_long %>%
  filter(country1 == "China") %>% 
  select(country1, rate_type, rate_value) %>% 
  View()


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
    
    
    
#FOREST PLOT
# need 4 variables: pair,temporality_type, unadjusted_type,95%CI 
    
    View(msud1[,c("pair", "temporality_type", "unadj_rate", "adj_rate", "unadj_lower", "unadj_upper")])
    
    #1. risk pair= ALA-DEP or DEP-ALA use filter fn for making risk pair
    #This is alcohol use-depression or depression-alcohol use 
    
# this only keeps rows with either ALA-DEP or DEP-ALA.....shringking
# muds2 <-muds1 %>%
# ala.dep <-dplyr::filter("pair" %in% c("ALA-DEP", "DEP-ALA"))
View(msud1$pair)
View(msud1[,c("pair", "temporality_type")])

# this creates new columns with 1 or 0 depending on conditions...NOT
#this creates new colums with ala.dep.lt.unadj   NOT WORKING...............
#msud_exp <- msud1 %>% 
#mutate(ala.dep = ifelse(pair %in% c("ALA-DEP", "DEP-ALA"), 1, 0), 
#lt = ifelse(temporality_type == "LT", 1, 0),)
#View(msud_exp)

#creating variables with group (pair), temporality (temporality_type) and adjusted (adjusted_type | unadjusted_type)

msud_exp <- msud1 %>% 
  mutate(
#1. Alcohol with DEP, DYS, MOOD, BIPOLAR
    
ala.dep = ifelse(pair %in% c("ALA-DEP", "DEP-ALA"), 1, 0),
ald.dep = ifelse(pair %in% c("ALD-DEP", "DEP-ALD"), 1, 0),
aud.dep = ifelse(pair %in% c("AUD-DEP", "DEP-AUD"), 1, 0),

ala.dys = ifelse(pair %in% c("ALA-DYS", "DYS-ALA"), 1, 0),
ald.dys = ifelse(pair %in% c("ALD-DYS", "DYS-ALD"), 1, 0),
aud.dys = ifelse(pair %in% c("AUD-DYS", "DYS-AUD"), 1, 0),

ala.mood = ifelse(pair %in% c("ALA-MOOD", "MOOD-ALA"), 1, 0),
ald.mood = ifelse(pair %in% c("ALD-MOOD", "MOOD-ALD"), 1, 0),
aud.mood = ifelse(pair %in% c("AUD-MOOD", "MOOD-AUD"), 1, 0),

ala.bipolar = ifelse(pair %in% c("ALA-BIPOLAR", "BIPOLAR-ALA",  "BIPOLAR I-ALA", "ALA-BIPOLAR I" , "BIPOLAR II-ALA","ALA-BIPOLAR II" ), 1, 0),
ald.bipolar = ifelse(pair %in% c("ALD-BIPOLAR", "BIPOLAR-ALD", "BIPOLAR I-ALD", "ALD-BIPOLAR I", "BIPOLAR II-ALD", "ALD-BIPOLAR II"), 1, 0),
aud.bipolar = ifelse(pair %in% c("AUD-BIPOLAR", "BIPOLAR-AUD",  "BIPOLAR I-AUD", "AUD-BIPOLAR I", "BIPOLAR II-AUD", "ALD-BIPOLAR II"), 1, 0),

# 2. DRUG with DEP, DYS, MOOD, BIPOLAR

dra.dep = ifelse(pair %in% c("DRA-DEP", "DEP-DRA"), 1, 0),
drd.dep = ifelse(pair %in% c("DRD-DEP", "DEP-DRD"), 1, 0),
dud.dep = ifelse(pair %in% c("DUD-DEP", "DEP-DUD"), 1, 0),

dra.dys = ifelse(pair %in% c("DRA-DYS", "DYS-DRA"), 1, 0),
drd.dys = ifelse(pair %in% c("DRD-DYS", "DYS-DRD"), 1, 0),
dud.dys = ifelse(pair %in% c("DUD-DYS", "DYS-DUD"), 1, 0),

dra.mood = ifelse(pair %in% c("DRA-MOOD", "MOOD-DRA"), 1, 0),
drd.mood = ifelse(pair %in% c("DRD-MOOD", "MOOD-DRD"), 1, 0),
dud.mood = ifelse(pair %in% c("DUD-MOOD", "MOOD-DUD"), 1, 0),

dra.bipolar = ifelse(pair %in% c("DRA-BIPOLAR", "BIPOLAR-DRA" , "BIPOLAR I-DRA" , "BIPOLAR II-DRA"), 1, 0),
drd.bipolar = ifelse(pair %in% c("DRD-BIPOLAR", "BIPOLAR-DRD" , "BIPOLAR I-DRD" , "BIPOLAR II-DRD"), 1, 0),
dud.bipolar = ifelse(pair %in% c("DUD-BIPOLAR", "BIPOLAR-DUD" , "BIPOLAR I-DUD" , "BIPOLAR II-DUD"), 1, 0),


# 3. SUBSTANCE with DEP, DYS, MOOD, BIPOLAR
sa.dep = ifelse(pair %in% c("SA-DEP", "DEP-SA"), 1, 0),
sd.dep = ifelse(pair %in% c("SD-DEP", "DEP-SD"), 1, 0),
sud.dep = ifelse(pair %in% c("SUD-DEP", "DEP-SUD"), 1, 0),

sa.dYS = ifelse(pair %in% c("SA-DYS", "DYS-SA"), 1, 0),
sd.dYS = ifelse(pair %in% c("SD-DYS", "DYS-SD"), 1, 0),
sud.dYS = ifelse(pair %in% c("SUD-DYS", "DYS-SUD"), 1, 0),

sa.mood = ifelse(pair %in% c("SA-mood", "mood-SA"), 1, 0),
sd.mood = ifelse(pair %in% c("SD-mood", "mood-SD"), 1, 0),
sud.mood = ifelse(pair %in% c("SUD-mood", "mood-SUD"), 1, 0),

#sa.bipolar = ifelse(pair %in% c("SA-BIPOLAR", "BIPOLAR-SA" | "BIPOLAR I-SA" | "BIPOLAR II-SA"), 1, 0),
#sd.bipolar = ifelse(pair %in% c("SD-BIPOLAR", "BIPOLAR-SD" | "BIPOLAR I-SD" | "BIPOLAR II-SD"), 1, 0),
#sud.bipolar = ifelse(pair %in% c("SUD-BIPOLAR", "BIPOLAR-SUD" | "BIPOLAR I-SUD" | "BIPOLAR II-SUD"), 1, 0),

# now temporality
temp.lt = ifelse(temporality_type == "LT", 1, 0),
temp.pp = ifelse(temporality_type == "PP", 1, 0),
temp.to = ifelse(temporality_type == "TO", 1, 0),

#finally adjusted typE
unadj = ifelse(!is.na(unadj_rate), 1, 0), 
adj = ifelse(!is.na(adj_rate), 1, 0))

View(msud_exp)
View(msud_exp[,c("ala.dep", "temp.lt")])


# this only keeps rows with either ALA-DEP or DEP-ALA.....NOT
msud2 <-muds1 %>%
  ala.dep <-dplyr::filter("pair" %in% c("ALA-DEP", "DEP-ALA"))
View(msud2)

    #2. selecting temporality var using filter fn from dataset msud2
ala.dep.lt <- dplyr::filter (msud2, temporality_type == "LT")
View(ala.dep.lt)

    #3. selecting unadjusted rate using filter fn from dataset msud2
    ala.dep.lt.unadj <-dplyr::filter (ala.dep.lt, !is.na ("unadj_rate") | !is.na(a))
    View(ala.dep.lt.unadj)
    
    #POOLING OF EFFECT SIZES
    #for studies reporting 2X2 data use escl fn to obtain log odds ratio and samplinG variance
    dat2 <- escalc (measure="OR", ai=a, bi=b, ci=c, di=d, data=msud_exp)
    
    # for studies reporting ORs, transform to log OR
    #dat2[,c("endnoteID", "author_year", "country1", "pair", "unadj_rate","unadj_lower", "unadj_upper", "unadj_pvalue", "a", "b", "c", "d", "yi", "vi")]
    dat2$yi <- replmiss(dat2$yi, log(dat2$unadj_rate))
    
    #TRANSFORM P-VALUES TO Z-VALUES and corresponding SEs
    dat2$zi <-sign(dat2$yi)*qnorm(dat2$unadj_pvalue/2, lower.tail= FALSE)
    dat2$sei <- dat2$yi/dat2$zi
    dat2$c
    # CI convert to SEs
    dat2$sei <-replmiss(dat2$sei, with(dat2, (log(unadj_upper)-log(unadj_lower))/(2*1.96)))
    
    #sampling variance vi to check any missing values
    dat2$vi <-replmiss (dat2$c, dat2$sei^2)
    dat2$vi
    
    #we can remove unused vars as before
    dat2$zi <- dat2$sei <-NULL
    dat2
    
  #FIT RANDOM-EFFECTS MODEL
  res2 <-rma(yi, vi, slab=paste(author_year, ",", country1), data=dat2)
  res2
  
  #back tranform to ORs
  predict(res2, transf=exp, digits=2)
  
  #Now the forest plot
  forest(res2, transf=exp, xlab="Unadjusted Odds Ratio", cex =0.6, xlim=c(-10,10),
         mlab="", at=log(c(0.01, 1, 5, 10, 25, 50, 100)), psize=1.4, order=order(res2$yi))
  #par (font=2)
 res2
  text(-10, nrow(res2 )+1.5, cex=0.6, "Study", pos=4)
  text(-10, nrow(res2)+1.5, cex=0.6, "Unadjusted Odds ratio[95% CI]", pos=2)
  text(-10,-1, pos=4,cex=0.6, bquote(paste("Heterogenity (Q=",.(formatC(res2$QE,
       digits=2, format="f")),", df=",.(res2$k-res2$p),",p=",.(formatC(res2$QEp, digits=2, 
       format="f")),";",1^2,"=",.(formatC(res2$I2, digits=1, format="f")), "%)")))

#publication bias: funnel plot

taf >-trimfill(res2)
funnel(taf, cex.lab=0.7, cex.axis=0.7)
regtest(res2, model="rma")

#Forest plot MACRO
unadj_fn <- function(y2,z ){
  dat2 <- escalc(measure="OR", ai=a, bi=b, ci=c, di=d, data=msud_exp)
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
  text(-10,   nrow(msud_exp)+1.5, cex = 0.6, "Study",  pos=4)
  text(10,    nrow(msud_exp)+1.5, cex = 0.6, "Unadjusted Odds Ratio [95% CI]", pos=2)
  text(-10, y2, pos=4,  cex = 0.6, bquote(paste("Heterogeneity (Q = ", .(formatC(res2$QE, digits=2, format="f")), ", df = ", .(res2$k - res2$p),", p = ", .(formatC(res2$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                                .(formatC(res2$I2, digits=1, format="f")), "%)")))
  
  funnel(taf, legend=TRUE)
  regtest(res2, model="rma") 
}

adj_fn <- function(y2,z ){
  dat2 <- escalc(measure="OR", ai=a, bi=b, ci=c, di=d, data=msud_exp)
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
  text(-10,  nrow(msud_exp)+1.5, cex = 0.6, "Study",  pos=4)
  text(10,   nrow(msud_exp)+1.5, cex = 0.6, "Adjusted Odds Ratio [95% CI]", pos=2)
  text(-10, y2, pos=4, cex=0.6, bquote(paste("Heterogeneity (Q = ",
                                             .(formatC(res2$QE, digits=2, format="f")), ", df = ", .(res2$k - res2$p),
                                             ", p = ", .(formatC(res2$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                             .(formatC(res2$I2, digits=1, format="f")), "%)")))
  funnel(taf, legend=TRUE)
  regtest(res2, model="rma") 
}

pdf("../test.pdf", width = 11, height = 7)
dev.off()

# dep and depression [unadjusted] 
ms <- msud_exp %>% filter(ald.dep == 1 & temporality_type == "LT"  & unadj == 1 )
unadj_fn(-1, 50)
# dep and depression [adjusted] 
ms <- msud_exp %>% filter(ald.dep == 1 & temp.lit == 1 & adj == 1 )
unadj_fn(-1, 50)
 
msud1 <- msud1 %>% filter(mood.dep == 1 & temporality_type == "LT")
unadj_fn(-1, 50)
# Mood and depression [adjusted] 
msud1 <- msud1 %>% filter(mood_dep_adj == 1 & temporality_type == "LT")
adj_fn(-1, 100)



                    