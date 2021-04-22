
install.packages("tidyverse")
install.packages("metafor")
install.packages("ggplot2")
install.packages("plotly")
install.packages("here")
install.packages("dplyr")
install.packages("tinytex")
install.packages("swirl")

library(Matrix)
library(metafor)
library(ggplot2)
library(plotly)
library(here)
library(dplyr)
library(tinytex)
library("swirl")
swirl()



PRB2008_ALL<-read.csv(url("https://raw.githubusercontent.com/coreysparks/data/master/PRB2008_ALL.csv"))
#PRB2008_ALL<- read.csv("C:/Users/uqssaha3/Dropbox/RBasics/PRB2008_ALL.csv", header=TRUE)
#View(PRB2008_ALL)


ma<-read.csv("C:/Users/uqssaha3/Dropbox/RBasics/ma3.csv", header=TRUE)
ma1<-read.csv("C:/Users/uqssaha3/Dropbox/RBasics/ma1.csv", header=TRUE)
ma2<-read.csv("C:/Users/uqssaha3/Dropbox/RBasics/ma2.csv", header=TRUE)
View(ma1)


ma<-read.csv("https://raw.githubusercontent.com/clim072/NB-SR_MOOD_ANX/master/files/Dataset/ma3.csv")
View(ma[,c("ID", "country")])
  
dep.so<-View
View(dep.so$temporality)

#ma <- read.csv(here("data", "ma3.csv"))

ma$R_UnadjRate1 <- as.numeric(ma$R_UnadjRate1)  

unadj_fn <- function(y2,z ){
  dat2 <- escalc(measure="OR", ai=a, bi=b, ci=c, di=d, data=ma1)
  dat2$yi <- replmiss(dat2$yi, log(dat2$R_UnadjRate1))
  dat2$zi <-sign(dat2$yi)*qnorm(dat2$R_PValue_Unadjusted/2, lower.tail = FALSE)
  dat2$sei <- dat2$yi/dat2$zi
  dat2$sei <- replmiss(dat2$sei, with(dat2, (log(R_UnadjUpperCI1) - log(R_UnadjLowerCI1))/(2*1.96)))
  dat2$vi <- replmiss(dat2$vi, dat2$sei^2)
  dat2$zi <- dat2$sei <-NULL
  res2 <- rma(yi, vi, slab=paste(author_year,",",country),   data=dat2)
  taf <- trimfill(res2)
  forest(res2,atransf=exp, xlab="Unadjusted Odds Ratio" )#, cex = 0.6, xlim=c(-10, 10),  mlab="", at=log(c(0.01, 1, 5, 10, 25, 50, z)), psize=1.4,order=order(res2$yi))
  #par(font=2)
  #text(-10,   nrow(ma1)+1.5, cex = 0.6, "Study",  pos=4)
  #text(10,    nrow(ma1)+1.5, cex = 0.6, "Unadjusted Odds Ratio [95% CI]", pos=2)
  #text(-10, y2, pos=4,  cex = 0.6, bquote(paste("Heterogeneity (Q = ", .(formatC(res2$QE, digits=2, format="f")), ", df = ", .(res2$k - res2$p),", p = ", .(formatC(res2$QEp, digits=2, format="f")), "; ", I^2, " = ",
  #                                              .(formatC(res2$I2, digits=1, format="f")), "%)")))
  
  #funnel(taf, legend=TRUE)
  #regtest(res2, model="rma") 
}


adj_fn <- function(y2,z ){
  dat2 <- escalc(measure="OR", ai=a, bi=b, ci=c, di=d, data=ma2)
  dat2$yi <- replmiss(dat2$yi, log(dat2$R_AdjRate1))
  dat2$zi <-sign(dat2$yi)*qnorm(dat2$R_PValue_Adjusted/2, lower.tail = FALSE)
  dat2$sei <- dat2$yi/dat2$zi
  dat2$sei <- replmiss(dat2$sei, with(dat2, (log(R_AdjUpperCI1) - log(R_AdjLowerCI1))/(2*1.96)))
  dat2$vi <- replmiss(dat2$vi, dat2$sei^2)
  dat2$zi <- dat2$sei <-NULL
  res2 <- rma(yi, vi, slab=paste(author_year, ",",country),   data=dat2)
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

### Mood and Social Phobia [unadjusted] 
ma1 <- ma %>% filter(mood_so_unadj == 1 & temporality == "lifetime")
unadj_fn(-1, 50) 


### Mood and Social Phobia [adjusted] 

#ma2 <- ma %>% filter(mood_so_adj == 1 & temporality == #"lifetime")
#adj_fn(-1, 100)


### Mood and Specific Phobia [unadjusted] 
  
ma1 <- ma %>% filter(mood_sp_unadj == 1 & temporality == "lifetime")
unadj_fn(-1, 10) 

### Mood and Specific Phobia [adjusted] 

### Mood and Anxiety disorder [unadjusted] 
  
ma1 <- ma %>% filter(mood_anx_unadj == 1 & temporality == "lifetime")
unadj_fn(-1, 100) 

### Mood and Anxiety disorder [adjusted] 
##  Period-prevalence estimates

  
  ### Mood and GAD [unadjusted] 

  
  ### Mood and GAD [adjusted] 

ma2 <- ma %>% filter(mood_gad_adj == 1 & temporality == "period-prevalence")
adj_fn(-1, 100)

### Mood and Social Phobia [unadjusted] 
ma1 <- ma %>% filter(mood_so_unadj == 1 & temporality == "period-prevalence")
unadj_fn(-1, 50) 

### Mood and Social Phobia [adjusted] 
ma2 <- ma %>% filter(mood_so_adj == 1 & temporality == "period-prevalence")
adj_fn(-1, 100)

### Mood and Specific Phobia [unadjusted] 

  ### Mood and Specific Phobia [adjusted] 

  ### Mood and Anxiety disorder [unadjusted] 
  
ma1 <- ma %>% filter(mood_anx_unadj == 1 & temporality == "period-prevalence")
unadj_fn(-1, 100) 

### Mood and Anxiety disorder [adjusted] 

ma2 <- ma %>% filter(mood_anx_adj == 1 & temporality == "period-prevalence")
adj_fn(-1, 100)

  #   Table 3.Broadly-defined depressive disorder (DEP) and anxiety disorders

  ##  Lifetime estimates

  ### DEP and Agoraphobia [unadjusted] 
  
ma1 <- ma %>% filter(dep_ago_unadj == 1 & temporality == "lifetime")
unadj_fn( -1, 150) 

### DEP and Agoraphobia [adjusted] 

  ### DEP and OCD [unadjusted] 
  
ma1 <- ma %>% filter(dep_ocd_unadj == 1 & temporality == "lifetime")
unadj_fn(-1, 150) 

### DEP and OCD [adjusted] 
ma2 <- ma %>% filter(dep_ocd_adj == 1 & temporality == "lifetime")
adj_fn(-1, 150)


### DEP and GAD [unadjusted] 
ma1 <- ma %>% filter(dep_gad_unadj == 1 & temporality == "lifetime")
unadj_fn( -1, 300) 

### DEP and GAD [adjusted] 
ma2 <- ma %>% filter(dep_gad_adj == 1 & temporality == "lifetime")
adj_fn( -1, 300)

### DEP and Panic disorder [unadjusted]   
ma1 <- ma %>% filter(dep_pds_unadj == 1 & temporality == "lifetime")
unadj_fn(-1, 300) 

### DEP and Panic disorder [adjusted]  
ma2 <- ma %>% filter(dep_pds_adj == 1 & temporality == "lifetime")
adj_fn(-1, 100)

### DEP and PTSD [unadjusted]  

ma1 <- ma %>% filter(dep_pts_unadj == 1 & temporality == "lifetime")
unadj_fn(-1, 250) 

### DEP and PTSD [adjusted]  

  ### DEP and Social phobia [unadjusted]   
ma1 <- ma %>% filter(dep_so_unadj == 1 & temporality == "lifetime")
unadj_fn( -1, 50) 

### DEP and Social phobia [adjusted]
ma2 <- ma %>% filter(dep_so_adj == 1 & temporality == "lifetime")
adj_fn( -1, 10)

### DEP and Specific phobia [unadjusted]   

ma1 <- ma %>% filter(dep_sp_unadj == 1 & temporality == "lifetime")
unadj_fn( -1, 50) 

### DEP and Specific phobia [adjusted]   
ma2 <- ma %>% filter(dep_sp_adj == 1 & temporality == "lifetime")
adj_fn( -1, 10)

### DEP and Anxiety disorder [unadjusted]   
ma1 <- ma %>% filter(dep_anx_unadj == 1 & temporality == "lifetime")
unadj_fn(-1, 1000) 



### DEP and Anxiety disorder [adjusted]    
ma2 <- ma %>% filter(dep_anx_adj == 1 & temporality == "lifetime")
adj_fn(-1, 1000) 


##  Period-prevalence estimates

  ### DEP and Agoraphobia [unadjusted] 
  
ma1 <- ma %>% filter(dep_ago_unadj == 1 & temporality == "period-prevalence")
unadj_fn( -1, 50)

### DEP and Agoraphobia [adjusted] 
ma2 <- ma %>% filter(dep_ago_adj == 1 & temporality == "period-prevalence")
adj_fn( -1, 50)


### DEP and OCD [unadjusted] 

ma1 <- ma %>% filter(dep_ocd_unadj == 1 & temporality == "period-prevalence")
unadj_fn(-1, 150) 

### DEP and OCD [adjusted] 

ma2 <- ma %>% filter(dep_ocd_adj == 1 & temporality == "period-prevalence")
adj_fn(-1, 150)

### DEP and GAD [unadjusted] 

ma1 <- ma %>% filter(dep_gad_unadj == 1 & temporality == "period-prevalence")
unadj_fn( -1, 300) 

### DEP and GAD [adjusted] 
ma2 <- ma %>% filter(dep_gad_adj == 1 & temporality == "period-prevalence")
adj_fn( -1, 300)

### DEP and Panic disorder [unadjusted] 

ma1 <- ma %>% filter(dep_pds_unadj == 1 & temporality == "period-prevalence")
unadj_fn(-1, 300) 

### DEP and Panic disorder [adjusted] 
ma2 <- ma %>% filter(dep_pds_adj == 1 & temporality == "period-prevalence")
adj_fn(-1, 100)

### DEP and PTSD [unadjusted] 

ma1 <- ma %>% filter(dep_pts_unadj == 1 & temporality == "period-prevalence")
unadj_fn(-1, 250) 

### DEP and PTSD [adjusted] 
ma2 <- ma %>% filter(dep_pts_adj == 1 & temporality == "period-prevalence")
adj_fn( -1, 100)

### DEP and Social phobia [unadjusted] 

ma1 <- ma %>% filter(dep_so_unadj == 1 & temporality == "period-prevalence")
unadj_fn( -1, 100) 

### DEP and Social phobia [adjusted] 
ma2 <- ma %>% filter(dep_so_adj == 1 & temporality == "period-prevalence")
adj_fn( -1, 100)


### DEP and Specific phobia [unadjusted] 

ma1 <- ma %>% filter(dep_sp_unadj == 1 & temporality == "period-prevalence")
unadj_fn( -1, 50) 

### DEP and Specific phobia [adjusted] 
ma2 <- ma %>% filter(dep_sp_adj == 1 & temporality == "period-prevalence")
adj_fn( -1, 10)

### DEP and Anxiety disorder [unadjusted] 

ma1 <- ma %>% filter(dep_anx_unadj == 1 & temporality == "period-prevalence")
unadj_fn(-1, 150)

### DEP and Anxiety disorder [adjusted] 
ma2 <- ma %>% filter(dep_anx_adj == 1 & temporality == "period-prevalence")
adj_fn(-1, 150)

# Table 4. Broadly-defined dysthymic disorder (DYS) and anxiety disorders
  ##  Lifetime estimates

  ### DYS and Agoraphobia [unadjusted] 

  ### DYS and Agoraphobia [adjusted] 

  ### DYS and OCD [unadjusted] 

  ### DYS and OCD [adjusted] 
ma1 <- ma %>% filter(dys_ocd_unadj == 1 & temporality == "lifetime")
unadj_fn(-1, 150) 

### DYS and GAD [unadjusted] 
ma1 <- ma %>% filter(dys_gad_unadj == 1 & temporality == "lifetime")
unadj_fn(-1, 600) 

### DYS and GAD [adjusted] 

  ### DYS and Panic disorder [unadjusted] 

  ### DYS and Panic disorder [adjusted] 

  ### DYS and PTSD [unadjusted] 

  ### DYS and PTSD [adjusted] 

  ### DYS and Social phobia [unadjusted] 
ma1 <- ma %>% filter(dys_so_unadj == 1 & temporality == "lifetime")
unadj_fn(-1, 200) 

### DYS and Social phobia [adjusted] 
ma2 <- ma %>% filter(dys_so_adj == 1 & temporality == "lifetime")
adj_fn(-1, 100)


### DYS and Specific phobia [unadjusted] 

ma1 <- ma %>% filter(dys_sp_unadj == 1 & temporality == "lifetime")
unadj_fn(-1, 100)  


### DYS and Specific phobia [adjusted] 

  ### DYS and Anxiety disorder [unadjusted] 

  ### DYS and Anxiety disorder [adjusted] 

  ##  Period-prevalence estimates

  ### DYS and Agoraphobia [unadjusted] 
  
  
ma1 <- ma %>% filter(dys_ago_unadj == 1 & temporality == "period-prevalence")
unadj_fn(-1, 50)  

### DYS and Agoraphobia [adjusted] 

  
  ### DYS and OCD [unadjusted] 
  
ma1 <- ma %>% filter(dys_ocd_unadj == 1 & temporality == "period-prevalence")
unadj_fn(-1, 600) 

### DYS and OCD [adjusted] 

  ### DYS and GAD [unadjusted] 
  
ma1 <- ma %>% filter(dys_gad_unadj == 1 & temporality == "period-prevalence")
unadj_fn(-1, 600) 

### DYS and GAD [adjusted] 
ma2 <- ma %>% filter(dys_gad_adj == 1 & temporality == "period-prevalence")
adj_fn(-1, 100)

### DYS and Panic disorder [unadjusted] 
ma1 <- ma %>% filter(dys_pds_unadj == 1 & temporality == "period-prevalence")
unadj_fn(-1, 150) 

### DYS and Panic disorder [adjusted] 

  ### DYS and PTSD [unadjusted] 
  
ma1 <- ma %>% filter(dys_pts_unadj == 1 & temporality == "period-prevalence")
unadj_fn(-1, 200) 

### DYS and PTSD [adjusted] 
ma2 <- ma %>% filter(dys_pts_adj == 1 & temporality == "period-prevalence")
adj_fn(-1, 100)

### DYS and Social phobia [unadjusted] 

ma1 <- ma %>% filter(dys_so_unadj == 1 & temporality == "period-prevalence")
unadj_fn(-1, 200) 

### DYS and Social phobia [adjusted] 
ma2 <- ma %>% filter(dys_so_adj == 1 & temporality == "period-prevalence")
adj_fn(-1, 100)

### DYS and Specific phobia [unadjusted] 

ma1 <- ma %>% filter(dys_sp_unadj == 1 & temporality == "period-prevalence")
unadj_fn( -1, 100)

### DYS and Specific phobia [adjusted] 

  ### DYS and Anxiety disorder [unadjusted] 

  ### DYS and Anxiety disorder [adjusted] 
 
  # Table 5. Broadly-defined bipolar disorder (BIPOLAR) and anxiety disorders
 
  
  ##  Lifetime estimates
 
  
  ### Bipolar and Agoraphobia [unadjusted] 
  
  
 
ma1 <- ma %>% filter(bi3_ago_unadj == 1 & temporality == "lifetime")
unadj_fn( -1, 150) 
 

### Bipolar and Agoraphobia [adjusted] 

 
  
  ### Bipolar and OCD [unadjusted] 
  
 
ma1 <- ma %>% filter(bi3_ocd_unadj == 1 & temporality == "lifetime")
unadj_fn( -1, 300) 
 

### Bipolar and OCD [adjusted]  
 
ma2 <- ma %>% filter(bi3_ocd_adj == 1 & temporality == "lifetime")
adj_fn( -1, 100)
 



### Bipolar and GAD [unadjusted] 

 
ma1 <- ma %>% filter(bi3_gad_unadj == 1 & temporality == "lifetime")
unadj_fn( -1, 150) 
 

### Bipolar and GAD [adjusted]   
 
ma2 <- ma %>% filter(bi3_gad_adj == 1 & temporality == "lifetime")
adj_fn( -1, 100)
 



### Bipolar and Panic disorder [unadjusted] 

 
ma1 <- ma %>% filter(bi3_pd_unadj == 1 & temporality == "lifetime")
unadj_fn( -1, 150) 
 

### Bipolar and Panic disorder [adjusted] 
 
ma2 <- ma %>% filter(bi3_pd_adj == 1 & temporality == "lifetime")
adj_fn( -1, 100)
 


### Bipolar and PTSD [unadjusted] 
 
  
  ### Bipolar and PTSD [adjusted] 
  
 
ma2 <- ma %>% filter(bi3_pts_adj == 1 & temporality == "lifetime")
adj_fn( -1, 10)
 

### Bipolar and Social phobia [unadjusted] 

 
ma1 <- ma %>% filter(bi3_so_unadj == 1 & temporality == "lifetime")
unadj_fn( -1, 50) 
 

### Bipolar and Social phobia [adjusted] 
 
ma2 <- ma %>% filter(bi3_so_adj == 1 & temporality == "lifetime")
adj_fn( -1, 10)
 


### Bipolar and Specific phobia [unadjusted] 
 
ma1 <- ma %>% filter(bi3_sp_unadj == 1 & temporality == "lifetime")
unadj_fn( -1, 50) 
 

### Bipolar and Specific phobia [adjusted] 
 
ma2 <- ma %>% filter(bi3_sp_adj == 1 & temporality == "lifetime")
adj_fn( -1, 10)
 

### Bipolar and Anxiety disorder [unadjusted] 
 
ma1 <- ma %>% filter(bi3_anx_unadj == 1 & temporality == "lifetime")
unadj_fn( -1, 200) 
 

### Bipolar and Anxiety disorder [adjusted] 
 
ma2 <- ma %>% filter(bi3_anx_adj == 1 & temporality == "lifetime")
adj_fn(-1, 20)
 

##  Period-prevalence estimates
 
  
  ### Bipolar and Agoraphobia [unadjusted] 
 
  
  ### Bipolar and Agoraphobia [adjusted] 
 
  
  ### Bipolar and OCD [unadjusted] 
  
 
ma1 <- ma %>% filter(bi3_ocd_unadj == 1 & temporality == "period-prevalence")
unadj_fn( -1, 150) 
 

### Bipolar and OCD [adjusted] 
 
ma2 <- ma %>% filter(bi3_ocd_adj == 1 & temporality == "period-prevalence")
adj_fn( -1, 100)
 

### Bipolar and GAD [unadjusted] 

 
ma1 <- ma %>% filter(bi3_gad_unadj == 1 & temporality == "period-prevalence")
unadj_fn( -1, 150) 
 

### Bipolar and GAD [adjusted] 
 
ma2 <- ma %>% filter(bi3_gad_adj == 1 & temporality == "period-prevalence")
adj_fn( -1, 100)


### Bipolar and Panic disorder [unadjusted] 


ma1 <- ma %>% filter(bi3_pd_unadj == 1 & temporality == "period-prevalence")
unadj_fn( -1, 150) 
 

### Bipolar and Panic disorder [adjusted] 

  
  ### Bipolar and PTSD [unadjusted] 

  
  ### Bipolar and PTSD [adjusted] 

  
  ### Bipolar and Social phobia [unadjusted] 
  
ma1 <- ma %>% filter(bi3_so_unadj == 1 & temporality == "period-prevalence")
unadj_fn( -1, 50) 

### Bipolar and Social phobia [adjusted] 

ma2 <- ma %>% filter(bi3_so_adj == 1 & temporality == "period-prevalence")
adj_fn( -1, 10)

### Bipolar and Specific phobia [unadjusted]
ma1 <- ma %>% filter(bi3_sp_unadj == 1 & temporality == "period-prevalence")
unadj_fn( -1, 50) 

### Bipolar and Specific phobia [adjusted]

  ### Bipolar and Anxiety disorder [unadjusted]
ma1 <- ma %>% filter(bi3_anx_unadj == 1 & temporality == "period-prevalence")
unadj_fn( -1, 200) 

rlang::last_error()


### Bipolar and Anxiety disorder [unadjusted]
dev.off()  
