# Potential drivers of urban green space availability in Latin American cities
# The code reproduces the analyses from the main text (Table 3, Figure 1, Box 1) as well as summary statistics table from the Supplementary Material

library(ggplot2)
library(dplyr)
library(skimr)
library(data.table)
library(reshape2)
library(foreign)
library(car)
options("scipen"=100, "digits"=3) # to force R not to use scientific notations

source("ms172_functions.R") # helper functions 

graphs = "descr_paper_greenspace/graphs"
working = "descr_paper_greenspace/working"
tables = "descr_paper_greenspace/tables"

becl1=read.csv("BEC_L1AD_20210104.csv")
becl1ux=read.csv("becux_upd.csv")
secensus=read.csv("SEC_Census_L1AD_02032021.csv")
secensus$un=paste(secensus$ISO2, secensus$YEAR, sep = "-")
table(secensus$un)


citynames=read.dbf("L1UX/level1_gcs_ux_modify4_20191127.dbf") %>%
  select(SALID1, L1Name)


secensus = secensus %>%
  filter(!un == "CL-2002") %>%
  filter(!un == "GT-2002" ) %>%
  filter(!un == "PE-2007") %>%
  filter(!un == "CO-2005") %>%
  select(CNSUNEMPL1AD, CNSMINHSL1AD, SALID1)

summary(secensus)

gdp=read.csv("SECGDPGPC_L1AD_20200124.csv")
gdp$un=paste(gdp$ISO2, gdp$YEAR, sep = "-")
table(gdp$un) # br, cl, mx only
gdp = gdp %>% 
  filter(YEAR == 2015)%>%
  dplyr::select(SALID1, SECGDPGPPC)
hist(gdp$SECGDPGPPC)


becl1ux = merge(becl1ux, secensus, by="SALID1") %>%
  merge(., gdp, by="SALID1", all.x = T)%>%
  merge(., citynames, by="SALID1", all.x = T)%>%
  merge(., becl1[,c("SALID1", "ISO2", 'BECTPOP2017L1AD')], all.x=T)
  
becl1ux = becl1ux %>%
  select(SALID1, BECTPOP2017L1UX, BECPOPDENS2017L1UX, BECPCTURBANL1UX,
         BECMEDNDVINW2017L1UX, BECTGSAREAL1UX, BECGSPCTL1UX, 
         BECGSPTCHDENSL1UX, 
         BECGSMNNNGHL1UX, BECGSCLUMPL1UX,
         BECGSAWMNNNGHL1UX,
         BECPARKNUMPERCAPL1UX, BECPARKTAREAPERCAPL1UX, 
         BECCZL1UX, BECCOASTL1UX, BECSLOPEAVEL1UX,
         BECADAREAL1UX, BECADINTDENSL1UX,
         CNSUNEMPL1AD, CNSMINHSL1AD, SECGDPGPPC, BECELEVATIONP75L1UX, BECPARKNUML1UX, L1Name.x, Country, ISO2, BECTPOP2017L1AD) 

becl1ux = becl1ux %>%
  mutate(gspercapita = BECTGSAREAL1UX*10000 / BECTPOP2017L1UX*1000 ) # m2 per 1000 residents
summary(becl1ux$gspercapita)

becl1ux$label = paste(becl1ux$L1Name.x, becl1ux$ISO2, sep = ", ")


########## Analysis ##########

# 1. Create categorical variables ######

becl1uxt = becl1ux

becl1uxt$topogroup = ifelse(((100*tan((becl1uxt$BECSLOPEAVEL1UX*pi/180)) <= 6)), "Flat", "Hilly") # convert to percent: pct = [100*tan(slope in degrees)*pi] / 180; 
becl1uxt$coast=as.factor(becl1uxt$BECCOASTL1UX)

becl1uxt$climzone[becl1uxt$BECCZL1UX == 5 | becl1uxt$BECCZL1UX == 6 |becl1uxt$BECCZL1UX == 7|becl1uxt$BECCZL1UX == 8 ] = "arid"
becl1uxt$climzone[becl1uxt$BECCZL1UX >= 9 & becl1uxt$BECCZL1UX <= 17 ] = "temperate"
becl1uxt$climzone[becl1uxt$BECCZL1UX <= 4 ] = "tropical"
becl1uxt$climzone[becl1uxt$BECCZL1UX <= 4 ] = "tropical"
becl1uxt$climzone[becl1uxt$BECCZL1UX == 31 ] = "polar"

#puno = becl1 %>%
#  filter(SALID1 == 105119)

becl1uxt$cityareagroup<-cut(becl1uxt$BECADAREAL1UX, 
                       breaks=quantile(becl1uxt$BECADAREAL1UX, probs=c(0:3/3), na.rm=T),
                       labels=c("L","M","H"), include.lowest = T)
table(becl1uxt$cityareagroup)

becl1uxt$popgroup<-cut(becl1uxt$BECTPOP2017L1UX	, 
                       breaks=quantile(becl1uxt$BECTPOP2017L1UX	, probs=c(0:3/3), na.rm=T),
                       labels=c("L","M","H"), include.lowest = T)

becl1uxt$popdensity<-cut(becl1uxt$BECPOPDENS2017L1UX, 
                            breaks=quantile(becl1uxt$BECPOPDENS2017L1UX, probs=c(0:3/3), na.rm=T),
                            labels=c("L","M","H"), include.lowest = T)
table(becl1uxt$popdensity)
becl1uxt$interdens<-cut(becl1uxt$BECADINTDENSL1UX, 
                      breaks=quantile(becl1uxt$BECADINTDENSL1UX, probs=c(0:3/3), na.rm=T),
                      labels=c("L","M","H"), include.lowest = T)

becl1uxt$GDPgroup<-cut(becl1uxt$SECGDPGPPC, 
                      breaks=quantile(becl1uxt$SECGDPGPPC, probs=c(0:3/3), na.rm=T),
                      labels=c("L","M","H"), include.lowest = T)

becl1uxt$educgroup<-cut(becl1uxt$CNSMINHSL1AD, 
                       breaks=quantile(becl1uxt$CNSMINHSL1AD, probs=c(0:3/3), na.rm=T),
                       labels=c("L","M","H"), include.lowest = T)

becl1uxt$unemplgroup<-cut(becl1uxt$CNSUNEMPL1AD, 
                              breaks=quantile(becl1uxt$CNSUNEMPL1AD, probs=c(0:3/3), na.rm=T),
                              labels=c("L","M","H"))
summary(becl1uxt$CNSUNEMPL1AD)


groupvar = c("climzone", "topogroup", "coast", 
             "cityareagroup", "popgroup", "popdensity", "interdens",
             "GDPgroup", "unemplgroup", "educgroup") 





# 2. Compute summary statistics (mean and sd) for each group #####
sumtable = list()

for (i in groupvar) {
  sumtable[[i]] = becl1uxt %>% 
    group_by(!!rlang::sym(i))%>%
    skim()%>%
    mutate(groupv=i)
  sumtable[[i]]=as.data.frame(sumtable[[i]])
}

sumt=as.data.frame(do.call(bind_rows, sumtable))%>%
  filter(skim_variable %in% c("BECMEDNDVINW2017L1UX", "BECGSPCTL1UX", "gspercapita",
                              "BECPARKNUMPERCAPL1UX", "BECPARKTAREAPERCAPL1UX",
         "BECGSPTCHDENSL1UX", "BECGSMNNNGHL1UX", "BECGSCLUMPL1UX"))%>%
  mutate(mean_sd = paste(round(numeric.mean, 2), " ","(",round(numeric.sd, 2), ")", sep = "" ))

write.csv(sumt, "tables/sumstats_by_group.csv", row.names = F)

table(becl1$ISO2)
becl1uxt = merge(becl1uxt, becl1[,c("SALID1", "ISO2")], by = "SALID1")
becl1uxt = becl1uxt%>%
  mutate(ISO2=ISO2.y)
table(becl1uxt$ISO2)





# 3. Anova #####

# NDVI

nopolarzone = becl1uxt %>%
  filter(!climzone == "polar")
test = aov(BECMEDNDVINW2017L1UX~climzone, data=nopolarzone)
summary(test)
TukeyHSD(test)

t.test(x=becl1uxt$BECMEDNDVINW2017L1UX[becl1uxt$topogroup=="Hilly"], y = becl1uxt$BECMEDNDVINW2017L1UX[becl1uxt$topogroup=="Flat"] )
test = aov(BECMEDNDVINW2017L1UX~topogroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECMEDNDVINW2017L1UX~coast, data=becl1uxt)
summary(test)
TukeyHSD(test)
summary(test)

test = aov(SECGDPGPPC~climzone, data=becl1uxt)
summary(test)
TukeyHSD(test)
summary(test)

test = aov(CNSUNEMPL1AD~climzone, data=becl1uxt)
summary(test)
TukeyHSD(test)
summary(test)

test = aov(CNSMINHSL1AD~climzone, data=becl1uxt)
summary(test)
TukeyHSD(test)
summary(test)

test = aov(BECMEDNDVINW2017L1UX~cityareagroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECMEDNDVINW2017L1UX~popgroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECMEDNDVINW2017L1UX~popgroup2, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECMEDNDVINW2017L1UX~popdensity, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECMEDNDVINW2017L1UX~popconc, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECMEDNDVINW2017L1UX~interdens, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECMEDNDVINW2017L1UX~GDPgroup, data=becl1uxt)
summary(test)
TukeyHSD(test)

test = aov(BECMEDNDVINW2017L1UX~ginigroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECMEDNDVINW2017L1UX~livcondgroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECMEDNDVINW2017L1UX~educgroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECMEDNDVINW2017L1UX~unemplgroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECMEDNDVINW2017L1UX~povertygroup, data=becl1uxt)
TukeyHSD(test)
summary(test)



# % green
nopolarzone = becl1uxt %>%
  filter(!climzone == "polar")
test = aov(BECGSPCTL1UX~climzone, data=nopolarzone)
TukeyHSD(test)
summary(test)

test = aov(BECGSPCTL1UX~topogroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSPCTL1UX~coast, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSPCTL1UX~cityareagroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSPCTL1UX~popgroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSPCTL1UX~popdensity, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSPCTL1UX~popconc, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSPCTL1UX~interdens, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSPCTL1UX~GDPgroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSPCTL1UX~ginigroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSPCTL1UX~livcondgroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSPCTL1UX~educgroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSPCTL1UX~unemplgroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSPCTL1UX~povertygroup, data=becl1uxt)
TukeyHSD(test)
summary(test)



# gs pre capita
test = aov(gspercapita~climzone, data=nopolarzone)
TukeyHSD(test)
summary(test)

test = aov(gspercapita~topogroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(gspercapita~coast, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(gspercapita~cityareagroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(gspercapita~popgroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(gspercapita~popdensity, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(gspercapita~popconc, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(gspercapita~interdens, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(gspercapita~GDPgroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(gspercapita~ginigroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(gspercapita~livcondgroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(gspercapita~educgroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(gspercapita~unemplgroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(gspercapita~povertygroup, data=becl1uxt)
TukeyHSD(test)
summary(test)


# n parks per capita
test = aov(BECPARKNUMPERCAPL1UX~climzone, data=nopolarzone)
TukeyHSD(test)
summary(test)

test = aov(BECPARKNUMPERCAPL1UX~topogroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECPARKNUMPERCAPL1UX~coast, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECPARKNUMPERCAPL1UX~cityareagroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECPARKNUMPERCAPL1UX~popgroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECPARKNUMPERCAPL1UX~popdensity, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECPARKNUMPERCAPL1UX~popconc, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECPARKNUMPERCAPL1UX~interdens, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECPARKNUMPERCAPL1UX~GDPgroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECPARKNUMPERCAPL1UX~ginigroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECPARKNUMPERCAPL1UX~livcondgroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECPARKNUMPERCAPL1UX~educgroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECPARKNUMPERCAPL1UX~unemplgroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECPARKNUMPERCAPL1UX~povertygroup, data=becl1uxt)
TukeyHSD(test)
summary(test)


# park area per capita
test = aov(BECPARKTAREAPERCAPL1UX~climzone, data=nopolarzone)
TukeyHSD(test)
summary(test)

test = aov(BECPARKTAREAPERCAPL1UX~topogroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECPARKTAREAPERCAPL1UX~coast, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECPARKTAREAPERCAPL1UX~cityareagroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECPARKTAREAPERCAPL1UX~popgroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECPARKTAREAPERCAPL1UX~popdensity, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECPARKTAREAPERCAPL1UX~popconc, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECPARKTAREAPERCAPL1UX~interdens, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECPARKTAREAPERCAPL1UX~GDPgroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECPARKTAREAPERCAPL1UX~ginigroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECPARKTAREAPERCAPL1UX~livcondgroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECPARKTAREAPERCAPL1UX~educgroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECPARKTAREAPERCAPL1UX~unemplgroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECPARKTAREAPERCAPL1UX~povertygroup, data=becl1uxt)
TukeyHSD(test)
summary(test)



# patch density
test = aov(BECGSPTCHDENSL1UX~climzone, data=nopolarzone)
TukeyHSD(test)
summary(test)

test = aov(BECGSPTCHDENSL1UX~topogroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSPTCHDENSL1UX~coast, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSPTCHDENSL1UX~cityareagroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSPTCHDENSL1UX~popgroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSPTCHDENSL1UX~popdensity, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSPTCHDENSL1UX~popconc, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSPTCHDENSL1UX~interdens, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSPTCHDENSL1UX~GDPgroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSPTCHDENSL1UX~ginigroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSPTCHDENSL1UX~livcondgroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSPTCHDENSL1UX~educgroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSPTCHDENSL1UX~unemplgroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSPTCHDENSL1UX~povertygroup, data=becl1uxt)
TukeyHSD(test)
summary(test)


# isolation
test = aov(BECGSMNNNGHL1UX~climzone, data=nopolarzone)
TukeyHSD(test)
summary(test)

test = aov(BECGSMNNNGHL1UX~topogroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSMNNNGHL1UX~coast, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSMNNNGHL1UX~cityareagroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSMNNNGHL1UX~popgroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSMNNNGHL1UX~popdensity, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSMNNNGHL1UX~popconc, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSMNNNGHL1UX~interdens, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSMNNNGHL1UX~GDPgroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSMNNNGHL1UX~ginigroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSMNNNGHL1UX~livcondgroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSMNNNGHL1UX~educgroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSMNNNGHL1UX~unemplgroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSMNNNGHL1UX~povertygroup, data=becl1uxt)
TukeyHSD(test)
summary(test)


# clumpiness
test = aov(BECGSCLUMPL1UX~climzone, data=nopolarzone)
TukeyHSD(test)
summary(test)

test = aov(BECGSCLUMPL1UX~topogroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSCLUMPL1UX~coast, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSCLUMPL1UX~cityareagroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSCLUMPL1UX~popgroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSCLUMPL1UX~popdensity, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSCLUMPL1UX~popconc, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSCLUMPL1UX~interdens, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSCLUMPL1UX~GDPgroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSCLUMPL1UX~ginigroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSCLUMPL1UX~livcondgroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSCLUMPL1UX~educgroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSCLUMPL1UX~unemplgroup, data=becl1uxt)
TukeyHSD(test)
summary(test)

test = aov(BECGSCLUMPL1UX~povertygroup, data=becl1uxt)
TukeyHSD(test)
summary(test)






# 4. Box 1 - City examples #####
sel = becl1uxt %>%
  filter(L1Name.x == "Teresopolis" | L1Name.x == "Buenos Aires" | L1Name.x == "Lima")%>%
  select(L1Name.x, BECGSPCTL1UX,
         BECMEDNDVINW2017L1UX,
         BECGSPTCHDENSL1UX,
         BECGSCLUMPL1UX,
         BECGSMNNNGHL1UX,
         BECPARKNUMPERCAPL1UX, BECPARKTAREAPERCAPL1UX, gspercapita)

# city examples
sel2 = becl1uxt %>%
  select(L1Name.x, BECGSPCTL1UX,
         BECMEDNDVINW2017L1UX,
        climzone, Country, BECCZL1UX)


# city examples
sel = becl1uxt %>%
  select(L1Name.x, Country, climzone, BECTPOP2017L1UX,BECTPOP2017L1AD,
         BECMEDNDVINW2017L1UX, BECGSPCTL1UX, gspercapita,
         BECGSPTCHDENSL1UX, BECGSMNNNGHL1UX, BECGSCLUMPL1UX,
         BECPARKNUMPERCAPL1UX, BECPARKTAREAPERCAPL1UX)



# 5. Correlation matrix ######
# select variables for the correlation matrix
datacor2 = becl1ux %>% dplyr::select(BECGSPCTL1UX,
                                     BECMEDNDVINW2017L1UX,
                                     BECGSPTCHDENSL1UX,
                                     BECGSCLUMPL1UX,
                                     BECGSMNNNGHL1UX,
                                     BECPARKNUMPERCAPL1UX, BECPARKTAREAPERCAPL1UX, gspercapita)
sum(is.na(datacor2))
datacor2 = na.omit(datacor2)

# assign descriptive column names
id <- match(colnames(datacor2), varlabs_cor$pred)
colnames(datacor2) <- varlabs_cor$descr.label[id]

cortable <-  round(cor(datacor2),2)
c=as.data.frame(cortable)

# Reorder the correlation matrix
cortable <- reorder_cormat(cortable)
upper_tri <- get_upper_tri(cortable)
c2=as.data.frame(cortable)

# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)


# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 14, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)


ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    text = element_text(size=14),
    axis.text.x = element_text(color="grey20", size=14),
    axis.text.y = element_text(color="grey20", size=14),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.8),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.vjust = 1))


