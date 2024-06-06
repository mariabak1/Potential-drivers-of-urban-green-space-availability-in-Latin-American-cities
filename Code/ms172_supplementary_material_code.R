# Potential drivers of urban green space availability in Latin American cities
# THE CODE REPRODUCES REGRESSIONS FROM THE SUPPLEMENTARY MATERIAL


library(ggplot2)
library(dplyr)
library(skimr)
library(data.table)
library(reshape2)
library(foreign)
library(car)
options("scipen"=100, "digits"=3) # to force R not to use scientific notations


varlabels = read.csv("ms172_varlabs.csv") # load descriptive variables' names for tables
source("ms172_functions.R") # functions to create tables


# 4. Regressions ####
# (A) Categorical city variables -- Supplementary Table 6 #####
becl1uxt = becl1uxt %>%
  filter(!climzone == "polar")


# ndvi
mod = lm(BECMEDNDVINW2017L1UX ~ climzone + topogroup + coast+ 
           popgroup + popdensity + interdens + unemplgroup + educgroup + GDPgroup, data=becl1uxt)
summary(mod)
vif(mod)
table1=create.table(mod)


mod = lm(BECGSPCTL1UX ~ climzone + topogroup + coast+ 
           popgroup + popdensity + interdens + unemplgroup + educgroup + GDPgroup, data=becl1uxt)
summary(mod)
table2=create.table(mod)


mod = lm(gspercapita ~ climzone + topogroup + coast+ 
           popgroup + popdensity + interdens + unemplgroup + educgroup + GDPgroup
         , data=becl1uxt)
summary(mod)
table3=create.table(mod)


mod = lm(BECGSPTCHDENSL1UX ~ climzone + topogroup + coast+ 
           popgroup + popdensity + interdens + unemplgroup + educgroup + GDPgroup
         + BECGSPCTL1UX, data=becl1uxt)
summary(mod)
table4=create.table(mod)
table4 = table4[!table4$Variable == 'BECGSPCTL1UX',]

mod = lm(BECGSMNNNGHL1UX ~ climzone + topogroup + coast+ 
           popgroup + popdensity + interdens + unemplgroup + educgroup + GDPgroup
         + BECGSPCTL1UX, data=becl1uxt)
summary(mod)
table5=create.table(mod)
table5 = table5[!table5$Variable == 'BECGSPCTL1UX',]


mod = lm(BECGSCLUMPL1UX ~ climzone + topogroup + coast+ 
           popgroup + popdensity + interdens + unemplgroup + educgroup + GDPgroup
         + BECGSPCTL1UX, data=becl1uxt)
summary(mod)
table6=create.table(mod)
table6 = table6[!table6$Variable == 'BECGSPCTL1UX',]


mod = lm(BECPARKNUMPERCAPL1UX ~ climzone + topogroup + coast+ 
           popgroup + popdensity + interdens + unemplgroup + educgroup + GDPgroup
         , data=becl1uxt)
summary(mod)
table7=create.table(mod)


mod = lm(BECPARKTAREAPERCAPL1UX ~ climzone + topogroup + coast+ 
           popgroup + popdensity + interdens + unemplgroup + educgroup + GDPgroup
         , data=becl1uxt)
summary(mod)
vif(mod)
table8=create.table(mod)



output = cbind(table1, table2, table3, table4, table5, table6, table7, table8)
write.csv(output, file.path(tables, "model_cat.csv"), row.names = F)



#### (B) Regressions, continuous green space and other variables #####
# Supplemental Table 8  #
becl1uxt = becl1uxt %>% # standardize to z-scores
  mutate(popsc = scale(BECTPOP2017L1UX),
         popdenss=scale(BECPOPDENS2017L1UX),
         interdenss = scale(BECADINTDENSL1UX),
         unempls = scale(CNSUNEMPL1AD),
         educs = scale(CNSMINHSL1AD),
         GDPs = scale(SECGDPGPPC))

# ndvi
mod = lm(BECMEDNDVINW2017L1UX ~ climzone + topogroup + coast+ 
           popsc + popdenss + interdenss + unempls + educs + GDPs, data=becl1uxt)
summary(mod)
vif(mod)
table1=create.table(mod)


mod = lm(BECMEDNDVINW2017L1UX ~ climzone*educgroup + topogroup + coast+
           popsc + popdenss  + interdenss + unempls + educs + GDPs, data=becl1uxt)
summary(mod)
vif(mod)

mod = lm(BECGSPCTL1UX ~ climzone + topogroup + coast+ 
           popsc + popdenss + interdenss + unempls + educs + GDPs, data=becl1uxt)
summary(mod)
table2=create.table(mod)


mod = lm(gspercapita ~ climzone + topogroup + coast+ 
           popsc + popdenss + interdenss + unempls + educs + GDPs
         , data=becl1uxt)
summary(mod)
table3=create.table(mod)


mod = lm(BECGSPTCHDENSL1UX ~ climzone + topogroup + coast+ 
           popsc + popdenss + interdenss + unempls + educs + GDPs
         + BECGSPCTL1UX, data=becl1uxt)
summary(mod)
table4=create.table(mod)
table4 = table4[!table4$Variable == 'BECGSPCTL1UX',]

mod = lm(BECGSMNNNGHL1UX ~ climzone + topogroup + coast+ 
           popsc + popdenss + interdenss + unempls + educs + GDPs
         + BECGSPCTL1UX, data=becl1uxt)
summary(mod)
table5=create.table(mod)
table5 = table5[!table5$Variable == 'BECGSPCTL1UX',]


mod = lm(BECGSCLUMPL1UX ~ climzone + topogroup + coast+ 
           popsc + popdenss + interdenss + unempls + educs + GDPs
         + BECGSPCTL1UX, data=becl1uxt)
summary(mod)
table6=create.table(mod)
table6 = table6[!table6$Variable == 'BECGSPCTL1UX',]


mod = lm(BECPARKNUMPERCAPL1UX ~ climzone + topogroup + coast+ 
           popsc + popdenss + interdenss + unempls + educs + GDPs
         , data=becl1uxt)
summary(mod)
table7=create.table(mod)


mod = lm(BECPARKTAREAPERCAPL1UX ~ climzone + topogroup + coast+ 
           popsc + popdenss + interdenss + unempls + educs + GDPs
         , data=becl1uxt)
summary(mod)
vif(mod)
table8=create.table(mod)



output_cont = cbind(table1, table2, table3, table4, table5, table6, table7, table8)
write.csv(output_cont, file.path(tables, "model_cont.csv"), row.names = F)



# (C) Regressions, random intercepts for countries #####
# Supplemental Table 7

library(lme4)
library(lmerTest)


# ndvi
mod = lmer(BECMEDNDVINW2017L1UX ~ climzone + topogroup + coast+ 
             popgroup + popdensity + interdens + unemplgroup + educgroup + GDPgroup + (1|Country), data=becl1uxt)
summary(mod)
vif(mod)
table1=create.table_ml(mod)
coef(summary(mod))

mod = lmer(BECGSPCTL1UX ~ climzone + topogroup + coast+ 
             popgroup + popdensity + interdens + unemplgroup + educgroup + GDPgroup + (1|Country), data=becl1uxt)
summary(mod)
table2=create.table_ml(mod)


mod = lmer(gspercapita ~ climzone + topogroup + coast+ 
             popgroup + popdensity + interdens + unemplgroup + educgroup + GDPgroup + (1|Country)
           , data=becl1uxt)
summary(mod)
table3=create.table_ml(mod)


mod = lmer(BECGSPTCHDENSL1UX ~ climzone + topogroup + coast+ 
             popgroup + popdensity + interdens + unemplgroup + educgroup + GDPgroup + (1|Country)
           + BECGSPCTL1UX, data=becl1uxt)
summary(mod)
table4=create.table_ml(mod)
table4 = table4[!table4$Variable == 'BECGSPCTL1UX',]

mod = lmer(BECGSMNNNGHL1UX ~ climzone + topogroup + coast+ 
             popgroup + popdensity + interdens + unemplgroup + educgroup + GDPgroup + (1|Country)
           + BECGSPCTL1UX, data=becl1uxt)
summary(mod)
table5=create.table_ml(mod)
table5 = table5[!table5$Variable == 'BECGSPCTL1UX',]


mod = lmer(BECGSCLUMPL1UX ~ climzone + topogroup + coast+ 
             popgroup + popdensity + interdens + unemplgroup + educgroup + GDPgroup + (1|Country)
           + BECGSPCTL1UX, data=becl1uxt)
summary(mod)
table6=create.table_ml(mod)
table6 = table6[!table6$Variable == 'BECGSPCTL1UX',]


mod = lmer(BECPARKNUMPERCAPL1UX ~ climzone + topogroup + coast+ 
             popgroup + popdensity + interdens + unemplgroup + educgroup + GDPgroup + (1|Country)
           , data=becl1uxt)
summary(mod)
table7=create.table_ml(mod)


mod = lmer(BECPARKTAREAPERCAPL1UX ~ climzone + topogroup + coast+ 
             popgroup + popdensity  + interdens + unemplgroup + educgroup + GDPgroup + (1|Country)
           , data=becl1uxt)
summary(mod)
table8=create.table_ml(mod)



output_ml = cbind(table1, table2, table3, table4, table5, table6, table7, table8)
write.csv(output_ml, file.path(tables, "model_cat_ml.csv"), row.names = F)


