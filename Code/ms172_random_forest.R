# Potential drivers of urban green space availability in Latin American cities
# The code reproduces the variables importance plot in the main text and the partial dependence plots in the supplementary material

library(ggplot2)
library(dplyr)
library(skimr)
library(data.table)
library(reshape2)

library(randomForest)
# devtools::install_github("MI2DataLab/randomForestExplainer")
library(randomForestExplainer)
library(pdp) # for parial dependence plots
library(rfinterval)
library(gridExtra)

options("scipen"=100, "digits"=3) # to force R not to use scientific notations
graphs = "descr_paper_greenspace/graphs" # direcory to store plots

varlabels = read.csv("descr_paper_greenspace/data/varlabs_RF.csv")

# reclassify climate zones to arid, temperate, tropical
becl1uxt$cz[becl1uxt$BECCZL1UX == '1'] = "1"
becl1uxt$cz[becl1uxt$BECCZL1UX == '2'] = "1"
becl1uxt$cz[becl1uxt$BECCZL1UX == '3'] = "1"
becl1uxt$cz[becl1uxt$BECCZL1UX == '4'] = "1"
becl1uxt$cz[becl1uxt$BECCZL1UX == '5'] = "2"
becl1uxt$cz[becl1uxt$BECCZL1UX == '6'] = "2"
becl1uxt$cz[becl1uxt$BECCZL1UX == '7'] = "2"
becl1uxt$cz[becl1uxt$BECCZL1UX == '8'] = "2"
becl1uxt$cz[becl1uxt$BECCZL1UX == '9'] = "3"
becl1uxt$cz[becl1uxt$BECCZL1UX == '10'] = "3"
becl1uxt$cz[becl1uxt$BECCZL1UX == '11'] = "3"
becl1uxt$cz[becl1uxt$BECCZL1UX == '12'] = "3"
becl1uxt$cz[becl1uxt$BECCZL1UX == '13'] = "3"
becl1uxt$cz[becl1uxt$BECCZL1UX == '14'] = "3"
becl1uxt$cz[becl1uxt$BECCZL1UX == '15'] = "3"
becl1uxt$cz[becl1uxt$BECCZL1UX == '16'] = "3"
becl1uxt$cz[becl1uxt$BECCZL1UX == '17'] = "3"
becl1uxt$cz[becl1uxt$BECCZL1UX == '31'] = "4"

table(becl1uxt$cz)

becl1uxt=becl1uxt%>%
  filter(!cz==4 ) # exclude a polar city

# one-hot encoding for climate zone
becl1uxt = becl1uxt%>%
  mutate(cz_trop = ifelse(climzone=="tropical",1,0),
         cz_arid = ifelse(climzone=="arid",1,0),
         cz_temp = ifelse(climzone=="temperate",1,0))


# NDVI #####

set.seed(2017)

becl1ux_rf = becl1uxt %>% #  
  filter(!cz==4 ) %>%
  mutate(BECTPOP2017L1UX=BECTPOP2017L1UX/1000)%>%
  select(BECPOPDENS2017L1UX, BECTPOP2017L1UX, topogroup, coast, BECADINTDENSL1UX,
         BECMEDNDVINW2017L1UX, CNSUNEMPL1AD, CNSMINHSL1AD, SECGDPGPPC, cz_trop, cz_temp, cz_arid)

id <- match(colnames(becl1ux_rf), varlabels$pred)
colnames(becl1ux_rf) <- varlabels$descr.label[id]


forest <- randomForest(NDVI ~ ., data = becl1ux_rf, localImp = TRUE, mtry=3, ntree=500)
forest
forest$mse[length(forest$mse)]
sqrt(forest$mse[length(forest$mse)])

# variable importance plot
p=vip(forest)

p =  p+
  ggtitle("NDVI")+
  theme_light()+
  theme(axis.text  = element_text(color="black", size=10))+
  ylab("Relative variable importance (%)")
p


# partial dependence plots
p1=ggplot(data=partial(forest, pred.var = "Population"), aes(x=Population, y=yhat)) +
  #geom_line()+
  geom_smooth(method="auto", se=F, fullrange=T)+
  geom_rug(aes(Population, y = NULL), alpha = 0.5, data = becl1ux_rf)+
  ylab("NDVI")+
  xlab("Population (thousands)")+
  theme_light()+
  theme(axis.text = element_text(size=13))+
  theme(axis.title = element_text(size=13))
p1


p2=ggplot(data=partial(forest, pred.var = "Pop_density"), aes(x=Pop_density, y=yhat)) +
  #geom_line()+
  geom_smooth(method="auto", se=F, fullrange=T)+
  geom_rug(aes(Pop_density, y = NULL), alpha = 0.5, data = becl1ux_rf)+
  ylab("NDVI")+
  xlab("Population density (per sq. km)")+
  theme_light()+
  theme(axis.text = element_text(size=13))+
  theme(axis.title = element_text(size=13))
p2


p3=ggplot(data=partial(forest, pred.var = "Intersection_density"), aes(x=Intersection_density, y=yhat)) +
  #geom_line()+
  geom_smooth(method="auto", se=F, fullrange=T)+
  geom_rug(aes(Intersection_density, y = NULL), alpha = 0.5, data = becl1ux_rf)+
  ylab("NDVI")+
  xlab("Intersection density (per sq. km)")+
  theme_light()+
  theme(axis.text = element_text(size=13))+
  theme(axis.title = element_text(size=13))
p3


p4=ggplot(data=partial(forest, pred.var = "GDP_per_capita"), aes(x=GDP_per_capita, y=yhat)) +
  #geom_line()+
  geom_smooth(method="auto", se=F, fullrange=T)+
  geom_rug(aes(GDP_per_capita, y = NULL), alpha = 0.5, data = becl1ux_rf)+
  ylab("NDVI")+
  xlab("GDP per capita (2011 International US$)")+
  theme_light()+
  theme(axis.text = element_text(size=13))+
  theme(axis.title = element_text(size=13))
p4


p5=ggplot(data=partial(forest, pred.var = "Unemployment"), aes(x=Unemployment, y=yhat)) +
  #geom_line()+
  geom_smooth(method="auto", se=F, fullrange=T)+
  geom_rug(aes(Unemployment, y = NULL), alpha = 0.5, data = becl1ux_rf)+
  ylab("NDVI")+
  xlab("Unemployment (%)")+
  theme_light()+
  theme(axis.text = element_text(size=13))+
  theme(axis.title = element_text(size=13))
p5


p6=ggplot(data=partial(forest, pred.var = "Education"), aes(x=Education, y=yhat)) +
  #geom_line()+
  geom_smooth(method="auto", se=F, fullrange=T)+
  geom_rug(aes(Education, y = NULL), alpha = 0.5, data = becl1ux_rf)+
  ylab("NDVI")+
  xlab("Education (%)")+
  theme_light()+
  theme(axis.text = element_text(size=13))+
  theme(axis.title = element_text(size=13))
p6


combined_plot_grob <- arrangeGrob(p1, p2, p3, p4, p5, p6, ncol = 3)
ggsave("pdp_ndvi.png", combined_plot_grob, width = 17 ,height = 10)





# % green: 41%, 8.5% #####
becl1ux_rf = becl1uxt %>% #  
  filter(!cz==4 ) %>%
  mutate(BECTPOP2017L1UX = BECTPOP2017L1UX/1000)%>%
  select(BECPOPDENS2017L1UX, BECTPOP2017L1UX, topogroup, coast, BECADINTDENSL1UX,
         BECGSPCTL1UX, CNSUNEMPL1AD, CNSMINHSL1AD, SECGDPGPPC, cz_trop, cz_temp, cz_arid)

id <- match(colnames(becl1ux_rf), varlabels$pred)
colnames(becl1ux_rf) <- varlabels$descr.label[id]


forest <- randomForest(`%_green_space_area` ~ ., data = becl1ux_rf, localImp = TRUE, mtry=3, ntree=500)
forest
forest$mse[length(forest$mse)]
sqrt(forest$mse[length(forest$mse)])

# variable importance plot
p2_pctgs=vip(forest)

p2_pctgs=p2_pctgs +
  ggtitle("% Green space area")+
  theme_light()+
  theme(axis.text  = element_text(color="black", size=10))+
  ylab("Relative variable importance (%)")
  

forest$mse[length(forest$mse)]
sqrt(forest$mse[length(forest$mse)])




# partial depence plots

p1=ggplot(data=partial(forest, pred.var = "Population"), aes(x=Population, y=yhat)) +
  #geom_line()+
  geom_smooth(method="auto", se=F, fullrange=T)+
  geom_rug(aes(Population, y = NULL), alpha = 0.5, data = becl1ux_rf)+
  ylab("% Green space area")+
  xlab("Population (thousands)")+
  theme_light()+
  theme(axis.text = element_text(size=13))+
  theme(axis.title = element_text(size=13))
p1


p2=ggplot(data=partial(forest, pred.var = "Pop_density"), aes(x=Pop_density, y=yhat)) +
  #geom_line()+
  geom_smooth(method="auto", se=F, fullrange=T)+
  geom_rug(aes(Pop_density, y = NULL), alpha = 0.5, data = becl1ux_rf)+
  ylab("% Green space area")+
  xlab("Population density (per sq. km)")+
  theme_light()+
  theme(axis.text = element_text(size=13))+
  theme(axis.title = element_text(size=13))
p2


p3=ggplot(data=partial(forest, pred.var = "Intersection_density"), aes(x=Intersection_density, y=yhat)) +
  #geom_line()+
  geom_smooth(method="auto", se=F, fullrange=T)+
  geom_rug(aes(Intersection_density, y = NULL), alpha = 0.5, data = becl1ux_rf)+
  ylab("% Green space area")+
  xlab("Intersection density (per sq. km)")+
  theme_light()+
  theme(axis.text = element_text(size=13))+
  theme(axis.title = element_text(size=13))
p3


p4=ggplot(data=partial(forest, pred.var = "GDP_per_capita"), aes(x=GDP_per_capita, y=yhat)) +
  #geom_line()+
  geom_smooth(method="auto", se=F, fullrange=T)+
  geom_rug(aes(GDP_per_capita, y = NULL), alpha = 0.5, data = becl1ux_rf)+
  ylab("% Green space area")+
  xlab("GDP per capita (2011 International US$)")+
  theme_light()+
  theme(axis.text = element_text(size=13))+
  theme(axis.title = element_text(size=13))
p4


p5=ggplot(data=partial(forest, pred.var = "Unemployment"), aes(x=Unemployment, y=yhat)) +
  #geom_line()+
  geom_smooth(method="auto", se=F, fullrange=T)+
  geom_rug(aes(Unemployment, y = NULL), alpha = 0.5, data = becl1ux_rf)+
  ylab("% Green space area")+
  xlab("Unemployment (%)")+
  theme_light()+
  theme(axis.text = element_text(size=13))+
  theme(axis.title = element_text(size=13))
p5


p6=ggplot(data=partial(forest, pred.var = "Education"), aes(x=Education, y=yhat)) +
  #geom_line()+
  geom_smooth(method="auto", se=F, fullrange=T)+
  geom_rug(aes(Education, y = NULL), alpha = 0.5, data = becl1ux_rf)+
  ylab("% Green space area")+
  xlab("Education (%)")+
  theme_light()+
  theme(axis.text = element_text(size=13))+
  theme(axis.title = element_text(size=13))
p6



combined_plot_grob <- arrangeGrob(p1, p2, p3, p4, p5, p6, ncol = 3)
ggsave("pdp_percent_gs.png", combined_plot_grob, width = 17 ,height = 10)




# % gs per capita: 63%, rmse 42352
becl1ux_rf = becl1uxt %>% #  
  filter(!cz==4 ) %>%
  mutate(BECTPOP2017L1UX=BECTPOP2017L1UX/1000)%>%
  select(BECPOPDENS2017L1UX, BECTPOP2017L1UX, topogroup, coast, BECADINTDENSL1UX,
         gspercapita, CNSUNEMPL1AD, CNSMINHSL1AD, SECGDPGPPC, cz_trop, cz_temp, cz_arid)

id <- match(colnames(becl1ux_rf), varlabels$pred)
colnames(becl1ux_rf) <- varlabels$descr.label[id]


forest <- randomForest(Green_space_area_per_capita ~ ., data = becl1ux_rf, localImp = TRUE, mtry=3, ntree=500)
forest
forest$mse[length(forest$mse)]
sqrt(forest$mse[length(forest$mse)])


# variable importance plot
p3=vip(forest)

p3_gspercapita=p3 +
  ggtitle("Green space area per capita")+
  theme_light()+
  theme(axis.text  = element_text(color="black", size=10))+
  ylab("Relative variable importance (%)")


forest$mse[length(forest$mse)]
sqrt(forest$mse[length(forest$mse)])


# partial dependence plots
p1=ggplot(data=partial(forest, pred.var = "Population"), aes(x=Population, y=yhat)) +
  #geom_line()+
  geom_smooth(method="auto", se=F, fullrange=T)+
  geom_rug(aes(Population, y = NULL), alpha = 0.5, data = becl1ux_rf)+
  ylab("Green space area per capita\n (m2/1,000 residents)")+
  xlab("Population (thousands)")+
  theme_light()+
  theme(axis.text = element_text(size=13))+
  theme(axis.title = element_text(size=13))
p1


p2=ggplot(data=partial(forest, pred.var = "Pop_density"), aes(x=Pop_density, y=yhat)) +
  #geom_line()+
  geom_smooth(method="auto", se=F, fullrange=T)+
  geom_rug(aes(Pop_density, y = NULL), alpha = 0.5, data = becl1ux_rf)+
  ylab("Green space area per capita\n (m2/1,000 residents)")+
  xlab("Population density (per sq. km)")+
  theme_light()+
  theme(axis.text = element_text(size=13))+
  theme(axis.title = element_text(size=13))
p2


p3=ggplot(data=partial(forest, pred.var = "Intersection_density"), aes(x=Intersection_density, y=yhat)) +
  #geom_line()+
  geom_smooth(method="auto", se=F, fullrange=T)+
  geom_rug(aes(Intersection_density, y = NULL), alpha = 0.5, data = becl1ux_rf)+
  ylab("Green space area per capita\n (m2/1,000 residents)")+
  xlab("Intersection density (per sq. km)")+
  theme_light()+
  theme(axis.text = element_text(size=13))+
  theme(axis.title = element_text(size=13))
p3


p4=ggplot(data=partial(forest, pred.var = "GDP_per_capita"), aes(x=GDP_per_capita, y=yhat)) +
  #geom_line()+
  geom_smooth(method="auto", se=F, fullrange=T)+
  geom_rug(aes(GDP_per_capita, y = NULL), alpha = 0.5, data = becl1ux_rf)+
  ylab("Green space area per capita\n (m2/1,000 residents)")+
  xlab("GDP per capita (2011 International US$)")+
  theme_light()+
  theme(axis.text = element_text(size=13))+
  theme(axis.title = element_text(size=13))
p4


p5=ggplot(data=partial(forest, pred.var = "Unemployment"), aes(x=Unemployment, y=yhat)) +
  #geom_line()+
  geom_smooth(method="auto", se=F, fullrange=T)+
  geom_rug(aes(Unemployment, y = NULL), alpha = 0.5, data = becl1ux_rf)+
  ylab("Green space area per capita\n (m2/1,000 residents)")+
  xlab("Unemployment (%)")+
  theme_light()+
  theme(axis.text = element_text(size=13))+
  theme(axis.title = element_text(size=13))
p5


p6=ggplot(data=partial(forest, pred.var = "Education"), aes(x=Education, y=yhat)) +
  #geom_line()+
  geom_smooth(method="auto", se=F, fullrange=T)+
  geom_rug(aes(Education, y = NULL), alpha = 0.5, data = becl1ux_rf)+
  ylab("Green space area per capita\n (m2/1,000 residents)")+
  xlab("Education (%)")+
  theme_light()+
  theme(axis.text = element_text(size=13))+
  theme(axis.title = element_text(size=13))
p6



combined_plot_grob <- arrangeGrob(p1, p2, p3, p4, p5, p6, ncol = 3)
ggsave("pdp_gs_per_capita.png", combined_plot_grob, width = 17 ,height = 10)






# parks # per capita: 30%, 0.193 #####
becl1ux_rf = becl1uxt %>% #  
  filter(!cz==4 ) %>%
  mutate(BECTPOP2017L1UX=BECTPOP2017L1UX/1000)%>%
  select(BECPOPDENS2017L1UX, BECTPOP2017L1UX, topogroup, coast, BECADINTDENSL1UX,
         BECPARKNUMPERCAPL1UX, CNSUNEMPL1AD, CNSMINHSL1AD, SECGDPGPPC, cz_trop, cz_temp, cz_arid)

id <- match(colnames(becl1ux_rf), varlabels$pred)
colnames(becl1ux_rf) <- varlabels$descr.label[id]

forest <- randomForest(Number_of_parks_per_capita ~ ., data = becl1ux_rf, localImp = TRUE, mtry=3, ntree=500)
forest

sqrt(forest$mse[length(forest$mse)])


# variable importance plot
p4=vip(forest)

p4_parkspercapita=p4 +
  ggtitle("Number of parks per capita")+
  theme_light()+
  theme(axis.text  = element_text(color="black", size=10))+
  ylab("Relative variable importance (%)")




# partial dependence plots

p1=ggplot(data=partial(forest, pred.var = "Population"), aes(x=Population, y=yhat)) +
  #geom_line()+
  geom_smooth(method="auto", se=F, fullrange=T)+
  geom_rug(aes(Population, y = NULL), alpha = 0.5, data = becl1ux_rf)+
  ylab("Number of parks per 1,000 residents")+
  xlab("Population (thousands)")+
  theme_light()+
  theme(axis.text = element_text(size=13))+
  theme(axis.title = element_text(size=13))
p1


p2=ggplot(data=partial(forest, pred.var = "Pop_density"), aes(x=Pop_density, y=yhat)) +
  #geom_line()+
  geom_smooth(method="auto", se=F, fullrange=T)+
  geom_rug(aes(Pop_density, y = NULL), alpha = 0.5, data = becl1ux_rf)+
  ylab("Number of parks per 1,000 residents)")+
  xlab("Population density (per sq. km)")+
  theme_light()+
  theme(axis.text = element_text(size=13))+
  theme(axis.title = element_text(size=13))
p2


p3=ggplot(data=partial(forest, pred.var = "Intersection_density"), aes(x=Intersection_density, y=yhat)) +
  #geom_line()+
  geom_smooth(method="auto", se=F, fullrange=T)+
  geom_rug(aes(Intersection_density, y = NULL), alpha = 0.5, data = becl1ux_rf)+
  ylab("Number of parks per 1,000 residents)")+
  xlab("Intersection density (per sq. km)")+
  theme_light()+
  theme(axis.text = element_text(size=13))+
  theme(axis.title = element_text(size=13))
p3


p4=ggplot(data=partial(forest, pred.var = "GDP_per_capita"), aes(x=GDP_per_capita, y=yhat)) +
  #geom_line()+
  geom_smooth(method="auto", se=F, fullrange=T)+
  geom_rug(aes(GDP_per_capita, y = NULL), alpha = 0.5, data = becl1ux_rf)+
  ylab("Number of parks per 1,000 residents)")+
  xlab("GDP per capita (2011 International US$)")+
  theme_light()+
  theme(axis.text = element_text(size=13))+
  theme(axis.title = element_text(size=13))
p4


p5=ggplot(data=partial(forest, pred.var = "Unemployment"), aes(x=Unemployment, y=yhat)) +
  #geom_line()+
  geom_smooth(method="auto", se=F, fullrange=T)+
  geom_rug(aes(Unemployment, y = NULL), alpha = 0.5, data = becl1ux_rf)+
  ylab("Number of parks per 1,000 residents)")+
  xlab("Unemployment (%)")+
  theme_light()+
  theme(axis.text = element_text(size=13))+
  theme(axis.title = element_text(size=13))
p5


p6=ggplot(data=partial(forest, pred.var = "Education"), aes(x=Education, y=yhat)) +
  #geom_line()+
  geom_smooth(method="auto", se=F, fullrange=T)+
  geom_rug(aes(Education, y = NULL), alpha = 0.5, data = becl1ux_rf)+
  ylab("Number of parks per 1,000 residents)")+
  xlab("Education (%)")+
  theme_light()+
  theme(axis.text = element_text(size=13))+
  theme(axis.title = element_text(size=13))
p6



combined_plot_grob <- arrangeGrob(p1, p2, p3, p4, p5, p6, ncol = 3)
ggsave("pdp_parks_per_capita.png", combined_plot_grob, width = 17 ,height = 10)




# park area # per capita: 28.3%, 1259 #####
becl1ux_rf = becl1uxt %>% #  
  filter(!cz==4 ) %>%
  mutate(BECTPOP2017L1UX=BECTPOP2017L1UX/1000)%>%
  select(BECPOPDENS2017L1UX, BECTPOP2017L1UX, topogroup, coast, BECADINTDENSL1UX,
         BECPARKTAREAPERCAPL1UX, CNSUNEMPL1AD, CNSMINHSL1AD, SECGDPGPPC, cz_trop, cz_temp, cz_arid)

id <- match(colnames(becl1ux_rf), varlabels$pred)
colnames(becl1ux_rf) <- varlabels$descr.label[id]

forest <- randomForest(Park_area_per_capita ~ ., data = becl1ux_rf, localImp = TRUE, mtry=3, ntree=500)
forest

sqrt(forest$mse[length(forest$mse)])


# partial dependence plots
p1=ggplot(data=partial(forest, pred.var = "Population"), aes(x=Population, y=yhat)) +
  #geom_line()+
  geom_smooth(method="auto", se=F, fullrange=T)+
  geom_rug(aes(Population, y = NULL), alpha = 0.5, data = becl1ux_rf)+
  ylab("Park area per capita\n (m2/1,000 residents)")+
  xlab("Population (thousands)")+
  theme_light()+
  theme(axis.text = element_text(size=13))+
  theme(axis.title = element_text(size=13))
p1


p2=ggplot(data=partial(forest, pred.var = "Pop_density"), aes(x=Pop_density, y=yhat)) +
  #geom_line()+
  geom_smooth(method="auto", se=F, fullrange=T)+
  geom_rug(aes(Pop_density, y = NULL), alpha = 0.5, data = becl1ux_rf)+
  ylab("Park area per capita\n (m2/1,000 residents)")+
  xlab("Population density (per sq. km)")+
  theme_light()+
  theme(axis.text = element_text(size=13))+
  theme(axis.title = element_text(size=13))
p2


p3=ggplot(data=partial(forest, pred.var = "Intersection_density"), aes(x=Intersection_density, y=yhat)) +
  #geom_line()+
  geom_smooth(method="auto", se=F, fullrange=T)+
  geom_rug(aes(Intersection_density, y = NULL), alpha = 0.5, data = becl1ux_rf)+
  ylab("Park area per capita\n (m2/1,000 residents)")+
  xlab("Intersection density (per sq. km)")+
  theme_light()+
  theme(axis.text = element_text(size=13))+
  theme(axis.title = element_text(size=13))
p3


p4=ggplot(data=partial(forest, pred.var = "GDP_per_capita"), aes(x=GDP_per_capita, y=yhat)) +
  #geom_line()+
  geom_smooth(method="auto", se=F, fullrange=T)+
  geom_rug(aes(GDP_per_capita, y = NULL), alpha = 0.5, data = becl1ux_rf)+
  ylab("Park area per capita\n (m2/1,000 residents)")+
  xlab("GDP per capita (2011 International US$)")+
  theme_light()+
  theme(axis.text = element_text(size=13))+
  theme(axis.title = element_text(size=13))
p4


p5=ggplot(data=partial(forest, pred.var = "Unemployment"), aes(x=Unemployment, y=yhat)) +
  #geom_line()+
  geom_smooth(method="auto", se=F, fullrange=T)+
  geom_rug(aes(Unemployment, y = NULL), alpha = 0.5, data = becl1ux_rf)+
  ylab("Park area per capita\n (m2/1,000 residents)")+
  xlab("Unemployment (%)")+
  theme_light()+
  theme(axis.text = element_text(size=13))+
  theme(axis.title = element_text(size=13))
p5


p6=ggplot(data=partial(forest, pred.var = "Education"), aes(x=Education, y=yhat)) +
  #geom_line()+
  geom_smooth(method="auto", se=F, fullrange=T)+
  geom_rug(aes(Education, y = NULL), alpha = 0.5, data = becl1ux_rf)+
  ylab("Park area per capita\n (m2/1,000 residents)")+
  xlab("Education (%)")+
  theme_light()+
  theme(axis.text = element_text(size=13))+
  theme(axis.title = element_text(size=13))
p6



combined_plot_grob <- arrangeGrob(p1, p2, p3, p4, p5, p6, ncol = 3)
ggsave("pdp_parkarea_per_capita.png", combined_plot_grob, width = 17 ,height = 10)

# variable importance plot

p5=vip(forest)

p5_parkarea_percapita=p5 +
  ggtitle("Park area per capita")+
  theme_light()+
  theme(axis.text  = element_text(color="black", size=10))+
  ylab("Relative variable importance (%)")



# combine variables importance plots across the UGS metrics


combined_plot_grob <- arrangeGrob(p, p2_pctgs,
                                  p3_gspercapita,
                                  p4_parkspercapita,
                                  p5_parkarea_percapita, ncol = 3)
ggsave("var_importance_rf_all.png", combined_plot_grob, width = 13 ,height = 7)







# patch density: 13.9%, 16.9
becl1ux_rf = becl1uxt %>% #  
  filter(!cz==4 ) %>%
  select(BECPOPDENS2017L1UX, BECTPOP2017L1UX, topogroup, coast, BECADINTDENSL1UX,
         BECGSPTCHDENSL1UX, CNSUNEMPL1AD, CNSMINHSL1AD, SECGDPGPPC, cz_trop, cz_temp, cz_arid)

forest <- randomForest(BECGSPTCHDENSL1UX ~ ., data = becl1ux_rf, localImp = TRUE, mtry=3, ntree=500)
forest

sqrt(forest$mse[length(forest$mse)])



# clump: 8.36, 0.0464
becl1ux_rf = becl1uxt %>% #  
  filter(!cz==4 ) %>%
  select(BECPOPDENS2017L1UX, BECTPOP2017L1UX, topogroup, coast, BECADINTDENSL1UX,
         BECGSCLUMPL1UX, CNSUNEMPL1AD, CNSMINHSL1AD, SECGDPGPPC, cz_trop, cz_temp, cz_arid)

forest <- randomForest(BECGSCLUMPL1UX ~ ., data = becl1ux_rf, localImp = TRUE, mtry=3, ntree=500)
forest

sqrt(forest$mse[length(forest$mse)])




# MNNN: 26%, 4.75
becl1ux_rf = becl1uxt %>% #  
  filter(!cz==4 ) %>%
  select(BECPOPDENS2017L1UX, BECTPOP2017L1UX, topogroup, coast, BECADINTDENSL1UX,
         BECGSMNNNGHL1UX, CNSUNEMPL1AD, CNSMINHSL1AD, SECGDPGPPC, cz_trop, cz_temp, cz_arid)

forest <- randomForest(BECGSMNNNGHL1UX ~ ., data = becl1ux_rf, localImp = TRUE, mtry=3, ntree=500)
forest

sqrt(forest$mse[length(forest$mse)])

