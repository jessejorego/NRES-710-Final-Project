##final project

waterrights <- read.csv("Final_Project.csv")  ##read in data

hay <- waterrights$HAY...HAYLAGE...ACRES.HARVESTED   ##rename variables
corn <- waterrights$CORN..GRAIN...ACRES.HARVESTED
vegetables <- as.numeric(waterrights$VEGETABLE.TOTALS..IN.THE.OPEN...ACRES.HARVESTED)
totalacres <- waterrights$AG.LAND..CROPLAND..HARVESTED...ACRES

percenttot_hay <- (hay/totalacres)   ##calculate % of total acres that is hay, corn, or veg to create response variable
percenttot_corn <- (corn/totalacres)
percenttot_veg <- (vegetables/totalacres)

#rename variables
percentsw <- (waterrights$X.sw/100)
medianpriority <- waterrights$Median_Priority

#create data frame to run pairs function
df <- data.frame(percentsw, medianpriority, percenttot_hay)
pairs(df)

#run model for hay
logtotal_hay <- qlogis(percenttot_hay) #log transform response

model_loghay <- lm(logtotal_hay ~ percentsw + medianpriority)
summary(model_loghay)
plot(model_loghay)

#run model for corn
logtotal_corn <- qlogis(percenttot_corn) #log transform response
model_corn <- lm(logtotal_corn ~ percentsw + medianpriority)
summary(model_corn)
plot(model_corn)

#run model for veg
logtotal_veg <- qlogis(percenttot_veg) #log transform response
model_veg <- lm(logtotal_veg ~ percentsw + medianpriority)
summary(model_veg)
plot(model_veg)

#plot the effects for the hay model
library(effects)
percentsw_plot_h <- plot(allEffects(model_loghay), selection=1, ylab="Log of % Acres Harvest (Hay)", main="Percent of Surface Water Effect Plot (Hay)", xlab="Percent Surface Water")
medianpriority_plot_h <- plot(allEffects(model_loghay), selection=2, ylab="Log of % Acres Harvest (Hay)", main="Median Priority Effect Plot (Hay)", xlab="Median Priority")

library(gridExtra)
hay_plots <- grid.arrange(percentsw_plot_h, medianpriority_plot_h)

#plot the effects for the corn model
percentsw_plot_c <- plot(allEffects(model_corn), selection=1, ylab="Log of % Acres Harvest (Corn)", main="Percent of Surface Water Effect Plot (Corn)", xlab="Percent Surface Water")
medianpriority_plot_c <- plot(allEffects(model_corn), selection=2, ylab="Log of % Acres Harvest (Corn)", main="Median Priority Effect Plot (Corn)", xlab="Median Priority")
corn_plots <- grid.arrange(percentsw_plot_c, medianpriority_plot_c)

#plot the effects for the vegetable model
percentsw_plot_v <- plot(allEffects(model_veg), selection=1, ylab="Log % of Acres (Vegetables)", main="Percent of Surface Water Effect Plot (Vegetables)", xlab="Percent Surface Water")
medianpriority_plot_v <- plot(allEffects(model_veg), selection=2, ylab="Log % Acres (Vegetables)", main="Median Priority Effect Plot (Vegetables)", xlab="Median Priority")
veg_plots <- grid.arrange(percentsw_plot_v, medianpriority_plot_v)

#run vif to make sure multicollinearity is not an issue
library(car)
car::vif(model_loghay)
car::vif(model_corn)
car::vif(model_veg)

#cite R packages
citation()
citation("effects")
citation("gridExtra")
citation("car")
