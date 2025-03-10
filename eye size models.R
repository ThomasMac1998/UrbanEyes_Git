#Load required packages. 
library(ggplot2)
library(dplyr)
library(lme4)

#Inspect dataframe. 
Final_Dataset
str(Final_Dataset)

#A few variables are loaded as characters. 
#Set characters to factors. 
Final_Dataset$site <- as.factor(Final_Dataset$site)
Final_Dataset$habitat <- as.factor(Final_Dataset$habitat)
Final_Dataset$picture_nr <- as.factor(Final_Dataset$picture_nr)
Final_Dataset$species <- as.factor(Final_Dataset$species)
Final_Dataset$bird_ring_number <- as.factor(Final_Dataset$bird_ring_number)
Final_Dataset$Observer <- as.factor(Final_Dataset$Observer)

#Check if all characters are set to factors. 
str(Final_Dataset)

#Model Selection using 'performance'. 
#Load the performance package. 
library(performance)

#Load the first model. 
model1 <- lmer(mean_diameter ~ habitat + species + scaled_mass_index + 
                 habitat*species + scaled_mass_index*species + scaled_mass_index*habitat + 
                 (1|Batch) + (1|site), 
               data = Final_Dataset, REML = F) 

drop1(model1, test = "Chisq")
summary(model1)

## interaction of habitat and SMI non-significant so removed from final model

#Load second model with dropped interaction habitat*SMI. 
model2 <- lmer(mean_diameter ~ habitat + species + scaled_mass_index + 
                 habitat*species + scaled_mass_index*species + (1|Batch) + (1|site), 
               data = Final_Dataset, REML = F) 
drop1(model2, test = "Chisq")
summary(model2)

## lrt between models

anova(model1,model2, test = "LRT")

hist(residuals(model2), breaks = 20, 
     main = "Histogram of Eye Size Final Model Residuals",
     xlab = "Residuals(final model: Eye Size)")
qqnorm(residuals(model2), main = "Normal Q-Q plot: Eye Size")
qqline(residuals(model2))

shapiro.test(residuals(model2))

# use this anova function to add explanatory variables to the final model (model1)
# and can see their statistical significance

## In the output for the final model (model 1) there is no distinct output for 
## habitat and species as there is a significance in the interaction so therefore
## both variables are significant 

# 3 KEY BITS OF INFORMATION

# summary function to see direction in response (i.e. corrections from baseline coefficient)
# I need the p-value of the terms included in the final model (model2) from drop 1 command 
# statistical significance of the p-values not in the final models (from model1 - i.e. SMI p-value = 0.2642).

###
#Conduct additional model checks with check_model() and model_performance(). 
###

#Load first model. 
model1 <- lmer(mean_diameter ~ habitat + species + scaled_mass_index + 
                 habitat*species + scaled_mass_index*species + scaled_mass_index*habitat + 
                 (1|Batch) + (1|site), 
               data = Final_Dataset, REML = F) 

#Calculating R2 for mixed models. 
r2_nakagawa(model1)

#Computing R2 without random effects. 
model1R <- glm(mean_diameter ~ habitat + species + scaled_mass_index + 
                 habitat*species + scaled_mass_index*species + scaled_mass_index*habitat, 
               data = Final_Dataset) 
#calculate R^2 with the r2() function. 
r2(model1R)

#Check model and calculate AIC, AICc, BIC, etc. 
check_model(model1)
model_performance(model1)

#Load second model. 
model2 <- lmer(mean_diameter ~ habitat + species + scaled_mass_index + 
                 habitat*species + scaled_mass_index*species + (1|Batch) + (1|site), 
               data = Final_Dataset, REML = F) 
check_model(model2)
model_performance(model2)

#Computing R2 without random effects. 
model2R <- glm(mean_diameter ~ habitat + species + scaled_mass_index + 
                 habitat*species + scaled_mass_index*species, 
               data = Final_Dataset) 

#calculate R^2 with the r2() function. 
r2(model2R)
#Comparing model1 and model2. 
test_performance(model1, model2)

#Visualization of model performance shows that both models are equally good. 
plot(compare_performance(model1, model2, rank = TRUE))

#Overall,a LRT tells us that 
#model2 is not a statistically significantly better fit than model1, but based on BIC, AICc, and AIC,
#Model2 is better. 


#Re-running model with residuals extracted from regression. 

#Detach the performance package first, since it interferes with base R commands (e.g., plot()). 
detach("package:performance")    

#Run a regression with mean_diameter and scaled_mass_index to correct for body size. 
resid_model <- glm(mean_diameter ~ scaled_mass_index, data = Final_Dataset)
#Attach the residuals of this regression to the data frame. 
Final_Dataset$residual_eye_size <- resid_model$residuals
#Check normality of residuals by eyeballing a frequency histogram. 
hist(Final_Dataset$residual_eye_size)

model3 <- lmer(residual_eye_size ~ habitat + species + 
                 habitat*species + 
                 (1|Batch) + (1|site), 
               data = Final_Dataset, REML = F)
drop1(model3, test = "Chisq") #Since SMI was already controlled for by using residual eye size, 
#it was removed from the model. This just leaves habitat and species as variables, as well as their interaction. 

#Using performance package for diagnostics of new models. 
library(performance)
check_model(model3)
model_performance(model3)


#Plotting data. 

library(RColorBrewer)
library(ggstance)
library(PupillometryR)

#Raincloud plots of mean visible diamter.  
p1 <- ggplot(Final_Dataset) +
  aes(x = species,
      y = mean_diameter,
      fill = habitat) + #split plot by habitat and add a flat violin plot. 
  geom_flat_violin(position = position_nudge(x = .2), #nudge the violin plot to the side to make room for the box/scatter plots.
                   alpha = .6, ) +
  scale_fill_manual(NULL, #Set custom colours for the plot. 
                    values=c("orange","lightslategrey"),
                    labels = c("Urban", "Rural"))

p2 <- p1 + geom_point(aes(color = habitat), 
             position = position_jitter(width = .15), #Jitter to show the data. 
             size = 0.8, 
             alpha = 1,
             show.legend = F) 

p3 <- p2 + geom_boxplot(aes(color = habitat,), 
               width = .3, 
               outlier.shape = NA,
               alpha = 0, 
               cex = 0.7) + 
  labs(x = "Species", 
       y = "Mean Visible Eye Diameter (mm)",
       title = NULL) +
  guides(fill = guide_legend(title="Habitat")) + 
    theme(axis.text = element_text(size = 15), 
          axis.title = element_text(size = 20), 
          plot.title = element_text(size = 20)) + 
  scale_color_manual(values = c("Urban" = "orange",
                                "Rural" = "lightslategrey")) + 
  theme(legend.position = c(0.95, 0.2)) 
  
print(p3)

#Raincloud plots of residual eye size. 

r1 <- ggplot(Final_Dataset) +
  aes(x = species,
      y = residual_eye_size,
      fill = habitat) + #split plot by habitat and add a flat violin plot. 
  geom_flat_violin(position = position_nudge(x = .2), #nudge the violin plot to the side to make room for the box/scatter plots.
                   alpha = .6, ) + 
scale_fill_manual(NULL, #Set custom colours for the plot. 
                    values=c("lightslategrey","orange"),
                    labels = c("Urban", "Rural"))

r2 <- r1 + geom_point(aes(color = habitat), 
                      position = position_jitter(width = .15), #Jitter to show the data. 
                      size = 0.8, 
                      alpha = 1,
                      show.legend = F) 

r3 <- r2 + geom_boxplot(aes(color = habitat,), 
                        width = .3, 
                        outlier.shape = NA,
                        alpha = 0, 
                        cex = 0.7) + 
  labs(x = "Species", 
       y = "Residual mean visible eye diameter (mm)",
       title = NULL) +
  guides(fill = guide_legend(title="Habitat")) + 
  theme(axis.text = element_text(size = 15), 
        axis.title = element_text(size = 20), 
        plot.title = element_text(size = 20)) + 
  scale_color_manual(values = c("Urban" = "orange",
                                "Rural" = "lightslategrey")) + 
  theme(legend.position = c(0.95, 0.2)) 

print(r3)

#I removed the second part of the legend in inkscape. 

r1 <- ggplot(Final_Dataset) +
  aes(x = species,
      y = residual_eye_size,
      fill = habitat) + #split plot by habitat and add a flat violin plot. 
  geom_flat_violin(position = position_nudge(x = .2), #nudge the violin plot to the side to make room for the box/scatter plots.
                   alpha = .6, ) 

r2 <- r1 + geom_point(aes(color = habitat), 
                      position = position_jitter(width = .15), #Jitter to show the data. 
                      size = 0.8, 
                      alpha = 1,
                      show.legend = F) 

r3 <- r2 + geom_boxplot(aes(color = habitat,), 
                        width = .3, 
                        outlier.shape = NA,
                        alpha = 0, 
                        cex = 0.7) + 
  labs(x = "Species", 
       y = "Residual mean visible eye diameter (mm)",
       title = NULL) +
  guides(fill = guide_legend(title="Habitat")) + 
  theme(axis.text = element_text(size = 15), 
        axis.title = element_text(size = 20), 
        plot.title = element_text(size = 20))  + 
  theme(legend.position = c(0.95, 0.2)) 

print(r3)
