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

install.packages("performance")

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

#Plotting data. 

ggplot(Final_Dataset, aes(x=species, y=mean_diameter, color = species)) + geom_point(position = "jitter") + geom_boxplot(alpha = 0)









