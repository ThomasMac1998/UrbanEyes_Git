library(ggplot2)
library(dplyr)
library(lme4)



model1 <- lmer(mean_diameter ~ habitat + species + scaled_mass_index + 
                 habitat*species + scaled_mass_index*species + scaled_mass_index*habitat + 
                 (1|Batch) + (1|site), 
               data = final_clean, REML = F) 

drop1(model1, test = "Chisq")

summary(model1)

## interaction of habitat and SMI non-significant so removed from final model

model2 <- lmer(mean_diameter ~ habitat + species + scaled_mass_index + 
                 habitat*species + scaled_mass_index*species + (1|Batch) + (1|site), 
               data = final_clean, REML = F) 
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

ggsave(path = "path",filename = "Histogram_residuals.jpeg", device = "jpeg", dpi=700)






# use this anova function to add explanatory variables to the final model (model1)
# and can see their statistical significance

## In the output for the final model (model 1) there is no distinct output for 
## habitat and species as there is a significance in the interaction so therefore
## both variables are significant 

# 3 KEY BITS OF INFORMATION

# summary function to see direction in response (i.e. corrections from baseline coefficient)
# I need the p-value of the terms included in the final model (model2) from drop 1 command 
# statistical significance of the p-values not in the final models (from model1 - i.e. SMI p-value = 0.2642).











