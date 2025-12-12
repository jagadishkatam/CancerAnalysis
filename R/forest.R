
# Load necessary libraries
library(broom)

load('./data/cancer.RData')


str(cancer)

cancer <- cancer[cancer$diagnosis!='Overall',]

table(cancer$gender,cancer$smoking, cancer$diagnosis) %>% as.data.frame() %>% 
  rename_all(tolower) %>% 
  mutate(var2=(ifelse(var2=='Smoker','evt','n')),
         new=paste0(var2,'_', str_replace(var3,'\\s','_'))
         ) %>% 
  pivot_wider(id_cols = var1, names_from = new, values_from = freq)




# Set reference levels for factors
cancer_logit <- cancer %>%
  mutate(
    gender = relevel(as.factor(gender), ref = "Male"),
    smoking = relevel(as.factor(smoking), ref = "Smoker"),
    diagnosis = relevel(as.factor(diagnosis), ref = "Cancer"),
    geneticrisk = relevel(as.factor(geneticrisk), ref = "0"),
  )

# Fit the logistic model
reg <- glm(diagnosis == "Cancer" ~ gender + smoking + geneticrisk, 
             data = cancer_logit, 
             family = binomial()
             )


# Extract parameter estimates
parameter_estimates <- tidy(reg, conf.int = TRUE, conf.level = 0.95)

# Calculate odds ratios
odds_ratios <- parameter_estimates %>%
  mutate(Odds_Ratio = exp(estimate))

# Display results
print(parameter_estimates)
print(odds_ratios)


library(forestmodel)
forest_model(model = reg, exponentiate = F,
             format_options = forest_model_format_options(
               colour = "darkblue",
               color = NULL,
               shape = 15,
               text_size = 3,
               point_size = 2,
               banded = TRUE
             ))


