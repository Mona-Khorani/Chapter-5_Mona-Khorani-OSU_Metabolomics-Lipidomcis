install.packages("tidyverse")
library(tidyverse)
my_data = read.csv("Hip-135Lipids-NormData_TIA-LogTra_Ready_for_LM.csv")

my_data$FCNO <- ifelse(
  grepl("FCI", my_data$Sample, fixed = TRUE) | grepl("FCS", my_data$Sample, fixed = TRUE), "FC", "NO"
)

my_data$IS <- ifelse(
  grepl("NOI", my_data$Sample, fixed = TRUE)|grepl("FCI", my_data$Sample, fixed = TRUE), "I", "S"
)

head(my_data)

my_data <- my_data %>%
  select(Sample, FCNO, IS, everything())

library(stats)

numerical_columns <- my_data[, 4:138]

lm_models <- list()

my_data$FCNO <- as.factor(my_data$FCNO)
my_data$FCNO <- relevel(my_data$FCNO, ref = "NO")

my_data$IS <- as.factor(my_data$IS)
my_data$IS <- relevel(my_data$IS, ref = "S")



for (col_name in colnames(numerical_columns)) {
  formula <- as.formula(paste(col_name, " ~ my_data$FCNO + my_data$IS + my_data$FCNO:my_data$IS"))
  model <- lm(formula, data = my_data)
  lm_models[[col_name]] <- model
}


for (col_name in names(lm_models)) {
  cat("Summary for", col_name, ":\n")
  print(summary(lm_models[[col_name]]))
}


install.packages("broom")
library(broom)

library(dplyr)
library(tidyr)

reshaped_results <- data.frame(
  Variable = character(0),
  Term = character(0),
  Estimate = numeric(0),
  P_Value = numeric(0)
)

for (col_name in names(lm_models)) {
  model <- lm_models[[col_name]]
  
  tidy_result <- tidy(model)
  
  terms_of_interest <- c("(Intercept)", "my_data$FCNOFC", "my_data$ISI", "my_data$FCNOFC:my_data$ISI")
  
  for (term in terms_of_interest) {
    term_result <- tidy_result[tidy_result$term == term, ]
    
    if (nrow(term_result) == 0) {
      estimate <- NA
      p_value <- NA
    } else {
      estimate <- term_result$estimate
      p_value <- term_result$p.value
    }
    
    reshaped_results <- bind_rows(reshaped_results, data.frame(
      Variable = col_name,
      Term = term,
      Estimate = estimate,
      P_Value = p_value
    ))
  }
}

reshaped_results <- reshaped_results %>%
  pivot_wider(names_from = Term, values_from = c(Estimate, P_Value))

colnames(reshaped_results) <- c("Variable", 
                                "(Intercept)_Estimate", "FCNOFC_Estimate", "ISI_Estimate", "FCNOFC_ISI_Estimate",
                                "(Intercept)t_P_Value", "FCNOFC_P_Value", "ISI_P_Value", "FCNOFC_ISI_P_Value")

write.csv(reshaped_results, file = "reshaped_lm_results_Hip.csv", row.names = FALSE)
