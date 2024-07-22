# generates CSV files from GAMs

library(mgcv)

# path to GAMs
setwd('GAMs')

# write headers
# CSVs are split by age to speed up search in website
for (age in 20:83) {
    header <- c("Measure", "Structure", "Sex", "Age", "Height", "Mean", "Variance")
    write.table(t(header), file = paste0('../data/', age, '.csv'), row.names = FALSE, col.names = FALSE, sep = ",", quote = FALSE)
}

# used to find correct model
measures = c("SAT", "VAT", "Muscle", "SMFF", "IMAT")
structures = c("", "_Abdomen_Vol", "_Oberbauch_Vol", "_Pelvis_Vol", "_Thorax_Vol", "area")
csv_structures = c("Body", "Abdomen", "Upper Abdomen", "Pelvis", "Thorax", "L3 Area") # slightly different naming for csv
sexes = c("male", "female")

for (i in seq_along(measures)) {
    for (j in seq_along(structures)) {
        for (k in seq_along(sexes)) {
            
            # exclude - vat not measured in thorax
            if (measures[i] == "VAT" && structures[j] == "_Thorax_Vol") {
                next
            }

            # construct model name from arguments
            model_name <- paste0(sexes[k], "_%s_model_")
            if (structures[j] == "area") {
                model_name <- paste0(model_name, "L3_", measures[i])
                if (measures[i] != "SMFF") {
                    model_name <- paste0(model_name, "_area")
                }
            } else {
                model_name <- paste0(model_name, measures[i], structures[j])
            }
            model_name <- paste0(model_name, ".rds")

            # fetch model
            mean_model <- readRDS(sprintf(model_name, "mean"))
            var_model <- readRDS(sprintf(model_name, "var"))

            # predict for all viable age and height combinations
            for (age in 20:83) {
                for (height in seq(1.40, 2.08, by = 0.01)) {
                    new_data <- data.frame(Age = age, Size = height)
                    
                    pred_mean <- predict(mean_model, newdata = new_data)
                    pred_var <- predict(var_model, newdata = new_data)

                    # append pred_mean and pred_var to CSV
                    row <- c(measures[i], csv_structures[j], sexes[k], age, height, pred_mean, pred_var)
                    write.table(t(row), file = paste0('../data/', age, '.csv'), append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE, quote = FALSE)
                }
            }
        }
    }
}
