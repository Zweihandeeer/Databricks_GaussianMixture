# Databricks notebook source
# PASO 1
library(readr)  
library(dplyr)  
url <- "https://archive.ics.uci.edu/static/public/14/data.csv"  
df <-  
  read_delim(url, delim = ",") %>%  
  dplyr::select(-Class)  

df <- dplyr::mutate(df, id = as.integer(rownames(df)))

# COMMAND ----------

df <- df %>% na.omit()

# COMMAND ----------

df

# COMMAND ----------

# PASO 2
library(SparkR)
ddf <- createDataFrame(df)

# COMMAND ----------


# PASO 3
seed <- 12345  
training_ddf <- sample(ddf, withReplacement=FALSE, fraction=0.7)  
test_ddf <- except(ddf, training_ddf)

# COMMAND ----------

# PASO 4: Ajustar el modelo de mezcla gaussiana
model <- spark.gaussianMixture(training_ddf, age ~ ., k = 3)

# COMMAND ----------

# PASO 5
summary(model)


# COMMAND ----------

# PASO 6: Predecir utilizando el modelo de mezcla gaussiana
predictions <- predict(model, test_ddf)  
prediction_df <- collect(select(predictions, "id", "prediction"))

# COMMAND ----------

predictions

# COMMAND ----------

# PASO 7: Comparar valores reales y predicciones
actual_vs_predicted <-  
  dplyr::inner_join(df, prediction_df, "id") %>%  
  dplyr::select(id, actual = "deg-malig", predicted = prediction)

# COMMAND ----------

mean(actual_vs_predicted$actual == actual_vs_predicted$predicted)

# COMMAND ----------

# PASO 8: Matriz de confusión
table(actual_vs_predicted$actual, actual_vs_predicted$predicted)
