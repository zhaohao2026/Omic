
 install.packages("randomForest")
 install.packages("dplyr")
 install.packages("tidyr")
 install.packages("readr")
 install.packages("caret") 
 install.packages("janitor") 
library(randomForest)
library(dplyr)
library(tidyr)
library(readr)
library(caret) 
library(janitor) 
genus_data <- read_csv("C:/Users/Administrator/Desktop/1/genus.csv")
genus_long <- genus_data %>%
  pivot_longer(cols = starts_with("F0-") | starts_with("F15-") | starts_with("R5-") | starts_with("R47-"),
               names_to = "SampleID",
               values_to = "Abundance")
genus_long <- genus_long %>%
  mutate(Group = case_when(
    grepl("^F0", SampleID) ~ "Group_F0",
    grepl("^F15", SampleID) ~ "Group_F15",
    grepl("^R5", SampleID) ~ "Group_R5",
    grepl("^R47", SampleID) ~ "Group_R47",
    TRUE ~ "Other_Group" # 处理其他情况，如果存在
  ))
genus_wide_for_rf <- genus_long %>%
  pivot_wider(id_cols = SampleID,
              names_from = Genus,
              values_from = Abundance)
group_info <- genus_long %>%
  distinct(SampleID, Group)

genus_wide_for_rf <- left_join(genus_wide_for_rf, group_info, by = "SampleID")
genus_wide_for_rf$Group <- as.factor(genus_wide_for_rf$Group)
rf_data <- genus_wide_for_rf %>%
  select(-SampleID)
rf_data <- rf_data %>%
  clean_names()
rf_data[is.na(rf_data)] <- 0
set.seed(123) 
rf_model <- randomForest(group ~ ., data = rf_data, ntree = 1000, importance = TRUE)
importance_df <- as.data.frame(importance_measures)
importance_df$Genus <- rownames(importance_df)
importance_df_sorted <- importance_df %>%
  arrange(desc(MeanDecreaseGini)) %>% # 使用正确的列名
  select(Genus, MeanDecreaseGini, MeanDecreaseAccuracy) # 使用正确的列名
top_50_genera <- head(importance_df_sorted, 50)
print("排名前50的菌及其特征重要性 (MeanDecreaseGini):")
print(top_50_genera)
rf_data_for_anova <- rf_data
p_values <- data.frame(Genus = character(), P_Value = numeric(), stringsAsFactors = FALSE)
feature_columns <- names(rf_data_for_anova)[!names(rf_data_for_anova) %in% "group"]
for (genus_name_cleaned in feature_columns) {
  formula_anova <- as.formula(paste(genus_name_cleaned, "~ group"))
  anova_result <- aov(formula_anova, data = rf_data_for_anova)
  p_value <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  p_values <- rbind(p_values, data.frame(Genus = genus_name_cleaned, P_Value = p_value))
}
p_values$Q_Value <- p.adjust(p_values$P_Value, method = "fdr")
top_50_genera_with_q <- left_join(top_50_genera, p_values, by = "Genus")
print("排名前50的菌及其特征重要性、P值和Q值:")
print(top_50_genera_with_q)
library(ggplot2)
 ggplot(top_50_genera, aes(x = reorder(Genus, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 50 Genera by Mean Decrease Gini",
        x = "Genus (Cleaned Name)",
        y = "Mean Decrease Gini") +
   theme_minimal()
