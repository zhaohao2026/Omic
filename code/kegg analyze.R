# 1. 安装和加载必要的R包
# 如果您尚未安装这些包，请先运行以下代码进行安装
# install.packages("randomForest")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("readr")
# install.packages("caret") # 用于数据预处理和模型评估，可选
# install.packages("janitor") # 用于清理列名

library(randomForest)
library(dplyr)
library(tidyr)
library(readr)
library(caret) # 可选，用于更全面的模型评估
library(janitor) # 添加 janitor 包用于清理列名

# 2. 读取数据
# 假设您的 genus.csv 文件在当前工作目录下
# 如果不在，请替换为您的文件路径
genus_data <- read_csv("C:/Users/Administrator/Desktop/1/genus.csv")

# 3. 数据预处理
# 将 "Genus" 列设置为行名（可选，但对于一些操作更方便）
# 或者在后续操作中直接使用Genus列作为标识符
# 为了随机森林，我们需要将Genus列转换为因子，并将其作为特征之一，或者将其移出特征矩阵
# 在这里，我们将其作为标识符，并准备数据用于随机森林

# 转换数据格式：将宽格式转换为长格式，以便处理
# 然后再转换为随机森林所需的格式
genus_long <- genus_data %>%
  pivot_longer(cols = starts_with("F0-") | starts_with("F15-") | starts_with("R5-") | starts_with("R47-"),
               names_to = "SampleID",
               values_to = "Abundance")

# 创建一个示例分组变量
# 您需要根据您的实际情况定义这个 'group' 变量
# 例如，如果F0是对照组，F15是处理组1，R5是处理组2，R47是处理组3
genus_long <- genus_long %>%
  mutate(Group = case_when(
    grepl("^F0", SampleID) ~ "Group_F0",
    grepl("^F15", SampleID) ~ "Group_F15",
    grepl("^R5", SampleID) ~ "Group_R5",
    grepl("^R47", SampleID) ~ "Group_R47",
    TRUE ~ "Other_Group" # 处理其他情况，如果存在
  ))

# 将数据重新转换为随机森林所需的宽格式
# 每行是一个样本，每列是一个菌，最后一列是分组信息
genus_wide_for_rf <- genus_long %>%
  pivot_wider(id_cols = SampleID,
              names_from = Genus,
              values_from = Abundance)

# 合并分组信息
group_info <- genus_long %>%
  distinct(SampleID, Group)

genus_wide_for_rf <- left_join(genus_wide_for_rf, group_info, by = "SampleID")

# 确保 Group 是一个因子变量
genus_wide_for_rf$Group <- as.factor(genus_wide_for_rf$Group)

# 移除 SampleID 列，因为它不是特征
rf_data <- genus_wide_for_rf %>%
  select(-SampleID)

# **关键修改：清理列名，将不合法的字符替换为R可接受的字符**
# 使用 janitor::clean_names() 可以自动化这个过程，非常方便
rf_data <- rf_data %>%
  clean_names()

# 确保没有NA值，如果存在，需要进行处理（例如，填充0或删除）
# 这里我们简单地填充0，但您可能需要更复杂的策略
rf_data[is.na(rf_data)] <- 0

# 4. 运行随机森林模型
# response_variable ~ . 表示 Group 是响应变量，所有其他列都是预测变量
# ntree: 树的数量，通常设置在500-2000之间
# importance: 设置为 TRUE 以计算特征重要性
set.seed(123) # 为了结果可重现性
rf_model <- randomForest(group ~ ., data = rf_data, ntree = 1000, importance = TRUE)
# 注意：这里响应变量 `Group` 在 clean_names() 后会变成小写 `group`

# 5. 获取特征重要性
importance_measures <- importance(rf_model)

# 随机森林通常提供两种重要性度量：
# - MeanDecreaseAccuracy: 通过随机置换变量后OOB错误的增加来衡量
# - MeanDecreaseGini: 通过变量在所有树中分裂节点的平均Gini系数减少来衡量
# 这里我们通常使用 MeanDecreaseGini 或 MeanDecreaseAccuracy，取决于您的偏好

# 将重要性度量转换为数据框并按重要性排序
importance_df <- as.data.frame(importance_measures)
importance_df$Genus <- rownames(importance_df)

# **修正点：使用正确的列名 'MeanDecreaseGini' 和 'MeanDecreaseAccuracy'**
# 根据 MeanDecreaseGini 排序（您也可以选择 MeanDecreaseAccuracy）
importance_df_sorted <- importance_df %>%
  arrange(desc(MeanDecreaseGini)) %>% # 使用正确的列名
  select(Genus, MeanDecreaseGini, MeanDecreaseAccuracy) # 使用正确的列名

# 6. 提取排名前50的菌
top_50_genera <- head(importance_df_sorted, 50)

print("排名前50的菌及其特征重要性 (MeanDecreaseGini):")
print(top_50_genera)

# 7. 计算 q 值 (FDR 校正后的 p 值)
# 为了进行ANOVA，我们需要使用清理后的列名
rf_data_for_anova <- rf_data

# 初始化一个数据框来存储p值和q值
p_values <- data.frame(Genus = character(), P_Value = numeric(), stringsAsFactors = FALSE)

# 对每个菌进行ANOVA
# 排除响应变量 'group'
feature_columns <- names(rf_data_for_anova)[!names(rf_data_for_anova) %in% "group"]

for (genus_name_cleaned in feature_columns) {
  formula_anova <- as.formula(paste(genus_name_cleaned, "~ group"))
  anova_result <- aov(formula_anova, data = rf_data_for_anova)
  p_value <- summary(anova_result)[[1]]$`Pr(>F)`[1]
  p_values <- rbind(p_values, data.frame(Genus = genus_name_cleaned, P_Value = p_value))
}

# 对p值进行FDR校正以获得q值
p_values$Q_Value <- p.adjust(p_values$P_Value, method = "fdr")

# 将 q 值合并到 top_50_genera 数据框中
# 注意：这里 Genus 列是清理后的菌名
top_50_genera_with_q <- left_join(top_50_genera, p_values, by = "Genus")

print("排名前50的菌及其特征重要性、P值和Q值:")
print(top_50_genera_with_q)

# 8. (可选) 绘制特征重要性图
library(ggplot2)
#
 ggplot(top_50_genera, aes(x = reorder(Genus, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 50 Genera by Mean Decrease Gini",
        x = "Genus (Cleaned Name)",
        y = "Mean Decrease Gini") +
   theme_minimal()
 
 