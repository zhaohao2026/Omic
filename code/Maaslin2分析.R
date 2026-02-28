安装并加载必要包（只需第一次运行时安装）
if (!requireNamespace("Maaslin2", quietly = TRUE)) 
{ install.packages("BiocManager")
  BiocManager::install("Maaslin2") }
if (!requireNamespace("readxl", quietly = TRUE)) 
  install.packages("readxl")
install.packages("Maaslin2")



install.packages("devtools")
library(devtools)
devtools::install_github("biobakery/Maaslin2", dependencies = TRUE)



library(Maaslin2) 
library(readxl) 
读取数据
菌群丰度表
otu <- read.csv("C:/Users/Administrator/Desktop/1/sp.csv", row.names = 1, check.names = FALSE)

代谢物丰度（用作metadata，假设第一列为样本ID，可按需调整！）
metadata_metabolite <-read.csv("C:/Users/Administrator/Desktop/1/DXW.csv", check.names = FALSE)
row.names(metadata_metabolite) <- metadata_metabolite[[1]]
指标（独立变量，假设第一列为样本ID，可按需调整！）
metadata_index <- read.csv("C:/Users/Administrator/Desktop/1/指标.csv", row.names = 1, check.names = FALSE) 
row.names(metadata_index) <- metadata_index[[1]]

合并所有metadata
metadata_all <- cbind(metadata_metabolite[rownames(metadata_index), -1], metadata_index[, -1]metadata_all <- as.data.frame(metadata_all)

保证样本顺序和otu表一致
metadata_all <- metadata_all[match(rownames(otu), rownames(metadata_all)), , drop = FALSE]

#Maaslin2分析
fit_data <- Maaslin2( 
  input_data = otu, 
  input_metadata = metadata_all,
  output = "maaslin2_results",
  normalization = "TSS",
  transform = "LOG",
  min_prevalence = 0.1,
  analysis_method = "LM", # 线性模型
  max_significance = 0.05 # 可根据需要修改
  )
                      
          # 提示输出
                      cat("分析完成！结果存储在当前工作目录下的 'maaslin2_results' 文件夹。\n")
                      
                      
                      
                      
                      
                      # 在循环前运行这个【侦查代码】，10秒定位问题：
                      
                      cat("======= 诊断开始 =======\n")
                      cat(sprintf("目标代谢物数量: %d\n", length(target_metabolites)))
                      cat(sprintf("代谢物列表: %s\n", paste(target_metabolites, collapse = ", ")))
                      
                      # 检查数据维度
                      cat(sprintf("otu_clean维度: %dx%d\n", nrow(otu_clean), ncol(otu_clean)))
                      cat(sprintf("met_clean维度: %dx%d\n", nrow(met_clean), ncol(met_clean)))
                      
                      # 检查干扰因素是否存在
                      cat(sprintf("干扰因素变量: %s\n", confounder_variable))
                      cat(sprintf("ind_clean中是否存在: %s\n", 
                                  ifelse(confounder_variable %in% colnames(ind_clean), "✓", "✗")))
                      
                      # 测试第一个代谢物的metadata能否创建
                      test_met <- target_metabolites[1]
                      test_meta <- cbind(ind_clean[, confounder_variable, drop=FALSE], 
                                         met_clean[, test_met, drop=FALSE])
                      cat(sprintf("测试metadata创建: %s (维度: %dx%d)\n\n",
                                  ifelse(!is.null(test_meta), "✓ 成功", "✗ 失败"),
                                  nrow(test_meta), ncol(test_meta)))
                      
                      cat("======= 诊断结束 =======\n")                     
                      
                      all_results_list <- list()
                      failed_metabolites <- character(0)  # 记录失败的代谢物   
                      for (i in seq_along(target_metabolites)) {
                        met_name <- target_metabolites[i]
                        cat(sprintf("\n【%d/%d】正在分析: %s\n", i, length(target_metabolites), met_name))   
                      
                      
                        
                      
                      
                      
                      
                      
                      
