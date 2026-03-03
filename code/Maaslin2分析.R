
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
otu <- read.csv("C:/Users/Administrator/Desktop/1/sp.csv", row.names = 1, check.names = FALSE)
metadata_metabolite <-read.csv("C:/Users/Administrator/Desktop/1/DXW.csv", check.names = FALSE)
row.names(metadata_metabolite) <- metadata_metabolite[[1]]
metadata_index <- read.csv("C:/Users/Administrator/Desktop/1/指标.csv", row.names = 1, check.names = FALSE) 
row.names(metadata_index) <- metadata_index[[1]]
metadata_all <- cbind(metadata_metabolite[rownames(metadata_index), -1], metadata_index[, -1]metadata_all <- as.data.frame(metadata_all)
metadata_all <- metadata_all[match(rownames(otu), rownames(metadata_all)), , drop = FALSE]
fit_data <- Maaslin2( 
  input_data = otu, 
  input_metadata = metadata_all,
  output = "maaslin2_results",
  normalization = "TSS",
  transform = "LOG",
  min_prevalence = 0.1,
  analysis_method = "LM", 
  max_significance = 0.05)   
                    cat("分析完成！结果存储在当前工作目录下的 'maaslin2_results' 文件夹。\n")
                      cat("======= 诊断开始 =======\n")
                      cat(sprintf("目标代谢物数量: %d\n", length(target_metabolites)))
                      cat(sprintf("代谢物列表: %s\n", paste(target_metabolites, collapse = ", ")))
                      cat(sprintf("otu_clean维度: %dx%d\n", nrow(otu_clean), ncol(otu_clean)))
                      cat(sprintf("met_clean维度: %dx%d\n", nrow(met_clean), ncol(met_clean)))
                      cat(sprintf("干扰因素变量: %s\n", confounder_variable))
                      cat(sprintf("ind_clean中是否存在: %s\n", 
                                  ifelse(confounder_variable %in% colnames(ind_clean), "✓", "✗")))
                      
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
                      
                      
                        
                      
                      
                      
                      
                      
                      

