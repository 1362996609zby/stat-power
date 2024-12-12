install.packages("readxl")  # 读取Excel文件
install.packages("pwr")     # 计算功效
install.packages("dplyr")   # 数据处理
install.packages("openxlsx")
# 加载必要的包
library(readxl)  # 用于读取Excel文件
library(pwr)     # 用于计算功效值
library(dplyr)   # 用于数据操作
library(stringr) # 用于字符串操作
library(openxlsx)
# 假设你的Excel文件名为 "all.xlsx"，并且数据在第一个sheet中
input_data <- read_excel("all.xlsx")

# 查看数据结构，确保数据正确读取
head(input_data)

# 清洗数据：将分号分隔的字符串转换为数字，并计算平均值
input_data <- input_data %>%
  mutate(
    # 将 `difference` 和 `std_dev` 的分号分隔值转换为数字向量并计算平均值
    Difference = sapply(str_split(as.character(difference), ";"), function(x) mean(as.numeric(x), na.rm = TRUE)),
    SD = sapply(str_split(as.character(std_dev), ";"), function(x) mean(as.numeric(x), na.rm = TRUE))
  )

# 查看清洗后的数据
head(input_data)

# 计算每个蛋白的效应大小（效应大小 = 差值 / 标准差）
input_data <- input_data %>%
  mutate(
    EffectSize = Difference / SD
  )

# 手动选择检验方法（默认为 "two.sample" 双样本检验）
# 你可以选择 "one.sample"（单样本检验），"paired"（配对样本检验），"two.sample"（双样本检验）
test_type <- "two.sample"  # 例如可以改成 "paired" 进行配对样本检验

# 计算功效值并添加到新的列
input_data <- input_data %>%
  rowwise() %>%
  mutate(
    Power = pwr.t.test(n = sample_size, d = EffectSize, sig.level = 0.05, type = test_type)$power
  ) %>%
  ungroup()

# 查看结果
print(input_data)

# 如果需要，将结果保存为新的Excel文件
write.xlsx(input_data, "all-protein_data_with_power.xlsx")
