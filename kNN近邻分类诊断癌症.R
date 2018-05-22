install.packages('RWeka')
library('RWeka')
library(RWeka)
 
SGSS <- read.csv('C:/Users/YuzuruLiu/Desktop/SGSSforR.csv' , stringsAsFactors = FALSE)
str(SGSS)

library(Rcmdr)
?boxplot
?dist

#读取肿瘤文件
wbcd <- read.csv('C:/Users/YuzuruLiu/Desktop/Machine-Learning-with-R-datasets-master/Machine-Learning-with-R-datasets-master/wisc_bc_data.csv', stringsAsFactors = FALSE)
str(wbcd)
wbcd <- wbcd[-1]
table(wbcd$diagnosis)

#编码为因子类型
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c('B', 'M'), labels = c('Benign', 'Malignant'))
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

#观察三个特征
summary(wbcd[c('radius_mean', 'area_mean', 'smoothness_mean')])

#进行min-max标准化
normalize <- function(x){
  return ((x - min(x)) / (max(x) - min(x)))
}

#测试是否标准化成功
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))

#将标准化应用到全部案例
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))

#测试是否应用成功
summary(wbcd_n$area_mean)

#创建训练集和测试集0
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

#训练模型
library(class)
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k = 21)

#评估模型
library(gmodels)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

#优化模型method1:z分数标准化
wbcd_z <- as.data.frame(scale(wbcd[-1]))

#测试是否变换正确
summary(wbcd_z$area_mean)

#创建训练集和测试集1
wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]
wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k = 14)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)







