
#导入原始数据
sms_raw <- read.csv('C:/Users/YuzuruLiu/Desktop/machine-learning-data/sms_spam.csv', stringsAsFactors = FALSE)
str(sms_raw)

#将type转化为因子
sms_raw$type <- factor(sms_raw$type)
str(sms_raw$type)
table(sms_raw$type)

#安装并加载tm包
library(tm)

#将文档中错误字符重编码
sms_raw$text<-iconv(sms_raw$text,"WINDOWS-1252","UTF-8")

#创建语料库
sms_corpus <- VCorpus(VectorSource(sms_raw$text))
print(sms_corpus)

#查看短信概要
inspect(sms_corpus[1:2])
as.character(sms_corpus[[1]])
lapply(sms_corpus[1:2], as.character)

#清洗语料库
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))

#check if success
as.character(sms_corpus[[1]])
as.character(sms_corpus_clean[[1]])

#去除语料库的数字
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)

#去除停用词
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords())

#去除标点并用空格替换
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)
replacePunctuation <- function(x) {
  gsub("[[:punct:]]+", "", x)}

as.character(sms_corpus_clean[[1]])

#提取词干
library(SnowballC)
wordStem(c('learn', "learned", "learning", "learns"))

#将提取规则应用于全语料库并去除多余的空格
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)

#test if success
as.character(sms_corpus[[1]])
as.character(sms_corpus_clean[[1]])

#创建DTM稀疏矩阵
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)

#method2:从原始语料库建立DTM
sms_dtm2 <- DocumentTermMatrix(sms_corpus, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  stopwords = TRUE,
  removePunctuation = TRUE,
  stemming = TRUE
))

#compare sms_dtm with sms_dtm2
sms_dtm
sms_dtm2

#同化
stopwords = function(x) {removeWords(x, stopwords())}

#创建75%训练集和25%测试集
sms_dtm_train <- sms_dtm[1:2623, ]
sms_dtm_test <- sms_dtm[2624:3498, ]
sms_train_labels <- sms_raw[1:2623, ]$type
sms_test_labels <- sms_raw[2624:3498, ]$type

#test
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

#创建词云
library(wordcloud)
wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE)

#创建子集
spam <- subset(sms_raw, type == 'spam')
ham <- subset(sms_raw, type == 'ham')

#为频繁出现的词创建指示特征
findFreqTerms(sms_dtm_train, 5)
sms_freq_words <- findFreqTerms(sms_dtm_train, 5)
str(sms_freq_words)

#过滤DTM
sms_dtm_freq_train <- sms_dtm_train[, sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[, sms_freq_words]
convert_counts <- function(x){
  x <- ifelse(x>0, 'Yes', 'No')
}
sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2, convert_counts)

#训练模型
library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_train_labels)

#评估模型
sms_test_pred <- predict(sms_classifier, sms_test)
library(gmodels)
CrossTable(sms_test_pred, sms_test_labels, prop.chisq = FALSE, prop.t = FALSE, dnn = c('predicted', 'actual'))

#优化模型（在本例中拉普拉斯优化效果不好）
sms_classifier2 <- naiveBayes(sms_train, sms_train_labels, laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_test_labels, prop.chisq = FALSE, prop.t = FALSE, dnn = c('predicted', 'actual'))


