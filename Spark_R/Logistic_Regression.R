#For Reproducibility
RNGkind(sample.kind="Rounding")
set.seed(77)

#Prepare Dataset
height <- modelr::heights
height <- height[-c(721,973,1611,1641,1866,2828,3122,6178,6319,6794),]
height$marital <- as.numeric(recode(height$marital, "divorced"="0", "married"="1", "separated"="2", "single"="3", widowed="4"))
height$sex <- ifelse(height$sex=="male",0,1)

#Prepare Spark Cluster
library(sparklyr)
spark_install(version = '3.0.0') # run only once ever
sc <- spark_connect(master = "local", version = '3.0.0')

#Send Data to Spark Cluster
height_tbl <- copy_to(sc, height, "height")

#Test-Train Split
partitions <- height_tbl %>% sdf_random_split(train = .8, test = .2)
train_tbl <- partitions$train
test_tbl <- partitions$test

#Logisitic Regression
ml_formula1 <- formula(sex ~ income + height)
ml_formula2 <- formula(sex ~ height +education)
ml_log1 <- ml_logistic_regression(train_tbl, ml_formula1)
ml_log2 <- ml_logistic_regression(train_tbl, ml_formula2)
pred_lr1 <- ml_predict(ml_log1, test_tbl) %>% collect
pred_lr1$p1 <- unlist(pred_lr1$probability)[ c(FALSE,TRUE) ]
pred_lr2 <- ml_predict(ml_log2, test_tbl) %>% collect
pred_lr2$p1 <- unlist(pred_lr2$probability)[ c(FALSE,TRUE) ]

#Plot ROC Curve
ROC_lr1 <- roc(pred_lr1, sex, p1)
ggroc(ROC_lr1) 
ROC_lr2 <- roc(pred_lr2, sex, p1)
ggroc(ROC_lr2) 
