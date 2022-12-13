# R基本的回归分析(线性回归的回归诊断：线性性 、独立性、正态性、同方差性、多重共线性、异常）
install.packages("flextable")
# install.packages("xtable")
install.packages("flextable")
install.packages("corrplot")   #安装corrplot包
install.packages("car")   #安装car包
install.packages("carData")   #安装car包
install.packages("lmtest")
install.packages('pls')
install.packages('lars')
install.packages('devtools')
install.packages('corrgram')
install.packages('psych')
install.packages("rpart")
install.packages("rpart.plot")
install.packages("neuralnet")
install.packages('tree')
install.packages('e1071')
install.packages('pROC')
install.packages('BSDA')
install.packages('caret')
install.packages('randomForest')
library(devtools)
install_github("vqv/ggbiplot")
library(corrplot)   #载入corrplot包
library(flextable)#制作表格
library(lmtest)
library(corrplot)
library(car)#检验多重共线性的包
library(lmtest)
library(carData)
a<-read.table(file="/Users/creative/Python/statistc-final-work/data/NMDS_coordinates.csv", header=TRUE, sep=",")
head(a)
b<-lm(Vel~DIST+se_rug+date_diff,data=a)
summary(b)
as_flextable(b)
fitted(b)#拟合模型预测值
residuals(b)#拟合模型的残差值
ab<-a[,c(11:17)]#只保留解释变量
corr<-cor(ab);corr
corrplot(corr,method="circle",type="full",mar=c(0,0,0,0) ,bg="black",tl.col = "blue")
vif(b)#b是线性回归的结果
XX<-cor(ab)#先做出相关系数矩阵
kappa(XX)#对相关系数矩阵做
eigen(XX)#求特征值和特征向量
aaa<-step(b)
as_flextable(aaa)
#利用ks检验正态性
ks.test(e,"pnorm")
#利用Shapiro-Wilk检验正态性
shapiro.test(e)
#通过成分残差图来判断各个自变量与因变量之间是否存在线性相关
crPlots(b)


# R主成分分析
a<-read.table(file="/Users/creative/Python/statistc-final-work/data/NMDS_coordinates.csv", header=TRUE, sep=",")
data = a[,c(11:17)]
data1=scale(data)
data.pr=princomp(data1,cor = FALSE, scores = TRUE)
summary(data.pr,loadings=TRUE)
data.pr1=prcomp(data,center = TRUE, scale. = TRUE)
data.pr1
summary(data.pr1)
screeplot(data.pr,type = "lines")
pre<-predict(data.pr)
pre

# 主成分回归、偏最小二乘回归(处理多重共线性)(pls库)
library(ggbiplot)
library(pls)
set.seed(1)
library(lars)
a<-read.table(file="/Users/creative/Python/statistc-final-work/data/NMDS_coordinates.csv", header=TRUE, sep=",")
data(diabetes)
y <- a[,c(11:17)]
x <- a[,c(18)]
fix(x)
x<-as.data.frame(x)

model_pcr1<-princomp(~.,data = x)
summary(model_pcr1,loadings = T)

# R因子分析
a <- read.table(file="/Users/creative/Python/statistc-final-work/data/NMDS_coordinates.csv", header=TRUE, sep=",")
test<-a[,c(11:17)]
R<-round(cor(test), 3); R   # 样本相关系数阵
library("corrgram")
corrgram(R,lower.panel = panel.shade,upper.panel = panel.pie,
         text.panel = panel.txt, main="correlation coefficent")
corrgram(R)
library(psych)
KMO(R)
library(psych)
fa.parallel(R, fa="both", n.obs=53, n.iter=100,
            main="Scree plots with parallel analysis")
# 极大似然法
factanal(test,factors=2,rotation="none")
# 主成分法
principal(r=R,nfactors=2,rotate="none")
# 主轴因子法
fa1<-fa(r=R, nfactors=2, fm="pa", rotate="none")
fa1
## 旋转
fa2<-fa(r=R, nfactors=2, fm="pa", rotate="varimax")
fa2
fa(r=R, nfactors=2, fm="pa", rotate="promax")
## 可视化
factor.plot(fa2,label=rownames(fa2$loadings))
fa.diagram(fa2,simple=TRUE)
## 因子得分与可视化
fa2$weight##显示因子值系数
fa2FS<-as.matrix(scale(test))%*%fa2$weight##计算因子得分
fa2FS
plot(fa2FS,main="fa Factor Scores",
     xlab="1st factor",ylab="2ed factor")
abline(h=0, v=0)
text(fa2FS, labels = , adj=1.4, cex=0.7)

# R决策树
library(rpart.plot)
a<-read.table(file="/Users/creative/Python/statistc-final-work/data/NMDS_coordinates.csv", header=TRUE, sep=",")
test <- a[,11:18]
test_formula1 <- rpart(formula =test)
test_formula1$terms
rpart.plot(test_formula1, type = 0, main = "type = 0")

# R boosting回归、bagging回归
library(tree)
a<-read.table(file="/Users/creative/Python/statistc-final-work/data/NMDS_coordinates.csv", header=TRUE, sep=",")
rt <- tree(MegUnd ~ DIST + Vel + EisArb, data = a)
summary(rt)
plot(rt)
text(rt, cex= 0.8)


# R神经网络
library(neuralnet)
a<-read.table(file="/Users/creative/Python/statistc-final-work/data/NMDS_coordinates.csv", header=TRUE, sep=",")
str(a)
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
concrete_norm<-as.data.frame(lapply(a[,c(11:18)],normalize)) #lapply()将函数运用于每一列
summary(concrete_norm$MesFra)
set.seed(12345)
index <- sample(nrow(concrete_norm),nrow(concrete_norm)*.7,replace=F)
concrete_train2 <- concrete_norm[index,]
concrete_test2 <- concrete_norm[-index,]
concrete_model <- neuralnet(PisGig ~ MesFra + StrPur + MegUnd + DerImb + PycHel + SteOsm + LamSpp, 
                            data=concrete_train2)

model_results <- compute(concrete_model,concrete_test2[1:8])
predicted_strength <- model_results$net.result
cor(predicted_strength,concrete_test2$strength)

plot(concrete_model)

# R支持向量机
library(e1071)
a<-read.table(file="/Users/creative/Python/statistc-final-work/data/NMDS_coordinates.csv", header=TRUE, sep=",")
fit <- svm(formula = MegUnd~ Vel + DIST + sd_rug + X_dist,
           data = a, 
           type = "C-classification",
           kernel = "radial", 
           scale = TRUE,
           gamma = 1,
           cost = 1)
summary(fit)

# 随机森林
library(randomForest)
library(ggplot2)
library(caret)
library(pROC)
a<-read.table(file="/Users/creative/Python/statistc-final-work/data/NMDS_coordinates.csv", header=TRUE, sep=",")
b.trainIndex <- createDataPartition(a$DIST, p=0.8, list=FALSE)
b.trainSet <- a[b.trainIndex,]
b.testSet <- a[b.trainIndex,]
set.seed(1234)
b.time.start <- Sys.time()
b.rf.train <- randomForest(as.factor(MegUnd)~.,
                               data=b.trainSet[,10:33],
                               important=TRUE,
                               na.action=na.pass,
                               norm.votes=TRUE,
                               proximity=TRUE)
b.time.end <- Sys.time()
b.time.calculation <- b.time.end - b.time.start 
b.rf.train # 查看具体数据.
importance(b.rf.train) # 查看不同特征的重要性.
MDSplot(b.rf.train, b.trainSet$MegUnd)  
plot(b.rf.train, main="data result")


# 用R作假设检验(均值检验、方差检验、非参数检 验)
a<-read.table(file="/Users/creative/Python/statistc-final-work/data/NMDS_coordinates.csv", header=TRUE, sep=",")
X<-a[,20:33]
t.test(X,alternative = "greater",mu=225)
library(BSDA)
z.test(X, y = NULL, alternative = "greater", 
       mu = 225, sigma.x = 10,conf.level = 0.95)
t.test(x=X[,1:7],y=X[8:14],alternative="two.sided",mu=0,var.equal=TRUE)

# R方差分析(单因素、双因素)
a<-read.table(file="/Users/creative/Python/statistc-final-work/data/NMDS_coordinates.csv", header=TRUE, sep=",")
a.aov <- aov(PisGig ~ MesFra + StrPur + MegUnd + DerImb + PycHel + SteOsm + LamSpp, data = a)
summary(a.aov)
# 将统计表的数据输入到命名为rats的数据框中
rats<-data.frame( Time=a[1:48,20],toxicant=gl(3,16,48,labels = c("I","II","III")),cure=gl(4,4,48,labels = c("A","B","C","D")))
op <- par(mfrow=c(1,2)) #更改图形显示的界面排版
plot(Time~toxicant + cure, data = rats)



