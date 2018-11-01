setwd("C:/Users/Kim Seok Joon/Desktop/연습")



library(randomForest)
library(pdp)
library(ggplot2)
library(corrplot)
library(RColorBrewer)
library(caret)

data("boston")
str(boston)
data(boston)  # load the boston housing data
summary(boston)
# x <- subset(boston, select=-chas)
# y <- boston$chas

##########################
###########EDA############
##########################
## 상관분석


head(boston)
cor=subset(boston,select=-7)
head(cor)
cor=cor[(complete.cases(cor)),]

corrplot <- cor(cor)
corrplot(corrplot, order = "hclust",addCoef.col="black", method="shade")
# 서로 속성이 거의 동일한 feature 다수 존재,
# 상관계수를 기준으로 유의한 feature 종류 중 하나만 남겨두고 나머진 제거
# feature간 상관계수 0.91 이상 제거

###############################
#Feature의 class별 밀도 그래프#
###############################
## -> 1. 클래스에 대한 설명력 파악
#         - 각 클래스의 특성파악으로 분류모델 학습 시 적절한 feature를 찾기 위함.
#         - 최빈값, 왜도, 첨도, 분산, 정규성
#     2. 모델에서의 feature engineering 
#         - Feature의 분포(형태)가 분류모델에 적/합한지 판단하기 위함


# 왜도 VS 첨도
# 그럼 이 두개는 어떨 때 중요한게 쓰일까요?
#   바로 정규분포를 확인할 때 쓰입니다.

# 왜도는 좌우로 치우치는 정도를 말합니다.
# 왜도가 0이라면 좌우대칭하다고 하며
# >0이면 오른쪽으로 비대칭, <0이면 왼쪽으로 비대칭
# 하다고 말합니다.
# 
# 첨도(최빈값의 빈도)는 위로 얼마나 뾰족한지 알 수 있는것인데
# 첨도는 양(+)일 경우 더 뾰족하고
# 음(-)일 경우 덜 뾰족한 편입니다.

head(boston)
a=ggplot(boston, aes(x=cmedv, colour = chas)) + 
  geom_density(fill = NA) + 
  geom_line(stat = "density") + 
  expand_limits(y = 0) + 
  theme_minimal() +
  ggtitle("Kernel Density Curve by Car Type_overlap")
a

b=ggplot(boston, aes(x= zn, colour = chas)) + 
  geom_density(fill = NA) + 
  geom_line(stat = "density") + 
  expand_limits(y = 0) + 
  theme_minimal() +
  ggtitle("Kernel Density Curve by Car Type_overlap")
b

c=ggplot(boston, aes(x=indus, colour = chas)) + 
  geom_density(fill = NA) + 
  geom_line(stat = "density") + 
  expand_limits(y = 0) + 
  theme_minimal() +
  ggtitle("Kernel Density Curve by Car Type_overlap")
C

grid.arrange(a,b,c)
# 해당 feature들은 밀도 그래프에서 o,1 최빈값이 유사한 것을 볼 수 있지만,
# 최빈값의 빈도(첨도)에서 차이가 나는 것으로 보아 0,1 분류에 좋은 feature가 될 것으로 판단


d=ggplot(boston, aes(x=crim, colour = chas)) + 
  geom_density(fill = NA) + 
  geom_line(stat = "density") + 
  expand_limits(y = 0) + 
  theme_minimal() +
  ggtitle("Kernel Density Curve by Car Type_overlap")
d # 클래스간 밀도 그래프의 개형 차이가 극명한 경우
# 밀도 그래프의 왜도
# 밀도 그래프의 첨도
# 밀도 그래프의 분산 등을 고려하였을 때 각 클래스를 분류할 때 좋은 feature가 될 것으로 판단


# 전처리
sum(is.na(boston))

# 이상치
par(mfcol=c(4,4))

for (i in c(1:6,8:16)){
  boxplot(boston[i])}

boxplot(boston)
str(df)
head(df)


# 모델링

set.seed(1000)
intrain=createDataPartition(y=boston$chas, p =0.7, list=FALSE)
train = boston[intrain,]
test = boston[-intrain,]

rf=train(x=train[,-7], y=train$chas, data=train, method='rf', trControl=trainControl(method="cv", number=5))

# Predicting
pred <- predict(rf, test, probability = TRUE)

# Confusion Matrix
confusionMatrix(pred,test$chas)

table(pred,test$chas)

# 변수중요도
par(mfrow=c(1,1))
plot(varImp(rf))
varimp=varImp(rf)
mean(varimp$importance$Overall)

# which(varimp$importance >mean(varimp$importance$Overall)) # 중요도 평균이상만 출력

# rownames(as.data.frame(varimp$importance))[which(varimp$importance >mean(varimp$importance$Overall))]

##########################
###########PDP############
##########################
# 변수해석
# - 트리 모델에서 변수 중요성은 알 수 있지만 직접적인 상관성의 파악은 어려움.
# 이에 대한 대안적 탐색 => PD(Partial Dependence)
# 해당 feature의 값이 증가할수록 0(True Positive: 진양성) 분류 확률이 높아짐


# 단일 예측변수에 대한 PDP
# Single Variable
head(boston)

par.Petal_W <- partial(rf, pred.var = c("cmedv"), chull = TRUE)
plot.Petal_W <- autoplot(par.Petal_W, contour = TRUE)+theme_minimal()+theme_bw()
plot.Petal_W # cmedv 증가 => 0(True Positive: 진양성) 분류 확률 낮아짐

# Single Variable
par.Sepal_W  <- partial(rf, pred.var = c("crim"), chull = TRUE)
plot.Sepal_W  <- autoplot(par.Sepal_W , contour = TRUE)+theme_minimal()+theme_bw()

# Single Variable
par.Petal_L <- partial(rf, pred.var = c("zn"), chull = TRUE)
plot.Petal_L <- autoplot(par.Petal_L, contour = TRUE)+theme_minimal()+theme_bw()

# Single Variable
par.Sepal_L  <- partial(rf, pred.var = c("nox"), chull = TRUE)
plot.Sepal_L  <- autoplot(par.Sepal_L , contour = TRUE)+theme_minimal()+theme_bw()

grid.arrange(plot.Petal_W, plot.Sepal_W, plot.Petal_L, plot.Sepal_L)

# 2개 예측변수의 상호작용을 고려한 PDP
# Two Variables
par.Petal_W.Sepal_W1 <- partial(rf, pred.var = c("rad", "tax"), chull = TRUE)
plot.Petal_W.Sepal_W1 <- autoplot(par.Petal_W.Sepal_W1, contour = TRUE, 
                                 legend.title = "Partial\ndependence")

par.Petal_W.Sepal_W2 <- partial(rf, pred.var = c("ptratio", "b"), chull = TRUE)
plot.Petal_W.Sepal_W2 <- autoplot(par.Petal_W.Sepal_W2, contour = TRUE, 
                                 legend.title = "Partial\ndependence")
plot.Petal_W.Sepal_W2
# b, ptratio 증가 => 0(True Positive: 진양성) 분류 확률 높아짐

par.Petal_W.Sepal_W3 <- partial(rf, pred.var = c("b", "lstat"), chull = TRUE)
plot.Petal_W.Sepal_W3 <- autoplot(par.Petal_W.Sepal_W3, contour = TRUE, 
                                 legend.title = "Partial\ndependence")

grid.arrange(plot.Petal_W.Sepal_W1,plot.Petal_W.Sepal_W2,plot.Petal_W.Sepal_W3)


##########################
###########도움말#########
##########################

# 결측치 처리 및 대치법 
# # Missing value rate > 5% => Delete
# sum(is.na(df)) # 220779
# nrow(df[df$asile_type=="",])/28663 *100 # 63.39532%
# nrow(df[df$earthquake=="",])/28663 *100 # 23.71001%
# 
# pMiss <- function(x){sum(is.na(x))/length(x)*100}
# apply(df, 2, pMiss)
# df=subset(df,select = -c(asile_type,building_count, building_coverage_ratio,commute_dmc, commute_seongsu
#                          ,commute_yongsan, commute_chungmuro, earthquake, floor_area_ratio, floor_max, floor_min
#                          ,parking_inside,parking_outside,parking_rate,permission_date, slope))
# 
# apply(df, 1, pMiss) < 5
# df=df[apply(df, 1, pMiss) < 5,]
# 
# # Dummy variable 
# df=dummy.data.frame(df)
# 
# # Multiple imputation
# imp = mice(df, m=5, maxit = 50, seed=1234, method='cart')
# df=complete(imp)


######################################################################################
# ####전처리####
# 
# library(dummies)
# library(xgboost)
# library(mice)
# library(caret)
# library(dplyr)
# library(ggplot2)
# library(corrplot)
# library(RColorBrewer)
# library(caret)
# library(car)
# library(ROSE)
# library(rpart)
# library(MASS)
# library(e1071)
# library(glmnet)
# 
# # 예측변수 생성
# df$accident = ifelse(is.na(df$관할해경서),0,1) # 발생X:'0',발생O:'1'
# df$accident=as.factor(df$accident)
# 
# 
# # 변수 분리 및 제거
# df$year=as.factor(substr(df$일시,1,4))
# df$month=as.factor(substr(df$일시,6,7))
# df$day=as.factor(substr(df$일시,9,10))
# df$hour=substr(df$일시,12,13)
# df$hour=as.factor(gsub(":","",df$hour))
# df$일시=as.POSIXct(df$일시, format="%Y-%m-%d %H:%M")
# df[1]=NULL
# df[15:22]=NULL
# df$week=strftime(df$일시, '%u') # 요일
# 
# ######################################################################################
# ####시계열그래프####
# 
# # 연도별 사고 데이터 시계열
# 
# df1=subset(df, accident==1)
# 
# year=df1 %>%
#   group_by(year) %>%
#   summarize(count=n())
# 
# ggplot(data = year, 
#        mapping = aes(x = year, y = count, col="red",group=1)) + theme_minimal() +
#   geom_point(data = year,size=3) +
#   geom_line(data = year,size=1) + 
#   ggtitle("연도별 해양 사고 발생 추이")+theme(text = element_text(size = 20))
