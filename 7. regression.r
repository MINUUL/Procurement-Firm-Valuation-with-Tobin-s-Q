# TobinsQ에 대한 이상치 제거를 한 데이터 vs 원본 데이터
library(plm)
library(dplyr)
library(stargazer)
library(stringr)

getwd()
setwd("C:/Users/0630r/inha2/Research/Dart/Dart_file/real_code/")
data <- read.csv("total_df1.csv")
data <- data[, -1]
View(data)

sum(is.na(data))
sum(is.null(data))
for (i in 1:length(data)) {
  inf_values <- sum(is.infinite(data[[i]]))
  nan_values <- sum(is.nan(data[[i]]))
  print(paste("Inf values:", inf_values, "NaN values:", nan_values))
}
summary(data)
names(data)
data <- data[!duplicated(index(data)), ]
panel_data <- pdata.frame(data, index = c("회사명", "시기"))
View(panel_data

f <- list()
x <- c("매출", "영업이익", "총당기순이익", "R.D", "업종분류코드", "시장", "종업원수")
f0 <- log1p(TobinsQ) ~ 입찰여부 
f[[1]] <- f0
f[[2]] <- update(f0, str_replace(" ~ log(X) + .", "X", x[1]))
f
f[[3]] <- update(f[[2]], str_replace(" ~ log(X) + .", "X", x[2]))
f[[4]] <- update(f[[3]], str_replace(" ~ log(X) + .", "X", x[3]))
f
f[[5]] <- update(f[[4]], str_replace(" ~ log(X) + .", "X", x[4]))
f[[6]] <- update(f[[5]], str_replace(" ~ I(X) + .", "X", x[5]))
f[[7]] <- update(f[[6]], str_replace(" ~ I(X) + .", "X", x[6]))
f[[8]] <- update(f[[7]], str_replace(" ~ log(X) + .", "X", x[7]))
f

# pdata.frame에서 중복된 (id-time) 쌍 확인
dup_pairs <- table(index(data), useNA = "ifany")
# 중복된 (id-time) 쌍 출력
print(dup_pairs)

# 중복된 (id-time) 쌍 제거
data <- data[!duplicated(index(data)), ]

res <- list()

## (-)값인 변수( 예를 들어 매출, 영업이익 등)에 log를 씌우면 nan이 되어 버림
## 모르겠다. 포기
for (i in 1:length(f)) {
    res[[i]] <- plm(f[[i]], index = c("회사명", "시기"),
                    data = panel_data, model = "within", effect = "twoways")
}

res
summary(res)
stargazer(res, type = 'text', title = '표 4. 회귀분석 비교 결과(신뢰구간 포함)', ci = TRUE, ci.level = 0.95)
# stargazer(res, out = "outs/Table-hetero.html", 
#     type = "text", digits=2, df=FALSE, single.row=TRUE, omit.stat="rsq", omit.table.layout="n") # l=dep.var.caption, n=notes)

## mod1
mod1 <- plm("TobinsQ ~ 입찰여부", 
            index = c("회사명", "연도", "분기"),
           data = data, 
           model = "within", 
           effect = "twoways")

mod1

##mod2
mod2 <- plm(f[[1]], 
            index = c("회사명", "연도", "분기"),
           data = data, 
           model = "within", 
           effect = "twoways")
mod2

##mod3
mod3 <- plm(f[[2]], 
            index = c("회사명", "연도", "분기"),
           data = data, 
           model = "within", 
           effect = "twoways")
mod3

##mod4
mod4 <- plm(f[[3]], index = c("회사명", "연도", "분기"),
           data = data, 
           model = "within", 
           effect = "twoways")
mod4

##mod5
mod5 <- plm(f[[5]], index = c("회사명", "연도", "분기"),
           data = data1, 
           model = "within", 
           effect = "twoways")
mod5

##mod6
mod6 <- plm(f[[6]], index = c("corp_nm", "year"),
           data = data1[data1$D_bid == "True",], model = "within", effect = "twoways")
mod6
data1[,data1$D_bid == TRUE]
class(data1$D_bid)
head(data1)
head(data1[data1$D_bid == "True",])
##stargazer
stargazer(mod6, type = 'text', title = '표 4. 회귀분석 비교 결과(신뢰구간 포함)', out = '5.txt', ci = TRUE, ci.level = 0.95)
head(data1)
class(data1$employee)
colnames(data1)
data1$employee2 <- data1$employee 
head(data1)
## mod6를 진행할 때 True인 애들만 데이터로 활용하여 plm을 돌릴 때 비교 년도가 있어서 fixed effect model를 돌릴 수 있다. 즉 D_bid == True인 애들을 groupby로 진행하여 비교 년도가 있는 기업만 활용하는 것이다.

## s