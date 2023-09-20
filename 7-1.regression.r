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
# panel_data <- pdata.frame(data, index = c("회사명", "시기"))
# View(panel_data)

## 정리 r&d = r&d / 종업원수                         

f <- list()
x <- c("매출", "영업이익률", "R.D","종업원수")
f0 <- TobinsQ ~ 낙찰여부 
f[[1]] <- f0
f[[2]] <- update(f0, str_replace(" ~ log(X) + .", "X", x[1]))
f
f[[3]] <- update(f[[2]], str_replace(" ~ X + .", "X", x[2]))
f[[4]] <- update(f[[3]], str_replace(" ~ log(1+X) + .", "X", x[3]))
f[[5]] <- update(f[[4]], str_replace(" ~ X + .", "X", x[4]))
#f[[6]] <- update(f[[5]], str_replace(" ~ X + .", "X", x[5]))
#f[[7]] <- update(f[[6]], str_replace(" ~ X + .", "X", x[6]))
# f[[8]] <- update(f[[7]], str_replace(" ~ X + .", "X", x[7]))
f[[5]]
factor(data$업종분류코드)



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
                    data = data, model = "within", effect = "twoways")
}

res
res[2]
res[5]
summary(res[1])
res[7]
stargazer(res, type = 'text', title = '표 4. 회귀분석 비교 결과(신뢰구간 포함)')
# stargazer(res, out = "outs/Table-hetero.html", 
#     type = "text", digits=2, df=FALSE, single.row=TRUE, omit.stat="rsq", omit.table.layout="n") # l=dep.var.caption, n=notes)
