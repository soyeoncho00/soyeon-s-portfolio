###################2번
rm(list = ls())
library(arules)
data(Income)
data <- Income
summary(data)

rules.all <- apriori(data)
summary(rules.all)
options(digits=3)
inspect(rules.all[1:20])
# 규칙의 우변(rhs)가 Income과 관계된 규칙
# 설정값 변경: 최소부분집합크기=2, 최소지지도=0.2, 최소신뢰도=0.8
rules <- apriori(data, control = list(verbose=F),
                 parameter = list(minlen=2, supp=0.2, conf=0.5),
                 appearance = list(rhs=c("income=$0-$40,000", "income=$40,000+"),
                                   default="lhs"))
#inspect(rules)


"""
rules <- apriori(data, control = list(verbose=F),
                 parameter = list(minlen=2, supp=0.2, conf=0.8),
                 appearance = list(rhs=c("income=$0-$40,000", "income=$40,000+"),
                                   default="lhs"))

rules



rules <- apriori(data, control = list(verbose=F),
                 parameter = list(minlen=2, supp=0.07, conf=0.8),
                 appearance = list(rhs=c("income=$0-$40,000", "income=$40,000+"),
                                   default="lhs"))
rules
inspect(rules)
"""

# 향상도(lift) 기준으로 정렬
rules.sorted <- sort(rules, by="lift")
# 규칙 확인
inspect(rules.sorted[1:20])


# 중복되는 규칙 찾기
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- F
redundant <- colSums(subset.matrix, na.rm = T) >= 1
which(redundant)

# 중복되는 규칙 삭제
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned[1:20])

## 연관규칙 시각화
#install.packages('arulesViz')
library(arulesViz)
#plot(rules.all) # 디폴트 옵션: measure=c("support", "confidence"), shading="lift"
#plot(rules.all, shading="order")
#plot(rules.all, method="grouped")



plot(rules.sorted) 
plot(rules.sorted, shading="order")

plot(rules.sorted, method="grouped")

plot(rules.sorted, method="graph")


# 평행좌표그림
plot(rules.sorted, method="paracoord", control=list(reorder=TRUE))



