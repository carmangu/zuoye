# a) read in data, n=8000
X = read.csv("hw2_X.csv", header = F)
y = read.csv("hw2_y.csv", header = F)

# b) create matrix for the quadratic model, which includes all X terms(77), quadratic terms (77) 
# and interaction term (2926), in total 3080 terms.
library(gtools)
comb <- combinations(ncol(X),2)
Model = cbind(X, (X^2)/2)
for(j in 1:nrow(comb)){Model = cbind(Model,X[,comb[j,1]]*X[,comb[j,2]])}
X <- Model

# c) split train test
test_num <- seq(4,8000, by= 4); length(test_num)
train_num = setdiff(seq(1,8000, by = 1),test_num);length(train_num)

X_train <- X[train_num,]
X_test <- X[test_num,]
y_train <- y[train_num,]
y_test <- y[test_num,]

# d1) center
X_train_mean <- apply(X,2,mean);table(is.na((X_train_mean)))
X_train_c <- X_train - X_train_mean
y_train_c <- y_train - mean(y_train, na.rm=T)

# d2) scale
scale_train <- sqrt(apply(X_train_c^2,2,sum)); length(scale_train)
X_train_s <- t(t(X_train_c)/scale_train)

num <- seq(-13,9,by=0.5)
lambda = 2^num

# e1) svd
train_svd <- svd(X_train_s)

