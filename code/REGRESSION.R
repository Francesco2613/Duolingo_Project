########################################
###        DUOLINGO REGRESSION       ###
########################################

francesco <- function(data_,alpha1,alpha2){
  q_inf1 <- quantile(table(data_$user_id),alpha1)
  q_sup1 <- quantile(table(data_$user_id),1-alpha1)
  q_inf2 <- quantile(table(data_$lexeme_id),alpha2)
  q_sup2 <- quantile(table(data_$lexeme_id),1-alpha2)
  
  #names <- names(which(table(data_$user_id) >= q_inf1 & table(data_$user_id) <= q_sup1))
  #words <- names(which(table(data_$lexeme_id) >= q_inf2 & table(data_$lexeme_id) <= q_sup2))
  names <- names(which(table(data_$user_id) >= 100))
  words <- names(which(table(data_$lexeme_id) >= q_inf2))
  
  data <- data_ %>%
    filter(lexeme_id %in% words == T) %>%
    filter(user_id %in% names == T)
  return(data)
  plot(dim(data))
}
# ____________________________________________________________________________

library(tidyverse)
library(rgl)
library(mgcv)
library(modelr)
library(pROC)

setwd('/Users/Francesco/Documents/DUOLINGO_PROJECT')
load("duolingo_huge.RData")
duolingo_huge <- as.data.frame(duolingo_huge)


duolingo <- francesco(duolingo_huge,alpha1=0.15,alpha2=0.15)
nrow(duolingo)
# or ---
duolingo_filtered <- francesco(duolingo_huge,alpha1=0.05,alpha2=0)
set.seed(26091996)
duolingo <- sample_n(duolingo_filtered,100000)

# We apply a logit transf. to p_recall:
duolingo <- duolingo %>%
  mutate(p_logit=qlogis(0.998*p_recall + 0.001))

set.seed(26091996)
train <- sample(round(nrow(duolingo)*0.8))
data_train <- duolingo[train,]
data_val <- duolingo[-train,]
c(training = nrow(data_train),
  validation = nrow(data_val))

# --------------------------------------------------------------

#### LINEAR MODELS #### -------------------------

model1 <- lm(p_recall ~ p_history + delta_day, data=data_train)
#model2 <- lm(p_recall ~ history_seen + history_correct + delta_day, data=data_train)
model3 <- lm(p_recall ~ p_history + delta_day + p_history:delta_day, data=data_train)
summary(model3)

# We create a grid for prediction:
attach(data_train)
p_history.grid=seq(range(p_history)[1],range(p_history)[2],length.out = 100)
delta_day.grid=seq(range(delta_day)[1],range(delta_day)[2],length.out = 100)
grid=expand.grid(p_history.grid,delta_day.grid)
names(grid)=c('p_history','delta_day')

model <- model3
pred=predict(model,newdata=grid) 
persp3d(p_history.grid,delta_day.grid,pred,col='magenta')
points3d(p_history,delta_day,p_recall,col='black',size=5)


#### GAM #### -------------------------

                            #################
# ------------------------- #### MODEL 1 #### ----------------------------------------
                            #################

model_gam <- gam(p_recall ~ s(p_history,bs='cr') +       ############## !!!
                   s(delta_day,bs='cr') +
                   s(lexeme_length,bs='cr') +
                   learning_language_3, data=data_train)
summary(model_gam)
mae(model_gam, data_val) # 0.1744718

model_gam <- gam(p_recall ~ s(history_correct,bs='cr') + 
                   s(history_seen,bs='cr') +
                   s(delta_day,bs='cr') +
                   s(lexeme_length,bs='cr'), data=data_train)
summary(model_gam)
mae(model_gam, data_val) # 0.1734254

model_gam <- gam(p_recall ~ s(p_history,bs='cr') + 
                   s(delta_day,bs='cr') +
                   s(lexeme_length,bs='cr'), data=data_train)
summary(model_gam)
mae(model_gam, data_val) # 0.1744677

model_gam <- gam(p_recall ~ s(p_history,bs='cr') + 
                 s(delta_day,bs='cr') +
                 s(lexeme_length,bs='cr') +
                 s(p_history,delta_day,bs='tp'), data=data_train)
summary(model_gam)
mae(model_gam, data_val) # 0.1743276

model_gam <- gam(p_recall ~ s(p_history,bs='cr') + 
                   s(delta_day,bs='cr') +
                   s(delta_day,p_history,bs='tp'), data=data_train)
summary(model_gam)
mae(model_gam, data_val) # 0.1745535

model_gam <- gam(p_recall ~ s(p_history,bs='cr') + 
                   s(delta_day,p_history,bs='tp'), data=data_train)
summary(model_gam)
mae(model_gam, data_val) # 0.1745535

model_gam <- gam(p_recall ~ s(p_history,bs='cr'), data=data_train)
summary(model_gam)
mae(model_gam, data_val) # 0.1748795

model_gam <- gam(p_recall ~ s(delta_day,bs='cr'), data=data_train)
summary(model_gam)
mae(model_gam, data_val) # 0.1771942

############################### family = 'binomial' ##############################

model_gam <- gam(p_recall ~ s(p_history,bs='cr') +       ############## !!!
                   s(delta_day,bs='cr') +
                   s(lexeme_length,bs='cr') +
                   learning_language_3, data=data_train, family='binomial')
pred <- predict(model_gam, newdata=data_val, type="response") 
t.test(abs(data_val$p_recall - pred), abs(data_val$p_recall - mean(data_val$p_recall)), alternative='l')
#  0.1745183

model_gam <- gam(p_recall ~ s(history_correct,bs='cr') + 
                   s(history_seen,bs='cr') +
                   s(delta_day,bs='cr') +
                   s(lexeme_length,bs='cr'), data=data_train, family='binomial')
summary(model_gam)
pred <- predict(model_gam, newdata=data_val, type="response") 
t.test(abs(data_val$p_recall - pred), abs(data_val$p_recall - mean(data_val$p_recall)), alternative='l')
# 0.1734291

model_gam <- gam(p_recall ~ s(p_history,bs='cr') + 
                   s(delta_day,bs='cr') +
                   s(lexeme_length,bs='cr'), data=data_train, family='binomial')
summary(model_gam)
pred <- predict(model_gam, newdata=data_val, type="response") 
t.test(abs(data_val$p_recall - pred), abs(data_val$p_recall - mean(data_val$p_recall)), alternative='l')
# 0.1745164

model_gam <- gam(p_recall ~ s(p_history,bs='cr') + 
                   s(delta_day,bs='cr') +
                   s(lexeme_length,bs='cr') +
                   s(p_history,delta_day,bs='tp'), data=data_train, family='binomial')
summary(model_gam)
pred <- predict(model_gam, newdata=data_val, type="response") 
t.test(abs(data_val$p_recall - pred), abs(data_val$p_recall - mean(data_val$p_recall)), alternative='l')
# 0.1743835

model_gam <- gam(p_recall ~ s(p_history,bs='cr') + 
                   s(delta_day,bs='cr') +
                   s(delta_day,p_history,bs='tp'), data=data_train, family='binomial')
summary(model_gam)
pred <- predict(model_gam, newdata=data_val, type="response") 
t.test(abs(data_val$p_recall - pred), abs(data_val$p_recall - mean(data_val$p_recall)), alternative='l')
# 0.1746067

model_gam <- gam(p_recall ~ s(p_history,bs='cr') + 
                   s(delta_day,p_history,bs='tp'), data=data_train, family='binomial')
summary(model_gam)
pred <- predict(model_gam, newdata=data_val, type="response") 
t.test(abs(data_val$p_recall - pred), abs(data_val$p_recall - mean(data_val$p_recall)), alternative='l')
# 0.1746066 

model_gam <- gam(p_recall ~ s(p_history,bs='cr'), data=data_train, family='binomial')
summary(model_gam)
pred <- predict(model_gam, newdata=data_val, type="response") 
t.test(abs(data_val$p_recall - pred), abs(data_val$p_recall - mean(data_val$p_recall)), alternative='l')
# 0.1748801

model_gam <- gam(p_recall ~ s(delta_day,bs='cr'), data=data_train, family='binomial')
summary(model_gam)
pred <- predict(model_gam, newdata=data_val, type="response") 
t.test(abs(data_val$p_recall - pred), abs(data_val$p_recall - mean(data_val$p_recall)), alternative='l')
# 0.1771993

# ------------------- Transformed p_recall -------------

model_gam <- gam(p_logit ~ s(history_correct,bs='cr') + 
                   s(history_seen,bs='cr') +
                   s(delta_day,bs='cr') +
                   s(lexeme_length,bs='cr'), data=data_train)
summary(model_gam)
pred <- predict(model_gam, newdata=data_val, type="response") %>% plogis()
t.test(abs(data_val$p_recall - pred), abs(data_val$p_recall - mean(data_val$p_recall)), alternative='l')
# 0.1734254 - 0.1087811

model_gam <- gam(p_logit ~ s(p_history,bs='cr') + 
                   s(delta_day,bs='cr') +
                   s(lexeme_length,bs='cr'), data=data_train)
summary(model_gam)
pred <- predict(model_gam, newdata=data_val, type="response") %>% plogis()
t.test(abs(data_val$p_recall - pred), abs(data_val$p_recall - mean(data_val$p_recall)), alternative='l')
# 0.1744677 - 0.1094400

model_gam <- gam(p_logit ~ s(p_history,bs='cr') + 
                   s(delta_day,bs='cr') +
                   s(lexeme_length,bs='cr') +
                   s(p_history,delta_day,bs='tp'), data=data_train)
summary(model_gam)
pred <- predict(model_gam, newdata=data_val, type="response") %>% plogis()
t.test(abs(data_val$p_recall - pred), abs(data_val$p_recall - mean(data_val$p_recall)), alternative='l')
# 0.1743276 - 0.1094434

model_gam <- gam(p_logit ~ s(p_history,bs='cr') + 
                   s(delta_day,bs='cr') +
                   s(lexeme_length,bs='cr') +
                   learning_language_3, data=data_train)
summary(model_gam)
pred <- predict(model_gam, newdata=data_val, type="response") %>% plogis()
t.test(abs(data_val$p_recall - pred), abs(data_val$p_recall - mean(data_val$p_recall)), alternative='l')
# 0.1744718 - 0.1094418

model_gam <- gam(p_logit ~ s(p_history,bs='cr') + 
                   s(delta_day,bs='cr') +
                   s(delta_day,p_history,bs='tp'), data=data_train)
summary(model_gam)
pred <- predict(model_gam, newdata=data_val, type="response") %>% plogis()
t.test(abs(data_val$p_recall - pred), abs(data_val$p_recall - mean(data_val$p_recall)), alternative='l')
# 0.1745535 - 0.1094501

model_gam <- gam(p_logit ~ s(p_history,bs='cr') + 
                   s(delta_day,p_history,bs='tp'), data=data_train)
summary(model_gam)
pred <- predict(model_gam, newdata=data_val, type="response") %>% plogis()
t.test(abs(data_val$p_recall - pred), abs(data_val$p_recall - mean(data_val$p_recall)), alternative='l')
# 0.1745535 - 0.1094501 

model_gam <- gam(p_logit ~ s(p_history,bs='cr'), data=data_train)
summary(model_gam)
pred <- predict(model_gam, newdata=data_val, type="response") %>% plogis()
t.test(abs(data_val$p_recall - pred), abs(data_val$p_recall - mean(data_val$p_recall)), alternative='l')
# 0.1748795 - 0.1094035

model_gam <- gam(p_logit ~ s(delta_day,bs='cr'), data=data_train)
summary(model_gam)
pred <- predict(model_gam, newdata=data_val, type="response") %>% plogis()
t.test(abs(data_val$p_recall - pred), abs(data_val$p_recall - mean(data_val$p_recall)), alternative='l')
# 0.1771942 - 0.1093016



#################### Now we consider avg_user_p #######################


model_gam <- gam(p_recall ~ s(avg_user_p,bs='cr'), data=data_train)
summary(model_gam)
mae(model_gam,data_val) # 0.1692333

model_gam <- gam(p_recall ~ s(avg_user_p,bs='cr') +
                   s(delta_day,bs='cr'), data=data_train)
summary(model_gam)
mae(model_gam,data_val) # 0.169221

model_gam <- gam(p_recall ~ s(avg_user_p,bs='cr') +
                   s(p_history,bs='cr'), data=data_train)
summary(model_gam)
mae(model_gam,data_val) # 0.1686143

model_gam <- gam(p_recall ~ s(avg_user_p,bs='cr') +
                 s(p_history,bs='cr') + 
                 s(delta_day,bs='cr'), data=data_train)
summary(model_gam)
mae(model_gam,data_val) # 0.1685893

model_gam <- gam(p_recall ~ s(avg_user_p,bs='cr') +
                   s(p_history,bs='cr') + 
                   s(delta_day,bs='cr') +
                   s(p_history,avg_user_p,bs='tp'), data=data_train)
summary(model_gam)
mae(model_gam,data_val) # 0.1685324

model_gam <- gam(p_recall ~ s(avg_user_p,bs='cr') +
                   s(p_history,bs='cr') + 
                   s(delta_day,bs='cr') +
                   s(delta_day,p_history,bs='tp'), data=data_train)
summary(model_gam)
mae(model_gam,data_val) # 0.1685398


# residuals             
dev.off()
hist(residuals(model_gam),breaks=50)
plot(model_gam)

# we check the model
gam.check(model_gam)

# ______________________________________________________________
# Parametric Test ----------------------------------------------
# H0: delta_day = 0 VS H1: delta_day != 0

summary(model_gam)
T0 <- abs(summary(model_gam)$s.table[3,3])
T0 # F-statistic

gam.H0 <- gam(p_recall ~ s(avg_user_p,bs='cr') +
                   s(p_history,bs='cr'), data=data_train)

residui.H0 <- gam.H0$residuals

B = 1000
n=nrow(data_train)

#to simulate the distribution under H0 of my test statistic, let's try another computational approach.
set.seed(26091996)
library(pbapply)

wrapper = function(){
  permutazione <- sample(n)
  residui.H0.perm <- residui.H0[permutazione]
  Y.perm.H0 <- gam.H0$fitted + residui.H0.perm
  gam.perm = gam(Y.perm.H0 ~ s(avg_user_p,bs='cr') +
                   s(p_history,bs='cr') + 
                   s(delta_day,bs='cr'), data=data_train)
  return(abs(summary(gam.perm)$s.table[3,3]))
}
# We ran the code:
T_H0 <- pbreplicate(B,wrapper(),simplify = 'vector')

hist(T_H0) + 
  abline(v=T0,col='red')

plot(ecdf(T_H0)) + 
  abline(v=T0,col='red')
sum(T_H0>=T0)/B


                            #################
# ------------------------- #### MODEL 2 #### ------------------------------------
                            #################

attach(data_train)
hist(delta_day,breaks=50)
quantile(delta_day)
d1 <- range(delta_day)[1]
d2 <- range(delta_day)[2]
delta_day_knots <- exp(seq(d1,log(d2),length.out=10))
hist(delta_day_knots,breaks=10)
detach(data_train)


model_gam <- gam(p_logit ~ avg_user_p +
                   s(delta_day, bs='cr'),
                 knots=list(delta_day=delta_day_knots),
                 data=data_train)
summary(model_gam)
pred <- predict(model_gam, newdata=data_val, type="response") %>% plogis()
t.test(abs(data_val$p_recall - pred), abs(data_val$p_recall - mean(data_val$p_recall)), alternative='l')
# 0.1093155 


### ------------------------------- Permutation test

# Model:
# qlogis(p_recall) ~ beta_0 + beta_1*avg_user_p + f(delta_day)
# 
# H0:                             vs    H1: 
# L2 norm of f(delta_day) = 0           L2 norm of f(delta_day) \neq 0

T.fun.delta_day.old <- function(smooth) {
  sum(smooth$coefficients^2)
}

smooth_squared <- function(d, model) {
  smooth.d <- predict.gam(model, 
                          data.frame(delta_day = d,
                                     avg_user_p = rep(1, length(d))), 
                          terms="s(delta_day)",
                          type="terms")[, 1]
  smooth.d^2
}

T.fun.delta_day <- function(model) {
  integrate(smooth_squared, d1, d2, model)$value
}

T0.delta_day <- T.fun.delta_day(model_gam)
gam.H0 = gam(p_logit ~ avg_user_p, data=data_train)
residui.H0 <- residuals(gam.H0, type="response")
B = 1000
n=length(resp)

wrapper.delta_day <- function(){
  perm <- sample(n)
  residui.H0.perm <- residui.H0[perm]
  resp.perm.H0 <- gam.H0$fitted + residui.H0.perm
  gam.perm = gam(resp.perm.H0 ~ avg_user_p +
                   s(delta_day, bs='cr'),
                 knots=list(delta_day=delta_day_knots), data=data_train)
  #smooth.perm <- gam.perm$smooth[[1]]
  #first <- smooth.perm$first.para
  #last <- smooth.perm$last.para
  #smooth.perm$coefficients <- gam.perm$coefficients[first:last]
  #T.fun.delta_day(smooth.perm)
  T.fun.delta_day(gam.perm)
}

#clusterExport(cl=clust,ls())
#clusterExport(cl=clust,list("gam", "avg_user_p", "delta_day", "predict.gam"))
#T_H0.delta_day <- pbreplicate(B,wrapper.delta_day(),simplify = 'vector', cl = clust)
T_H0.delta_day <- pbreplicate(B,wrapper.delta_day(),simplify = 'vector')

hist(T_H0.delta_day,breaks=20)
abline(v=T0.delta_day, col="green")
plot(ecdf(T_H0.delta_day))
abline(v=T0.delta_day, col="green")
sum(T_H0.delta_day>=T0.delta_day)/B
# p-value: 0.131 (Giacomo: 0.18)


