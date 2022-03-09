########################################
###    DUOLINGO DATA EXPLORATION     ###
########################################
setwd('/Users/Francesco/Documents/DUOLINGO_PROJECT')

library(tidyverse)
library(DepthProc)
library(hexbin) 

#duolingo_big <- read_csv('duolingo_learning_traces.csv')
#duolingo_huge <- duolingo_big %>%
#  mutate(delta_day = delta/3600) %>%
#  mutate(delta_day = floor(delta_day/24), p_history = history_correct/history_seen)

# We compute the avg_user_p:
#duolingo_huge <- duolingo_huge %>%
#  group_by(user_id) %>%
#  mutate(avg_user_p = mean(p_recall))
#hist(duolingo_huge$avg_user_p,breaks=100)

#group_learning_lan <- function(ll) {
#  switch(ll, fr=0, it=,pt=1,2)
#}
#duolingo_huge <- duolingo_huge %>%
#  mutate(learning_language_3 = pbsapply(learning_language, group_learning_lan, USE.NAMES = F),
#       ui_italian = ui_language == "it") 
#duolingo$delta_day[which.max(duolingo$delta_day)]
#floor( duolingo_big$delta[which.max(duolingo_big$delta)]/(3600*24) )

#save(duolingo_huge,file = 'duolingo_huge.RData')

load("duolingo_huge.RData")

#_____________________________________________________________________________

###########################
####   PREPROCESSING   ####
###########################

users_100 <- names(which(table(duolingo_huge$user_id) > 100))
length(users_100)/length(unique(duolingo_huge$user_id))*100

duolingo_filtered <- duolingo_huge %>%
  filter(user_id %in% users_100 == T)

set.seed(26091996)
duolingo <- sample_n(duolingo_filtered,100000)

length(unique(duolingo_huge$lexeme_id))
# _________________________________________________________________-

francesco <- function(data_,alpha1,alpha2){
  q_inf1 <- quantile(table(data_$user_id),alpha1)
  q_sup1 <- quantile(table(data_$user_id),1-alpha1)
  q_inf2 <- quantile(table(data_$lexeme_id),alpha2)
  q_sup2 <- quantile(table(data_$lexeme_id),1-alpha2)
  
  names <- names(which(table(data_$user_id) >= q_inf1 & table(data_$user_id) <= q_sup1))
  words <- names(which(table(data_$lexeme_id) >= q_inf2 & table(data_$lexeme_id) <= q_sup2))
  data <- data_ %>%
    filter(lexeme_id %in% words == T) %>%
    filter(user_id %in% names == T)
  return(data)
  plot(dim(data))
}

duolingo <- francesco(duolingo_huge,alpha1=0.15,alpha2=0.15)
nrow(duolingo)

# __________________________________________________________________________

###############################
#### We start our analysis ####
###############################

detach(duolingo)
attach(duolingo)

# ------------------ delta_day
duolingo %>%
  filter(delta_day<=100) %>%
  ggplot(aes(x=delta_day)) +
  stat_bin(binwidth=5,fill='green4') +
  theme_bw()

boxplot(duolingo$delta_day)

duolingo_filtered %>%
  ggplot(aes(x=delta_day)) +
  stat_bin(binwidth=10)

duolingo %>%
  select(delta_day) %>%
  filter(delta_day <= 31) %>%
  ggplot(aes(x=delta_day)) +
  stat_bin(binwidth=1)

# ------------------ history_correct - history_seen

duolingo %>%
  ggplot(aes(x=history_correct)) +
  stat_bin(binwidth=10)

duolingo %>%
  filter(history_correct<200) %>%
  ggplot(aes(x=history_correct)) +
  stat_bin(binwidth=5,fill='green4')

duolingo_filtered %>%
  filter(history_correct<200) %>%
  ggplot(aes(x=history_correct)) +
  stat_bin(binwidth=5)

duolingo %>%
  ggplot(aes(x=history_seen)) +
  stat_bin(binwidth=10)

duolingo %>%
  filter(history_seen<200) %>%
  ggplot(aes(x=history_seen)) +
  stat_bin(binwidth=5,fill='green4')

duolingo_filtered %>%
  filter(history_seen<200) %>%
  ggplot(aes(x=history_seen)) +
  stat_bin(binwidth=5)


# ------------------ p_history
duolingo %>%
  ggplot(aes(x=p_history)) +
  stat_bin(binwidth=.05, fill='green4')

duolingo_filtered %>%
  ggplot(aes(x=p_history)) +
  stat_bin(binwidth=.05)


# ------------------ p_recall
duolingo %>%
  ggplot(aes(x=p_recall)) +
  stat_bin(binwidth=.05,fill='green4') 

duolingo %>%
  filter(history_correct<30) %>%
  filter(delta_day < 31) %>%
  ggplot(aes(x=p_recall)) +
  stat_bin(binwidth=.05) 

# --------------------- lexeme_length
duolingo %>%
  ggplot(aes(x=lexeme_length)) +
  stat_bin(binwidth=1,fill='green4') 


# ------------------- p_recall ~ delta_day
duolingo %>%
  filter(delta_day<150) %>%
  group_by(delta_day) %>%
  mutate(avg_p = mean(p_recall)) %>%
#  mutate(avg_p = median(p_recall)) %>%
  ggplot(aes(x=delta_day, y=p_recall)) +
  geom_point(col="gray80") #+
#  geom_point(aes(x=delta_day, y=avg_p), col="red") 

depthContour(cbind(delta_day,p_recall),depth_params = list(method='Tukey'),legend=F)
depthPersp(cbind(delta_day,p_recall),depth_params = list(method='Tukey'),xlab='delta_day',ylab='p_recall')


# ------------------- p_recall ~ history_seen
duolingo %>%
  filter(history_seen < 1000) %>%
  group_by(history_seen) %>%
  mutate(avg_p = mean(p_recall)) %>%
  ggplot(aes(x=history_seen, y=p_recall)) +
  geom_point(alpha=0.05) +
  geom_point(aes(x=history_seen, y=avg_p), col="red", alpha=0.05)

# ------------------- p_recall ~ history_correct
duolingo %>%
  filter(history_correct < 1500) %>%
  group_by(history_correct) %>%
  mutate(avg_p = mean(p_recall)) %>%
  ggplot(aes(x=history_correct, y=p_recall)) +
  geom_point(alpha=0.05) +
  geom_point(aes(x=history_correct, y=avg_p), col="red", alpha=0.05)

# ------------------- p_recall ~ p_history
duolingo %>%
  group_by(p_history) %>%
  mutate(avg_p = mean(p_recall)) %>%
  ggplot(aes(x=p_history, y=p_recall)) +
  geom_point(alpha=0.05) +
  geom_point(aes(x=p_history, y=avg_p), col="red")

bin=hexbin(p_recall,p_history, xbins=10, xlab="exp 1", ylab="exp 2")
plot(bin, main="Hexagonal Binning")

depthContour(cbind(p_history,p_recall),depth_params = list(method='Tukey'),legend=F)
depthPersp(cbind(p_history,p_recall),depth_params = list(method='Tukey'),xlab='p_history',ylab='p_recall')

# ------------------- p_recall ~ avg_user_p
duolingo %>%
  group_by(avg_user_p) %>%
  mutate(avg_p = mean(p_recall)) %>%
  ggplot(aes(x=avg_user_p, y=p_recall)) +
  geom_point(alpha=0.05) +
  geom_point(aes(x=avg_user_p, y=avg_p), col="red")

bin=hexbin(avg_user_p,p_recall, xbins=10, xlab="exp 1", ylab="exp 2")
plot(bin, main="Hexagonal Binning")

depthContour(cbind(avg_user_p,p_recall),depth_params = list(method='Tukey'),legend=F)
depthPersp(cbind(avg_user_p,p_recall),depth_params = list(method='Tukey'),xlab='avg_user_p',ylab='p_recall')

# ------------------- p_recall ~ lexeme_length

duolingo %>%
  #group_by(lexeme_length) %>%
  #mutate(avg_p = mean(p_recall)) %>%
  ggplot(aes(x=lexeme_length, y=p_recall)) +
  geom_point(alpha=0.05) #+
  #geom_point(aes(x=p_history, y=avg_p), col="red")

bin=hexbin(lexeme_length, p_recall, xbins=10, xlab="exp 1", ylab="exp 2")
plot(bin, main="Hexagonal Binning")

depthContour(cbind(lexeme_length,p_recall),depth_params = list(method='Tukey'),legend=F)
depthPersp(cbind(lexeme_length,p_recall),depth_params = list(method='Tukey'),xlab='p_history',ylab='p_recall')



# ------------------ lexeme_string
duolingo %>%
  filter(history_seen < 30) %>%
  group_by(lexeme_string) %>%
  mutate(avg_p = mean(p_recall)) %>%
  ggplot(aes(x=lexeme_string, y=p_recall)) +
  geom_point(col="gray80") +
  geom_point(aes(x=lexeme_string, y=avg_p), col="red") 










