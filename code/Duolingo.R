MIN_HALF_LIFE <- 15.0 / (24 * 60)
MAX_HALF_LIFE <- 274

mydata <- read.csv('/Users/Francesco/Documents/DUOLINGO_PROJECT/duolingo_learning_traces.csv')
dim(mydata)
head(mydata)

# attach(mydata)
# detach(mydata)
#
# length(which(lexeme_id=='56429751fdaedb6e491f4795c770f5a4'))
#
# p_recall[which(lexeme_id=='56429751fdaedb6e491f4795c770f5a4' & 
#                     user_id=='u:FO' )]


pclip <- function(p){
  # bound min/max model predictions (helps with loss optimization)
  min(max(p, 0.0001), .9999)
}


hclip <- function(h){
  # bound min/max half-life
  min(max(h, MIN_HALF_LIFE), MAX_HALF_LIFE)
}



#### Let us treat our data: ####

p <- NULL
for(i in 1:dim(mydata)[1]){
  p[i] <- pclip(as.numeric(mydata[['p_recall']][i]))
}
p <- as.data.frame(p)
head(p)

t <- mydata['delta']/(60*60*24)
head(t)

plot(t[1:10000,],p[1:10000,])
eq = function(x){2^-x}
points(eq(min(t):max(t)), type='l',col='red',lwd=3)














