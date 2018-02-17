#install.packages("tidyverse")
#install.packages("Hmisc")
#install.packages("mice")
library(tidyverse)
# ---- Load the millennial feature data ----
data <- read.table('C:/Data/millennial')
col <- read.table('C:/Data/millennial_column')
colnames(data) <- col$V2
# ---- brief analysis of data ----
#  
nrow(data)
ncol(data)
dim(data)
head(data,3)
str(data)
attributes(data)
names(data)
summary(data)
# factors analysis
library(Hmisc)
describe(data[,1:30])
# missing value 
library(mice)
md.pattern(data)
# colrelation examples
## strong cor between google_visit and google_aveSpend
cor(data$google_visit,data$google_aveSpend)
## small cor between fossil_visit and rue21_visit
cor(data$fossil_visit,data$rue21_visit)
# ---- Sample/separate data ----

n <- dim(data)[1]
p <- dim(data)[2]

visit <- which( 1:p %% 2 == 0 )
spend <- which( 1:p %% 2 == 1 )[-1]
data_v <- data[,visit]
data_s <- data[,spend]

mystats <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  pctls<-quantile(a,probs=c(0.01, 0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99))
  max <- max(a)
  return(c(n=n, nmiss=nmiss,  mean=m, stdev=s,min = min, pctls=pctls,max=max))
}

# ---- visit ----
dim(data_v)
head(data_v,3)
str(data_v)
summary(data_v)
attributes(data_v)


diag_stats<-t(data.frame(apply(data_v[,-1], 2, FUN = mystats)))
write.csv(diag_stats, "C:/Data/diag_stats.csv")
## FACTOR ANALYSIS 
corrm<- cor(scaleDataV)
### DECIDING NUMBER OF FACTORS USING SCREE PLOT & EIGEN VALUES OVER 1
require(psych)
scree(corrm, factors=T, pc=T, main="scree plot", hline=NULL, add=FALSE) ### SCREE PLOT


#-------------------------
# ralphlauren_visit vs. guess_visit
#-------------------------

ggplot(data=data_v, aes(x=ralphlauren_visit, y=guess_visit)) +
  geom_point(alpha=.4, size=4, color="#880011") +
  ggtitle("ralphlauren_visit vs. guess_visit") +
  labs(x="ralphlauren_visit, visitCount", y="guess_visit,visitCount")

#------------------------
# Histogram of gap_visit
#------------------------

ggplot(data=data_v, aes(x=gap_visit)) +
  geom_histogram(fill="#880011") +  
  ggtitle("Histogram of gap_visit") +
  labs(x="gap_visit, visitCount", y="sum\nof Records")

# ---- spend ----
dim(data_s)
head(data_s,3)
str(data_s)
summary(data_s)
attributes(data_s)
#-------------------------
# google_aveSpend vs. apple_aveSpend
#-------------------------

ggplot(data=data_s, aes(x=google_aveSpend, y=apple_aveSpend)) +
  geom_point(alpha=.4, size=4, color="#880011") +
  ggtitle("google_aveSpend vs. apple_aveSpend") +
  labs(x="google_aveSpend, aveSpend", y="apple_aveSpend,\n aveSpend")

#------------------------
# Histogram of netflix_aveSpend
#------------------------

ggplot(data=data_s, aes(x=netflix_aveSpend)) +
  geom_histogram(fill="#880011") +  
  ggtitle("Histogram of netflix_aveSpend") +
  labs(x="netflix_aveSpend, dollar", y="sum\nof Records")

