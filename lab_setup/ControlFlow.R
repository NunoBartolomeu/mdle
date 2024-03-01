####################
#
# MDLE, 2019-2024
#
####################
#### Ex1 ####
x<-0
for (i in seq(0,10,1)) 
{
  x<-x+i  
}

#### Ex2 ####
for (i in rep(0,10)) 
{
  print(i)  
}

#### Ex3 ####
v<-c(1,2,3,4,5,6,7,8,9)
while(length(v)>0) 
{
  v<-v[-1]
  print(v)
  if( (6 %in% v) == FALSE)
    break
}

#### Ex2 d ####
#Gets the first 2 columns of my.data, using range operator indexation
my.data[, 1:2]

#### Ex2 e ####
#Gets the first two columns of my.data, using vector indexation
my.data[, c("ID", "Name")]

#### Ex2 f ####
describe(my.data)

#### Ex4 a ####
#Checks if sparklyr is loaded
if (require(sparklyr)) {
  print("sparklyr is loaded")
} else {
  print("sparklyr is not loaded.")
}

#Connection to Spark
ss <- spark_connect('local', version = '3.4.2', hadoop_version = '3', config = list())
print(ss)

#### Ex4 c ####
library(dplyr)
df <- copy_to(ss, iris)
show(df)

#### Ex4 d ####
head( select(df, Petal_Width, Species))
head( filter(df, Petal_Width > 0.3))
df %>% head

#### Ex4 e ####
library(DBI)
df_sql <- dbGetQuery(ss, "SELECT * FROM iris WHERE Petal_Width > 0.3 LIMIT 5" )
show(df_sql)

#### Ex4 f ####
local_df <- collect(df)
show(local_df)
show(df)

#### Ex4 g ####
spark_disconnect(ss)

if (spark_connection_is_open(ss)) {
  print("spark connection is open")
} else {
  print("spark connection is closed.")
}

#### Ex4 h ####
#Supervised Learning
#It is defined by its use of labeled datasets to train algorithms 
#that to classify data or predict outcomes accurately.
#Supervised learning can be separated into two types of problems when data mining
#—classification and regression:
#One example of supervised learning on Spark is random forest which is a classification type.

#### Ex4 i ####
#Unsupervised Learning
#Uses machine learning algorithms to analyze and cluster unlabeled datasets.
#These algorithms discover hidden patterns or data groupings without the need for human intervention.
#One example of unsupervised learning on Spark is K-means clustering

#### Ex4 j ####
#Spark's ML Pipelines provide a way to easily combine multiple transformations
#and algorithms into a single workflow, or pipeline.