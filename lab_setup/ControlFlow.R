####################
#
# MDLE, 2019-2024
#
####################
#### Ex1 ####
#The EX.1 increments x 10 times with the values (0 1 2 3 4 5 6 7 8 9 10) to a total of 55
x<-0
for (i in seq(0,10,1)) 
{
  x<-x+i  
}

#### Ex2 ####
#The EX.2 we can see that rep(0, 10) creates a list with exactly 10 instances of 0, and because of the print the system prints 0 10 times
for (i in rep(0,10)) 
{
  print(i)  
}

#### Ex3 ####
#The EX.3 we start with a list with all the numbers from 1 to 9 and on every iteration we remove the first one, then we print the list. The last part checks if 6 belongs to v and if it doesn't the loop is broken 
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
df <- copy_to(ss, iris) #Uploads the data frame iris to the spark_connection 
show(df)

#### Ex4 d ####
#head function returns the first parts of the data frame(6 rows by default) 
#select function allow to select variables in the data frame
#filter function subset the data frame, retaining all the rows that satisfy the condition

head( select(df, Petal_Width, Species))
head( filter(df, Petal_Width > 0.3))
df %>% head

#### Ex4 e ####
#dbGetQuery function returns the result of a query as a data frame
library(DBI)
df_sql <- dbGetQuery(ss, "SELECT * FROM iris WHERE Petal_Width > 0.3 LIMIT 5" )
show(df_sql)

#### Ex4 f ####
#collect function retrieves data from df into local simple data frame
local_df <- collect(df)
show(local_df)
show(df)

#### Ex4 g ####
#spark_disconnect function allows to disconnect the open connection 
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