

# Load required libraries
install.packages(c("pacman", "tidyverse", "lubridate", "tm", "topicmodels",
                   "sentimentr", "udpipe","fastDummies","dplyr","caret",
                   "lares","randomForest","partykit"))

library(pacman)
p_load(tidyverse, lubridate, tm, topicmodels, sentimentr, udpipe,dplyr,caret,lares)
p_load(DMwR)
p_load(cluster)# Clustering
p_load(factoextra) # clustering algorithms & visualization
p_load(GGally)
p_load(dendextend)
p_load(dynamicTreeCut)

# Read the CSV file
df <- read.csv("C:/Users/sj01148/OneDrive - University of Surrey/Documents/Disseratation/dissertation analysis/diss_data.csv", stringsAsFactors = FALSE)

str(df)

# Data Cleaning
# Convert date columns to Date format
df$ENQUIRY.DATE <- dmy(df$ENQUIRY.DATE)
df$OFFER.PROCESS.START.DATE <- dmy(df$OFFER.PROCESS.START.DATE)
df$OFFER.SENT.DATE <- dmy(df$OFFER.SENT.DATE)
df$First.Follow.Up..Date. <- dmy(df$First.Follow.Up..Date.)
df$Second.Follow.Up.Date <- dmy(df$Second.Follow.Up.Date)
df$Third.Follow.Up.Date <- dmy(df$Third.Follow.Up.Date)

# Convert character columns to factors
df$CLIENT <- as.factor(df$CLIENT)


summary(df)

hist(df$QTY, main="Histogram of Quantity", xlab="Quantity", col="lightblue", border="black")

# Create a table from the data
enquiry_table <- table(df$ENQUIRY.TYPE)


# Adjust the max value  based on your requirements
y_limits <- c(0, 1600)

# Create the barplot
barplot(enquiry_table, main="Enquiry Type Distribution", xlab="Enquiry Type", 
        ylab="Frequency", ylim=y_limits, col="lightblue")

# Create a table from the data
enquiry_table <- table(df$EQUIPMENT.TYPE)

# Adjust the max value  based on your requirements
y_limits <- c(0, 350)

# Create the barplot
barplot(enquiry_table, main="Enquipment Type Distribution", xlab="Enquipment Type", 
        ylab="Frequency", ylim=y_limits, col="lightblue")

# Create a table from the data
enquiry_table <- table(df$EQUIPMENT.MATERIAL)

# Adjust the max value  based on your requirements
y_limits <- c(0, 600)

# Create the barplot
barplot(enquiry_table, main="Enquipment Material Distribution", xlab="Enquipment Materials", 
        ylab="Frequency", ylim=y_limits, col="lightblue")



# Create a table from the data
enquiry_table <- table(df$STATUS)

# Adjust the max value (150 in this case) based on your requirements
y_limits <- c(0, 1600)

# Create the barplot
barplot(enquiry_table, main="Status Distribution", xlab="Status", 
        ylab="Frequency", ylim=y_limits, col="lightblue")


enquiry_table <- table(df$ENQUIRY.PROCESSED.BY)

# Adjust the max value based on your requirements
y_limits <- c(0, 500)

# Create the barplot
barplot(enquiry_table, main="Enquiry Processed By Distribution", xlab="Enquiry Processed By", 
        ylab="Frequency", ylim=y_limits, col="lightblue")


boxplot(df$QTY, main="Boxplot of Quantity", ylab="Quantity")

library(e1071)
skewness(df$QTY)

#to understand the status column
table(df$STATUS)




# Feature Engineering
# Extract month from date_column and create a new column named 'month'
df$ENQUIRY.MONTH <- month(df$ENQUIRY.DATE)

print(df)

# Calculate number of Follow ups in dataset
df$Number.OF.Follow.Up <- ifelse(df$First.Follow.Up.Details== "CLOSED",1,
                                 ifelse(df$Second.Follow.Up.Details=="CLOSED",2,3))

# Enquiry start date to end in dataset
df$ENQUIRY.START.TO.END.IN.DAYS <- ifelse(df$Number.OF.Follow.Up== "1",as.numeric(df$First.Follow.Up..Date. - df$ENQUIRY.DATE),
                                          ifelse(df$Number.OF.Follow.Up=="2",as.numeric(df$Second.Follow.Up.Date - df$ENQUIRY.DATE),
                                                 as.numeric(df$Third.Follow.Up.Date - df$ENQUIRY.DATE)))


# Remove unnecessary columns
data <- df[, -which(names(df) %in% c("QUOTATION.NO", "First.Follow.Up..Date.", "First.Follow.Up.Details",
                                         "Second.Follow.Up.Date", "Second.Follow.Up.Details",
                                         "Third.Follow.Up.Date", "Third.Follow.Up.Details",
                                         "Status.Details","Equipment.Specification", "JOB.No.",
                                         "OFFER.PROCESS.START.DATE","ENQUIRY.DATE","OFFER.SENT.DATE"))]


# Convert character columns to factors
data$CLIENT <- as.factor(data$CLIENT)
data$ENQUIRY.TYPE <- as.factor(data$ENQUIRY.TYPE)
#... similarly for other character columns

hist(data$QTY, main="Histogram of Quantity", xlab="Quantity", col="lightblue", border="black")

# ... similarly for other numeric variables

library(ggplot2)
ggplot(data, aes(x=factor(ENQUIRY.MONTH))) + 
  geom_bar(fill="lightblue") + 
  labs(title="Number of Enquiries by Month", x="Month", y="Count")

# Create a table from the data
enquiry_table <- table(df$Number.OF.Follow.Up)

# Adjust the max value based on your requirements
y_limits <- c(0, 1300)

# Create the barplot
barplot(enquiry_table, main="Number of Follow-ups Distribution", xlab="Number of Follow-ups", 
        ylab="Frequency", ylim=y_limits, col="lightblue")



# Create a table from the data
enquiry_table <- table(df$ENQUIRY.START.TO.END.IN.DAYS)

# Adjust the max value (150 in this case) based on your requirements
y_limits <- c(0, 200)

# Create the barplot
barplot(enquiry_table, main="Duration of Enquiries Distribution", xlab="Duration of Enquiries", 
        ylab="Frequency", ylim=y_limits, col="lightblue")




#one hot encoding
data_encoded <- data %>%
  select(ENQUIRY.TYPE,CLIENT, EQUIPMENT.TYPE, 
         QTY, EQUIPMENT.MATERIAL, Final.Discount.Offered, 
         STATUS) %>% 
  mutate(STATUS = ifelse(STATUS == "PO received", 1, 0)) 
data_matrix <- data_encoded %>%
  mutate_at(vars(-STATUS), as.factor) %>% 
  model.matrix(STATUS ~ ., data = .) %>% 
  as.data.frame()

# Add STATUS back to the encoded data
data_matrix$STATUS <- data_encoded$STATUS

unique(data_encoded$STATUS)
# scaling
data_matrix_without_status <- data_matrix %>% select(-STATUS)
scaled_matrix <- scale(data_matrix_without_status)

# Convert the scaled matrix to a dataframe
scaled_data <- as.data.frame(scaled_matrix)

str(data)
# Assign the STATUS column
scaled_data$STATUS <- data_encoded$STATUS
#preprocessing data
scaled_data$`(Intercept)` <- NULL
names(scaled_data) <- gsub("[^a-zA-Z0-9_]", "_", names(scaled_data))
scaled_data$ENQUIRY.MONTH <- data$ENQUIRY.MONTH
scaled_data$Number.OF.Follow.Up <- data$Number.OF.Follow.Up
scaled_data$ENQUIRY.START.TO.END.IN.DAYS <- data$ENQUIRY.START.TO.END.IN.DAYS

#preprocessing data
scaled_data$`(Intercept)` <- NULL
names(scaled_data) <- gsub("[^a-zA-Z0-9_]", "_", names(scaled_data))
data_clustred <- scaled_data
# Assuming scaled_data is your data
set.seed(123)  # Setting a seed for reproducibility


##########################################################################################################
###### K means Clustering #####################################
###########################################################################################################
# Compute total within-cluster sum of square
wss <- sapply(1:10, function(k){
  kmeans(data_clustred, centers = k, nstart = 25)$tot.withinss
})


# Plot the elbow curve
plot(1:10, wss, type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters k",
     ylab="Total within-clusters sum of squares")

# with elbow curve we got to know k=3
colnames(data_clustred)
data_clustred[is.na(data_clustred)] <- sapply(data_clustred, mean, na.rm = TRUE)
data_clustred[sapply(data_clustred, is.infinite)] <- NA



# 2. Apply k-means clustering
kmeans_result <- kmeans(data_clustred, centers = 3, nstart = 25)  # Choose 3 centers, and try 25 random sets of initial centers
data_clustred$cluster <- as.factor(kmeans_result$cluster)  # Assign cluster results back to the data frame



# Identify columns with zero variance
zero_vars <- nearZeroVar(data_clustred, saveMetrics= TRUE)

# Select only the columns with variance
data_clustred <- data_clustred[, zero_vars$nzv == FALSE]


# Keeping only numeric columns from data_clustred
numeric_data <- data_clustred[, sapply(data_clustred, is.numeric)]

# Visualizing clusters
cluster2plot <- fviz_cluster(kmeans_result, data = numeric_data)
print(cluster2plot)
library(GGally)

ggpairs(data_clustred, columns = 1:10, aes(color = cluster))  # Assuming you're plotting the first four features.

# for specific cunctions
library(ggplot2)

str(data_clustred)
ggplot(data_clustred, aes(x = Number_OF_Follow_Up , y = ENQUIRY_START_TO_END_IN_DAYS, color = cluster)) +
  geom_point() +
  ggtitle("2D K-means Clustering") +
  theme_minimal()

ggplot(data_clustred, aes(x = ENQUIRY_MONTH , y = STATUS, color = cluster)) +
  geom_point() +
  ggtitle("2D K-means Clustering") +
  theme_minimal()

# to gnerate cluster data on power BI
data_clustered_orig <- data
data_clustered_orig$STATUS <- ifelse(data_clustered_orig$STATUS == "Closed", 0, 1) # Convert to binary (0 for Closed, 0 otherwise)
# Convert percentage column to numeric
data_clustered_orig$Final.Discount.Offered <- as.numeric(gsub("%", "", data_clustered_orig$Final.Discount.Offered)) / 100
data_clustered_orig$cluster <- data_clustred$cluster
barplot(table(data_clustered_orig$cluster), main="cluster Distribution", xlab="Cluster", ylab="Frequency")
library(ggplot2)


ggplot(data_clustered_orig, aes(x = cluster)) + 
  geom_bar(fill = "steelblue") +
  labs(title = "Cluster Distribution", x = "Cluster", y = "Frequency") +
  theme_minimal()
#write.csv(data_clustered_orig,"C:/Users/sj01148/OneDrive - University of Surrey/Documents/Disseratation/dissertation analysis/data_clustered_orig.csv")

#######################################################################################################################################################
####  Hierarchical clustering  #######################################################################################################
#######################################################################################################################################################

data_clustred <- scaled_data
# Compute the distance matrix
dist_matrix <- dist(data_clustred, method = "euclidean")

# Hierarchical clustering using the complete linkage method
hclust_result <- hclust(dist_matrix, method = "complete")

# Plot the dendrogram
plot(hclust_result)

# Cut the dendrogram to create 3 clusters
clusters <- cutree(hclust_result, k = 3)

# Use the dendextend package to color and label branches
dend <- as.dendrogram(hclust_result)
dend <- color_branches(dend, k = 3) # Color branches by clusters

# Plot the enhanced dendrogram
plot(dend)

set.seed(123)  # for reproducibility
data_sample <- data_clustred[sample(1:nrow(data_clustred), 1000), ]  # Adjust the number based on your data size and memory capacity.
clusters <- cutreeDynamic(dend = hclust_result,method = "tree")




# Add clusters to data
data_clustred$hcluster_dynamic <- as.factor(clusters)


# Add clusters to data
data_clustred$hcluster_dynamic <- as.factor(clusters)
data_clustered_orig<- data
# Convert percentage column to numeric
data_clustered_orig$Final.Discount.Offered <- as.numeric(gsub("%", "", data_clustered_orig$Final.Discount.Offered)) / 100
data_clustered_orig$cluster <- data_clustred$hcluster_dynamic
barplot(table(data_clustered_orig$cluster), main="cluster Distribution", xlab="Cluster", ylab="Frequency")
library(ggplot2)


ggplot(data_clustered_orig, aes(x = cluster)) + 
  geom_bar(fill = "steelblue") +
  labs(title = "Cluster Distribution", x = "Cluster", y = "Frequency") +
  theme_minimal()

ggplot(data_clustered_orig, aes(x = Number.OF.Follow.Up, y = ENQUIRY.START.TO.END.IN.DAYS, color = cluster)) +
  geom_point(alpha = 0.6, size = 3) +
  labs(title = "Visualization of Clusters", x = "Number of Follow Up", y = "Enquiry Duration (Days)") +
  theme_minimal()

