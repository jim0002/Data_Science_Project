#Final Project
#install.packages("factoextra")
library(factoextra)

Dataset1 <- read.csv("F://vehicle.csv", header = TRUE, sep = ",")
Dataset1
names(Dataset1)
str(Dataset1)
summary(Dataset1)
attributes <- names(Dataset1)
head(Dataset1)
colSums(is.na(Dataset1))

Dataset1_omit <- na.omit(Dataset1)
Dataset1_omit

z_scores <- (Dataset1_omit$compactness - mean(Dataset1_omit$compactness)) / sd(Dataset1_omit$compactness)
outliers <- abs(z_scores) > 3
Dataset1_omit$compactness[outliers]
Dataset1_omit$compactness[outliers] <- NA


z_scores <- (Dataset1_omit$circularity - mean(Dataset1_omit$circularity)) / sd(Dataset1_omit$circularity)
outliers <- abs(z_scores) > 3
Dataset1_omit$circularity[outliers]
Dataset1_omit$circularity[outliers] <- NA

z_scores <- (Dataset1_omit$distance_circularity - mean(Dataset1_omit$distance_circularity)) / sd(Dataset1_omit$distance_circularity)
outliers <- abs(z_scores) > 3
Dataset1_omit$distance_circularity[outliers]
Dataset1_omit$distance_circularity[outliers] <- NA


z_scores <- (Dataset1_omit$radius_ratio - mean(Dataset1_omit$radius_ratio)) / sd(Dataset1_omit$radius_ratio)
outliers <- abs(z_scores) > 3
Dataset1_omit$radius_ratio[outliers]
Dataset1_omit$radius_ratio[outliers] <- NA


z_scores <- (Dataset1_omit$pr.axis_aspect_ratio - mean(Dataset1_omit$pr.axis_aspect_ratio)) / sd(Dataset1_omit$pr.axis_aspect_ratio)
outliers <- abs(z_scores) > 3
Dataset1_omit$pr.axis_aspect_ratio[outliers]
Dataset1_omit$pr.axis_aspect_ratio[outliers] <- NA


z_scores <- (Dataset1_omit$max.length_aspect_ratio - mean(Dataset1_omit$max.length_aspect_ratio)) / sd(Dataset1_omit$max.length_aspect_ratio)
outliers <- abs(z_scores) > 3
Dataset1_omit$max.length_aspect_ratio[outliers]
Dataset1_omit$max.length_aspect_ratio[outliers] <- NA


z_scores <- (Dataset1_omit$scatter_ratio - mean(Dataset1_omit$scatter_ratio)) / sd(Dataset1_omit$scatter_ratio)
outliers <- abs(z_scores) > 3
Dataset1_omit$scatter_ratio[outliers]
Dataset1_omit$scatter_ratio[outliers] <- NA


z_scores <- (Dataset1_omit$elongatedness - mean(Dataset1_omit$elongatedness)) / sd(Dataset1_omit$elongatedness)
outliers <- abs(z_scores) > 3
Dataset1_omit$elongatedness[outliers]
Dataset1_omit$elongatedness[outliers] <- NA


z_scores <- (Dataset1_omit$pr.axis_rectangularity - mean(Dataset1_omit$pr.axis_rectangularity)) / sd(Dataset1_omit$pr.axis_rectangularity)
outliers <- abs(z_scores) > 3
Dataset1_omit$pr.axis_rectangularity[outliers]
Dataset1_omit$pr.axis_rectangularity[outliers] <- NA



z_scores <- (Dataset1_omit$max.length_rectangularity - mean(Dataset1_omit$max.length_rectangularity)) / sd(Dataset1_omit$max.length_rectangularity)
outliers <- abs(z_scores) > 3
Dataset1_omit$max.length_rectangularity[outliers]
Dataset1_omit$max.length_rectangularity[outliers] <- NA


z_scores <- (Dataset1_omit$scaled_variance - mean(Dataset1_omit$scaled_variance)) / sd(Dataset1_omit$scaled_variance)
outliers <- abs(z_scores) > 3
Dataset1_omit$scaled_variance[outliers]
Dataset1_omit$scaled_variance[outliers] <- NA


z_scores <- (Dataset1_omit$scaled_variance.1 - mean(Dataset1_omit$scaled_variance.1)) / sd(Dataset1_omit$scaled_variance.1)
outliers <- abs(z_scores) > 3
Dataset1_omit$scaled_variance.1[outliers]
Dataset1_omit$scaled_variance.1[outliers] <- NA


z_scores <- (Dataset1_omit$scaled_radius_of_gyration - mean(Dataset1_omit$scaled_radius_of_gyration)) / sd(Dataset1_omit$scaled_radius_of_gyration)
outliers <- abs(z_scores) > 3
Dataset1_omit$scaled_radius_of_gyration[outliers]
Dataset1_omit$scaled_radius_of_gyration[outliers] <- NA


z_scores <- (Dataset1_omit$scaled_radius_of_gyration.1 - mean(Dataset1_omit$scaled_radius_of_gyration.1)) / sd(Dataset1_omit$scaled_radius_of_gyration.1)
outliers <- abs(z_scores) > 3
Dataset1_omit$scaled_radius_of_gyration.1[outliers]
Dataset1_omit$scaled_radius_of_gyration.1[outliers] <- NA


z_scores <- (Dataset1_omit$skewness_about - mean(Dataset1_omit$skewness_about)) / sd(Dataset1_omit$skewness_about)
outliers <- abs(z_scores) > 3
Dataset1_omit$skewness_about[outliers]
Dataset1_omit$skewness_about[outliers] <- NA


z_scores <- (Dataset1_omit$skewness_about.1 - mean(Dataset1_omit$skewness_about.1)) / sd(Dataset1_omit$skewness_about.1)
outliers <- abs(z_scores) > 3
Dataset1_omit$skewness_about.1[outliers]
Dataset1_omit$skewness_about.1[outliers] <- NA


z_scores <- (Dataset1_omit$skewness_about.2 - mean(Dataset1_omit$skewness_about.2)) / sd(Dataset1_omit$skewness_about.2)
outliers <- abs(z_scores) > 3
Dataset1_omit$skewness_about.2[outliers]
Dataset1_omit$skewness_about.2[outliers] <- NA



z_scores <- (Dataset1_omit$hollows_ratio - mean(Dataset1_omit$hollows_ratio)) / sd(Dataset1_omit$hollows_ratio)
outliers <- abs(z_scores) > 3
Dataset1_omit$hollows_ratio[outliers]
Dataset1_omit$hollows_ratio[outliers] <- NA


Dataset1_omit1 <- na.omit(Dataset1_omit)
Dataset1_omit1


hist(Dataset1_omit1$compactness,
     col="Sky Blue",
     main="Histogram for value counts of compactness",
     xlab="compactness",
     ylab="Frequency",
     labels=TRUE)


hist(Dataset1_omit1$circularity,
     col="Sky Blue",
     main="Histogram for value counts of circularity",
     xlab="circularity",
     ylab="Frequency",
     labels=TRUE)


hist(Dataset1_omit1$distance_circularity,
     col="Sky Blue",
     main="Histogram for value counts of distance_circularity",
     xlab="distance_circularity",
     ylab="Frequency",
     labels=TRUE)


hist(Dataset1_omit1$radius_ratio,
     col="Sky Blue",
     main="Histogram for value counts of radius_ratio",
     xlab="radius_ratio",
     ylab="Frequency",
     labels=TRUE)


hist(Dataset1_omit1$pr.axis_aspect_ratio,
     col="Sky Blue",
     main="Histogram for value counts of pr.axis_aspect_ratio",
     xlab="pr.axis_aspect_ratio",
     ylab="Frequency",
     labels=TRUE)


hist(Dataset1_omit1$max.length_aspect_ratio,
     col="Sky Blue",
     main="Histogram for value counts of max.length_aspect_ratio",
     xlab="max.length_aspect_ratio",
     ylab="Frequency",
     labels=TRUE)


hist(Dataset1_omit1$scatter_ratio,
     col="Sky Blue",
     main="Histogram for value counts of scatter_ratio",
     xlab="scatter_ratio",
     ylab="Frequency",
     labels=TRUE)


hist(Dataset1_omit1$elongatedness,
     col="Sky Blue",
     main="Histogram for value counts of elongatedness",
     xlab="elongatedness",
     ylab="Frequency",
     labels=TRUE)


hist(Dataset1_omit1$pr.axis_rectangularity,
     col="Sky Blue",
     main="Histogram for value counts of pr.axis_rectangularity",
     xlab="pr.axis_rectangularity",
     ylab="Frequency",
     labels=TRUE)


hist(Dataset1_omit1$max.length_rectangularity,
     col="Sky Blue",
     main="Histogram for value counts of max.length_rectangularity",
     xlab="max.length_rectangularity",
     ylab="Frequency",
     labels=TRUE)


hist(Dataset1_omit1$scaled_variance,
     col="Sky Blue",
     main="Histogram for value counts of scaled_variance",
     xlab="scaled_variance",
     ylab="Frequency",
     labels=TRUE)


hist(Dataset1_omit1$scaled_variance.1,
     col="SKy Blue",
     main="Histogram for value counts of scaled_variance.1",
     xlab="scaled_variance.1",
     ylab="Frequency",
     labels=TRUE)


hist(Dataset1_omit1$scaled_radius_of_gyration,
     col="Sky Blue",
     main="Histogram for value counts of scaled_radius_of_gyration",
     xlab="scaled_radius_of_gyration",
     ylab="Frequency",
     labels=TRUE)


hist(Dataset1_omit1$scaled_radius_of_gyration.1,
     col="Sky Blue",
     main="Histogram for value counts of scaled_radius_of_gyration.1",
     xlab="scaled_radius_of_gyration.1",
     ylab="Frequency",
     labels=TRUE)


hist(Dataset1_omit1$skewness_about,
     col="Sky Blue",
     main="Histogram for value counts of skewness_about",
     xlab="skewness_about",
     ylab="Frequency",
     labels=TRUE)


hist(Dataset1_omit1$skewness_about.1,
     col="Sky Blue",
     main="Histogram for value counts of .1",
     xlab="skewness_about.1",
     ylab="Frequency",
     labels=TRUE)


hist(Dataset1_omit1$skewness_about.2,
     col="Sky Blue",
     main="Histogram for value counts of hollows_ratio",
     xlab="skewness_about.2",
     ylab="Frequency",
     labels=TRUE)


hist(Dataset1_omit1$hollows_ratio,
     col="Sky Blue",
     main="Histogram for value counts of hollows_ratio",
     xlab="hollows_ratio",
     ylab="Frequency",
     labels=TRUE)



median_of_compactness <- median(Dataset1_omit1$compactness)
median_of_circularity <- median(Dataset1_omit1$circularity)
median_of_distance_circularity <- median(Dataset1_omit1$distance_circularity)
median_of_radius_ratio <- median(Dataset1_omit1$radius_ratio)
median_of_pr.axis_aspect_ratio <- median(Dataset1_omit1$pr.axis_aspect_ratio)
median_of_max.length_aspect_ratio <- median(Dataset1_omit1$max.length_aspect_ratio)
median_of_scatter_ratio <- median(Dataset1_omit1$scatter_ratio)
median_of_elongatedness <- median(Dataset1_omit1$elongatedness)
median_of_pr.axis_rectangularity <- median(Dataset1_omit1$pr.axis_rectangularity)
median_of_max.length_rectangularity <- median(Dataset1_omit1$max.length_rectangularity)
median_of_scaled_variance <- median(Dataset1_omit1$scaled_variance)
median_of_scaled_variance.1 <- median(Dataset1_omit1$scaled_variance.1)
median_of_scaled_radius_of_gyration <- median(Dataset1_omit1$scaled_radius_of_gyration)
median_of_scaled_radius_of_gyration.1 <- median(Dataset1_omit1$scaled_radius_of_gyration.1)
median_of_skewness_about <- median(Dataset1_omit1$skewness_about)
median_of_skewness_about.1 <- median(Dataset1_omit1$skewness_about.1)
median_of_skewness_about.2 <- median(Dataset1_omit1$skewness_about.2)
median_of_hollows_ratio <- median(Dataset1_omit1$hollows_ratio)

median_of_all <- data.frame(median_of_compactness, median_of_circularity, median_of_distance_circularity, median_of_radius_ratio, median_of_pr.axis_aspect_ratio, 
                            median_of_max.length_aspect_ratio, median_of_scatter_ratio, median_of_elongatedness, median_of_pr.axis_rectangularity, 
                            median_of_max.length_rectangularity, median_of_scaled_variance, 
                            median_of_scaled_variance.1, median_of_scaled_radius_of_gyration, median_of_scaled_radius_of_gyration.1, 
                            median_of_skewness_about, median_of_skewness_about.1, median_of_skewness_about.2,
                            median_of_hollows_ratio)
median_of_all

colSums(is.na(Dataset1))

recover_missing_with_median <- Dataset1
recover_missing_with_median$compactness[is.na(recover_missing_with_median$compactness)] <- median_of_compactness
recover_missing_with_median$circularity [is.na(recover_missing_with_median$circularity)] <- median_of_circularity
recover_missing_with_median$distance_circularity[is.na(recover_missing_with_median$distance_circularity)] <- median_of_distance_circularity
recover_missing_with_median$radius_ratio[is.na(recover_missing_with_median$radius_ratio)] <- median_of_radius_ratio
recover_missing_with_median$pr.axis_aspect_ratio[is.na(recover_missing_with_median$pr.axis_aspect_ratio)] <- median_of_pr.axis_aspect_ratio
recover_missing_with_median$max.length_aspect_ratio[is.na(recover_missing_with_median$max.length_aspect_ratio)] <- median_of_max.length_aspect_ratio
recover_missing_with_median$scatter_ratio[is.na(recover_missing_with_median$scatter_ratio)] <- median_of_scatter_ratio
recover_missing_with_median$elongatedness [is.na(recover_missing_with_median$elongatedness)] <- median_of_elongatedness
recover_missing_with_median$pr.axis_rectangularity[is.na(recover_missing_with_median$pr.axis_rectangularity)] <- median_of_pr.axis_rectangularity
recover_missing_with_median$max.length_rectangularity[is.na(recover_missing_with_median$max.length_rectangularity)] <- median_of_max.length_rectangularity
recover_missing_with_median$scaled_variance[is.na(recover_missing_with_median$scaled_variance)] <- median_of_scaled_variance
recover_missing_with_median$scaled_variance.1[is.na(recover_missing_with_median$scaled_variance.1)] <- median_of_scaled_variance.1
recover_missing_with_median$scaled_radius_of_gyration[is.na(recover_missing_with_median$scaled_radius_of_gyration)] <- median_of_scaled_radius_of_gyration
recover_missing_with_median$scaled_radius_of_gyration.1[is.na(recover_missing_with_median$scaled_radius_of_gyration.1)] <- median_of_scaled_radius_of_gyration.1
recover_missing_with_median$skewness_about[is.na(recover_missing_with_median$skewness_about)] <- median_of_skewness_about
recover_missing_with_median$skewness_about.1 [is.na(recover_missing_with_median$skewness_about.1)] <- median_of_skewness_about.1
recover_missing_with_median$skewness_about.2[is.na(recover_missing_with_median$skewness_about.2)] <- median_of_skewness_about.2
recover_missing_with_median$hollows_ratio[is.na(recover_missing_with_median$hollows_ratio)] <- median_of_hollows_ratio

recover_missing_with_median
colSums(is.na(recover_missing_with_median))


z_scores <- (recover_missing_with_median$compactness - mean(recover_missing_with_median$compactness)) / sd(recover_missing_with_median$compactness)
outliers <- abs(z_scores) > 3
recover_missing_with_median$compactness[outliers]
recover_missing_with_median$compactness[outliers] <- NA


z_scores <- (recover_missing_with_median$circularity - mean(recover_missing_with_median$circularity)) / sd(recover_missing_with_median$circularity)
outliers <- abs(z_scores) > 3
recover_missing_with_median$circularity[outliers]
recover_missing_with_median$circularity[outliers] <- NA

z_scores <- (recover_missing_with_median$distance_circularity - mean(recover_missing_with_median$distance_circularity)) / sd(recover_missing_with_median$distance_circularity)
outliers <- abs(z_scores) > 3
recover_missing_with_median$distance_circularity[outliers]
recover_missing_with_median$distance_circularity[outliers] <- NA


z_scores <- (recover_missing_with_median$radius_ratio - mean(recover_missing_with_median$radius_ratio)) / sd(recover_missing_with_median$radius_ratio)
outliers <- abs(z_scores) > 3
recover_missing_with_median$radius_ratio[outliers]
recover_missing_with_median$radius_ratio[outliers] <- NA


z_scores <- (recover_missing_with_median$pr.axis_aspect_ratio - mean(recover_missing_with_median$pr.axis_aspect_ratio)) / sd(recover_missing_with_median$pr.axis_aspect_ratio)
outliers <- abs(z_scores) > 3
recover_missing_with_median$pr.axis_aspect_ratio[outliers]
recover_missing_with_median$pr.axis_aspect_ratio[outliers] <- NA


z_scores <- (recover_missing_with_median$max.length_aspect_ratio - mean(recover_missing_with_median$max.length_aspect_ratio)) / sd(recover_missing_with_median$max.length_aspect_ratio)
outliers <- abs(z_scores) > 3
recover_missing_with_median$max.length_aspect_ratio[outliers]
recover_missing_with_median$max.length_aspect_ratio[outliers] <- NA


z_scores <- (recover_missing_with_median$scatter_ratio - mean(recover_missing_with_median$scatter_ratio)) / sd(recover_missing_with_median$scatter_ratio)
outliers <- abs(z_scores) > 3
recover_missing_with_median$scatter_ratio[outliers]
recover_missing_with_median$scatter_ratio[outliers] <- NA


z_scores <- (recover_missing_with_median$elongatedness - mean(recover_missing_with_median$elongatedness)) / sd(recover_missing_with_median$elongatedness)
outliers <- abs(z_scores) > 3
recover_missing_with_median$elongatedness[outliers]
recover_missing_with_median$elongatedness[outliers] <- NA


z_scores <- (recover_missing_with_median$pr.axis_rectangularity - mean(recover_missing_with_median$pr.axis_rectangularity)) / sd(recover_missing_with_median$pr.axis_rectangularity)
outliers <- abs(z_scores) > 3
recover_missing_with_median$pr.axis_rectangularity[outliers]
recover_missing_with_median$pr.axis_rectangularity[outliers] <- NA



z_scores <- (recover_missing_with_median$max.length_rectangularity - mean(recover_missing_with_median$max.length_rectangularity)) / sd(recover_missing_with_median$max.length_rectangularity)
outliers <- abs(z_scores) > 3
recover_missing_with_median$max.length_rectangularity[outliers]
recover_missing_with_median$max.length_rectangularity[outliers] <- NA


z_scores <- (recover_missing_with_median$scaled_variance - mean(recover_missing_with_median$scaled_variance)) / sd(recover_missing_with_median$scaled_variance)
outliers <- abs(z_scores) > 3
recover_missing_with_median$scaled_variance[outliers]
recover_missing_with_median$scaled_variance[outliers] <- NA


z_scores <- (recover_missing_with_median$scaled_variance.1 - mean(recover_missing_with_median$scaled_variance.1)) / sd(recover_missing_with_median$scaled_variance.1)
outliers <- abs(z_scores) > 3
recover_missing_with_median$scaled_variance.1[outliers]
recover_missing_with_median$scaled_variance.1[outliers] <- NA


z_scores <- (recover_missing_with_median$scaled_radius_of_gyration - mean(recover_missing_with_median$scaled_radius_of_gyration)) / sd(recover_missing_with_median$scaled_radius_of_gyration)
outliers <- abs(z_scores) > 3
recover_missing_with_median$scaled_radius_of_gyration[outliers]
recover_missing_with_median$scaled_radius_of_gyration[outliers] <- NA


z_scores <- (recover_missing_with_median$scaled_radius_of_gyration.1 - mean(recover_missing_with_median$scaled_radius_of_gyration.1)) / sd(recover_missing_with_median$scaled_radius_of_gyration.1)
outliers <- abs(z_scores) > 3
recover_missing_with_median$scaled_radius_of_gyration.1[outliers]
recover_missing_with_median$scaled_radius_of_gyration.1[outliers] <- NA


z_scores <- (recover_missing_with_median$skewness_about - mean(recover_missing_with_median$skewness_about)) / sd(recover_missing_with_median$skewness_about)
outliers <- abs(z_scores) > 3
recover_missing_with_median$skewness_about[outliers]
recover_missing_with_median$skewness_about[outliers] <- NA


z_scores <- (recover_missing_with_median$skewness_about.1 - mean(recover_missing_with_median$skewness_about.1)) / sd(recover_missing_with_median$skewness_about.1)
outliers <- abs(z_scores) > 3
recover_missing_with_median$skewness_about.1[outliers]
recover_missing_with_median$skewness_about.1[outliers] <- NA


z_scores <- (recover_missing_with_median$skewness_about.2 - mean(recover_missing_with_median$skewness_about.2)) / sd(recover_missing_with_median$skewness_about.2)
outliers <- abs(z_scores) > 3
recover_missing_with_median$skewness_about.2[outliers]
recover_missing_with_median$skewness_about.2[outliers] <- NA



z_scores <- (recover_missing_with_median$hollows_ratio - mean(recover_missing_with_median$hollows_ratio)) / sd(recover_missing_with_median$hollows_ratio)
outliers <- abs(z_scores) > 3
recover_missing_with_median$hollows_ratio[outliers]
recover_missing_with_median$hollows_ratio[outliers] <- NA


Dataset1_omit2 <- na.omit(recover_missing_with_median)
Dataset1_omit2


Dataset1_omit2.labels = Dataset1_omit2$class
table(Dataset1_omit2.labels)
Dataset1_omit2_data <- Dataset1_omit2[1:18]


Dataset1_omit2_data_scale <- scale(Dataset1_omit2_data)

Dataset1_omit2_data <- dist(Dataset1_omit2_data_scale)


fviz_nbclust(Dataset1_omit2_data_scale, kmeans, method = "wss")+
  labs(subtitle="Elbow Method")


km.out <- kmeans(Dataset1_omit2_data_scale, centers=3,nstart=100)
print(km.out)


km.clusters<-km.out$cluster
rownames(Dataset1_omit2_data_scale)<-paste(Dataset1_omit2$class, 1:dim(Dataset1_omit2)[1], sep = "_")
fviz_cluster(list(data=Dataset1_omit2_data_scale, cluster = km.clusters))

table(km.clusters, Dataset1_omit2$class)
