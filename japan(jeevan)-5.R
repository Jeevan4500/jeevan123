rm(list = ls())

#setwd("E://Documents//Jeevan(japan)")

japan <- read.csv("Japan earthquakes 2001 - 2018.csv", header = TRUE)

head(japan)
getwd()
str(japan)

dim(japan)

summary(japan)

head(japan, 2)

install.packages("ggpubr")

required_packages <- c("ggplot2", "dplyr", "tidyr", "corrplot")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  } else {
    library(pkg, character.only = TRUE)
  }
}

print("All required packages are successfully installed and loaded.")

library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)




# Question-1(A):

# Scatter plot with linear trendline
ggplot(japan, aes(x = mag, y = depth)) +
  geom_point(color = "blue", alpha = 0.5) + 
  geom_smooth(method = "lm", color = "red", se = FALSE) + 
  labs(
    title = "Scatter Plot of Earthquake Magnitude vs. Depth",
    x = "Magnitude",
    y = "Depth (km)"
  ) +
  theme_minimal()





# Question-1(B):


japan$mag_category <- cut(
  japan$mag,
  breaks = c(-Inf, 4, 6, Inf),
  labels = c("Low", "Medium", "High")
)


# Boxplot for depth by magnitude categories
ggplot(japan, aes(x = mag_category, y = depth)) +
  geom_boxplot(outlier.color = "red", fill = "lightblue") +
  labs(
    title = "Boxplot of Earthquake Depth by Magnitude Categories",
    x = "Magnitude Category",
    y = "Depth (km)"
  ) +
  theme_minimal()



# Question-2 (A):

japan$mag_category <- cut(
  japan$mag,
  breaks = c(-Inf, 4, 6, Inf),
  labels = c("Low", "Medium", "High")
)


ggplot(japan, aes(x = mag_category, y = depth)) +
  geom_boxplot(outlier.color = "red", fill = "lightblue") +
  labs(
    title = "Boxplot of Earthquake Depth by Magnitude Categories",
    x = "Magnitude Category",
    y = "Depth (km)"
  ) +
  theme_minimal()


# Question-2 (B):

# Histogram of depth with normal curve overlay
ggplot(japan, aes(x = depth)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black", alpha = 0.6) +
  stat_function(fun = dnorm, args = list(mean = mean(japan$depth, na.rm = TRUE), sd = sd(japan$depth, na.rm = TRUE)), color = "red", size = 1) +
  labs(
    title = "Histogram of Earthquake Depth with Normal Curve Overlay",
    x = "Depth (km)",
    y = "Density"
  ) +
  theme_minimal()






# Question-3

library(ggplot2)
library(dplyr)

japan <- japan %>%
  mutate(
    depth_category = case_when(
      depth < 70 ~ "Shallow",
      depth >= 70 & depth <= 300 ~ "Intermediate",
      depth > 300 ~ "Deep"
    )
  )

proportion_data <- japan %>%
  group_by(mag, depth_category) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) 

# Create the normalized stacked bar chart
ggplot(proportion_data, aes(x = mag, y = percentage, fill = depth_category)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Proportion of Earthquakes by Depth Level and Magnitude Category",
    x = "Magnitude Category",
    y = "Percentage of Earthquakes (%)",
    fill = "Depth Level"
  ) +
  theme_minimal()





# Question-4 (a):

install.packages("ggpubr")

library(ggplot2)
library(ggpubr)
library(dplyr)

# Perform Spearman's correlation test
cor_test_result <- cor.test(japan$depth, japan$mag, method = "spearman")

cat("Spearman's Correlation Test Results (Depth -> Magnitude):\n")
cat("Correlation Coefficient (rho):", cor_test_result$estimate, "\n")
cat("p-value:", cor_test_result$p.value, "\n")

# Interpretation based on the p-value
alpha <- 0.05
if (cor_test_result$p.value < alpha) {
  cat("The result is significant. Depth influences magnitude.\n")
} else {
  cat("The result is not significant. Depth does not significantly influence magnitude.\n")
}






# Question-4 (b):

library(corrplot)

# Select numerical columns
num_vars <- japan %>%
  select(latitude, longitude, depth, mag, gap)

# Correlation matrix
cor_matrix <- cor(num_vars, use = "complete.obs")

corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black")

wilcox_result <- wilcox.test(japan$depth , data = japan, exact = FALSE)

# Display the Wilcoxon test result
print("Wilcoxon (Mann-Whitney U) Test Result:")
print(wilcox_result)




# Question- 5

japan <- japan[!is.na(japan$mag) & !is.na(japan$depth), ]

cor_test_result <- cor.test(japan$mag, japan$depth, method = "pearson")

cat("Correlation Coefficient (r):", cor_test_result$estimate, "\n")
cat("Test Statistic (t-value):", cor_test_result$statistic, "\n")
cat("p-value:", cor_test_result$p.value, "\n")

# Interpret the result
alpha <- 0.05  
if (cor_test_result$p.value < alpha) {
  cat("The result is significant. Reject the null hypothesis.\n")
} else {
  cat("The result is not significant. Fail to reject the null hypothesis.\n")
}

cat("Interpretation: If the null hypothesis is rejected, it suggests that there is a significant correlation between earthquake magnitude and depth. This finding could provide insights into seismic patterns and their potential impact on earthquake prediction models.\n")



# Combine sparse categories of 'mag'
japan$mag_cat <- cut(japan$mag, 
                     breaks = c(-Inf, 4.5, 5.5, Inf), 
                     labels = c("Low-Moderate", "Moderate-High", "High"))

# Combine sparse categories of 'depth'
japan$depth_cat <- cut(japan$depth, 
                       breaks = c(-Inf, 100, 300, Inf), 
                       labels = c("Shallow", "Intermediate", "Deep"))

# Create the contingency table
contingency_table <- table(japan$mag_cat, japan$depth_cat)

# Check the contingency table
print("Contingency Table:")
print(contingency_table)

# Perform the Chi-square test
chi_square_result <- chisq.test(contingency_table)
print("Chi-Square Test Results:")
print(chi_square_result)



# Scatter plot with linear regression line
ggplot(japan, aes(x = mag, y = depth)) +
  geom_point(color = "blue", alpha = 0.5) + 
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Scatter Plot of Earthquake Magnitude vs. Depth",
    subtitle = paste("Correlation Coefficient (r):", round(cor_test_result$estimate, 2)),
    x = "Magnitude",
    y = "Depth (km)"
  ) +
  theme_minimal()

# Save the plot
ggsave("Magnitude_vs_Depth_Correlation.png")

