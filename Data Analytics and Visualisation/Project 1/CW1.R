spend <- rnorm(500,105,15.75)
qtime <- rpois(500,7)
famsize <- rpois(500,4)
store <- sample(1:3, 500, replace = "True")
shoptype <- rbinom (500, 2, 0.14)
income <- rnorm(500, 38000, 4500)

weeklyshop <- data.frame (spend, qtime, famsize, store, shoptype, income)
head(weeklyshop)

#Descriptive Statistics

#Spend

mean_spend <- mean(spend)
median_spend <- median(spend)
sd_spend <- sd(spend) 
range_spend <- range(spend)
iqr_spend <- IQR(spend)

mean_spend 
median_spend 
sd_spend 
range_spend 
iqr_spend 

#qtime

mean_qtime <- mean(qtime)
median_qtime <- median(qtime)
sd_qtime <- sd(qtime)
range_qtime <- range(qtime)
iqr_qtime <- IQR(qtime)

mean_qtime 
median_qtime 
sd_qtime 
range_qtime 
iqr_qtime 

#Income

mean_income <- mean(income)
median_income <- median(income)
sd_income <- sd(income)
range_income <- range(income)
iqr_income <- IQR(income)

mean_income 
median_income 
sd_income 
range_income 
iqr_income 

#Famsize

famsize_freq <- table(famsize)
mode_famsize <- names(famsize_freq)[which.max(famsize_freq)]

famsize_freq 
mode_famsize

#Store

store_freq <- table(store)
mode_store <- names(store_freq)[which.max(store_freq)]

store_freq 
mode_store 

#Shoptype

shoptype_freq <- table(shoptype)
mode_shoptype <- names(shoptype_freq)[which.max(shoptype)]

shoptype_freq
mode_shoptype

#Visualization of single variable

hist(weeklyshop$spend, main ="Histogram of Spend", xlab = "Spend Values", col = "lightblue", border = "black")

#Visualization of double variable

plot(qtime, famsize, main="Family vs Qtime", xlab="qtime", ylab="famsize", pch=16, col="lightgreen")

#Association between Spend and Another Categorical Variable

#t.test 1
t.test(spend~shoptype)

#chisq test 1
chisq_test<- table(spend, shoptype)
chisq.test(chisq_test)

#chisq test 2
chisq_test<- table(spend, store)
chisq.test(chisq_test)


# Chi-squared test for 'Spend' and 'Shoptype'
# Assuming 'Shoptype' is a categorical variable with levels 1 and 2

# Creating a contingency table
contingency_table_Shoptype <- table(weeklyshop$store, cut(weeklyshop$spend, breaks = 3))

# Performing chi-squared test
chi_squared_test_Shoptype <- chisq.test(contingency_table_Shoptype)

#Annova 1
group = factor(spend)
out = aov(shoptype~spend)
out

#Annova 2
group = factor(spend)
out = aov(store~spend)
out

#Correlation 1
cor_var1 <- famsize
cor_var2 <- income

cor_res <- cor(cor_var1, cor_var2)

cor_res

