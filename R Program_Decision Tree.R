# Step 1
week3 <- read.csv(file.choose())
str(week3)
summary(week3)
head(week3)

# Data Preparation
copy_week3=week3
library(rpart)
library(rpart.plot)
library(ROCR)
tree_depth=rpart.control(maxdepth = 10)

# Step 2
# Gini
Gtree_week3=rpart(data=copy_week3,TARGET_BAD_FLAG~.-TARGET_LOSS_AMT,control=tree_depth,method="class",parms=list(split='gini'))
rpart.plot(Gtree_week3)
Gtree_week3$variable.importance

# Entropy
Etree_week3=rpart(data=copy_week3,TARGET_BAD_FLAG~.-TARGET_LOSS_AMT,control=tree_depth,method="class",parms=list(split='information'))
rpart.plot(Etree_week3)
Etree_week3$variable.importance

# ROC
PG= predict(Gtree_week3,copy_week3)
dfPG= as.data.frame(PG)
PG2 = prediction(PG[,2], copy_week3$TARGET_BAD_FLAG)
PG3 = performance(PG2,"tpr","fpr")

PE= predict(Etree_week3,copy_week3)
dfPE=as.data.frame(PE)
PE2=prediction(PE[,2],copy_week3$TARGET_BAD_FLAG)
PE3=performance(PE2,"tpr","fpr")

plot(PG3,col='red')
plot(PE3,col='blue', add=TRUE)
abline(0,1)
legend("bottomright", c("Gini","Entropy"),col=c("red","blue"),bty ="y",lty = 1)
# Summary
# Gini and Entropy showcased similar ROC curve, with Gini displayed slightly larger area under the curve. Hence, Gini is the better solution in this case.
# People has lower debt to income ratio, less delayed payment, and longer insurance history tends to default on good loan.

# Step 3

# Anova
TreeAnova=rpart(data=copy_week3,TARGET_LOSS_AMT~.-TARGET_BAD_FLAG,control = tree_depth, method = "anova")
rpart.plot(TreeAnova)
TreeAnova$variable.importance
P_A=predict(TreeAnova,copy_week3)
RMSE_A=sqrt(mean((copy_week3$TARGET_LOSS_AMT-P_A)^2))
print(RMSE_A)

# Poisson
TreePoisson=rpart(data=copy_week3,TARGET_LOSS_AMT~.-TARGET_BAD_FLAG,control = tree_depth, method = "poisson")
rpart.plot(TreePoisson)
TreePoisson$variable.importance
P_P=predict(TreePoisson,copy_week3)
RMSE_P=sqrt(mean((copy_week3$TARGET_LOSS_AMT-P_P)^2))
print(RMSE_P)

# Summary: Based on the RMSE, the ANOVA method is more accurate as it has less RMSE which means the predictions are closer to the results. 
# The debt to income existence, ratio, and loan amount are the top 3 factors  for both methods.

# Step 4
# Predict Bad Flag
Tree_Badflag=rpart(data=copy_week3,TARGET_BAD_FLAG~.-TARGET_LOSS_AMT,control=tree_depth)
rpart.plot(Tree_Badflag)
P_Badflag=predict(Tree_Badflag,copy_week3)
# Predict only when bad flag
week3_subset=subset(week3,TARGET_BAD_FLAG==1)
Tree_TrueBadFlag=rpart(data = week3_subset,TARGET_LOSS_AMT~.-TARGET_BAD_FLAG,control = tree_depth, method = "poisson")
rpart.plot(Tree_TrueBadFlag)
P_TrueBadFlag=predict(Tree_TrueBadFlag,copy_week3)
# Multiply two results
P_Multiply = P_Badflag*P_TrueBadFlag
RMSE_Multiply=sqrt(mean((copy_week3$TARGET_LOSS_AMT-P_Multiply)^2))
print(RMSE_Multiply)
# Summary: Compare to the methods of Step 3, the combined model has a lower RMSE which means less error.

