# Myopia Probability Analysis


# Calculating-the-probablity-of-MYPIC-depending-upon-SPHEQ
#	Based on the R output below we have 618 observations with 18 variables.

data=read.csv("C:\\Users\\aryalg\\Purdue-Northwest\\Fall2019\\STAT 40001\\Test\\Myopic.csv")
head(data)
# ID STUDYYEAR MYOPIC AGE GENDER  SPHEQ    AL   ACD    LT   VCD SPORTHR READHR COMPHR STUDYHR TVHR DIOPTERHR MOMMY DADMY
# 1  1      1992      1   6      1 -0.052 21.89 3.690 3.498 14.70      45      8      0       0   10        34     1     1
# 2  2      1995      0   6      1  0.608 22.38 3.702 3.392 15.29       4      0      1       1    7        12     1     1
# 3  3      1991      0   6      1  1.179 22.49 3.462 3.514 15.52      14      0      2       0   10        14     0     0
# 4  4      1990      1   6      1  0.525 22.20 3.862 3.612 14.73      18     11      0       0    4        37     0     1
# 5  5      1995      0   5      0  0.697 23.29 3.676 3.454 16.16      14      0      0       0    4         4     1     0
# 6  6      1995      0   6      0  1.744 22.14 3.224 3.556 15.36      10      6      2       1   19        44     0     1

dim(data)
# [1] 618  18


attach(data)
plot(SPHEQ, MYOPIC, col=2, pch=19)

#Based on the R output below the fitted logistic regression model is

π ̂=[1+expa(-0.05397+3.8331×SPHEQ)]^(-1)

model=glm(MYOPIC~SPHEQ, family="binomial")
model

Call:  glm(formula = MYOPIC ~ SPHEQ, family = "binomial")

# Coefficients:
# (Intercept)        SPHEQ
# 0.05397     -3.83310

# Degrees of Freedom: 617 Total (i.e. Null);  616 Residual



# Null Deviance:      480.1
# Residual Deviance: 337.3        AIC: 341.3


#	We use R code below to display the probability curve
plot(SPHEQ, MYOPIC, col=2, pch=19)
curve(predict(model, data.frame(SPHEQ=x), type="resp"), add=TRUE)
points(SPHEQ, fitted(model), pch=20)

# OR

library(popbio)
logi.hist.plot(SPHEQ,MYOPIC, type="hist", col="purple")




predict(model, data.frame(SPHEQ=0.525), type="resp")
# 1
# 0.12364
#For a person with SPHEQ=0.525, the estimated probability that the person has myopia is
1-0.12364=0.87636.

#Note that in the given dataset MYOPIC is the variable that takes value 1 if the subject doesn’t have myopia and 0 if the subject has myopia. So we have to change the response of MYOPIC as we are interested to find the probability of having myopic.
model=glm(y1~x, family="binomial") # y1=new response variable which take 1 if the subject have myopia and 0 if the subject  doesn’t have myopia.

model

Call:  glm(formula = y1 ~ x, family = "binomial")

# Coefficients:
# (Intercept)            x
# -0.05397      3.83310













