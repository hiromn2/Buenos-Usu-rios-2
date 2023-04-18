#install.packages(c("ggplot2", "readr",  "dplyr", "fastDummies", "stargazer", "Rcpp", "arrow", "parquetize", "quantreg"))
library(scales)
library(arrow)
library(parquetize)
library(dplyr)
library(readr)
library(quantreg)
library(ggplot2)
library(locfit)
library(stargazer)
library(plm)
library(tidyr)
library(fastDummies)
library(fixest)
library(modelsummary)


data <- read_csv("dataps2s2023.csv")
data <- data %>% drop_na(market)
data <- fastDummies::dummy_cols(data, select_columns = "year")
data$market <- factor(data$market)
data$year <- factor(data$year)

data_2008 <- data %>% filter(year == "2008")
data_2011 <- data %>% filter(year == "2011")



#Question 1

unique(data$market)
names(data) 
#"schoolid","studentid","market","gpa","public","private_voucher","private_nonvoucher",
#"ptje_lect4b_alu", "ptje_mate4b_alu","asiste_mate","asiste_lect","year",              
#"school_inprogram","treat_intesity","nalu"         

data_2007 <- data %>% filter(year == "2007")
unique_data_2007 <- data_2007[!duplicated(data_2007$market),]
#unique_data_2007 <- select(unique_data_2007, c("market", "treat_intesity")) 

unique_data_2007




ggplot(data = unique_data_2007, aes(x= treat_intesity)) +
  geom_histogram(aes(y = ..density.. * 10),colour = 4, fill = "white",bins=10,breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8, 0.9, 1.0)) +
  ggtitle("A. Unweighted") + xlab("% disadvantaged students in municipality") + ylab("Percentage")


#QUESTION 2


# Fit a local polynomial regression of degree 2
data_2008 <- filter(data_2008, private_voucher == TRUE)
x <- data_2008$treat_intesity
y <- data_2008$school_inprogram
fit <- lm(school_inprogram ~ poly(treat_intesity,8), data = data_2008)

# Calculate the predicted values and confidence intervals
pred <- predict(fit, se=T)
pred_df <- data.frame(x=x, y=pred$fit, lower=pred$fit-1.96*pred$se.fit, upper=pred$fit+1.96*pred$se.fit)

pred_df <- pred_df %>% arrange(desc(pred_df))
pred_df <- pred_df[-1,] #PROBLEMATIC ONE DATA POINT

# Create the plot
library(ggplot2)
ggplot(data.frame(x=x, y=y), aes(x=x, y=y)) +
  geom_point() +
  geom_ribbon(data=pred_df, aes(x=x, ymin=lower, ymax=upper), alpha=0.3) +
  geom_line(data=pred_df, aes(x=x, y=y), color="red") +
  labs(x="x", y="y", title="Local Polynomial Regression 2008") +
  theme_bw()

###################################################################################################
###############################################################################################
data_2011 <- filter(data_2011, private_voucher == TRUE)
data_2011 <- filter(data_2011, treat_intesity != TRUE)

x <- data_2011$treat_intesity
y <- data_2011$school_inprogram
fit <- lm(school_inprogram ~ poly(treat_intesity,8), data = data_2011)

# Calculate the predicted values and confidence intervals
pred <- predict(fit, se=T)
pred_df <- data.frame(x=x, y=pred$fit, lower=pred$fit-1.96*pred$se.fit, upper=pred$fit+1.96*pred$se.fit)

pred_df <- pred_df %>% arrange(desc(pred_df))
pred_df <- pred_df[-1,] #PROBLEMATIC ONE DATA POINT

# Create the plot
library(ggplot2)
ggplot(data.frame(x=x, y=y), aes(x=x, y=y)) +
  geom_point() +
  geom_ribbon(data=pred_df, aes(x=x, ymin=lower, ymax=upper), alpha=0.3) +
  geom_line(data=pred_df, aes(x=x, y=y), color="red") +
  labs(x="x", y="y", title="Local Polynomial Regression 2011") +
  theme_bw()



#Question 3
names(data) 
#"schoolid","studentid","market","gpa","public","private_voucher","private_nonvoucher",
#"ptje_lect4b_alu", "ptje_mate4b_alu","asiste_mate","asiste_lect","year",              
#"school_inprogram","treat_intesity","nalu",
#"year_2006","year_2007","year_2008","year_2009","year_2010","year_2011" 

model1 <- feols(ptje_mate4b_alu ~ market + year + year_2005*treat_intesity + 
                  year_2006*treat_intesity  +
                  + year_2008*treat_intesity + year_2009*treat_intesity+ year_2010*treat_intesity +
                  year_2011*treat_intesity|market+year, data = data)

model2 <- feols(ptje_lect4b_alu ~ market + year +year_2005*treat_intesity + year_2006*treat_intesity 
                + year_2008*treat_intesity + year_2009*treat_intesity+ year_2010*treat_intesity + 
                  year_2011*treat_intesity|market+year, data = data)
summary(model2, cluster = c("market"))

modelsummary(model1, output = "Q3.tex")
modelsummary(model2, output = "Q3.1.tex")


#Question 4
a <- model1$coefficients
a <- c(a[1:2], 0, a[3:6])
b <- model1$se
b <- c(b[1:2], 0, b[3:6])
c <- c(2005, 2006, 2007, 2008, 2009, 2010, 2011)
c <- c - 2008
#class(a,b,c)

plot_data <- data.frame(a,b,c)
names(plot_data) <- c("Coefficients", "SE", "Year")

ggplot(plot_data) +
  geom_point(aes(x=Year, y=Coefficients), stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar( aes(x=Year, ymin=Coefficients-SE, ymax=Coefficients+SE), width=0.4, colour="orange", alpha=0.9, size=1.3) +
  geom_vline(xintercept = -0.5) + labs(x = "Period relative to treatement", y = "Test score", title = "Test scores: Math")

a <- model2$coefficients
a <- c(a[1:2], 0, a[3:6])
b <- model2$se
b <- c(b[1:2], 0, b[3:6])
c <- c(2005, 2006, 2007, 2008, 2009, 2010, 2011)
c <- c - 2008
#class(a,b,c)

plot_data <- data.frame(a,b,c)
names(plot_data) <- c("Coefficients", "SE", "Year")

ggplot(plot_data) +
  geom_point(aes(x=Year, y=Coefficients), stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar( aes(x=Year, ymin=Coefficients-SE, ymax=Coefficients+SE), width=0.4, colour="orange", alpha=0.9, size=1.3) +
  geom_vline(xintercept = -0.5) + labs(x = "Period relative to treatement", y = "Test score", title = "Test scores: verbal")


#Question 5
data_public <- filter(data, public == TRUE)

model1_public <- feols(ptje_mate4b_alu ~ market + year + year_2005*treat_intesity + 
                         year_2006*treat_intesity  +
                         + year_2008*treat_intesity + year_2009*treat_intesity+ year_2010*treat_intesity +
                         year_2011*treat_intesity|market+year, data = data_public) 

model2_public <- feols(ptje_lect4b_alu ~ market + year +year_2005*treat_intesity + year_2006*treat_intesity +
                       + year_2008*treat_intesity + year_2009*treat_intesity+ year_2010*treat_intesity + 
                         year_2011*treat_intesity|market+year, data = data_public)

modelsummary(model1_public, output = "Q5.tex")
modelsummary(model2_public, output = "Q5.1.tex")

a <- model1_public$coefficients
a <- c(a[1:2], 0, a[3:6])
b <- model1_public$se
b <- c(b[1:2], 0, b[3:6])
c <- c(2005, 2006, 2007, 2008, 2009, 2010, 2011)
c <- c - 2008


plot_data_public_math <- data.frame(a,b,c)
names(plot_data_public_math) <- c("Coefficients", "SE", "Year")

ggplot(plot_data_public_math) +
  geom_point(aes(x=Year, y=Coefficients), stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar( aes(x=Year, ymin=Coefficients-SE, ymax=Coefficients+SE), width=0.4, colour="orange", alpha=0.9, size=1.3) +
  geom_vline(xintercept = -0.5, linetype="dotted") + labs(x = "Period relative to treatement", y = "Test score", title = "Test scores: Math")

a <- model2_public$coefficients
a <- c(a[1:2], 0, a[3:6])
b <- model2_public$se
b <- c(b[1:2], 0, b[3:6])
c <- c(2005, 2006, 2007, 2008, 2009, 2010, 2011)
c <- c - 2008

plot_data_public_language <- data.frame(a,b,c)
names(plot_data_public_language) <- c("Coefficients", "SE", "Year")

ggplot(plot_data_public_language) +
  geom_point(aes(x=Year, y=Coefficients), stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar( aes(x=Year, ymin=Coefficients-SE, ymax=Coefficients+SE), width=0.4, colour="orange", alpha=0.9, size=1.3) +
  geom_vline(xintercept = -0.5, linetype="dotted") + labs(x = "Period relative to treatement", y = "Test score", title = "Test scores: Verbal")


 # QUESTION 6
data_voucher <- filter(data, private_voucher == TRUE)

model1_voucher <- feols(ptje_mate4b_alu ~ market + year + year_2005*treat_intesity + 
                          year_2006*treat_intesity + 
                          + year_2008*treat_intesity + year_2009*treat_intesity+ year_2010*treat_intesity +
                          year_2011*treat_intesity|market+year, data = data_voucher)

model2_voucher <- feols(ptje_lect4b_alu ~ market + year +year_2005*treat_intesity + year_2006*treat_intesity 
                        + year_2008*treat_intesity + year_2009*treat_intesity+ year_2010*treat_intesity + 
                          year_2011*treat_intesity|market+year, data = data_voucher)

modelsummary(model1_voucher, output = "Q6.tex")
modelsummary(model2_voucher, output = "Q6.1.tex")

a <- model1_voucher$coefficients
a <- c(a[1:2], 0, a[3:6])
b <- model1_voucher$se
b <- c(b[1:2], 0, b[3:6])
c <- c(2005, 2006, 2007, 2008, 2009, 2010, 2011)
c <- c - 2008

plot_data_voucher_math <- data.frame(a,b,c)
names(plot_data_voucher_math) <- c("Coefficients", "SE", "Year")

ggplot(plot_data_voucher_math) +
  geom_point(aes(x=Year, y=Coefficients), stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar( aes(x=Year, ymin=Coefficients-SE, ymax=Coefficients+SE), width=0.4, colour="orange", alpha=0.9, size=1.3) +
  geom_vline(xintercept = -0.5, linetype="dotted") + labs(x = "Period relative to treatement", y = "Test score", title = "Test scores: Math")
  
a <- model2_voucher$coefficients
a <- c(a[1:2], 0, a[3:6])
b <- model2_voucher$se
b <- c(b[1:2], 0, b[3:6])
c <- c(2005, 2006, 2007, 2008, 2009, 2010, 2011)
c <- c - 2008

plot_data_voucher_language <- data.frame(a,b,c)
names(plot_data_voucher_language) <- c("Coefficients", "SE", "Year")

ggplot(plot_data_voucher_language) +
  geom_point(aes(x=Year, y=Coefficients), stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar( aes(x=Year, ymin=Coefficients-SE, ymax=Coefficients+SE), width=0.4, colour="orange", alpha=0.9, size=1.3) +
  geom_vline(xintercept = -0.5, linetype="dotted") + labs(x = "Period relative to treatement", y = "Test score", title = "Test scores: Verbal")



# QUESTION 7

names(data) 
#"schoolid","studentid","market","gpa","public","private_voucher","private_nonvoucher",
#"ptje_lect4b_alu", "ptje_mate4b_alu","asiste_mate","asiste_lect","year",              
#"school_inprogram","treat_intesity","nalu",
#"year_2006","year_2007","year_2008","year_2009","year_2010","year_2011" 

model1 <- feols(asiste_mate ~ market + year + year_2005*treat_intesity + 
                  year_2006*treat_intesity + 
                  + year_2008*treat_intesity + year_2009*treat_intesity+ year_2010*treat_intesity +
                  year_2011*treat_intesity|market+year, data = data)

model2 <- feols(asiste_lect ~ market + year +year_2005*treat_intesity + year_2006*treat_intesity + 
                + year_2008*treat_intesity + year_2009*treat_intesity+ year_2010*treat_intesity + 
                  year_2011*treat_intesity|market+year, data = data)

modelsummary(model1, output = "Q7.tex")
modelsummary(model2, output = "Q7.1.tex")

a <- model1$coefficients
a <- c(a[1:2], 0, a[3:6])
b <- model1$se
b <- c(b[1:2], 0, b[3:6])
c <- c(2005, 2006, 2007, 2008, 2009, 2010, 2011)
c <- c - 2008
#class(a,b,c)

plot_data <- data.frame(a,b,c)
names(plot_data) <- c("Coefficients", "SE", "Year")

ggplot(plot_data) +
  geom_point(aes(x=Year, y=Coefficients), stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar( aes(x=Year, ymin=Coefficients-SE, ymax=Coefficients+SE), width=0.4, colour="orange", alpha=0.9, size=1.3) + 
  geom_vline(xintercept = -0.5, linetype="dotted") + labs(x = "Period relative to treatement", y = "Test score", title = "Taking Math")

a <- model2$coefficients
a <- c(a[1:2], 0, a[3:6])
b <- model2$se
b <- c(b[1:2], 0, b[3:6])
c <- c(2005, 2006, 2007, 2008, 2009, 2010, 2011)
c <- c - 2008
#class(a,b,c)

plot_data <- data.frame(a,b,c)
names(plot_data) <- c("Coefficients", "SE", "Year")

ggplot(plot_data) +
  geom_point(aes(x=Year, y=Coefficients), stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar( aes(x=Year, ymin=Coefficients-SE, ymax=Coefficients+SE), width=0.4, colour="orange", alpha=0.9, size=1.3) +
  geom_vline(xintercept = -0.5, linetype="dotted") + labs(x = "Period relative to treatement", y = "Test score", title = "Taking Verbal")



# QUESTION 8

names(data) 

a <- tapply(data$gpa, data$schoolid, summary) 

data_8 <- data.frame("names" =names(a) ) 
data_8$names <- names(a)

quart <- c()
for (i in 1:length(data_8$names)){
  quart <- c(quart, a[[i]][5])
}
data_8$quart <- quart

test <- table(data$schoolid)

quartile <- c()
for (i in 1:length(test)){
  quartile <- c(quartile, rep(data_8$quart[i],test[i]))
}
data$top_quartile <- quartile

data$top <- ifelse(data$gpa >= data$top_quartile, TRUE, FALSE)

data_high <- filter(data, top == TRUE)

model1 <- feols(asiste_mate ~ market + year + year_2005*treat_intesity + 
                  year_2006*treat_intesity + year_2007*treat_intesity +
                  + year_2008*treat_intesity + year_2009*treat_intesity+ year_2010*treat_intesity +
                  year_2011*treat_intesity|market+year, data = data_high)

model2 <- feols(asiste_lect ~ market + year +year_2005*treat_intesity + year_2006*treat_intesity + year_2007*treat_intesity
                + year_2008*treat_intesity + year_2009*treat_intesity+ year_2010*treat_intesity + 
                  year_2011*treat_intesity|market+year, data = data_high)

modelsummary(model1)
modelsummary(model2)

a <- model1$coefficients
b <- model1$se
c <- c(2005, 2006, 2007, 2008, 2009, 2010, 2011)
c <- c - 2008
#class(a,b,c)

plot_data <- data.frame(a,b,c)
names(plot_data) <- c("Coefficients", "SE", "Year")

ggplot(plot_data) +
  geom_point(aes(x=Year, y=Coefficients), stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar( aes(x=Year, ymin=Coefficients-SE, ymax=Coefficients+SE), width=0.4, colour="orange", alpha=0.9, size=1.3)

a <- model2$coefficients
b <- model2$se
c <- c(2005, 2006, 2007, 2008, 2009, 2010, 2011)
c <- c - 2008
#class(a,b,c)

plot_data <- data.frame(a,b,c)
names(plot_data) <- c("Coefficients", "SE", "Year")

ggplot(plot_data) +
  geom_point(aes(x=Year, y=Coefficients), stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar( aes(x=Year, ymin=Coefficients-SE, ymax=Coefficients+SE), width=0.4, colour="orange", alpha=0.9, size=1.3)






# QUESTIONO 9



names(data) 

a <- tapply(data$gpa, data$schoolid, summary) 

data_9 <- data.frame("names" =names(a) ) 
data_9$names <- names(a)

quart <- c()
for (i in 1:length(data_9$names)){
  quart <- c(quart, a[[i]][2])
}
data_9$quart <- quart

test <- table(data$schoolid)

quartile <- c()
for (i in 1:length(test)){
  quartile <- c(quartile, rep(data_9$quart[i],test[i]))
}
data$low_quartile <- quartile

data$low <- ifelse(data$gpa <= data$low_quartile, TRUE, FALSE)

data_low <- filter(data, low == TRUE)

model1 <- feols(asiste_mate ~ market + year + year_2005*treat_intesity + 
                  year_2006*treat_intesity + year_2007*treat_intesity +
                  + year_2008*treat_intesity + year_2009*treat_intesity+ year_2010*treat_intesity +
                  year_2011*treat_intesity|market+year, data = data_low)

model2 <- feols(asiste_lect ~ market + year +year_2005*treat_intesity + year_2006*treat_intesity + year_2007*treat_intesity
                + year_2008*treat_intesity + year_2009*treat_intesity+ year_2010*treat_intesity + 
                  year_2011*treat_intesity|market+year, data = data_low)

modelsummary(model1)
modelsummary(model2)

a <- model1$coefficients
b <- model1$se
c <- c(2005, 2006, 2007, 2008, 2009, 2010, 2011)
c <- c - 2008
#class(a,b,c)

plot_data <- data.frame(a,b,c)
names(plot_data) <- c("Coefficients", "SE", "Year")

ggplot(plot_data) +
  geom_point(aes(x=Year, y=Coefficients), stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar( aes(x=Year, ymin=Coefficients-SE, ymax=Coefficients+SE), width=0.4, colour="orange", alpha=0.9, size=1.3)

a <- model2$coefficients
b <- model2$se
c <- c(2005, 2006, 2007, 2008, 2009, 2010, 2011)
c <- c - 2008
#class(a,b,c)

plot_data <- data.frame(a,b,c)
names(plot_data) <- c("Coefficients", "SE", "Year")

ggplot(plot_data) +
  geom_point(aes(x=Year, y=Coefficients), stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar( aes(x=Year, ymin=Coefficients-SE, ymax=Coefficients+SE), width=0.4, colour="orange", alpha=0.9, size=1.3)


# QUESTION 10
