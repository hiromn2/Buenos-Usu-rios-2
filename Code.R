#install.packages(c("ggplot2", "readr",  "dplyr", "fastDummies", "stargazer", "Rcpp", "arrow", "parquetize", "quantreg"))
library(scales)
library(arrow)
library(parquetize)
library(dplyr)
library(readr)
library(quantreg)
library(ggplot2)
library(locfit)

data <- read_csv("dataps2s2023.csv")
#data <- read_parquet("dataps2s2023.parquet")


z <- getwd()
z <- paste(z,'/data.parquet')

tmp_pqt <- tempfile(fileext = ".parquet")
download.file("https://github.com/hiromn2/Buenos-Usu-rios-2/blob/main/dataps2s2023.parquet",
              destfile = z)

arrow:::read_parquet(tmp_pqt)

csv_to_parquet(
  path_to_csv = "dataps2s2023.csv",
  path_to_parquet = getwd()
)



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
data_2008 <- data %>% filter(year == "2008")
data_2011 <- data %>% filter(year == "2011")

#fit <- locfit(data_2008$school_inprogram ~ lp(data_2008$treat_intesity, degree = 2))
fit <- lm(school_inprogram ~ poly(treat_intesity,8), data = data_2008)
plot(data_2008$treat_intesity, data_2008$school_inprogram, col = "blue", pch = 19)
lines(sort(data_2008$treat_intesity),                 # Draw polynomial regression curve
      fitted(fit)[order(data_2008$treat_intesity)],
      col = "red",
      type = "l")



# Generate some sample data
#x <- seq(-5, 5, length.out=100)
#y <- sin(x) + rnorm(100, 0, 0.1)

# Fit a local polynomial regression of degree 2
x <- data_2008$treat_intesity
y <- data_2008$school_inprogram
fit <- lm(school_inprogram ~ poly(treat_intesity,8), data = data_2008)

# Calculate the predicted values and confidence intervals
pred <- predict(fit, se=T)
pred_df <- data.frame(x=x, y=pred$fit, lower=pred$fit-1.96*pred$se.fit, upper=pred$fit+1.96*pred$se.fit)

pred_df <- pred_df %>% arrange(desc(pred_df))
pred_df <- pred_df[-1,]
# Create the plot
library(ggplot2)
ggplot(data.frame(x=x, y=y), aes(x=x, y=y)) +
  geom_point() +
  geom_ribbon(data=pred_df, aes(x=x, ymin=lower, ymax=upper), alpha=0.3) +
  geom_line(data=pred_df, aes(x=x, y=y), color="red") +
  labs(x="x", y="y", title="Local Polynomial Regression") +
  theme_bw()





#Question 3
names(data) 
#"schoolid","studentid","market","gpa","public","private_voucher","private_nonvoucher",
#"ptje_lect4b_alu", "ptje_mate4b_alu","asiste_mate","asiste_lect","year",              
#"school_inprogram","treat_intesity","nalu",
#"year_2006","year_2007","year_2008","year_2009","year_2010","year_2011" 
library(fastDummies)
data <- fastDummies::dummy_cols(data, select_columns = "year")
data$market <- factor(data$market)
data$year <- factor(data$year)

library(stargazer)

rm(data_2007, unique_data_2007)

#model1 <- lm(ptje_mate4b_alu ~ market + year + year_2006*treat_intesity + year_2007*treat_intesity
#             + year_2008*treat_intesity + year_2009*treat_intesity+ year_2010*treat_intesity + year_2011*treat_intesity, data = data)
#model2 <- lm(ptje_lect4b_alu ~ market + year + year_2006*treat_intesity + year_2007*treat_intesity
#             + year_2008*treat_intesity + year_2009*treat_intesity+ year_2010*treat_intesity + year_2011*treat_intesity, data = data)

model1 <- lm(ptje_mate4b_alu ~ market + year + year_2006*treat_intesity + year_2007*treat_intesity
             + year_2008*treat_intesity + year_2009*treat_intesity+ year_2010*treat_intesity + year_2011*treat_intesity, data = data)
model2 <- lm(ptje_lect4b_alu ~ market + year + year_2006*treat_intesity + year_2007*treat_intesity
             + year_2008*treat_intesity + year_2009*treat_intesity+ year_2010*treat_intesity + year_2011*treat_intesity, data = data)

stargazer(model1, model2)


stargazer(model1, model2, omit=c("var"), 
          align=TRUE, type = "text", no.space = TRUE, 
          title = "Regression", out = "fit.tex")

#Question 4
a <- model1$coefficients[c(360:365)]
j <- tail(summary(model1)$coefficients)
b <- j[,"Std. Error"]
c <- c(2006, 2007, 2008, 2009, 2010, 2011)
class(a,b,c)

plot_data <- data.frame(a,b,c)
names(plot_data) <- c("Coefficients", "SE", "Year")

ggplot(plot_data) +
  geom_bar(aes(x=Year, y=Coefficients), stat="identity", fill="skyblue", alpha=0.7) +
  geom_errorbar( aes(x=Year, ymin=Coefficients-SE, ymax=Coefficients+SE), width=0.4, colour="orange", alpha=0.9, size=1.3)


#Question 5

