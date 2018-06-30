# Exploratory Analsysis of Prosper Loan Data


```r
knitr::opts_chunk$set(fig.width=10, fig.height=7,echo=TRUE, warning=FALSE, 
                      message=FALSE)
```

<style>
body {
text-align: justify}
</style>

##Introduction

Prosper is an online peer to peer lending platform that connects people who need money with those who are willing to loan the same for an interest. In contrast to the traditional lending systems such banks where the entire loan amount is funded by a single entity, prosper allows borrowing from multiple lenders. Borrowers do not need to pledge collateral while applying for loans, and the loan is approved based on a number of parameters that describe the background of the customers, including (but not limited to) their income, credit hisotry etc. For each application, prosper assigns a 'rating' and a prosper score, to describe the risk associated with the loan as well the probability of it going bad. 

In this work, I analyse historical data from Prosper for the period 2005-2014, to understand more about the prosper business. Attempt has been made to answer a few central questions which shall form the theme of this work, and based on which the entire analysis is performed. Specifically, the analysis tries to understand how the prosper business has grown (and performed) over the years, identify the types of customers who are most likely to default, and evaluate how good  prosper is at identifying good and bad loans.

 I have tried to ensure that the flow of this work consistent with the above mentioned theme. A summary and reflection section is provided at the end of this docuument to state some concluding remarks about the analysis, in addition to a brief description of the challenges faced during the course of this work. 
 
 

####Loading the packages required for the analysis

```r
library(knitr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(scales)
library(grid)
```

####Read and summarise the data. 

```r
loans = read.csv("/Users/deepudilip/ML/Udacity_nano_degree/Project_4/Prosper_data/prosperLoanData.csv",na.strings=c("NA",""))
```

There are 113937 rows of 81 variables in the dataset. The description of each variable is available in the accompanying excel file by the name "Prosper Loan Data - Variable Definitions". It is not within the scope of this work to explore all the 81 variables in the dataset, since that would be too cumbersome at the moment. Hence this analysis will only look into a subset of those 81 variables. For this, let us first of all identify some potentially interesting variables and store them in a vector as shown below. [Note: The number of variables in the vector below are still too large; we will only look at a still smaller subset from the list below]


```r
cols_interest = c("CreditGrade","Term","LoanStatus","ClosedDate","BorrowerAPR",
            "BorrowerRate","LenderYield","EstimatedEffectiveYield",
            "EstimatedLoss","ProsperRating..Alpha.", "ProsperScore", 
            "BorrowerState", "Occupation","EmploymentStatus", 
            "EmploymentStatusDuration","IsBorrowerHomeowner", 
            "DateCreditPulled", "CreditScoreRangeLower", 
            "CreditScoreRangeUpper", "CurrentCreditLines", "OpenCreditLines",
            "TotalCreditLinespast7years","InquiriesLast6Months",
            "TotalInquiries","CurrentDelinquencies","AmountDelinquent",
            "DelinquenciesLast7Years","DebtToIncomeRatio","IncomeRange",
            "IncomeVerifiable","StatedMonthlyIncome","TotalProsperLoans",
            "TotalProsperPaymentsBilled","OnTimeProsperPayments",
            "ProsperPaymentsLessThanOneMonthLate","ProsperPaymentsOneMonthPlusLate",
            "ProsperPrincipalBorrowed","ProsperPrincipalOutstanding",
            "ScorexChangeAtTimeOfListing","LoanCurrentDaysDelinquent",
            "LoanFirstDefaultedCycleNumber","LoanMonthsSinceOrigination",
            "LoanOriginalAmount","MonthlyLoanPayment","PercentFunded",
            "InvestmentFromFriendsCount","InvestmentFromFriendsAmount","Investors")
```

```r
#Checking column names in the above vector variables for typos
# all_cols = colnames(loans)
# for (var in variables) {
#   if (!(var %in% all_cols)){
#     print (var)
#   }
# }
```


####Data Sanctity Check

For the above mentioned columns, let us count the number and  percentage of NA values in them . Only those columns with at least one NA value is shown 


```r
all_columns_with_na = colnames(loans)[colSums(is.na(loans))>0] #Get names of all columns with NA values
cols_interest_with_na = intersect(all_columns_with_na,cols_interest) # Get all columns which we are interested in having NA values
#sum(colSums(is.na(loans))!=0)

#Total NA values in numbers
colSums(is.na(loans[cols_interest_with_na]))
```

```
##                         CreditGrade                          ClosedDate 
##                               84984                               58848 
##                         BorrowerAPR             EstimatedEffectiveYield 
##                                  25                               29084 
##                       EstimatedLoss               ProsperRating..Alpha. 
##                               29084                               29084 
##                        ProsperScore                       BorrowerState 
##                               29084                                5515 
##                          Occupation                    EmploymentStatus 
##                                3588                                2255 
##            EmploymentStatusDuration               CreditScoreRangeLower 
##                                7625                                 591 
##               CreditScoreRangeUpper                  CurrentCreditLines 
##                                 591                                7604 
##                     OpenCreditLines          TotalCreditLinespast7years 
##                                7604                                 697 
##                InquiriesLast6Months                      TotalInquiries 
##                                 697                                1159 
##                CurrentDelinquencies                    AmountDelinquent 
##                                 697                                7622 
##             DelinquenciesLast7Years                   DebtToIncomeRatio 
##                                 990                                8554 
##                   TotalProsperLoans          TotalProsperPaymentsBilled 
##                               91852                               91852 
##               OnTimeProsperPayments ProsperPaymentsLessThanOneMonthLate 
##                               91852                               91852 
##     ProsperPaymentsOneMonthPlusLate            ProsperPrincipalBorrowed 
##                               91852                               91852 
##         ProsperPrincipalOutstanding         ScorexChangeAtTimeOfListing 
##                               91852                               95009 
##       LoanFirstDefaultedCycleNumber 
##                               96985
```

```r
cat("\n\n\n")
```

```r
#NA values as percentage
colSums(is.na(loans[cols_interest_with_na]))*100/nrow(loans)
```

```
##                         CreditGrade                          ClosedDate 
##                         74.58858843                         51.64959583 
##                         BorrowerAPR             EstimatedEffectiveYield 
##                          0.02194195                         25.52638739 
##                       EstimatedLoss               ProsperRating..Alpha. 
##                         25.52638739                         25.52638739 
##                        ProsperScore                       BorrowerState 
##                         25.52638739                          4.84039425 
##                          Occupation                    EmploymentStatus 
##                          3.14910872                          1.97916392 
##            EmploymentStatusDuration               CreditScoreRangeLower 
##                          6.69229486                          0.51870771 
##               CreditScoreRangeUpper                  CurrentCreditLines 
##                          0.51870771                          6.67386363 
##                     OpenCreditLines          TotalCreditLinespast7years 
##                          6.67386363                          0.61174158 
##                InquiriesLast6Months                      TotalInquiries 
##                          0.61174158                          1.01722882 
##                CurrentDelinquencies                    AmountDelinquent 
##                          0.61174158                          6.68966183 
##             DelinquenciesLast7Years                   DebtToIncomeRatio 
##                          0.86890123                          7.50765774 
##                   TotalProsperLoans          TotalProsperPaymentsBilled 
##                         80.61648104                         80.61648104 
##               OnTimeProsperPayments ProsperPaymentsLessThanOneMonthLate 
##                         80.61648104                         80.61648104 
##     ProsperPaymentsOneMonthPlusLate            ProsperPrincipalBorrowed 
##                         80.61648104                         80.61648104 
##         ProsperPrincipalOutstanding         ScorexChangeAtTimeOfListing 
##                         80.61648104                         83.38731053 
##       LoanFirstDefaultedCycleNumber 
##                         85.12160229
```

The columns "EstimatedEffectiveYeild","EsimtatedLoss","ProsperScore", "ProsperRating..Alpha." have about 25% of their values missing, but this is due to the fact that these variables are available only for loans post july 2009 (as mentioned in the dataset descroption). The columns TotalPropserLoans, TotalProsperPaymentsBilled,OnTimeProsperPayments, ProsperPaymentsLessThanOneMonthLate, ProsperPaymentsOneMonthPlusLate, ProsperPrincipalBorrowed,ProsperPrincipalOutstanding and ScorexChangeAtTimeofListing are available only for those applicants who had applied with prosper at least once in the past. 
Hence these columns do not require any missing value treatment. Additionally, the column CreditGrade has meaningful values only for those listings pre-2009. The other columns with missing data have less than 10% of their values missing. These shall be left as it is for now, and we will continue with the analysis.


#Univariate Analysis

First of all, let us do make some univariate plots to get familiar with the dataset.

- **Percentage of loans under different status**

```r
ggplot(data=loans,aes(y=(..count..)*100/sum(..count..),
                      x=reorder(LoanStatus,LoanStatus,function(x) length(x)))) +
  geom_bar(fill="#336633") + scale_y_continuous() + coord_flip() + 
  ylab("Percentage of loans")+xlab("Loans Status")+theme(axis.text = element_text(size = 14),axis.title = element_text(size=14))
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

About 50% of the listings are loans are that currently ongoing. Around 33% listings are completed loans whereas 15% of the listings show chargedoff and defaulted loans.

Let us now create some new date variables which will help in further analysis

```r
loans$ListDate = as.Date(as.character(loans$ListingCreationDate),
                          "%Y-%m-%d %H:%M:%S")
loans$LoanDate = as.Date(as.character(loans$LoanOriginationDate),
                          "%Y-%m-%d %H:%M:%S")
```


- **Listings according to the years**


```r
loans$ListYear = year(loans$ListDate)
year_labels=factor(2005:2014)
ggplot(data=loans,aes(y=(..count..)*100/sum(..count..),
                      x=reorder(ListYear,ListYear,function(x) length(x))))+
  geom_bar(fill="#336633") + 
  #scale_x_continuous(breaks=c(2005:2014),labels=year_labels) +
  scale_y_continuous()+ylab("Percentage of loans")+
  xlab("Year")+
    theme(axis.text = element_text(size = 14),axis.title = element_text(size=14))
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

Only about 25% of the listings belong to the pre-2009 period. From 2009, we see a steady increase in the number of listings. For 2014, we have data only until the month of march, and hence we see a lesser number of listings than in 2014.

- Typical time between listing and loan approval 


```r
loans$DaysElapsed = as.numeric(difftime(loans$LoanDate,loans$ListDate))
summary(loans$DaysElapsed)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    1.00    5.00    9.00   12.34   13.00 1095.00
```

The minimum number of days between listing of a loan and its approval is 1 day. The median time gap is 9 days, with 50% of the loans originating between 5 and 13 days. Let us have a look at the 99 percentile value of the number of days elapsed.


```r
quantile(loans$DaysElapsed,0.99)
```

```
## 99% 
##  77
```

Basically, 99% of the loans were originated within 3 months of the date the loan was originally listed. However, we could see that the longest time it took for a loan to get approved was 1095 days. 



- **Percentage of different loans according to credit grades or prosper scores**

It is to be noted that the "CreditGrade" variable is applicable to only those loans prior to 2009, which as we saw earlier constituted only about 25% of the listings.Following 2009, instead of CreditGrade, the variable "ProsperRating" was used for assessing loans. Hence we will plot these two variables only for the loans listed in their respective relevant time frames.



```r
ggplot(data=filter(loans,ListDate<="2009-07-31" & !is.na(CreditGrade)),
  aes(y=(..count..)/sum(..count..),
      x=reorder(CreditGrade,CreditGrade, function(x) length(x)))) +
  geom_bar(fill="#336633") + scale_y_continuous(labels=percent) +
  ylab("Percentage of loans") + xlab("CreditGrade")+theme(axis.text = element_text(size = 14),axis.title = element_text(size=14))
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

For the loans in the pre 2009 period, we see that the majority were assigned a C grade, while A, B and D grades were quite close in number. Around 12% of the ratings were classified under the HR (High Risk) category. Let us now look into the post 2009 loans



```r
ggplot(data=filter(loans,LoanDate>"2009-07-31" &!is.na(ProsperRating..Alpha.)),
 aes(y=(..count..)*100/sum(..count..),
  x=reorder(ProsperRating..Alpha.,ProsperRating..Alpha.,function(x) length(x))))+
  geom_bar(fill="#336633") + scale_y_continuous()+ylab("Percentage of loans") + 
  xlab("ProsperRating")+theme(axis.text = element_text(size = 14),axis.title = element_text(size=14))
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

The post 2009 period saw some differences with the pre-2009 period. For instance, percentage of loans with AA rating are lesser in this period, perhaps indicating a more rigorous evaluation during this time. Also, the percentage of loans with HR (High Risk) rating have also gone down.


- **Loan Amounts**



```r
p1 = ggplot(data=loans,aes(x=LoanOriginalAmount))+
     geom_histogram(binwidth=1000,center=500,fill='blue',color='black')+
     scale_x_continuous(breaks=seq(0,30000,by=5000),limits=c(0,40000))+
     theme(axis.text.x=element_text(angle=45))+ylab("Number of Loans")+
  theme(axis.text = element_text(size = 14),axis.title = element_text(size=14)) 

p2 = ggplot(data=loans,aes("loanAmount",LoanOriginalAmount))+
     geom_boxplot(fill = "#0066cc")+
     annotate(geom="text",x=0.85, y=fivenum(loans$LoanOriginalAmount)+1000, 
     label=as.character(fivenum(loans$LoanOriginalAmount)),color="black") +
      theme(axis.text = element_text(size = 14),axis.title = element_text(size=14)) #adding annotation to include the 5 point summary in the box plot

p3 = ggplot(data=loans,aes("loanAmount",LoanOriginalAmount))+
     geom_violin(fill='dark blue') +
    theme(axis.text = element_text(size = 14),axis.title = element_text(size=14))

grid.arrange(p1,p2,p3,ncol=2,widths=c(4,4))
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

A mildly interesting observation from the above histograms are the spikes that can be seen for the ranges 3000-4000, 9000-10000, 14000-15000, 19000-20000, and 24000-25000. These could be attributed to tendencies of loans to be in round figures. However, by that argument, we would have expected a spike to be at 4000-5000 range instead of 3000-4000 range, but perhaps since they are relatively smaller amounts, there might not be tendency to round off the loan to 5000. (5000 is 25% more than 4000). These spikes are also evident from the violin plot, which shows the density of the distribution oft he loan amounts. A summary of the loan amounts is shown below


```r
summary(loans$LoanOriginalAmount)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    1000    4000    6500    8337   12000   35000
```

Almost 50% of the loans are between $4000 and $1200, with a median value for loan amount at $6500. From the boxplot we can see a bunch of data points that have been classified as outliers. The interquartile range is $8000 and based on this, the position of the top whisker should be at $24000. Let us see the number of "outliers" in the dataset with respect to LoanAmount.


```r
nrow(subset(loans,LoanOriginalAmount>24000))
```

```
## [1] 4395
```

Which is about 4% of the listings in the dataset. Let us also look at the typical term of loans

-**Typical duration of loans**


```r
table(loans$Term)*100/nrow(loans)
```

```
## 
##        12        36        60 
##  1.416572 77.040821 21.542607
```

The loans are of either 1 year, 2 years or 5 years durations, with 77% of the listings falling in the 2 year category. Let us look at the APR and Interest rates


```r
interest_par = par(mfrow=c(1, 2))
boxplot(loans$BorrowerAPR, main="APR") 
boxplot(loans$BorrowerRate,main="Interest Rate")
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

```r
par(interest_par)
```

Interest rates are low for Prosper loans, with the APR and borrower interest rate touching at most 0.5%, with most of the loans remaining between 0.1-0.3% range. The higher interests are probably charged for those loans in high risk categories.


Now lets have a look at the income statistics of the borrowers.

```r
p1 = ggplot(data=loans,
            aes(x=reorder(IncomeRange,IncomeRange,function(x) length(x))))+
            geom_bar(fill='dark green') + ylab("Number of Loans") + coord_flip()+
            xlab("IncomeRange") +theme(axis.text = element_text(size = 14),axis.title = element_text(size=14)) 

p2 = ggplot(data=loans,aes("MonthlyIncome",StatedMonthlyIncome))+geom_boxplot() +
  scale_y_log10()+theme(axis.text = element_text(size = 14),axis.title = element_text(size=14))

grid.arrange(p1,p2,ncol=2,widths=c(3.5,3.5))
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

The highest percentae of borrowers are from the $25000-$49,999 bracket, followed closely by the $50,000-%74,999 bracket. The boxplot however is not so clear due to the presence of some outliers, who apparently have incomes as high as $1750000. These figures look rather strange, and it would be interesting to see if these are verified income. But first, let us look at the boxplot excluding the outliers.


```r
ggplot(data=loans,aes("MonthlyIncome",StatedMonthlyIncome))+geom_boxplot() + 
   scale_y_continuous(limits=c(0,12000))+theme(axis.text = element_text(size = 14),axis.title = element_text(size=14)) 
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-19-1.png)<!-- -->
The median monthly income is between $4000 and $5000, with the outliers starting from approximately $11,0000. Lets look at the outliers and see if their incomes are generally verified or not


```r
ggplot(data=filter(loans,StatedMonthlyIncome>11000),aes(x=IncomeVerifiable))+
   geom_bar(fill="dark green") +theme(axis.text = element_text(size = 14),axis.title = element_text(size=14))
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

Most of the applicants who claimed more than $11,000 in income had supporting documentary evidences to substantiate their claims. However, let us take a look at those who did not provide any such evidence, yet claimed very high income


```r
p1 = ggplot(data=filter(loans,StatedMonthlyIncome>11000),
            aes(x=IncomeVerifiable,y=StatedMonthlyIncome))+geom_boxplot()+
  theme(axis.text = element_text(size = 14),axis.title = element_text(size=14))
p2 = ggplot(data=filter(loans,StatedMonthlyIncome>11000),
            aes(x=IncomeVerifiable,y=StatedMonthlyIncome))+geom_boxplot() +   
    scale_y_continuous(limits=c(0,100000))+
  theme(axis.text = element_text(size = 14),axis.title = element_text(size=14))
grid.arrange(p1,p2,ncol=2,widths=c(5,5))
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-21-1.png)<!-- -->

The right figure is a zoomed in version of the left figure to exclude the extreme outliers. Amongst the high income applicants, it seems those applicants who have not had their income verified claimed higher incomes on the average. It should also be noted that the top applicant in terms of claimed monthly incomes has not had his(her) incomes verified either, raising questions on the legitimacy of the claim. It could also be an error.

- **Geographical distribution of the Prosper Applicants**


```r
#This chunk plots the distribution of Applicants across the States  on the map of the US

loans$BorrwerStateName = "" #A new column which will have the full name of the state instead of the abbreviations
#get row index

loans$BorrowerStateName =state.name[match(loans$BorrowerState,state.abb)] #Get the name of state from state.name by match the abbreviations in the                                                                                column with those in state.abb
loans$BorrowerStateName[loans$BorrowerState=="DC"]="District of Columbia" #Rename Washington DC as District of Columbia
loans$BorrowerStateName = tolower(loans$BorrowerStateName)

map_plot_data = map_data("state") 

#as.data.frame(table(loans$BorrowerStateName))
loan_count_by_state = as.data.frame(table(loans$BorrowerStateName)) #Get a count of applicants per state and convert it to dataframe
#merge with the map_data
names(loan_count_by_state)=c("region","count")
#combined = merge(map_plot_data,loan_count_by_state,by="region")
combined= merge(x = map_plot_data, y = loan_count_by_state, 
                by = "region", all.x = TRUE) #Merge the above dataframe with the map data for plotting

#testing
#loan_count_by_state = loan_count_by_state[loan_count_by_state$region!=alaska,]



p <- ggplot()
p <- p + geom_polygon(data=combined, aes(x=long, y=lat, group = group, 
                                         fill=combined$count),colour="white") +
     scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar")+
    theme(axis.text = element_text(size = 14),axis.title = element_text(size=14))
p
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-22-1.png)<!-- -->

```r
# p <- ggplot()
# p <- p + geom_polygon(data=combined, aes(x=long, y=lat, group = group),colour="white")
# p 
```

We see that Californians have formed a disproportionately large portion of the prosper applicats over time.  This is perhaps due to California being the hotbed of innovation in the US and hence having a majority of the population working in the technology sector. They might be more inclined towards adopting new ideas such as p2p lending. The other areas where prosper lending is popular are Texas, Florida, New York, Georgia and Illinois, which are all areas of high commerical and tech activity.

- **Number of Loans according to the occupation of the applicant**


```r
ggplot(filter(loans,!is.na(loans$Occupation)), 
  aes(x=reorder(Occupation,Occupation,function(x) length(x)))) + 
  geom_bar(fill='dark green')+ylab("Number of Loans") +coord_flip() +
  xlab("Occupation")+theme(axis.text = element_text(size = 14),axis.title = element_text(size=14))
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-23-1.png)<!-- -->

Not much interesting info here, except it appears that students are among the least popular users of Prosper loans. This could be because they are not employed while at school which could increase the risk of their loans. 


Lets take a look at the number of credit lines of the applicants


```r
interest.par <- par(mfrow=c(1, 2)) #to plot two boxplots side by side
boxplot(loans$CurrentCreditLines,main="Current Credit Lines")
boxplot(loans$OpenCreditLines,main="Open Credit Lines")
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-24-1.png)<!-- -->

```r
par(interest.par)
```

```r
summary(loans$CurrentCreditLines)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00    7.00   10.00   10.32   13.00   59.00    7604
```

```r
summary(loans$OpenCreditLines)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##    0.00    6.00    9.00    9.26   12.00   54.00    7604
```

50% of the listings have credit lines in the range 9-13, with the median number of credit lines of around 10. Few outliers are present here as well. 


- **Typical percentage funding obtained for the loans**

```r
nrow(subset(loans,PercentFunded<1))
```

```
## [1] 867
```

It was seen from a separate boxplot(not shown) that almost all the loans were 100% funded.Only a few of the listings(867) were not fully funded, as shown in the above code output.


- **Distribution of debt to Income ratio**


```r
p1 = ggplot(loans, aes(x=DebtToIncomeRatio))+
     geom_histogram(fill='blue',color='black',binwidth=0.125,center=0.0625) + 
     ylab("Number of Loans")+
    theme(axis.text = element_text(size = 14),axis.title = element_text(size=14))
p2 = ggplot(loans,aes(x="",y=DebtToIncomeRatio))+geom_boxplot() + 
  scale_y_log10(breaks=c(0.1,1,10)) +
  theme(axis.text = element_text(size = 14),axis.title = element_text(size=14))
grid.arrange(p1,p2,ncol=2,widths=c(3.5,3.5))
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-27-1.png)<!-- -->

```r
summary(loans$DebtToIncomeRatio)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##   0.000   0.140   0.220   0.276   0.320  10.010    8554
```

The median value is 0.220, and for half the loans the value lies between 0.32 and 0.14. These numbers sound reasonable. However, we do see some outliers, having Debt to Income Ratio (DTI) as high as 10.


##Bivariate Analysis

So far we have been looking at the data one variable at a time. This gives a brief overview and big picture of the dataset, however to answer more interesting questions, we need to go for bi-variate and multi-variate analysis. We start with bi-variate analysis. Although a general investigation of the dataset through plotting each variable against the other would reveal a lot of insights, the number of variables in this dataset is far too large and consequently, the number of questions that can be asked are also too many. Hence, it is important that to develop a central theme for this investigation, which would encompass a few broad questions we wish to be answered from the dataset. This will help us narrow down the scope of this analysis and develop fewer sub-questions for which we would like to get answers.

This dataset when investigated should potentially tell us details of the performance of the Prosper Loan Enterprise. Hence, there are two major questions I wish to explore from this dataset. How has the prosper business fared over the years? Since this is a p2p lending platform, one of the major concerns is the ability to differentiate between a good and bad loan in advance. Hence, another question would be, who are the customers most likely to default on the loan?. How good is prosper in identifying the good vs bad loans? On a similar note, has their judgements regarding prosper loans improved with time? These are the two major questions that I have tried to answer through this analysis. Besides satisfying one's curiosity, I believe answers to thes questions are of some definite business value to Prosper.

First, let us look at some time seris plots to evaluate the growth of Prosper over the years. Before that, it would be convenient to classify all the loans under Defaulted and Chargedoff status into a single categoy by the name "Defaulted", and those loans that are ongoing into the "Current" category. The current would thus include all the loans in the 'Past Due' and the 'FinalPaymentinProgress' status as well, since technically they  are all still on-going loans. It can be argued that the loans due past 90 or 120 days are most likely going to default, and hence should be put in the defaulted category. But since those constitute only a tiny percentage of the dataset (<0.5%), we will just list them in the 'current' category for now. Also, the number of cancelled loans in the entire dataset is just 5, so let us ignore those records for our analysis. The completed loans will be classifed as "Completed"

Let us create a column called 'Status'. If the LoanStatus falls under the category "Defaulted" or "Charged off", then this column will have value "Defaulted". The value will remain "Current", if the LoanStatus is Current or FinalPaymentinProgress or any of the Past Due status. The value will be "Completed", if the LoanStatus is "Completed". We shall put this new column in a new dataframe by the name 'loans_deft'


```r
loans_deft =  loans %>% filter(LoanStatus!="Cancelled") %>%
              mutate(Status=if_else(LoanStatus %in%                                                                         
              c("Defaulted","Chargedoff"),"Defaulted",
              if_else(LoanStatus=="Completed","Completed","Current")))
```

It would also be convenient to rename the factors representing quarters. This is done for convenience of plotting the data later.

```r
#Defining the function
rename = function(level){
    level_split = strsplit(as.character(level)," ") 
    level_new = paste(level_split[[1]][2],level_split[[1]][1],sep=" ") #e.g., rename factor level Q1 2011 as 2011 Q1
    return (level_new)
} 
#Renaming

loans_deft$LoanQuarter = sapply(loans_deft[,"LoanOriginationQuarter"],rename)  
```


Now, let us look the historical trend of the number of loans, classified according  to the quarter in which they originated and their Status


```r
#Get percentage share of Defaulted, current and completed loans in each quarter. 

loans_deft_quarterly_summary = loans_deft %>% group_by(LoanQuarter) %>%
  mutate(total=n())%>% group_by(LoanQuarter,Status)%>%
  summarize(count=n(),perc=n()*100/first(total))

ggplot(data=loans_deft,aes(x=LoanQuarter,fill=Status))+geom_bar()+
  theme(axis.text.x=element_text(angle=45)) +ylab("Number of Loans")+
  theme(axis.text = element_text(size = 14),axis.title = element_text(size=14))
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-31-1.png)<!-- -->

The above plot is interesting. It seems the prosper business didnt take off so much in volume in the intial period, particularly over the 4 year period 2005-2009. Also, we see a big dip in the number of prosper loans in the period 2008Q4 - 2009Q3. This trend should be kept in mind. Recollect that the propser rating system got revamped in July 2009. Perhaps some major restructuring of the business occured around this time  
The general trend is that number of prosper loans  seems to be increasing generally from 2009, with a few dips here and there. And the business really seems to havetaken off from Q2 2013. 
It would also be interesting to look at the historical trend of loan defaults as well, which can be seen below.



```r
loans_defaulted_quarterly=filter(loans_deft_quarterly_summary,
                                 Status=="Defaulted")

ggplot(loans_defaulted_quarterly,aes(x=LoanQuarter,y=perc,group=1))+
  geom_point(color='blue')+
geom_line(color='blue')+theme(axis.text.x=element_text(angle=45))+
  ylab("Percentage of Defaults") +theme(axis.text = element_text(size = 14),axis.title = element_text(size=14))
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-32-1.png)<!-- -->

We see a dip in the default rates after after Q2 2008. As we saw from the earlier figure, the number of loans originated in Q2 of 2009 is very low, however the loans did start increasing again in number from Q3 of 2009. And we see that for the loans originated during the period Q3 2009 to Q4 2010, the default rates are lower compared tothe pre-2009 era. The above graph should be used to see the default rates only until 2010 Q4, since some of the loans originated from 2011 Q1 are still on-going and hence the default rate cannot be calculated correctly. (This is why see the default rates going down after 2011 Q2).
It is difficult to concretely say that default rates have lowered since 2009, since we do not have enough data for the post 2009 period. However, from whatever data that is available, we can suspect this is indeed the case, and whatever changes propser brought into their business, particularly with regard to rating the loan listings, have led to decreased default rate. 


Let us also look at the average loan amount with time


```r
loan_amt_quar_summary = loans_deft %>% group_by(LoanQuarter) %>% 
  summarize(avg_loan_amt = mean(LoanOriginalAmount))

ggplot(data=loan_amt_quar_summary,aes(x=LoanQuarter,y=avg_loan_amt,group=1))+
  geom_line(color='red')+
  geom_point(color='red')+theme(axis.text.x=element_text(angle=45))+
  ylab("Average Loan Amount")
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-33-1.png)<!-- -->

In addition to the number of loans, the average loan amount had also gone down in the 2008-2010 period. However, ever since then we see a more or less steady increase in the average loan amount.


Having looked at some of the time series plots, we get a general idea over how the propser business have fared over the years, in terms  of the number of loans listed and originated, as well as the varaition in the quality of loans over time. This was just one of the general question we wished to answer from the data. The other important question remains. Is it possible to say which loans are most likely to default based on the historical data? Prosper has put in a place a rating system for the loans (using metrics such as ProsperScore or ProsperRating) to classify them according to their risk. How good is this rating system in identifying bad loans? How are these ProsperScores/ProsperRatings assigned, i.e., what factors are considered while generating these scores/ratings? 

Let us continue to investigate the dataset with these questions in mind

Let has have a look at the distirbution of of Debt to Income Ratio according to the Loan Status

- **Debt to Income Ratio vs Loan Status**


```r
ggplot(loans, aes(x=LoanStatus, y=DebtToIncomeRatio)) + 
geom_boxplot() + theme(axis.text.x=element_text(angle=45))+
  scale_y_log10(breaks=c(0.1,1,10))+theme(axis.text = element_text(size = 14),axis.title = element_text(size=14))
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-34-1.png)<!-- -->

Note that the y-axis scale is logarithmic. We see a lot of outliers in the above boxplots. Let us exclude the outliers and look at the same



```r
meds = loans %>% group_by(LoanStatus) %>% 
      summarize(med_DTI= median(DebtToIncomeRatio,na.rm=T)) #Get median DTI for each loan status, to be displayed                                                                                                  in the box plot
ggplot(loans, aes(x=LoanStatus, y=DebtToIncomeRatio)) + 
  geom_boxplot(outlier.color = NA) +
  theme(axis.text.x=element_text(angle=45)) +
  scale_y_continuous(limits = c(0,0.75)) +
  geom_text(data=meds,aes(y = meds$med_DTI, 
          label = round(meds$med_DTI,2)),size = 3, vjust = -0.5)+
  theme(axis.text = element_text(size = 14),axis.title = element_text(size=14))
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-35-1.png)<!-- -->

Interestingly, there isn't a considerable difference in the median value of debt to income ratio between the defaulters and the completed loans. If anything, we should point out that current loans have a slighlty higher median DTI than charged off and defaulted loans. Even those loans that are past due have lower DTI in general than current loans. This is an observation that is quite counter-intuitive. Normally, we would expect DTI to be the single most important factor affecting a loan repayment. We would expect a higher DTI value for defautlers, but apparently that is not the case here. This is telling us that predicting whether a loan would default or not is not so straightforward as it might appear on the outset.

As explaiend earlier, we again see that it is more convenient to club the loan status together, to include just 3 categories, - Current, Completed or Defaulted. This was done earlier, hence we will use that column for grouping our data in all the following analysis.

Let us look at the credit score range of the customers according to the loan status



```r
ggplot(data=loans_deft,aes(x="Upper",y=CreditScoreRangeUpper))+
  geom_boxplot(color='blue')+
  geom_boxplot(aes(x="Lower",y=CreditScoreRangeLower),color='red')+
  facet_wrap(~Status)+ylab("CreditScore")+xlab("CreditScoreRange")+
  theme(axis.text = element_text(size = 14),axis.title = element_text(size=14))
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-36-1.png)<!-- -->

The defaulted loans have  a lower median upper and lower credit score values in comparison to completed loans and even current loans. Since some of the loans in the current category will probably default, the real comparison should be made between the completed and the defaulted loans.


Loan Status according to the prosper score


```r
ggplot(data=loans_deft,aes(x=Status,y=ProsperScore))+
  geom_boxplot(color='blue',fill='orange')+
  theme(axis.text.x = element_text(angle=45))+
  theme(axis.text = element_text(size = 14),axis.title = element_text(size=14))
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-37-1.png)<!-- -->

The defaulted loans indeed have a lower prosper score than completed loans. The plot does seem as expcted. The completed loans have the highest prosper socores (median), and defaulted loans have the lowest, and the current loans stay somewhere in between, possibly due to the fact that some of the current loans will default. This probably indicates that the propser scores are good predictors of the loan default chances. 

It would be better if we treat the propser scores as discrete factor levels instead of continuous numbers. This is done in the plot below. We also subset the loans_deft dataframe to include only those loans post 2009, since ProsperScore and ProsperRatings are available only for those loans



```r
loans_post = filter(loans_deft,LoanDate>"2009-07-31")
PRscore_status = loans_post%>%group_by(Status)%>%mutate(x=n())%>%
                group_by(ProsperScore,add=TRUE)%>%
                summarize(total_by_status=first(x),total=n())


ggplot(PRscore_status,aes(x=as.factor(ProsperScore),y=total*100/total_by_status))+
  geom_bar(stat='identity',fill='dark blue',color="white")+facet_wrap(~Status)+ 
  xlab("Prosper Score")+ylab("Percentage of loans")+
  geom_text(aes(y=round(total*100/total_by_status,2)+0.5,
                  label=paste0(round(total*100/total_by_status,2),'%')),vjust=0)+
  theme(axis.text = element_text(size = 14),axis.title = element_text(size=14))
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-38-1.png)<!-- -->

We see that in case of defaulted loans, only about 32% applicants had prosper score greater than equal to 7, whereas in case of completed loans, more than 50% of the applicants had prosper score greater than or equal to 7. This is more evident if we further club the 11 prosper scores into three buckets, 1-3, 4-6, and 7+ as categories 1, 2 and 3. Let us do that here.

```r
loans_post = loans_post %>% mutate(ProsperScore_bucket = 
              if_else(ProsperScore %in% c(1,2,3),1,
              if_else(ProsperScore %in% c(4,5,6),2,3)))
```

Let us make the same plot as above, but using this new ProsperScore Bucket Variable.


```r
PRscore_status = loans_post%>%group_by(Status)%>%mutate(x=n())%>%
                group_by(ProsperScore_bucket,add=TRUE)%>%
                summarize(total_by_status=first(x),total=n())

ggplot(PRscore_status,aes(x=as.factor(ProsperScore_bucket),
                          y=total*100/total_by_status))+
  geom_bar(stat='identity',fill='dark blue',color="white")+
  facet_wrap(~Status)+xlab("Prosper Score")+
  ylab("Percentage of loans") + 
  geom_text(aes(y=round(total*100/total_by_status,2)+0.5,
                label=paste0(round(total*100/total_by_status,2),'%')),vjust=0)+
  scale_x_discrete(labels = c("(1-3)","(4-6)","(7+)"))+
  theme(axis.text = element_text(size = 14),axis.title = element_text(size=14))
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-40-1.png)<!-- -->

Let us look at the same information in a different perspective through a different plot. Let us ignore the on-going loans and take only the past (completed/defaulted) loans, and look at the percentage of completed and defaulted loans for each ProsperScore


```r
PRscore_status = filter(loans_post,Status %in% c("Completed","Defaulted")) %>% 
  group_by(ProsperScore) %>% mutate(x=n()) %>% 
  group_by(Status,add=TRUE) %>% 
  summarize(total_by_status=first(x),total=n())

ggplot(filter(PRscore_status,is.na(ProsperScore)!=T),
       aes(x=Status,y=total*100/total_by_status))+ 
geom_bar(stat='identity',fill='dark blue',color="white") + 
  facet_wrap(~as.factor(ProsperScore))+ 
  xlab("Loan Status")+ylab("Percentage of loans") +
  geom_text(aes(y=round(total*100/total_by_status,2)+0.5,
                label=paste0(round(total*100/total_by_status,2),'%')),vjust=0) +
  ggtitle("Prosper Score")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text = element_text(size = 14),axis.title = element_text(size=14))
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-41-1.png)<!-- -->

We see that as the prosper score increases from 1-11, the percentage of loan default keeps continuously dcecreasing. This is an interesting trend, and indicates that prosper scores are good predictors of a chance of loan default

Let us look at the ProsperRating Variable now, according to the loan status. Since the ProsperRating variable is only available for loans past 2009, we will use the loans_post dataframe. We will need to create the Status column again in this dataframe, which is done below.


```r
loans_post =  loans_post %>% filter(LoanStatus!="Cancelled") %>% 
  mutate(Status=if_else(LoanStatus %in% c("Defaulted","Chargedoff"),"Defaulted",
                        if_else(LoanStatus=="Completed","Completed","Current")))
```


```r
PRating_status = filter(loans_post) %>% group_by(Status) %>% mutate(x=n()) %>%
  group_by(ProsperRating..Alpha.,add=TRUE) %>% 
  summarize(total_by_status=first(x),total=n())

# PRating_status = loans%>%group_by(LoanStatus,ProsperRating..numeric.)%>%summarize(total=n())%>%filter(LoanStatus=="Completed" | LoanStatus=="Chargedoff")
ggplot(PRating_status,aes(x=as.factor(ProsperRating..Alpha.),
                          y=total*100/total_by_status))+
  geom_bar(stat='identity',fill='dark blue',color="black")+
  facet_wrap(~Status)  + xlab("Prosper Loan Rating (Alpha)")+
  ylab("Percentage of loans") + 
  geom_text(aes(y=round(total*100/total_by_status,2)+0.5,
                label=paste0(round(total*100/total_by_status,2),'%')),vjust=0)+
  theme(axis.text = element_text(size = 14),axis.title = element_text(size=14))
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-43-1.png)<!-- -->

Generally we see that loans rated AA,A,B,C and D constitute about 75% of the completed loans, whereas only about 57% in the defaulted loans. The percentage of loans rated E and HR are higher in case of defaulted loans. In all cases, loans rated D constituted the major proportion.



We have seen that both ProsperRating as well as ProsperScores are used to rate a loan. Let us have a look at how they are correlated to each other

```r
PRating_score = loans_post %>% group_by(ProsperScore,ProsperRating..Alpha.) %>% 
  summarize(total=n())

ggplot(PRating_score,aes(ProsperScore,ProsperRating..Alpha.)) + 
  geom_tile(aes(fill=-total))+geom_text(aes(label =total))+
  theme(axis.text = element_text(size = 14),axis.title = element_text(size=14))
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-44-1.png)<!-- -->

We do see a general correlation between the two variables, though it is not the case that a prosper score always directly translates to a corresponding prosper grade. For instance, we see that there are few cases where even a prosper score less than 5 has gotten AA rating, and high prosperscores  (>7) having gotten HR rating. According to Prosper, the prosper rating indicates the expected average annualized loss rate to the investor, whereas prosper score indicates the probability of a loan going 'bad'. However, it is not clear why these two aspects would be different, as in the case of those loans where the prosper score does not seem to match well with the assigned prosper rating. Further investigation would be needed to understand this.


Let us now look at the loan amount according to the loan status.


```r
ggplot(data=loans_deft,aes(x=Status,y=LoanOriginalAmount))+geom_boxplot()+
  theme(axis.text = element_text(size = 14),axis.title = element_text(size=14))
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-45-1.png)<!-- -->

Surprisingly, the defaulted loans are comparable in magnitude in comparison to the non-defaulted loans, as in the case of debt to income ratio (DTI) we saw earlier. Let us also look at some quantile values


```r
loans_deft_grp = loans_deft%>%group_by(Status)
cat("Defaulted\n\n")
```

```
## Defaulted
```

```r
(quantile(filter(loans_deft_grp,Status=="Defaulted")$LoanOriginalAmount))
```

```
##    0%   25%   50%   75%  100% 
##  1000  2900  4500  8000 25000
```

```r
cat("\n\nCompleted\n\n")
```

```
## 
## 
## Completed
```

```r
(quantile(filter(loans_deft_grp,Status=="Completed")$LoanOriginalAmount))
```

```
##    0%   25%   50%   75%  100% 
##  1000  2550  4500  8000 35000
```

The median value of loan amount for defaulted loans is $4500, whereas 75% of the defaulted loans are under $8000. In case of completed loans, the median value is same as that of defaulted loans, with the 75% percentile value is at $8000 as well. 


Based on our investigation so far,we do see a that prosper scores are a good indicator of the chance of a loan going bad. Hence, for the rest of the analysis, let us turn our approach towards identifying what factors lead to a better prosper score.In fact, rather than the investigating the effect of each varible in predicting a loan default, let us just see their effects on prosper score. Note that Prosper uses a proprietary rating system to come up with scores and ratings for their loans. This analysis does not hope to identify their method, however, we might still be able to get some insights as to how the rating is done.


Let us look at the percentage of propser ratings assigned within each category of EmploymentStatus of the customers. In the following analysis, we will be doing several such percentage calculations to find of the share of one variable within a group, grouped by another variable. Hence let us create a generic function for the same


```r
perc_in_group = function(df,grouping_var,perc_var){
  
  df_group = df %>% group_by_(grouping_var) %>% mutate(total_var_1 = n())
  
  df_group_summ = df_group %>%group_by_(grouping_var,perc_var)%>%
    summarize(total_var_1 = first(total_var_1),percentage=n()*100/first(total_var_1))
  return (df_group_summ)
}
```



```r
#To calculate the percentage of different ProsperRatings within each employment status
#loans_by_employment = loans_post %>% group_by(EmploymentStatus) %>% mutate(emp_status_tot = n()) 
loans_by_employment_summ = perc_in_group(loans_post,
                                         "EmploymentStatus","ProsperRating..Alpha.")

ggplot(data=loans_by_employment_summ,aes(x=ProsperRating..Alpha.,y=percentage))+
  geom_bar(stat='identity',fill='dark blue') + facet_wrap(~EmploymentStatus)+
  theme(axis.text = element_text(size = 14),axis.title = element_text(size=14))
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-48-1.png)<!-- -->

```r
#Alternative : 
#dotchart(table(loans$ProsperRating..Alpha.,loans$EmploymentStatus))
```

Some interesting insights here. Retired and Full time employees have the highest chance of getting a AA rating. Interestingly, the share of D-rating is also highest amongst retirees. Over 20% listings from Full time and part time employees have gotten A rating. In general, C and D ratings are the most common across all groups. The percentage of HR ratings are the most amongst unemployed applicants, which shouldn't be surprising. 


We had earlier seen that the difference in debt to income ratio between defaulted and completed loans were not significantly different. Let us see how debt to income ratio influences prosper score. 



```r
ggplot(loans_post,aes(x=as.factor(ProsperScore),y=DebtToIncomeRatio))+
  geom_boxplot()+xlab("ProsperScore")+scale_y_log10(breaks=c(0.1,1,10))+
  theme(axis.text = element_text(size = 14),axis.title = element_text(size=14))
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-49-1.png)<!-- -->

The above plot has y-axis on the logarithmic scale to improve clarity in presence of outliers. Let us take a look at the same plot as above, without the outliers


```r
ggplot(loans_post,aes(x=as.factor(ProsperScore),y=DebtToIncomeRatio))+
  geom_boxplot()+scale_y_continuous(limits = c(0,0.75))+xlab("ProsperScore")+
  theme(axis.text = element_text(size = 14),axis.title = element_text(size=14))
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-50-1.png)<!-- -->

We see a trend of decrease in the median DTI with increase in prosper score, indicating that DTI is probably an important factor considered while calculating the prosper score.


Let us look at the effect of total number of inquiries as well as the inquiries in the last months into the credit profile, on the prosper score.


```r
ggplot(loans_post,aes(x=as.factor(ProsperScore),y=TotalInquiries))+
  geom_boxplot()+
  scale_y_continuous(limits = c(0,50))+xlab("ProsperScore")+
  theme(axis.text = element_text(size = 14),axis.title = element_text(size=14))
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-51-1.png)<!-- -->

We see a minor trend that Lower total inquiries lead to better prosper score. The trend is more obvious in case of Inquiriries in last 6 months (as shown below)


```r
ggplot(loans_post,aes(x=as.factor(ProsperScore),y=InquiriesLast6Months))+
  geom_boxplot()+
  scale_y_continuous(limits = c(0,10))+xlab("ProsperScore")+
  theme(axis.text = element_text(size = 14),axis.title = element_text(size=14))
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-52-1.png)<!-- -->

In fact, we can also look at the percetnage of completed and defaulted loans with at least 5 inquiries, as below

```r
kable(loans_deft %>% group_by(Status)%>%mutate(total=n())  %>%
        filter(InquiriesLast6Months>=5) %>% 
        summarise(Percentage_with_More_than_5_Inquiries = n()*100/first(total)))
```



Status       Percentage_with_More_than_5_Inquiries
----------  --------------------------------------
Completed                                 8.788149
Current                                   2.175095
Defaulted                                20.934744

We also see that the CreditScoreRange affect prosper score, in that a higher credit score range could possible lead to better ProsperScore. This can be seen in the figure below.


```r
ggplot(data=loans_post,aes(x=as.factor(ProsperScore),y=CreditScoreRangeLower))+
  geom_boxplot(color='blue')+ xlab("ProsperScore")+
  theme(axis.text = element_text(size = 14),axis.title = element_text(size=14))
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-54-1.png)<!-- -->

Does being a homeowner affect the prosper score? Apparently not, as is seen below.

```r
ggplot(data=loans_post,aes(x=IsBorrowerHomeowner,y=ProsperScore))+geom_boxplot()+
  theme(axis.text = element_text(size = 14),axis.title = element_text(size=14))
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-55-1.png)<!-- -->

Now let us look at the number of delinquencies in 7 years prior to the date the credit profile was pulled. 

```r
ggplot(data=loans_post,aes(x=as.factor(ProsperScore),y=DelinquenciesLast7Years))+
  geom_boxplot(alpha=1/20)+xlab("ProsperScore")+
  theme(axis.text = element_text(size = 14),axis.title = element_text(size=14))
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-56-1.png)<!-- -->

Although the median delinquencies for all prosperscores is zero, the outliers seems to be showing some pattern. For those loans with delinquencies, the number of delinquencies tends to be lower in case of a higher prosper score.  

The number of tradelines openened in the 6 months prior to the time credit profile was pulled also seems to be having a slight influence on the prosper score, as can be visualized below.

```r
ggplot(data=loans_post,aes(x=as.factor(ProsperScore),y=TradesOpenedLast6Months))+
  geom_boxplot(alpha=1/20)+xlab("ProsperScore")+
  theme(axis.text = element_text(size = 14),axis.title = element_text(size=14))
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-57-1.png)<!-- -->

The income bracket into which the applicant falls into also affects the prosper score, which is shown below.

```r
ggplot(loans_post,aes(x=IncomeRange,y=ProsperScore))+geom_boxplot()+
  theme(axis.text = element_text(size = 14),axis.title = element_text(size=14))
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-58-1.png)<!-- -->

Higher income brackets tend to get a higher ProsperScore, which is afterall expected. 

So far we have identified few factors influencing Prosper Score - DTI, TotalInquiries, Inquirieslast7 months, deliquencies last 7 years,CreditScoreRange and IncomeRange. Now it is time to do some multivariate analysis to identify the combined influence of these above variables on a loan default.


#Multivariate Analysis

With our bivariate analyses having identified the influence of some variables in determining the prosper scores, it is time to look at some multivariate plots. Multivariate plots  are helpful in visualizing the distribution of datapoints, segregated according to the loan default status. These might help us understand the combined influence of two (or more) variables in causing a loan default. We have observed so far that variables such as DTI, TotalInquiries, Inquirieslast7months, delinquencieslast7years have some influence on the prosper score, which in turn predicts the chances of a loan defaulting. Hence, let us now use multivariate plots to see their combined influence.

Let us start by looking at the distribution of debt to income ratio vs credit score Range for both categories of loans. It would be easier to visualize the data if we subset our dataframe to include only the completed and defaulted loan listings. Let us do that now

```r
loans_deft_sub = filter(loans_deft,Status %in% c("Completed","Defaulted"))
```

Let us first look at the effect of DebtToIncomeRatio and CreditScoreRangeLower on Loan Default Status**

```r
ggplot(loans_deft_sub,aes(y=DebtToIncomeRatio,
                          x=CreditScoreRangeLower,color=Status))+
  geom_jitter(aplha=0.5)+theme(axis.text = element_text(size = 14),
                               axis.title = element_text(size=14))
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-60-1.png)<!-- -->

It is hard to say anything concreterly from this plot, except that the applicants with completed loans tend to have a generaly higher credit score associated with their credit profile. Most of the debt to income (DTI) ratios are below 1.25. As for higher DTI values, we see both completed and defautled loans distributed across the data set. It appears as if irrespective of the DTI, a lower customer credit score have much higher chances of default.

Let us now look at the combined effect of Credit Score and number of delinquent accounts at the time of application


```r
ggplot(loans_deft_sub,aes(y=CurrentDelinquencies,
                          x=CreditScoreRangeLower,color=Status))+
  geom_jitter(alpha=0.5)+scale_y_continuous(limits=c(0,20))+
  theme(axis.text = element_text(size = 14),axis.title = element_text(size=14))
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-61-1.png)<!-- -->

Couple of observations here. Generally customers with higher score associated with their profiles tend to have lower delinquencies. Also, the chances of default are much higher in case of customers with lower CreditScore and higher delinquencies. Similar inferenes can be drawn about the delinquences of customers in the past 7 years as well, as can be seen from below


```r
ggplot(loans_deft_sub,aes(y=DelinquenciesLast7Years,
                          x=CreditScoreRangeLower,color=Status))+
  geom_jitter(alpha=0.5)+theme(axis.text = 
                                 element_text(size = 14),axis.title = element_text(size=14))
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-62-1.png)<!-- -->


Let us now analyse the combined effect of Debt to Income Ratio (DTI) and Delinquencies.

```r
p1 = ggplot(loans_deft_sub,aes(y=DebtToIncomeRatio,
                               x=CurrentDelinquencies,color=Status))+
  geom_jitter(alpha=1/2,size=3)+theme(axis.text = 
                                        element_text(size = 14),axis.title = element_text(size=14))
p2 = ggplot(loans_deft_sub,aes(y=DebtToIncomeRatio,
                               x=DelinquenciesLast7Years,color=Status))+
  geom_jitter(alpha=1/2,size=3) +theme(axis.text = 
                                         element_text(size = 14),axis.title = element_text(size=14))
grid.arrange(p1,p2)
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-63-1.png)<!-- -->

We can see that among the folks with low DTI, those with more number of delinquent accounts at the time of application are more likely to default. The trend is not so clear in case of delinquenies in the past 7 years, however we can still say that a combination of high DTI and more delinquencies are will most certainly lead to default. 

Let us now take the combined effect of Delinquencies and Inquiries into the credit profile


```r
p1 = ggplot(loans_deft_sub,aes(x=InquiriesLast6Months,
                               y=CurrentDelinquencies,color=Status))+
  geom_jitter(alpha=1/2) +theme(axis.text = element_text(size = 14),axis.title = element_text(size=14))
p2 = ggplot(loans_deft_sub,aes(x=InquiriesLast6Months,
                               y=DelinquenciesLast7Years,color=Status))+
  geom_jitter(alpha=1/2)+theme(axis.text = element_text(size = 14),axis.title = element_text(size=14))
grid.arrange(p1,p2)
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-64-1.png)<!-- -->

Similar to the case above, we can say higher number of inquiries with higher delinquencies are probably going to default. Generally it seems that the percentage of appliants with higher number of inquiries are larger among the defaulters than the completed ones

Now, to the effect of Credit Score and number of trade lines opened in the last 6 months. I have filtered the data to include those cases with at least two trade lines opened, for better clarity


```r
p1 = ggplot(filter(loans_deft_sub,TradesOpenedLast6Months>=2),
            aes(x=CreditScoreRangeLower,y=TradesOpenedLast6Months,color=Status))+
     geom_jitter(alpha=1/2)+theme(axis.text = element_text(size = 14),axis.title = element_text(size=14))
p1
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-65-1.png)<!-- -->

Again applicants with low credit score + more trade lines opened are more prone to default. Whereas high credit score and more trade lines opened are slightly more likely to complete.



```r
p1 = ggplot(loans_deft_sub,aes(x=CreditScoreRangeLower,
                               y=CurrentCreditLines,color=Status))+
  geom_jitter(alpha=1/2)+theme(axis.text = element_text(size = 14),axis.title = element_text(size=14))
p1
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-66-1.png)<!-- -->

We can say that for a given number of credit lines, a customer with higher credit score is more likely to complete the loan. This is especially true for the cases where the number of credit lines is less than 30. As the number of credit lines increases furhter this trend becomes less clear.

#Final Plots and Summary

##PLOT 1: Historical Growth of the Prosper Business


```r
ggplot(data=loans_deft,aes(x=LoanQuarter,fill=Status))+geom_bar()+
  theme(axis.text.x=element_text(angle=45)) + 
  ylab("Nummber of Loans") + geom_line(data=loans_defaulted_quarterly,
                                     aes(y=perc*250,group=1),color='blue') + 
  scale_y_continuous(sec.axis = sec_axis(~./250, name = "Default Rate (%)"))+
  ggtitle('Growth of Prosper Business over time')
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-67-1.png)<!-- -->

The above plots combines two plots shown earlier to show how the Prosper Business has fared over the years. The stacked bars shows the number of loans made in each quarter, colored according to their Status. The blue line shows the historical trend of Default rate, calculated as the number of loans defaulted over the total number of loans originated in the quarter.

The historical progress of Prosper Business was one of the primary questions I wished to answer through this EDA process. The above figure tells us that the prosper business didnt take off so much in volume in the intial period, particularly over the 4 year period 2005-2009. Also, the period 
period 2008 Q4 - 2009 Q3 saw a severe reduction in their activity level, but perhaps that was due to some major changes being implemented to the propser business at the time. We can suspect so since we already know that the propser rating system got revamped in July 2009, and perhaps that was just a part of a larger restructuring process.

We can see from the above graph that the number of prosper loans  has been increasing from 2009 for the most part, with the business really taking off from Q2 2013. We can also see the historical trend of loan defaults, which is a critical metric to watch for with regard to the sucess of the Prosper Business. A dip in the default rates is observed after Q2 2008, although this could be due to the fact that the number of loans originated in Q2 of 2009 is very low. However the loans did start increasing again in number from Q3 of 2009 and yet we observe that the default rates are lower for the loans originated during the Q3 2009 to Q4 2010, in comparison to the pre 2009 era. The default rate line is only helpful for the period until 2010 Q4, since some of the loans originated from 2011 Q1 are still on-going, which makes default rate shown less meaningful. As such, though it is difficult to conclude that default rates have lowered since 2009, we can suspect this is indeed the case, from whatever data that is available.

##PLOT 2: Effectiveness of ProsperScore variable in predicting the chance of a loan defaulting


```r
ggplot(filter(PRscore_status,is.na(ProsperScore)!=T),aes(x=Status,y=total*100/total_by_status)) + geom_bar(stat='identity',fill='dark blue',color="white") + facet_wrap(~as.factor(ProsperScore))  + xlab("Loan Status") + ylab("Percentage of loans") + geom_text(aes(y=round(total*100/total_by_status,2)+0.5,label=paste0(round(total*100/total_by_status,2),'%')),vjust=0) +
ggtitle("Prosper Score") + theme(plot.title = element_text(hjust = 0.5))
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-68-1.png)<!-- -->

The second plot was chosen to be this, because it answers our second major question at the beginning of the analysis, i.e, how good is Prosper in identifying good vs bad loans. The above plot somewhat answers this question. We see that as the prosper score increases, the chances of a loan defaulting continually keeps going down, indicating that the metric is quite effective in predicting a bad loan. In fact, identifying this fact inspired this analysis to take a direction towards identifying those factors that contribute to better Prosper Score (and consequently, better loans). Such an analysis led us to identifying few variables that helps improve the prosper score, namely, Debt To Income Ratio (DTI), Total Inquiries into the customer's  credit profile at the time it was pulled, Inquiries into the customer's credit profile in the 7 months prior to the evaluation of the loan application, number of deliquencies in the  last 7 years and IncomeRange of the customers. 

##PLOT 3: Multivariate visualization of variables influencing Loan Default


```r
p1 = ggplot(loans_deft_sub,aes(y=CurrentDelinquencies,
                               x=CreditScoreRangeLower,color=Status))+
  geom_jitter(alpha=0.5)+scale_y_continuous(limits=c(0,20)) +
  scale_x_continuous(limits=c(250,1000))
p2 = ggplot(loans_deft_sub,aes(y=DelinquenciesLast7Years,
                               x=CreditScoreRangeLower,color=Status))+
  geom_jitter(alpha=0.5)+scale_x_continuous(limits=c(250,1000))
p3 = ggplot(filter(loans_deft_sub,TradesOpenedLast6Months>=2),
            aes(x=CreditScoreRangeLower,y=TradesOpenedLast6Months,color=Status))+
  geom_jitter(alpha=1/2)+scale_x_continuous(limits=c(250,1000))
p4 = ggplot(loans_deft_sub,aes(x=CreditScoreRangeLower,
                               y=CurrentCreditLines,color=Status))+
  geom_jitter(alpha=1/2) +scale_x_continuous(limits=c(250,1000))


grid.arrange(p1,p2,p3,p4,ncol=1,
             top=textGrob("Combined influence of variables affecting loan default",
                            gp=gpar(fontsize=17,font=3)))
```

![](EDA_prosper_final_files/figure-html/unnamed-chunk-69-1.png)<!-- -->

The above plot, which is a combination of 4 multivariate plots was chosen for a numbrer of reasons. For one, it shows the effect of some of the variables that affect the prosper score, on the probability of a loan defaulting. Secondly, all the 4 sub-plots in the above plot seems to show that the variable CreditScoreRangeLower, which shows the lower limit of the Customer's credit score, is a very important predictor of the chance of a loan defaulting, perhaps more so than the other variables. In all the above 4 subplots we se that it is the the CreditScoreRangeLower variable which acts more like a dominant factor, influencing the default chance more than the other variable.


##Reflection

This has been one of the more challenging data exploration projects amongst the ones I have done so far. With 113937 rows and 81 columns, this was not exactly a small dataset. With 81 different variables, there are innumerable number of questions one can ask this data. This meant that it was easy to get lost amongst a bunch of plots if one does not identify a specific theme for their analysis, i.e., develop a central idea as to what exactly one should look from  from the dataset. Hence, I identified a few central questions I wished to be answered from the data, which in a broad sense were - the growth of propser business over time, identifying the types of customers who are most likely to default, and the ability of prosper to identify the good vs bad loans. Hence, after  getting familiar with the dataset through some univariate analysis, the rest of the data exploration was done keeping these ideas in mind. Having developed such a theme for the analysis helped me to stay on track without wandering off, and develop a coherent story from this work.

However, this alone did not make the analysis simple, and there were several other challenges that came up throughout the course of this work. For instance, the variable definitions were not clear enough for someone like myself who had no prior knowledge of the Prosper business (or even P2P lending schemes for that matter). However, as data analysts, this should be considered normal, as we are expected to pick up the background knowledge and domain information regarding the problem before and during the process of data analysis. Hence, during the course of this analysis, I read more about peer to peer lending schemes, the meaning of technical terms commonly used in the financial domain (such as delinquency, trade lines etc.,), specific details of the Prosper Enterprise and so on. Apart from that, even during the analysis process, the data manipulation and the plotting operations done to generate all the plots in this report did not work right away, and involved a lot of  trial and errors. Special thanks to google and especially to the numerous questions answered at stackoverflow, all of which helped immensly while doing this work.

This work is only the tip of the iceberg when it comes to the number of things that can be done with this dataset. For instance, of the 81 variables, only a small subset has been explored properly in this work. This was done intentionally to keep this work brief and concise. There were 2-3 central questions that were investigated, and all the sub-questions were connected to this theme. We can think of a number of other questions to be explored and answered with this dataset. To give an example, some directions that were not taken up involves the geographical distribution of the customers. Why are Californians overwhelmingly large in number? How has the geographical distribution of customers been changing with time? Consequently are there any patterns to the growth of customers across that states? Can we find some information from the data set that can help Prosper increase their popularity in the states where they are not quite popular yet? This is just one direction in which we can take the analysis, and there are for sure other themes to be explored. Then there are other things that can be done as an extension of the current work itself, without having to do any major change of theme of the analysis. For instance, one question that I have still not answered (or attempted to answer) includes identifying the difference between ProsperScore and ProsperRating. Why are some loans assigned high ProsperScore, despite given a high risk prosper rating, and vice-versa? Besides these, there are other things that can be done as well. Prosper uses a proprietary method to rate their loan applications and assign prosper rating. Can we replicate the same using Machine learning algorithms? If not that, can we at least develop a binary classification algorithm to predict which loans will default and which will be completed without delay?

This work turned out to be more challenging that I expected it to be when I chose to work on it. Nevertheless, the learning experience has been fullfilling, and I'm glad that I have made it so far with this analysis. It is only through challenges that one improves his or her skill, since tackling easy problems does not make you a better problem solver. Being a beginner data analyst, I'm sure there are plenty of shortcomings with this work, however, having been able to do at least as much as has been done in this work has improved my confidence immensly, and I believe I am better prepared now to tackle new datasets with even larger number of records and variables in the future.

####References:
1) https://www.prosper.com
2) https://www.thebalance.com/before-you-borrow-at-prosper-com-315590


