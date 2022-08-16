
---
title: "Coronavirus Project"
output: html_document
date: '2022-07-28'
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
# Investigating COVID-19 Trends By Country

## Introduction

My goal here is to investigate the number of COVID-19 tests performed daily against the number of positive tests by country as an indicator of which countries have the highest testing rate.  This has a number of uses such as giving guidence on which countries governments should advise travelers be on  high alert when visiting, suggesting where resources should be allocated to help reduce the number of cases and providing data for predicting future pandemic trends.

The data set I am using can be found [here](https://www.kaggle.com/datasets/lin0li/COVID19testing).  It is an open source dataset with data from over 100 countries spanning from 05/03/2020 to 05/11/2020.  Due to the time frame, conclusions drawn have limited applications to current dates as these were before the vaccine was available.


## Import and Investigate

First we use the read_csv function from the reader package to import the COVID19 dataset.


```{r}
library(readr)
covid_df <- read_csv("C:/Users/alice/Downloads/COVID19.csv")

```

Now we have our dataset saved as `data` we can explore, to find out the length of the dataset, the type of data in each column and any values that may interfere with our analysis.  

From the `read_csv` function output we can see that our data has 10903 rows and 14 columns.  Now we will use further functions to investigate the above queries.  

The `dim` function returns the dimensions of the dataset. The `colnames` function retrieves the column names for our dataset.  The `head` function retrieves the first n rows of the dataset, which allows us to have a brief look at the data.  Here, I set n to 5 to give us a brief look at the data.  I install below the `dplyr` library - the grammar of data manipulation - which allows several useful functions which I will utilize through this project.  Here I installed to use the `glimpse` function, which gives us a brief overview of our dataset.  This shows column names, data type and the first few values.

```{r}
dimensions <- dim(covid_df)
vector_cols <- colnames(covid_df)
head(covid_df,5)
library(dplyr)
glimpse(covid_df)

```
Now, we can look at our 14 columns in more detail.  
1. `Date`
      i) Date, data type date
2. `Continent_Name`
      i) Continent Name, data type Character
3. `Two_Letter_Country_Code`
      i) Country Code, data type character
4. `Country_Region`
      i) Country Names, data type character
5. `Province_State`
      i) State or province name, will show `All States` when not available, data type character
6. `positive`
      i) Number of reported positive cases, data type double
7. `active`
      i) Number of active cases on specified date, data type double
8. `hospitalized`
      i) Cumulative number of hospitalized cases reported, data type double
9. `hospitalizedCurr`
      i) Number of current hospitalized cases on date, data type double
10. `recovered`
      i) Cumulative number of reported recovered cases, data type double
11. `death`
      i) Cumulative number of reported deaths, data type double
12. `total_tested`
      i) Cumulative number of tests reported, data type double
13. `daily_tested`
      i) Number of tests conducted on specified date.  If unavailable, this figure is the average of dates in between, data type double
14. `daily_positive`
      i) Number of positive cases reported on specified day. If unavailable, this figure is the average of dates in between, data type double
      
## Data Cleaning
      
From the overview given by the `head` function we can see that data from different countries is assigned the `All States` value in the `Province_State` column.  This is mixing data from different levels and may confuse our analysis.

In order to not skew our analysis I will keep only rows with `All States`, as these are at a country level.  Below, I filter for rows with `All States` and then remove `Province_State` from the dataset.  This is because all values for this column are now the same so it is made redundant.

```{r}

covid_df_all_states <- covid_df %>%
  filter(Province_State == "All States") %>%
  select(-Province_State)

```
Now I have removed data at the state level and are left with data at a country level, it is clear that we have data showing daily figures (e.g. `daily_tested`) and cumulative figures (e.g.'total tested`).

As these two types of data cannot be compared without distorting the analysis, I need to isolate the columns related to daily measures.

These are:
* `Date`
* `Country_Region`
* `active`
* `hospitalizedCurr`
* `daily_tested`
* `daily_positive`

``` {r}

covid_df_all_states_daily <- covid_df_all_states %>%
  select(Date, Country_Region, active, hospitalizedCurr, daily_tested, daily_positive) %>%
  print

```
## Extract Top Ten Cases Countries

My goal in this project is to extract the top ten cases countries data.  Do do this I need to identify how to get the overall number of COVID-19 tested, positive, active and hospitalized by country and then extract the top ten cases.

To do this, I need to use the `group_by` and `summarize` functions to get summary statistics by country and then use the `arrange` and `head` functions to identify the top ten rows.

``` {r}

covid_df_all_states_daily_sum <- covid_df_all_states_daily %>%
  group_by(Country_Region) %>%
  summarize(
    tested = sum(daily_tested),
    positive = sum(daily_positive),
    active = sum(active),
    hospitalized = sum(hospitalizedCurr)
  ) %>%
  arrange(-tested)

covid_top_10 <- head(covid_df_all_states_daily_sum, 10) %>%
  print

```
### Which countries have had the highest number of positive cases against the number of tests?

To answer this question I will create vectors of the values in the following columns from the covid_top_10 dataframe:
* `Country_Region`
* `tested`
* `positive`
* `active`
* `hospitalized`

I will then name the final four vectors by the country names and use this to identify the top three ratio by dividing positive cases by tested cases.
```{r}
countries <- covid_top_10$Country_Region
tested_cases <- covid_top_10$tested
positive_cases <- covid_top_10$positive
active_cases <- covid_top_10$active
hospitalized_cases <- covid_top_10$hospitalized

names(tested_cases) <- countries
names(positive_cases) <- countries
names(active_cases) <- countries
names(hospitalized_cases) <- countries

positive_tested_top <- covid_top_10 %>%
  mutate(
    positive_against_tested_ratio = positive_cases / tested_cases) %>%
  arrange(-positive_against_tested_ratio)

head(positive_tested_top, 3)

positive_tested_top_3 <- c("United Kingdom" = 0.11,"United States" = 0.10,"Turkey" = 0.08)

```
We can see from the above that the United Kingdom (0.11), United States (0.10) and Turkey (0.08) had the highest ratios.

### Create Matrix of Top Three Countries

Now that we have identified our top three countries, we want to extract information on these and store it in a matrix.

First we create vectors for each contry containing important information, then we combine this to create a matrix and name each column.

```{r}
united_kingdom <- c(0.11, 1473672, 166909, 0, 0)
united_states <- c(0.10, 17282363, 1877179, 0, 0)
turkey <- c(0.08, 2031192, 163941, 2980960, 0)

covid_mat <- rbind(united_kingdom, united_states, turkey)
colnames(covid_mat) <- c("Ratio", "tested", "positive", "active", "hospitalized")
```

### Conclusion

Now that we have isolated answers we can combine these together to give us a coherent conclusion from the data.

Below we combine our dataframes, lists, matrices and vectors to determine the answer to our question.

```{r}
question <- "Which countries have had the highest number of positive cases against the number of tests?"
answer <- c("Positive tested cases" = positive_tested_top_3)
df_list <- list(covid_df,
                covid_df_all_states,
                covid_df_all_states_daily,
                covid_top_10)

matrix_list <- list(covid_mat)
vector_list <- list(vector_cols, countries)
names(df_list) <- c("dataframes", "matrices", "vectors")


covid_analysis_list <- list(question, answer, df_list)

covid_analysis_list[[2]]
```
From  the above we can conclude that the countries with the highest number of positive cases against tested cases was the United Kingdom, followed by the United States and Turkey.