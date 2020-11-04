---
title: "Tuberculosis analysis"
author: "Łukasz Pięta"
date: "11/2/2020"
output: 
  html_document: 
    keep_md: yes
    df_print: kable
    code_folding: "hide"
    toc: true
    toc_float: true
---


### Libraries

```r
library(dplyr)
library(knitr)
library(ggplot2)
library(reshape2)
library(EDAWR)
```

### Read data

```r
df <- tb
```

### Data summary


```r
kable(summary(df))
```



|   |  country        |     year    |    sex          |    child       |    adult      |   elderly       |
|:--|:----------------|:------------|:----------------|:---------------|:--------------|:----------------|
|   |Length:3800      |Min.   :1995 |Length:3800      |Min.   :    0.0 |Min.   :     0 |Min.   :     0.0 |
|   |Class :character |1st Qu.:1999 |Class :character |1st Qu.:   25.0 |1st Qu.:  1128 |1st Qu.:    84.5 |
|   |Mode  :character |Median :2004 |Mode  :character |Median :   76.0 |Median :  2589 |Median :   230.0 |
|   |NA               |Mean   :2004 |NA               |Mean   :  493.2 |Mean   : 10864 |Mean   :  1253.0 |
|   |NA               |3rd Qu.:2009 |NA               |3rd Qu.:  264.5 |3rd Qu.:  6706 |3rd Qu.:   640.0 |
|   |NA               |Max.   :2013 |NA               |Max.   :25661.0 |Max.   :731540 |Max.   :125991.0 |
|   |NA               |NA           |NA               |NA's   :396     |NA's   :413    |NA's   :413      |

### Tuberculosis cases grouped by gender


```r
group_by_sex_summary <- df %>%
    filter(!is.na(child) & !is.na(adult) & !is.na(elderly)) %>%
    mutate(all=child + adult + elderly) %>%
    group_by(sex) %>%
    summarise(cases = sum(all)) 

kable(group_by_sex_summary)
```



|sex    |    cases|
|:------|--------:|
|female | 15610599|
|male   | 27016456|

### Tuberculosis cases grouped by age


```r
gruped_by_age_and_year <- df %>%
    filter(!is.na(child) & !is.na(adult) & !is.na(elderly)) %>%
    group_by(year) %>%
    summarise(child_cases = sum(child) / 1000, adult_cases = sum(adult) / 1000, elderly_cases = sum(elderly) / 1000)

 d <- melt(gruped_by_age_and_year, id.vars="year")
 
ggplot(d, aes(year,value, col=variable)) + 
    geom_line() +
    labs(x = "Year", y = "Cases [thousands]", color = "Legend") +
    theme_linedraw() +
    scale_x_continuous(breaks = seq(min(d$year), max(d$year), by = 2))
```

![](tuberculosis-analysis_files/figure-html/plot_age-1.png)<!-- -->

### Tuberculosis cases grouped by age and country


```r
single_graph <- function(df, group) {
  melted <- melt(df, id.vars="year")
  
  print(ggplot(melted, aes(year,value, col=variable)) + 
    geom_line() +
    labs(x = "Year", y = "Cases", color = "Legend") +
    ggtitle(group$country[1]) +
    theme_linedraw() +
    scale_x_continuous(breaks = seq(min(df$year), max(df$year), by = 2))
  )
}
```




```r
grouped_by_age_country_year <- df %>%
    filter(!is.na(child) & !is.na(adult) & !is.na(elderly)) %>%
    group_by(country, year) %>%
    summarise(child_cases = sum(child), adult_cases = sum(adult), elderly_cases = sum(elderly)) %>%
    group_by(country) 

    invisible(group_map(grouped_by_age_country_year,single_graph))
```

![](tuberculosis-analysis_files/figure-html/plot_age_country-1.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-2.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-3.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-4.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-5.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-6.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-7.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-8.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-9.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-10.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-11.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-12.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-13.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-14.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-15.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-16.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-17.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-18.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-19.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-20.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-21.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-22.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-23.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-24.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-25.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-26.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-27.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-28.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-29.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-30.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-31.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-32.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-33.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-34.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-35.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-36.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-37.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-38.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-39.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-40.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-41.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-42.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-43.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-44.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-45.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-46.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-47.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-48.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-49.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-50.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-51.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-52.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-53.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-54.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-55.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-56.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-57.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-58.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-59.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-60.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-61.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-62.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-63.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-64.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-65.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-66.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-67.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-68.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-69.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-70.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-71.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-72.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-73.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-74.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-75.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-76.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-77.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-78.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-79.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-80.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-81.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-82.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-83.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-84.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-85.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-86.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-87.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-88.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-89.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-90.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-91.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-92.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-93.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-94.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-95.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-96.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-97.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-98.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-99.png)<!-- -->![](tuberculosis-analysis_files/figure-html/plot_age_country-100.png)<!-- -->



