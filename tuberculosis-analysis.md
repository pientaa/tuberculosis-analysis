---
title: "Tuberculosis analysis"
author: "Łukasz Pięta"
date: "11/2/2020"
output: 
  html_document: 
    keep_md: yes
    df_print: kable
    code_folding: "hide"
---
### Libraries


```r
library(dplyr)
library(knitr)
#install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)
#devtools::install_github("rstudio/EDAWR")
library(EDAWR)
```

### Read data

```r
original_data <- tb
```

### Data summary


```r
kable(summary(original_data))
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

### Tuberculosis cases grouped by sex


```r
group_by_sex_summary <- original_data %>%
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
tb %>%
    filter(!is.na(child) & !is.na(adult) & !is.na(elderly)) %>%
    group_by(year) %>%
    summarise(child_cases = sum(child), adult_cases = sum(adult), elderly_cases = sum(elderly)) %>%
    mutate(child_cases = child_cases / 1000) %>%
    select(year, child_cases) %>% plot(type="b")
```

![](tuberculosis-analysis_files/figure-html/plot_age-1.png)<!-- -->

### Tuberculosis cases grouped by age and country


```r
# TODO
```
