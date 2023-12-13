## Title: package.final

## Author: Jiaxin Huang and Hanmo Li

## Description:

The package includes complete documentation and detailed vignette with examples for t-test, linear regression, and k-nearest neighbors cross-validation functions.

## Exported functions:

1\. my_t.test(): evaluates if the value of the given numeric vector is significantly different from the null value (mu).

```{r}
helium_data <- read.csv("<https://www.openintro.org/data/csv/helium.csv>")
my_t.test(x = helium_data$helium, alternative = "two.sided", mu = 20)
```

2\. my_lm(): fits a linear regression model to given data

```{r}
grades_data <- read.csv("<https://www.openintro.org/data/csv/gpa.csv>") 
my_lm(gpa ~ studyweek, grades_data)
```

3\. my_knn_cv(): performs k-nearest neighbors cross-validation test to predict class using given covariates

```{r}
peng_clean <- na.omit(my_penguins) 
my_knn_cv(peng_clean[, 3:6], peng_clean$species, 1, 5)
```

## URL to original R package:

<https://github.com/jh277/project3>

## URL to the deployed website:

<https://jhu-statprogramming-fall-2023.github.io/biostat777-project3-part1-jh277/>

## 5 things I've customized in pkgdown website:

1.  I changed the navbar height to 200px.
2.  I changed the theme to "minty".
3.  I changed the structure of navbar (removed tutorials and news sections from the left, and github section from the right).
4.  I changed the default fonts for main text to Montserrat
5.  I deleted sidebar
