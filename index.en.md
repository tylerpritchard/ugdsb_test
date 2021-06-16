---
title: F Distribution
author: Tyler Pritchard
date: '2021-04-05'
slug: []
categories: []
tags:
  - Statistics
subtitle: ''
summary: ''
authors: []
lastmod: '2021-04-05T11:04:43-04:00'
featured: no
image:
  caption: ''
  focal_point: ''
  preview_only: yes
projects: []
---



### Front Matter

I remember hearing about the *F*-test during my third year undergraduate statistics class. I enjoyed statistics courses, more so than the average psychology student, at least I believed so. I felt comfortable with the equations to calculate *SSE*, *MSB*, and so on, but I never gave much thought about why a certain *F* value was considered statistically significant. In fact, we were never taught about what p-values mean during my undergrad (2012-2015). Well, if you are an undergrad - or anyone really, I won't discriminate - you are in luck! I will (hopefully) enlighten you about the F-distribution. Before we dive into the F-distribution, we must better understand the underpinning: the chi-square distribution. 

### The Chi-sqauare Distribution

Consider an independent and normally distributed variable: IQ. Let us take a random sample of four 20-year-olds and measure their IQ. Perhaps their scores are:


```
##       name  iq
## 1   Alicia  85
## 2   Ramona 108
## 3    Marie  91
## 4 Julianna 108
```

Given that we know the mean IQ is 100 with a standard deviation of 15, we can standardize this variable by converting them to z-scores. To convert to a z-score, we subtract each score from the mean and divide by the standard deviation: $z = \frac{(\overline{x}-\mu)}{SD}$. Thus, our z-scores are:


```r
IQ <- IQ %>% 
  mutate(z = (iq - 100)/15)
head(IQ)
```

```
##       name  iq          z
## 1   Alicia  85 -1.0000000
## 2   Ramona 108  0.5333333
## 3    Marie  91 -0.6000000
## 4 Julianna 108  0.5333333
```

So, Alicia has a z-score of  -1.0 (i.e., 1 SD below the mean) and Ramona about 0.5 (i.e., about 0.5 SD above the mean). We can also call these z-scores *standard deviates*. They deviate around the mean in standard distribution. These are also known as independent and identically distributed variables ([IID](https://en.wikipedia.org/wiki/Independent_and_identically_distributed_random_variables)). We can calculate the sum of squares, $SS = \sum_{i=1}^n(x_n-\mu)^2$ for any given number of observations. That is, we take each score, subtract it from the mean, square it, and add them all up. Because the standardized distribution has a mean of $0$ and a standard deviation os $1$, the sum of squares of our sample is, simply, our squared z-scores:


```r
IQ %>% 
  mutate(iqss = (z)^2) %>% 
  summarise("Sum of Squares" = sum(iqss))
```

```
##   Sum of Squares
## 1       1.928889
```

So...why does all this matter? A chi-square value is the sum of squares of standard deviates. The chi-square distribution is the distribution of the sum of squares of a given number of deviates, our *df*, under repeated (infinite) samples. So, if we randomly sampled four people an infinite number of times, the sum of squared deviates would form a chi-square distribution with four *df*. Let's simulate this. First, here is what we aim to create using simulated data:


```r
chiexample <- data.frame(x = seq(0,20, .01)) %>% 
  mutate(density=dchisq(x, 4)) 

ggplot(chiexample, aes(x=x, y=density))+
geom_line(color="deepskyblue4")+
theme_minimal(12)+
labs(title="Chi-square Distribution (df=4)", x="Chi-square", y="Density")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

We will generate 10,000 random samples of four individuals. We will calculate the chi-square value for each sample using the sum of their squared standard deviate (i.e., mean of 0 and standard deviation of 1). We can use a function:


```r
our_function <- function(a){
  sample <- data.frame(z = rnorm(n = a, mean = 0, sd = 1)) %>% 
    mutate(z2 = z^2)
  
  print(sum(sample$z2))
}
```

and then generate 10,000 samples:


```r
set.seed(2837)
ourdata <- data.frame(lapply(rep(4, each=10000), 
                          our_function)) %>% 
  t() %>% 
  data.frame() %>% 
  rename("ss"=".")
```

##### Disclaimer: I know the `rchisq` exists, but have created a function for demonstrative purposes.

We can plot the chi-square values in a histogram; I will overlay the above chi-square density line *over* our graph.


```r
ggplot(data = ourdata, aes(ss)) + geom_histogram(aes(y = ..density..), color = "black", fill = "white", binwidth = 0.5) + 
    geom_line(data = chiexample, aes(x = x, y = density), color = "deepskyblue4") + labs(title = "Chi-square Distribution (df=4)", 
    x = "Chi-square", y = "Density") + theme_minimal(12) + geom_segment(x = 5.16, xend = 10, y = 0.1, yend = 0.15, 
    color = "gold") + annotate("text", label = "Our sample from above", x = 10, y = 0.155) + theme(text = element_text(family = "Avenir"))
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

That is gorgeous. The histogram aligns well with the density function. 

The shape of the chi-square distribution depends on one parameter: the number of observations drawn or degrees of freedom (*df*). The mean of the distribution is equal to its df (i.e., for our above data the mean is `round(mean(ourdata$ss), 2)`, approximately 4. Furthermore, the distribution approaches a normal distribution as df increases.  Here is the distribution with some other *df*:


![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

As you can see, the distribution approaches normality as the df increases. 

So, let us consider the components that make up a chi-square value: the sum of squares (SS):
$SS = \sum_{i=1}^n(x_n-\mu)^2$. We can scale the SS to estimate variance. Recall that variance is the sum of squares deivided by number of observations:

$\sigma^2 = \frac{\sum_{i=1}^n(x_n-\mu)^2}{n}$

or an unbiased estimate in the sample:

$s^2 = \frac{\sum_{i=1}^n(x_n-\mu)^2}{n-1}$

Thus, we can scale the chi-sqare by dividing by *df*, which will give us estimate of a variance.

### The F-distribution

The F-distribution is a ratio of two scaled (i.e., divided by their *df*) chi-square distributions. We can see the similarities between a scaled chi-square:

$\chi^2 = \frac{\sum_{i=1}^n(x_n-\mu)^2}{n}$

and the equation for population variance:

$\sigma^2 = \frac{\sum_{i=1}^n(x_n-\mu)^2}{n}$

Thus, the F-distribution is appoximately the ratio of two variances. What are these variances? Well, it is the ratio of the variance of means of the IV group (mean squared between; $MSB$) and variance between each score within a group with that group's mean (i.e., mean squared error/within; $MSE/MSW$). If the groups do not differ (i.e., they are randomly sampled from the sample populuation), the variances should be approximately equal. Given that they are from the same random and indepdenent population, the resulting F-distribution should be the ratio of chi-squares. Let's simulate it. Our goal will be to recreate the following F-distribution density graph:


```r
f <- data.frame(f=seq(0, 10, .01)) %>% 
  mutate(density=df(f, 4, 36))

ggplot(data=f, aes(x=f, y=density))+
  geom_line()+
    labs(title="F Distribution (df1=4, df2=36)", x="F", y="Density")+
  theme_minimal(12)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)

We will reuse our extant chi-square distribution and divide by a new chi-square distribution with 36 degrees of freedom. What do I mean by divide? Without using fancy calculus, I will divide 1 of each distribution by 1 of the other. Although this is not an exact estimation, it should be decent with 10'000 samples each. Here is our simulated chi-square (df=36) distribution:


```r
set.seed(238873)
ourdata36 <- data.frame(lapply(rep(36, each=10000), 
                          our_function)) %>% 
  t() %>% 
  data.frame() %>% 
  rename("ss"=".")

chiexample2 <- data.frame(x = seq(0,80, .01)) %>% 
  mutate(density=dchisq(x, 36)) 
```


```r
ggplot(data=ourdata36, aes(ss))+
  geom_histogram(aes(y=..density..), 
                 color="black", 
                 fill="white",
                 binwidth = .5) +
  geom_line(data=chiexample2, aes(x=x, y=density),
            color="deepskyblue4")+
  labs(title="Chi-square Distribution (df=36)", x="Chi-square", y="Density")+
  theme_minimal(12)+
  theme(text=element_text(family="Avenir"))
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)

Let's combine the data and scale them appropriately.


```r
chi4 <- ourdata %>% 
  mutate(df="4", scaled=ss/4, id=1:10000)

chi36 <- ourdata36 %>% 
  mutate(df="36", scaled=ss/36, id=1:10000)

datafinal <- rbind(chi4, chi36)
```

Let's look at each scaled distribution:


```r
ggplot(datafinal, group=df)+
  geom_histogram(aes(x=scaled, fill=df), color="black", alpha=.3, bins=50)+
  labs(title="Scaled Chi-square Distributions", x="Chi-square", y="Frequency")+
  theme_minimal(12)+
  theme(text=element_text(family="Avenir"))
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png)

Notice how that scaling the chi-square distributions do not create a distribution with df=1. Let's divide the chi-square values to get f-values.


```r
datafinal2 <- datafinal %>% 
  select(-ss) %>% 
  spread(df, scaled) %>% 
  rename("df4"="4", "df36"="36") %>% 
  mutate(f=df4/df36)
```

Ok, lets plot this and overlay the above F-density line.


```r
ggplot(datafinal2, aes(x=f))+
  geom_histogram(fill="white", color="black", aes(y=..density..), bins=70)+
  geom_line(data=f, aes(x=f, y=density), color="deepskyblue4", size=1.3)+
    labs(title="F Distribution (df1 = 4, df2 = 36)", x="F", y="Frequency",
         subtitle = "The blue line is the true density function")+
  theme_minimal(12)+
  theme(text=element_text(family="Avenir"))
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png)

Fantastic. Even with just 10'000 observations, we have approximated the F-distributions density function. Remember, the true distribution would account for an infitinite number of ratios of chi-squares of df=4 and 36. For fun (yeah, fun!), let's to simulate the F-distribution with larger degrees of freedom: 12 and 132. Here's the density function our simulation should approximate:


```r
f2 <- data.frame(f=seq(0, 8, .01)) %>% 
  mutate(density=df(f, 12, 132))

ggplot(data=f2, aes(x=f, y=density))+
  geom_line()+
    labs(title="F Distribution (df1=12, df2=132)", x="F", y="Density")+
  theme_minimal(12)
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-1.png)

Let's create out two chi-square samples and divide them. Here's is df=12:


```r
set.seed(238873)
ourdata12 <- data.frame(lapply(rep(12, each=10000), 
                          our_function)) %>% 
  t() %>% 
  data.frame() %>% 
  rename("ss"=".")

chiexample3 <- data.frame(x = seq(0,80, .01)) %>% 
  mutate(density=dchisq(x, 12)) 
```


```r
ggplot(data=ourdata12, aes(ss))+
  geom_histogram(aes(y=..density..), 
                 color="black", 
                 fill="white",
                 binwidth = .5) +
  geom_line(data=chiexample3, aes(x=x, y=density),
            color="deepskyblue4")+
  labs(title="Chi-square Distribution (df=12)", x="Chi-square", y="Density")+
  theme_minimal(12)+
  theme(text=element_text(family="Avenir"))
```

![plot of chunk unnamed-chunk-19](figure/unnamed-chunk-19-1.png)

and df=132:


```r
set.seed(238873)
ourdata132 <- data.frame(lapply(rep(132, each=10000), 
                          our_function)) %>% 
  t() %>% 
  data.frame() %>% 
  rename("ss"=".")

chiexample4 <- data.frame(x = seq(0,200, .01)) %>% 
  mutate(density=dchisq(x, 132)) 
```


```r
ggplot(data=ourdata132, aes(ss))+
  geom_histogram(aes(y=..density..), 
                 color="black", 
                 fill="white",
                 binwidth =2) +
  geom_line(data=chiexample4, aes(x=x, y=density),
            color="deepskyblue4")+
  labs(title="Chi-square Distribution (df=132)", x="Chi-square", y="Density")+
  coord_cartesian(xlim=c(50, 200))+
  theme_minimal(12)+
  theme(text=element_text(family="Avenir"))
```

![plot of chunk unnamed-chunk-21](figure/unnamed-chunk-21-1.png)




```r
chi12 <- ourdata12 %>% 
  mutate(df="12", scaled=ss/12, id=1:10000)

chi132 <- ourdata132 %>% 
  mutate(df="132", scaled=ss/132, id=1:10000)

datafinal3 <- rbind(chi12, chi132)
```

Let's look at each scaled distribution:


```r
ggplot(datafinal3, group=df)+
  geom_histogram(aes(x=scaled, fill=df), color="black", alpha=.3, bins=50)+
  labs(title="Scaled Chi-square Distributions", x="Chi-square", y="Frequency")+
  theme_minimal(12)+
  theme(text=element_text(family="Avenir"))
```

![plot of chunk unnamed-chunk-23](figure/unnamed-chunk-23-1.png)


And now, for F:


```r
datafinal3 <- datafinal3 %>% 
  select(-ss) %>% 
  spread(df, scaled) %>% 
  rename("df12"="12", "df132"="132") %>% 
  mutate(f=df12/df132)
```

Ok, lets plot this and overlay the above F-density line.


```r
ggplot(datafinal3, aes(x=f))+
  geom_histogram(fill="white", color="black", aes(y=..density..), bins=70)+
  geom_line(data=f2, aes(x=f, y=density), color="deepskyblue4", size=1.3)+
    labs(title="F Distribution (df1 = 12, df2 = 132)", x="F", y="Frequency",
         subtitle = "The blue line is the true density function")+
  theme_minimal(12)+
  theme(text=element_text(family="Avenir"))
```

![plot of chunk unnamed-chunk-25](figure/unnamed-chunk-25-1.png)

#### The F-test

These F-dsitributions are expected when the two scaled chi-sqare distributions are completely random. The F-test, as in ANOVA, are testing the null hypothesis that the between group differences are the same as the within group differences. Considering that we randomly select and assign individuals to groups, any differences in scores within groups (i.e., the within group sum of squares, SSW) are considered random and independent and would follow a chi-square distirbution with $N-p$ degreed of freedom, where N is the total sample size and p is the number of groups. Examples of these distributions are above. Additionally, if the groups are the same (i.e., the between group sum of squares SSB), that is our independent variable has no affact or association on the dependent variable, we would also expect a chi-square distirbution, with p-1 degrees of freedom again P being the number of groups. But what would happen if the groups *are* different? Well, we would expect the SSB to be greater than expected with random error. A higher SSB is directly related to the F-statistic:

$MSB = \frac{SSB}{p-1}$

and

$F = \frac{MSB}{MSE}$

Thus, when between groups differences are larger than expected at random, we would expect a higher F-statistic. How high? Well, that depends on you alpha-level. For example, if you perform NHST with an alpha = .05, then a *statistically significant* result would be an F-statistic at or higher than the 95th percentile. Or, for our F-distribution with df1=12 and df2=132:


```r
qf(p=.95, df1=12, df2=132)
```

```
## [1] 1.826197
```

and in graph form, anything at or beyond the red line:



```r
ggplot(datafinal3, aes(x=f))+
  geom_histogram(fill="white", color="black", aes(y=..density..), bins=70)+
  geom_line(data=f2, aes(x=f, y=density), color="deepskyblue4", size=1.3)+
    labs(title="F Distribution (df1 = 12, df2 = 132)", x="F", y="Frequency",
         subtitle = "The blue line is the true density function")+
  theme_minimal(12)+
  theme(text=element_text(family="Avenir"))+
  geom_vline(xintercept=1.826197, color="red")+
  annotate("rect", xmin=1.826, xmax=Inf, ymin=-Inf, ymax=Inf, fill="red", alpha=.2)
```

![plot of chunk unnamed-chunk-27](figure/unnamed-chunk-27-1.png)

I hope that this post has helped you better understand the F-test through exploring the chi-square and F-distributions. I wish I had known this during my undergraduate degree. 
