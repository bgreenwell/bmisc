misc
================================================================================

This package contains some random (but hopefully useful) functions that I have
used frequently over the years. Some functions may seem unuseful, and were
created purely for practice or enjoyment. If you have suggestions for further
improvements, please submit them. Also, please report any bugs that you come 
across.

### Create matrices using NumPy style syntax

```S
> mat('1, 2, 3; 4, 5, 6; 7, 8, pi')
     [,1] [,2]     [,3]
[1,]    1    2 3.000000
[2,]    4    5 6.000000
[3,]    7    8 3.141593
```

```S
> (m <- mat(paste('exp(', 1:8, ')')))
             [,1]
 [1,]    2.718282
 [2,]    7.389056
 [3,]   20.085537
 [4,]   54.598150
 [5,]  148.413159
 [6,]  403.428793
 [7,] 1096.633158
 [8,] 2980.957987
>
> resize(m, nrows = 2)
           [,1]       [,2]       [,3]       [,4]
[1,]   2.718282   7.389056   20.08554   54.59815
[2,] 148.413159 403.428793 1096.63316 2980.95799
>
> resize(m, 4, 2, byrow = F)
          [,1]      [,2]
[1,]  2.718282  148.4132
[2,]  7.389056  403.4288
[3,] 20.085537 1096.6332
[4,] 54.598150 2980.9580
```

### Create terminal node dummy variables for hybrid models
For more information, see the corresponding whitepaper at https://www.salford-systems.com/resources/whitepapers.
```S
> library(rpart)  # classification and regression trees (CART)
> library(earth)  # multivariate adaptive regression splines (MARS)
> data(Boston, package = "MASS")  # Boston housing data
> 
> ## CART model
> boston_cart <- rpart(medv ~ ., data = Boston, cp = 0.005)
> Boston2 <- nodeFactor(boston_cart)  # add CART dummy variable
>
> ## MARS model
> earth(medv ~ ., data = Boston, degree = 2, linpreds = T)
Selected 14 of 14 terms, and 9 of 13 predictors 21 linear predictors
Importance: lstat, rm, ptratio, rad, tax, dis, crim, nox, chas, zn-unused, ...
Number of terms at each degree of interaction: 1 5 8
GCV 15.14321    RSS 6681.454    GRSq 0.821328    RSq 0.8435854
>
> ## CART/MARS hybrid model
> earth(medv ~ ., data = Boston2, degree = 2, linpreds = T)
Selected 24 of 25 terms, and 22 of 24 predictors 29 linear predictors
Importance: lstat, node15, node14, node16, node21, node18, node12, node17, node19, ...
Number of terms at each degree of interaction: 1 17 6
GCV 9.894404    RSS 3915.843    GRSq 0.8832577    RSq 0.9083291
```

### Easily set new seed values for simulation experiments
I commonly find myself needing to set new seed values for small simulation experiments. While this is as easy as ```set.seed(123)```, I find it much easier to think of words or sentences instead. For example, in demoonstrating the law of large numbers (LLN), we would have to generate some random numbers. For reproducibility, we specify a seed such as above. However, I find it much easier to use a descriptive string: ```setSeed('LLN simulation')```.
```S
> setSeed("Some random numbers")
> rnorm(3)  # should give: 1.7500983 -0.1093635 -0.9958618
[1]  1.7500983 -0.1093635 -0.9958618
> 
> setSeed("Some more random numbers")
> rnorm(3)  # should give: 0.007765185 -1.138536203  0.091017129
[1]  0.007765185 -1.138536203  0.091017129
```