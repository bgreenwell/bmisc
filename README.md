misc
================================================================================

### Create matrices using NumPy-like functionality

```S
> mat('1, 2, 3; 4, 5, 6; 7, 8, 9')
     [,1] [,2] [,3]
[1,]    1    2    3
[2,]    4    5    6
[3,]    7    8    9
```

```S
> (m <- mat(paste('exp(', 1:9, ')')))
             [,1]
 [1,]    2.718282
 [2,]    7.389056
 [3,]   20.085537
 [4,]   54.598150
 [5,]  148.413159
 [6,]  403.428793
 [7,] 1096.633158
 [8,] 2980.957987
 [9,] 8103.083928
> resize(m, 3, 3)
            [,1]        [,2]       [,3]
[1,]    2.718282    7.389056   20.08554
[2,]   54.598150  148.413159  403.42879
[3,] 1096.633158 2980.957987 8103.08393
> resize(m, 3, 3, byrow = FALSE)
          [,1]      [,2]     [,3]
[1,]  2.718282  54.59815 1096.633
[2,]  7.389056 148.41316 2980.958
[3,] 20.085537 403.42879 8103.084
```

### Easily create hybrid CART-like models
```S
> ## Boston housing data
> library(rpart)
> library(rpart.plot)
> data(Boston, package = "MASS")
> boston_cart <- rpart(medv ~ ., data = Boston, cp = 0.005)
> Boston2 <- nodeFactor(boston_cart)  
> 
> ## CART/MARS hybrid model for Boston housing data
> library(earth)
> earth(medv ~ ., data = Boston, degree = 2, linpreds = T)
Selected 14 of 14 terms, and 9 of 13 predictors 21 linear predictors
Importance: lstat, rm, ptratio, rad, tax, dis, crim, nox, chas, zn-unused, ...
Number of terms at each degree of interaction: 1 5 8
GCV 15.14321    RSS 6681.454    GRSq 0.821328    RSq 0.8435854
> earth(medv ~ ., data = Boston2, degree = 2, linpreds = T)
Selected 24 of 25 terms, and 22 of 24 predictors 29 linear predictors
Importance: lstat, node15, node14, node16, node21, node18, node12, node17, node19, ...
Number of terms at each degree of interaction: 1 17 6
GCV 9.894404    RSS 3915.843    GRSq 0.8832577    RSq 0.9083291
```
