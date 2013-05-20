## An issue in Medley ##

This script doesn't work properly
```
require(medley);
require(e1071);

data(swiss)

x <- swiss[,1:5];
y <- swiss[,6];
set.seed(1)
train <- sample(nrow(swiss), 30);
m <- create.medley(x[train,], y[train]);
for (gamma in c(1e-3, 2e-3, 5e-3, 1e-2, 2e-2, 5e-2, 1e-1)) {
   m <- add.medley(m, svm, list(gamma=gamma));
}
# use only the best 80% of individual models
m <- prune.medley(m, 0.8)
p <- predict(m, x[-train,]);
rmse(p, y[-train]);
```

Output:
```
CV model 1 svm (gamma = 0.001) time: 0.03 error: 2.649394 
CV model 2 svm (gamma = 0.002) time: 0.02 error: 2.645305 
CV model 3 svm (gamma = 0.005) time: 0.03 error: 2.615573 
CV model 4 svm (gamma = 0.01) time: 0.04 error: 2.607492 
CV model 5 svm (gamma = 0.02) time: 0.03 error: 2.599948 
CV model 6 svm (gamma = 0.05) time: 0.03 error: 2.646888 
CV model 7 svm (gamma = 0.1) time: 0.04 error: 2.604546 
Sampled...
Error in apply(mixpred, 1, mixer) : dim(X) must have a positive length
Error in mean((true - pred)^2) : object 'p' not found
```    

The problem is in `medley.prune` function.

**`R.version`**
```
platform       x86_64-w64-mingw32          
arch           x86_64                      
os             mingw32                     
system         x86_64, mingw32             
status                                     
major          3                           
minor          0.0                         
year           2013                        
month          04                          
day            03                          
svn rev        62481                       
language       R                           
version.string R version 3.0.0 (2013-04-03)
nickname       Masked Marvel 
```

