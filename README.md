# testpackage
A useful R package
You need to contact this mailbox to use it.
## How to use it
```r
library(devtools)
install_github("anhuikylin/testpackage")
library(testpackage)
kmeans_test_data <- kmeans_test_data
kmeans_test_group <- kmeans_test_group
data <- testpackage::means_calculate(kmeans_test_data,
                                     kmeans_test_group)
testpackage::kmeans_plot(data,
                         centers = 6,
                         row = 2,
                         col = 3,
                         center_col = "blue",
                         las = 2)
```

![image](https://github.com/anhuikylin/printhello/blob/master/Rplot68.jpeg) 

email:fyliangfei@163.com
