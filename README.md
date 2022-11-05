# testpackage
A useful R package
You need to contact this mailbox to use it.<br/>
email:fyliangfei@163.com<br/>
author:Fei Liang<br/>
中文名：梁飞
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
```r
testpackage::kmeans_plot2(data,
                          centers = 6,
                          row = 2,
                          col = 3,
                          center_col = "blue",
                          las = 2
)
```
```r
testpackage::kmeans_plot3(data,
                          centers = 6,
                          row = 2,
                          col = 3,
                          center_col = "blue",
                          las = 2
)
```
```r
testpackage::kmeans_plot3(data = data,
                          centers = 6,
                          row = 2,
                          col = 3,
                          center_col = "blue",
                          las = 2,
                          box = FALSE,
                          point_plot = TRUE
)
```
```r
testpackage::kmeans_plot3(data = data,
                          centers = 6,
                          row = 2,
                          col = 3,
                          center_col = "blue",
                          las = 2,
                          box = FALSE,
                          point_plot = FALSE
)
```

![image](https://github.com/anhuikylin/printhello/blob/master/Rplot68.jpeg) 

![image](https://user-images.githubusercontent.com/103125590/200128089-111b4bab-3bcd-4a58-8872-e42ae15bc561.png)
