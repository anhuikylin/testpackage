# testpackage
A useful R package
You need to contact this mailbox to use it.<br/>
email:fyliangfei@163.com<br/>
author:Fei Liang<br/><br/>
作者：梁飞<br/>
程序包的用途：为了研究基因在不同分组中的相对含量变化趋势，将基因相对含量在每组中的平均值进行z-score 标准化，然后进行K均值聚类（Kmeans）分析，趋势分析完成后，接下来可对感兴趣的Cluster进一步做基因功能分析，如GO、KEGG富集分析等，直到挖掘到感兴趣的基因。
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
![image](https://user-images.githubusercontent.com/103125590/200128123-e66dc16e-d660-40df-b96d-ba1606e1a2fb.png)

```r
testpackage::kmeans_plot2(data,
                          centers = 6,
                          row = 2,
                          col = 3,
                          center_col = "blue",
                          las = 2
)
```
![image](https://user-images.githubusercontent.com/103125590/200128145-d3e69b1a-08c0-4968-8178-bca89ed8d5ae.png)

```r
testpackage::kmeans_plot3(data,
                          centers = 6,
                          row = 2,
                          col = 3,
                          center_col = "blue",
                          las = 2
)
```
![image](https://user-images.githubusercontent.com/103125590/200128157-483bedc4-75ab-489d-a51a-4a923fc67397.png)

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
![image](https://user-images.githubusercontent.com/103125590/200128173-d4436813-b3b8-4cfa-ab78-55ef31f357ed.png)

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
![image](https://user-images.githubusercontent.com/103125590/200128184-2338da76-dcfd-4de6-b2ad-0c37812ef8a7.png)


