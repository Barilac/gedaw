# Gedaw


## Introduction
<br>
Now we are developing a new geostatistical package. Our package *gedaw* combines geophysical methods and compositional data.
<br><br>


## Data types
<br>
There are several types of data
<br>
| Data name | Data type | Form of measurements |
| ------------- | ------------- | ------------- |
| GPR  | data frame  | slices  |
| Content Cell  | Content Cell  |



```r
dftomat = function (a, x=1, y=2, v=3) 
{
  
  a.res=matrix(NA, nrow=length(unique(a[,x])), ncol=length(unique(a[,y])))
  for (i in 1:dim(a)[1])
 {
  a.res[a[i,x],a[i,y]]=a[i,v]
 }
  
  a.res
  
}
```
