ready<-ready[-c(9)]
prin_comp <- prcomp(ready, scale. = T)
prin_comp$rotation
biplot(prin_comp, scale = 0)
