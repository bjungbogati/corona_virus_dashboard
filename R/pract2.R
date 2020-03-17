df <- data.frame(A = c(1,1,1,1), B = c(2,2,2,2), C = c(2,2,2,2), D = c(1,2,3,4), 
                 E = c(1,2,3,4), E = c(1,2,3,4), F = c(1,2,3,4), G = c(1,2,3,4))
, 
# for(i in 2:ncol(data)){
# 
#   map2_df(df1, df2[1,], `-`)
# }


df2[2:ncol(df)] <- df[2:ncol(df)]-df[,2]



# data$B <- (data$B - data$A)
# data

set.seed(24)
df1 <- as.data.frame(matrix(sample(1:10, 5*10, replace = TRUE), ncol=5))
df2 <- as.data.frame(matrix(sample(1:5, 5*10, replace = TRUE), ncol=5))



