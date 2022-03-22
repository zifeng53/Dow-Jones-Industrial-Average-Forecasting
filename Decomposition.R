file.exists('C:\\Users\\Admin\\Downloads\\stock_prices_sample.csv')
df <- read.csv(file='C:\\Users\\Admin\\Downloads\\Dow_Jones_Industrial_Average_Historical_Data.csv',header=TRUE,stringsAsFactors=FALSE)
head(df)
df <- df[0:2]
head(df)
is.ts(df)
CLOSE <- df[[2]]
head(CLOSE)
Y <- c(CLOSE)
Y <- ts(Y, frequency = 12, start=1990)

plot(Y, type="o", col="Blue", main="Additive Stock Price", xlab="Date", ylab="Closing Price")
components <- decompose(Y,type = "additive")
plot(components)
cbind(components$x,components$trend,components$seasonal,components$random)


plot(Y,type="o", col="Blue", main="Multiplicative Stock Price",
     xlab="Date", ylab="Closing Price")
components <- decompose(Y, type="multiplicative")
plot(components)
cbind(components$x,components$trend,components$seasonal,components$random)

Y