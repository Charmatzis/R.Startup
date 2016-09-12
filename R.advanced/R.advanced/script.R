#Exercise 1.1
exp(0 - 1 / 2)
exp(0 + 1 / 2)
x <- rnorm(1000)
y <- exp(x)
mean(y)

#Exercise 1.2
# ??^2 + 3?? + 1 = 0
A <- 1
B <- 3
C <- 1

splus <- ( -B + sqrt(B ^ 2 - 4 * A * C)) / (2 * A)
sminus <- ( - B - sqrt(B ^ 2 - 4 * A * C)) / (2 * A)

options(digits=1)
x <- c(splus, sminus)
x

options(digits = 6)
y <- c(splus, sminus)
y
error_percentage <- 100* ( c(-0.4, -2.6) / y)
error_percentage

#Exercise 1.3
set.seed(1234)
x <- rnorm(100, mean = .5, sd = .3)
mean(x)
sd(x)
hist(x, axes = FALSE, ylab = "")
axis(1)
axis(4)

set.seed(1)
x <- rnorm(100)
head(x)

x <- 5 + 6
y <- x + 3
z <- y - 10
z

#Lab1
A <- seq(1, by = 2, len = 5)
B <- mean(A)
X <- seq(2, by = 2, length = 5)
Z <- A + X

#Exercise 2.1
a <- c(16, 18, 36, 36)
b <- c(2, 2, 2, 2)
d <- c("s", "w", "a", "d")
df <- data.frame(a, b)
df
df + 1


#Exercise 2.2
my.standard <- function(x) 
    {
        (x - mean(x)) / sd(x)

    }
my.standard(c(1, 2, 3))
my.standard(c(1))

#Exercise 2.3
my.custom <- function(x) {
     3*sin(x/2)+x 
}
my.custom(0)
my.custom(-1)
my.custom(pi)


my.display <- function(x) {
    hist(x, ylab = "Frequency")
    cat("Summary of input: \n")
    return(summary(x))
}

set.seed(1234)
my.data <- rnorm(200)
median(my.data)

#Exercise 3.1

set.seed(1786)
data.exercise.3.1 <- exp(matrix(rnorm(2000), nrow = 100))
index1.temp <- sample(1:100, 10)
index2.temp <- sample(1:20, 10)
for (i in 1:10) {
    data.exercise.3.1[index1.temp[i], index2.temp[i]] <- -1
}
my.data<- data.exercise.3.1
count.negatives<-0
for(i in 1:length(my.data[,1])){
 negative<-(min(my.data[i,])<0)
 count.negatives<-count.negatives+negative
 if(count.negatives<=3 & !negative){
 cat("The mean of row",i,"is",mean(my.data[i,]),"\n")
 }
 if(count.negatives<=3 & negative){
 cat("<Row",i,"contains negative values>\n")
 }
 if(count.negatives>3){
 cat("Too many negative values\n")
  break
 }
}

#Lab3
k <- 1000
r <- 100
set.seed(5556)
x <- as.data.frame(matrix(rnorm(r * k), nrow = r))


my.summary <- matrix(nrow = 4, ncol = k)


#Exercise 4.2
x <- matrix(1:12, 4)
x[cbind(c(1, 3, 2), c(3, 3, 2))]
x[c(1, 3, 2), c(3, 3, 2)]

#Exercise 4.3
row <- matrix(rep(1:100, 100), nrow = 100)
column <- matrix(rep(1:100, 100), nrow = 100, byrow = T)
A <- 3 * column ^ 3 / (1 + row * column)
sum(A)
sum(A[row <= column])

#Lab 4
set.seed(9852)
my.negatives <- matrix(rep(0, 16), nrow = 4)
my.data <- list()
my.index <- list()
my.negative.values <- numeric(0)
for (i in 1:100) {
    my.data[[i]] <- matrix(rnorm(16), nrow = 4)
    my.index[[i]] <- (my.data[[i]] < 0)
    my.negatives <- my.negatives + my.index[[i]]
    my.negative.values <- c(my.negative.values, my.data[[i]][my.index[[i]]])
}
#my.negatives
#length(my.negatives)
#sum(my.negatives)
summary(my.negative.values)


#Lab 6
set.seed(9007)
my.data <- data.frame(x = rnorm(10), y = rnorm(10) + 5, z = rchisq(10, 1))
my.data2 <- my.data * 10e5
write.table(my.data, "Assignment 6b.txt", row.names = FALSE)
my.data3 <- read.table(file = "Assignment 6b.txt", header = T)
my.data3 <- my.data3/ 10e6
my.data[1, 1] - my.data3[1, 1]


#Lab 7 

library(RODBC)
connStr <- paste("Server=msedxeus.database.windows.net",
                    "Database=DAT209x01", "uid=RLogin",
                    "pwd=P@ssw0rd",
                    "Driver={SQL Server}",
                    sep = ";")
conn <- odbcDriverConnect(connStr)
my.data.frame <- sqlQuery(conn,
                         "SELECT SUM(Revenue), SUM(Units), ProductID
                         FROM bi.salesFact
                         WHERE Date > '2013-12-31' AND Date < '2015-01-01'
                         GROUP BY ProductID"
)
names(my.data.frame) <- c("SUM(Revenue)", "SUM(Units)", "ProductID")

head(my.data.frame)
head(my.data.frame[order(my.data.frame$'SUM(Units)', decreasing = T),])
head(my.data.frame[order(my.data.frame$'SUM(Revenue)', decreasing = T),])
close(conn)

#Exercise 8.1
data.frame.x<-data.frame(names=c("Gretha","Robert","John","Heather", "Chris"),
            age1=c(30,18,25,70,1))
data.frame.y <- data.frame(names = c("William", "Nancy", "Charlotte", "Henry", "Chris"),
     age = c(15, 75, 32, 51,1))

data.frame.z <- merge(data.frame.y, data.frame.x, all = TRUE)
data.frame.z


#Exercise 11.1
my.analysis <- lm(log(Ozone) ~ Solar.R + Wind + Temp, data = airquality[airquality > 1,])
qqnorm(my.analysis$res)
sd.1 <- sd(my.analysis$res)
lines((-3):3, ((-3):3) * sd.1, type = "l", lwd = 3, col = "red")

#Exercise 11.2
my.analysis <- lm(log(Ozone) ~ Solar.R + Wind + Temp + Solar.R:Wind + Solar.R:Temp + Wind:Temp, data = airquality[airquality > 1,])
drop1(my.analysis, test = "F")

my.analysis <- update(my.analysis, ~ . - Solar.R:Wind)
drop1(my.analysis, test = "F")


my.analysis <- update(my.analysis, ~ . - Wind:Temp)
drop1(my.analysis, test = "F")

#Exercise 11.3
library(glm2)
data(crabs)
head(crabs)
crab.data <- data.frame(satellite = 1 * (crabs$Satellites > 0), width = crabs$Width)


my.analysis <- glm(satellite ~ width, family = binomial, data = crab.data)
my.analysis

my.linear.predictor <- data.frame(prediction = predict(my.analysis, se.fit = TRUE)$fit, lower = predict(my.analysis, se.fit = TRUE)$fit - 1.96 * predict(my.analysis, se.fit = TRUE)$se.fit, upper = predict(my.analysis, se.fit = TRUE)$fit + 1.96 * predict(my.analysis, se.fit = TRUE)$se.fit)


my.linear.predictor <- my.linear.predictor[order(crab.data$width),]
logistic <- function(x) { exp(x) / (1 + exp(x)) }
my.predictor <- logistic(my.linear.predictor)


plot(sort(crab.data$width),my.predictor$prediction,type="l", ylab='p(satellite)')
lines(sort(crab.data$width),my.predictor$upper,type="l",lty=2)
lines(sort(crab.data$width), my.predictor$lower, type = "l", lty = 2)


summary(crab.data$width)


my.cut<-cut(crab.data$width,breaks=20+(0:5)*3)
my.means<-tapply(crab.data$satellite,my.cut,mean)
lines(20 + (0:4) * 3 + 1.5, my.means, type = "p", pch = 16)

#Exercise 11.4

diabetes.data <- read.csv2("Data/my.diabetes.data.csv")
new.diabetes.data <- read.csv2("Data/my.new.diabetes.data.csv")

diabetes.data <- diabetes.data[, -1]
new.diabetes.data <- new.diabetes.data[, -1]


my.model<-"gender"
for (i in 2:17) {
    my.model <- paste(my.model, "+", names(diabetes.data)[i])

}
my.formula<-as.formula(paste("readmi_class~",my.model))
my.analysis <- glm(my.formula, family = binomial(link = logit), data = diabetes.data)


my.linear.predictor <- c(predict(my.analysis, newdata = new.diabetes.data))

logistic <- function(x) { exp(x) / (1 + exp(x)) }
predict.diabetes <- logistic(my.linear.predictor)

plot(density(predict.diabetes[new.diabetes.data$readmi_class=="YES"]) , main="Fitted values", ylim=c(0,4))
lines(density(predict.diabetes[new.diabetes.data$readmi_class == "NO"]), lty = 2)

#Lab 11
library(UsingR)
father.son
my.analysisdata <- lm(sheight ~ +fheight, data = father.son)
summary(my.analysisdata)


install.packages("R330")
library(R330)
data(wine.df)
head(data)
head(wine.df)

my.wine.analysis <- lm(price ~ year + temp + h.rain * w.rain, data = wine.df)
summary(my.wine.analysis)


drop1(my.wine.analysis, test = "F")

coef(my.wine.analysis)
coef(my.wine.analysis)[4] + 400 * coef(my.wine.analysis)[6]

coef(my.wine.analysis)[4] + 800 * coef(my.wine.analysis)[6]

summary(wine.df)


my.wine.analysis.log <- lm(log(price) ~ year + temp + h.rain * w.rain, data = wine.df)
summary(my.wine.analysis.log)


drop1(my.wine.analysis.log, test = "F")
my.wine.analysis.log <- update(my.wine.analysis.log, ~ . - h.rain: w.rain)
drop1(my.wine.analysis.log, test = "F")


summary(my.wine.analysis.log)

#Exercise 12.1
qplot(hp, qsec, data = mtcars)
qplot(hp, qsec, data = mtcars, geom = c("point", "smooth"))
qplot(hp, qsec, data = mtcars, geom = c("point", "smooth"), method = "lm")
qplot(hp, qsec, data = mtcars, geom = c("point"), method = "lm")


hist(Temp, data = airquality, breaks = 10)
hist(airquality$Temp, breaks = 10)
qplot(Temp, data = airquality, binwidth = 5)
qplot(airquality$Temp, breaks = 5)

#Exercise 12.1
p <- ggplot(data = diamonds)
p <- p + aes(x = carat, y = depth)
p <- p + geom_point()
p <- p + geom_density2d()
p

#Exercise 12.2
depth.groups <- cut(diamonds$depth, breaks = 40 + (0:5) * 8)


#Exercise 12.3
library(ggplot2)

library(ggmap)

head(state.x77)

popdata <- data.frame(state = row.names(state.x77), murder = state.x77[, 5])
popdata$state <- as.character(popdata$state)


for (i in 1:nrow(popdata)) {
    latlon = geocode(popdata$state[i])
    popdata$lon[i] = as.numeric(latlon[1])
     popdata$lat[i] = as.numeric(latlon[2])
}

usa_center = geocode("United States")
USA <- ggmap(get_map(location = usa_center, zoom = 4), extent = "panel")


USA + geom_point(aes(x = lon, y = lat), data = popdata, col = "black",  size = popdata$murder, alpha=.4)


#Lab 12
my.data<-data.frame(federal.states=c("Baden-Württemberg","Bayern","Berlin",
    "Brandenburg","Bremen","Hamburg","Hessen",
    "Mecklenburg-Vorpommern","Niedersachsen",
 "Nordrhein-Westfalen","Rheinland-Pfalz",
 "Saarland","Sachsen","Sachsen-Anhalt",
 "Schleswig-Holstein","Thüringen"),
 Population=c(10716644,12691568,3469849,2457872,661888,1762791,
 6093888,1599138,7826739,17638098,4011582,989035,4055274,
 2235548, 2830864, 2156759))



my.data$federal.states <- as.character(my.data$federal.states)
latlon <- geocode(my.data$federal.states)

my.data$federal.states[1] <- "Baden-Wurttemberg"


my.data$federal.states[16] <- "Thuringen Germany"
latlon <- geocode(my.data$federal.states)

my.data$lon <- latlon$lon;
my.data$lat <- latlon$lat

my.data <- cbind(my.data, latlon)

Germany <- ggmap(get_map(location = "Germany", zoom = 6), extent = "panel")
