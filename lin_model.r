#to view the dataset built-in called cars in another tab

#1. Basic R dataset commands 
View(cars) #the cars dataset is what we will be using for this test

dim(cars) #gives u the row x column of the cars dataset

nrow(cars) # gives u the number of rows

ncol(cars) # gives u the number of cols

#2. this is the plot function to plot only the points on the graph
plot(dist~speed,data=cars,xlab="speed",ylab="distance",main="speed vs dist",pch=16,cex=1,col="red")
#first argument : tells u the relation between x and y so y~x or speed~dist
#dist~speed is telling between what parameters u have to draw graph

#second argument : data=cars tells u the dataset that we are using which is cars here

#third argument : xlab tells us what should be the x axis label on graph

#fourth argument : ylab tells us what should be the y axis label on graph

#fifth argument : main tells us what should be title of the graph

#sixth argument : pch tells us what should be the shape of the points on the graph
#you can change this value to anything like pch=5 etc

#seventh arugument : cex tells us the size or radius of the points u can change to this cex=6 etc

#eigth argument : col tells u what color u want the points to be

#SO IF U DONT WANT THE HARDCODED FORMULAS PLEASE JUMP TO LINE 93.
#3. SOME HARDCODED FORMULAS WHICH U CAN OPT NOT TO STUDY THESE ARE JUST FOR INFO

x<-cars$speed #this just gets all the x values or speed values from car dataset

y<-cars$dist #this just gets all the y values or distance values from car dataset

#you can opt to not know these formulas
Sxy<-sum((x-mean(x))*(y-mean(y))) #this is the sigma value for Sigma [(Xi-X')(Yi-Y')]

Sxx<-sum((x-mean(x))^2) #this is to find  Sigma [(Xi-X')^2]

Syy<-sum((y-mean(y))^2) #this is to find Sigma [(Yi-y')^2]
Syy
#c(Sxy,Sxx,Syy)

#4. so using these formula we will find the value of beta1.hat and beta0.hat
#the general formula is : y = beta0.hat + beta1.hat * x 
#beta0.hat is the y-intercept and beta1.hat is the slope or m
#In y=mx+c line here m=beta1.hat and c=beta0.hat

#value of beta1.hat from formula is beta1.hat = Sxy / Sxx
beta1.hat<-Sxy/Sxx

#value of beta0.hat from formula is beta0.hat = mean(y) - beta1.hat*mean(x)
beta0.hat<-mean(y)-beta1.hat*mean(x)

#c(beta0.hat,beta1.hat)

#5. variance estimator 
y.hat = beta0.hat + beta1.hat * x #gives the vector of all the predicted y values of our line
e = y - y.hat # gives us the difference of the observed - expected values in vector 
n = length(e) # n is the length of the dataset it was always 50
s2_e = sum(e^2) / (n - 2) #this is the value of variance


# imp: 6. This is important this will tell us how good our model is
#SST IS THE SUM OF SQUARE TOTALS , The length between the observed point from the mean of speed line
SST<-sum((y-mean(y))^2) # this is the foruma sigma[(speed-(mean(speed))^2)]

#SSR IS THE SUM OF SQUARE REGRESIONS
#SSR : THe length between the expected point which is on the line to the mean line
SSR<-sum((y.hat-mean(y))^2)

#SSE IS THE SUM OF SQUARE ERROR
#SSE IS THE DISTANCE BETWEEN the expected point and the observed point
SSE<-sum((y-y.hat)^2)

#c(SST,SSR,SSE)

# VERY VERY IMPORTANT 7. Coefficient of determination 
#the coefficient of determination is called R^2
# THIS VALUE R^2 tells us how good the model is, higher the value of R^2 the better it is
# 0 <= R^2 <= 1   higher the value of R^2 the better it is

R2<-SSR/SST # as SST comes close to SSR the model becomes more accurate.
R2
#SO R^2 gives is inference on how good the model is
#hence coefficient of determination is super important

#EVERYTHING UPTO HERE WAS HARDCODING WHICH U DONT NEED TO ACTuALLY KNOW
#U CAN IGNORE EVERYTHING ABOVE ALSO
#VERY IMP AGAIN 8. DRAWING THE BEST FIT LINEEEEE!
#We saw the below function before
plot(dist~speed,data=cars,xlab="speed",ylab="distance",main="dist vs speed",pch=16,cex=1,col="red")

#NOW USING the lm function
stop_dist_model<-lm(dist~speed,data=cars)
#first argument : again takes the relation y~x or between what parameters u want to plot
#in this case its dist vs speed
#second argument : takes whats the data set in this case its cars

#ANDS THATS ALL


stop_dist_model
#so this will give u the value of model which contains the intercept and slop

#!!!!!!!!!!!!TO DRAW THE LINE OF BEST FITTTTTTTTT!!!!!!!!!!
abline(stop_dist_model) #abline takes the intercept and slope to draw the graph
#THIS DRAWS UR GRAPH AND THIS IS THE MOST IMPORTANT THING U NEED TO KNOW

#TO FIND beta1.hat and beta0.hat
#stop_dist_model$coefficients : this contains vector of beta1.hat and beta0.hat
#vector indices start from 1 in R.
beta1.hat<-stop_dist_model$coefficients[1] #this is done in just 1 line of code
beta0.hat<-stop_dist_model$coefficients[2] # 
c(beta1.hat,beta0.hat)

#TO CALCULATE THE SST,SSR,SSE
SSE<-sum((fitted(stop_dist_model)-cars$dist)^2) #SSE VALUE
SSR<-sum((fitted(stop_dist_model)-mean(cars$dist))^2) #SSR VALUE
SST<-SSE+SSR # SST
SST


R2<-SSR/SST #THIS GIVES U HOW GOOD THE MODEL IS 
R2 #HIGHER THE VALUE the BETTER IT IS




