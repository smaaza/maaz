
#Name: Syed Maaz Ali
#Student ID: 19637352
#Unit: Programming for Data Science
#Unit Code: 301113

#-----------------------------------------------------------------------------#


#Q1




load.data <- function(method = c('scan', 'read.table', 'read.delim'),
                      filename = 'iris_badvalues.txt')

{
  
  f1 <- scan('iris_badvalues.txt', skip = 1,
             what=character() , sep = '\t') 
  
  #what=character() is used to interpret all data as text
  
  M <- matrix(f1 , ncol=6 ,byrow = T) #converting vector into a matrix
  
  M1 <- matrix( as.numeric ( M[,-6]), ncol = 5) #converting text data to numeric
  
  spec <- M[, 6] #species coloumn
  
  combined <- data.frame( M1, spec) #combining to form a single matrix
  
  colnames(combined) <- c('Sample.Index', 'Sepal.Length', 'Sepal.Width',
                     'Petal.Length', 'Petal.Width', 'Species')
  
  func1 <- combined[, -1] #deleting the sample index column from matrix
  
  
  
  
  f2 <- read.table('iris_badvalues.txt', 
                   header= T , sep = '\t')
  func2 <- f2[, -1]
  
  f3 <-  read.delim( 'iris_badvalues.txt',
                     header = T, sep = '\t')
  func3 <- f3[, -1]
  
  
  
  print(head(func1))
  print(head(func2))
  print(head(func3))
  
}

load.data()

#produces identical data for all three methods




#Part2 of Q1



test.load.data<- function()
  
{
  if(func1 == func2  &&  func1 == func3) #testing data using loop functions if & else
    
  {print ("The results are same")
    
  } else
    print ("The results are different")
  
  }

test.load.data()

#The results are same

#--------------------------------------------------------------------#



#Q2

Ndata <- func2 #fun2 will be used to represent the data in Q2 and clean the data




#reviewing text data


unique(Ndata$Species)


#no spelling error, hence text data is good.

#now we will check numeric data for large values,
# negative values and outliers



summary(Ndata)
plot(Ndata[, -5], main = "Iris data with bad values",
     pch =21, col= c('red', 'yellow', 'green4', 'blue'))



#summary and the plot give us an idea that there are negative and large values
#Now we will use the boxplot to look for outliers more clearly




par(mfrow = c(1,1))

boxplot(Ndata$Sepal.Length, Ndata$Sepal.Width, Ndata$Petal.Length, Ndata$Petal.Width,
        main = "Boxplot of Iris data with bad values", ylab = 'Measurements',
        col = 3:6)

legend(0.5, 28, inset=0, title = 'Legend',
       c("Sepal Length", "Sepal Width", "Petal Length", "Petal width"),
       fill = 3:6, cex = 0.8)



#Outliers present in the data
#Now we will clean the data



clean.indices <- function(x)
  
{
  #this block code finds all the bad values present in the data
  missval <- any(is.na(x)) 
  negval <- any( x <= 0) 
  larval <- any( x > 10)
  res <- missval | negval | larval
  
  return(!res)
}



#Applying filters

filter <- apply( Ndata[-5], 1, clean.indices) #applying filter to clean the bad values 

cleaned <- Ndata [filter,]

color<-c('red','yellow','green', 'blue')

#plotting the data to see the difference in cleaned data

plot(cleaned[, -5], main = 'Cleaned Iris data', pch = 21, bg = color[cleaned$Species])

summary(cleaned)

par(mfrow = c(1, 1))



#showing the cleaned data with a boxplot



boxplot(cleaned$Sepal.Length, cleaned$Sepal.Width, 
        cleaned$Petal.Length, cleaned$Petal.Width,
        main = "Boxplots of Cleaned Data", col=5:8,
        xlab = 'Sepal/Petal Length & Width', 
        ylab = 'Measurements')

legend(0.5, 3, inset = 0, title = 'Legend',
       c('Sepal Length', 'Sepal Width', 'Petal Length', 'Petal width'),
       fill = 5:8, cex = 0.8)



#the new box plot with cleaned data presents no outliers,
#hence data quality has improved.


#----------------------------------------------------------------------------#





#Q3i)

build.repair.data <- function(b)
  
{
  
  M.SepL <- tapply(Ndata$Sepal.Length, Ndata$Species, median, na.rm = T) 
  #finding median values for each species 
  M.SepW <- tapply(Ndata$Sepal.Width, Ndata$Species, median, na.rm = T)
  
  M.PetL <-  tapply(Ndata$Petal.Length, Ndata$Species, median, na.rm = T)
  
  M.PetW <-  tapply(Ndata$Petal.Width, Ndata$Species, median, na.rm = T)


  med.matrix <- cbind(M.SepL, M.SepW, M.PetL, M.PetW) #combining all the median values into
  #a matrix form

  dimnames(med.matrix) <- list ( c ('setosa', 'versicolor', 'virginca'), 
                              c ('Sepal.Length', 'Sepal.Width',
                                'Petal.Length', 'Petal.Width'))
  
  cat('Median Values', '\n',  sep='') #giving a heading to the matrix display
  
  print(med.matrix)
  
}

build.repair.data()

#Q3ii
 
build.repair.data <- function(b)

  {
  
  missval1 <- any(is.na(b)) #using the same function codes from Q2 with slight change
  negval1 <- any(b < 0)
  larval1 <- any( b > 10)
  res1 <- missval1 | negval1 | larval1
  
  return(!res1)

}


{
  replace( Ndata$Sepal.Length, res1, M.SepL ) #replacing the bad values with median values
  #for each species and measurment
  
  replace( Ndata$Sepal.Width, res1, M.SepW )
  
  replace( Ndata$Petal.Length, res1, M.PetL)
  
  replace( Ndata$Petal.Width, res1, M.PetW)
 
   
}

  build.repair.data()


filter1 <- apply( Ndata[-5,], 1, build.repair.data )

repaired<- Ndata[ filter1,]

summary(repaired)



# creating two plots together to compare the cleaned
# and repaired data


par(mfrow = c(1, 2))

color<-c('red','yellow','green', 'blue')

plot(cleaned[, -5], main = 'Cleaned Iris data', pch = 21, bg = color[cleaned$Species])


plot(repaired[, -5], main = 'Repaired Iris data', pch = 21, bg = color[repaired$Species])



# Now creating two boxplots together to provide another visual comparison
# of cleaned and repaired data

par(mfrow = c(1, 2))


boxplot(cleaned$Sepal.Length, cleaned$Sepal.Width, 
        cleaned$Petal.Length, cleaned$Petal.Width,
        main = "Boxplots of Cleaned Data", col=5:8,
        xlab = 'Sepal/Petal Length & Width', 
        ylab = 'Measurements')

legend(0.5, 3, inset = 0, title = 'Legend',
       c('Sepal Length', 'Sepal Width', 'Petal Length', 'Petal width'),
       fill = 5:8, cex = 0.8)


boxplot(repaired$Sepal.Length, repaired$Sepal.Width, 
        repaired$Petal.Length, repaired$Petal.Width,
        main = "Boxplots of Repaired Data", col=3:6,
        xlab = 'Sepal/Petal Length & Width', 
        ylab = 'Measurements')

legend(0.5, 3, inset = 0, title = 'Legend',
       c('Sepal Length', 'Sepal Width', 'Petal Length', 'Petal width'),
       fill = 3:6, cex = 0.8)


#-------------------------------------------------------------------------#




#Q4i)


apply(iris[1:4], 2, summary) #producing summary usig only the functions allowed

D <- apply(iris[1:4], 2, summary)
boxplot(D, main= "Measuerments using all species",
        col= c('Red', 'Green', 'Blue', 'Skyblue'))




#Q4ii)

#For textual output, we first create three subsets according to species
#and then produce a combined summary for all three species using one function


spec.summary <- function()
  
{

Setos <- subset(iris, Species == "setosa")
Versic  <- subset(iris, Species == "versicolor")
Virgini <- subset(iris, Species == "virginica")


Setosa <- apply(Setos[1:4], 2, summary)
cat('Species = Setosa', '\n',  sep='')
print(Setosa)
 
Versicolor <- apply(Versic[1:4], 2, summary)
cat('Species = Versicolor', '\n',  sep='')
print(Versicolor)

Virginica <- apply(Virgini[1:4], 2, summary)
cat('Species = Virginica', '\n',  sep='')
print(Virginica)

}
spec.summary()



#producing multiple boxplots with separate codes for each output
#using one function


mul.bplots <- function()
{
par(mfrow=c(2,2))

boxplot(iris$Sepal.Length ~ iris$Species,
        main = 'Sepal.length', xlab = '', ylab = '',
        col = c('red', 'green', 'blue'))


boxplot(iris$Petal.Length ~ iris$Species,
        main = 'Petal.length', xlab = '', ylab = '',
        col = c('red', 'green', 'blue'))


boxplot(iris$Sepal.Width ~ iris$Species,
        main = 'Sepal.Width', xlab = '', ylab = '',
        col = c('red', 'green', 'blue'))


boxplot(iris$Petal.Width ~ iris$Species,
        main = 'Petal.Width', xlab = '', ylab = '',
        col = c('red', 'green', 'blue'))

}

mul.bplots()
