"data types = vectors, arrays, matrices, strings, logical, double, 
data frames, factors, lists (of objects)"


#  ------------------- Vectors ------------------- 

aCharVector = c("Dewey", 'Hewey', "Louey")
aCharVector

anIntVector <- c(1, 2, 3, 4, 5)
anIntVector

unsorted <- c(2, 6, 4, 3, 1, 5)
sortedUp = sort(unsorted)
sortedUp
sortedDown = sort(unsorted, decreasing=TRUE)
sortedDown

# Ignore missing NA values
a <- c(1,2,3,4, NA)
sum(a)
sum(a, na.rm=TRUE)

is.na(a)
a[!is.na(a)]

#removing entries matching certain criteria
a = c(6,2,5,3,8,2)
a[a<6]

" Other functions: 
min, max, sum, mean, var, sd, sqrt, sort, median, summary
log, exp"
quartile(unsorted)



# numbers
a <- 3
b <- sqrt(a*a+3)
b

# get list of variabels used in multiple files
ls()
a <- c(1, 2, 3, 4, 5)
a+1
mean(a)
sd(a)
var(a)
summary(a)

# numeric = list
a <- numeric(10)
a
a+3

# data type
typeof(a)


# ------------------- Strings ------------------- 

word <- "hi there"
word
b <- c("hello", "there")
b
typeof(b)
a = character(20)
a


# ------------------- Factors ------------------- 
# (factors as in statistics experiment)
# summary on a factor prints out factors and their frequencies

summary(tree$CHBR) 
tree$CHBR

" The 1, 2, 3, 4 in C column are factors; different 
levels of carbon dioxide for trees"

summary(tree$C) # not our intention
tree$C <- factor(tree$C)
tree$C
summary(tree$C) # correct
levels(tree$C)


# ------------------- Data Frames ------------------- 
# data frame is list of differently-typed vectors

a <- c(1, 2, 3, 4)
b <- c(2, 4, 6, 8)
levels <- factor(c("A", "B", "A", "B"))
dataFrame <- data.frame(first=a,
                        second=b,
                        f=levels)
dataFrame
summary(dataFrame)

dataFrame$first
dataFrame$second
dataFrame$f
dataFrame[[3]]
dataFrame[3]

# ------------------- Logical ------------------- 
"  | entry-wise or
   || or
   xor(a, b) exclusive or
   & entry-wise and
   && and
"
A = c(TRUE, TRUE, FALSE, FALSE)
B = c(TRUE, FALSE, TRUE, FALSE)
a|b
a||b
xor(A, B) 


a = c(1,2,3)
is.numeric(a)
is.factor(a)
is.unsorted(a)


# ------------------- Tables ------------------- 
" table's argument is a vector of factors, and it calculates
frequency that each factor occurs"


# one-way tables (have one row)
# Method 1
a <- factor(c("A", "A", "B", "A", "B", "B", "C", "A", "C"))
results <- table(a)
results

attributes(results)

summary(results)

# Method 2
occur <- matrix(c(4,3,2,5,6,7,8,8,9,10,11,12), ncol=3, byrow=TRUE)
colnames(occur) <- c("A", "B", "C")
occur <- as.table(occur)
occur

attributes(occur)



# two-way tables

# Method 1
a <- c("Sometimes", "Sometimes", "Never", "Always", "Always", "Sometimes", "Sometimes", "Never")
b <- c("Maybe", "Maybe", "Yes", "Maybe", "Maybe", "No", "Yes","No")
results <- table(a, b)
results

# Method 2
genderNSmoke <- matrix(c(70, 120, 65, 140), ncol=2, byrow=TRUE)
genderNSmoke
rownames(genderNSmoke) <- c("male", "female")
colnames(genderNSmoke) <- c("smoker", "non-smoker")
genderNSmoke <- as.table(genderNSmoke)
genderNSmoke
