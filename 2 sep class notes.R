## file extension
.txt
.csv
.R
.Rproj

#Vector object with only one dimension 
vector = c(1,2,3,4) logic, numeric, character
cha = c("apple", "banana", "Orange") inside the quotes is an element. 
cha1 = ["apple"]
cha2 = ["banana"]
cha3 = ["orange"]

y= c('apple', 1, TRUE) gives a result of the characters.

vec1 = c(1,2,3) 
vec2 = c(2,3,4)
vec1 +1
vec1 + vec2
# Matrix: object with 2 dimensions 
matrix(data = 1:6, nrow = 2) 
matrix(data, nrow = 1 rows, ncol = 2 cols)
#array: similar to matrix, but multiple dimensions
arr = array(1:12,dim = c(2,2,3))
is.array(arr)
#data frame: most common type. 2 dimensions and can like an excel spreadsheet.

data_biol3100 = data.frame
data.frame(
name = c("Evangeline", "Hasan", "Tyler")
favorite number = c(7,8,7))
data_biol3100$Name
data_biol3100$new_col <- c(TRUE, TRUE, TRUE)
data_biol3100$passing_class
###List#####
#list is a multi dimension, different type, different length.
list()
list_1 = list(name = c("Evangeline", "Hasan", "Tyler")
              favorite number = c(7,8,7))
read.csv
read.csv(file = 'data/Bird_Measurements.csv')
list.files(data_biol3100)
#loop
I like cherries
I like raspberries
I like apples

vec_fruit = c('cherries', 'raspberries', 'apples') 
for (i in vec_fruit) {
new_sent = paste('I like', i)
print(new_sent)
}

for (variable in vector) {
  
}
