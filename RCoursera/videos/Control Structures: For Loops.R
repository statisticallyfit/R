x <- c("a", "b", "c", "d")

# Types of for loops
for(i in 1:4){
  print(x[i])
}
for(i in seq_along(x)){
  print(x[i])
}
for(letter in x) {
  print(letter)
}


# Nested for loops
x <- matrix(1:6, nrow=2, ncol=3)

for(i in seq_len(nrow(x))){
  for(j in seq_len(ncol(x))){
    print(x[i, j])
  }
}

# Difference between seq_len and seq_along
seq_len(10) # makes sequence up to the number in argument
seq_along(10) # makes sequence up to the count of given arguments
