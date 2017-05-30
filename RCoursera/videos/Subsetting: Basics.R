# Types of subsetting: 
# [] returns object of same class as original, for
  # selecting more than one element
# [[]] extracts from list or data frame, single element
# $ to extract from list or dataframe by name

x = c("a", "b", "c", "c", "d", "a")
x[1]
x[1:4]
x[x > "a"] # lexicographical ordering

u = x > "a"
u
x[u] # all elements greater than a
