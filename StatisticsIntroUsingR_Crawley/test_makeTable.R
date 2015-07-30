makeTable <- function(values, colNames, rowNames){
  table <- matrix(args, byrow=T, nrow=2)
  
  table <- cbind(table, c(sum(table[1,]), sum(table[2,])))
  colnames(table) = c(colNames, "ROW TOTALS")
  
  table <- rbind(table, c(sum(table[,1]), sum(table[,2]), sum(table[,3])))
  rownames(table) = c(rowNames, "COL TOTALS")
  
  table
}


# Method 1 for pvalue
situation1 <- makeTable(c(6,2,4,8), c("Tree A", "Tree B"), c("With ants", "Without ants"))

args <- c(6,2,4,8)
table <- matrix(args, byrow=T, nrow=2)
table
