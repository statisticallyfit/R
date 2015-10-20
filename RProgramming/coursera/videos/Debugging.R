# Indications of problem: message, warning, error, (only error is fatal)

printmessage2 <- function(x) {
      if(is.na(x))
            print("x is a missing value")
      else if(x > 0)
            print("x is greater than 0")
      else
            print("x is less than or equal to 0")
      invisible(x)
}
x <- log(-1)
x
printmessage2(x)
printmessage2(NaN)


# Debugging Tools
      # traceback: prints function call stack ONLY after error
      # debug:  flags function so you can step through 
      # browser: suspends function execution wherever it is called and puts it in debug mode 
      # trace: to insert debugging code into a function in specific places
      # recover: modify error behavior so you can browse function call stack

# traceback()
mean(m)
traceback()

lm(j ~ i)
traceback() # occurred at seventh level

# debug: type 'n' then enter... and so on until find error
debug(lm)
lm(j ~ i)

# recover:
options(error = recover) #sets global options, goes away when q()
read.csv("nofile") # error occurred at 3rd level
