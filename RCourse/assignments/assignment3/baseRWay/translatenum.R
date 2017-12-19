
" This function translates 'num' to a number index if it is either 'best' or
'worst'
"
translateNum <- function(info, num){
      if(num == "best") return(1)
      else if(num == "worst") return(dim(info)[1])
      else return(num)
}