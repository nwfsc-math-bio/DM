######### Various utility function
str_proper=function(string) {
  for(i in 1:length(string)){
    x=tolower(string[i])
    s <- strsplit(x, " ")[[1]]
    string[i]=paste(toupper(substring(s, 1,1)), substring(s, 2),
                    sep="", collapse=" ")
  }
  string
}