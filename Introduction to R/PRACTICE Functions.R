#Assignment 

# 1 .Create a function named pass_check that takes a number for 
# the argument grade and returns "pass" if the number is equal or greater than 5 and "fail" if it's lower.

pass_check<-function(grade){
  if (grade < 5) {
    print('Fail')
  }
  else {
    print('Pass')
  }
}

pass_check(3)
pass_check(5)
pass_check(9)

# 1.1 Try to make it more reliable by creating an error whenever the number is negative or greater than 10

pass_check <- function(grade) {
  if (grade<0 | grade>10) {
    print('Error:put number between 0 and 10')
  }  
  else if (grade < 5) {
    return('Fail')
  } 
  else {
    return('Pass')
  }
}

pass_check(6)
pass_check(-2)
pass_check(12)

# 1.2 Add two aditional arguments, on_time and optional_part. When on_time is FALSE, 
# then 1 point should be substracted from the number entered, and when optional_part is TRUE. 

pass_check<-function(grade, on_time=NA, optional_part=NA){
  if(grade>10|grade<0){
    print('Error:put number between 0 and 10')
  }
  else {
    if(on_time==FALSE){
      grade<-grade-1
    }
    if(optional_part==TRUE){
      grade<-grade+1
    }
    if(grade<5){
      print('Fail')
    }
    else {
      print('Pass')
    }
  }
}

pass_check(4,T,T)
pass_check(5,T,T)
pass_check(0,F,F)
pass_check(-1,T,T)
pass_check(12,T,T)


# 1.3 1.3 Create a vector grades with numbers 1 to 10. Pass the function to each element of the vector as the argument grade, 
# with values for on_time and optional_part always fixed at TRUE and FALSE respectively.

grades<-seq(1,10)

for(grade in grades){
  pass_check(grade, T, F)
}


## PART 2 ##

# 2. Read the df_states.csv from local, with the argument stringsAsFactors=F. 

df_states<-read.csv('df_states.csv', sep = ';', stringsAsFactors=FALSE)

str(df_states)

# Manage to automatically coerce all character variables to factor ones, without naming any specific column. 
# This function should work with any data.frame no matter the dimension, number, names or position of character variables

changechar<-function(x){
  for(i in 1:ncol(x))
    if(is.character(x[ ,i])==TRUE){
      x[ ,i]<-as.factor(x[ ,i])
    }
  str(x)
}

changechar(df_states)

# I do not know how to make the change in dataframe permanent... I tried with assign('x',x,envir=.GlobalEnv) and with <<-
# but it was not working for me properly
