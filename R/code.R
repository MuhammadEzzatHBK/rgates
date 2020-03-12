#' @title  check_bool
#' @description Helper function checls whether the input is a logical or not, stops the run if it's not a logical.
#' @param x The case checked variable
#' @examples check_bool(c(TRUE,TRUE))
#' @export
check_bool <- function(x){
  if(class(x)!='logical'){
    stop("Input must be a logical variable")
  }

}

#'@title check_length
#'@description Helper functions asserts that both inputs are of the same length.
#'@param x The first input
#'@param y Thes second input
#'@examples check_length(c(1:3),c(1:3))
#'@export
check_length <- function(x,y){
  if(length(x)!=length(y))
    stop("Both inputs must be of same length")
}



#' @title truth_table
#' @description A function that creates a general table of cases to test with two variables on several logic gates.
#' Isn't expected to throw an error.
#' @return A data frame of two variables x & y, contains all possible logical combinations of two logical variables.
#' @export
truth_table <- function(){
  x <- c(FALSE,FALSE,TRUE,TRUE)
  y <- c(FALSE,TRUE,FALSE,TRUE)
  foo <- data.frame(x,y)
  return(foo)
}





#' @title  not
#' @description Simple function represents the not logic gate & an equivallent for the
#'  '!' sign in R & most preogramming languages. Throws an error if the input isn't a logical variable.
#'@param x The logical to be inverted.
#'@return A logical vector, the inverse of the logical parameter x.
#'@examples not(c(TRUE,FALSE))
#'@export
not <- function(x){
  check_bool(x)
  return(!x)
}


#' @title and
#' @description Simple function returns true if both of the inputs are true, otherwise it returns false. Equivallent to
#' the '&' symbol in R & most programming languages. Throws an error if both inputs aren't logical.
#' @param x The first logical input.
#' @param y The second logical input.
#' @return A logical input containig the resutls of x & y.
#' @examples and(c(FALSE,FALSE,TRUE,TRUE),c(FALSE,TRUE,FALSE,TRUE))
#' @export
and <- function(x,y){
  check_bool(x)
  check_bool(y)
  check_length(x,y)
  return(x&y)
}


#' @title or
#' @description Simple function returns true if one of the inputs is true, otherwise it returns false. Equivallent to
#' the '|' symbol in R & most programming languages. Throws an error if both inputs aren't logical.
#' @param x The first logical input.
#' @param y The second logical input.
#' @return A logical input containig the resutls of x | y.
#' @examples or(c(FALSE,FALSE,TRUE,TRUE),c(FALSE,TRUE,FALSE,TRUE))
#' @export
or <- function(x,y){
  check_bool(x)
  check_bool(y)
  check_length(x,y)
  return(x|y)
}

#' @title nand
#' @description Simple function returns false if both of the inputs are true, otherwise it returns true.
#' Throws an error if both inputs aren't logical.
#' @param x The first logical input.
#' @param y The second logical input.
#' @return A logical input containig the resutls of !(x&y).
#' @examples nand(c(FALSE,FALSE,TRUE,TRUE),c(FALSE,TRUE,FALSE,TRUE))
#' @export
nand <- function(x,y){
  check_bool(x)
  check_bool(y)
  check_length(x,y)
  return(!and(x,y))
}



#' @title nor
#' @description Simple function returns false if one of the inputs is true, otherwise it returns true.
#' Throws an error if both inputs aren't logical.
#' @param x The first logical input.
#' @param y The second logical input.
#' @return A logical input containig the resutls of !(x|y).
#' @examples nor(c(FALSE,FALSE,TRUE,TRUE),c(FALSE,TRUE,FALSE,TRUE))
#' @export
nor <- function(x,y){
  check_bool(x)
  check_bool(y)
  check_length(x,y)
  return(!or(x,y))
}





#' @title xor
#' @description Simple function returns true if the two inputs are not of same value, otherwise it returns false.
#'Throws an error if both inputs aren't logical.
#' @param x The first logical input.
#' @param y The second logical input.
#' @return A logical input containig the resutls of x!=y.
#' @examples xor(c(FALSE,FALSE,TRUE,TRUE),c(FALSE,TRUE,FALSE,TRUE))
#' @export
xor <- function(x,y){
  check_bool(x)
  check_bool(y)
  check_length(x,y)
  return(x!=y)
}

#' @title xnor
#' @description Simple function returns true if both of the inputs are of the same value, otherwise it returns false.
#' Throws an error if both inputs aren't logical.
#' @param x The first logical input.
#' @param y The second logical input.
#' @return A logical input containig the resutls of x==y.
#' @examples xnor(c(FALSE,FALSE,TRUE,TRUE),c(FALSE,TRUE,FALSE,TRUE))
#' @export
xnor <- function(x,y){
  check_bool(x)
  check_bool(y)
  check_length(x,y)
  return(x==y)
}



#' @title implies
#' @description Simple function returns false only if x is true and y is false , otherwise it returns true.
#' Throws an error if both inputs aren't logical.
#' @param x The first logical input.
#' @param y The second logical input.
#' @return A logical input containig the resutls of x => y.
#' @examples implies(c(FALSE,FALSE,TRUE,TRUE),c(FALSE,TRUE,FALSE,TRUE))
#' @export
implies <- function(x,y){
  check_bool(x)
  check_bool(y)
  check_length(x,y)
  z <-rep(TRUE,length(x))
  for(i in c(1:length(z)))
    if(x[i] == TRUE & y[i] == FALSE)
      z[i] <- FALSE
  return(z)
}

#'@title null
#'@description Simple function returns a vector of FALSE's of a given length
#'@param x A number indicates the length of returned vector
#'@retrun a vector of FALSE's of X length
#'@examples null(3)
#'@export
null <- function(x){
  if(class(x)!='numeric')
    stop('Input must be a number')
  x <- as.integer(x)
  return(rep(FALSE,x))
}

#'@title on
#'@description Simple function returns a vector of TRUE's of a given length
#'@param x A number indicates the length of returned vector
#'@retrun a vector of TRUE's of X length
#'@examples identity(3)
#'@export
on <- function(x){
  if(class(x)!='numeric')
    stop('Input must be a number')
  x <- as.integer(x)
  return(rep(TRUE,x))
}

#'@title inhibit
#'@description A function that resutls in x but not y (oreder matters).
#'True if X is true and y is false, otherwise returns false.
#'Throws errors with non logical inputs & different length inputs.
#'@param x First logical input.
#'@param y Second logical input.
#'@return A logical vector that conatins X and Not Y.
#'@examples inhibit(c(FALSE,FALSE,TRUE,TRUE),c(FALSE,TRUE,FALSE,TRUE))
#'@export
inhibit <- function(x,y){
  check_bool(x)
  check_bool(y)
  check_length(x,y)
  return(x&(!y))
}


#'@title transefer
#'@description The simplest gate. Returns it's input without any modification, usually transefer gates
#'are used to amplify the signal incoming. So in boolean algerbra terms a transefer gate is neutral.
#'@param x Input
#'@return The same entered input
#'@examples transefer(TRUE)
#'@export
transefer <- function(x){
  check_bool(x)
  return(x)
}

