# convert a value to ...
# select a custom value and convert it to a target value
# for use inside apply/map family of functions

# x is a vector
# value is the value that you want to change
# to is the target value

convert_to_vec <- function(x, value = 0, to = NA) {
  if (is.numeric(value)) {
    x[x == value] <- to
    return(x)
  } else if (is.na(value)) {
    x[is.na(x)] <- to
    return(x)
  } else {
    stop("value should be numeric or NA", call. = FALSE)
  }
}


#example
# convert 5 to 14
vect <- seq(1:10)
convert_to_vec(vect, value = 5, to = 14)
