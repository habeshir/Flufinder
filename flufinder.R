
# Function 2: 
trypsinize <- function(proteins) {
  # Opening stringr for simple string manipulation
  library(stringr)
  # Using str_split_1 to split proteins after R or K amino acids; "(?<=R|K)" 
  # is a regular expression for splitting after "?<=" R or K "R|K"
  lapply(proteins, str_split_1, pattern="(?<=R|K)")
}

