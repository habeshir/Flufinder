

# Function 1: Reads a fasta file and returns a list of named protein sequences.
upload_fasta <- function(testfasta.txt) {
  read.fasta(testfasta.txt, seqtype = "AA", as.string = TRUE, set.attributes = FALSE)
}


# Function 2: 
trypsinize <- function(proteins) {
  # Opening stringr for simple string manipulation
  library(stringr)
  # Using str_split_1 to split proteins after R or K amino acids; "(?<=R|K)" 
  # is a regular expression for splitting after "?<=" R or K "R|K"
  lapply(proteins, str_split_1, pattern="(?<=R|K)")
}


#Function 3:
split_peptides <- function(peptides) {
  # Opening stringr for simple string manipulation
  library(stringr)
  #Splitting peptides into individual amino acids
  lapply(peptides, str_split, pattern="")
}



# Function 4: Calculates peptide masses from amino acids using a mass table.
splitpeptides_to_masses <- function(aa) {
  aa_masses <- c(A=71.037, R=156.101, N=114.042, D=115.026, C=103.009, 
                 Q=128.058, E=129.042, G=57.021, H=137.058, I=113.084, 
                 L=113.084, K=128.094, M=131.040, F=147.068, P=97.052, 
                 S=87.032, T=101.047, W=186.079, Y=163.063, V=99.068)
  
  peptide_masses <- aa
  for(i in 1:length(aa)) {
    peptide_masses[[i]] <- lapply(aa[[i]], function(x) sum(aa_masses[x]))
  }
  
  lapply(peptide_masses, unlist)
}


# Function 5: 
count_matching_masses <- function(protein_masses, sample) {
  #Virus masses is a list of masses for each protein so we use sapply to
  # iterate over the list; sum (of TRUEs) is used to count the number of
  # times a mass in the sample is found (%in%) among the masses of each of
  # the proteins (virus_masses); note that masses are converted into strings
  # (as.character) because %in% is not very reliable with numbers
  df <- as.data.frame(sapply(protein_masses, function (x)
    sum(as.character(sample) %in% as.character(x))))
  # Adding peptide_counts as the column name of the counts column
  names(df) <- "peptide_counts"
  return(df)
}