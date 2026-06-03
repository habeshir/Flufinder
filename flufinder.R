library(seqinr)

# Function 1: Reads a fasta file and returns a list of named protein sequences.
upload_fasta <- function(testfasta.txt) {
  read.fasta(testfasta.txt, seqtype = "AA", as.string = TRUE, set.attributes = FALSE)
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