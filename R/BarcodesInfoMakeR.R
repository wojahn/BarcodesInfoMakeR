#' Gets Barcodes Info and Infers Unique IDs
#' @param LastNum numeric: LAST Unique ID number e.g. 123
#' @param NEB_File_Path character: path to NEB tsv file (under Protocols, Manusals, and Usage -> Protocols)
#' @export
# Written by John M. A. Wojahn May 2023
# This is Free and Open-Source Software (F.O.S.S.)
# © J. M. A. Wojahn
# Provided under the GNU AGPLv3 License

BarcodesInfoMakeR <- function(LastNum, NEB_File_Path)
{
  if(class(LastNum) != "numeric")
  {
    stop("LastNum must be a whole number!")
  }
  if(LastNum %% 1 != 0)
  {
    stop("LastNum must be a whole number!")
  }
  NEB_File <- as.data.frame(read.delim(NEB_File_Path,  sep = "\t", header = T))
  FileCountR <- 1
  while(paste(NEB_File[FileCountR,],collapse="") != "NameSequenceIndexReadNumber")
  {
   FileCountR <- FileCountR + 1
  }
  NEB_File <- as.data.frame(NEB_File[-seq(1,FileCountR, by = 1),])
  i7z <- NEB_File[NEB_File[,3] == "1",2]
  i5z <- NEB_File[NEB_File[,3] == "2",2]
  CounteR <- LastNum
  OUT <- as.data.frame(matrix(nrow = 96, ncol = 3))
  OUT[,2] <- i5z
  OUT[,3] <- i7z
  colnames(OUT) <- c("SmithNumber","i5 sequence","i7 sequence")
  LoopR <- 0
  for(i in 1:12)
  {
    ThisLetter <- c("A","B","C","D","E","F","G","H")
    for(j in 1:length(ThisLetter))
    {
      LoopR <- LoopR + 1
      CounteR <- CounteR + 1
      OUT[LoopR,1] <- sprintf("%s-%s%s",CounteR,ThisLetter[j],i)
    }
  }
  return(OUT)
}


