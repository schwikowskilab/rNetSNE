Write_binary_file <- function(matrix, path.to.bin.file){
  connection = file(path.to.bin.file,"wb")
  rownames(matrix)=NULL;colnames(matrix)=NULL
  writeBin(as.integer(dim(matrix)),connection)
  for(i in 1:nrow(matrix)){
    writeBin(as.double(matrix[i,]),connection)
  }
  close(connection)
}
