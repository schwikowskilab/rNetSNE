Compute_similarities <- function(perp=30L, verbose = TRUE,
                                 path.netSNE.dir = path.netSNE.dir,
                                 path.to.bin.file = path.to.bin.file,
                                 path.output.file){

  command = paste(path.netSNE.dir,"ComputeP",sep="/")
  command = paste(command, "--input-file", path.to.bin.file,
                  "--output-file", path.output.file,
                  "--perp", perp, sep = " ")

  if(!verbose){command = paste(command,"> /dev/null",sep = " ")}

  if("crayon"%in%installed.packages()[,1]){
    cat(crayon::bold(crayon::red("\n\nComputing similarities !\n\n")))
  }else{cat("\n\nComputing similarities !\n\n")}

  system(command)
}
