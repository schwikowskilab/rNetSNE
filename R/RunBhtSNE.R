RunBhtSNE <- function(out.dims = 2L, max.iter = 1e3L, theta = 0.5,
                      mom.init = 0.5, mom.final = 0.8, mom.switch.iter = 250L,
                      early.exag.iter = 250L, learn.rate = 200,
                      batch.frac = NULL, seed = -1, verbose = TRUE,
                      random.init = TRUE, save.iters.cache = FALSE,
                      path.netSNE.dir = path.netSNE.dir,
                      path.to.bin.file = path.to.bin.file,
                      path.to.simil.file, path.output.dir){

  command = paste(path.netSNE.dir,"RunBhtsne",sep="/")
  command = paste(command, path.netSNE.dir,
                  "--input-P", path.to.simil.file,
                  "--out-dir", path.output.dir,
                  sep = " ")

  command = paste(command,"--out-dim",out.dims,"--max-iter",max.iter,"--rand-seed",seed,
                  "--theta",theta,"--learn-rate",learn.rate,"--mom-init",mom.init,
                  "--mom-final",mom.final,"--mom-switch-iter",mom.switch.iter,
                  "--early-exag-iter",early.exag.iter,sep=" ")

  if(!random.init){command=paste(command,"--skip-random-init",sep=" ")}
  if(!is.null(batch.frac)){command=paste(command,"--batch-frac",batch.frac, sep=" ")}
  if(!is.null(save.iters.cache)){command=paste(command,"--cache-iter",save.iters.cache,sep=" ")}
  if(!verbose){command = paste(command,"> /dev/null",sep = " ")}

  if("crayon"%in%installed.packages()[,1]){
    cat(crayon::bold(crayon::red("\n\nRunning BhtSNE !\n\n")))
  }else{cat("\n\nRunning BhtSNE !\n\n")}

  system(command)
}
