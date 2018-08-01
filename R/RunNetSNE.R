RunNetSNE <- function(utilis = c("t-SNE","Learn","Project"),
                      out.dims = 2L, max.iter = 1e3L, theta = 0.5, step.method = "adam",
                      NN.layers = 2L, NN.units = 50L, NN.function = "relu", l2.reg.param = 0,
                      mom.init = 0.5, mom.final = 0.8, mom.switch.iter = 250L,
                      early.exag.iter = 250L,  learn.rate = 0.02,
                      local.sample = 20L, batch.frac = 0.1, min.sample.Z = 0.1,
                      sgd = TRUE, seed = -1, verbose = TRUE,
                      permute.after.iters = NULL, save.iters.cache = NULL,
                      path.netSNE.dir = path.netSNE.dir,
                      path.to.bin.train.file = path.to.bin.file, path.to.bin.test.file = NULL,
                      path.to.simil.file = NULL, path.output.dir, path.ref.embedding = NULL,
                      path.to.model.dir = NULL, model.prefix = "model_final"){

  utilis = tolower(utilis)
  for(i in 1:length(utilis)){
    if(!utilis[i]%in%c("t-sne","tsne","learn", "train","project","proj","projection")){
      stop(paste(utilis[i],": Unknown use of Net-SNE (param: utilis).\nPossible values:\n\t- 't-SNE': t-SNE like embedding\n\t- 'Learn': train Net-SNE on a training dataset to latter project new data on the embedding\n\t- 'Project': project a new dataset on a prior embedding (obtain with utilis = Train)"),call. = FALSE)
    }
  }
  if(step.method!="adam" & step.method!="mom" & step.method!="mom_gain" & step.method!="fixed"){
    stop(paste(step.method,": Unknown gradient step schedule (param: step.method).\nPossible values:'adam', 'mom' (momentum), 'mom_gain' (momentum with gains) or 'fixed'"),call. = FALSE)
  }
  if(NN.function!="relu" & NN.function!="sigmoid"){
    stop(paste(NN.function,": Unknown activation function of the neural network (param: NN.function).\nPossible values:'relu'or 'sigmoid'"),call. = FALSE)
  }

  path.netSNE.dir = paste(path.netSNE.dir,"RunNetsne",sep="/")

  if("t-sne" %in% utilis | "tsne" %in% utilis){
    command = paste(path.netSNE.dir,
                    "--input-P", path.to.simil.file,
                    "--input-X", path.to.bin.train.file,
                    "--out-dir", path.output.dir,
                    sep=" ")

    command = paste(command,"--out-dim",out.dims,"--max-iter",max.iter,"--rand-seed",seed,
                    "--theta",theta,"--learn-rate",learn.rate,"--mom-init",mom.init,
                    "--mom-final",mom.final,"--mom-switch-iter",mom.switch.iter,
                    "--early-exag-iter",early.exag.iter,'--num-local-sample',local.sample,
                    "--batch-frac",batch.frac,"--min-sample-Z",min.sample.Z,
                    "--l2-reg",l2.reg.param,"--step-method",step.method,"--num-layers",NN.layers,
                    "--num-units",NN.units,"--act-fn",NN.function,sep=" ")

    if(!is.null(permute.after.iters)){command=paste(command,"--perm-iter",permute.after.iters,sep=" ")}
    if(!is.null(save.iters.cache)){command=paste(command,"--cache-iter",save.iters.cache,sep=" ")}
    if(!sgd){command=paste(command,"--no-sgd",sep=" ")}
    if(!verbose){command = paste(command,"> /dev/null",sep = " ")}

    if("crayon"%in%installed.packages()[,1]){
      cat(crayon::bold(crayon::red("\n\nRunning NetSNE !  (t-SNE like)\n\n")))}else{
        cat("\n\nRunning NetSNE !  (t-SNE like)\n\n")
      }

    system(command)
  }


  if("learn" %in% utilis | "train" %in% utilis){
    command = paste(path.netSNE.dir,
                    "--input-Y", path.ref.embedding,
                    "--input-X", path.to.bin.train.file,
                    "--out-dir", path.output.dir,
                    sep=" ")

    command = paste(command,"--out-dim",out.dims,"--max-iter",max.iter,"--rand-seed",seed,
                    "--theta",theta,"--learn-rate",learn.rate,"--mom-init",mom.init,
                    "--mom-final",mom.final,"--mom-switch-iter",mom.switch.iter,
                    "--early-exag-iter",early.exag.iter,'--num-local-sample',local.sample,
                    "--batch-frac",batch.frac,"--min-sample-Z",min.sample.Z,
                    "--l2-reg",l2.reg.param,"--step-method",step.method,"--num-layers",NN.layers,
                    "--num-units",NN.units,"--act-fn",NN.function,sep=" ")

    if(!is.null(permute.after.iters)){command=paste(command,"--perm-iter",permute.after.iters,sep=" ")}
    if(!is.null(save.iters.cache)){command=paste(command,"--cache-iter",save.iters.cache,sep=" ")}
    if(!sgd){command=paste(command,"--no-sgd",sep=" ")}
    if(!verbose){command = paste(command,"> /dev/null",sep = " ")}

    if("crayon"%in%installed.packages()[,1]){
      cat(crayon::bold(crayon::red("\n\nRunning NetSNE !  (learnining embedding)\n\n")))
    }else{cat("\n\nRunning NetSNE !  (learnining embedding)\n\n")}

    system(command)
  }


  if("projection" %in% utilis | "project" %in% utilis | "proj" %in% utilis){
    command = paste(path.netSNE.dir,
                    "--input-X", path.to.bin.test.file,
                    "--init-model-prefix", paste(path.to.model.dir, model.prefix,sep="/"),
                    "--test-model", "--no-target",
                    "--out-dir", path.output.dir,
                    sep=" ")
    if(!verbose){command = paste(command,"> /dev/null",sep = " ")}

    if("crayon"%in%installed.packages()[,1]){
      cat(crayon::bold(crayon::red("\n\nRunning NetSNE !  (projection)\n\n")))
    }else{cat("\n\nRunning NetSNE !  (projection)\n\n")}

    system(command)
  }
}
