R_netSNE <- function(to.run = c("Compute.sim", "BhtSNE", "NetSNE.basic","NetSNE.train","NetSNE.project"), path.netSNE.dir,
                        out.dims = 2L, max.iter = 1e3L, perp = 70L, theta.BhtSNE = 0.5, theta.NetSNE = 0.5,
                        mom.init = 0.5, mom.final = 0.8, mom.switch.iter = 250L,
                        early.exag.iter = 250L, learn.rate.bhtSNE = 200, learn.rate.netSNE = 0.02,
                        NN.layers = 2L, NN.units = 50L, NN.function = "relu", sgd = TRUE,
                        batch.frac.bhtSNE = NULL, batch.frac.netSNE = 0.1, random.init = TRUE,
                        local.sample = 20L, min.sample.Z = 0.1, l2.reg.param = 0, step.method = "adam",
                        save.iters.cache = NULL, permute.after.iters = NULL, seed = -1, verbose = TRUE,
                        path.output.dir = path.output.dir, train.data = NULL, test.data = NULL, ref.embedding = c("BhtSNE","NetSNE.basic"),
                        path.to.bin.train.file = NULL, path.to.bin.test.file = NULL, force.rm.old.outdir = TRUE,
                        name.bhtSNE.output.dir = "Bhtsne_out", name.netSNE.output.dir = "Netsne_out"){
  wd = getwd()
  setwd("~/")

  to.run = unique(tolower(to.run)) ; to.rm = c()

  if(length(to.run)==0){
    stop("You didn't specify anything in param: 'to.run'. Please do so !\nPossible values:\n\t- 'Compute.sim': to compute a knn-graph used by Bht-SNE and NetSNE\n\t- 'Bht-SNE': to get a usual t-SNE embedding of a dataset\n---> Requirments: to.run = 'Compute.sim'\n\t- 'NetSNE.basic': to perform a t-SNE-like embedding using Net-SNE\n---> Requirments: to.run = 'Compute.sim'\n\t- 'NetSNE.train': train Net-SNE on a training/sampled dataset to latter project new data on the embedding\n---> Requirments : to.run = c('Compute.sim', 'BhtSNE' &/| 'NetSNE.basic')\n\t- 'NetSNE.project': project a new dataset on a prior embedding\n---> Requirments: to.run = c('Compute.sim, 'BhtSNE' &/| 'NetSNE.basic', 'NetSNE.train')", call. = FALSE)
  }

  for(i in 1:length(to.run)){
    if(!to.run[i] %in% c("compute.sim", "bhtsne", "netsne.basic","netsne.train","netsne.project")){
      to.rm[(length(to.rm)+1)] = to.run[i]
    }
  }
  if(length(to.rm)>0){
    stop(paste(paste(to.rm,collapse = " and "),": Unknown task(s) for param: to.run.\nPossible values:\n\t- 'Compute.sim': to compute a knn-graph used by Bht-SNE and NetSNE\n\t- 'Bht-SNE': to get a usual t-SNE embedding of a dataset\n---> Requirments: to.run = 'Compute.sim'\n\t- 'NetSNE.basic': to perform a t-SNE-like embedding using Net-SNE\n---> Requirments: to.run = 'Compute.sim'\n\t- 'NetSNE.train': train Net-SNE on a training/sampled dataset to latter project new data on the embedding\n---> Requirments : to.run = c('Compute.sim', 'BhtSNE' &/| 'NetSNE.basic')\n\t- 'NetSNE.project': project a new dataset on a prior embedding\n---> Requirments: to.run = c('Compute.sim, 'BhtSNE' &/| 'NetSNE.basic', 'NetSNE.train')"),call. = FALSE)
  }else{rm(to.rm)}

  if("netsne.train" %in% to.run){
    if(length(ref.embedding) != 1){
      stop("When training Net-SNE on a reference embedding (param: 'ref.embedding'), you can only use one of those options: 'BhtSNE' or 'NetSNE.basic' (corresponds to a t-SNE-like embedding). Therefore, c('", paste(ref.embedding, collapse  = "', '"),"') is not an acceptable value.", call. = FALSE)
    }else{
      ref.embedding = unique(tolower(ref.embedding))
      if(length(which(c("bhtsne","netsne.basic","netsne") %in% ref.embedding))<1){
        stop(ref.embedding," is not a correct option for 'ref.embedding'.\nPossible values:\n\n\t- 'BhtSNE'\n ---> Requirment: to.run = 'BhtSNE'\n\n\t- 'NetSNE.basic'\n ---> Requirment: to.run = 'NetSNE.basic'", call. = FALSE)
      }
    }
  }


  path.output.dir = strsplit(path.output.dir,"/")[[1]]
  if(0 %in% nchar(path.output.dir)){
    path.output.dir = path.output.dir[-which(path.output.dir=="")]
  }
  if(!dir.exists(paste(path.output.dir, collapse = "/"))){dir.create(paste(path.output.dir, collapse = "/"),recursive = FALSE)}

  path.netSNE.dir = strsplit(path.netSNE.dir,"/")[[1]]
  if(0 %in% nchar(path.netSNE.dir)){
    path.netSNE.dir = path.netSNE.dir[-which(path.netSNE.dir=="")]
  }
  if(path.netSNE.dir[length(path.netSNE.dir)]!="bin"){
    path.netSNE.dir[length(path.netSNE.dir)+1] = "bin"
  }

  path.netSNE.dir = paste(path.netSNE.dir, collapse = "/")
  if(!file.exists(path.netSNE.dir)){                   #tests the existance of the path to netSNE directory
    if(!file.exists(paste("/",path.netSNE.dir,sep = ""))){
      stop(paste("The specified path to the 'bin' directory containing the executable files (",path.netSNE.dir,") does not exist",sep=""),call.=FALSE)
    }else{path.netSNE.dir = paste("/",path.netSNE.dir,sep = "")}
  }

  if(length(which(c("netsne.basic","netsne.train","netsne.project") %in% to.run))>0){
    name.netSNE.output.dir = strsplit(name.netSNE.output.dir,"/")[[1]]
    if(0 %in% nchar(name.netSNE.output.dir)){
      name.netSNE.output.dir = name.netSNE.output.dir[-which(name.netSNE.output.dir=="")]
    }
    name.netSNE.output.dir = name.netSNE.output.dir[length(name.netSNE.output.dir)]
  }





  if(length(which(c("compute.sim", "bhtsne", "netsne.basic","netsne.train")%in%to.run))>0){

    if(is.null(train.data) & is.null(path.to.bin.train.file)){
      stop("Neither the train data ('train.data') nor the path to train data binary file ('path.to.bin.train.file') were specified, please re-lunch the function with one of them as input.\n---> Required for running ",
           paste(to.run[which(to.run%in%c("compute.sim", "netsne.basic","netsne.train"))],collapse = " and "), call. = FALSE)
    }

    else if(!is.null(path.to.bin.train.file)){
      path.to.bin.train.file = strsplit(path.to.bin.train.file,"/")[[1]]
      if(0 %in% nchar(path.to.bin.train.file)){
        path.to.bin.train.file = path.to.bin.train.file[-which(path.to.bin.train.file=="")]
      }
      name.train = path.to.bin.train.file[length(path.to.bin.train.file)]
      if(length(which(c("compute.sim","bhtsne","netsne.basic") %in% to.run))>0){
        name.sim = strsplit(name.train,"\\.")[[1]]
        extention = name.sim[length(name.sim)]
        name.sim = name.sim[-length(name.sim)]
        name.sim = paste(name.sim, collapse = ".")
        name.sim = paste(name.sim,"_",perp,"_P.",extention,sep="")
      }
      if(!dir.exists(paste(path.to.bin.train.file[1:(length(path.to.bin.train.file)-1)], collapse = "/"))){dir.create(paste(path.to.bin.train.file[1:(length(path.to.bin.train.file)-1)], collapse = "/"), recursive = TRUE)}
      path.to.bin.train.file = paste(path.to.bin.train.file, collapse = "/")
      if(!is.null(train.data)){
        if(file.exists(path.to.bin.train.file)){
          status = suppressWarnings(file.remove(path.to.bin.train.file))
          if(!status){
            stop("Impossible to remove the file '",name.train,"' in ", substr(path.to.bin.train.file,1,nchar(path.to.bin.train.file)-nchar(name.train)),". Please do it yourself or change the path for param: 'path.to.bin.train.file'.", call. = FALSE)
          }else{cat("\nRemoving the file '", name.train, "' in ", substr(path.to.bin.train.file,1,nchar(path.to.bin.train.file)-nchar(name.train)), sep = "")}
        }
        cat("\nWriting '",name.train,"' in ",substr(path.to.bin.train.file,1,nchar(path.to.bin.train.file)-nchar(name.train)),"\n",sep = "")
        Write_binary_file(train.data, path.to.bin.train.file)
      }else if(!file.exists(path.to.bin.train.file)){     #!is.null(path.to.bin.train.file) & is.null(data.train)
        stop("The file '",name.train,"' in ", substr(path.to.bin.train.file,1,nchar(path.to.bin.train.file)-nchar(name.train))," doesn't exist and you didn't provide a matrix corresponding to the training data. Please provide the data train matrix or change the path for param: 'path.to.bin.train.file'.", call. = FALSE)

      }else{
        cat("\nUsing the file '", name.train,"' in ", substr(path.to.bin.train.file,1,nchar(path.to.bin.train.file)-nchar(name.train)),".\n", sep = "")
      }
    }else{     #is.null(path.to.bin.train.file) & !is.null(data.train)
      name.train = gsub(pattern = "\\[.*",replacement = "",deparse(substitute(train.data)))
      if(length(which(c("compute.sim","bhtsne","netsne.basic") %in% to.run))>0){name.sim = paste(name.train,"_",perp,"_P.dat",sep="")}
      name.train = paste(name.train,"dat", sep=".")
      if(file.exists(paste(paste(path.output.dir,collapse = "/"),name.train,sep = "/"))){
        path.to.bin.train.file = paste(paste(path.output.dir,collapse = "/"),name.train,sep = "/")
        rm.file = TRUE
      }else{
        path.to.bin.train.file = paste(paste(path.output.dir[1:(length(path.output.dir)-1)],collapse = "/"),name.train,sep = "/")
        if(file.exists(path.to.bin.train.file)){
          rm.file = TRUE
        }else{rm.file = FALSE}
      }
      if(rm.file){
        status = suppressWarnings(file.remove(path.to.bin.train.file))
        if(!status){
          stop("Impossible to remove the file '",name.train,"' in ", substr(path.to.bin.train.file,1,nchar(path.to.bin.train.file)-nchar(name.train)),". Please do it yourself or change the path for param: 'path.to.bin.train.file'.", call. = FALSE)
        }else{cat("\nRemoving the file '", name.train, "' in ", substr(path.to.bin.train.file,1,nchar(path.to.bin.train.file)-nchar(name.train)), "\n", sep = "")}
      }
      cat("\nWriting '",name.train,"' in ", substr(path.to.bin.train.file, 1, nchar(path.to.bin.train.file)-nchar(name.train)),"\n",sep = "")
      Write_binary_file(train.data, path.to.bin.train.file)
    }
  }









  if("netsne.project"%in%to.run){

    if(is.null(test.data) & is.null(path.to.bin.test.file)){
      stop("Neither the test data ('test.data') nor the path to test data binary file ('path.to.bin.test.file') were specified, please re-lunch the function with one of them as input.\n---> Required for running to.run = c('netsne.project')", call. = FALSE)
    }

    else if(!is.null(path.to.bin.test.file)){
      path.to.bin.test.file = strsplit(path.to.bin.test.file,"/")[[1]]
      if(0 %in% nchar(path.to.bin.test.file)){
        path.to.bin.test.file = path.to.bin.test.file[-which(path.to.bin.test.file=="")]
      }
      name.test = path.to.bin.test.file[length(path.to.bin.test.file)]
      if(!dir.exists(paste(path.to.bin.test.file[1:(length(path.to.bin.test.file)-1)], collapse = "/"))){dir.create(paste(path.to.bin.test.file[1:(length(path.to.bin.test.file)-1)], collapse = "/"), recursive = TRUE)}
      path.to.bin.test.file = paste(path.to.bin.test.file, collapse = "/")
      if(!is.null(test.data)){
        if(file.exists(path.to.bin.test.file)){
          status = suppressWarnings(file.remove(path.to.bin.test.file))
          if(!status){
            stop("Impossible to remove the file '",name.test,"' in ", substr(path.to.bin.test.file,1,nchar(path.to.bin.test.file)-nchar(name.test)),". Please do it yourself or change the path for param: 'path.to.bin.test.file'.", call. = FALSE)
          }else{cat("\nRemoving the file '", name.test, "' in ", substr(path.to.bin.test.file,1,nchar(path.to.bin.test.file)-nchar(name.test)), sep = "")}
        }
        cat("\nWriting '",name.test,"' in ",substr(path.to.bin.test.file,1,nchar(path.to.bin.test.file)-nchar(name.test)),"\n",sep = "")
        Write_binary_file(test.data, path.to.bin.test.file)
      }else if(!file.exists(path.to.bin.test.file)){     #!is.null(path.to.bin.test.file) & is.null(data.test)
        stop("The file '",name.test,"' in ", substr(path.to.bin.test.file,1,nchar(path.to.bin.test.file)-nchar(name.test))," doesn't exist and you didn't provide a matrix corresponding to the testing data. Please provide the data test matrix or change the path for param: 'path.to.bin.test.file'.", call. = FALSE)

      }else{
        cat("\nUsing the file '", name.test,"' in ", substr(path.to.bin.test.file,1,nchar(path.to.bin.test.file)-nchar(name.test)),".\n", sep = "")
      }
    }else{     #is.null(path.to.bin.test.file) & !is.null(data.test)
      name.test = gsub(pattern = "\\[.*",replacement = "",deparse(substitute(test.data)))
      name.test = paste(name.test,"dat", sep=".")
      if(file.exists(paste(paste(path.output.dir,collapse = "/"),name.test,sep = "/"))){
        path.to.bin.test.file = paste(paste(path.output.dir,collapse = "/"),name.test,sep = "/")
        rm.file = TRUE
      }else{
        path.to.bin.test.file = paste(paste(path.output.dir[1:(length(path.output.dir)-1)],collapse = "/"),name.test,sep = "/")
        if(file.exists(path.to.bin.test.file)){
          rm.file = TRUE
        }else{rm.file = FALSE}
      }
      if(rm.file){
        status = suppressWarnings(file.remove(path.to.bin.test.file))
        if(!status){
          stop("Impossible to remove the file '",name.test,"' in ", substr(path.to.bin.test.file,1,nchar(path.to.bin.test.file)-nchar(name.test)),". Please do it yourself or change the path for param: 'path.to.bin.test.file'.", call. = FALSE)
        }else{cat("\nRemoving the file '", name.test, "' in ", substr(path.to.bin.test.file,1,nchar(path.to.bin.test.file)-nchar(name.test)), "\n", sep = "")}
      }
      cat("\nWriting '",name.test,"' in ", substr(path.to.bin.test.file, 1, nchar(path.to.bin.test.file)-nchar(name.test)),"\n",sep = "")
      Write_binary_file(test.data, path.to.bin.test.file)
    }
  }

  path.output.dir = paste(path.output.dir, collapse = "/")



  if(length(which(c("bhtsne","netsne.train") %in% to.run))>0){
    name.bhtSNE.output.dir = strsplit(name.bhtSNE.output.dir,"/")[[1]]
    if(0 %in% nchar(name.bhtSNE.output.dir)){
      name.bhtSNE.output.dir = name.bhtSNE.output.dir[-which(name.bhtSNE.output.dir=="")]
    }
    name.bhtSNE.output.dir = name.bhtSNE.output.dir[length(name.bhtSNE.output.dir)]
    path.BhtSNE.output.dir = paste(path.output.dir,name.bhtSNE.output.dir,sep = "/")
  }

  if(length(which(c("compute.sim", "bhtsne", "netsne.basic")%in%to.run))>0){path.to.bin.P = paste(path.output.dir,name.sim, sep= "/")}

  if("compute.sim"%in%to.run){
    if(file.exists(path.to.bin.P)){
      if(force.rm.old.outdir){
        status = suppressWarnings(file.remove(path.to.bin.P))
        if(!status){
          stop("Impossible to remove the file '",name.sim,"' in ",path.output.dir,". Please do it yourself or remove 'compute.sim' from param: to.run if the file is correct.", call. = FALSE)
        }else{compute.sim =TRUE}
      }else{
        WALL = TRUE
        while(WALL){
          cat("The binary file '", name.sim, "' specifying the similarities seems to already exist (PATH: ", path.to.bin.P, ").\nPlease choose:\n\t- [1]: Remove the file and compute similarities,\n\t- [2]: Keep the file and skip the computing of similarities,\n\t- [3]: Stop and do nothing.\n", sep = "")
          answer = readline(prompt = "Your choice [[1]/[2]/[3]]: ")
          if(answer%in%c(1:3)){
            WALL = FALSE
            if(answer == "1"){
              status = suppressWarnings(file.remove(path.to.bin.P))
              if(!status){
                stop("Impossible to remove the file '",name.sim,"' in ",path.output.dir,". Please do it yourself or remove 'compute.sim' from param: to.run if the file is correct.", call. = FALSE)
              }
            }else if(answer == "2"){compute.sim = FALSE}
            else{return("Stopped")}

            }
          }
        }
      }else{compute.sim = TRUE}
    if(compute.sim){
      Compute_similarities(perp = perp, verbose = verbose,
                           path.netSNE.dir = path.netSNE.dir,
                           path.to.bin.file = path.to.bin.train.file,
                           path.output.file = path.to.bin.P)
    }
  }else{compute.sim = FALSE}

  if(!compute.sim && length(which(c("bhtsne","netsne.basic") %in% to.run))>0 && !file.exists(path.to.bin.P)){
    stop("The file ", name.sim, " in ", path.output.dir," is unreachable.", call. = FALSE)
  }



  if("bhtsne" %in% to.run){
    if(dir.exists(path.BhtSNE.output.dir)){
      if(force.rm.old.outdir){unlink(path.BhtSNE.output.dir, recursive = TRUE) ; run.bhtsne = TRUE}
      else{
        WALL = TRUE
        while(WALL){
          cat("The output directory '", name.bhtSNE.output.dir, "' for saving the outputs of Bht-SNE seems to already exist (PATH: ", path.BhtSNE.output.dir, ").\nPlease choose:\n\t- [1]: Remove the directory and run Bht-SNE,\n\t- [2]: Keep the directory and skip the computing of Bht-SNE,\n\t- [3]: Stop and do nothing.\n", sep = "")
          answer = readline(prompt = "Your choice [[1]/[2]/[3]]: ")
          if(answer%in%c(1:3)){
            WALL = FALSE
            if(answer == "1"){
              unlink(path.BhtSNE.output.dir, recursive = TRUE)
              run.bhtsne = TRUE
            }else if(answer == "2"){run.bhtsne = FALSE}
            else{return("Stopped")}

          }
        }
      }
    }else{run.bhtsne = TRUE}
    if(run.bhtsne){
      RunBhtSNE(out.dims = out.dims, max.iter = max.iter, theta = theta.BhtSNE,
                mom.init = mom.init, mom.final = mom.final, mom.switch.iter = mom.switch.iter,
                early.exag.iter = early.exag.iter, learn.rate = learn.rate.bhtSNE,
                batch.frac = batch.frac.bhtSNE, seed = seed, verbose = verbose,
                random.init = random.init, save.iters.cache = save.iters.cache,
                path.netSNE.dir = path.netSNE.dir,
                path.to.bin.file = path.to.bin.train.file,
                path.to.simil.file = path.to.bin.P, path.output.dir = path.BhtSNE.output.dir)
    }
  }else{run.bhtsne = FALSE}

  if("netsne.basic" %in% to.run){
    path.NetSNE.output.dir = paste(path.output.dir,paste(name.netSNE.output.dir, "basic", sep = "_"),sep = "/")
    if(dir.exists(path.NetSNE.output.dir)){
      if(force.rm.old.outdir){unlink(path.NetSNE.output.dir, recursive = TRUE) ; run.netsne.basic = TRUE}
      else{
        WALL = TRUE
        while(WALL){
          cat("The output directory '", name.netSNE.output.dir, "' for saving the outputs of Net-SNE (t-SNE-like use) seems to already exist (PATH: ", path.NetSNE.output.dir, ").\nPlease choose:\n\t- [1]: Remove the directory and run Net-SNE.basic (t-SNE-like),\n\t- [2]: Keep the directory and skip the computing of Net-SNE (t-SNE-like),\n\t- [3]: Stop and do nothing.\n", sep = "")
          answer = readline(prompt = "Your choice [[1]/[2]/[3]]: ")
          if(answer%in%c(1:3)){
            WALL = FALSE
            if(answer == "1"){
              unlink(path.NetSNE.output.dir, recursive = TRUE)
              run.netsne.basic = TRUE
            }else if(answer == "2"){run.netsne.basic = FALSE}
            else{return("Stopped")}

          }
        }
      }
    }else{run.netsne.basic = TRUE}
    if(run.netsne.basic){
      RunNetSNE(utilis = "t-SNE", out.dims = out.dims, max.iter = max.iter, theta = theta.NetSNE, step.method = step.method,
                NN.layers = NN.layers, NN.units = NN.units, NN.function = NN.function, l2.reg.param = l2.reg.param,
                mom.init = mom.init, mom.final = mom.final, mom.switch.iter = mom.switch.iter,
                early.exag.iter = early.exag.iter, learn.rate = learn.rate.netSNE,
                local.sample = local.sample, batch.frac = batch.frac.netSNE, min.sample.Z = min.sample.Z,
                sgd = sgd, seed = seed, verbose = verbose,
                permute.after.iters = permute.after.iters, save.iters.cache = save.iters.cache,
                path.netSNE.dir = path.netSNE.dir,
                path.to.bin.train.file = path.to.bin.train.file, path.to.bin.test.file = NULL,
                path.to.simil.file = path.to.bin.P, path.output.dir = path.NetSNE.output.dir, path.ref.embedding = NULL,
                path.to.model.dir = NULL, model.prefix = "WHATEVER")
    }
  }else{run.netsne.basic = FALSE}


  if("netsne.train" %in% to.run){
    if(ref.embedding == 'bhtsne'){
      path.ref.embedding = paste(path.output.dir,name.bhtSNE.output.dir, "Y_final.txt", sep = "/")
    }else{   #ref.embedding = 'netsne(.basic)'
      path.ref.embedding = paste(path.output.dir, paste(name.netSNE.output.dir, "basic", sep = "_"),"Y_final.txt", sep = "/")
    }
    if(!file.exists(path.ref.embedding)){
      stop("There is no embedding for Net-SNE to learn in ",path.ref.embedding,". Please change the value of param: 'ref.embedding' or include the relevant option in 'to.run'.", call. = FALSE)
    }
    path.NetSNE.output.dir = paste(path.output.dir,paste(name.netSNE.output.dir, "model", sep = "_"),sep = "/")
    if(dir.exists(path.NetSNE.output.dir)){
      if(force.rm.old.outdir){unlink(path.NetSNE.output.dir, recursive = TRUE) ; run.netsne.train = TRUE}
      else{
        WALL = TRUE
        while(WALL){
          cat("The output directory '", name.netSNE.output.dir, "' for saving the outputs of Net-SNE (model creation after learning embedding) seems to already exist (PATH: ", path.NetSNE.output.dir, ").\nPlease choose:\n\t- [1]: Remove the directory and run Net-SNE.train (learn embedding),\n\t- [2]: Keep the directory and skip the computing of Net-SNE (learn embedding),\n\t- [3]: Stop and do nothing.\n", sep = "")
          answer = readline(prompt = "Your choice [[1]/[2]/[3]]: ")
          if(answer%in%c(1:3)){
            WALL = FALSE
            if(answer == "1"){
              unlink(path.NetSNE.output.dir, recursive = TRUE)
              run.netsne.train = TRUE
            }else if(answer == "2"){run.netsne.train = FALSE}
            else{return("Stopped")}

          }
        }
      }
    }else{run.netsne.train = TRUE}
    if(run.netsne.train){
      RunNetSNE(utilis = "Learn", out.dims = out.dims, max.iter = max.iter, theta = theta.NetSNE, step.method = step.method,
                NN.layers = NN.layers, NN.units = NN.units, NN.function = NN.function, l2.reg.param = l2.reg.param,
                mom.init = mom.init, mom.final = mom.final, mom.switch.iter = mom.switch.iter,
                early.exag.iter = early.exag.iter,  learn.rate = learn.rate.netSNE,
                local.sample = local.sample, batch.frac = batch.frac.netSNE, min.sample.Z = min.sample.Z,
                sgd = sgd, seed = seed, verbose = verbose,
                permute.after.iters = permute.after.iters, save.iters.cache = save.iters.cache,
                path.netSNE.dir = path.netSNE.dir,
                path.to.bin.train.file = path.to.bin.train.file, path.to.bin.test.file = NULL,
                path.to.simil.file = NULL, path.output.dir = path.NetSNE.output.dir, path.ref.embedding = path.ref.embedding,
                path.to.model.dir = NULL, model.prefix = "whatever")
    }
  }else{run.netsne.train = FALSE}


  if("netsne.project" %in% to.run){
    path.to.model.dir = paste(path.output.dir,paste(name.netSNE.output.dir, "model", sep = "_"),sep = "/")
    if(!dir.exists(path.to.model.dir)){
      stop("The directory '", path.to.model.dir,"' supposed to contain the model of Net-SNE (after learning a reference embedding) does not exist.")
    }
    l = list.files(path.to.model.dir, pattern = "_.*[.txt]", full.names = FALSE, include.dirs = FALSE)
    l = l[which(regexpr("_L[0-9]*_[:alnum:]*",l)>0)]
    l = gsub("_L[0-9]*_.*.txt","", l)
    if(length(unique(l))==1){
      model.prefix = unique(l)
    }else{stop("Impossible to find a model prefix (usually 'model_final') in ",path.to.model.dir,". Please check the content of the directory, and re-run the function (maybe with 'netsne.train' in to.run) ", call. = FALSE)}
    path.NetSNE.output.dir = paste(path.output.dir,paste(name.netSNE.output.dir, "projection", sep = "_"),sep = "/")
    if(dir.exists(path.NetSNE.output.dir)){
      if(force.rm.old.outdir){unlink(path.NetSNE.output.dir, recursive = TRUE) ; run.netsne.project = TRUE}
      else{
        WALL = TRUE
        while(WALL){
          cat("The output directory '", name.netSNE.output.dir, "' for saving the outputs of Net-SNE (projection) seems to already exist (PATH: ", path.NetSNE.output.dir, ").\nPlease choose:\n\t- [1]: Remove the directory and run Net-SNE.project (to project),\n\t- [2]: Keep the directory and skip the computing of Net-SNE (to project),\n\t- [3]: Stop and do nothing.\n", sep = "")
          answer = readline(prompt = "Your choice [[1]/[2]/[3]]: ")
          if(answer%in%c(1:3)){
            WALL = FALSE
            if(answer == "1"){
              unlink(path.NetSNE.output.dir, recursive = TRUE)
              run.netsne.project = TRUE
            }else if(answer == "2"){run.netsne.project = FALSE}
            else{return("Stopped")}

          }
        }
      }
    }else{run.netsne.project = TRUE}
    if(run.netsne.project){
      RunNetSNE(utilis = "Project",
                verbose = TRUE,
                path.netSNE.dir = path.netSNE.dir,
                path.to.bin.train = "irrelevant", path.to.bin.test = path.to.bin.test.file,
                path.output.dir = path.NetSNE.output.dir,
                path.to.model.dir = path.to.model.dir, model.prefix = model.prefix)
    }
  }else{run.netsne.project = FALSE}

  Result = list()
  i = 1
  if(run.bhtsne){
    Result[[i]] = read.table(file = paste(path.BhtSNE.output.dir,"Y_final.txt", sep = "/"), header = FALSE, skip = 2, sep = "")
    names(Result)[i] = "Bht-SNE"
    i = i+1
  }
  if(run.netsne.basic){
    Result[[i]] = read.table(file = paste(path.output.dir, paste(name.netSNE.output.dir, "basic", sep = "_"), "Y_final.txt", sep = "/"), header = FALSE, skip = 2, sep = "")
    names(Result)[i] = "Net-SNE (Basic)"
    i = i+1
  }
  if(run.netsne.project){
    Result[[i]] = read.table(file = paste(path.output.dir, paste(name.netSNE.output.dir, "projection", sep = "_"), "Y_final.txt", sep = "/"), header = FALSE, skip = 2, sep = "")
    names(Result)[i] = "Net-SNE (Projection)"
    i = i+1
  }
  setwd(wd)
  return(Result)
}
