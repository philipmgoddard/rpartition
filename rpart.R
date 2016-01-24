# TO DO- at terminal nodes, write out entire set of rules
# from there, think about how to implement- eg this just splits,
# but as it stands cannot use for prediction


recursivePart <- function(pred, y, min_grp = 10) {
  npred <- ncol(pred)
  count <- 1
  for (i in 1:npred) {
    x <- pred[, i]
    ord <- order(x)
    arrx <- x[ord]
    arry <- y[ord]
    len <- length(arry)
    #cat("\n\ni = ", i)
    for (j in 1:(len-1)) {
      grp1 <- arry[1:j]
      grp2 <- arry[(j+1):len]
      sse <- sum((grp1 - mean(grp1)) ^ 2) + sum((grp2 - mean(grp2)) ^ 2)
      #cat("\ngrp1", length(grp1))
      #cat("\ngrp2", length(grp2))
      #cat("\nsse", sse)
      if (count == 1) {
        sseBest <- sse
      } else {
        if (sse < sseBest) {
          sseBest <- sse
          colInd <- i
          value <- arrx[j]
        }
      }
      count <- count + 1
    }
  }
  cat("\n%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
  cat("\nDepth =", depth,
      "\nPartition:",
      "\nnum predictors = ", nrow(pred),
      "\ncol name =", names(pred)[colInd],
      "\nvalue =", value,
      "\nsse =", sseBest,
      "\ncol index =", colInd, "\n")
  
  #split groups
  S <- which(pred[, colInd] > value)
  group1x <- pred[S, , drop = FALSE]
  group1y <- y[S]
  group2x <- pred[-S, , drop = FALSE]
  group2y <- y[-S]

  cat("\ngrp1\n")
  cat("num pred =", nrow(group1x), "\n")
  print(cbind(group1y, group1x))
  cat("\ngrp2\n")
  cat("num pred =", nrow(group2x), "\n")
  print(cbind(group2y, group2x))
  
  depth <<- depth + 1
  #node_info[depth] <<- depth
  #node_info[depth][[1]] <<- sseBest

  # rememeber depth where both split (not into terminal nodes)
  if(nrow(group1x) > min_grp & nrow(group2x) > min_grp) depth_mem <<- depth
  
  # clunky but i think you need several statements
  # due to recursive nature... investigate
  if(nrow(group1x) <= min_grp)  {
    cat("\nterminal node:",
        "\ndepth =", depth,
        "\ncondition:",names(pred)[colInd], "GT", value,
        #"\nsplit column index= ", colInd,
        #"\nsplit val= ", value,
        #"\nsse = ", sseBest,
        "\nmean node y value =", mean(group1y), "\n")
  }
  if(nrow(group2x) <= min_grp)  {
    cat("\nterminal node:",
        "\ndepth =", depth,
        "\ncondition:",names(pred)[colInd], "LE", value,
        #"\nsplit column index= ", colInd,
        #"\nsplit val= ", value,
        #"\nsse = ", sseBest,
        "\nmean node y value =", mean(group2y), "\n")
  }
  
  # reset the depth memory to where both groups split without terminal node
  if(nrow(group2x) <= min_grp & nrow(group1x) <= min_grp) depth <<- depth_mem 
  
  if(nrow(group1x) > min_grp) {
    recursivePart(group1x, group1y, min_grp = 10)
  } 
  if(nrow(group2x) > min_grp) {
    recursivePart(group2x, group2y, min_grp = 10)
  }
  
}

### test

rPartPhil <- function(inputPred, inputOut, min_split = 10, ...) {
  depth <<- 0 
  path <- c("")
  node_info <<- list()
  depth_mem <<- NULL
  
  recursivePart(inputPred, inputOut, min_split)
  
}


rPartPhil(mtcars[, 3:7, drop = FALSE], mtcars[, 1], min_grp = 10)


