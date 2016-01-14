
# make a wrapper function for recursivePart
depth <- -1 
node_info <- vector(mode = "list")

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
      sse <- sum((grp1 - mean(grp1)) ^ 2) + sum((grp2 - mean(grp1)) ^ 2)
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
  cat("\nPartition:")
  cat("\nsse=", sseBest,
      "\ncol index=", colInd,
      "\nvalue=", value, "\n")
  
  #split groups
  S <- which(pred[, colInd] > value)
  group1x <- pred[S, , drop = FALSE]
  group1y <- y[S]
  group2x <- pred[-S, , drop = FALSE]
  group2y <- y[-S]
  
  cat("\ngrp1\n")
  cat("nrow =", nrow(group1x), "\n")
  print(cbind(group1y, group1x))
  cat("\ngrp2\n")
  cat("nrows =", nrow(group2x), "\n")
  print(cbind(group2y, group2x))
  
  depth <<- depth + 1

  # clunky but i think you need the 4 statements
  # due to recursive nature
  if(nrow(group1x) <= min_grp)  {
    cat("\nterminal node:",
        "\ndepth =", depth,
        "\nsplit column index= ", colInd,
        "\nsplit val= ", value,
        "\nsse = ", sseBest,
        "\nmean node y value =", mean(group1y), "\n")
  }
  if(nrow(group2x) <= min_grp)  {
    cat("\nterminal node:",
        "\ndepth =", depth,
        "\nsplit column index= ", colInd,
        "\nsplit val= ", value,
        "\nsse = ", sseBest,
        "\nmean node y value =", mean(group2y), "\n")
  }
  if(nrow(group1x) > min_grp) {
    recursivePart(group1x, group1y, min_grp = 10)
  } 
  if(nrow(group2x) > min_grp) {
    recursivePart(group2x, group2y, min_grp = 10)
  }
  
}

### test
recursivePart(mtcars[, 3:7], mtcars[, 1], min_grp = 10)


