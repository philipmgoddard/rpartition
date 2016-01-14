# Recursive partition for regression

At this stage, this is purely half baked ideas

###Plan:
  - implement in R
  - test vs rpart
  - implement using S4, complete with generics
  - implement loops in C++
  - see if a Rust implementation can improve peformance

###Status
  - recursive partitioning seems to work

###TO DO
  - implement in form of predictive equation. How to translate splits into
    if-thens?
  - pruning
  - once it works, compare to rpart package
  - once it really works, make into R S4 class with generics
  - once it really, really works, write loop intensive parts in C++
