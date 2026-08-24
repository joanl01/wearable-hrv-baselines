# Using the "PELT algorithm" 
# Change point function
cptfn <- function(data, pen) {
  ans <- cpt.mean(data, test.stat="Normal", method = "PELT", penalty = "Manual", pen.value = pen, minseglen = 2)
  length(cpts(ans)) + 1
}
