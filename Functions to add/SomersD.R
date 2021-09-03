somersD = function(table, dependent="rows", digits=2){

  # concordant pairs
  C = function(t) {
    r_ndx = row(t)
    c_ndx = col(t)
    sum(t * mapply(function(r, c){sum(t[(r_ndx > r) & (c_ndx > c)])},
                   r = r_ndx, c = c_ndx))
  }

  # discordant pairs
  D = function(t) {
    r_ndx = row(t)
    c_ndx = col(t)
    sum(t * mapply( function(r, c){
      sum(t[(r_ndx > r) & (c_ndx < c)])
    },
    r = r_ndx, c = c_ndx) )
  }

  # ties in the dependent variable
  if (dependent=="rows"){
    E = function(t) {
      r_ndx = row(t)
      c_ndx = col(t)
      sum(t * mapply( function(r, c){
        sum(t[(r_ndx == r)])
      },
      r = r_ndx, c = c_ndx) )
    }
  } else if (dependent=="cols"){
    E = function(t) {
      r_ndx = row(t)
      c_ndx = col(t)
      sum(t * mapply( function(r, c){
        sum(t[(c_ndx == c)])
      },
      r = r_ndx, c = c_ndx) )
    }
  } else { warning("'dependent' argument must be 'rows' or 'cols'") }

  c = C(table)
  d = D(table)
  e = E(table)

  somers = (c-d)/(c+d+e)

  print(paste("Somers' D: ", round(somers, digits=digits)))

}


x <- matrix(c(12, 4, 3, 5, 10, 6, 3, 5, 14),nrow=3, byrow=TRUE)
ConDisPairs(x)

somersD(x,dependent="rows")

library(DescTools)
SomersDelta(x, direction="row", conf.level=0.95)
