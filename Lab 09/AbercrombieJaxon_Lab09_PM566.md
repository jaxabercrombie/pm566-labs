Problem 2

    library(microbenchmark)

    fun1 <- function(n = 100, k = 4, lambda = 4) {
      x <- NULL
      for (i in 1:n)
        x <- rbind(x, rpois(k, lambda))

      x
    }
    fun1alt <- function(n = 100, k = 4, lambda = 4) {
      matrix(rpois(n * k, lambda), nrow = n, ncol = k, byrow = TRUE)
    }
    microbenchmark::microbenchmark(
      fun1(n = 1000),
      fun1alt(n = 1000), "relative"
    )

    ## Unit: nanoseconds
    ##               expr     min        lq       mean    median        uq      max
    ##     fun1(n = 1000) 5864934 6656247.5 9026947.97 7298924.5 9781676.5 31357135
    ##  fun1alt(n = 1000)  177916  193980.5  234208.42  199003.0  210539.0  2833220
    ##         "relative"       1       4.0      46.66      22.5      84.5      318
    ##  neval cld
    ##    100   b
    ##    100  a 
    ##    100  a

Problem 3

    set.seed(1234)
    x <- matrix(rnorm(1e4), nrow=10)
    fun2 <- function(x) {
      apply(x, 2, max)
    }
    fun2alt <- function(x) {
      idx <- max.col(t(x)) 
      x[ cbind(idx, 1:ncol(x)) ]
    }

    all(fun2(x) == fun2alt(x))

    ## [1] TRUE

    x <- matrix(rnorm(5e4), nrow=10)

    microbenchmark::microbenchmark(
      fun2(x),
      fun2alt(x), "relative"
    )

    ## Unit: nanoseconds
    ##        expr     min        lq       mean    median        uq      max neval cld
    ##     fun2(x) 5703575 6157525.0 6903535.91 6497028.5 7332599.5 12605937   100   c
    ##  fun2alt(x)  577438  737001.5  825716.09  765696.5  850091.5  4086147   100  b 
    ##  "relative"       0       5.5      66.41      38.5      94.0      508   100 a

    set.seed(42343)
    M <- matrix(runif(12), ncol = 4)
    M

    ##           [,1]      [,2]       [,3]      [,4]
    ## [1,] 0.5193214 0.7539021 0.01253299 0.7891065
    ## [2,] 0.4644698 0.4484567 0.06041682 0.1218734
    ## [3,] 0.7760090 0.1781403 0.76544068 0.6561172

    fun2(M)

    ## [1] 0.7760090 0.7539021 0.7654407 0.7891065

    t(M)

    ##            [,1]       [,2]      [,3]
    ## [1,] 0.51932135 0.46446984 0.7760090
    ## [2,] 0.75390212 0.44845674 0.1781403
    ## [3,] 0.01253299 0.06041682 0.7654407
    ## [4,] 0.78910647 0.12187338 0.6561172

    idx <- max.col(t(M))
    idx

    ## [1] 3 1 3 1

    cbind(idx, 1:ncol(M))

    ##      idx  
    ## [1,]   3 1
    ## [2,]   1 2
    ## [3,]   3 3
    ## [4,]   1 4

    M[cbind(idx, 1:ncol(M))]

    ## [1] 0.7760090 0.7539021 0.7654407 0.7891065

Problem 4

    library(parallel)
    my_boot <- function(dat, stat, R, ncpus = 1L) {
      
      n <- nrow(dat)
      idx <- matrix(sample.int(n, n*R, TRUE), nrow=n, ncol=R)
      
      cl <- makePSOCKcluster(ncpus)
      
      clusterSetRNGStream(cl, 123)
      clusterExport(cl, c("stat", "dat", "idx"), envir = environment())
      
      ans <- parLapply(cl = cl, seq_len(R), function(i) {
        stat(dat[idx[,i], , drop=FALSE])
      })
      
      ans <- do.call(rbind, ans)
      
      stopCluster(cl)
      
      ans
      
    }

    my_stat <- function(d) coef(lm(y ~ x, data=d))
    set.seed(1)
    n <- 500; R <- 5e3
    x <- cbind(rnorm(n)); y <- x*5 + rnorm(n)
    ans0 <- confint(lm(y~x))
    ans1 <- my_boot(dat = data.frame(x, y), my_stat, R = R, ncpus = 2L)
    t(apply(ans1, 2, quantile, c(.025,.975)))

    ##                   2.5%      97.5%
    ## (Intercept) -0.1395732 0.05291612
    ## x            4.8686527 5.04503468

    system.time(my_boot(dat = data.frame(x, y), my_stat, R = 4000, ncpus = 1L))

    ##    user  system elapsed 
    ##   0.080   0.015   4.135

    system.time(my_boot(dat = data.frame(x, y), my_stat, R = 4000, ncpus = 2L))

    ##    user  system elapsed 
    ##   0.104   0.019   2.506
