-5+.618*(2.63+5)
2.64^2
7.36^2
2.63848^2
(-0.27848)^2
(-.284)^2
-0.2848+0.382*(0.83+0.2848)
(0.4041464)^2


f <- function(x) {(x)^2};f
res <- optimize(f, interval = c(-5,15))
print(res)


golden_section_search <- function(f, a, b, tol = 0.3, max_iter = 15) {
  gr <- (sqrt(5) + 1) / 2  # golden ratio
  
  c <- b - (b - a) / gr
  d <- a + (b - a) / gr
  
  iter <- 0
  while (abs(b - a) > tol && iter < max_iter) {
    if (f(c) < f(d)) {
      b <- d
    } else {
      a <- c
    }
    
    c <- b - (b - a) / gr
    d <- a + (b - a) / gr
    iter <- iter + 1
  }
  
  return((a + b) / 2)
}

# Example usage:
f <- function(x) { x*(x-1.5) }  # minimum at x = 2
result <- golden_section_search(f, a = 0, b = 1)
cat(sprintf("Minimum at x ≈ %.5f\n", result))

