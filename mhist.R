mhist = function(xx, linecol = "blue", lwd = 2, col = "lightyellow", border = "darkgray", normdist = TRUE, main = NULL, xlab = deparse(substitute(xx)), ...) {
  
  xxx = seq(min(xx), max(xx), length.out = 100)
  if(is.null(main)) main = paste0("Histograma de ", deparse(substitute(xx)))
  yyy = 0
  if(normdist) yyy = dnorm(xxx, mean(xx), sd(xx))
  h = graphics::hist(x = xx, ..., plot = FALSE)
  sub = ""
  if(normdist) sub = paste("Shapiro p-value", (shapiro.test(xx)$p.value %>% round(2)), collapse = " = ")
  h = graphics::hist(x = xx, ..., freq = FALSE, plot = TRUE, ylim = c(0, max(c(h$density, yyy))), col = col, border = border, sub = sub, main = main, xlab = xlab)
  if(normdist) {
    curve(dnorm(x, mean(xx), sd(xx)), add = TRUE, col = linecol, lwd = lwd)
  }
}
