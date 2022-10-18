corr_grad_fill <- function(data, mapping, method=c("pearson", "kendall", "spearman"), use="everything",   
                           low = "#3B9AB2",
                           mid = "#EEEEEE",
                           high = "#F21A00", ...){
  
  # grab data
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  
  # calculate correlation
  corr <- cor(x, y, method=method, use=use)
  
  # calculate colour based on correlation value
  # Here I have set a correlation of minus one to blue, 
  # zero to white, and one to red 
  colFn <- colorRampPalette(c(low, mid, high), interpolate ='spline')
  fill <- colFn(100)[findInterval(corr, seq(-1, 1, length=100))]
  
  p <- 
    ggally_cor(data = data, mapping = mapping, ...) + 
    theme_void() +
    theme(panel.background = element_rect(fill=fill))
  p
}
