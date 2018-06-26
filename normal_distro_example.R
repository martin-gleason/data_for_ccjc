#Normal Distrubtion Example

normal_d <- data.frame(X = rnorm(1000), Y = abs(rnorm(1000)))





ggplot(normal_d, aes(X, y = Y)) + 
  geom_density()



myd = data.frame(xvar=rnorm(2000),yvar=rnorm(2000))

#xd <- data.frame(density(myd$xvar)[c("x", "y")])
p <- ggplot(xd, aes(x = xvar,y = yvar)) + 
  
  geom_area(data = subset(xd, x < -1), fill = "pink") +
  geom_area(data = subset(xd, x < -1.96), fill = "red") +
  geom_area(data = subset(xd, x > 1), fill = "lightgreen") +
  geom_area(data = subset(xd, x > 1.96), fill = "green") +
  
  geom_line()

p 

class(myd$xvar)
class(myd$yvar)
hist(myd$xvar)
hist(myd$yvar)

plot(xvar, yvar)



x = rnorm(1000);

# overlay histogram, empirical density and normal density
p0 = qplot(x, geom = 'blank') +   
  geom_line(aes(y = ..density.., colour = 'Empirical'), stat = 'density') +  
  stat_function(fun = dnorm, aes(colour = 'Normal')) +                       
  geom_histogram(aes(y = ..density..), alpha = 0.4) +                        
  scale_colour_manual(name = 'Density', values = c('red', 'blue')) + 
  theme(legend.position = c(0.85, 0.85))

print(p0)

