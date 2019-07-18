#packages.install(plot3d)
library(plot3D)

u = seq(0, 1, length=10)
v = seq(-pi, pi, length=40)

x = outer(sqrt(u), cos(v))
y = outer(sqrt(u), sin(v))
z = x*y

surf3D(x,
       y,
       z,
       colkey=FALSE,
       bty="b2",
       main="Hyperbolic Paraboloid in R")