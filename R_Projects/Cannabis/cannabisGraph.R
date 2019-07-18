#install.packages("tidyverse")

library(tidyverse)

cannabis <- tibble(
  t = seq(-pi,pi, length.out=1000),
  r1 = (1+.9*cos(8*t)), ## this will draw 8 petals  ## this number determines number of leafs!
  r2 = r1 * (1+.1*cos(24*t)), ## this make it pointy
  r3 = r2 * (.9+0.5*cos(200*t)), ## this makes it jaggy
  r4 = r3 * (1+sin(t)), ## Hmm.. I think I want to rorate it 90 degree... 
  r4_alt = r3 * (1+sin(t-pi/2)), ## one way to do it...
  r = (1+.9*cos(8*t)) * (1+.1*cos(24*t)) * (.9+0.5*cos(200*t)) * (1+sin(t))  ## Put all inline!
) 

# this plots the individual rn variables declared in the tibble above

# draws 8 petals
cannabis %>% 
  ggplot(aes(x=t, y=r1)) + 
  geom_path(color="#7ABA71", size=2) +
  coord_polar() +
  theme_void(base_family="Roboto Condensed") +
  labs(title = "(1+.9*cos(8*t) draws 8 petals")

# makes tips pointy
cannabis %>% 
  ggplot(aes(x=t, y=r2)) + 
  geom_path(color="#7ABA71", size=2) +
  coord_polar() +
  theme_void(base_family="Roboto Condensed") +
  labs(title = "(1+.9*cos(8*t) * * (1+.1*cos(24*t)) makes the tip pointy")

# makes the zag
cannabis %>% 
  ggplot(aes(x=t, y=r3)) + 
  geom_path(color="#7ABA71", size=0.5) +
  coord_polar() +
  theme_void(base_family="Roboto Condensed") +
  labs(title = "(1+.9*cos(8*t) * * (1+.1*cos(24*t)) * (.9+0.5*cos(200*t)) makes zaggy")

# rotate with PI/n
cannabis %>% 
  ggplot(aes(x=t, y=r4)) + 
  geom_path(color="#7ABA71", size=0.5) +
  coord_polar(start=pi/2) +
  theme_void(base_family="Roboto Condensed") +
  labs(title = "(1+.9*cos(8*t) * * (1+.1*cos(24*t)) * (.9+0.5*cos(200*t)) * (1+sin(t)) - OK Cool, Now 2 leaves are small!", subcaption="Notice I used start=pi/2 to rotate!")

# use polygons
cannabis %>% 
  ggplot(aes(x=t, y=r)) + 
  geom_polygon(fill="#499b4a", color="#74Ba71", size=0.1) +
  coord_polar(theta="x", start=pi/2) +
  theme_void(base_family="Roboto Condensed") +
  labs(title = "Instead of using geom_path, I used geom_polygon")

# clean-up
cannabis %>% 
  ggplot(aes(x=t, y=r)) +
  theme_void()+
  theme(panel.background = element_rect(fill = "black",
                                    colour = "black"))+
  geom_polygon(fill="#499b4a", color="#74Ba71", size=0.1) +
  coord_polar(theta="x", start=pi/2) +
  labs(title = "r = (1+.9*cos(8*t)) * (1+.1*cos(24*t)) * (.9+0.5*cos(200*t)) * (1+sin(t))")
