library(tidyverse)


kernel_all = dnorm(0:300, mean = 0, sd = 35)
kernel_fir = dnorm(0:300, mean = 0, sd = 145)
kernel_pine = dnorm(0:300, mean = 0, sd = 45)

d_all = data.frame(kernel = kernel_all, dist = 0:300, species = "All")
d_fir = data.frame(kernel = kernel_fir, dist = 0:300, species = "Fir")
d_pine = data.frame(kernel = kernel_pine, dist = 0:300, species = "Pine")

d = bind_rows(d_all, d_fir, d_pine)

p = ggplot(d, aes(x = dist, y = kernel, color = species)) +
  geom_line() +
  scale_color_viridis_d(begin = 0.2, end = 0.8, name = "Species\ngroup") +
  theme_bw(12) +
  labs(x = "Distance (m)",
       y = "Kernel density")
p

png("~/Downloads/postcrpt_kernel.png", res = 700, width = 3000, height = 2000)
p
dev.off()
