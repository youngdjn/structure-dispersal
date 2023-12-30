library(tidyverse)

calc_kern_2Dt = function(a, k, r) {
  kern = k / (pi*a) * (1 + r^2/a)^(-1-k) # 2Dt
}

calc_kern_exppow = function(a, k, r) {
  kern = k / (2*pi * a^2 * gamma(2/k)) * exp(-(r / a)^k) # exppow
}


a = 100 # for 2Dt, fitted a is around 10000, for exppow, it's around 100
k = 1
r = 0:500
kern = calc_kern_exppow(a = a, k = k, r = r)
kern_df_1 = data.frame(r = r, kern = kern, a = as.character(a), k = as.character(k), form = "1")

a = 100 # for 2Dt, fitted a is around 10000, for exppow, it's around 100
k = 2
r = 0:500
kern = calc_kern_exppow(a = a, k = k, r = r)
kern_df_2 = data.frame(r = r, kern = kern, a = as.character(a), k = as.character(k), form = "2")

kern_df = bind_rows(kern_df_1, kern_df_2)

ggplot(kern_df, aes(x=r, y=kern, color=k)) +
  geom_line()
