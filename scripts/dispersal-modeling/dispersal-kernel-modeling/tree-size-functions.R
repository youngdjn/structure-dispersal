# Allometric functions to estimate diam and ba from height

ba01 = function(h) {
  a = 1.4721
  b = 0.6848
  3.14 * (   ((h/a)^(1/b))   /2)^2
}

diam01 = function(h) {
  a = 1.4721
  b = 0.6848
  ((h/a)^(1/b))
}

height = function(h) {
  h
}