#!/opt/local/bin/Rscript

# @author Alexander Taran

train = read.csv("train.csv")
test  = read.csv("test.csv" )

# Launch params:

vis = c(1, 5, 11, 101, 401)
errs = c(1:100)
grid_size = 500

# General functions:
dst = function(m1,m2) {
  return( (m1[['x']]-m2[['x']]) ** 2 + (m1[['y']]-m2[['y']]) ** 2 )
}

gen_grid = function(train, w, h) {
  k = 1.1
  left = min(train[['x']])
  bottom = min(train[['y']])
  right = max(train[['x']])
  top = max(train[['y']])
  width = right - left
  height = top - bottom
  cx = (left + right) / 2
  cy = (top + bottom) / 2
  
  x = seq(from = cx-width/2*k, to = cx+width/2*k, length = w)
  y = seq(from = cy-height/2*k, to = cy+height/2*k, length = h)
  df = expand.grid(x,y)
  names(df) = c('x','y')
  return(df)
}

Classify = function(train, objects, k) {
  knn_single = function (o) {
    ds = function(o1) {
      return(dst(o1,o))
    }
    or = train[order(apply(train, 1, ds)),][c(1:k),]
    return(as.integer(nrow(subset(or,class==1)) > k / 2))
  }
  apply(objects, 1, knn_single)
}

# Part 1: visualization
print('Starting visualization part')
grid = gen_grid(train, grid_size, grid_size)
zone_col = c('green','red')
pt_col = c('black', 'blue')
start = proc.time()
for(idx in 1:length(vis)) {
  print(vis[idx])
  result = Classify(train, grid, vis[idx])
  plot(grid[['x']], grid[['y']], pch='.', col = zone_col[result+1])
  points(train[['x']], train[['y']], pch=20, col=pt_col[train[['class']]+1])
}
end = proc.time()
print(end-start)

# Part 2: error plots
print('Staring error-plots part')
start = proc.time()
cl = c()
ck = c()
for(idx in 1:length(errs)) {
  Xl = length(train[['class']]) - sum(Classify(train, train,errs[idx]) == train[['class']])
  print(c(errs[idx], Xl))
  Xk = length(test [['class']]) - sum(Classify(train, test, errs[idx]) == test [['class']])
  print(c(errs[idx], Xk))
  cl = c(cl,Xl)
  ck = c(ck,Xk)
}
plot(errs, cl, type='b', pch = 20)
plot(errs, ck, type='b', pch = 20)

end = proc.time()
print(end - start)


