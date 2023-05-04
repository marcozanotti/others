## Memory Limit and Buffer

memory.limit(size = xxx)
memory.size(max = T)


#img è il path dell'immagine
i1 <- raster(img)
delta <- raster(img)
delta <- writeStart(delta, paste(output_path, fname, sep=''), overwrite = TRUE)
bs <- blockSize(delta)

for (i in 1:bs$n) 
{
  message("\tprocessing chunk ", i, " of ", bs$n)
  v1 <- getValuesBlock(i1, row = bs$row[i], nrows = bs$nrows[i])
  
  newRow <- # qui si fa il processing di v1 e lo si caccia in una variabile 
    
    delta <- writeValues(delta, newRow, bs$row[i]) # qui si scarica il buffer
}

delta <- writeStop(delta) # qui viene finalizzato lo stream

https://cran.r-project.org/web/packages/raster/vignettes/functions.pdf

