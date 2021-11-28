# Mise en format MARK

# Données capture-recapture hiver 1997/1998 -> hiver 2018/2019
dat <- readRDS("dat/cmrlouphiver.rds")

dat[dat == 2] <- 1

# Reset file
cat("", file = "dat/mark.inp")

# Output rows as strings
for (row in 1:nrow(dat)) { 
  cat(paste(paste(dat[row,], collapse = ""), "1;\n"), 
      file = "dat/mark.inp", append = TRUE)
}


