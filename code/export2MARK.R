# Mise en format MARK

# Données capture-recapture hiver 1997/1998 -> hiver 2018/2019
dat <- as.matrix(readRDS("dat/cmrlouphiver.rds"))

# Convert to live and dead (LD) format
to_ld <- function(inp) {
  return(switch(as.character(inp), "0" = "00", "1" = "10", "2" = "01"))
}

dat_ld <- apply(dat, c(1, 2), to_ld)

# Reset recaptures and recoveries file
cat("", file = "dat/mark_ld.inp")

# Output rows as strings
for (row in 1:nrow(dat_ld)) { 
  cat(paste(paste(dat_ld[row,], collapse = ""), "1;\n"), 
      file = "dat/mark_ld.inp", append = TRUE)
}

# Convert to live (L) format
dat[dat == 2] <- 1

# Reset recaptures only file
cat("", file = "dat/mark.inp")

# Output rows as strings
for (row in 1:nrow(dat)) { 
  cat(paste(paste(dat[row,], collapse = ""), "1;\n"), 
      file = "dat/mark.inp", append = TRUE)
}


