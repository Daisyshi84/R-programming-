
# example snellen convert to LogMAR   (snellen use the denominator to calculate i.e. 20/16 )
snellen <- c(20, 16)
LogMAR <- -log10(snellen)
 


# example LogMAR convert to ETDRSscore
LogMAR <- c(0.3, 0.4)   
ETDRSscore <- 30 + (1.1 - LogMAR) / 0.02
ETDRSscore



# Example ETDRS scores convert to logmar and snellen
etdrs <- c(85, 70)  # replace with your actual ETDRS scores
# Calculate LogMAR values from ETDRS scores
logmar <- 1.1 - (etdrs - 30) * 0.02
# Convert LogMAR values to Snellen values
snellen <- 20 / (10 ^ (-logmar))
# Print the Snellen values
snellen




# Example use Snellen values convert to logmar,etdrs
snellen <- c(1002, 796, 632, 502, 399, 317, 252, 200, 159, 126, 100, 80, 63, 50, 40, 32, 25, 20, 16, 13, 10)
# Convert Snellen values to LogMAR
logmar <- round(log10(snellen / 20),1)
# Calculate ETDRS scores from LogMAR
etdrs <- round(30 + (1.1 - logmar) / 0.02, 0)
# Combine the results into a matrix
result <- cbind(etdrs, logmar, snellen)
# Print the combined results
result 

result<- as.data.frame(result)

result<- result %>% 
  mutate(ETDRSscore = round(30 + (1.1 - logmar) / 0.02, 0),
         Logmar = round(log10(snellen / 20),1),
         Snellen = round(20 / (10 ^ (-logmar)),0) )
