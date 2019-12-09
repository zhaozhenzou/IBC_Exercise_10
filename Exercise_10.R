library(ggplot2)
library(reshape2)

# set up parameters for the dynamic model
# assume drug treatment starting from day 300
time <- 1000
drug <- 300
N0 <- 99
M0 <- 1
Rn <- 0.1
Rm <- 0.1
Rn_drug <- -0.1
Rm_drug <- 0.05
k <- 1000000

# dynamic model
N <- numeric(length=time)
N[1] <- N0
M <- numeric(length=time)
M[1] <- M0
for (i in 1:time) {
  if (i>=drug) {
    N[i+1] <- N[i] + Rn_drug*N[i]*(1-(N[i]+M[i])/k)
    M[i+1] <- M[i] + Rm_drug*M[i]*(1-(N[i]+M[i])/k)
  }else {
    N[i+1] <- N[i] + Rn*N[i]*(1-(N[i]+M[i])/k)
    M[i+1] <- M[i] + Rm*M[i]*(1-(N[i]+M[i])/k)
  }
}

# plot line graphs for normal and mutant cancer cells
df <- data.frame(time=0:time, normal=N, mutant=M)
df_factor <- melt(df, id.vars="time", variable.name="type", value.name="cancer")
ggplot(data=df_factor, aes(x=time, y=cancer, colour=type)) +
  geom_line() + theme_classic()
  