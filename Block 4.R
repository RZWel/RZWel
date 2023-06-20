# Uppgift 4. . Jämföra två stickprov gruppvis, populationens standardavvikelse är okänd

Grupp=c("A","A","A","A", "A","A","A","A", "A", "A", "A","A", "A","A","A","A","B","B","B","B","B","B","B","B","B","B","B","B","B","B","B","B")
Vikt=c(90,91,93,106,97,108,97,105,106,103,105,96,105,95,90,101,90,91,85,99,93,104,89,103,102,95,103,95,105,87,86,101)
Gruppvis_Vikt<-data.frame(Vikt,Grupp)

 A<-Gruppvis_Vikt[1:16,]
 B<-Gruppvis_Vikt[17:32,]
 
 mean(A[,1])
 mean(B[,1])
 
 attach(Gruppvis_Vikt)
 t.test(Vikt~Grupp, var.equal=TRUE, conf.level = 0.95) 
 
 attach(Gruppvis_Vikt)
 t.test(Vikt~Grupp, var.equal=TRUE, conf.level = 0.95) 
 # Welch t-test 
 attach(Gruppvis_Vikt)
 t.test(Vikt~Grupp, conf.level = 0.95) 
 
 # paired t-test 
 Parvis.Vikt<- read.table("clipboard", header=T)
 t.test(Vikt ~ Grupp, paired = T)
 
 
 # uppgift 6 
 count_diagnosis<- read.table("clipboard", header=T)
 hist(count_diagnosis$count)
 
 count_dengue<-count_diagnosis[1:8,1]
 count_scrub<-count_diagnosis[9:16,1]
 
  wilcox.test(count_dengue,count_scrub)
t.test(count_dengue,count_scrub, var.equal=TRUE, conf.level=0.95)
 
 #Nollhypotes och mothypotes 
 #Nollhypotesen är att skillnaden i vikt mellan grupperna är signifikant. Mothypotesen är att det inte finns en signifikant skillnad mellan grupperna avseende vikt

# Uppgift 2 
z = ( X- µ)/ (??/???n)  
x=31.38
µ=31.2 
??=0.4
n=16 
z=(31.38-31.2)/(0.4/(4))
print(z)

1-pnorm(1.8)

curve(dnorm(x,31.2, 0.4/sqrt(16)), from= 30.80, to=31.60, ylab="distribution")  # from och to baserat på sd=0.4. x är vårt medelvärde
abline(v=31.38)  # vertikal linje för medelvärdet av stickprovet.
kord.x2 <- c(31.38,seq(31.38,31.60,0.01),31.60)  # Koordinater för att beskriva delen som skall målas
kord.y2 <- c(0,dnorm(seq(31.38,31.60,0.01), 31.2, 0.4/sqrt(16)),0) # Koordinater för y som skall målas
polygon(cord.x1,cord.y1,col='green')  # polynom 


# Standard deviations plot 

# Grid of X-axis values # standard deviations
x <- seq(-4,4,1)

#-----------------------------------------
#-----------------------------------------
# Mean 
plot(x, dnorm(x, mean =1.8, sd = 1), type = "l",
     ylim = c(0, 0.6), ylab = "", lwd = 2, col = "red")


#
blodtry <- read.delim("clipboard")
anova(lm(blodtry$blodtryck ~blodtry$behandling))
Pr(>)



# Mean 3, sd 1
lines(x, dnorm(x, mean = 3, sd = 1), col = "blue", lty = 1, lwd = 2)

# Adding a legend
legend("topright", legend = c("0 1", "3 1"), col = c("red", "blue"),
       title = expression(paste(mu, " ", sigma)),
       title.adj = 0.9, lty = 1, lwd = 2, box.lty = 0)
