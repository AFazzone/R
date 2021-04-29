# Explore the following to understand the sizes of the mosaic plot

x <- matrix(c(50,25,10,15), nrow=2,
            byrow = TRUE)
x

rownames(x) <- c("Class1", "Class2")
colnames(x) <- c("Pass", "Fail")

x

addmargins(x)

t(x)

addmargins(t(x))


par(mfrow=c(2,2))

mosaicplot(x, color=c("green", "red"),
           main = "mosaicplot( x )")

mosaicplot(t(x), color=c("red", "blue"),
           main = "mosaicplot( t(x) )")


barplot(t(x), xlab = "Class", 
        main = "barplot( t(x) )",
        ylim=c(0,80), col=c("green", "red"))

legend("topright", legend = c("Pass", "Fail"), 
       fill = c("green", "red"), horiz = TRUE, cex=0.8)


barplot(x, xlab = "Pass/Fail",
        main = "barplot( x )",
        ylim=c(0,80), col=c("red", "blue"))

legend("topright", legend = c("Class1", "Class2"), 
       fill = c("red", "blue"), horiz = TRUE, cex=0.8)

par(mfrow=c(1,1))


