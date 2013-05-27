#############################################################################
## Function to Calculate Monthly Mortgage Payments and Amortization Tables ##
#############################################################################
# Author: Thomas Girke
# Last update: Feb 27, 2007
# Utility: Calculates monthly and annual loan or mortgage payments, generates amortization tables and plots the results
# How to run the script:
#	source("http://faculty.ucr.edu/~tgirke/Documents/R_BioCond/My_R_Scripts/mortgage.R")

# Definitions: 
#	  P = principal, the initial amount of the loan
#	  I = annual interest rate
#	  L = length of the loan in years, or at least the length over which the loan is amortized.
#	  J = monthly interest in decimal form = I / (12 x 100)
#	  M = monthly payment; formula: M = P * ( J / (1 - (1 + J) ^ -N))
#	  N = number of months over which loan is amortized = L x 12
# see also: http://www.jeacle.ie/mortgage/instructions.html

mortgage <- function(asking.price=500000, pct.down=20, I=4, L=30, amort=T, plotData=F, 
                      taxes.year=0, condofees=0, parking=0, lastyearstatetax=3438,
                     std.deduction=12200, marginal.tax.rate = 0.25) { 
  P <- asking.price*(1 - pct.down/100)
	J <- I/(12 * 100)
	N <- 12 * L
	M <- P*J/(1-(1+J)^(-N))
	monthPay <<- M
  taxes.month <- taxes.year/12
  tot.month.cost <- M + condofees + taxes.month + parking
	cat("\nAsking price of home: $", format(asking.price,scientific=FALSE,big.mark=","), "\n
    ",pct.down,"% down: $", format(asking.price*pct.down/100,scientific=FALSE,big.mark=",") ,"\n
    Mortgage amount: $", format(P,scientific=FALSE,big.mark=","),"\n
The payments for this loan are:\n 
    TOTAL MONTHLY COST: $", format(round(tot.month.cost,2),big.mark=",",digits=2,nsmall=2), "\n		
      Monthly mortgage payment: $", format(round(M,2),big.mark=","), " (stored in monthPay)\n
      Condo fees: $", round(condofees,2), " \n
      Monthly taxes: $", round(taxes.month,2) ," \n
      Parking: $", round(parking,2) ," \n
    Total lifetime cost of mortgage: $", format(M*N,scientific=FALSE,big.mark=",",nsmall=2), "\n\n", sep="")
	# Calculate Amortization for each Month
	if(amort==T) {
		Pt <- P # current principal or amount of the loan
		currP <- NULL
		while(Pt>=0) {
			H <- Pt * J # this is the current monthly interest
			C <- M - H # this is your monthly payment minus your monthly interest, so it is the amount of principal you pay for that month
			Q <- Pt - C # this is the new balance of your principal of your loan
			Pt <- Q # sets P equal to Q and goes back to step 1. The loop continues until the value Q (and hence P) goes to zero
			currP <- c(currP, Pt)
		}
		monthP <- c(P, currP[1:(length(currP)-1)])-currP
		aDFmonth <<- data.frame(
					      Amortization=c(P, currP[1:(length(currP)-1)]), 
					      Monthly_Payment=monthP+c((monthPay-monthP)[1:(length(monthP)-1)],0),
					      Monthly_Principal=monthP, 
					      Monthly_Interest=c((monthPay-monthP)[1:(length(monthP)-1)],0), 
					      Year=sort(rep(1:ceiling(N/12), 12))[1:length(monthP)]
				)
		aDFyear <- data.frame(
					     Amortization=tapply(aDFmonth$Amortization, aDFmonth$Year, max), 
					     Annual_Payment=tapply(aDFmonth$Monthly_Payment, aDFmonth$Year, sum), 
					     Annual_Principal=tapply(aDFmonth$Monthly_Principal, aDFmonth$Year, sum), 
					     Annual_Interest=tapply(aDFmonth$Monthly_Interest, aDFmonth$Year, sum), 
               Year=as.vector(na.omit(unique(aDFmonth$Year)))
					     )
		aDFyear <<- aDFyear
		cat("The amortization data for each of the", N, "months are stored in \"aDFmonth\".\n\n")
		cat("The amortization data for each of the", L, "years are stored in \"aDFyear\".\n\n")
    cat("*************** Tax Stuff ***************\n\n")
    cat("First year interest paid is $", format(aDFyear$Annual_Interest[1],scientific=F,big.mark=",",nsmall=2), "\n\n", sep = "")
    cat("Last year's state income tax paid: $", format(lastyearstatetax,scientific=F,big.mark=","), "\n\n", sep = "")
    cat("Yearly property tax: $", format(taxes.year,scientific=F,big.mark=","), "\n\n", sep = "")
    cat("Standard deduction for this year is $", format(std.deduction,scientific=F,big.mark=","), 
        "\nand marginal tax rate is ", marginal.tax.rate*100,"%\n\n", sep = "")
    deduction <- aDFyear$Annual_Interest[1] + lastyearstatetax + taxes.year
    if(deduction > std.deduction){
      monthly.tax.savings <- marginal.tax.rate * (deduction - std.deduction) / 12
      cat("Monthly tax savings is $", format(monthly.tax.savings,scientific=F,big.mark=",",digits=2,nsmall=2), "\n\n", sep = "")
      after.tax.cost <- tot.month.cost - monthly.tax.savings
      cat("After tax monthly cost of mortgage is $", format(after.tax.cost,scientific=F,big.mark=",",digits=2,nsmall=2), "\n\n", sep = "")
    } 
    else {
      cat("Standard deduction is greater and you should not itemize!!")
    }
	}
	if(plotData==T) {
	barplot(t(aDFyear[,c(3,4)]), 
		col=c("blue", "red"), 
		main="Annual Interest and Principal Payments", 
		sub="The data for this plot is stored in aDFyear.",
		xlab="Years", ylab="$ Amount", 
		legend.text=c("Principal", "Interest"), 
		ylim=c(0, max(aDFyear$Annual_Payment)*1.3))
	}
}

mortgage(asking.price=582000, I=3.99, pct.down=20, condofees=477, 
         taxes.year=3357, parking=150)


