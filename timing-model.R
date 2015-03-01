# 10-month MA timing model (rebalanced monthly)

# Generates a backtest value stream based on holding the one (of two) funds that has higher momentum score (3/6/12-month adj. return)
library(PerformanceAnalytics)
library(quantmod)
source('backTest.R')

# Start date of the "latest" of these is 2002-07-30
getSymbols(c('VTI','TLT','EFA','IYR','XLV','SHY','IEF'), from='2002-07-30')

stock <- VTI
bonds <- TLT
inter <- EFA
cash <- SHY
reale <- IYR
sector <- XLV

ma.list <- list()


MAorder <- c(10,10,10,10,10)

stock.mo <- SMA(Cl(to.monthly(Ad(stock))), MAorder[1])
bonds.mo <- SMA(Cl(to.monthly(Ad(bonds))), MAorder[2])
inter.mo <- SMA(Cl(to.monthly(Ad(inter))), MAorder[3])
reale.mo <- SMA(Cl(to.monthly(Ad(reale))), MAorder[4])
healt.mo <- SMA(Cl(to.monthly(Ad(sector))), MAorder[5])

startpts <- c(as.Date(first(index(stock.mo[which(!is.na(stock.mo))]))), as.Date(first(index(bonds.mo[which(!is.na(bonds.mo))]))), 
              as.Date(first(index(inter.mo[which(!is.na(inter.mo))]))), as.Date(first(index(reale.mo[which(!is.na(reale.mo))]))), 
              as.Date(first(index(healt.mo[which(!is.na(healt.mo))]))))
# Get starting point in string form (for use in subsetting xts objects)
start.date <- max(startpts)
start.range <- paste(start.date, "::", sep="")

# Remove leading NAs
stock.mo <- stock.mo[start.range]
bonds.mo <- bonds.mo[start.range]
inter.mo <- inter.mo[start.range]
reale.mo <- reale.mo[start.range]
healt.mo <- healt.mo[start.range]

# Generate monthly prices to match up with these
price.stock <- Cl(to.monthly(Ad(stock[start.range])))
price.bonds <- Cl(to.monthly(Ad(bonds[start.range])))
price.inter <- Cl(to.monthly(Ad(inter[start.range])))
price.reale <- Cl(to.monthly(Ad(IYR[start.range])))
price.healt <- Cl(to.monthly(Ad(XLV[start.range])))
price.cash  <- Cl(to.monthly(Ad(cash[start.range])))
# Portfolio holding signals
#
# Need vector of weights
weights <- c(.2,.2,.2,.2,.2)

hold.stock <- weights[1]*(price.stock > stock.mo)
hold.bonds <- weights[2]*(price.bonds > bonds.mo)
hold.inter <- weights[3]*(price.inter > inter.mo)
hold.reale <- weights[4]*(price.reale > reale.mo)
hold.healt <- weights[5]*(price.healt > healt.mo)
hold.cash  <- cbind((price.stock < stock.mo) , (price.bonds < bonds.mo) , (price.inter < inter.mo) , (price.reale < reale.mo), (price.healt < healt.mo))%*%weights

# Put signal/timing into data frame
df <- cbind(hold.stock, hold.bonds, hold.inter, hold.reale, hold.healt, hold.cash
            , price.stock, price.bonds, price.inter, price.reale, price.healt, price.cash)
names(df) <- c('HoldStock', 'HoldBonds', 'HoldInter', 'HoldReale', 'HoldHealt', 'HoldCash'
               , 'PriceStock', 'PriceBond', 'PriceInter','PriceReale','HoldHealt', 'PriceCash')

timing_model.xts <- backTest(df)