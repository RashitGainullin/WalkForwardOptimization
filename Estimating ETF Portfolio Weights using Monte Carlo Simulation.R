library( QuantTools )

#set root folder
setwd('...')


symbols = fread( '
    symbol | comment
    SPY    | S&P500
    QQQ    | Nasdaq100
    VGK    | Europe Developed International Market equities
    EWJ    | Japan, Developed International Market equities
    EEM    | Emerging Market equities
    IYR    | REIT
    GSG    | Commodities
    GLD    | Gold
    HYG    | High Yield bonds
    SHY    | Short Term US Treasuries 1-3y
                      ', blank.lines.skip = T )

from = '2010-01-01'
to = Sys.Date()
data              = symbols[,{ get_yahoo_data( symbol, from = from, to = to, split.adjusted = T, dividend.adjusted = T )}, by = symbol ]
close_prices      = dcast( data, date ~ symbol, value.var = 'close' ) 

etf_returns = data[ ,.( date, return = close / shift( close , fill = open[1] ) - 1 ), by = symbol]
etf_dynamic = dcast( etf_returns[,.(date, pnl = 100 * cumprod( return + 1 ) - 1 ), symbol], date ~ symbol, value.var = 'pnl' ) 

#Plot ETF Universe Performance
par( mfrow = c( 1,1 ), oma = c( 5, 4, 2, 4 ) + 0.1, mar = c( 0, 0, 1, 0 ), xaxt = 's',cex = 0.8 )
plot_dts( etf_dynamic )$lines( lwd = 2 )$plot( )
title( 'ETF PnL, %', adj = 0, line = 0 )

etf_dynamic_m = as.matrix( etf_dynamic[,-1] )
dates = etf_dynamic[,date] 

get_mc_portfolio_weights  = function( N = 10000, n = 10, min = 0.0, max = .3, step = 0.01 ) {
  
  if( max * n < 1 ) stop( 'max weights not add up to 1' )
  
  W = matrix( NA, nrow = N, ncol = n )
  
  i = 1
  j = 1
  
  while( i <= N ) {
    
    j = j + 1
    
    w = runif( n, min = min, max = max )
    
    w = round( w / sum( w ) / step ) * step
    
    if( sum( w ) == 1 & all( w >= min & w <= max ) ) {
      
      W[ i, ] = w
      i = i + 1
      
    }
    
  }
  message( j )
  
  data.table( W )
  
}
weights = get_mc_portfolio_weights( 10000 )

portfolio_dynamic = rbindlist( apply( weights, 1, function( w ) {
  data.table( date = dates, price = c(etf_dynamic_m %*% ( w )) )
}), idcol = 'id' )

#Plot Portfolio Universe Performance
png(filename = 'portfolio universe pnl dynamic.png',width = 600, height = 600, units = "px")
xx = dcast( portfolio_dynamic, date ~ id, value.var = 'price' )
xx = xx[,c(1,order(-xx[.N,-1])+1),with = F] 
par( mfrow = c( 1,1 ), oma = c( 5, 4, 2, 4 ) + 0.1, mar = c( 0, 0, 1, 0 ), xaxt = 's',cex = 0.8 )
plot_dts(xx)$lines( col = rainbow(ncol(xx)-1) )$style( legend = list( visible = F ), time = list( resolution = 'years'))$plot( )
title( 'Portfolio\'s PnL, %', adj = 0, line = 0 )
dev.off()

portfolio_return = portfolio_dynamic[,.(date,return = price / shift( price, fill = 100) - 1 ),id]

is_oos_split_date = '2018-01-01'
train_return = portfolio_return[date <  is_oos_split_date,.(return = ( prod( return + 1 ) ^ ( 252 / .N ) - 1 ) * 100, sd = sd( return ), sharpe = mean(return) / sd( return ) * sqrt(252)),id]
test_return  = portfolio_return[date >= is_oos_split_date,.(return = ( prod( return + 1 ) ^ ( 252 / .N ) - 1 ) * 100, sd = sd( return ), sharpe = mean(return) / sd( return ) * sqrt(252)),id]

efficient_frontier = train_return[,.SD[which.max(return),.(id,return,sd,sharpe)][1], by = .(round(sd*20,3))][sd < 0.009]
setorder( efficient_frontier, sd )
oos_efficient_frontier = test_return[id %in% efficient_frontier[,unique(id)]]

is_etf_statistics  = etf_returns[date <  is_oos_split_date,.(return = ( prod( return + 1 ) ^ ( 252 / .N ) - 1 ) * 100, sd = sd( return ), sharpe = mean(return) / sd( return )* sqrt(252)), .(id = symbol)]
oos_etf_statistics = etf_returns[date >=  is_oos_split_date,.(return = ( prod( return + 1 ) ^ ( 252 / .N ) - 1 ) * 100, sd = sd( return ), sharpe = mean(return) / sd( return )* sqrt(252)), .(id = symbol)]


library("ggplot2")
ggplot(data = train_return[,.(id,return,sd, sharpe,tag = sharpe)], aes(x=sd,y = return)) + 
  geom_point( aes( color = tag ), show.legend = TRUE ) +
  geom_point( data = efficient_frontier[ ,.( id, return, sd, sharpe, tag = 'Efficient Frontier')], aes(x = sd, y = return), colour = 'red')  +
  geom_point( data = is_etf_statistics    [ ,.( id, return, sd, sharpe, tag =                'ETF')], aes(x = sd, y = return), colour = 'black', shape = 18, size = 2)  +
  geom_text( data = is_etf_statistics, aes( label = id ), hjust = 0, vjust = 0 ) +
  ylab("Annualized Return, %") + xlab("Standard Deviation") + 
  scale_y_continuous( limits = c(-10,20)) + 
  scale_colour_continuous(limits =c(0,1.25)) + 
  scale_x_continuous( limits = c(0,0.02)) +
  guides(colour = guide_legend("Sharpe Ratio")) + ggtitle("Annualized Return-Standard Deviation plot \nbefore 2018-01-01")


ggplot(data = test_return[,.(id,return,sd, sharpe,tag = sharpe)], aes(x=sd,y = return)) + 
  geom_point( aes( color = tag ), show.legend = TRUE ) +
  geom_point( data = oos_efficient_frontier[ ,.( id, return, sd, sharpe, tag = 'Efficient Frontier')], aes(x = sd, y = return), colour = 'red')  +
  geom_point( data = oos_etf_statistics    [ ,.( id, return, sd, sharpe, tag =                'ETF')], aes(x = sd, y = return), colour = 'black', shape = 18, size = 2)  +
  geom_text( data = oos_etf_statistics, aes( label = id ), hjust = 0, vjust = 0 ) +
  ylab("Annualized Return, %") + xlab("Standard Deviation")+
  scale_y_continuous( limits = c(-10,20)) + 
  scale_colour_continuous(limits =c(0,1.25)) + 
  scale_x_continuous( limits = c(0,0.02)) +
  guides(colour = guide_legend("Sharpe Ratio")) + ggtitle("\nafter 2018-01-01")

#Walk Forwart Statistics
lti = data.table( lti = c(6,12,24,36) )
Rcpp::sourceCpp( "roll_statistics_monthly.cpp" )

#see how fast calculate rolling statistics on 10k portfolio paths woth 4 different rolling windows
system.time({
  is_statistics = lti[, portfolio_return[, roll_statistics_monthly( .SD, lti ), by = .( id ) ], by = lti ]
})

#set optimal solution metrics
osm = fread(
  'osm_id | osm_formula
    1      | pnl 
    2      | pnl * r_squared
    3      | pnl * r_squared / -max_dd
    4      | sharpe
    ' )

osms = osm[, is_statistics[, .( interval_date, id,  lti, osm = eval( parse( text = osm_formula ) ) ) ], by = osm_id ]
optimums = osms[ , .( id = id[ which.max( osm ) ]), by = list( train_date = interval_date,  lti, osm_id ) ]
portfolio_return[,train_date := as.Date(format(date,"%Y-%m-01"))]

oos_return = merge( optimums, portfolio_return, by = c('train_date','id'))
oos_return[,pnl := 100 * (cumprod( return + 1) - 1), by = .(lti,osm_id)]

oos_pnl = dcast( oos_return, date ~ osm_id + lti, value.var = 'pnl')

xx = oos_pnl[,c(1,order(-oos_pnl[.N,-1])+1),with = F] 

par( mfrow = c( 1,1 ), oma = c( 5, 4, 2, 4 ) + 0.1, mar = c( 0, 0, 1, 0 ), xaxt = 's',cex = 0.8 )
plot_dts( xx )$lines( lwd = 1.2 )$style( legend = list( visible = T ), time = list( resolution = 'years'))$plot( )
title( 'OOS Portfolio\'s PnL, %', adj = 0, line = 0 )





