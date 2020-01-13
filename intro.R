library( QuantTools )


#Let's store market data for GAZP and SBER tickers (used by default) locally
store_finam_data(to = Sys.Date(),verbose = T)

#Load market data locally
candles = get_finam_data('GAZP', '2016-01-01', to = '2020-01-01', period = "1min", local = T)
ticks = to_ticks( candles )

# set options, see ?Processor 'Options' section
options = list(
  cost    = list( tradeAbs = -0.01, shortRel = 0, slippageRel = 0 ),
  latency = 0.1 # 100 milliseconds
)

#set strategy parameters
parameters = data.table(
  n         = 100,
  k         = 2,
  timeframe = 300,
  is_inverse = T
)

#Put your folder directory
directory = '...'
strategy_cpp_file = paste0(directory,"bbands.cpp")
Rcpp::sourceCpp( strategy_cpp_file )

# see how fast back testing done on over 2 millin ticks
system.time( { test = bbands( ticks, parameters, options ) } )
# summary of the strategy 
test$summary 

#Plot Result in 2019 year
interval = '2016/2019'
par( mfrow = c( 3,1 ), oma = c( 5, 4, 2, 4 ) + 0.1, mar = c( 0, 0, 1, 0 ), xaxt = 'n' )
# price
plot_dts( test$daily_performance[ date %bw% interval ,.( date, close) ] )$lines(lwd = 2)$style( legend = list( visible = F ), time = list( resolution = 'year' ) )$plot()
title('Stock Price', adj = 0, line = 0 )
# profit and loss
plot_dts(  test$daily_performance[ date %bw% interval ,.( date, pnl ) ] )$lines(lwd = 2,col = 'blue')$style( legend = list( visible = F ), time = list( resolution = 'year'))$plot()
title('P&L, %', adj = 0, line = 0 )
#drawdown
par( xaxt = 's' )
plot_dts(  test$daily_performance[ date %bw% interval ,.( date, drawdown ) ] )$lines(col = 'red',lwd = 2)$style( legend = list( visible = F ), time = list( resolution = 'year' ))$plot()
title('DrawDown, %', adj = 0, line = 0 )
title('BBands Strategy Performance', outer = T)

#Let's set parameters which we will use in strategy optimization
parameters = CJ( 
  n = seq(50,500,50),
  k = seq(.5,5,0.5),
  timeframe = c(5,10,30) * 60,
  is_inverse = c(T,F)
)

#Calculate daily performance for each parameter in order to work statistics out easily
daily_performance = parameters[, cbind( .SD, bbands( ticks, .SD, options )$daily_performance ), by = ( id = 1 : nrow( parameters ) )]

is_performance  = daily_performance[,.(pnl = sum(return[date < is_oos_split_date])), by = names(paramaters)]
oos_performance = daily_performance[,.(pnl = sum(return[!date < is_oos_split_date])), by = names(paramaters)]


#IS-OOS Optimization
is_oos_split_date = '2019-01-01'

par( mfrow = c( 1,1 ), oma = c( 2, 1, 2, 0 ))
multi_heatmap( is_performance,  pars = names( paramaters ), value = 'pnl', peak_value = 1)
title('PnL. In-Sample. Before 2019-01-01')
multi_heatmap( oos_performance,  pars = names( paramaters ), value = 'pnl', peak_value = 1)
title('PnL. Out-Of-Sample. After 2019-01-01')


is_sharpe_statistics = daily_performance[,.(sharpe = .SD[date < is_oos_split_date, mean( return ) / sd( return )]),by = id]
is_best_id = is_sharpe_statistics[, .SD[ which.max( sharpe ), id ] ]
is_oos_dp = daily_performance[ id %in% is_best_id ]

#Plot IS-OOS Result
interval = '2016/2020'
par( mfrow = c( 3,1 ), oma = c( 5, 4, 2, 4 ) + 0.1, mar = c( 0, 0, 1, 0 ), xaxt = 'n' )
# profit and loss
plot_dts( is_oos_dp[ date %bw% interval ,.( date, close) ] )$lines()$style( time = list( resolution = 'years' ) )$plot()
title('Close Price', adj = 0, line = 0 )
# profit and loss
plot_dts(  is_oos_dp[ date %bw% interval ,.( date, pnl = pnl * 100) ] )$lines(col = 'blue')$style( legend = list( visible = F ), time = list( resolution = 'years'))$plot()
title('P&L, %', adj = 0, line = 0 )
#drawdown
par( xaxt = 's' )
plot_dts(  is_oos_dp[ date %bw% interval ,.( date, drawdown = drawdown * 100) ] )$lines(col = 'red')$style( legend = list( visible = F ), time = list( resolution = 'years' ))$plot()
title('DrawDown, %', adj = 0, line = 0 )
title('IS/OOS Performance', outer = T)


#Walk Forward Test
statistics_cpp_file = paste0(directory,"roll_statistics_monthly.cpp")
Rcpp::sourceCpp( statistics_cpp_file )

#calculate rolling statistics monthly 
train_interval = 24
interval_statistics = daily_performance[, roll_statistics_monthly( .SD, train_interval ), by = id ]
#choose best parameter by Sharpe Ratio in IS data
selection = interval_statistics[ , .SD[ which.max( sharpe ) ], by = interval_date ]

daily_performance[,interval_date := as.Date( format(date, '%Y-%m-01') )]
wft_dp = merge( selection, daily_performance[,.(interval_date,id,date,return,close)], by = c( 'interval_date', 'id' ) )

#Plot WFT Result
interval = '2016/2020'
par( mfrow = c( 3,1 ), oma = c( 5, 4, 2, 4 ) + 0.1, mar = c( 0, 0, 1, 0 ), xaxt = 'n' )
# profit and loss
plot_dts( wft_dp[ date %bw% interval ,.( date, close) ] )$lines()$style( time = list( resolution = 'years' ) )$plot()
title('Close Price', adj = 0, line = 0 )
# profit and loss
plot_dts(  wft_dp[ date %bw% interval ,.( date, pnl = cumsum( return ) * 100 ) ] )$lines(col = 'blue')$style( legend = list( visible = F ), time = list( resolution = 'years'))$plot()
title('P&L, %', adj = 0, line = 0 )
#drawdown
par( xaxt = 's' )
plot_dts(  wft_dp[ date %bw% interval ,.( date, drawdown = 100*(cumsum( return ) - cummax( cumsum( return ) ) ) )] )$lines(col = 'red')$style( legend = list( visible = F ), time = list( resolution = 'years' ))$plot()
title('DrawDown, %', adj = 0, line = 0 )
title('WFT Performance', outer = T)


#Compare IS-OOS and WFT Results
par( mfrow = c( 1,1 ), oma = c( 5, 4, 2, 4 ) + 0.1, mar = c( 0, 0, 1, 0 ), xaxt = 's' , cex = 0.8)
plot_dts( wft_dp    [date >= is_oos_split_date,.(date, 'WFT, %' = cumsum( return ) * 100)],
          is_oos_dp [date >= is_oos_split_date,.(date, 'IS/OOS, %' = cumsum( return ) * 100)])$lines(lwd = 2)$style( legend = list( visible = T, position = 'bottomleft' ))$plot()

title('WFT vs IS/OOS performance', outer = T, cex = 0.6)










