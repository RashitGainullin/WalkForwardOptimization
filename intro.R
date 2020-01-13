library( QuantTools )

strategy_cpp_file = "MEGA/Habr/Article WFT/bbands.cpp"
Rcpp::sourceCpp( strategy_cpp_file )

#Let's store market data for GAZP and SBER tickers (used by default) locally
store_finam_data(to = Sys.Date(),verbose = T)


candles = get_finam_data('GAZP', '2016-01-01', to = '2020-01-01', period = "1min", local = T)[ format(time,"%H:%M") %bw% c("10:00","18:40")]
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

# see how fast back testing done on over 2 millin ticks
system.time( { test = bbands( ticks, parameters, options ) } )

#Plot Result
interval = '2018/2020'
par( mfrow = c( 3,1 ), oma = c( 5, 4, 2, 4 ) + 0.1, mar = c( 0, 0, 1, 0 ), xaxt = 'n' )
# price
plot_dts( test$indicators[ time %bw% interval,.( time, open, high, low, close) ] )$style( legend = list( visible = F), time = list( resolution = 'years' ))$plot()
title('Stock Price', adj = 0, line = 0 )
# profit and loss
plot_dts(  test$indicators[ time %bw% interval,.( time, pnl ) ] )$style( legend = list( visible = F ), time = list( resolution = 'years'))$plot()
title('P&L, %', adj = 0, line = 0 )
#drawdown
par( xaxt = 's' )
plot_dts(  test$indicators[ time %bw% interval,.( time, drawdown ) ] )$lines(col = 'red')$style( legend = list( visible = F ), time = list( resolution = 'years' ))$plot()
title('DrawDown, %', adj = 0, line = 0 )
title('Strategy Performance', outer = T)

#Let's set set of parameters which we will use in strategy optimization
parameters = CJ( 
  n = seq(50,500,50),
  k = seq(.5,5,0.5),
  timeframe = c(5,10,30) * 60,
  is_inverse = c(T,F)
)

#Calculate Performance and visualize results on the heatmap
summary_statistics = parameters[, cbind( .SD, bbands( ticks, .SD, options )$summary ), by = ( id = 1 : nrow( parameters ) )]
summary_statistics[order(-sharpe)]
multi_heatmap( summary_statistics,  pars = names( paramaters ), value = 'pnl')

#Calculate daily performance for each parameter in order to work statistics out easily
daily_performance = parameters[, cbind( .SD, bbands( ticks, .SD, options )$daily_performance ), by = ( id = 1 : nrow( parameters ) )]

#IS-OOS Optimization
is_oos_split_date = '2019-01-01'
is_sharpe_statistics = daily_performance[,.(sharpe = .SD[date < is_oos_split_date, mean( return ) / sd( return )]),by = id]
is_best_id = is_sharpe_statistics[, .SD[ which.max( sharpe ), id ] ]
is_oos_dp = daily_performance[ id %in% is_best_id ]

#Plot Result
interval = '2016/2020'
par( mfrow = c( 2,1 ), oma = c( 5, 4, 2, 4 ) + 0.1, mar = c( 0, 0, 1, 0 ), xaxt = 'n' )
# profit and loss
plot_dts(  is_oos_dp[ date %bw% interval ,.( date, pnl ) ] )$style( legend = list( visible = F ), time = list( resolution = 'years'))$plot()
title('P&L, %', adj = 0, line = 0 )
#drawdown
par( xaxt = 's' )
plot_dts(  is_oos_dp[ date %bw% interval ,.( date, drawdown ) ] )$lines(col = 'red')$style( legend = list( visible = F ), time = list( resolution = 'years' ))$plot()
title('DrawDown, %', adj = 0, line = 0 )
title('IS/OOS Performance', outer = T)


#WFT
statistics_cpp_file = "MEGA/Habr/Article WFT/roll_statistics_monthly.cpp"
Rcpp::sourceCpp( statistics_cpp_file )

#calculate roll_statistics
train_interval = 12
interval_statistics = daily_performance[, roll_statistics_monthly( .SD, train_interval ), by = id ]
#choose best parameter by sharpe in IS data
selection = interval_statistics[ , .SD[ which.max( sharpe ) ], by = interval_date ]

daily_performance[,interval_date := as.Date( format(date, '%Y-%m-01') )]
wft_dp = merge( selection, daily_performance[,.(interval_date,id,date,return)], by = c( 'interval_date', 'id' ) )

plot_dts( wft_dp[date >= is_oos_split_date,.(date, pnl_wft = cumsum( return ))],
          is_oos_dp[date >= is_oos_split_date,.(date, pnl_is_oos = cumsum( return ))])










