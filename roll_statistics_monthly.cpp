// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(QuantTools)]]
#include "BackTest.h"
#include <queue>
#include <vector>
#include <time.h>

int  getNumberOfDays(int month, int year)
{
  //leap year condition, if month is 2
  if( month == 2 )
  {
    if( ( year % 400 == 0 ) || ( year % 4 == 0 && year % 100 != 0 ) )
      return 29;
    else
      return 28;
  }
  //months which has 31 days
  else if( month == 1 || month == 3 || month == 5 || month == 7 || month == 8 || month == 10 || month==12 )
    return 31;
  else
    return 30;
}

// [[Rcpp::export]]
Rcpp::List roll_statistics_monthly( Rcpp::List daily_performance, std::size_t n ) {
  
  
  Rcpp::IntegerVector onDayCloseHistoryDates             = daily_performance[ "date"     ];
  Rcpp::DoubleVector  onDayCloseHistoryMarketValueChange = daily_performance[ "return"   ];

  struct day_summary {
    
    int    date;
    int    month;
    double marketValue;
    double marketValueChange;
    int    id;
    
  };
  
  std::deque< day_summary > window;

  int    nDaysTested = 0;
  double rSquared    = 0;
  double sharpe      = 0;
  double sortino     = 0;
  double pnl         = 0;
  double marketValue = 0;
  
  std::vector<int>    onMonthCloseHistoryDates;
  std::vector<int>    onMonthCloseHistoryNDaysTested;
  std::vector<double> onMonthCloseHistoryRSquared;
  std::vector<double> onMonthCloseHistorySharpe;
  std::vector<double> onMonthCloseHistoryPnl;
  std::vector<double> onMonthCloseHistoryMaxDrawdown;
  std::vector<double> onMonthCloseHistoryDrawdown;
  
  double sumV  = 0;
  double sumVV = 0;
  double sumNV = 0;
  int    sumN  = 0;
  double sumR  = 0;
  double sumRR = 0;

  const int secondsInDay = 24 * 60 * 60;
  const int nTradingDaysInYear = 252;
  
  int nRows = onDayCloseHistoryDates.size();
  
  if( nRows == 0 ) return R_NilValue;
  
  for( int i = 0; i < nRows; i++ ) {
    

    //what month is it today and tomorrow.
    time_t t( onDayCloseHistoryDates[i] * secondsInDay );
    struct tm * ptm;
    ptm = gmtime(&t);
    int currentMonth  = ptm->tm_year * 12 + ptm->tm_mon;
    int nextMonth = currentMonth + 1;
    
    
    int nm = ( ptm -> tm_mon + 1 ) % 12 + 1;
    time_t t1( (onDayCloseHistoryDates[i] + getNumberOfDays( nm, ptm -> tm_year + 1900 )  ) * secondsInDay );
    ptm = gmtime( &t1 );
    
    time_t tmonth = t1 - ( ptm -> tm_mday - 1 ) * secondsInDay;
    
    Rcpp::Date dateNextMonth( ( int ) tmonth / secondsInDay );
    
    time_t ttomorrow( onDayCloseHistoryDates[i+1] * secondsInDay );
    if( i == nRows - 1 )   time_t ttomorrow( ( onDayCloseHistoryDates[i] + 27 ) * secondsInDay );
    ptm = gmtime( &ttomorrow );
    int tomorrowMonth = ptm -> tm_year * 12 + ptm -> tm_mon;
    
    
    
    // remove from window until empty or front is old
    if( not window.empty() ) while ( nextMonth - window.front().month > n ) {
      
      auto front = window.front();
      window.pop_front();
      
      nDaysTested--;
      
      pnl   -= front.marketValueChange;
      sumV  -= front.marketValue;
      sumVV -= front.marketValue * front.marketValue;
      sumNV -= front.marketValue * front.id;
      sumN  -= front.id;
      sumR  -= front.marketValueChange;
      sumRR -= front.marketValueChange * front.marketValueChange;
      
      if( window.empty() ) break;
      
    }
    
    marketValue += onDayCloseHistoryMarketValueChange[i];
    
    // add to window
    day_summary back;
    back.marketValue       = marketValue  ;
    back.month             = currentMonth ;
    back.date              = onDayCloseHistoryDates            [i];
    back.marketValueChange = onDayCloseHistoryMarketValueChange[i];
    back.id                = i;
    
    window.push_back( back );
    
    nDaysTested++;
    
    pnl   += back.marketValueChange;
    sumV  += back.marketValue;
    sumVV += back.marketValue * back.marketValue;
    sumNV += back.marketValue * back.id;
    sumN  += back.id;
    sumR  += back.marketValueChange;
    sumRR += back.marketValueChange * back.marketValueChange;
    
    double covNV = nDaysTested * sumNV - sumV * sumN;                                      
    double sdN   = nDaysTested * std::sqrt( ( nDaysTested * nDaysTested - 1 ) * 1. / 12 ); 
    double sdV   = std::sqrt( nDaysTested * sumVV - sumV * sumV );                         
    double r     = covNV / sdN / sdV;
    
    rSquared = r * r;
    
    double avgR = sumR / nDaysTested;
    double varR = ( nDaysTested * sumRR - sumR * sumR ) / nDaysTested / ( nDaysTested - 1 );
    
    sharpe = avgR / std::sqrt( varR ) * std::sqrt( nTradingDaysInYear );

    if( i == nRows - 1 or tomorrowMonth == nextMonth ) {
      
      double cumMaxPnl   = 0;
      double curPnl      = 0;
      double maxDrawdown = 0;
      double drawdown    = 0;
      double avgDrawDown = 0;
      for( auto daySummary : window ) {
        
        curPnl += daySummary.marketValueChange;
        if( curPnl > cumMaxPnl ) cumMaxPnl = curPnl;
        drawdown = curPnl - cumMaxPnl;
        
        avgDrawDown += drawdown / nDaysTested;
        
        if( drawdown < maxDrawdown ) maxDrawdown = drawdown;
        
      }
      
      onMonthCloseHistoryMaxDrawdown  .push_back( maxDrawdown           );
      onMonthCloseHistoryDrawdown     .push_back( drawdown              );
      onMonthCloseHistoryDates        .push_back( dateNextMonth         );
      onMonthCloseHistoryNDaysTested  .push_back( nDaysTested           );
      onMonthCloseHistoryPnl          .push_back( pnl                   );
      onMonthCloseHistoryRSquared     .push_back( rSquared              );
      onMonthCloseHistorySharpe       .push_back( sharpe                );
    }
    
  }
  
  if( onMonthCloseHistoryDates.empty() ) return R_NilValue;
  
  Rcpp::List statistics = ListBuilder().AsDataTable()
  .Add( "interval_date"  , IntToDate( onMonthCloseHistoryDates ) )
  .Add( "pnl"            , onMonthCloseHistoryPnl                )
  .Add( "days_tested"    , onMonthCloseHistoryNDaysTested        )
  .Add( "r_squared"      , onMonthCloseHistoryRSquared           )
  .Add( "sharpe"         , onMonthCloseHistorySharpe             )
  .Add( "max_dd"         , onMonthCloseHistoryMaxDrawdown        )
  .Add( "drawdown"       , onMonthCloseHistoryDrawdown           );
  
  return statistics;
  
}

