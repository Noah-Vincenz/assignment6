// Advanvced Part 3 about really dump investing strategy
//=======================================================

//two test portfolios

val blchip_portfolio = List("GOOG", "AAPL", "MSFT", "IBM", "FB", "YHOO", "AMZN", "BIDU")
val rstate_portfolio = List("PLD", "PSA", "AMT", "AIV", "AVB", "BXP", "CBG", "CCI",
  "DLR", "EQIX", "EQR", "ESS", "EXR", "FRT", "GGP", "HCP")

// (1) The function below should obtain the first trading price
// for a stock symbol by using the query
//
//    http://ichart.yahoo.com/table.csv?s=<<symbol>>&a=0&b=1&c=<<year>>&d=1&e=1&f=<<year>> 
// 
// and extracting the first January Adjusted Close price in a year.

def get_first_price(symbol: String, year: Int): Option[Double] = {
  import io.Source
  try {
    val arr = Source.fromURL("http://ichart.yahoo.com/table.csv?s=" + symbol + "&a=0&b=1&c=" + year + "&d=1&e=1&f=" + year).mkString.split(",")
    Some(arr(arr.size-1).toDouble)
  } catch {
    case e: Exception => None
  }
}

// Complete the function below that obtains all first prices
// for the stock symbols from a portfolio for the given
// range of years

def get_prices(portfolio: List[String], years: Range): List[List[Option[Double]]] = {
  var listToReturn = List[List[Option[Double]]]()
  for (year <- years) {
    var list = List[Option[Double]]()
    for (symbol <- 0 until portfolio.size) {
      list = list :+ get_first_price(portfolio(symbol), year)
    }
    listToReturn = listToReturn :+ list
  }
  listToReturn
}

// test case
//val p = get_prices(List("GOOG", "AAPL"), 2010 to 2012)


// (2) The first function below calculates the change factor (delta) between
// a price in year n and a price in year n+1. The second function calculates
// all change factors for all prices (from a portfolio).

def get_delta(price_old: Option[Double], price_new: Option[Double]): Option[Double] = {
  try {
    Some((price_new.get-price_old.get) / price_old.get)
  } catch {
    case e: Exception => None
  }
}

def get_deltas(data: List[List[Option[Double]]]):  List[List[Option[Double]]] = {
  var listToReturn = List[List[Option[Double]]]()
  for (numberOfYears <- 0 until data.size-1) {
    var listToAdd = List[Option[Double]]()
    for (numberOfSymbols <- 0 until data(0).size) {
      listToAdd = listToAdd :+ get_delta(data(numberOfYears)(numberOfSymbols), data(numberOfYears+1)(numberOfSymbols))
    }
    listToReturn = listToReturn :+ listToAdd
  }
  listToReturn
}

// test case using the prices calculated above
//val d = get_deltas(p)


// (3) Write a function that given change factors, a starting balance and a year
// calculates the yearly yield, i.e. new balanace, according to our dump investment
// strategy. Another function calculates given the same data calculates the
// compound yield up to a given year. Finally a function combines all
// calculations by taking a portfolio, a range of years and a start balance
// as arguments.

def yearly_yield(data: List[List[Option[Double]]], balance: Long, year: Int): Long = {
  (for (n <- 0 until data(year).size) yield balance + (data(year)(n).getOrElse(0.0) * (balance / data(year).size)).toLong).sum[Long]
}

//test case
//yearly_yield(d, 100, 0)

def compound_yield(data: List[List[Option[Double]]], balance: Long, year: Int): Long = {
  var tempBalance: Long = balance;
  for (n <- 0 until year) {
    tempBalance = yearly_yield(data, tempBalance, n)
  }
  tempBalance
}

def investment(portfolio: List[String], years: Range, start_balance: Long): Long = {
  compound_yield(get_deltas(get_prices(portfolio, years)), start_balance, years.length-1)
}


//test cases for the two portfolios given above
//investment(rstate_portfolio, 1978 to 2016, 100)
//investment(blchip_portfolio, 1978 to 2016, 100)

