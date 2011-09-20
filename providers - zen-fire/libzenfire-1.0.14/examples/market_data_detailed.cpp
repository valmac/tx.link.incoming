
/******************************************************************************
 ******************************************************************************/

/** \file market_data_detailed.cpp
 *  market data example program
 *
 * $Id: market_data_detailed.cpp 3299 2010-10-05 18:39:06Z grizz $
 */

//############################################################################//
// I N C L U D E S ###########################################################//
//############################################################################//

#include <zenfire/client.hpp>

#include <iostream>
#include <sstream>
#include <ctime>
#include <cstdlib>

#if defined(_MSC_VER)
#  define LOCALTIME_R(tp, tm) localtime_s((tm), (tp))
#else
#  define LOCALTIME_R(tp, tm) localtime_r((tp), (tm))
#endif

#include <zenfire/debug.hpp>
using namespace zenfire::debug;

//############################################################################//
// F U N C T I O N S #########################################################//
//############################################################################//

//############################################################################//
//! read istream into a product_t

std::istream& operator>>(std::istream& is, zenfire::arg::product& prod)
  {
  if(!is.good())
    return(is);

  std::string s;
  std::getline(is, s);

  // split on .
  size_t idx = s.find_first_of('.');
  prod.symbol = s.substr(0, idx);
  prod.exchange = zenfire::exchange::from_string(s.substr(idx+1));

  std::cout << "got " << prod.symbol << " on " << prod.exchange << std::endl;
  return is;
}

//############################################################################//
//! format timestamp of a tick message

std::string fmtts(const zenfire::tick_t& t)
  {
  size_t sz;
  char tsbuf[9];
  std::stringstream str;
  struct tm ltm;


  LOCALTIME_R(&t.ts, &ltm);
  sz = strftime(tsbuf, sizeof(tsbuf), "%H:%M:%S", &ltm);
  str << std::string(tsbuf, sz) << ".";
  str.width(6);
  str.fill('0');
  str << t.usec;
  return(str.str());
  }

//############################################################################//
//! display a tick

void print_tick(const zenfire::tick_t& m, const std::string& typ)
  {
  std::cout << fmtts(m) << " [" << typ << "] " << *m.product <<
   " " << m.size << " @ " << m.price << " " << std::endl;
  }

//############################################################################//
//! print mode

void print_mode(const zenfire::tick_t& m)
  {
  std::cout << fmtts(m) << " [MODE] " << *m.product << " " <<
   mode_to_string(m.flags) << std::endl;
  }

//############################################################################//
//! display usage and exit

void die_usage()
  {
  std::cout << "usage : user password [Sim|Live]" << std::endl;
  std::exit(0);
  }

//############################################################################//
// C A L L B A C K S #########################################################//
//############################################################################//

//############################################################################//
//! display a tick

void recv_alert(const zenfire::alert_t& m)
  {
  switch(m.type())
    {
  case zenfire::alert::UP:
    std::cout << "Connection [UP]" << std::endl;
    break;
  case zenfire::alert::DOWN:
    std::cout << "Connection [DOWN]" << std::endl;
    break;
  case zenfire::alert::DISABLED:
    std::cout << "Connection [DISABLED]" << std::endl;
    break;
    }
  }

//############################################################################//
//! receive ask tick

void recv_ask(const zenfire::tick_t& m)
  {
  print_tick(m, "ASK");
  }

//############################################################################//
//! best bid offer history class

class bbo_history_t
  {
public:

  //##########################################################################//
  //! receive best ask tick

  void
  ask_tick(const zenfire::tick::best_ask_t& t)
    {
    // print tick
    print_tick(t, "BEST ASK");

    // save in history
    ask.push_back(t.price);
    }

  //##########################################################################//
  //! receive best bid tick

  void
  bid_tick(const zenfire::tick::best_bid_t& t)
    {
    // print tick
    print_tick(t, "BEST BID");

    // save in history
    bid.push_back(t.price);
    }

private:
  std::vector<double> ask, bid;
  };

//############################################################################//
//! trade print history class

class trade_history_t
  {
public:

  //##########################################################################//
  //! receive trade tick

  void
  operator()(const zenfire::tick::trade_t& t)
    {
    // output tick
    print_tick(t, "TRADE");

    // save in history
    hist.push_back(t.price);
    }

private:
  std::vector<double> hist;
  };

//############################################################################//
// M A I N ###################################################################//
//############################################################################//

int main(int argc, char **argv)
  {
  if(argc != 4)
    die_usage();

  // get auth info from the command line
  std::string user(argv[1]);
  std::string pass(argv[2]);
  std::string sys(argv[3]);

try
  {
  // connect to zenfire
  zenfire::client_t &zf = *zenfire::client::create("../cert");
  zf.hook_alerts(recv_alert);
  zf.login(user, pass, sys);

  // read a product
  zenfire::arg::product prod;
  std::cout << "Enter symbol.exchange: ";
  std::cin >> prod;

  // look up product
  zenfire::product_t sym = zf.lookup_product(prod);
  std::cout << sym.symbol << " (" << sym.description << " tick "
   << sym.increment << " : " << sym.point_value << sym.currency
   << "/pt): OK" << std::endl;

  // set up callbacks
  zenfire::tick::callback_t tick_callback;

  // callback functions can be set to existing functions matching the prototype
  tick_callback.ask = recv_ask;

  // callback functions can be set using tr1's functional bind
  tick_callback.bid = std::tr1::bind(print_tick, std::tr1::placeholders::_1,
   "BID");
  tick_callback.low = std::tr1::bind(print_tick, std::tr1::placeholders::_1,
   "LOW");
  tick_callback.high = std::tr1::bind(print_tick, std::tr1::placeholders::_1,
   "HIGH");
  tick_callback.open = std::tr1::bind(print_tick, std::tr1::placeholders::_1,
   "OPEN");
  tick_callback.close = std::tr1::bind(print_tick, std::tr1::placeholders::_1,
   "CLOSE");
  tick_callback.volume = std::tr1::bind(print_tick, std::tr1::placeholders::_1,
   "VOLUME");
  tick_callback.mode = std::tr1::bind(print_mode, std::tr1::placeholders::_1);

  // callback functions can be set to use member functions on specific objects
  bbo_history_t bbo_hist;
  tick_callback.best_ask = std::tr1::bind(&bbo_history_t::ask_tick, &bbo_hist,
   std::tr1::placeholders::_1);
  tick_callback.best_bid = std::tr1::bind(&bbo_history_t::bid_tick, &bbo_hist,
   std::tr1::placeholders::_1);

  // callback functions can set to objects with operator() set to receive tick
  trade_history_t trade_hist;
  tick_callback.trade = trade_hist;

  // set callbacks and subscribe
  zf.hook(tick_callback);
  zf.subscribe(sym);

  // wait for "quit" to exit
  std::string cmd;
  while(cmd != "quit")
    {
    std::cout << "Type quit to exit" << std::endl;
    std::cin >> cmd;
    }

  delete &zf;
  }
catch(std::exception& e)
  {
  std::cerr << "exception " << e.what() << ", exiting" << std::endl;
  return(1);
  }

  return(0);
  }

/******************************************************************************
 ******************************************************************************/

