
/******************************************************************************
 ******************************************************************************/

/** \file market_data.cpp
 *  market data example program
 *
 * $Id: market_data.cpp 3299 2010-10-05 18:39:06Z grizz $
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
  is >> s;

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
//! receive tick data

void all_ticks(const zenfire::tick_t& m)
  {
  std::cout << fmtts(m) << " [" << "" << m.type() << "] " <<
   m.product->symbol << "." << m.product->exchange << " " << m.size << " @ " << m.price
   << std::endl;
  }

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
  zf.login(user, pass, sys);

  // read a product
  zenfire::arg::product prod;
  std::cout << "Enter symbol.exchange: ";
  std::cin >> prod;

  // look up product
  zenfire::product_t sym = zf.lookup_product(prod);
  std::cout << sym.symbol << "(tick " << sym.increment << ", precision " << sym.precision << ", point value " << sym.point_value << "): OK" << std::endl;

  zf.hook_ticks(all_ticks);
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



