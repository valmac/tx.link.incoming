
/******************************************************************************
 ******************************************************************************/

/** \file place_order.cpp
 *  place 1 market order example program
 *
 * $Id: place_order.cpp 260 2009-02-05 03:10:33Z grizz $
 */

//############################################################################//
// I N C L U D E S ###########################################################//
//############################################################################//

#include <zenfire/client.hpp>
#include <zenfire/debug.hpp>

#include <iostream>
#include <sstream>
#include <ctime>
#include <cstdlib>

namespace zc = zenfire::client;
using namespace zenfire::debug;

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
//! format timestamp of a price message

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

void all_report(const zenfire::report_t& m)
  {
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
//! order reports

void dump_report(const zenfire::report_t& m)
  {
  zenfire::order_ptr order = m.order;


  std::string msg = m.message();
  if(!msg.empty())
    msg = "(" + msg + ") ";

  std::cout << std::fixed << m.type() << " " << msg << *order <<
   std::endl;
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
  size_t sz;


  // connect to zenfire
  zenfire::client_t &zf = *zenfire::client::create("../cert");
  zf.login(user, pass, sys);

  // display list of accounts user has access to
  std::vector<std::string> acct = zf.list_accounts();
  if(acct.size() > 1)
    {
    int acctno = -1;


    while(acctno == -1)
      {
      std::cout << "possible accounts: " << acct.size() << std::endl;
      for(sz=0; sz<acct.size(); sz++)
        std::cout << sz << ": " << acct[sz] << std::endl;
      std::cout << "please select an account number: ";

      std::cin >> acctno;
      std::cin.clear();
      std::cin.ignore(255,'\n');

      if(acctno < 0 || (unsigned int)acctno >= acct.size())
        acctno = -1;
      }

    zf.set_account(acctno);
    }
  else
    std::cout << "Using account " << acct[0] << std::endl;
    

  // read a product
  zenfire::arg::product prod;
  std::cout << "Enter symbol.exchange: ";
  std::cin >> prod;

  // look up product
  zenfire::product_t sym = zf.lookup_product(prod);
  std::cout << sym.symbol << "(tick " << sym.increment << "): OK" << std::endl;

  // get action
  zenfire::order::action_t action(zenfire::order::NO_ACTION);

  while(action == zenfire::order::NO_ACTION)
    {
    int act;


    std::cout << "possible actions:" << std::endl;
    std::cout << zenfire::order::BUY << ": Buy" << std::endl;
    std::cout << zenfire::order::SELL << ": Sell" << std::endl;
    std::cout << "please select an action to place market order: ";

    std::cin >> act;
    std::cin.clear();
    std::cin.ignore(255,'\n');

    switch(act)
      {
    case zenfire::order::BUY:
      action = zenfire::order::BUY;
      break;
    case zenfire::order::SELL:
      action = zenfire::order::SELL;
      break;
      }
    }

  // subscribe to the default account
  zf.hook_reports(dump_report);
  zf.subscribe_account();

  // create a market order
  zenfire::arg::market order(sym, action, 1, zenfire::order::DAY);
  zf.place_order(order);

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

