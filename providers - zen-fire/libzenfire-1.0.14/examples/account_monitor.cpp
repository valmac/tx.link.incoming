
/******************************************************************************
 ******************************************************************************/

/** \file account_monitor.cpp
 *  example program to monitor positions and P/L for all user accounts
 *
 * $Id: account_monitor.cpp 3332 2011-06-02 16:11:06Z grizz $
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

using namespace zenfire::debug;

//############################################################################//
// G L O B A L S #############################################################//
//############################################################################//

std::vector<std::string> acctlist;

//############################################################################//
// F U N C T I O N S #########################################################//
//############################################################################//

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
//! dump position

void dump_pos(const zenfire::position_t& m)
  {
  std::cout << "[" << acctlist[m.acctno] << "] " <<
   m.product << ": " << m.size << " (fill " << m.avg_fill_price() << " " <<
   m.product << ": open P&L $" << m.open_pl <<
   " | closed P&L $" << m.closed_pl <<
   std::endl;
  }

//############################################################################//
//! dump account level profit/loss

void dump_acct(const zenfire::profitloss_t& m)
  {
  std::cout << std::fixed << "[" << acctlist[m.acctno] << "] " <<
   "open P&L $" << m.open << " | closed P&L $" << m.closed <<
   " | balance $" << m.balance <<
   " | margin $" << m.margin <<
   std::endl;
  }

//############################################################################//
//! order reports

void dump_report(const zenfire::report_t& m)
  {
  zenfire::order_ptr order = m.order;


  std::cout << std::fixed << "[" << acctlist[order->acctno()] << "] " <<
   order->product() << ": #" << order->number() <<
   " | status " << order->status() << ":" << order->reason() <<
   order->message() <<
   std::endl;
  }

//############################################################################//
// M A I N ###################################################################//
//############################################################################//

int main(int argc, char **argv)
  {
  size_t idx;


  if(argc != 4)
    die_usage();

  // get auth info from the command line
  std::string user(argv[1]);
  std::string pass(argv[2]);
  std::string sys(argv[3]);

try 
  {
  // connect to zen-fire
  zenfire::client_t &zf = *zenfire::client::create("../cert");
  zf.login(user, pass, sys);

  // set up callbacks
  zenfire::account::callback_t acctcb;
  acctcb.position = dump_pos;
  acctcb.profitloss = dump_acct;
  zf.hook(acctcb);
  zf.hook_reports(dump_report);

  // load list of accounts to global var
  acctlist = zf.list_accounts();

  // subscribe to each account
  std::cout << "watching accounts: ";
  for(idx=0; idx<acctlist.size(); idx++)
    {
    zf.subscribe_account(idx);
    zf.request_orders(0, 0, idx);

    std::cout << acctlist[idx] << " ";
    }
  std::cout << std::endl;

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

