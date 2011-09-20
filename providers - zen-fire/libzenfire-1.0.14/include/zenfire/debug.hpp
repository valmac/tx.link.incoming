
/******************************************************************************
 ******************************************************************************/

/** \file debug.hpp
 *  \brief helper debug functions for printing objects
 *
 * $Id: debug.hpp 2804 2010-04-30 03:36:35Z grizz $
 */

#ifndef ZENFIRE_DEBUG_HPP__
#define ZENFIRE_DEBUG_HPP__

//############################################################################//
// L I C E N S E #############################################################//
//############################################################################//

/*
 * (C) 2008 BigWells Technology (Zen-Fire). All rights reserved. 
 * Confidential and proprietary information of BigWells Technology.
 */

//############################################################################//
// I N C L U D E S ###########################################################//
//############################################################################//

#include <zenfire/zenfire.hpp>

#include <iostream>
#include <sstream>
#include <ctime>

//############################################################################//

#if defined(_MSC_VER)
#  define LOCALTIME_R(tp, tm) localtime_s((tm), (tp))
#else
#  define LOCALTIME_R(tp, tm) localtime_r((tp), (tm))
#endif

namespace zenfire {
namespace debug {

//############################################################################//
//! dump tick to stream

template <typename T>
T& dump(T& s, const tick_t& m)
  {
  s << *m.product << " " << m.ts << "." << m.usec / 1000 <<
   " " << m.size << " @ " << m.price << " (" << m.type() << ")";
  return(s);
  } 

template <typename T>
T& operator<<(T& s, const tick_t& m)
  {
  return(dump(s, m));
  }

//############################################################################//
//! dump tick type to stream

inline
std::string
tick_type_to_string(const tick::type_t& t)
  {
  switch(t)
    {
  case tick::ASK:
    return("ASK");
  case tick::BEST_ASK:
    return("BEST_ASK");
  case tick::BID:
    return("BID");
  case tick::BEST_BID:
    return("BEST_BID");
  case tick::TRADE:
    return("TRADE");
  case tick::LOW:
    return("LOW");
  case tick::HIGH:
    return("HIGH");
  case tick::OPEN:
    return("OPEN");
  case tick::CLOSE:
    return("CLOSE");
  case tick::VOLUME:
    return("VOLUME");
  case tick::MODE:
    return("mode");
  default:
    return("");
    }
  } 

template <typename T>
T& operator<<(T& s, const tick::type_t& m)
  {
  s << tick_type_to_string(m);
  return(s);
  }

//############################################################################//
//! order status

template <typename T>
T& operator<< (T& s, const zenfire::order::status_t& status)
  {
  switch(status)
    {
  case PREPARED:
    s << "prepared";
    break;
  case SENT:
    s << "sent";
    break;
  case PENDING:
    s << "pending";
    break;
  case OPEN:
    s << "open";
    break;
  case UPDATING:
    s << "updating";
    break;
  case COMPLETE:
    s << "complete";
    break;
  case NO_STATUS:
    s << "none";
    break;
    }

  return(s);
  }

//############################################################################//
//! order completion reason

template <typename T>
T& operator<< (T& s, const zenfire::order::reason_t& reason)
  {
  switch(reason)
    {
  case FILLED:
    s << "filled";
    break;
  case PARTIAL:
    s << "partial";
    break;
  case CANCELED:
    s << "canceled";
    break;
  case FAILED:
    s << "failed";
    break;
  case REJECTED:
    s << "rejected";
    break;
  case BUSTED:
    s << "busted";
    break;
  case NO_REASON:
    s << "none";
    break;
    }

  return(s);
  }

//############################################################################//
//! order type

template <typename T>
T& operator<< (T& s, const zenfire::order::type_t& typ)
  {
  switch(typ)
    {
  case MARKET:
    s << "market";
    break;
  case LIMIT:
    s << "limit";
    break;
  case STOP_MARKET:
    s << "stop market";
    break;
  case STOP_LIMIT:
    s << "stop limit";
    break;
  case NO_TYPE:
    s << "none";
    break;
    }

  return(s);
  }

//############################################################################//
//! order action

template <typename T>
T& operator<< (T& s, const zenfire::order::action_t& action)
  {
  switch(action)
    {
  case BUY:
    s << "buy";
    break;
  case SELL:
    s << "sell";
    break;
  case NO_ACTION:
    s << "unknown";
    break;
    }

  return(s);
  }

//############################################################################//
//! order duration

template <typename T>
T& operator<< (T& s, const zenfire::order::duration_t& duration)
  {
  switch(duration)
    {
  case DAY:
    s << "day";
    break;
  case FOK:
    s << "fok";
    break;
  case GTC:
    s << "gtc";
    break;
  case NO_DURATION:
    s << "unknown";
    break;
    }

  return(s);
  }

//############################################################################//
//! 

inline
std::string
mode_to_string(const zenfire::mode::mode_t typ)
  {
  std::string rv;

  switch(typ)
    {
  case mode::OPEN:
    return("open");
  case mode::CLOSED:
    return("closed");
  case mode::PRE_OPEN:
    return("closed:pre-open");
  case mode::HALTED:
    return("closed:halted");
    }

  return(rv);
  }

//############################################################################//
//! dump order
//T& operator<< (T& s, const zenfire::order_t& o)

template <typename T>
T& operator<< (T& s, zenfire::order_t& o)
  {
  std::stringstream str;


  switch(o.type())
    {
  case order::LIMIT:
    str << "@ " << o.price() << " "; 
    break;
  case order::STOP_LIMIT:
    str << "@ " << o.price() << " (trigger @ " << o.trigger() << ") " ; 
    break;
  case order::STOP_MARKET:
    str << "(trigger @ " << o.trigger() << ") " ; 
    break;
  default:
    break;
    }

  if(o.qty_filled())
    str << "(filled @ " << o.fill_price() << ") ";

  s << o.type() << " order " << o.number() << " " <<
   o.product() << " " << o.action() << " " << o.qty() << " (" <<
   o.qty_open() << " / " << o.qty_filled() << " / " << o.qty_canceled() <<
   ") " << str.str() << o.duration() << " " << o.status() << ":" <<
   o.reason() << " " << o.message();

//<< "trig" << o.trigger();
  return(s);
  }

//############################################################################//
//! order report type

template <typename T>
T& operator<< (T& s, const zenfire::report::type_t& typ)
  {
  switch(typ)
    {
  case report::BUST:
    s << "BUST";
    break;
  case report::CANCEL:
    s << "CANCEL";
    break;
  case report::FAIL:
    s << "FAIL";
    break;
  case report::FILL:
    s << "FILL";
    break;
  case report::MODIFY:
    s << "MODIFY";
    break;
  case report::CANCEL_FAIL:
    s << "CANCEL_FAIL";
    break;
  case report::UPDATE_FAIL:
    s << "UPDATE_FAIL";
    break;
  case report::OPEN:
    s << "OPEN";
    break;
  case report::REJECT:
    s << "REJECT";
    break;
  case report::TRIGGER:
    s << "TRIGGER";
    break;
  case report::REPLAY:
    s << "REPLAY";
    break;
  case report::END_OF_REPLAY:
    s << "END_OF_REPLAY";
    break;
  case report::UNKNOWN:
    s << "UNKNOWN";
    break;
    }

  return(s);
  }

//############################################################################//
//! dump product position to stream

template <typename T>
T& operator<< (T& s, const zenfire::position_t& m)
  {
  s << "[" << m.acctno << "] " << m.product << ": " << m.size << " " <<
   ": open P&L $" << m.open_pl << " | closed P&L $" << m.closed_pl << " fill at " << m.avg_fill_price();
  return(s);
  }

//############################################################################//
//! dump account level profit/loss to stream
   
template <typename T>
T& operator<< (T& s, const zenfire::profitloss_t& m)
  {
  s << "[" << m.acctno << "] " <<
   "open P&L $" << m.open << " | closed P&L $" << m.closed <<
   " | balance $" << m.balance <<
   " | margin $" << m.margin;
  return(s);
  }

//############################################################################//
//! dump product to stream

template <typename T>
T& operator<< (T& s, const zenfire::product_t& m)
  {
  s << m.symbol << "." << exchange::to_string(m.exchange);
  return(s);
  }

}} // namespace zenfire::debug
//############################################################################//

#endif // end ZENFIRE_DEBUG_HPP__

/******************************************************************************
 ******************************************************************************/

