
/******************************************************************************
 ******************************************************************************/

/** \file arg.hpp
 *  \brief helper argument data structures
 *
 * $Id: arg.hpp 3332 2011-06-02 16:11:06Z grizz $
 */

#ifndef ZENFIRE_ARG_HPP__
#define ZENFIRE_ARG_HPP__

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

#include <zenfire/exchange.hpp>
#include <zenfire/product.hpp>

#include <string>

namespace zenfire {
namespace order {

class market_t;
class limit_t;
template<typename T> class stop_t;

//! order action types
enum action_t
  {
  //! Not set
  NO_ACTION = 0,
  //! Buy
  BUY,
  //! Sell
  SELL,
  };

//! order duration types
enum duration_t
  {
  //! Not set
  NO_DURATION = 0,
  //! Fill or Kill
  FOK,
  //! Day
  DAY,
  //! Good Til Canceled
  GTC,
  };

} // namespace order
//############################################################################//

namespace arg {

//############################################################################//
// product

struct product
  {
  std::string symbol;
  exchange_t exchange;

  product();
  product
    (
    const std::string& symbol,
    exchange_t exchange
    );
  product
    (
    const std::string& symbol,
    const std::string& exchange
    );

  bool
  operator<(const product &rhs) const
    {
    return(symbol < rhs.symbol);
    }
  };

//############################################################################//
// orders

using zenfire::order::action_t;
using zenfire::order::duration_t;

//############################################################################//
//! abstract base class for order arguments

struct order
  {
  //! fully qualified product containing all product information and exchange
  product_t product;

  //! order action
  action_t action;

  //! quantity
  int qty;

  //! order duration
  duration_t duration;

  //! user defined context to be included on order callbacks
  //! - specific to current connection
  void *context;

  //! user defined string associated with the order
  //! - specific to zenfire
  std::string zentag;

  //! user defined string sent to the exchange with the order
  //! - persistant on exchange (if supported)
  std::string tag;

  //! true for automated system, false for manual (CME requirement)
  bool automated;

  //##########################################################################//
  //! constructor

  order();

  order
    (
    const product_t &product,
    action_t action,
    int quantity,
    duration_t duration,
    void *context=0,
    const std::string& zentag=std::string(""),
    const std::string& tag=std::string("")
    );

  virtual ~order() = 0;
  };

//############################################################################//
//! market order argument structure

struct market : public order
  {
  typedef zenfire::order::market_t order_type;

  //##########################################################################//
  //! constructor

  market();

  market
    (
    const product_t &product,
    action_t action,
    int quantity,
    duration_t duration,
    void *context=0,
    const std::string& zentag=std::string(""),
    const std::string& tag=std::string("")
    );

  ~market();
  };


//############################################################################//
//! limit order argument structure

class limit : public order
  {
public:
  typedef zenfire::order::limit_t order_type;
  //! limit price
  double price;

  //##########################################################################//
  //! constructor

  limit();

  limit
    (
    double price,
    const order& order
    );

  limit
    (
    double price,
    const product_t &product,
    action_t action,
    int quantity,
    duration_t duration,
    void *context=0,
    const std::string& zentag=std::string(""),
    const std::string& tag=std::string("")
    );

  virtual ~limit();
  };

//############################################################################//
//! stop order argument structure

template<typename T>
class stop : public T
  {
public:
  typedef zenfire::order::stop_t<typename T::order_type> order_type;

  //! trigger price
  double trigger;

  //##########################################################################//
  //! create a stop limit order
  //! @param trigger price
  //! @param order limit order variables

  stop(double trigger, const T& order);

  //##########################################################################//
  //! create a stop market order
  //! @param trigger price
  //! @param order market order variables

  virtual ~stop() {};
  };

//############################################################################//
//! ctor stop market order

template<> inline
stop<market>::stop(double t_, const market& o_)
 : market(o_),
   trigger(t_)
  {
  }

//############################################################################//
//! ctor stop limit order
  
template<> inline
stop<limit>::stop(double t_, const limit& o_)
 : limit(o_),
   trigger(t_)
  {
  }

typedef stop<limit> stop_limit;
typedef stop<market> stop_market;

}} // namespace zenfire::arg
//############################################################################//

#endif // end ZENFIRE_ARG_HPP__

/******************************************************************************
 ******************************************************************************/

