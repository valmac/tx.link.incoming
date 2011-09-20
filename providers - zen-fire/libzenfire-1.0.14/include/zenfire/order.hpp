
/******************************************************************************
 ******************************************************************************/

/** \file order.hpp
 *  \brief order interface
 *
 * $Id: order.hpp 651 2009-06-26 05:09:29Z grizz $
 */

#ifndef ZENFIRE_ORDER_HPP
#define ZENFIRE_ORDER_HPP

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

#include <zenfire/platform.hpp>
#include <zenfire/noncopyable.hpp>
#include <zenfire/product.hpp>
#include <zenfire/arg.hpp>

#include <string>

//############################################################################//

namespace zenfire {

namespace client {
  class client_t;
  class client_1_t;
};

namespace order {

//! order type
enum type_t
  {
  //! Not set
  NO_TYPE = 0,
  //! market
  MARKET = 1,
  //! limit
  LIMIT = 2,
  //! stop market
  STOP_MARKET = 3,
  //! stop limit
  STOP_LIMIT = 4,
  };

//! order status
enum status_t
  {
  //! Not set
  NO_STATUS = 0,
  //! prepared client side, not yet sent
  PREPARED = 1,
  //! sent
  SENT = 2,
  //! received
  PENDING = 3,
  //! open on exchange
  OPEN = 4,
  //! pending update
  UPDATING = 5,
  //! complete
  COMPLETE = 6,
  };

//! order completion reason, not set unless order is complete
enum reason_t
  {
  NO_REASON = 0,
  //! filled
  FILLED = 1,
  //! partially filled, then canceled
  PARTIAL = 2,
  //! canceled
  CANCELED = 3,
  //! failed
  FAILED = 4,
  //! canceled
  REJECTED = 5,
  //! busted
  BUSTED = 6,
  };


namespace request_flags
  {
  typedef uint32_t type;

  //! only open
  const uint32_t OPEN   = 1<<0;
  //! all known orders
  const uint32_t ALL    = 0xffff;
  };

class impl_t;

//############################################################################//
//! abstract base class for order types

class order_t : public sys::noncopyable
  {
public:
  virtual ~order_t();

  //! order type
  virtual type_t type();

  //! order number if received
  //! @returns 0 if not received
  unsigned int number();

  //! order status
  status_t status();

  //! order completion reason
  reason_t reason();
 
  //! message explaining order status
  const std::string& message();

  //! account index from client_t::list_accounts()
  int acctno();

  //! account name
  std::string acct();

  //! product
  const product_t& product();

  //! action
  action_t action();

  //! duration
  duration_t duration();

  //! user defined context to be included on order callbacks
  //! - specific to current connection
  void* context();

  //! user defined string associated with the order
  //! - specific to zenfire
  const std::string& zentag();

  //! user defined string sent to the exchange with the order
  //! - persistant on exchange (if supported)
  const std::string& tag();

  //! total quantity of the order
  //! - before sending this is the prepared quantity
  //! - after open it's qty_open() + qty_filled() + qty_canceled()
  int qty();

  //! quantity confirmed and opened on the exchange
  int qty_open();
  //! quantity filled
  int qty_filled();
  //! quantity canceled
  int qty_canceled();

  //! deprecated - will be removed in future versions
  int open() { return(qty_open()); }
  //! deprecated - will be removed in future versions
  int filled() { return(qty_filled()); }
  //! deprecated - will be removed in future versions
  int canceled() { return(qty_canceled()); }

  //! order price - for limit orders only, others will throw error::invalid_t
  double price();

  //! trigger price - for stop orders only, others will throw error::invalid_t
  double trigger();

  //! fill price
  //! - if the order has multiple partial fills with difference prices,
  //!   this will be the average price
  double fill_price();

  //! update order
  int set_qty(int);
  double set_price(double);
  double set_trigger(double);

  //! get updated stats, useful if update() hasn't been called yet
  int set_qty();
  double set_price();
  double set_trigger();

  //! send order to exchange
  void send();

  //! update already opened order
  void update();

  //! cancel order or any remaining unfilled portion
  //! @param reason optional reason to be sent with cancel
  void cancel(const std::string& reason=std::string(""));

protected:
  friend class zenfire::client::client_t;
  friend class zenfire::client::client_1_t;;

  friend class ::zenfire::order::impl_t;

  //##########################################################################//
  //! constructor

  order_t();

  std::auto_ptr<impl_t> impl_;
  virtual impl_t& impl();
  };

} // namespace order

using namespace order;

typedef sys::shared_ptr<order::order_t> order_ptr;

} // namespace zenfire
//############################################################################//

#endif // ZENFIRE_ORDER_HPP

/******************************************************************************
 ******************************************************************************/

