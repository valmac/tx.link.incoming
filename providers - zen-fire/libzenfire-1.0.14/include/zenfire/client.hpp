
/******************************************************************************
 ******************************************************************************/

/** \file client.hpp
 *  high level client interface
 *
 * $Id: client.hpp 1361 2009-09-16 16:59:11Z grizz $
 */

#ifndef ZENFIRE_CLIENT_HPP__
#define ZENFIRE_CLIENT_HPP__

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
#include <zenfire/noncopyable.hpp>
#include <zenfire/tick.hpp>
#include <zenfire/report.hpp>
#include <zenfire/alert.hpp>

#include <vector>

namespace zenfire {
namespace client {

//############################################################################//
//! high level client interface - noncopyable

class client_t : public sys::noncopyable
  {
public:
  virtual ~client_t();

  //##########################################################################//
  //##########################################################################//

  //##########################################################################//
  //! open a connection and login
  //! @param user username to connect with
  //! @param pass password to connect with
  //! @param environment to use (see list_environments)

  virtual void login
    (
    const std::string& user,
    const std::string& pass,
    const std::string& environment
    ) = 0;

  //##########################################################################//
  //! log out from an existing sesson and close connection

  virtual void logout() = 0;

  //##########################################################################//
  //! get engine option
  //! @param opt name of option
  //! @returns option value

  virtual int option(const std::string &opt) = 0;

  //##########################################################################//
  //! set engine option
  //! @param opt name of option
  //! @param val value to set to
  //! @returns new option value

  virtual int option(const std::string &opt, int val) = 0;

  //##########################################################################//
  //! get a vector of environments available

  virtual std::vector<std::string> list_environments() = 0;

  //##########################################################################//
  //! get a vector of accounts available to user

  virtual std::vector<std::string> list_accounts() = 0;

  //##########################################################################//
  //! lookup up an account index by name
  //! @param account string of exact account name

  virtual int lookup_account(const std::string&) = 0;

  //##########################################################################//
  //! set default account
  //! - for users with only one account, this is set automatically
  //! @param acctno account number index from list_accounts()

  virtual void set_account(int acctno) = 0;

  //##########################################################################//
  //! set default account
  //! - for users with only one account, this is set automatically
  //! @param acct string representation of account name

  virtual void set_account(const std::string& acct) = 0;

  //##########################################################################//
  //! get a vector of exchanges

  virtual std::vector<exchange_t> list_exchanges() = 0;

  //##########################################################################//
  //! lookup product

  virtual product_t lookup_product
    (
    const arg::product&
    ) = 0;

  //##########################################################################//
  //! subscribe to a product
  //! - if product is already subscribed with different flags,
  //!   it will be unsubscribed and resubscribed

  virtual void subscribe
    (
    const product_t& product,
    uint32_t flags=subscribe::ALL|subscribe::SNAPSHOT
    ) = 0;

  //##########################################################################//
  //! unsubscribes a subscribed product

  virtual void unsubscribe
    (
    const product_t& product
    ) = 0;

  //##########################################################################//
  //! subcribes to reports for specified account

  virtual void subscribe_account
    (
    int acctno=account::DEFAULT,
    report::flags_t flags=report::ALL
    ) = 0;

  //##########################################################################//
  //! unsubscribes specified account

  virtual void unsubscribe_account
    (
    int acctno=account::DEFAULT
    ) = 0;

  //##########################################################################//
  //! lists open orders for account

  virtual void
  request_open_orders
    (
    int acctno=account::DEFAULT
    ) = 0;

  //##########################################################################//
  //! requests order events fired for previous orders
  //! - only lists orders from the current trading day
  //! @param from time (UTC), default all day
  //! @param to time, default now (UTC)

  virtual void
  request_orders
    (
    int from=0,
    int to=0,
    int acctno=account::DEFAULT
    ) = 0;

  //##########################################################################//
  //! requests current product profit/loss for account be sent to callback
  //! @param acctno account

  virtual void
  request_pl
    (
    int acctno=account::DEFAULT
    ) = 0;

  //##########################################################################//
  //! requests current positions for account
  //! @param acctno account

  virtual void
  request_positions
    (
    int acctno=account::DEFAULT
    ) = 0;

  //##########################################################################//
  //! requests trade tick events fired for all trade ticks since from
  //! - only replays ticks from current day
  //! - must be enabled by passing a non zero value to option("intraday_tick")
  //! @param from time (UTC), default start of trade session
  //! @param to time, default now (UTC)

  virtual void
  replay_ticks
    (
    const product_t& product,
    int from=0,
    int to=0
    ) = 0;

  //##########################################################################//
  //! send a previously prepared order 
  //! sends an order to the exchange, copies all relevant order info from
  //! order and returns a shared pointer to zenfire's internal order reference
  //!
  //! @param order to send
 
  virtual void
  place_order
    (
    sys::shared_ptr<order_t> order
    ) = 0;

  //##########################################################################//
  //! send an order
  //! sends an order to the exchange, copies all relevant order info from
  //! order and returns a shared pointer to zenfire's internal order reference
  //!
  //! @param order to send
  //! @param account optional if not set order gets placed on default account
 
  template <typename T>
  order_ptr
  place_order
    (
    const T& order_args,
    int acctno=account::DEFAULT
    )
    {
    order_ptr order = prepare_order(order_args, acctno);
    order->send();
    return(order);
    }

  //##########################################################################//
  //! cancel all orders
  //! @param acctno optional account index from list_accounts()

  virtual void cancel_all(int acctno=account::DEFAULT) = 0;

  //##########################################################################//
  //! set callback for all tick messages

  virtual void hook_ticks(const callback<tick_t>::function&) = 0;

  //##########################################################################//
  //! set callback for all order report messages

  virtual void hook_reports(const callback<report_t>::function&) = 0;

  //##########################################################################//
  //! set callback for alert messages (network up, down, disabled)

  virtual void hook_alerts(const callback<alert::alert_t>::function&) = 0;

  //##########################################################################//
  //! hook all tick messages by callback struct

  virtual void hook(const tick::callback_t&) = 0;

  //##########################################################################//
  //! hook all report messages by callback struct

  virtual void hook(const report::callback_t&) = 0;

  //##########################################################################//
  //! hook all account messages by callback struct

  virtual void hook(const account::callback_t&) = 0;

  //##########################################################################//
  //! prepare an order for later sending

  virtual order_ptr
  prepare_order
    (
    const arg::market& order,
    int acctno=account::DEFAULT
    ) = 0;

  //! prepare an order for later sending
  virtual order_ptr
  prepare_order
    (
    const arg::limit& order,
    int acctno=account::DEFAULT
    ) = 0;

  //! prepare an order for later sending
  virtual order_ptr
  prepare_order
    (
    const arg::stop_market& order,
    int acctno=account::DEFAULT
    ) = 0;

  //! prepare an order for later sending
  virtual order_ptr
  prepare_order
    (
    const arg::stop_limit& order,
    int acctno=account::DEFAULT
    ) = 0;

protected:
  client_t();

private:

  //##########################################################################//
  };

//############################################################################//
//! client factory
//! @param path path to ssl certs

client_t* ver_create(const std::string &path, const std::string &version);

inline
client_t*
create(const std::string& path)
  {
  return(ver_create(path, version));
  }

} // namespace client

using client::client_t;

} // namespace zenfire
//############################################################################//

#endif // end ZENFIRE_CLIENT_HPP__

/******************************************************************************
 ******************************************************************************/

