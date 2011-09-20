
/******************************************************************************
 ******************************************************************************/

/** \file tick.hpp
 *  \brief tick structures
 *
 * $Id: tick.hpp 1397 2009-09-20 16:51:48Z grizz $
 */

#ifndef ZENFIRE_TICK_HPP__
#define ZENFIRE_TICK_HPP__

//############################################################################//
// L I C E N C E #############################################################//
//############################################################################//

/*
 * (C) 2008 BigWells Technology (Zen-Fire). All rights reserved. 
 * Confidential and proprietary information of BigWells Technology.
 */

//############################################################################//
//# I N C L U D E S ##########################################################//
//############################################################################//

#include "zenfire/order.hpp"
#include "zenfire/product.hpp"
#include "zenfire/callback.hpp"

namespace zenfire {
namespace tick {

//! tick types

enum type_t
  {
  //! type is unknown
  UNKNOWN = 0,
  //! ask
  ASK = 1,
  //! best ask
  BEST_ASK = 2,
  //! bid
  BID = 3,
  //! best bid
  BEST_BID = 4,
  //! trade print
  TRADE = 5,
  //! day low (currently unimplmented)
  LOW = 6,
  //! day high (currently unimplemented)
  HIGH = 7,
  //! market open price (currently unimplemented)
  OPEN = 8,
  //! market close price (currently unimplemented)
  CLOSE = 9,
  //! trade volume
  VOLUME = 10,
  //! market mode
  MODE = 11,
  };

//############################################################################//
//! Base class for tick messages

struct tick_t
  {
  //! price
  double price;
  //! size
  int32_t size;
  //! timestamp (seconds since epoch)
  time_t ts;
  //! microseconds
  uint32_t usec;
  //! flags
  uint32_t flags;

  /****************************************************************************/

  tick_t();
  ~tick_t();

  //! get type of tick
  //! @returns type of tick
  type_t type() const { return(typ_); }

  type_t typ_;
  product_ptr product;
  };

//############################################################################//
//! product ask

struct ask_t : public tick_t
  {
  ask_t();
  };

//############################################################################//
//! best offer

struct best_ask_t : public tick_t
  {
  best_ask_t();
  };

//############################################################################//
//! product bid

struct bid_t : public tick_t
  {
  bid_t();
  };

//############################################################################//
//! best bid

struct best_bid_t : public tick_t
  {
  best_bid_t();
  };

//############################################################################//
//! trade print

struct trade_t : public tick_t
  {
  trade_t();
  };

//############################################################################//
//! day low

struct low_t : public tick_t
  {
  low_t();
  };

//############################################################################//
//! day high

struct high_t : public tick_t
  {
  high_t();
  };

//############################################################################//
//! market open

struct open_t : public tick_t
  {
  open_t();
  };

//############################################################################//
//! market close

struct close_t : public tick_t
  {
  close_t();
  };

//############################################################################//
//! trade volume

struct volume_t : public tick_t
  {
  volume_t();
  };

//############################################################################//
//! market mode

struct mode_t : public tick_t
  {
  mode_t();
  };

//############################################################################//
//! detailed callback, one function per tick type
//! - this will overwrite all callbacks set by hook_ticks()

struct callback_t
  {
  callback<ask_t>::function ask;
  callback<best_ask_t>::function best_ask;
  callback<bid_t>::function bid;
  callback<best_bid_t>::function best_bid;
  callback<trade_t>::function trade;
  callback<low_t>::function low;
  callback<high_t>::function high;
  callback<open_t>::function open;
  callback<close_t>::function close;
  callback<volume_t>::function volume;
  callback<mode_t>::function mode;
  };

} // namespace tick

using tick::tick_t;

//! product subscription flags
namespace subscribe {
  typedef uint32_t flags_t;

  // type flags

  //! bid, ask
  const uint32_t QUOTE   = 1<<0;
  //! best_bid, best_ask
  const uint32_t BEST    = 1<<1;
  //! trade print
  const uint32_t TRADE   = 1<<2;
  //! open, close
  const uint32_t MODE    = 1<<3;
  //! high, low
  const uint32_t EXTREME = 1<<4;
  //! trade volume
  const uint32_t VOLUME  = 1<<5;
  //! all
  const uint32_t ALL     = 0xFFFF;

  // subscribe flags

  //! send a snapshot of current level 2 data via event callbacks
  const uint32_t SNAPSHOT = 1<<16;
} // namespace subscribe

//! market modes
namespace mode {
  typedef uint32_t mode_t;

  //! open
  const uint32_t OPEN = 1;
  //! closed
  const uint32_t CLOSED = 2;
  //! pre-open
  const uint32_t PRE_OPEN = CLOSED|8;
  //! halted
  const uint32_t HALTED = CLOSED|9;
} // namespace subscribe

} // namespace zenfire
//############################################################################//

#endif // end ZENFIRE_TICK_HPP__

/******************************************************************************
 ******************************************************************************/

