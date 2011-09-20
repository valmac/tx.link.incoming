
/******************************************************************************
 ******************************************************************************/

/** \file exchange.hpp
 *  zenfire exchange
 *
 * $Id: exchange.hpp 3297 2010-10-05 18:21:52Z grizz $
 */

#ifndef ZENFIRE_EXCHANGE_HPP__
#define ZENFIRE_EXCHANGE_HPP__

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

#include <string>
#include <vector>

namespace zenfire {

namespace exchange {

//! exchanges
enum exchange_t
  {
  AUTO=0,
  //! CME - Chicago Mercantile Exchange
  CME=1,
  //! CBOT - Chicago Board of Trade
  CBOT=2,
  //! NYMEX - New York Mercantile Exchange
  NYMEX=3,
  //! NYBOT - New York Board of Trade
  NYBOT=4,
  //! EUREX
  EUREX=5,
  //! LIFFE - London International Financial Futures and Options Exchange
  LIFFE=6,
  //! MATIF
  MATIF=7,
  //! HSFX - HotSpot Forex
  HSFX=8,
  //! KCBT - Kansas City Board of Trade
  KCBT=9,
  //! MGEX - Minneapolis Grain Exchange
  MGEX=10,
  //! FXCM_R - Forex Capital Markets Retail
  FXCM_R=11,
  //! ICE - InterContinental Exchange
  ICE=12,
  //! NLX - NYSE Liffe US
  NLX=13,
  //! COMEX
  COMEX=18,
  };

  //! get a string representation of exchange
  std::string
  to_string(exchange_t);

  //! find the exchange type from string
  exchange_t
  from_string(const std::string&);

} // namespace exchange

using exchange::exchange_t;

} // namespace zenfire
//############################################################################//

#endif // end ZENFIRE_EXCHANGE_HPP__

/******************************************************************************
 ******************************************************************************/

