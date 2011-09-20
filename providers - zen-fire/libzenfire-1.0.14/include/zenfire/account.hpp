
/******************************************************************************
 ******************************************************************************/

/** \file account.hpp
 *  \brief account classes
 *
 * $Id: account.hpp 1316 2009-09-11 16:46:14Z grizz $
 */

#ifndef ZENFIRE_ACCOUNT_HPP__
#define ZENFIRE_ACCOUNT_HPP__

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

#include <zenfire/product.hpp>
#include <zenfire/callback.hpp>
#include <string>

//############################################################################//
namespace zenfire {

namespace position {
typedef uint32_t type_t;

const type_t UPDATE = 1;
const type_t REPLAY = 2;
const type_t END_OF_REPLAY = 3;
}

namespace account {

static const int DEFAULT = -1;

//############################################################################//
//! position report

struct position_t
  {
  //! account number
  int acctno;

  product_t product;

  //! position size, negative for short, positive for long
  int size;

  //! open profit loss
  double open_pl;

  //! closed profit loss
  double closed_pl;

  //! average price position opened at
  double
  avg_fill_price() const;

  //! type of position event
  position::type_t
  type() const
    { return(typ_); }

  position::type_t typ_;
  double fill_;
  };

//############################################################################//
//! profit loss report for specific symbol

struct profitloss_t
  {
  //! account number
  int acctno;

  //! open profit / loss
  double open;
  //! closed profit / loss
  double closed;
  //! account balance
  double balance;
  //! margin
  double margin;

  //! true if account has a balance
  bool has_balance;
  };

//############################################################################//
//! callback structure

struct callback_t
  {
  callback<position_t>::function position;
  callback<profitloss_t>::function profitloss;
  };

} // namespace account

using account::position_t;
using account::profitloss_t;

} // namespace zenfire::account
//############################################################################//

#endif // end ZENFIRE_ACCOUNT_HPP__

/******************************************************************************
 ******************************************************************************/

