
/******************************************************************************
 ******************************************************************************/

/** \file product.hpp
 *  \brief product classes
 *
 * $Id: product.hpp 1392 2009-09-20 14:49:00Z grizz $
 */

#ifndef ZENFIRE_PRODUCT_HPP__
#define ZENFIRE_PRODUCT_HPP__

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
#include <zenfire/exchange.hpp>

#include <string>
#include <vector>

namespace zenfire {
namespace product {

//! product type, high order byte (&0xff00) contains ORable flags about type,
//! low order (&0x00ff) contains type;  use product::is_*() functions for ease
typedef uint16_t type_t;
//! future
const type_t FUTURE = 1;
//! spot (cash)
const type_t SPOT = 2;
//! security
const type_t SECURITY = 3;

//! future option
const type_t FUTURE_OPTION = 4;
//! spot option
const type_t SPOT_OPTION = 5;
//! security option
const type_t SECURITY_OPTION = 6;

//! ORable flag denoting spread
const type_t SPREAD = 0x80 <<8;

//############################################################################//
//! product

class product_t
  {
public:
  //! product symbol
  std::string symbol;
  //! exchange string
  exchange_t exchange;

  //! token for comparing products
  int token;
  type_t type;
  //! minimum trade price increment (tick size)
  double increment;
  //! number of decimal places
  int precision;

  //! has_point_value is deprecated in favor of has_specs
  bool has_point_value;
  //! has specs populated (specs are point_value, currency, description)
  bool has_specs;
  //! point value
  double point_value;
  //! currency code
  std::string currency;
  //! product description
  std::string description;

  product_t();

  //##########################################################################//
  //! constructor
  //! @param product product product
  //! @param exchange

  product_t(const std::string& product, exchange_t exchange);
  ~product_t();

  //##########################################################################//
  };

//############################################################################//

//! checks if instrument is a future
inline
bool
is_future(const product_t &p)
  {
  return((p.type & 0xff) == FUTURE);
  }

//! checks if instrument is a spot
inline
bool
is_spot(const product_t &p)
  {
  return((p.type & 0xff) == SPOT);
  }

//! checks if instrument is a security
inline
bool
is_security(const product_t &p)
  {
  return((p.type & 0xff) == SECURITY);
  }

//! checks if instrument is a futures option
inline
bool
is_future_option(const product_t &p)
  {
  return((p.type & 0xff) == FUTURE_OPTION);
  }

//! checks if instrument is a spot option
inline
bool
is_spot_option(const product_t &p)
  {
  return((p.type & 0xff) == SPOT_OPTION);
  }

//! checks if instrument is a securities option
inline
bool
is_security_option(const product_t &p)
  {
  return((p.type & 0xff) == SECURITY_OPTION);
  }

//! checks if instrument is a spread
inline
bool
is_spread(const product_t &p)
  {
  return((p.type & SPREAD) != 0);
  }


} // namespace product

using product::product_t;

typedef sys::shared_ptr<product_t> product_ptr;

} // namespace zenfire
//############################################################################//

#endif // end ZENFIRE_PRODUCT_HPP__

/******************************************************************************
 ******************************************************************************/

