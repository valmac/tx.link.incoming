
/******************************************************************************
 ******************************************************************************/

/** \file alert.hpp
 *  \brief network information
 *
 * $Id: alert.hpp 651 2009-06-26 05:09:29Z grizz $
 */

#ifndef ZENFIRE_ALERT_HPP__
#define ZENFIRE_ALERT_HPP__

//############################################################################//
// L I C E N C E #############################################################//
//############################################################################//

/*
 * (C) 2008 BigWells Technology (Zen-Fire). All rights reserved. 
 * Confidential and proprietary information of BigWells Technology.
 */

//############################################################################//
//$ I N C L U D E S ##########################################################//
//############################################################################//

#include <zenfire/platform.hpp>
#include <zenfire/callback.hpp>

namespace zenfire {
namespace alert {

enum type_t
  {
  UNKNOWN = 0,
  //! connection is up
  UP,
  //! connection is down, attempting to reconnect if possible
  DOWN,
  //! connection has been disabled administratively
  DISABLED,
  };


//############################################################################//
//! base class for order alerts

struct alert_t
  {
  alert_t() {};
  virtual ~alert_t() {};
  virtual type_t type() const { return(typ_); }

  virtual
  const int32_t
  number() const
    { return(num_); }

  virtual
  const std::string&
  message() const
    { return(msg_); }

  type_t typ_;
  int32_t num_;
  std::string msg_;
  };

//############################################################################//
//! detailed callback, one function per alert type
//! - this will overwrite all callbacks set by hook_alerts()

struct callback_t
  {
  callback<alert_t>::function alert;
  };

} // namespace alert

using alert::alert_t;

} // namespace zenfire
//############################################################################//

#endif // end ZENFIRE_ALERT_HPP__

/******************************************************************************
 ******************************************************************************/

