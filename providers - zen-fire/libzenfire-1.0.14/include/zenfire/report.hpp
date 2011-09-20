
/******************************************************************************
 ******************************************************************************/

/** \file report.hpp
 *  report messages for callback
 *
 * $Id: report.hpp 497 2009-05-04 13:32:17Z grizz $
 */

#ifndef ZENFIRE_REPORT_HPP__
#define ZENFIRE_REPORT_HPP__

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

#include <zenfire/account.hpp>
#include <zenfire/order.hpp>
#include <zenfire/callback.hpp>
#include <zenfire/platform.hpp>

namespace zenfire {
namespace report {

enum type_t
  {
  UNKNOWN = 0,
  BUST = 1,
  CANCEL = 2,
  FAIL = 3,
  FILL = 4,
  MODIFY = 5,
  CANCEL_FAIL = 6,
  UPDATE_FAIL = 7,
  OPEN = 8,
  REJECT = 9,
  TRIGGER = 10,
  REPLAY = 11,
  END_OF_REPLAY = 12,
  };

//############################################################################//
//! base class for order reports

struct report_t
  {
  type_t typ_;
  std::string msg_;
  int qty_;
  double price_;
  sys::shared_ptr<order_t> order;

  time_t ts;
  uint32_t usec;

  report_t();
  virtual ~report_t();
  //! order report type
  virtual type_t type() const { return(typ_); }
  virtual std::string message() const { return(msg_); }
  virtual double price() const { return(price_); }
  virtual int qty() const { return(qty_); }
  };

//############################################################################//

struct bust_t : public report_t
  {
  bust_t();
  };

//############################################################################//

struct cancel_t : public report_t
  {
  cancel_t();
  };

//############################################################################//

struct fail_t : public report_t
  {
  fail_t();
  };

//############################################################################//

struct fill_t : public report_t
  {
  fill_t();
  };

//############################################################################//

struct modify_t : public report_t
  {
  modify_t();
  };

//############################################################################//

struct cancel_fail_t : public report_t
  {
  cancel_fail_t();
  };

//############################################################################//

struct update_fail_t : public report_t
  {
  update_fail_t();
  };

//############################################################################//

struct open_t : public report_t
  {
  open_t();
  };

//############################################################################//

struct reject_t : public report_t
  {
  reject_t();
  };

//############################################################################//

struct trigger_t : public report_t
  {
  trigger_t();
  };

//############################################################################//

struct replay_t : public report_t
  {
  replay_t();
  };

//############################################################################//
//! detailed callback, one function per report type
//! - this will overwrite all callbacks set by hook_reports()

struct callback_t
  {
  callback<bust_t>::function bust;
  callback<cancel_t>::function cancel;
  callback<fail_t>::function fail;
  callback<fill_t>::function fill;
  callback<modify_t>::function modify;
  callback<cancel_fail_t>::function cancel_fail;
  callback<update_fail_t>::function update_fail;
  callback<open_t>::function open;
  callback<reject_t>::function reject;
  callback<trigger_t>::function trigger;
  callback<replay_t>::function replay;
  };

} // namespace report

using report::report_t;

} // namespace zenfire
//############################################################################//

#endif // end ZENFIRE_REPORT_HPP__

/******************************************************************************
 ******************************************************************************/

