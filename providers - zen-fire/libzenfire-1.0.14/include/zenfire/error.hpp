
/******************************************************************************
 ******************************************************************************/

/** \file error.hpp
 *  zenfire base exceptions
 *
 * $Id: error.hpp 160 2009-01-09 18:00:15Z grizz $
 */

#ifndef ZENFIRE_ERROR_HPP__
#define ZENFIRE_ERROR_HPP__

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

#include <stdexcept>

namespace zenfire {
namespace error {

//! access denied, invalid credentials, etc
class access_t : public std::runtime_error
  {
  public:
    access_t(const std::string& what) : std::runtime_error(what) {}
  };

//! connection, network, etc
class connection_t : public std::runtime_error
  {
  public:
    connection_t(const std::string& what) : std::runtime_error(what) {}
  };

//! operation timed out
class timeout_t : public std::runtime_error
  {
  public:
    timeout_t(const std::string& what) : std::runtime_error(what) {}
  };

//! invalid argument
class invalid_t : public std::runtime_error
  {
  public:
    invalid_t(const std::string& what) : std::runtime_error(what) {}
  };

//! invalid account
class invalid_account_t : public std::runtime_error
  {
  public:
    invalid_account_t(const std::string& what) : std::runtime_error(what) {}
  };

//! invalid symbol
class invalid_product_t : public std::runtime_error
  {
  public:
    invalid_product_t(const std::string& what) : std::runtime_error(what) {}
  };

//! internal zenfire error
class internal_t : public std::runtime_error
  {
  public:
    internal_t(const std::string& what) : std::runtime_error(what) {}
  };

}} // namespace zenfire::error
//############################################################################//

#endif // end ZENFIRE_ERROR_HPP__

/******************************************************************************
 ******************************************************************************/

