
/******************************************************************************
 ******************************************************************************/

/** \file zenfire.hpp
 *  base zenfire header
 *
 * $Id: zenfire.hpp 3334 2011-06-03 15:28:49Z grizz $
 */

#ifndef ZENFIRE_HPP__
#define ZENFIRE_HPP__

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

#include "platform.hpp"

#include "error.hpp"
#include "product.hpp"
#include "account.hpp"
#include "arg.hpp"
#include "order.hpp"
#include "report.hpp"
#include "tick.hpp"

namespace zenfire {

const std::string version = "1.0.14";

namespace report {
typedef uint32_t flags_t;

const uint32_t NONE    = 0;
//! only orders placed by current connection
const uint32_t SELF    = 1<<0;
//! all orders placed on account
const uint32_t OTHERS  = 1 <<2;
//! all flags
const uint32_t ALL     = 0xFFFF;
//! send a snapshot of all orders for the current trading day
const uint32_t SNAPSHOT = 1<<16;
}

} // namespace zenfire
//############################################################################//

#endif // end ZENFIRE_HPP__

/******************************************************************************
 ******************************************************************************/

