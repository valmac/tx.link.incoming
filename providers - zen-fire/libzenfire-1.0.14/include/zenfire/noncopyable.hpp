
/******************************************************************************
 ******************************************************************************/

/** \file platform.hpp
 *  noncopyable
 *
 * $Id: noncopyable.hpp 51 2008-10-28 23:30:50Z grizz $
 */

#ifndef ZENFIRE_NONCOPYABLE_HPP__
#define ZENFIRE_NONCOPYABLE_HPP__

//############################################################################//
// L I C E N S E #############################################################//
//############################################################################//

/*
 * (C) 2008 BigWells Technology (Zen-Fire). All rights reserved. 
 * Confidential and proprietary information of BigWells Technology.
 */

//############################################################################//

namespace zenfire {
namespace sys {

class noncopyable
  {
protected:
  noncopyable() {}
  ~noncopyable() {}
private:
  noncopyable(const noncopyable&);
  const noncopyable& operator=(const noncopyable&);
  };

}} // namespace zenfire
//############################################################################//

#endif // end ZENFIRE_NONCOPYABLE_HPP__

/******************************************************************************
 ******************************************************************************/

