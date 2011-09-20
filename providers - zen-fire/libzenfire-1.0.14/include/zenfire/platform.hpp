
/******************************************************************************
 ******************************************************************************/

/** \file platform.hpp
 *  zenfire platform and compiler specific mappings
 *
 * $Id: platform.hpp 1270 2009-09-02 20:29:58Z grizz $
 */

#ifndef ZENFIRE_PLATFORM_HPP__
#define ZENFIRE_PLATFORM_HPP__

//############################################################################//
// L I C E N S E #############################################################//
//############################################################################//

/*
 * (C) 2008 BigWells Technology (Zen-Fire). All rights reserved. 
 * Confidential and proprietary information of BigWells Technology.
 */

//############################################################################//
// GNU CC

#if defined(__GNUC__)
// tr1
#  include <tr1/memory>
#  include <tr1/functional>

// int types
#  include <stdint.h>

// compat
namespace zenfire {
namespace sys {
  using std::tr1::function;
  using std::tr1::shared_ptr;
  using std::tr1::weak_ptr;
  using std::tr1::enable_shared_from_this;

  using std::tr1::bind;
}} // zenfire::sys


//############################################################################//
// Microsoft VC

#elif defined(_MSC_VER)

// auto link
#  ifndef ZENFIRE_NO_AUTOLINK
#    ifdef _DLL
#      pragma comment(lib, "libzenfire-md.lib")
#    else
#      pragma comment(lib, "libzenfire-mt.lib")
#    endif
#  endif

// tr1
#  include <memory>
#  include <functional>

// int types
typedef unsigned __int8 uint8_t;
typedef __int8 int8_t;
typedef unsigned __int16 uint16_t;
typedef __int16 int16_t;
typedef unsigned __int32 uint32_t;
typedef __int32 int32_t;
typedef unsigned __int64 uint64_t;
typedef __int64 int64_t;

typedef unsigned int size_t;
typedef int ssize_t;

// compat
namespace zenfire {
namespace sys {
  using std::tr1::function;
  using std::tr1::shared_ptr;
  using std::tr1::weak_ptr;
  using std::tr1::enable_shared_from_this;

  using std::tr1::bind;
}} // zenfire::sys

//############################################################################//

#else
#  error "Unknown compiler type, please contact Zen-Fire for assistance"
#endif

//############################################################################//

#endif // end ZENFIRE_PLATFORM_HPP__

/******************************************************************************
 ******************************************************************************/

