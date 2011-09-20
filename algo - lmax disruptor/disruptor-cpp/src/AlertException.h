/*
 * Copyright 2011 ThroughPut, Inc.
 *
 * This is a C++ port of the LMAX Disruptor, originally developed for Java
 * by LMAX Ldt.
 *
 * This port requires the Boost C++ library, as well as boost::atomic, which is
 * available at http://www.chaoticmind.net/~hcb/projects/boost.atomic/.
 * boost::atomic can be replaced with std::atomic if your compiler supports it.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#ifndef disruptor_AlertException_h
#define disruptor_AlertException_h

#include <boost/signals2/mutex.hpp>
#include <exception>

namespace throughput {
namespace disruptor {

/**
 * Used to alert consumers waiting at a {@link ConsumerBarrier} of status changes.
 * <P>
 * It does not fill in a stack trace for performance reasons.
 */
//@SuppressWarnings("serial")
class AlertException : public std::exception
{
public:
  
  static AlertException & getInstance();
    
private:
  
  /** Singleton pattern to avoid memory allocation in an exception */
  static AlertException * ALERT_EXCEPTION;
  static boost::signals2::mutex mutex;  
};

} //-- end namespace disruptor
} //-- end namespace throughput
#endif
