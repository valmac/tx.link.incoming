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

#include <iostream>
#include "AlertException.h"

namespace throughput {
namespace disruptor {

AlertException* AlertException::ALERT_EXCEPTION = NULL;
boost::signals2::mutex AlertException::mutex;
  
AlertException & AlertException::getInstance() {
  if (ALERT_EXCEPTION == NULL) {
    // Conceivably, we have a race condition here where the singleton may be
    // created several times the first time this is called. We'll wrap the
    // singleton construction in a mutex to keep it thread-safe.
    
    mutex.lock();
    if (ALERT_EXCEPTION == NULL) {
      ALERT_EXCEPTION = new AlertException();
    }
    mutex.unlock();
  }

  return *ALERT_EXCEPTION;
}
  
} //-- end namespace disruptor
} //-- end namespace throughput
