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

#include <assert.h>

#include "util.h"

namespace throughput {
namespace disruptor {

int ceilingNextPowerOfTwo(const int & x)
{
  return 1 << fls(x - 1);
}


long getMinimumSequence(const std::vector<Consumer*> & consumers)
{
  long minimum = LONG_MAX;
  
  for (std::vector<Consumer*>::const_iterator itr = consumers.begin();
       itr < consumers.end();
       ++itr)
  {
    long sequence = (*itr)->getSequence();
    minimum = minimum < sequence ? minimum : sequence;
  }
  
  return minimum;
}


}
}