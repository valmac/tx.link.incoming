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


#include <limits.h>
#include <vector.h>

#include "consumer.h"

#ifndef disruptor_util_h
#define disruptor_util_h


namespace throughput {
namespace disruptor {
  
/**
 * Calculate the next power of 2, greater than or equal to x.<p>
 * From Hacker's Delight, Chapter 3, Harry S. Warren Jr.
 *
 * @param x Value to round up
 * @return The next power of 2 from x inclusive
 */
int ceilingNextPowerOfTwo(const int & x); 
  
  
/**
 * Get the minimum sequence from an array of {@link Consumer}s.
 *
 * @param consumers to compare.
 * @return the minimum sequence found or Long.MAX_VALUE if the array is empty.
 */
long getMinimumSequence(const std::vector<Consumer*> & consumers);
  
  
  
} //-- end namespace disruptor
} //-- end namespace throughput

#endif
