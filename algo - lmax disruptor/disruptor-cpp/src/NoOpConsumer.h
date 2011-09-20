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

#include "Consumer.h"

#ifndef disruptor_NoOpConsumer_h
#define disruptor_NoOpConsumer_h

namespace throughput {
namespace disruptor {

/**
 * No operation version of a {@link Consumer} that simply tracks a {@link RingBuffer}.
 * This is useful in tests or for pre-filling a {@link RingBuffer} from a producer.
 */
template <class Entry>
class NoOpConsumer : public Consumer
{
private:
  RingBuffer<Entry> & ringBuffer_;

public:
  /**
   * Construct a {@link Consumer} that simply tracks a {@link RingBuffer}.
   *
   * @param ringBuffer to track.
   */
  NoOpConsumer(RingBuffer<Entry> & ringBuffer) :
    ringBuffer_(ringBuffer)
  {
  }
  
  //@Override
  virtual long getSequence() const
  {
    return ringBuffer_.getCursor();
  }
  
  //@Override
  virtual void halt()
  {
  }
  
  //@Override
  virtual void run()
  {
  }
};

  
} //-- end namespace disruptor
} //-- end namespace throughput

#endif
