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

#include "RingBufferInitialCursorValue.h"

#ifndef disruptor_SequenceBatch_h
#define disruptor_SequenceBatch_h

namespace throughput {
namespace disruptor {

class SequenceBatch
{
public:
  
  /**
   * Create a holder for tracking a batch of claimed sequences in a {@link RingBuffer}
   * @param size of the batch to claim.
   */
  SequenceBatch(const int & size)  :
    size_(size),
    end_(RINGBUFFER_INITIAL_CURSOR_VALUE)
  { }

  
  /**
   * Get the end sequence of a batch.
   *
   * @return the end sequence in a batch
   */
  long getEnd()
  {
    return end_;
  }
  
  /**
   * Set the end of the batch sequence.  To be used by the {@link ProducerBarrier}.
   *
   * @param end sequence in the batch.
   */
  void setEnd(const long & end)
  {
    end_ = end;
  }
  
  /**
   * Get the size of the batch.
   *
   * @return the size of the batch.
   */
  int getSize()
  {
    return size_;
  }
  
  /**
   * Get the starting sequence for a batch.
   *
   * @return the starting sequence of a batch.
   */
  long getStart()
  {
    return end_ - (size_ - 1L);
  }

  
  
private:
  const int size_;
  long end_;
};
  
  
} //-- end namespace disruptor
} //-- end namespace throughput

#endif
