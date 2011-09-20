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

#ifndef DISRUPTOR_CLAIMSTRATEGY_H_
#define DISRUPTOR_CLAIMSTRATEGY_H_

#include <boost/atomic.hpp>

#include "RingBufferInitialCursorValue.h"

namespace throughput {
namespace disruptor {

using namespace boost;
 
  
/**
  * Indicates the threading policy to be applied for claiming {@link Entry}s by producers to the {@link RingBuffer}
  */
typedef enum ClaimStrategyOption_
  {
    /** Makes the {@link RingBuffer} thread safe for claiming {@link Entry}s
     * by multiple producing threads.
     */
    MULTI_THREADED,
    /** Optimized {@link RingBuffer} for use by single thread claiming
     * {@link Entry}s as a producer.
     */
    SINGLE_THREADED

  } ClaimStrategyOption;

/**
 * Strategies employed for claiming the sequence of {@link AbstractEntry}s in the {@link RingBuffer} by producers.
 *
 * The {@link AbstractEntry} index is a the sequence value mod the {@link RingBuffer} capacity.
 */
class ClaimStrategy
{
public:
  /**
   * Claim the next sequence index in the {@link RingBuffer} and increment.
   *
   * @return the {@link Entry} index to be used for the producer.
   */
  virtual long incrementAndGet() = 0;
  
  /**
   * Increment by a delta and get the result.
   *
   * @param delta to increment by.
   * @return the result after incrementing.
   */
  virtual long incrementAndGet(const int & delta ) = 0;
  
  /**
   * Set the current sequence value for claiming {@link Entry} in the {@link RingBuffer}
   *
   * @param sequence to be set as the current value.
   */
  virtual void setSequence(const long & sequence) = 0;
  
  /**
   * Used by the {@link RingBuffer} as a polymorphic constructor.
   *
   * @return a new instance of the ClaimStrategy
   */
  static ClaimStrategy* newInstance(const ClaimStrategyOption & claimStrategyOption);
};
  


/**
 * Strategy to be used when there are multiple producer threads
 * claiming {@link Entry}s.
 */
class MultiThreadedStrategy : public ClaimStrategy
{
public:
  MultiThreadedStrategy() : sequence_(RINGBUFFER_INITIAL_CURSOR_VALUE) {}
  
  virtual long incrementAndGet()
  {
    return ++sequence_;
  }
  
  //@Override
  virtual long incrementAndGet(const int & delta)
  {
    return sequence_ += delta;
  }
  
  //@Override
  virtual void setSequence(const long & sequence)
  {
    sequence_.store(sequence);
  }

private:
  atomic<long> sequence_;
};


/**
  * Optimised strategy can be used when there is a
  * single producer thread claiming {@link Entry}s.
  */
class SingleThreadedStrategy : public ClaimStrategy
{
public:
  SingleThreadedStrategy() : sequence_(RINGBUFFER_INITIAL_CURSOR_VALUE) {}
  
  //@Override
  virtual long incrementAndGet()
  {
    return ++sequence_;
  }
  
  //@Override
  virtual long incrementAndGet(const int & delta)
  {
    sequence_ += delta;
    return sequence_;
  }
  
  //@Override
  virtual void setSequence(const long & sequence)
  {
    sequence_ = sequence;
  }

private:
  
  long sequence_;
};

} //-- end namespace disruptor
} //-- end namespace throughput
#endif
