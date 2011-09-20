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

#include <vector>
#include <boost/signals2/mutex.hpp>
#include <boost/thread/recursive_mutex.hpp>
#include <boost/thread/condition_variable.hpp>
#include <boost/type_traits.hpp>
#include <boost/timer.hpp>
#include <boost/static_assert.hpp>

#include "AlertException.h"
#include "AbstractEntry.h"
#include "Util.h"

#ifndef DISRUPTOR_WAITSTRATEGY_H_
#define DISRUPTOR_WAITSTRATEGY_H_

namespace throughput {
namespace disruptor {

using namespace boost;
using namespace std;
  
class Consumer;
template <class T> class ConsumerBarrier;
template <class T> class RingBuffer;
  

/**
 * Strategy options which are available to those waiting on a {@link RingBuffer}
 */
typedef enum WaitStrategyOption_ {
  /** This strategy uses a condition variable inside a lock to block the
   *  consumer which saves CPU resource as the expense of lock contention.
   */
  BLOCKING,
  /** This strategy calls Thread.yield() in a loop as a waiting strategy which reduces contention at the expense of CPU resource. */
  YIELDING,
  /** This strategy call spins in a loop as a waiting strategy which is lowest and most consistent latency but ties up a CPU */
  BUSY_SPIN
} WaitStrategyOption;


/**
 * Strategy employed for making {@link Consumer}s wait on a {@link RingBuffer}.
 */
template <class Entry>
class WaitStrategy
{
  BOOST_STATIC_ASSERT(( is_base_of<AbstractEntry, Entry>::value ));
public:
  /**
   * Wait for the given sequence to be available for consumption in a {@link RingBuffer}
   *
   * @param consumers further back the chain that must advance first
   * @param ringBuffer on which to wait.
   * @param barrier the consumer is waiting on.
   * @param sequence to be waited on.
   * @return the sequence that is available which may be greater than the requested sequence.
   * @throws AlertException if the status of the Disruptor has changed.
   * @throws InterruptedException if the thread is interrupted.
   */
  virtual long waitFor(const std::vector<Consumer*> & consumers,
                       RingBuffer<Entry> & ringBuffer,
                       ConsumerBarrier<Entry> & barrier,
                       const long & sequence)
  throw (AlertException) = 0;
  
  /**
   *
   
   Wait for the given sequence to be available for consumption in a {@link RingBuffer} with a timeout specified.
   *
   * @param consumers further back the chain that must advance first
   * @param ringBuffer on which to wait.
   * @param barrier the consumer is waiting on.
   * @param sequence to be waited on.
   * @param timeout value to abort after.
   * @param units of the timeout value.
   * @return the sequence that is available which may be greater than the requested sequence.
   * @throws AlertException if the status of the Disruptor has changed.
   * @throws InterruptedException if the thread is interrupted.
   */
  virtual long waitFor(const std::vector<Consumer*> & consumers,
                        RingBuffer<Entry> & ringBuffer,
                        ConsumerBarrier<Entry> & barrier,
                        const long & sequence,
                        const unsigned long & timeout_micros)
    throw (AlertException) = 0;
  
  /**
    * Signal those waiting that the {@link RingBuffer} cursor has advanced.
    */
  virtual void signalAll() = 0;
  
    
  /**
    * Used by the {@link com.lmax.disruptor.RingBuffer} as a polymorphic constructor.
    *
    * @return a new instance of the WaitStrategy
    */
  static WaitStrategy<Entry> * newInstance(const WaitStrategyOption & waitStrategyOption)
  {
    switch (waitStrategyOption) {
      case BLOCKING:
        return new BlockingStrategy();
      case YIELDING:
        return new YieldingStrategy();
      case BUSY_SPIN:
        return new BusySpinStrategy();
    }
  }

private:

  /**
   * Blocking strategy that uses a lock and condition variable for {@link Consumer}s waiting on a barrier.
   *
   * This strategy should be used when performance and low-latency are not as important as CPU resource.
   */
  class BlockingStrategy : public WaitStrategy<Entry>
  {
  public:

    //@Override
    virtual long waitFor(const std::vector<Consumer*> & consumers,
                         RingBuffer<Entry> & ringBuffer,
                         ConsumerBarrier<Entry> & barrier,
                         const long & sequence)
      throw (AlertException)
    {
      long availableSequence = 0;
      
      if ((availableSequence = ringBuffer.getCursor()) < sequence)  {
        boost::unique_lock<boost::recursive_mutex> ulock(mutex);
        while ((availableSequence = ringBuffer.getCursor()) < sequence)    {
          if (barrier.isAlerted()) {
            throw AlertException::getInstance();
          }
          consumerNotifyCondition.wait(ulock);
        }
      }
      
      if (0 != consumers.size())  {
        while ((availableSequence = getMinimumSequence(consumers)) < sequence)    {
          if (barrier.isAlerted())      {
            throw AlertException::getInstance();
          }
        }
      }
      
      return availableSequence;
    }

    
    //@Override
    virtual long waitFor(const std::vector<Consumer*> & consumers,
                         RingBuffer<Entry> & ringBuffer,
                         ConsumerBarrier<Entry> & barrier,
                         const long & sequence,
                         const unsigned long & timeout_micros)
      throw (AlertException)
    {
      long availableSequence;
      
      if ((availableSequence = ringBuffer.getCursor()) < sequence)
      {
        boost::unique_lock<boost::recursive_mutex> ulock(mutex);
        ulock.lock();
        while ((availableSequence = ringBuffer.getCursor()) < sequence)    {
          if (barrier.isAlerted())        {
            throw AlertException::getInstance();
          }
          
          if (!consumerNotifyCondition.timed_wait(ulock,
                                                  boost::posix_time::microseconds(timeout_micros)))
          {
            break;
          }
        }
        ulock.unlock();
      }
      
      if (0 != consumers.size())  {
        while ((availableSequence = getMinimumSequence(consumers)) < sequence)    {
          if (barrier.isAlerted())      {
            throw AlertException::getInstance();
          }
        }
      }
      
      return availableSequence;
    }

    
    //@Override
    virtual void signalAll()
    {
      consumerNotifyCondition.notify_all();
    }


  private:
    boost::recursive_mutex mutex;
    boost::condition_variable_any consumerNotifyCondition;
  };

    
  /**
   * Yielding strategy that uses a sleep(0) for {@link Consumer}s waiting on a barrier.
   *
   * This strategy is a good compromise between performance and CPU resource.
   */
  class YieldingStrategy : public WaitStrategy<Entry>
  {
  public:
    //@Override
    virtual long waitFor(const std::vector<Consumer*> & consumers,
                         RingBuffer<Entry> & ringBuffer,
                         ConsumerBarrier<Entry> & barrier,
                         const long & sequence)
    throw (AlertException)
    {
      long availableSequence = 0;
      
      if (0 == consumers.size()) {
        while ((availableSequence = ringBuffer.getCursor()) < sequence) {
          if (barrier.isAlerted())          {
            throw AlertException::getInstance();
          }
          sleep(0); // yield thread
        }
      }
      else {
        while ((availableSequence = getMinimumSequence(consumers)) < sequence) {
          if (barrier.isAlerted())          {
            throw AlertException::getInstance();
          }
          sleep(0); // yield thread
        }
      }
      
      return availableSequence;
    }

        
    //@Override
    virtual long waitFor(const std::vector<Consumer*> & consumers,
                         RingBuffer<Entry> & ringBuffer,
                         ConsumerBarrier<Entry> & barrier,
                         const long & sequence,
                         const unsigned long & timeout_micros)
    throw (AlertException)
    {
      boost::timer timer;
      long availableSequence;
      
      if (0 == consumers.size()) {
        while ((availableSequence = ringBuffer.getCursor()) < sequence) {
          if (barrier.isAlerted()) {
            throw AlertException::getInstance();
          }
          
          sleep(0);
          if (timeout_micros < (timer.elapsed() * 1000000)) {
            break;
          }
        }
      }
      else {
        while ((availableSequence = getMinimumSequence(consumers)) < sequence) {
          if (barrier.isAlerted()) {
            throw AlertException::getInstance();
          }
          
          sleep(1);
          if (timeout_micros < (timer.elapsed() * 1000000)) {
            break;
          }
        }
      }
      
      return availableSequence;
    }
    
    //@Override
    virtual void signalAll() {}
  };


  /**
   * Busy Spin strategy that uses a busy spin loop for {@link Consumer}s waiting on a barrier.
   *
   * This strategy will use CPU resource to avoid syscalls which can introduce latency jitter.  It is best
   * used when threads can be bound to specific CPU cores.
   */
  class BusySpinStrategy : public WaitStrategy<Entry>
  {
  public:
    //@Override
    virtual long waitFor(const std::vector<Consumer*> & consumers,
                         RingBuffer<Entry> & ringBuffer,
                         ConsumerBarrier<Entry> & barrier,
                         const long & sequence)
      throw (AlertException)
    {
      long availableSequence;
      
      if (0 == consumers.size()) {
        while ((availableSequence = ringBuffer.getCursor()) < sequence) {
          if (barrier.isAlerted()) {
            throw AlertException::getInstance();
          }
        }
      }
      else {
        while ((availableSequence = getMinimumSequence(consumers)) < sequence) {
          if (barrier.isAlerted()) {
            throw AlertException::getInstance();
          }
        }
      }
      
      return availableSequence;
    }

    
    //@Override
    virtual long waitFor(const std::vector<Consumer*> & consumers,
                         RingBuffer<Entry> & ringBuffer,
                         ConsumerBarrier<Entry> & barrier,
                         const long & sequence,
                         const unsigned long & timeout_micros)
      throw (AlertException)
    {
      boost::timer timer;
      long availableSequence;
      
      if (0 == consumers.size()) {
        while ((availableSequence = ringBuffer.getCursor()) < sequence) {
          if (barrier.isAlerted()) {
            throw AlertException::getInstance();
          }
          
          if (timeout_micros < (timer.elapsed() * 1000000)) {
            break;
          }
        }
      }
      else {
        while ((availableSequence = getMinimumSequence(consumers)) < sequence) {
          if (barrier.isAlerted()) {
            throw AlertException::getInstance();
          }
          
          if (timeout_micros < (timer.elapsed() * 1000000)) {
            break;
          }
        }
      }
      
      return availableSequence;
    }

    
    //@Override
    virtual void signalAll() {}
  };
};

} //-- end namespace disruptor
} //-- end namespace throughput

#endif
