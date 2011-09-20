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

#include "ClaimStrategy.h"
#include "WaitStrategy.h"
#include "ConsumerBarrier.h"
#include "ProducerBarrier.h"
#include "RingBufferInitialCursorValue.h"
#include "SequenceBatch.h"
#include "AbstractEntry.h"
#include "Util.h"

#include <vector>
#include <boost/static_assert.hpp>
#include <boost/type_traits.hpp>


#ifndef DISRUPTOR_RINGBUFFER_H_
#define DISRUPTOR_RINGBUFFER_H_

namespace throughput {
namespace disruptor {
  
using namespace boost;
  
// Forward decleration of Entry class
template <class T> class ForceFillProducerBarrier;
class SequenceBatch;

/**
  * Ring based store of reusable entries containing the data representing an {@link Entry} 
  * being exchanged between producers and consumers.
  *
  * @param t is the data storage class you wish to use.
  *
  */
template <class Entry>
class RingBuffer
{
  BOOST_STATIC_ASSERT(( is_base_of<AbstractEntry, Entry>::value ));

  public:
  
  /**
   * Construct a RingBuffer with the full option set.
   *
   * @param size of the RingBuffer that will be rounded up to the next power of 2
   * @param claimStrategyOption threading strategy for producers claiming {@link AbstractEntry}s in the ring.
   * @param waitStrategyOption waiting strategy employed by consumers waiting on {@link AbstractEntry}s becoming available.
   */
   RingBuffer(const int & size,
              const ClaimStrategyOption & claimStrategyOption,
              const WaitStrategyOption  & waitStrategyOption)  : 
    claimStrategy(*ClaimStrategy::newInstance(claimStrategyOption)),
    waitStrategy(*WaitStrategy<Entry>::newInstance(waitStrategyOption)),
    claimStrategyOption_(claimStrategyOption),
    waitStrategyOption_(waitStrategyOption),
    entries(ceilingNextPowerOfTwo(size)),
    cursor(RINGBUFFER_INITIAL_CURSOR_VALUE)
  {
    int sizeAsPowerOfTwo = ceilingNextPowerOfTwo(size);
    ringModMask = sizeAsPowerOfTwo - 1;
  };

  
  
  /**
   * Construct a RingBuffer with default strategies of:
   * {@link ClaimStrategy.Option#MULTI_THREADED} and {@link WaitStrategy.Option#BLOCKING}
   *
   * @param entryFactory to create {@link AbstractEntry}s for filling the RingBuffer
   * @param size of the RingBuffer that will be rounded up to the next power of 2
   */
  RingBuffer(const int & size) :
    claimStrategy(*ClaimStrategy::newInstance(MULTI_THREADED)),
    waitStrategy(*WaitStrategy<Entry>::newInstance(BLOCKING)),
    claimStrategyOption_(MULTI_THREADED),
    waitStrategyOption_(BLOCKING),
    entries(ceilingNextPowerOfTwo(size)),
    cursor(RINGBUFFER_INITIAL_CURSOR_VALUE)
  {
    int sizeAsPowerOfTwo = ceilingNextPowerOfTwo(size);
    ringModMask = sizeAsPowerOfTwo - 1;
  }

  
  /**
   * Create a {@link ConsumerBarrier} that gates on the RingBuffer
   * and a list of {@link Consumer}s
   *
   * @param consumersToTrack this barrier will track
   * @return the barrier gated as required
   */
  ConsumerBarrier<Entry> * createConsumerBarrier(std::vector<Consumer*> consumersToTrack)
  {
      return new ConsumerTrackingConsumerBarrier(this, consumersToTrack);
  }

  /**
   * Create a {@link ProducerBarrier} on this RingBuffer that tracks dependent {@link Consumer}s.
   *
   * @param consumersToTrack to be tracked to prevent wrapping.
   * @return a {@link ProducerBarrier} with the above configuration.
   */
  ProducerBarrier<Entry> * createProducerBarrier(std::vector<Consumer*> consumersToTrack)
  {
    return new ConsumerTrackingProducerBarrier(this, consumersToTrack);
  }
  
  /**
   * Create a {@link ForceFillProducerBarrier} on this RingBuffer that
   * tracks dependent {@link Consumer}s.
   * 
   * This barrier is to be used for filling a RingBuffer when no other producers exist.
   *
   * @param consumersToTrack to be tracked to prevent wrapping.
   * @return a {@link ForceFillProducerBarrier} with the above configuration.
   */
  ForceFillProducerBarrier<Entry> * createForceFillProducerBarrier(std::vector<Consumer*> consumersToTrack)
  {
    return new ForceFillConsumerTrackingProducerBarrier(this, consumersToTrack);
  }


  /**
   * The capacity of the RingBuffer to hold entries.
   *
   * @return the size of the RingBuffer.
   */
  int getCapacity()
  {
    return entries.size();
  }
  
  /**
   * Get the current sequence that producers have committed to the RingBuffer.
   *
   * @return the current committed sequence.
   */
  long getCursor()
  {
    return cursor;
  }

  /**
   * Get the {@link AbstractEntry} for a given sequence in the RingBuffer.
   *
   * @param sequence for the {@link AbstractEntry}
   * @return {@link AbstractEntry} for the sequence
   */
  //@SuppressWarnings("unchecked")
  virtual Entry & getEntry(const long & sequence)
  {
    return entries[(int)sequence & ringModMask];
  }

  
  private:
    long int _p1_, _p2_, _p3_, _p4_, _p5_, _p6_, _p7_; // cache line padding
    volatile long int cursor;
    long int _p8_, _p9_, _p10_, _p11_, _p12_, _p13_, _p14_; // cache line padding

    std::vector<Entry> entries;
    int ringModMask;

    ClaimStrategy & claimStrategy;
    WaitStrategy<Entry> & waitStrategy;
    const ClaimStrategyOption claimStrategyOption_;
    const WaitStrategyOption waitStrategyOption_;
  

  
  /**
   * ConsumerBarrier handed out for gating consumers of the RingBuffer and dependent {@link Consumer}(s)
   */
  class ConsumerTrackingConsumerBarrier : public ConsumerBarrier<Entry>
  {
  public:
    ConsumerTrackingConsumerBarrier(RingBuffer<Entry> * parent,
                                    std::vector<Consumer*> consumers) :
    alerted(false), consumers_(consumers), parent_(parent)
    { }
    
    //@Override
    //@SuppressWarnings("unchecked")
    virtual Entry * getEntry(const long & sequence)
    {
      return &(parent_->entries[(int)sequence & parent_->ringModMask]);
    }
    
    //@Override
    virtual long waitFor(const long & sequence)
      throw (AlertException)
    {
      return parent_->waitStrategy.waitFor(consumers_, *parent_, *this, sequence);
    }
    
    //@Override
    virtual long waitFor(const long & sequence, const long & timeout_micros)
      throw(AlertException)
    {
      return parent_->waitStrategy.waitFor(consumers_, *parent_, *this, sequence, timeout_micros);
    }
    
    //@Override
    virtual long getCursor() const
    {
      return parent_->cursor;
    }
    
    //@Override
    virtual bool isAlerted() const
    {
      return alerted;
    }
    
    //@Override
    virtual void alert()
    {
      alerted = true;
      parent_->waitStrategy.signalAll();
    }
    
    //@Override
    virtual void clearAlert()
    {
      alerted = false;
    }
    
  private:
    volatile bool alerted;
    std::vector<Consumer*> consumers_;
    RingBuffer<Entry> * parent_;
  };
  
  /**
   * {@link ProducerBarrier} that tracks multiple {@link Consumer}s when trying to claim
   * an {@link AbstractEntry} in the {@link RingBuffer}.
   */
  class ConsumerTrackingProducerBarrier : public ProducerBarrier<Entry>
  {
  public:
    ConsumerTrackingProducerBarrier(RingBuffer<Entry> * parent,
                                    std::vector<Consumer*> consumers)
    : consumers_(consumers),
      parent_(parent),
      lastConsumerMinimum(RINGBUFFER_INITIAL_CURSOR_VALUE)
    {
      if (0 == consumers.size())      {
        //throw new IllegalArgumentException("There must be at least one Consumer to track for preventing ring wrap");
#warning THROW AN APPROPRIATE EXCEPTION
        exit(37);
      }
    }
    
    //@Override
    //@SuppressWarnings("unchecked")
    virtual Entry * nextEntry()
    {
      const long sequence = parent_->claimStrategy.incrementAndGet();
      ensureConsumersAreInRange(sequence);
      
      Entry * entry = &(parent_->entries[(int)sequence & parent_->ringModMask]);
      entry->setSequence(sequence);
      
      return entry;
    }
    
    //@Override
    virtual void commit(const Entry & entry)
    {
      commit(entry.getSequence(), 1);
    }
    
    //@Override
    virtual SequenceBatch nextEntries(SequenceBatch sequenceBatch)
    {
      const long sequence = parent_->claimStrategy.incrementAndGet(sequenceBatch.getSize());
      sequenceBatch.setEnd(sequence);
      ensureConsumersAreInRange(sequence);
      
      for (long i = sequenceBatch.getStart(), end = sequenceBatch.getEnd(); i <= end; i++)
      {
        Entry entry = parent_->entries[(int)i & parent_->ringModMask];
        entry.setSequence(i);
      }
      
      return sequenceBatch;
    }
    
    
    //@Override
    virtual void commit(SequenceBatch sequenceBatch)
    {
      commit(sequenceBatch.getEnd(), sequenceBatch.getSize());
    }
    
    //@Override
    //@SuppressWarnings("unchecked")
    virtual Entry * getEntry(const long & sequence)
    {
      return &(parent_->entries[(int)sequence & parent_->ringModMask]);
    }
    
    //@Override
    virtual long getCursor()
    {
      return parent_->cursor;
    }
    
    
  private:
    std::vector<Consumer*> consumers_;
    RingBuffer<Entry> * parent_;
    long lastConsumerMinimum;    
    
    void ensureConsumersAreInRange(const long & sequence)
    {
      const long wrapPoint = sequence - parent_->entries.size();
      while (wrapPoint > lastConsumerMinimum &&
             wrapPoint > (lastConsumerMinimum = getMinimumSequence(consumers_)))
      {
        // yield the thread
        sleep(0);
      }
    }
    
    void commit(const long & sequence, const long & batchSize)
    {
      if (MULTI_THREADED == parent_->claimStrategyOption_)
      {
        const long expectedSequence = sequence - batchSize;
        while (expectedSequence != parent_->cursor)
        {
          // busy spin
        }
      }
      
      parent_->cursor = sequence;
      parent_->waitStrategy.signalAll();
    }
    
  };
  
  /**
   * {@link ForceFillProducerBarrier} that tracks multiple {@link Consumer}s when trying to claim
   * a {@link AbstractEntry} in the {@link RingBuffer}.
   */
  class ForceFillConsumerTrackingProducerBarrier : public ForceFillProducerBarrier<Entry>
  {
  public:
    ForceFillConsumerTrackingProducerBarrier(RingBuffer<Entry> * parent,
                                             std::vector<Consumer*> consumers)
    : consumers_(consumers),
    parent_(parent),
    lastConsumerMinimum(RINGBUFFER_INITIAL_CURSOR_VALUE)
    {
      if (0 == consumers.size())      {
        //throw new IllegalArgumentException("There must be at least one Consumer to track for preventing ring wrap");
        exit(35);
#warning THROW A BETTER EXCEPTION
      }
    }
    
    //@Override
    //@SuppressWarnings("unchecked")
    virtual Entry & claimEntry(const long & sequence)
    {
      ensureConsumersAreInRange(sequence);
      
      Entry entry = entries[(int)sequence & ringModMask];
      entry.setSequence(sequence);
      
      return entry;
    }
    
    //@Override
    virtual void commit(Entry & entry)
    {
      long sequence = entry.getSequence();
      claimStrategy.setSequence(sequence);
      cursor = sequence;
      waitStrategy.signalAll();
    }
    
    //@Override
    virtual long getCursor()
    {
      return cursor;
    }
    
    
  private:
    std::vector<Consumer*> consumers_;
    RingBuffer<Entry> * parent_;
    long lastConsumerMinimum;    
    
    void ensureConsumersAreInRange(const long & sequence)
    {
      const long wrapPoint = sequence - entries.length;
      while (wrapPoint > lastConsumerMinimum &&
             wrapPoint > (lastConsumerMinimum = getMinimumSequence(consumers_)))
      {
        // yield the thread
        sleep(0);
      }
    }
  };

};

} //-- end namespace disruptor
} //-- end namespace throughput
#endif