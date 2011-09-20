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

#ifndef disruptor_BatchConsumer_h
#define disruptor_BatchConsumer_h

#include "RingBuffer.h"
#include "ExceptionHandler.h"

namespace throughput {
namespace disruptor {

template <class T> class BatchHandler;
template <class T> class SequenceTrackingHandler;
  
/**
 * Convenience class for handling the batching semantics of consuming entries from a {@link RingBuffer}
 * and delegating the available {@link AbstractEntry}s to a {@link BatchHandler}.
 *
 * If the {@link BatchHandler} also implements {@link LifecycleAware} it will be notified just after the thread
 * is started and just before the thread is shutdown.
 *
 * @param <T> Entry implementation storing the data for sharing during exchange or parallel coordination of an event.
 */
template <class Entry>
class BatchConsumer : public Consumer
{
private:
  ConsumerBarrier<Entry> * consumerBarrier_;
  BatchHandler<Entry>    * handler_;
  ExceptionHandler<Entry>* exceptionHandler_;
  
  long p1, p2, p3, p4, p5, p6, p7;  // cache line padding
  volatile bool running;
  long p8, p9, p10, p11, p12, p13, p14; // cache line padding
  volatile long sequence;
  long p15, p16, p17, p18, p19, p20; // cache line padding

public:
  /**
   * Construct a batch consumer that will automatically track the progress by updating its sequence when
   * the {@link BatchHandler#onAvailable(AbstractEntry)} method returns.
   *
   * @param consumerBarrier on which it is waiting.
   * @param handler is the delegate to which {@link AbstractEntry}s are dispatched.
   */
  BatchConsumer(ConsumerBarrier<Entry> & consumerBarrier,
                BatchHandler<Entry> & handler) :
    running(true),
    sequence(RINGBUFFER_INITIAL_CURSOR_VALUE),
    consumerBarrier_(&consumerBarrier),
    handler_(&handler)
  {
    exceptionHandler_ = NULL;
  }

  
  /**
   * Construct a batch consumer that will rely on the {@link SequenceTrackingHandler}
   * to callback via the {@link BatchConsumer.SequenceTrackerCallback} when it has
   * completed with a sequence within a batch.  Sequence will be updated at the end of
   * a batch regardless.
   *
   * @param consumerBarrier on which it is waiting.
   * @param entryHandler is the delegate to which {@link AbstractEntry}s are dispatched.
   */
  BatchConsumer(ConsumerBarrier<Entry> & consumerBarrier,
                SequenceTrackingHandler<Entry> & entryHandler) :
    running(true),
    sequence(RINGBUFFER_INITIAL_CURSOR_VALUE),
    consumerBarrier_(&consumerBarrier),
    handler_(&entryHandler)
  {
    entryHandler.setSequenceTrackerCallback(new SequenceTrackerCallback());
    exceptionHandler_ = NULL;
  }

  
  //@Override
  virtual long getSequence() const
  {
    return sequence;
  }

  //@Override
  virtual void halt()
  {
    running = false;
    consumerBarrier_->alert();
  }

  
  /**
   * Set a new {@link ExceptionHandler} for handling exceptions propagated out of the {@link BatchConsumer}
   *
   * @param exceptionHandler to replace the existing exceptionHandler.
   */
  void setExceptionHandler(ExceptionHandler<Entry>* exceptionHandler)
  {
    exceptionHandler_ = exceptionHandler;
  }
  
  /**
   * Get the {@link ConsumerBarrier} the {@link Consumer} is waiting on.
   *
   * @return the barrier this {@link Consumer} is using.
   */
  virtual ConsumerBarrier<Entry> & getConsumerBarrier()
  {
    return *consumerBarrier_;
  }
  
  /**
   * It is ok to have another thread rerun this method after a halt().
   */
  //@Override
  virtual void run()
  {
    
    long nextSequence = sequence + 1 ;
    Entry * entry;
    while (running)
    {
      try
      {
        long availableSequence = consumerBarrier_->waitFor(nextSequence);
        for (; nextSequence <= availableSequence; nextSequence++)
        {
          entry = consumerBarrier_->getEntry(nextSequence);
          handler_->onAvailable(*entry);
        }
        
        handler_->onEndOfBatch();
        sequence = entry->getSequence();
      }
      catch (AlertException ex)
      {
        // Wake up from blocking wait and check if we should continue to run
      }
      catch (std::exception ex)
      {
        if (exceptionHandler_ != NULL) {
          exceptionHandler_->handle(ex, *entry);
        }
        sequence = entry->getSequence();
        nextSequence = entry->getSequence() + 1;
      }
    }
  }

  
  /**
   * Used by the {@link BatchHandler} to signal when it has completed consuming a given sequence.
   */
  class SequenceTrackerCallback
  {
  public:
    SequenceTrackerCallback(BatchConsumer<Entry> *parent) :
      parent_(parent)
    {}
    
    /**
     * Notify that the handler has consumed up to a given sequence.
     *
     * @param sequence that has been consumed.
     */
    void onCompleted(const long & sequence)
    {
      parent_->sequence = sequence;
    }

  private:
    BatchConsumer<Entry> * parent_;
  };
};
  
  
  
} //-- end namespace disruptor
} //-- end namespace throughput

#endif
