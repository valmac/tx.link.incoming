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

#include "RingBuffer.h"
#include "BatchHandler.h"
#include "NoOpConsumer.h"
#include "ConsumerBarrier.h"
#include "ProducerBarrier.h"
#include "Consumer.h"
#include "BatchConsumer.h"
#include "AbstractEntry.h"

#include <boost/thread.hpp>

using namespace throughput::disruptor;

// Entry holder for data to be exchange that must extend AbstractEntry
class ValueEntry : public AbstractEntry
{
private:
  long value_;
  
public:
  long getValue()
  {
    return value_;
  }
  
  void setValue(const long & value)
  {
    value_ = value;
  }
};

// Callback handler which can be implemented by consumers
class MyBatchHandler : public throughput::disruptor::BatchHandler<ValueEntry>
{
public:
  virtual void onAvailable(ValueEntry & entry)
  {
    // process a new entry as it becomes available.
    std::cout << "Entry Value: " << entry.getValue() << std::endl;
    
  }
  
  virtual void onEndOfBatch()
  {
    // useful for flushing results to an IO device if necessary.
  }
  
  virtual void onCompletion()
  {
    // do any clean up before shutdown.
  }
};



int
main(int arc, char* argv[], char* argp[])
{
  RingBuffer<ValueEntry> ringBuffer(8, MULTI_THREADED, BLOCKING);
  
  std::vector<Consumer *> consumerList(0);
  ConsumerBarrier<ValueEntry> * consumerBarrier = ringBuffer.createConsumerBarrier(consumerList);
  
  MyBatchHandler batchHandler;
  BatchConsumer<ValueEntry> * batchConsumer = 
            new BatchConsumer<ValueEntry>(*consumerBarrier, batchHandler);
  
  std::vector<Consumer *> batchConsumers(1);
  batchConsumers[0] = batchConsumer;
  
  ProducerBarrier<ValueEntry> * producerBarrier = 
  ringBuffer.createProducerBarrier(batchConsumers);
  
  // Each consumer runs on a separate thread
  boost::thread consumerThread(boost::ref<Consumer>(*batchConsumer));
  

  for (int i=0; i <= 1023; ++i) {
    // Producers claim entries in sequence
    ValueEntry * entry = producerBarrier->nextEntry();

    entry->setValue(i);

    // make the entry available to consumers
    producerBarrier->commit(*entry);
    sleep(0);
  }

  sleep(1);
}
