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

#ifndef disruptor_BatchHandler_h
#define disruptor_BatchHandler_h


namespace throughput {
namespace disruptor {
  
/**
 * Callback interface to be implemented for processing {@link AbstractEntry}s as they become available in the {@link RingBuffer}
 *
 * @see BatchConsumer#setExceptionHandler(ExceptionHandler) if you want to handle exceptions propigated out of the handler.
 *
 * @param <T> AbstractEntry implementation storing the data for sharing during exchange or parallel coordination of an event.
 */
template <class Entry>
class BatchHandler
{
public:
  /**
   * Called when a publisher has committed an {@link AbstractEntry} to the {@link RingBuffer}
   *
   * @param entry committed to the {@link RingBuffer}
   * @throws Exception if the BatchHandler would like the exception handled further up the chain.
   */
  virtual void onAvailable(Entry & entry) = 0;
  
  /**
   * Called after each batch of items has been have been processed before the next waitFor call on a {@link ConsumerBarrier}.
   * <p>
   * This can be taken as a hint to do flush type operations before waiting once again on the {@link ConsumerBarrier}.
   * The user should not expect any pattern or frequency to the batch size.
   *
   * @throws Exception if the BatchHandler would like the exception handled further up the chain.
   */
  virtual void onEndOfBatch() = 0;
};

  
} //-- end namespace disruptor
} //-- end namespace throughput



#endif
