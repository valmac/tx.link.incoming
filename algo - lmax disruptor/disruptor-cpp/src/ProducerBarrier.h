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

#ifndef disruptor_ProducerBarrier_h
#define disruptor_ProducerBarrier_h

namespace throughput {
namespace disruptor {

class SequenceBatch;

/**
 * Abstraction for claiming {@link AbstractEntry}s in a {@link RingBuffer} while tracking dependent {@link Consumer}s
 *
 * @param <T> {@link AbstractEntry} implementation stored in the {@link RingBuffer}
 */
template <class Entry>
class ProducerBarrier
{
public:
  /**
   * Claim the next {@link AbstractEntry} in sequence for a producer on the {@link RingBuffer}
   *
   * @return the claimed {@link AbstractEntry}
   */
  virtual Entry * nextEntry() = 0;
  
  /**
   * Claim the next batch of {@link AbstractEntry}s in sequence.
   *
   * @param sequenceBatch to be updated for the batch range.
   * @return the updated sequenceBatch.
   */
  virtual SequenceBatch nextEntries(SequenceBatch sequenceBatch) = 0;
  
  /**
   * Commit an entry back to the {@link RingBuffer} to make it visible to {@link Consumer}s
   * @param entry to be committed back to the {@link RingBuffer}
   */
  virtual void commit(const Entry & entry) = 0;
  
  /**
   * Commit the batch of entries back to the {@link RingBuffer}.
   *
   * @param sequenceBatch to be committed.
   */
  virtual void commit(SequenceBatch sequenceBatch) = 0;
  
  /**
   * Get the {@link AbstractEntry} for a given sequence from the underlying {@link RingBuffer}.
   *
   * @param sequence of the {@link AbstractEntry} to get.
   * @return the {@link AbstractEntry} for the sequence.
   */
  virtual Entry * getEntry(const long & sequence) = 0;
  
  /**
   * Delegate a call to the {@link RingBuffer#getCursor()}
   *
   * @return value of the cursor for entries that have been published.
   */
  virtual long getCursor() = 0;
};

  
} //-- end namespace disruptor
} //-- end namespace throughput



#endif
