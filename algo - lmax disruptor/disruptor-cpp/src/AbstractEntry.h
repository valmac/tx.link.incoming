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

#ifndef DISRUPTOR_ABSTRACT_ENTRY_H_
#define DISRUPTOR_ABSTRACT_ENTRY_H_

namespace throughput {
namespace disruptor {
  
/**
 * Base implementation that must be extended for {@link RingBuffer} entries.
 */
class AbstractEntry
{
public:

  /**
   * Get the sequence number assigned to this item in the series.
   *
   * @return the sequence number
   */
  virtual const long int getSequence() const
  {
    return sequence_;
  }
  
  /**
   * Explicitly set the sequence number for this Entry and a CommitCallback for
   * indicating when the producer is finished with assigning data for exchange.
   *
   * @param sequence to be assigned to this Entry
   */
  void setSequence(const long int sequence)
  {
    sequence_ = sequence;
  }

private:
  long int sequence_;
};

  
  
} //-- end namespace disruptor
} //-- end namespace throughput

#endif
