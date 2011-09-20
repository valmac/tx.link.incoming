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

#ifndef disruptor_Consumer_h
#define disruptor_Consumer_h

namespace throughput {
namespace disruptor {

class Consumer {
public:
  /**
    * Get the sequence up to which this Consumer has consumed {@link AbstractEntry}s
    *
    * @return the sequence of the last consumed {@link AbstractEntry}
    */
  virtual long getSequence() const = 0;
    
  /**
    * Signal that this Consumer should stop when it has finished consuming at the next clean break.
    * It will call {@link ConsumerBarrier#alert()} to notify the thread to check status.
    */
  virtual void halt() = 0;
  

  /**
    * This is a runnable class, is the thread entry point.
    */
  virtual void run() = 0;
  
  /**
   
   */
  void operator()() { run(); }
};
  
  
} //-- end namespace disruptor
} //-- end namespace throughput



#endif
