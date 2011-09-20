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

#ifndef disruptor_ExceptionHandler_h
#define disruptor_ExceptionHandler_h

namespace throughput {
namespace disruptor {
    
template <class T> class Entry;

/**
 * Callback handler for uncaught exceptions in the {@link AbstractEntry} processing cycle of the {@link BatchConsumer}
 */
template <class Entry>
class ExceptionHandler
{
public:
  /**
   * Strategy for handling uncaught exceptions when processing an {@link AbstractEntry}.
   *
   * If the strategy wishes to suspend further processing by the {@link BatchConsumer}
   * then is should throw a {@link RuntimeException}.
   *
   * @param ex the exception that propagated from the {@link BatchHandler}
   * @param currentEntry being processed when the exception occurred.
   */
  virtual void handle(std::exception ex, const Entry & currentEntry)=0;
};
  
} //-- end namespace disruptor
} //-- end namespace throughput

#endif
