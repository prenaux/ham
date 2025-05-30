/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

#define USE_JSON_PROTOCOL

#include <iostream>

#ifdef USE_JSON_PROTOCOL
  #include <thrift/protocol/TJSONProtocol.h>
  #include <thrift/transport/THttpClient.h>
#else
  #include <thrift/protocol/TBinaryProtocol.h>
#endif
#include <thrift/transport/TSocket.h>
#include <thrift/transport/TTransportUtils.h>

#include "../gen-cpp/Calculator.h"

using namespace std;
using namespace apache::thrift;
using namespace apache::thrift::protocol;
using namespace apache::thrift::transport;

using namespace tutorial;
using namespace shared;

static constexpr int kPORT = 40990;

int main()
{
  std::shared_ptr<TTransport> socket(new TSocket("localhost", kPORT));
#ifdef USE_JSON_PROTOCOL
  std::shared_ptr<TTransport> transport(
    new THttpClient(socket, "localhost", "/service"));
  std::shared_ptr<TProtocol> protocol(new TJSONProtocol(transport));
#else
  std::shared_ptr<TTransport> transport(new TBufferedTransport(socket));
  std::shared_ptr<TProtocol> protocol(new TBinaryProtocol(transport));
#endif
  CalculatorClient client(protocol);

  try {
    transport->open();

    client.ping();
    cout << "ping()" << endl;

    cout << "1 + 1 = " << client.add(1, 1) << endl;

    Work work;
    work.op = Operation::DIVIDE;
    work.num1 = 1;
    work.num2 = 0;

    try {
      client.calculate(1, work);
      cout << "Whoa? We can divide by zero!" << endl;
    } catch (InvalidOperation& io) {
      cout << "InvalidOperation: " << io.why << endl;
      // or using generated operator<<: cout << io << endl;
      // or by using std::exception native method what(): cout << io.what() << endl;
    }

    work.op = Operation::SUBTRACT;
    work.num1 = 15;
    work.num2 = 10;
    int32_t diff = client.calculate(1, work);
    cout << "15 - 10 = " << diff << endl;

    // Note that C++ uses return by reference for complex types to avoid
    // costly copy construction
    SharedStruct ss;
    client.getStruct(ss, 1);
    cout << "Received log: " << ss << endl;

    transport->close();
    return 0;
  } catch (TException& tx) {
    cout << "ERROR: " << tx.what() << endl;
    return 1;
  }
}
