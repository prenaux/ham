#!/bin/bash -ex
ham-cppm-run thrift_cppm ham_thriftc -r --gen cpp --gen js --gen py --gen php --gen json --gen xml --gen markdown --gen html thrift/tutorial.thrift
