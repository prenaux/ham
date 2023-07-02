#!/bin/bash -ex
ham_thriftc_ra -r --gen cpp --gen js --gen py --gen php --gen json --gen xml --gen markdown --gen html thrift/tutorial.thrift
