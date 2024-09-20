#!/bin/bash
#
# run:
#   bash "$HAM_HOME/bin/ham-bash-lib-unittest-tests.sh"
#
if [[ -z "$HAM_HOME" ]]; then
  echo "E/HAM_HOME not set !"
  exit 1
fi
. "$HAM_HOME/bin/ham-bash-lib.sh"
. "$HAM_HOME/bin/ham-bash-lib-unittest.sh"

###########################################################################
# TESTS: unittest library usage examples
###########################################################################
FIXTURE "basic_math"

# shellcheck disable=SC2317
test_addition() {
  local result=$((2 + 2))
  log_info "... result: $result"
  CHECK_EQUAL 4 "$result" "2 + 2 should equal 4"
}
TEST "basic_math" "test_addition"

# shellcheck disable=SC2317
test_subtraction() {
  local result=$((5 - 3))
  log_info "... result: $result"
  CHECK_EQUAL 2 "$result" "5 - 3 should equal 2"
}
TEST "basic_math" "test_subtraction"

# shellcheck disable=SC2317
test_bad_subtraction() {
  local result=$((5 - 123))
  log_info "... result: $result"
  CHECK_EQUAL 2 "$result" "5 - 3 should equal 2"
}
TEST_SKIP "basic_math" "test_bad_subtraction"

###########################################################################
# RUN_TESTS
###########################################################################
RUN_TESTS "$1"
exit $?
