#!/bin/bash
if [[ -z "$HAM_HOME" ]]; then
  echo "E/HAM_HOME not set !"
  exit 1
fi

###########################################################################
# LIBRARY
###########################################################################

# Define fixtures and tests
declare -A FIXTURES
declare -a TESTS
declare -a FAILED_TESTS

# usage: CHECK_NOT_EQUAL cond (desc)
#   CHECK macro to test boolean conditions
CHECK() {
  local condition=$1
  local desc=$2

  if ! $condition; then
    complain "CHECK" "CHECK FAILED: $desc"
    return 1
  fi

  return 0
}

# usage: CHECK_EQUAL expected value (desc)
#   CHECK_EQUAL macro to compare two values
CHECK_EQUAL() {
  local expected=$1
  local actual=$2
  local desc=$3

  if [[ "$expected" != "$actual" ]]; then
    if [ -n "$desc" ]; then
      complain "CHECK_EQUAL" "$desc"
    else
      complain "CHECK_EQUAL" "Expected both values to be equal."
    fi
    echo "EXPECTED: <<<$expected>>>"
    echo "  ACTUAL: <<<$actual>>>"
    return 1
  fi

  return 0
}

# usage: CHECK_NOT_EQUAL expected value (desc)
#   CHECK_NOT_EQUAL macro to compare two values
CHECK_NOT_EQUAL() {
  local expected=$1
  local actual=$2
  local desc=$3

  if [[ "$expected" == "$actual" ]]; then
    if [ -n "$desc" ]; then
      complain "CHECK_NOT_EQUAL" "$desc"
    else
      complain "CHECK_NOT_EQUAL" "Expected both values to be different."
    fi
    echo "EXPECTED: <<<$expected>>>"
    echo "  ACTUAL: <<<$actual>>>"
    return 1
  fi

  return 0
}

# usage: CHECK_CONTAINS expected value (desc)
#   CHECK_CONTAINS macro that the value contains the expected value
CHECK_CONTAINS() {
  local expected=$1
  local actual=$2
  local desc=$3

  if [[ "$actual" != *"$expected"* ]]; then
    if [ -n "$desc" ]; then
      complain "CHECK_CONTAINS" "$desc"
    else
      complain "CHECK_CONTAINS" "Expected value should be contained in the actual value."
    fi
    echo "EXPECTED: <<<$expected>>>"
    echo "  ACTUAL: <<<$actual>>>"
    return 1
  fi

  return 0
}

# usage: FIXTURE name
#   Define a fixture
FIXTURE() {
  local fixture_name=$1
  FIXTURES["$fixture_name"]=1
}

_TEST_VALIDATE() {
  local func=$1
  local fixture=$2
  local test_name=$3
  local full_name="$fixture:$test_name"

  # Check if fixture is defined
  if [[ -z "${FIXTURES[$fixture]}" ]]; then
    complain "$func" "'$full_name' fixture '$fixture' does not exist."
    exit 1
  fi

  # Check if test_name is provided and the corresponding function is defined
  if [[ -z "$test_name" ]]; then
    complain "$func" "'$full_name' test name is not specified."
    exit 1
  elif ! declare -f "$test_name" >/dev/null; then
    complain "$func" "'$full_name' test function '$test_name' does not exist."
    exit 1
  fi
}

# usage: TEST fixture_name test_and_func_name
#   Define a test within a fixture
TEST() {
  local fixture=$1
  local test_name=$2
  _TEST_VALIDATE TEST "$fixture" "$test_name" || return 1
  TESTS+=("$fixture:$test_name")
}

# usage: TEST_SKIP fixture_name test_and_func_name
#   Define a skipped test within a fixture
TEST_SKIP() {
  local fixture=$1
  local test_name=$2
  _TEST_VALIDATE TEST_SKIP "$fixture" "$test_name" || return 1
  TESTS+=("__skip__:$fixture:$test_name")
}

# usage: RUN_TESTS (filter)
#   Run tests, filtered by fixture or test name
RUN_TESTS() {
  local filter=$1
  local total=0
  local failed=0
  local succeeded=0
  local skipped=0

  for test in "${TESTS[@]}"; do
    if [[ -z "$filter" || "$test" == *"$filter"* ]]; then
      ((total++))

      if [[ "$test" == __skip__* ]]; then
        ((skipped++))
        local test_name
        test_name=$(echo "$test" | cut -d':' -f2-)
        local test_func_name
        test_func_name=$(echo "$test_name" | cut -d':' -f2-)
        if [[ "$test_name" == "$filter" || "$test_func_name" == "$filter" ]]; then
          log_info "Exact filter match of skipped test, force run it."
          test=$test_name
        else
          echo "### skipped: $test_name ###"
          continue
        fi
      fi

      local fixture
      fixture=$(echo "$test" | cut -d':' -f1)
      local test_func_name
      test_func_name=$(echo "$test" | cut -d':' -f2)
      echo "### $test ###"

      # Run the test by dynamically invoking the function
      eval "$test_func_name"
      local result=$?

      if [[ $result -eq 0 ]]; then
        log_success "SUCCEEDED '$test'"
        ((succeeded++))
      else
        log_error "FAILED '$test'"
        ((failed++))
        FAILED_TESTS+=("$test")
      fi
    fi
  done

  echo "### $total tests ###"
  if [[ "$total" == "0" ]]; then
    log_error "No test found for filter '$filter'."
    return 1
  elif [[ "$succeeded" == "0" ]]; then
    log_error "All failed, '$skipped' skipped."
    return 1
  elif [[ "$failed" == "0" ]]; then
    log_success "All succeeded, '$skipped' skipped."
    return 0
  else
    log_error "Some failed: '$failed' failed, '$succeeded' succeeded, '$skipped' skipped."
    echo "Failed tests:"
    for failed_test in "${FAILED_TESTS[@]}"; do
      echo "- $failed_test"
    done
    return 1
  fi
}

###########################################################################
# TESTING, run with:
#   bash "$HAM_HOME/bin/ham-bash-lib-unittest.sh" unittest-unittest
#   # Exact match of skipped test name force runs it
#   bash "$HAM_HOME/bin/ham-bash-lib-unittest.sh" unittest-unittest test_bad_subtraction
###########################################################################
if [[ "$1" == "unittest-unittest" ]]; then
  shift

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

fi # if [[ "$1" == "unittest" ]]
