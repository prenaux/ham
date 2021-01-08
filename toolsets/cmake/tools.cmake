include(CheckIncludeFile)
include(CheckIncludeFileCXX)
include(CheckIncludeFiles)
include(ExternalProject)
include(CMakeParseArguments)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)  # enable compile_commands.json
set(CMAKE_CXX_STANDARD 14) # using c++ 14

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/libs)

set(NISDK_OUTPUT_DIR "${CMAKE_CURRENT_SOURCE_DIR}/bin/$ENV{CMAKE_BUILD_ARCH}")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${NISDK_OUTPUT_DIR})
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${NISDK_OUTPUT_DIR})

# if(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
# endif()

macro(get_all_targets_recursive targets dir)
  get_property(subdirectories DIRECTORY ${dir} PROPERTY SUBDIRECTORIES)
  foreach(subdir ${subdirectories})
    get_all_targets_recursive(${targets} ${subdir})
  endforeach()

  get_property(current_targets DIRECTORY ${dir} PROPERTY BUILDSYSTEM_TARGETS)
  list(APPEND ${targets} ${current_targets})
endmacro()

function(list_all_targets)
  set(targets)
  get_all_targets_recursive(targets ${CMAKE_CURRENT_SOURCE_DIR})
  message("-- CURRENT TARGETS --")
  foreach (arg ${targets})
    message("-- ${arg}")
  endforeach()
endfunction()

macro(set_runtime_output_dirs dir)
  set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_DEBUG          ${CMAKE_BINARY_DIR}/${dir}/Debug)
  set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_RELEASE        ${CMAKE_BINARY_DIR}/${dir}/Release)
  set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_RELWITHDEBINFO ${CMAKE_BINARY_DIR}/${dir}/RelWithDebInfo)
  set(CMAKE_RUNTIME_OUTPUT_DIRECTORY_MINSIZEREL     ${CMAKE_BINARY_DIR}/${dir}/MinSizeRel)
endmacro()

macro(set_library_output_dirs dir)
  set(CMAKE_LIBRARY_OUTPUT_DIRECTORY_DEBUG          ${CMAKE_BINARY_DIR}/${dir}/Debug)
  set(CMAKE_LIBRARY_OUTPUT_DIRECTORY_RELEASE        ${CMAKE_BINARY_DIR}/${dir}/Release)
  set(CMAKE_LIBRARY_OUTPUT_DIRECTORY_RELWITHDEBINFO ${CMAKE_BINARY_DIR}/${dir}/RelWithDebInfo)
  set(CMAKE_LIBRARY_OUTPUT_DIRECTORY_MINSIZEREL     ${CMAKE_BINARY_DIR}/${dir}/MinSizeRel)
  if (${CMAKE_BUILD_TYPE} STREQUAL "Debug")
    link_directories(${CMAKE_LIBRARY_OUTPUT_DIRECTORY_DEBUG})
  elseif (${CMAKE_BUILD_TYPE} STREQUAL "Release")
    link_directories(${CMAKE_LIBRARY_OUTPUT_DIRECTORY_RELEASE})
  elseif (${CMAKE_BUILD_TYPE} STREQUAL "RelWithDebInfo")
    link_directories(${CMAKE_LIBRARY_OUTPUT_DIRECTORY_RELWITHDEBINFO})
  elseif (${CMAKE_BUILD_TYPE} STREQUAL "MinSizeRel")
    link_directories(${CMAKE_LIBRARY_OUTPUT_DIRECTORY_MINSIZEREL})
  endif()
endmacro()

macro(set_archive_output_dirs dir)
  set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY_DEBUG          ${CMAKE_BINARY_DIR}/${dir}/Debug)
  set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY_RELEASE        ${CMAKE_BINARY_DIR}/${dir}/Release)
  set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY_RELWITHDEBINFO ${CMAKE_BINARY_DIR}/${dir}/RelWithDebInfo)
  set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY_MINSIZEREL     ${CMAKE_BINARY_DIR}/${dir}/MinSizeRel)
  if (${CMAKE_BUILD_TYPE} STREQUAL "Debug")
    link_directories(${CMAKE_ARCHIVE_OUTPUT_DIRECTORY_DEBUG})
  elseif (${CMAKE_BUILD_TYPE} STREQUAL "Release")
    link_directories(${CMAKE_ARCHIVE_OUTPUT_DIRECTORY_RELEASE})
  elseif (${CMAKE_BUILD_TYPE} STREQUAL "RelWithDebInfo")
    link_directories(${CMAKE_ARCHIVE_OUTPUT_DIRECTORY_RELWITHDEBINFO})
  elseif (${CMAKE_BUILD_TYPE} STREQUAL "MinSizeRel")
    link_directories(${CMAKE_ARCHIVE_OUTPUT_DIRECTORY_MINSIZEREL})
  endif()
endmacro()

function(list_files ret dir)
  file(GLOB _tmp
    "${dir}/*.cpp"
    "${dir}/*.hpp"
    "${dir}/*.c"
    "${dir}/*.h"
    "${dir}/*.cc"
    "${dir}/*.hh"
    "${dir}/*.mm")
  set(${ret} ${_tmp} PARENT_SCOPE)
endfunction()

macro(add_thirdparty_dir dir)
  set(MESSAGE_QUIET ON)
  add_subdirectory(thirdparty/${dir})
  set(MESSAGE_QUIET OFF)
endmacro()

macro(add_include)
  foreach(arg ${ARGN})
    include_directories(${CMAKE_CURRENT_SOURCE_DIR}/${arg})
  endforeach()
endmacro()

macro(target_add_include)
  foreach(arg ${ARGN})
    foreach(src ${PARSED_ARGS_SRCS})
      target_include_directories(${arg} PRIVATE ${basedir}/${src})
    endforeach()

    foreach(inc ${PARSED_ARGS_APIS})
      target_include_directories(${arg} PUBLIC ${basedir}/${inc})
    endforeach()
  endforeach()
endmacro()

macro(add_c_flag)
  foreach(arg ${ARGN})
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${arg}")
  endforeach()
endmacro()

macro(add_cxx_flag)
  foreach(arg ${ARGN})
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${arg}")
  endforeach()
endmacro()

function(my_add_executable _target)
  foreach(_source IN ITEMS ${ARGN})
    if (NOT TARGET "${_source}")
      list(APPEND _source_list "${_source}")
    else()
      get_target_property(_type "${_source}" TYPE)
      if (_type STREQUAL "OBJECT_LIBRARY")
        list(APPEND _source_list "$<TARGET_OBJECTS:${_source}>")
      else()
        message(SEND_ERROR "my_add_executable: '${_source}' given as parameter is not a object-library target.")
      endif()
    endif()
  endforeach()
  add_executable(${_target} ${_source_list})
endfunction(my_add_executable)

function(add_target_library)
  cmake_parse_arguments(
    PARSED_ARGS # prefix of output variables
    "" # list of names of the boolean arguments (only defined ones will be true)
    "NAME;DIR" # list of names of mono-valued arguments
    "SRCS;INCS;LIBS;FLAGS;APIS;FILES" # list of names of multi-valued arguments (output variables are lists)
    ${ARGN} # arguments of the function to parse, here we take the all original ones
    )
  # # note: if it remains unparsed arguments, here, they can be found in variable PARSED_ARGS_UNPARSED_ARGUMENTS
  if(NOT PARSED_ARGS_NAME)
    message(FATAL_ERROR "You must provide a name")
  endif(NOT PARSED_ARGS_NAME)
  project(${PARSED_ARGS_NAME})

  if(NOT PARSED_ARGS_SRCS)
    set(PARSED_ARGS_SRCS "src")
  endif()

  if(NOT PARSED_ARGS_APIS)
    set(PARSED_ARGS_APIS "src/API")
  endif()

  if(NOT PARSED_ARGS_DIR)
    set(basedir ${CMAKE_CURRENT_SOURCE_DIR}/sources/${PARSED_ARGS_NAME})
  else()
    set(basedir ${PARSED_ARGS_DIR})
  endif()

  foreach(src ${PARSED_ARGS_SRCS})
    list_files(r ${basedir}/${src})
    set(all_src "${all_src}" "${r}")
  endforeach()

  foreach(arg ${PARSED_ARGS_FILES})
    set(all_src "${all_src}" "${basedir}/${arg}")
  endforeach()

  add_library(${PARSED_ARGS_NAME} ${all_src})

  # shared libraries need PIC
  set_property(TARGET ${PARSED_ARGS_NAME} PROPERTY POSITION_INDEPENDENT_CODE 1)
  set_property(TARGET ${PARSED_ARGS_NAME} PROPERTY ENABLE_EXPORTS 1)
  # # set_property(TARGET ${PARSED_ARGS_NAME} PROPERTY STATIC_LIBRARY_FLAGS "-no_warning_for_no_symbols")

  target_compile_options(${PARSED_ARGS_NAME} PRIVATE ${PARSED_ARGS_FLAGS})

  foreach(src ${PARSED_ARGS_SRCS})
    target_include_directories(${PARSED_ARGS_NAME} PRIVATE ${basedir}/${src})
  endforeach()

  foreach(inc ${PARSED_ARGS_INCS})
    target_include_directories(${PARSED_ARGS_NAME} PRIVATE ${basedir}/${inc})
  endforeach()

  foreach(api ${PARSED_ARGS_APIS})
    target_include_directories(${PARSED_ARGS_NAME} PUBLIC ${basedir}/${api})
  endforeach()

  target_link_libraries(${PARSED_ARGS_NAME} PUBLIC ${PARSED_ARGS_LIBS})
  target_compile_options(${PARSED_ARGS_NAME} PRIVATE ${PARSED_ARGS_FLAGS})
endfunction()

function(add_target_executable)
    cmake_parse_arguments(
    PARSED_ARGS # prefix of output variables
    "" # list of names of the boolean arguments (only defined ones will be true)
    "NAME;DIR" # list of names of mono-valued arguments
    "SRCS;INCS;LIBS;FLAGS;FILES" # list of names of multi-valued arguments (output variables are lists)
    ${ARGN} # arguments of the function to parse, here we take the all original ones
    )
  # # note: if it remains unparsed arguments, here, they can be found in variable PARSED_ARGS_UNPARSED_ARGUMENTS
  if(NOT PARSED_ARGS_NAME)
    message(FATAL_ERROR "You must provide a name")
  endif(NOT PARSED_ARGS_NAME)
  project(${PARSED_ARGS_NAME})

  if(NOT PARSED_ARGS_SRCS)
    set(PARSED_ARGS_SRCS "src")
  endif()

  if(NOT PARSED_ARGS_DIR)
    set(basedir ${CMAKE_CURRENT_SOURCE_DIR}/sources/${PARSED_ARGS_NAME})
  else()
    set(basedir ${PARSED_ARGS_DIR})
  endif()

  foreach(src ${PARSED_ARGS_SRCS})
    list_files(r ${basedir}/${src})
    set(all_src "${all_src}" "${r}")
  endforeach()

  foreach(arg ${PARSED_ARGS_FILES})
    set(all_src "${all_src}" "${basedir}/${arg}")
  endforeach()

  add_executable(${PARSED_ARGS_NAME} ${all_src})

  target_compile_options(${PARSED_ARGS_NAME} PRIVATE ${PARSED_ARGS_FLAGS})

  foreach(src ${PARSED_ARGS_SRCS})
    target_include_directories(${PARSED_ARGS_NAME} PRIVATE ${basedir}/${src})
  endforeach()

  foreach(inc ${PARSED_ARGS_INCS})
    target_include_directories(${PARSED_ARGS_NAME} PRIVATE ${basedir}/${inc})
  endforeach()

  target_link_libraries(${PARSED_ARGS_NAME} PRIVATE ${PARSED_ARGS_LIBS})
  target_compile_options(${PARSED_ARGS_NAME} PRIVATE ${PARSED_ARGS_FLAGS})
endfunction()

function(add_target_test NAME)
  # # note: if it remains unparsed arguments, here, they can be found in variable PARSED_ARGS_UNPARSED_ARGUMENTS
  if(NOT NAME)
    message(FATAL_ERROR "You must provide a name")
  endif(NOT NAME)

  set(DEPS niUnitTest ${NAME})

  if (TARGET niAppLib)
    set(DEPS ${DEPS} niAppLib)
  endif()

  add_target_executable(
    NAME "Test_${NAME}"
    DIR "${CMAKE_CURRENT_SOURCE_DIR}/sources/${NAME}"
    SRCS "tsrc"
    LIBS ${DEPS})
endfunction(add_target_test)
