include(CheckIncludeFile)
include(CheckIncludeFileCXX)
include(CheckIncludeFiles)
include(ExternalProject)
include(CMakeParseArguments)

function(add_target name)
  get_property(current_project_name GLOBAL PROPERTY PROPERTY_PROJECT_NAME)
  include(niCMake/${current_project_name}/${name}.cmake)
endfunction()

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

macro(add_flags)
  foreach(arg ${ARGN})
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${arg}")
  endforeach()
  foreach(arg ${ARGN})
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${arg}")
  endforeach()
endmacro()

macro(project_init)
  message("-- Compiler: ${CMAKE_CXX_COMPILER_ID}")
  message("-- Build for system ${CMAKE_SYSTEM_NAME}")
  set_property(GLOBAL PROPERTY PROPERTY_PROJECT_NAME "${PROJECT_NAME}")
  set(NISDK_OUTPUT_DIR "${CMAKE_CURRENT_SOURCE_DIR}/${CMAKE_PROJECT_NAME}/bin/$ENV{CMAKE_BUILD_ARCH}")
  set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${NISDK_OUTPUT_DIR})
  set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${NISDK_OUTPUT_DIR})
  set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/${CMAKE_PROJECT_NAME}/libs/$ENV{CMAKE_BUILD_ARCH}")


  set(CMAKE_EXPORT_COMPILE_COMMANDS ON)  # enable compile_commands.json
  set(CMAKE_CXX_STANDARD 14) # using c++ 14

  # We need to use libtool on apple to stop warnings
  if(NOT CMAKE_LIBTOOL)
    find_program(CMAKE_LIBTOOL NAMES libtool)
  endif()

  if(CMAKE_LIBTOOL)
    set(CMAKE_LIBTOOL ${CMAKE_LIBTOOL} CACHE PATH "libtool executable")
    message(STATUS "Found libtool - ${CMAKE_LIBTOOL}")
    get_property(languages GLOBAL PROPERTY ENABLED_LANGUAGES)
    foreach(lang ${languages})
      set(CMAKE_${lang}_CREATE_STATIC_LIBRARY
        "${CMAKE_LIBTOOL} -static -o <TARGET> -a -no_warning_for_no_symbols <LINK_FLAGS> <OBJECTS> ")
    endforeach()
  endif()

  # Global flags
  if (CMAKE_CXX_COMPILER_ID STREQUAL "Clang" OR CMAKE_CXX_COMPILER_ID STREQUAL "AppleClang")
    add_c_flag(
      "-Wno-implicit-function-declaration"
      "-Wno-deprecated-declarations"
      "-Wno-tautological-pointer-compare"
      "-Wno-tautological-constant-out-of-range-compare"
      "-Wno-shift-negative-value"
      "-Wno-nonportable-include-path"
      "-Wno-non-literal-null-conversion"
      "-Wno-format"
      )
    add_cxx_flag(
      "-fno-common -fvisibility=hidden -ffunction-sections -fdata-sections"
      "-Wno-inconsistent-missing-override"
      "-Wno-ignored-attributes"
      "-Wno-switch"
      "-Wno-return-type-c-linkage"
      "-Wno-return-type"
      "-Wno-c++11-narrowing"
      "-Wno-format"
      "-Wno-uninitialized"
      "-Wno-int-to-pointer-cast"
      "-Wno-int-to-void-pointer-cast"
      "-Wno-unused-value"
      "-Wno-#pragma-messages"
      "-Wno-macro-redefined"
      "-Wno-pointer-to-int-cast"
      "-Wno-deprecated-declarations"
      "-Wno-tautological-pointer-compare"
      "-Wno-tautological-constant-out-of-range-compare"
      "-Wno-shift-negative-value"
      "-Wno-nonportable-include-path"
      "-Wno-non-literal-null-conversion"
      )
  elseif(CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
    add_cxx_flag("-Zm200 -Zc:forScope -W4 -EHa -D_HAS_EXCEPTIONS=1")
  elseif(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    message("gcc not supported yet..")
  endif()

endmacro()

macro(target_rename target)
  if (CMAKE_BUILD_TYPE STREQUAL "Release")
    set_target_properties(${target} PROPERTIES OUTPUT_NAME "${target}_ra")
  else()
    set_target_properties(${target} PROPERTIES OUTPUT_NAME "${target}_da")
  endif()
endmacro()

function(add_target_sources)
  if(NOT PARSED_ARGS_NAME)
    message(FATAL_ERROR "You must provide a name")
  endif()

  project(${PARSED_ARGS_NAME})

  if(NOT PARSED_ARGS_SRCS)
    set(PARSED_ARGS_SRCS "src")
  endif()

  get_property(current_project_name GLOBAL PROPERTY PROPERTY_PROJECT_NAME)
  if(NOT PARSED_ARGS_DIR)
    set(basedir ${CMAKE_CURRENT_SOURCE_DIR}/${current_project_name}/sources/${PARSED_ARGS_NAME})
  else()
    set(basedir ${CMAKE_CURRENT_SOURCE_DIR}/${current_project_name}/${PARSED_ARGS_DIR})
  endif()

  foreach(src ${PARSED_ARGS_SRCS})
    list_files(r ${basedir}/${src})
    set(all_src "${all_src}" "${r}")
  endforeach()

  foreach(arg ${PARSED_ARGS_FILES})
    set(all_src "${all_src}" "${basedir}/${arg}")
  endforeach()

  set(SOURCES "${all_src}" PARENT_SCOPE)
  set(BASEDIR "${basedir}" PARENT_SCOPE)
endfunction()

function(setup_target)
  foreach(src ${PARSED_ARGS_SRCS})
    target_include_directories(${PARSED_ARGS_NAME} PRIVATE ${BASEDIR}/${src})
  endforeach()

  foreach(inc ${PARSED_ARGS_INCS})
    target_include_directories(${PARSED_ARGS_NAME} PRIVATE ${BASEDIR}/${inc})
  endforeach()

  foreach(sys ${PARSED_ARGS_SYSTEM})
    target_include_directories(${PARSED_ARGS_NAME} SYSTEM PRIVATE ${BASEDIR}/${sys})
  endforeach()

  foreach(api ${PARSED_ARGS_APIS})
    target_include_directories(${PARSED_ARGS_NAME} PUBLIC ${BASEDIR}/${api})
  endforeach()

  target_link_libraries(${PARSED_ARGS_NAME} PUBLIC ${PARSED_ARGS_LIBS})
  target_compile_options(${PARSED_ARGS_NAME} PRIVATE ${PARSED_ARGS_FLAGS})
  target_rename(${PARSED_ARGS_NAME})
endfunction()

function(add_target_library)
  cmake_parse_arguments(
    PARSED_ARGS # prefix of output variables
    "STATIC" # list of names of the boolean arguments (only defined ones will be true)
    "NAME;DIR" # list of names of mono-valued arguments
    "SRCS;INCS;LIBS;FLAGS;APIS;SYSTEM;FILES" # list of names of multi-valued arguments (output variables are lists)
    ${ARGN} # arguments of the function to parse, here we take the all original ones
    )

  add_target_sources()

  if (PARSED_ARGS_STATIC)
    add_library(${PARSED_ARGS_NAME} STATIC ${SOURCES})
  else()
    add_library(${PARSED_ARGS_NAME} ${SOURCES})
  endif()

  if(NOT PARSED_ARGS_APIS)
    set(PARSED_ARGS_APIS "src/API")
  endif()

  set_property(TARGET ${PARSED_ARGS_NAME} PROPERTY POSITION_INDEPENDENT_CODE 1)
  set_property(TARGET ${PARSED_ARGS_NAME} PROPERTY ENABLE_EXPORTS 1)
  setup_target()

endfunction()

function(add_target_executable)
  cmake_parse_arguments(
    PARSED_ARGS # prefix of output variables
    "" # list of names of the boolean arguments (only defined ones will be true)
    "PKG;NAME;DIR" # list of names of mono-valued arguments
    "SRCS;INCS;LIBS;FLAGS;FILES" # list of names of multi-valued arguments (output variables are lists)
    ${ARGN} # arguments of the function to parse, here we take the all original ones
    )

  add_target_sources()

  add_executable(${PARSED_ARGS_NAME} ${SOURCES})

  setup_target()
endfunction()

function(add_target_test)
  cmake_parse_arguments(
    PARSED_ARGS # prefix of output variables
    "" # list of names of the boolean arguments (only defined ones will be true)
    "NAME" # list of names of mono-valued arguments
    "DEPS" # list of names of multi-valued arguments (output variables are lists)
    ${ARGN} # arguments of the function to parse, here we take the all original ones
    )

  set(TARGET_NAME "Test_${PARSED_ARGS_NAME}")
  add_target_executable(
    NAME ${TARGET_NAME}
    DIR "sources/${PARSED_ARGS_NAME}"
    SRCS "tsrc"
    LIBS niUnitTest niAppLib ${PARSED_ARGS_NAME} ${PARSED_ARGS_DEPS})

  target_rename(${TARGET_NAME})
endfunction(add_target_test)
