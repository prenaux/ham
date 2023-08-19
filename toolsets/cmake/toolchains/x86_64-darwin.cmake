set(CMAKE_SYSTEM_NAME Darwin)
set(CMAKE_SYSTEM_PROCESSOR x86_64)
include($ENV{HAM_CMAKE_HOME}/tools.cmake)
add_flags("-m64" "-DOSX" "-mmacosx-version-min=10.14")
