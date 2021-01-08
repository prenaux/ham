set(CMAKE_SYSTEM_NAME Windows)
set(CMAKE_SYSTEM_PROCESSOR x86_64)

if (WIN32)
  set(CLANG_BASE_PATH "C:/Program Files (x86)/Microsoft Visual Studio/2019/Community/VC/Tools/Llvm/bin")
  set(CLANG_EXEC_EXT ".exe")
elseif (APPLE)
  set(CLANG_BASE_PATH "/usr/local/Cellar/llvm/11.0.0/bin")
endif()

set(CMAKE_RC_COMPILER "${CLANG_BASE_PATH}/clang${CLANG_EXEC_EXT}")
set(CMAKE_C_COMPILER "${CLANG_BASE_PATH}/clang${CLANG_EXEC_EXT}")
set(CMAKE_CXX_COMPILER "${CLANG_BASE_PATH}/clang++${CLANG_EXEC_EXT}")

set(triple x86_64-pc-win32)
set(CMAKE_C_COMPILER_TARGET ${triple})
set(CMAKE_CXX_COMPILER_TARGET ${triple})

set(TOOLSET_BASE "/Users/Arantir/Work/ham/toolsets/msvc_19_x64/nt-x86/")
set(MSVC_BASE "${TOOLSET_BASE}/2019/BuildTools/VC/Tools/MSVC/14.27.29110")
set(WIN_SDK_BASE "${TOOLSET_BASE}/winsdk/10")
set(WIN_SDK_VERSION "10.0.18362.0")
set(WIN_SDK_INCLUDE ${WIN_SDK_BASE}/Include/${WIN_SDK_VERSION})
set(WIN_SDK_LIB ${WIN_SDK_BASE}/Lib/${WIN_SDK_VERSION})

link_directories("${MSVC_BASE}/lib/x64")
link_directories("${WIN_SDK_LIB}/ucrt/x64")
link_directories("${WIN_SDK_LIB}/um/x64")

include_directories(niLang SYSTEM
  "${MSVC_BASE}/include"
  "${WIN_SDK_INCLUDE}/ucrt"
  "${WIN_SDK_INCLUDE}/um"
  "${WIN_SDK_INCLUDE}/shared"
  "${WIN_SDK_INCLUDE}/winrt"
  "${WIN_SDK_INCLUDE}/cppwinrt"
  )
