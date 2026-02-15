# Based on stdlib
macro(ADDTEST name)
    add_executable(test_${name} test_${name}.f90)
    target_link_libraries(test_${name} "${PROJECT_NAME}")
    add_test(NAME ${name}
             COMMAND $<TARGET_FILE:test_${name}>
             WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
endmacro(ADDTEST)

macro(ADDTESTFYPP name)
    fypp_f90("${fyppFlags}" "test_${name}.fypp" outname)
    add_executable(test_${name} ${outname})
    target_link_libraries(test_${name} "${PROJECT_NAME}")
    add_test(NAME ${name}
             COMMAND $<TARGET_FILE:test_${name}>
             WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
endmacro(ADDTESTFYPP)

ADDTEST(hash)
#ADDTEST(list)
ADDTEST(table)
ADDTESTFYPP(table_stream)

if(HT_HDF5_ENABLE)
  ADDTESTFYPP(table_hdf5)
endif()
