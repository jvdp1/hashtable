# Based on stdlib
macro(ADDTEST name)
    add_executable(test_${name} test_${name}.f90)
    target_link_libraries(test_${name} "${PROJECT_NAME}")
    add_test(NAME ${name}
             COMMAND $<TARGET_FILE:test_${name}> ${CMAKE_CURRENT_BINARY_DIR}
             WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
endmacro(ADDTEST)

ADDTEST(hash)
#ADDTEST(list)
ADDTEST(table)
