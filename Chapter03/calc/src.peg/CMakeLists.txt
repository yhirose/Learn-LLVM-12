add_executable (pegcalc Calc.cpp)
target_include_directories(pegcalc PRIVATE ../../../libs)
target_link_libraries(pegcalc PRIVATE ${llvm_libs})
