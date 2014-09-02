set(LLVM_LINK_COMPONENTS
  Option
  Support
  )

add_clang_executable(another-ast-printer
  AnotherAstPrinter.cpp
  DeclPrinter.cpp
  DeclPrinter.h
  StmtPrinter.cpp
  StmtPrinter.h
  TypePrinter.cpp
  TypePrinter.h
  PrintingPolicy.h
  )

target_link_libraries(another-ast-printer
  clangAST
  clangBasic
  clangDriver
  clangFrontend
  clangRewriteFrontend
  clangStaticAnalyzerFrontend
  clangTooling
  )

add_subdirectory(testcases)

#install(TARGETS another-ast-printer
#  RUNTIME DESTINATION bin)
