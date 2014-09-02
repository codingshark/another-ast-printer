set(LLVM_LINK_COMPONENTS
  Option
  Support
  )

add_clang_executable(another-ast-printer-test
main.cpp
  )

target_link_libraries(another-ast-printer-test
  clangAST
  clangBasic
  clangDriver
  clangFrontend
  clangRewriteFrontend
  clangStaticAnalyzerFrontend
  clangTooling
  )

