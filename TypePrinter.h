#ifndef _ANOTHER_TYPE_PRINTER_
#define _ANOTHER_TYPE_PRINTER_

#include "clang/AST/PrettyPrinter.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclObjC.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/Expr.h"
#include "clang/AST/Type.h"
#include "clang/Basic/LangOptions.h"
#include "clang/Basic/SourceManager.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/SaveAndRestore.h"
#include "llvm/Support/raw_ostream.h"

#include "PrintingPolicy.h"

using namespace clang;

namespace another_printer{
  /// \brief RAII object that enables printing of the ARC __strong lifetime
  /// qualifier.
  class IncludeStrongLifetimeRAII {
    another_printer::PrintingPolicy &Policy;
    bool Old;
    
  public:
    explicit IncludeStrongLifetimeRAII(another_printer::PrintingPolicy &Policy) 
      : Policy(Policy), Old(Policy.SuppressStrongLifetime) {
        if (!Policy.SuppressLifetimeQualifiers)
          Policy.SuppressStrongLifetime = false;
    }
    
    ~IncludeStrongLifetimeRAII() {
      Policy.SuppressStrongLifetime = Old;
    }
  };

  class ParamPolicyRAII {
    another_printer::PrintingPolicy &Policy;
    bool Old;
    
  public:
    explicit ParamPolicyRAII(another_printer::PrintingPolicy &Policy) 
      : Policy(Policy), Old(Policy.SuppressSpecifiers) {
      Policy.SuppressSpecifiers = false;
    }
    
    ~ParamPolicyRAII() {
      Policy.SuppressSpecifiers = Old;
    }
  };

  class ElaboratedTypePolicyRAII {
    another_printer::PrintingPolicy &Policy;
    bool SuppressTagKeyword;
    bool SuppressScope;
    
  public:
    explicit ElaboratedTypePolicyRAII(another_printer::PrintingPolicy &Policy) : Policy(Policy) {
      SuppressTagKeyword = Policy.SuppressTagKeyword;
      SuppressScope = Policy.SuppressScope;
      Policy.SuppressTagKeyword = true;
      Policy.SuppressScope = true;
    }
    
    ~ElaboratedTypePolicyRAII() {
      Policy.SuppressTagKeyword = SuppressTagKeyword;
      Policy.SuppressScope = SuppressScope;
    }
  };
  
  class TypePrinter {
    another_printer::PrintingPolicy Policy;
    bool HasEmptyPlaceHolder;
    bool InsideCCAttribute;

  public:
    explicit TypePrinter(const another_printer::PrintingPolicy &Policy)
      : Policy(Policy), HasEmptyPlaceHolder(false), InsideCCAttribute(false) { }

    void print(const Type *ty, Qualifiers qs, raw_ostream &OS,
               StringRef PlaceHolder);
    void print(QualType T, raw_ostream &OS, StringRef PlaceHolder);

    static bool canPrefixQualifiers(const Type *T, bool &NeedARCStrongQualifier);
    void spaceBeforePlaceHolder(raw_ostream &OS);
    void printTypeSpec(const NamedDecl *D, raw_ostream &OS);

    void printBefore(const Type *ty, Qualifiers qs, raw_ostream &OS);
    void printBefore(QualType T, raw_ostream &OS);
    void printAfter(const Type *ty, Qualifiers qs, raw_ostream &OS);
    void printAfter(QualType T, raw_ostream &OS);
    void AppendScope(DeclContext *DC, raw_ostream &OS);
    void printTag(TagDecl *T, raw_ostream &OS);
#define ABSTRACT_TYPE(CLASS, PARENT)
#define TYPE(CLASS, PARENT) \
    void print##CLASS##Before(const CLASS##Type *T, raw_ostream &OS); \
    void print##CLASS##After(const CLASS##Type *T, raw_ostream &OS);
#include "clang/AST/TypeNodes.def"
  };
 
}//namespace another_printer


#endif
