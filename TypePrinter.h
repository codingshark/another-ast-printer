#ifndef _ANOTHER_TYPE_PRINTER_
#define _ANOTHER_TYPE_PRINTER_

#include "clang/AST/PrettyPrinter.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclObjC.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/Expr.h"
#include "clang/Basic/LangOptions.h"
#include "clang/Basic/SourceManager.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/SaveAndRestore.h"

#include "PrinterBase.h"

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
  
  class TypePrinter: public PrinterBase
  {
    another_printer::PrintingPolicy Policy;
    bool HasEmptyPlaceHolder;
    bool InsideCCAttribute;

  public:
    explicit TypePrinter(const another_printer::PrintingPolicy &Policy)
      : Policy(Policy), HasEmptyPlaceHolder(false), InsideCCAttribute(false) { }

    void printNestedNameSpecifier(NestedNameSpecifier * T, raw_ostream &OS,  const another_printer::PrintingPolicy & a_policy) const;
    void printTemplateName(const TemplateName & T, raw_ostream &OS, const another_printer::PrintingPolicy &Policy, bool SuppressNNS=false) const;
    void PrintTemplateArgumentList(raw_ostream &OS,
                          const TemplateArgumentLoc *Args, unsigned NumArgs,
                          const another_printer::PrintingPolicy &Policy) const;
    void PrintTemplateArgumentList(raw_ostream &OS,
                            const TemplateArgumentListInfo &Args,
                            const another_printer::PrintingPolicy &Policy) const;
    void PrintTemplateArgumentList( raw_ostream &OS,
                                                const TemplateArgument *Args,
                                                unsigned NumArgs, const another_printer::PrintingPolicy &Policy,
                                                bool SkipBrackets = false ) const;
                                               
    void printTemplateArgument(const TemplateArgument & T, const another_printer::PrintingPolicy &Policy,  raw_ostream &Out) const;
    
    void printQualifiers(const Qualifiers & T, raw_ostream &OS, const another_printer::PrintingPolicy& Policy, bool appendSpaceIfNonEmpty = false) const;
    void printExceptionSpecification(const FunctionProtoType * T, raw_ostream &OS,  const PrintingPolicy &Policy) const;
                                               
    void print(const Type *ty, Qualifiers qs, raw_ostream &OS,
               StringRef PlaceHolder);

    static void print(const Type *ty, Qualifiers qs,
                      raw_ostream &OS, const PrintingPolicy &policy,
                      const Twine &PlaceHolder);

    void printSplitQualType(SplitQualType split, raw_ostream &OS,
                     const another_printer::PrintingPolicy &policy, const Twine &PlaceHolder) const
    {
       return print(split.Ty, split.Quals, OS, policy, PlaceHolder);
    }
    
                      
    void printQualType(const QualType & T, raw_ostream &OS, StringRef PlaceHolder);
    void printQualType (const QualType & T, raw_ostream &OS, const another_printer::PrintingPolicy &Policy, const Twine &PlaceHolder=Twine()) const
    {
         printSplitQualType(T.split(), OS, Policy, PlaceHolder);
    }

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
