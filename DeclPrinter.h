#ifndef _DECL_PRINTER_
#define _DECL_PRINTER_

#include "clang/AST/ASTContext.h"
#include "clang/AST/Attr.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclObjC.h"
#include "clang/AST/DeclVisitor.h"
#include "clang/AST/Expr.h"
#include "clang/AST/ExprCXX.h"
#include "clang/AST/PrettyPrinter.h"
#include "clang/Basic/Module.h"
#include "llvm/Support/raw_ostream.h"

#include "PrintingPolicy.h"

using namespace clang;

namespace another_printer
{
  class DeclPrinter : public DeclVisitor<DeclPrinter> {
    raw_ostream &Out;
    another_printer::PrintingPolicy Policy;
    unsigned Indentation;
    bool PrintInstantiation;

    raw_ostream& Indent() { return Indent(Indentation); }
    raw_ostream& Indent(unsigned Indentation);
    void ProcessDeclGroup(SmallVectorImpl<Decl*>& Decls);

    void Print(AccessSpecifier AS);

    void PrintStmt(const Stmt* p_stmt,
    				raw_ostream & an_out,
    				PrinterHelper * a_helper,
    				const another_printer::PrintingPolicy & a_policy,
				unsigned an_indentation);
  public:
    DeclPrinter(raw_ostream &Out, const clang::PrintingPolicy &Policy,
    			bool a_suppress_decl_tag_flag,
    			bool a_suppress_type_tag_flag,
    			bool a_suppress_stmt_tag_flag,
    			bool a_suppress_omp_tag_flag,
    			bool a_suppress_expr_tag_flag,
                unsigned Indentation = 0, bool PrintInstantiation = false)
      : Out(Out),
      Policy(Policy, a_suppress_decl_tag_flag,
      			a_suppress_type_tag_flag,
      			a_suppress_stmt_tag_flag,
      			a_suppress_omp_tag_flag,
      			a_suppress_expr_tag_flag),
        Indentation(Indentation),
        PrintInstantiation(PrintInstantiation) { }

    DeclPrinter(raw_ostream &Out, const another_printer::PrintingPolicy &Policy,
                unsigned Indentation = 0, bool PrintInstantiation = false)
      : Out(Out), Policy(Policy), Indentation(Indentation),
        PrintInstantiation(PrintInstantiation) { }

   static void printGroup(Decl** Begin, unsigned NumDecls,
                          raw_ostream &Out, const PrintingPolicy &Policy,
                          unsigned Indentation = 0);

  static void PrintDecl(const Decl* p_decl,
    				raw_ostream & an_out,
    				const another_printer::PrintingPolicy & a_policy,
				unsigned an_indentation,
				bool PrintInstantiation = false);

    void VisitDeclContext(DeclContext *DC, bool Indent = true);

    void VisitTranslationUnitDecl(TranslationUnitDecl *D);
    void VisitTypedefDecl(TypedefDecl *D);
    void VisitTypeAliasDecl(TypeAliasDecl *D);
    void VisitEnumDecl(EnumDecl *D);
    void VisitRecordDecl(RecordDecl *D);
    void VisitEnumConstantDecl(EnumConstantDecl *D);
    void VisitEmptyDecl(EmptyDecl *D);
    void VisitFunctionDecl(FunctionDecl *D);
    void VisitFriendDecl(FriendDecl *D);
    void VisitFieldDecl(FieldDecl *D);
    void VisitVarDecl(VarDecl *D);
    void VisitLabelDecl(LabelDecl *D);
    void VisitParmVarDecl(ParmVarDecl *D);
    void VisitFileScopeAsmDecl(FileScopeAsmDecl *D);
    void VisitImportDecl(ImportDecl *D);
    void VisitStaticAssertDecl(StaticAssertDecl *D);
    void VisitNamespaceDecl(NamespaceDecl *D);
    void VisitUsingDirectiveDecl(UsingDirectiveDecl *D);
    void VisitNamespaceAliasDecl(NamespaceAliasDecl *D);
    void VisitCXXRecordDecl(CXXRecordDecl *D);
    void VisitLinkageSpecDecl(LinkageSpecDecl *D);
    void VisitTemplateDecl(const TemplateDecl *D);
    void VisitFunctionTemplateDecl(FunctionTemplateDecl *D);
    void VisitClassTemplateDecl(ClassTemplateDecl *D);
    void VisitObjCMethodDecl(ObjCMethodDecl *D);
    void VisitObjCImplementationDecl(ObjCImplementationDecl *D);
    void VisitObjCInterfaceDecl(ObjCInterfaceDecl *D);
    void VisitObjCProtocolDecl(ObjCProtocolDecl *D);
    void VisitObjCCategoryImplDecl(ObjCCategoryImplDecl *D);
    void VisitObjCCategoryDecl(ObjCCategoryDecl *D);
    void VisitObjCCompatibleAliasDecl(ObjCCompatibleAliasDecl *D);
    void VisitObjCPropertyDecl(ObjCPropertyDecl *D);
    void VisitObjCPropertyImplDecl(ObjCPropertyImplDecl *D);
    void VisitUnresolvedUsingTypenameDecl(UnresolvedUsingTypenameDecl *D);
    void VisitUnresolvedUsingValueDecl(UnresolvedUsingValueDecl *D);
    void VisitUsingDecl(UsingDecl *D);
    void VisitUsingShadowDecl(UsingShadowDecl *D);
    void VisitOMPThreadPrivateDecl(OMPThreadPrivateDecl *D);

    void PrintTemplateParameters(const TemplateParameterList *Params,
                                 const TemplateArgumentList *Args = nullptr);
    void prettyPrintAttributes(Decl *D);
    void printDeclType(QualType T, StringRef DeclName, bool Pack = false);
  };
}

#endif

