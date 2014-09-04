#include "clang/AST/ASTContext.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Tooling/Tooling.h"

 #include "clang/AST/DeclVisitor.h"
 
#include "clang/Tooling/CommonOptionsParser.h"
#include "llvm/Support/CommandLine.h"

#include "DeclPrinter.h"

using namespace clang::tooling;
using namespace llvm;
using namespace clang;

// Apply a custom category to all command-line options so that they are the
// only ones displayed.
static llvm::cl::OptionCategory AnotherASTPrinterCategory("another-ast-printer options");

// CommonOptionsParser declares HelpMessage with a description of the common
// command-line options related to the compilation database and input files.
// It's nice to have this help message in all tools.
static cl::extrahelp CommonHelp(CommonOptionsParser::HelpMessage);

// A help message for this specific tool can be added afterwards.
static cl::extrahelp MoreHelp("\nUsage:\n\tanother-ast-printer -d -t -s -o -e file.cpp --\n");

static cl::opt<bool> ShowDecl("d", cl::desc("Show declaration types (Decl) "), cl::cat(AnotherASTPrinterCategory));
static cl::opt<bool> ShowType("t", cl::desc("Show declaration types (Type) "), cl::cat(AnotherASTPrinterCategory));
static cl::opt<bool> ShowStmt("s", cl::desc("Show statement types (Stmt) "), cl::cat(AnotherASTPrinterCategory));
static cl::opt<bool> ShowOMP("o", cl::desc("Show OpenMP types (OMP) "), cl::cat(AnotherASTPrinterCategory));
static cl::opt<bool> ShowExpr("e", cl::desc("Show expression types (Expr) "), cl::cat(AnotherASTPrinterCategory));

class AnotherASTPrinter: public ASTConsumer
{
	public:
	AnotherASTPrinter(bool flag_show_decl,
				bool flag_show_type,
				bool flag_show_stmt,
				bool flag_show_omp,
				bool flag_show_expr):
	m_show_decl(flag_show_decl),
	m_show_type(flag_show_type),
	m_show_stmt(flag_show_stmt),
	m_show_omp(flag_show_omp),
	m_show_expr(flag_show_expr){}
	
	void HandleTranslationUnit(ASTContext &Context) override
	{//traverse from root
		TranslationUnitDecl *D = Context.getTranslationUnitDecl();
		//D->print(llvm::outs());
		
		another_printer::DeclPrinter Printer(llvm::outs(),
			D->getASTContext().getPrintingPolicy(),
			!m_show_decl,
			!m_show_type,
			!m_show_stmt,
			!m_show_omp,
			!m_show_expr);
		Printer.Visit(D);
	}
	private:
	bool m_show_decl;
	bool m_show_type;
	bool m_show_stmt;
	bool m_show_omp;
	bool m_show_expr;
};

class AnotherASTPrinterAction : public clang::ASTFrontendAction
{
	public:
		virtual clang::ASTConsumer *CreateASTConsumer(clang::CompilerInstance &Compiler, llvm::StringRef InFile)
		{
			llvm::outs()<<"Print file:\n"<<InFile.str()<<"\n\n";
			return new AnotherASTPrinter(ShowDecl, ShowType, ShowStmt, ShowOMP, ShowExpr);
		}
};

int main(int argc, const char **argv)
{
	CommonOptionsParser OptionsParser(argc, argv, AnotherASTPrinterCategory);
	ClangTool Tool(OptionsParser.getCompilations(), OptionsParser.getSourcePathList());

	if(ShowDecl)
	{
		llvm::outs()<<"Decl ";
	}
	
	if(ShowType)
	{
		llvm::outs()<<"ShowType ";
	}
	
	if(ShowStmt)
	{
		llvm::outs()<<"Stmt ";
	}
	
	if(ShowOMP)
	{
		llvm::outs()<<"OMP ";
	}
	
	if(ShowExpr)
	{
		llvm::outs()<<"Expr ";
	}
	llvm::outs()<<"\n";
	
	return Tool.run(newFrontendActionFactory<AnotherASTPrinterAction>().get());
}

