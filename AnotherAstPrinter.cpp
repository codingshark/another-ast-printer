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

class AnotherAstPrinter: public ASTConsumer
{
	public:
	void HandleTranslationUnit(ASTContext &Context) override
	{//traverse from root
		TranslationUnitDecl *D = Context.getTranslationUnitDecl();
		//D->print(llvm::outs());
		
		another_printer::DeclPrinter Printer(llvm::outs(), D->getASTContext().getPrintingPolicy(), false, false, false, false, false);
		Printer.Visit(D);
	}
};

class AnotherAstPrinterAction : public clang::ASTFrontendAction
{
	public:
		virtual clang::ASTConsumer *CreateASTConsumer(clang::CompilerInstance &Compiler, llvm::StringRef InFile)
		{
			llvm::outs()<<"Print file:\n"<<InFile.str()<<"\n";
			return new AnotherAstPrinter();
		}
};

// Apply a custom category to all command-line options so that they are the
// only ones displayed.
static llvm::cl::OptionCategory AnotherAstPrinterCategory("another-ast-printer options");

// CommonOptionsParser declares HelpMessage with a description of the common
// command-line options related to the compilation database and input files.
// It's nice to have this help message in all tools.
static cl::extrahelp CommonHelp(CommonOptionsParser::HelpMessage);

// A help message for this specific tool can be added afterwards.
static cl::extrahelp MoreHelp("\nMore help text...");

int main(int argc, const char **argv)
{
	CommonOptionsParser OptionsParser(argc, argv, AnotherAstPrinterCategory);
	ClangTool Tool(OptionsParser.getCompilations(), OptionsParser.getSourcePathList());
	return Tool.run(newFrontendActionFactory<AnotherAstPrinterAction>().get());
}

