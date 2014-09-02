//===--- StmtPrinter.cpp - Printing implementation for Stmt ASTs ----------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the Stmt::dumpPretty/Stmt::printPretty methods, which
// pretty print the AST back out to C code.
//
//===----------------------------------------------------------------------===//


#include "StmtPrinter.h"

/*
//===----------------------------------------------------------------------===//
// Stmt method implementations
//===----------------------------------------------------------------------===//

void Stmt::dumpPretty(const ASTContext &Context) const {
  printPretty(llvm::errs(), nullptr, another_printer::PrintingPolicy(Context.getLangOpts()));
}

void Stmt::printPretty(raw_ostream &Out,
                       PrinterHelper *Helper,
                       const another_printer::PrintingPolicy &Policy,
                       unsigned Indentation) const {
  StmtPrinter P(Out, Helper, Policy, Indentation);
  P.Visit(const_cast<Stmt*>(this));
}

//===----------------------------------------------------------------------===//
// PrinterHelper
//===----------------------------------------------------------------------===//

// Implement virtual destructor.
PrinterHelper::~PrinterHelper() {}
*/

namespace another_printer{

//===----------------------------------------------------------------------===//
//  Stmt printing methods.
//===----------------------------------------------------------------------===//

void StmtPrinter::PrintStmt(const Stmt* p_stmt,
    				raw_ostream & an_out,
    				PrinterHelper * p_helper,
    				const another_printer::PrintingPolicy & a_policy,
				unsigned an_indentation)
{
	another_printer::StmtPrinter P(an_out, p_helper, a_policy, an_indentation);
	P.Visit(const_cast<Stmt*>(p_stmt));
}

/// PrintRawCompoundStmt - Print a compound stmt without indenting the {, and
/// with no newline after the }.
void StmtPrinter::PrintRawCompoundStmt(CompoundStmt *Node) {
  Out << "{\n";
  for (auto *I : Node->body())
    PrintStmt(I);

  Indent() << "}";
}

void StmtPrinter::PrintRawDecl(Decl *D) {
  D->print(Out, Policy, IndentLevel);
}

void StmtPrinter::PrintRawDeclStmt(const DeclStmt *S) {
  SmallVector<Decl*, 2> Decls(S->decls());
  Decl::printGroup(Decls.data(), Decls.size(), Out, Policy, IndentLevel);
}

void StmtPrinter::VisitNullStmt(NullStmt *Node) {
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"<NullStmt>";
  }
  
  Indent() << ";\n";
  
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"</NullStmt>";
  }
  
}

void StmtPrinter::VisitDeclStmt(DeclStmt *Node) {
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"<DeclStmt>";
  }
  Indent();
  PrintRawDeclStmt(Node);
  Out << ";\n";
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"</DeclStmt>";
  }
}

void StmtPrinter::VisitCompoundStmt(CompoundStmt *Node) {
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"<CompoundStmt>";
  }
  
  Indent();
  PrintRawCompoundStmt(Node);
  Out << "\n";
  
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"</CompoundStmt>";
  }
}

void StmtPrinter::VisitCaseStmt(CaseStmt *Node) {
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"<CaseStmt>";
  }
  
  Indent(-1) << "case ";
  PrintExpr(Node->getLHS());
  if (Node->getRHS()) {
    Out << " ... ";
    PrintExpr(Node->getRHS());
  }
  Out << ":\n";

  PrintStmt(Node->getSubStmt(), 0);
  
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"</CaseStmt>";
  }
}

void StmtPrinter::VisitDefaultStmt(DefaultStmt *Node) {
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"<DefaultStmt>";
  }
  
  Indent(-1) << "default:\n";
  PrintStmt(Node->getSubStmt(), 0);
  
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"</DefaultStmt>";
  }
}

void StmtPrinter::VisitLabelStmt(LabelStmt *Node) {
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"<LabelStmt>";
  }
  
  Indent(-1) << Node->getName() << ":\n";
  PrintStmt(Node->getSubStmt(), 0);
  
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"</LabelStmt>";
  }
}

void StmtPrinter::VisitAttributedStmt(AttributedStmt *Node) {
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"<AttributedStmt>";
  }
  
  for (const auto *Attr : Node->getAttrs()) {
    Attr->printPretty(Out, Policy);
  }

  PrintStmt(Node->getSubStmt(), 0);
  
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"</AttributedStmt>";
  }
}

void StmtPrinter::PrintRawIfStmt(IfStmt *If) {
  Out << "if (";
  if (const DeclStmt *DS = If->getConditionVariableDeclStmt())
    PrintRawDeclStmt(DS);
  else
    PrintExpr(If->getCond());
  Out << ')';

  if (CompoundStmt *CS = dyn_cast<CompoundStmt>(If->getThen())) {
    Out << ' ';
    PrintRawCompoundStmt(CS);
    Out << (If->getElse() ? ' ' : '\n');
  } else {
    Out << '\n';
    PrintStmt(If->getThen());
    if (If->getElse()) Indent();
  }

  if (Stmt *Else = If->getElse()) {
    Out << "else";

    if (CompoundStmt *CS = dyn_cast<CompoundStmt>(Else)) {
      Out << ' ';
      PrintRawCompoundStmt(CS);
      Out << '\n';
    } else if (IfStmt *ElseIf = dyn_cast<IfStmt>(Else)) {
      Out << ' ';
      PrintRawIfStmt(ElseIf);
    } else {
      Out << '\n';
      PrintStmt(If->getElse());
    }
  }
}

void StmtPrinter::VisitIfStmt(IfStmt *If) {
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"<IfStmt>";
  }
  Indent();
  PrintRawIfStmt(If);
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"</IfStmt>";
  }
}

void StmtPrinter::VisitSwitchStmt(SwitchStmt *Node) {
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"<SwitchStmt>";
  }
  
  Indent() << "switch (";
  if (const DeclStmt *DS = Node->getConditionVariableDeclStmt())
    PrintRawDeclStmt(DS);
  else
    PrintExpr(Node->getCond());
  Out << ")";

  // Pretty print compoundstmt bodies (very common).
  if (CompoundStmt *CS = dyn_cast<CompoundStmt>(Node->getBody())) {
    Out << " ";
    PrintRawCompoundStmt(CS);
    Out << "\n";
  } else {
    Out << "\n";
    PrintStmt(Node->getBody());
  }
  
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"</SwitchStmt>";
  }
}

void StmtPrinter::VisitWhileStmt(WhileStmt *Node) {
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"<WhileStmt>";
  }
  
  Indent() << "while (";
  if (const DeclStmt *DS = Node->getConditionVariableDeclStmt())
    PrintRawDeclStmt(DS);
  else
    PrintExpr(Node->getCond());
  Out << ")\n";
  PrintStmt(Node->getBody());
  
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"</WhileStmt>";
  }
}

void StmtPrinter::VisitDoStmt(DoStmt *Node) {
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"<DoStmt>";
  }
  
  Indent() << "do ";
  if (CompoundStmt *CS = dyn_cast<CompoundStmt>(Node->getBody())) {
    PrintRawCompoundStmt(CS);
    Out << " ";
  } else {
    Out << "\n";
    PrintStmt(Node->getBody());
    Indent();
  }

  Out << "while (";
  PrintExpr(Node->getCond());
  Out << ");\n";
  
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"</DoStmt>";
  }
}

void StmtPrinter::VisitForStmt(ForStmt *Node) {
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"<ForStmt>";
  }
  
  Indent() << "for (";
  if (Node->getInit()) {
    if (DeclStmt *DS = dyn_cast<DeclStmt>(Node->getInit()))
      PrintRawDeclStmt(DS);
    else
      PrintExpr(cast<Expr>(Node->getInit()));
  }
  Out << ";";
  if (Node->getCond()) {
    Out << " ";
    PrintExpr(Node->getCond());
  }
  Out << ";";
  if (Node->getInc()) {
    Out << " ";
    PrintExpr(Node->getInc());
  }
  Out << ") ";

  if (CompoundStmt *CS = dyn_cast<CompoundStmt>(Node->getBody())) {
    PrintRawCompoundStmt(CS);
    Out << "\n";
  } else {
    Out << "\n";
    PrintStmt(Node->getBody());
  }
  
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"</ForStmt>";
  }
}

void StmtPrinter::VisitObjCForCollectionStmt(ObjCForCollectionStmt *Node) {
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"<ObjCForCollectionStmt>";
  }
  
  Indent() << "for (";
  if (DeclStmt *DS = dyn_cast<DeclStmt>(Node->getElement()))
    PrintRawDeclStmt(DS);
  else
    PrintExpr(cast<Expr>(Node->getElement()));
  Out << " in ";
  PrintExpr(Node->getCollection());
  Out << ") ";

  if (CompoundStmt *CS = dyn_cast<CompoundStmt>(Node->getBody())) {
    PrintRawCompoundStmt(CS);
    Out << "\n";
  } else {
    Out << "\n";
    PrintStmt(Node->getBody());
  }
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"</ObjCForCollectionStmt>";
  }
}

void StmtPrinter::VisitCXXForRangeStmt(CXXForRangeStmt *Node) {
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"<CXXForRangeStmt>";
  }
  
  Indent() << "for (";
  another_printer::PrintingPolicy SubPolicy(Policy);
  SubPolicy.SuppressInitializers = true;
  Node->getLoopVariable()->print(Out, SubPolicy, IndentLevel);
  Out << " : ";
  PrintExpr(Node->getRangeInit());
  Out << ") {\n";
  PrintStmt(Node->getBody());
  Indent() << "}";
  if (Policy.IncludeNewlines) Out << "\n";
  
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"</CXXForRangeStmt>";
  }
}

void StmtPrinter::VisitMSDependentExistsStmt(MSDependentExistsStmt *Node) {
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"<MSDependentExistsStmt>";
  }
  
  Indent();
  if (Node->isIfExists())
    Out << "__if_exists (";
  else
    Out << "__if_not_exists (";
  
  if (NestedNameSpecifier *Qualifier
        = Node->getQualifierLoc().getNestedNameSpecifier())
    Qualifier->print(Out, Policy);
  
  Out << Node->getNameInfo() << ") ";
  
  PrintRawCompoundStmt(Node->getSubStmt());
  
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"</MSDependentExistsStmt>";
  }
}

void StmtPrinter::VisitGotoStmt(GotoStmt *Node) {
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"<GotoStmt>";
  }
  
  Indent() << "goto " << Node->getLabel()->getName() << ";";
  if (Policy.IncludeNewlines) Out << "\n";
  
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"</GotoStmt>";
  }
}

void StmtPrinter::VisitIndirectGotoStmt(IndirectGotoStmt *Node) {
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"<IndirectGotoStmt>";
  }
  
  Indent() << "goto *";
  PrintExpr(Node->getTarget());
  Out << ";";
  if (Policy.IncludeNewlines) Out << "\n";
  
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"</IndirectGotoStmt>";
  }
}

void StmtPrinter::VisitContinueStmt(ContinueStmt *Node) {
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"<ContinueStmt>";
  }
  
  Indent() << "continue;";
  if (Policy.IncludeNewlines) Out << "\n";
  
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"</ContinueStmt>";
  }
}

void StmtPrinter::VisitBreakStmt(BreakStmt *Node) {
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"<BreakStmt>";
  }
  
  Indent() << "break;";
  if (Policy.IncludeNewlines) Out << "\n";
  
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"</BreakStmt>";
  }
}


void StmtPrinter::VisitReturnStmt(ReturnStmt *Node) {
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"<ReturnStmt>";
  }
  
  Indent() << "return";
  if (Node->getRetValue()) {
    Out << " ";
    PrintExpr(Node->getRetValue());
  }
  Out << ";";
  if (Policy.IncludeNewlines) Out << "\n";
  
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"</ReturnStmt>";
  }
}


void StmtPrinter::VisitGCCAsmStmt(GCCAsmStmt *Node) {
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"<GCCAsmStmt>";
  }
  
  Indent() << "asm ";

  if (Node->isVolatile())
    Out << "volatile ";

  Out << "(";
  VisitStringLiteral(Node->getAsmString());

  // Outputs
  if (Node->getNumOutputs() != 0 || Node->getNumInputs() != 0 ||
      Node->getNumClobbers() != 0)
    Out << " : ";

  for (unsigned i = 0, e = Node->getNumOutputs(); i != e; ++i) {
    if (i != 0)
      Out << ", ";

    if (!Node->getOutputName(i).empty()) {
      Out << '[';
      Out << Node->getOutputName(i);
      Out << "] ";
    }

    VisitStringLiteral(Node->getOutputConstraintLiteral(i));
    Out << " ";
    Visit(Node->getOutputExpr(i));
  }

  // Inputs
  if (Node->getNumInputs() != 0 || Node->getNumClobbers() != 0)
    Out << " : ";

  for (unsigned i = 0, e = Node->getNumInputs(); i != e; ++i) {
    if (i != 0)
      Out << ", ";

    if (!Node->getInputName(i).empty()) {
      Out << '[';
      Out << Node->getInputName(i);
      Out << "] ";
    }

    VisitStringLiteral(Node->getInputConstraintLiteral(i));
    Out << " ";
    Visit(Node->getInputExpr(i));
  }

  // Clobbers
  if (Node->getNumClobbers() != 0)
    Out << " : ";

  for (unsigned i = 0, e = Node->getNumClobbers(); i != e; ++i) {
    if (i != 0)
      Out << ", ";

    VisitStringLiteral(Node->getClobberStringLiteral(i));
  }

  Out << ");";
  if (Policy.IncludeNewlines) Out << "\n";
  
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"</GCCAsmStmt>";
  }
}

void StmtPrinter::VisitMSAsmStmt(MSAsmStmt *Node) {
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"<MSAsmStmt>";
  }
  
  // FIXME: Implement MS style inline asm statement printer.
  Indent() << "__asm ";
  if (Node->hasBraces())
    Out << "{\n";
  Out << Node->getAsmString() << "\n";
  if (Node->hasBraces())
    Indent() << "}\n";
    
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"</MSAsmStmt>";
  }
}

void StmtPrinter::VisitCapturedStmt(CapturedStmt *Node) {
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"<CapturedStmt>";
  }
  
  PrintStmt(Node->getCapturedDecl()->getBody());
  
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"</CapturedStmt>";
  }
}

void StmtPrinter::VisitObjCAtTryStmt(ObjCAtTryStmt *Node) {
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"<ObjCAtTryStmt>";
  }
  
  Indent() << "@try";
  if (CompoundStmt *TS = dyn_cast<CompoundStmt>(Node->getTryBody())) {
    PrintRawCompoundStmt(TS);
    Out << "\n";
  }

  for (unsigned I = 0, N = Node->getNumCatchStmts(); I != N; ++I) {
    ObjCAtCatchStmt *catchStmt = Node->getCatchStmt(I);
    Indent() << "@catch(";
    if (catchStmt->getCatchParamDecl()) {
      if (Decl *DS = catchStmt->getCatchParamDecl())
        PrintRawDecl(DS);
    }
    Out << ")";
    if (CompoundStmt *CS = dyn_cast<CompoundStmt>(catchStmt->getCatchBody())) {
      PrintRawCompoundStmt(CS);
      Out << "\n";
    }
  }

  if (ObjCAtFinallyStmt *FS = static_cast<ObjCAtFinallyStmt *>(
        Node->getFinallyStmt())) {
    Indent() << "@finally";
    PrintRawCompoundStmt(dyn_cast<CompoundStmt>(FS->getFinallyBody()));
    Out << "\n";
  }
  
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"</ObjCAtTryStmt>";
  }
}

void StmtPrinter::VisitObjCAtFinallyStmt(ObjCAtFinallyStmt *Node) {
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"<ObjCAtFinallyStmt>";
  }
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"</ObjCAtFinallyStmt>";
  }
}

void StmtPrinter::VisitObjCAtCatchStmt (ObjCAtCatchStmt *Node) {
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"<ObjCAtCatchStmt>";
  }
  
  Indent() << "@catch (...) { /* todo */ } \n";
  
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"</ObjCAtCatchStmt>";
  }
}

void StmtPrinter::VisitObjCAtThrowStmt(ObjCAtThrowStmt *Node) {
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"<ObjCAtThrowStmt>";
  }
  
  Indent() << "@throw";
  if (Node->getThrowExpr()) {
    Out << " ";
    PrintExpr(Node->getThrowExpr());
  }
  Out << ";\n";
  
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"</ObjCAtThrowStmt>";
  }
}

void StmtPrinter::VisitObjCAtSynchronizedStmt(ObjCAtSynchronizedStmt *Node) {
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"<ObjCAtSynchronizedStmt>";
  }
  
  Indent() << "@synchronized (";
  PrintExpr(Node->getSynchExpr());
  Out << ")";
  PrintRawCompoundStmt(Node->getSynchBody());
  Out << "\n";
  
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"</ObjCAtSynchronizedStmt>";
  }
}

void StmtPrinter::VisitObjCAutoreleasePoolStmt(ObjCAutoreleasePoolStmt *Node) {
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"<ObjCAutoreleasePoolStmt>";
  }
  
  Indent() << "@autoreleasepool";
  PrintRawCompoundStmt(dyn_cast<CompoundStmt>(Node->getSubStmt()));
  Out << "\n";
  
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"</ObjCAutoreleasePoolStmt>";
  }
}

void StmtPrinter::PrintRawCXXCatchStmt(CXXCatchStmt *Node) {
  Out << "catch (";
  if (Decl *ExDecl = Node->getExceptionDecl())
    PrintRawDecl(ExDecl);
  else
    Out << "...";
  Out << ") ";
  PrintRawCompoundStmt(cast<CompoundStmt>(Node->getHandlerBlock()));
}

void StmtPrinter::VisitCXXCatchStmt(CXXCatchStmt *Node) {
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"<CXXCatchStmt>";
  }
  
  Indent();
  PrintRawCXXCatchStmt(Node);
  Out << "\n";
  
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"</CXXCatchStmt>";
  }
}

void StmtPrinter::VisitCXXTryStmt(CXXTryStmt *Node) {
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"<CXXTryStmt>";
  }
  
  Indent() << "try ";
  PrintRawCompoundStmt(Node->getTryBlock());
  for (unsigned i = 0, e = Node->getNumHandlers(); i < e; ++i) {
    Out << " ";
    PrintRawCXXCatchStmt(Node->getHandler(i));
  }
  Out << "\n";
  
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"</CXXTryStmt>";
  }
}

void StmtPrinter::VisitSEHTryStmt(SEHTryStmt *Node) {
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"<SEHTryStmt>";
  }
  
  Indent() << (Node->getIsCXXTry() ? "try " : "__try ");
  PrintRawCompoundStmt(Node->getTryBlock());
  SEHExceptStmt *E = Node->getExceptHandler();
  SEHFinallyStmt *F = Node->getFinallyHandler();
  if(E)
    PrintRawSEHExceptHandler(E);
  else {
    assert(F && "Must have a finally block...");
    PrintRawSEHFinallyStmt(F);
  }
  Out << "\n";
  
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"</SEHTryStmt>";
  }
}

void StmtPrinter::PrintRawSEHFinallyStmt(SEHFinallyStmt *Node) {
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"<SEHFinallyStmt>";
  }
  
  Out << "__finally ";
  PrintRawCompoundStmt(Node->getBlock());
  Out << "\n";
  
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"</SEHFinallyStmt>";
  }
}

void StmtPrinter::PrintRawSEHExceptHandler(SEHExceptStmt *Node) {
  Out << "__except (";
  VisitExpr(Node->getFilterExpr());
  Out << ")\n";
  PrintRawCompoundStmt(Node->getBlock());
  Out << "\n";
}

void StmtPrinter::VisitSEHExceptStmt(SEHExceptStmt *Node) {
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"<SEHExceptStmt>";
  }
  
  Indent();
  PrintRawSEHExceptHandler(Node);
  Out << "\n";
  
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"</SEHExceptStmt>";
  }
}

void StmtPrinter::VisitSEHFinallyStmt(SEHFinallyStmt *Node) {
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"<SEHFinallyStmt>";
  }
  
  Indent();
  PrintRawSEHFinallyStmt(Node);
  Out << "\n";
  
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"</SEHFinallyStmt>";
  }
}

void StmtPrinter::VisitSEHLeaveStmt(SEHLeaveStmt *Node) {
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"<SEHLeaveStmt>";
  }
  
  Indent() << "__leave;";
  if (Policy.IncludeNewlines) Out << "\n";
  
  if(Policy.SuppressStmtTag == false)
  {
  	Out<<"</SEHLeaveStmt>";
  }
}

//===----------------------------------------------------------------------===//
//  OpenMP clauses printing methods
//===----------------------------------------------------------------------===//

//namespace {
class OMPClausePrinter : public OMPClauseVisitor<OMPClausePrinter> {
  raw_ostream &Out;
  const another_printer::PrintingPolicy &Policy;
  /// \brief Process clauses with list of variables.
  template <typename T>
  void VisitOMPClauseList(T *Node, char StartSym);
  
  void PrintStmt(const Stmt* p_stmt,
    				raw_ostream & an_out,
    				PrinterHelper * p_helper,
    				const another_printer::PrintingPolicy & a_policy,
				unsigned an_indentation)
  {
     another_printer::StmtPrinter P(an_out, p_helper, a_policy, an_indentation);
     P.Visit(const_cast<Stmt*>(p_stmt));
  }
public:
  OMPClausePrinter(raw_ostream &Out, const another_printer::PrintingPolicy &Policy)
    : Out(Out), Policy(Policy) { }
#define OPENMP_CLAUSE(Name, Class)                              \
  void Visit##Class(Class *S);
#include "clang/Basic/OpenMPKinds.def"
};

void OMPClausePrinter::VisitOMPIfClause(OMPIfClause *Node) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPIfClause>";
  }
  
  Out << "if(";
//  Node->getCondition()->printPretty(Out, nullptr, Policy, 0);
  PrintStmt(Node->getCondition(), Out, nullptr, Policy, 0);
  Out << ")";
  
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPIfClause>";
  }
}

void OMPClausePrinter::VisitOMPFinalClause(OMPFinalClause *Node) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPFinalClause>";
  }
  
  Out << "final(";
//  Node->getCondition()->printPretty(Out, nullptr, Policy, 0);
  PrintStmt(Node->getCondition(), Out, nullptr, Policy, 0);
  Out << ")";
  
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPFinalClause>";
  }
}

void OMPClausePrinter::VisitOMPNumThreadsClause(OMPNumThreadsClause *Node) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPNumThreadsClause>";
  }
  
  Out << "num_threads(";
//  Node->getNumThreads()->printPretty(Out, nullptr, Policy, 0);
  PrintStmt(Node->getNumThreads(), Out, nullptr, Policy, 0);
  Out << ")";
  
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPNumThreadsClause>";
  }
}

void OMPClausePrinter::VisitOMPSafelenClause(OMPSafelenClause *Node) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPSafelenClause>";
  }
  
  Out << "safelen(";
//  Node->getSafelen()->printPretty(Out, nullptr, Policy, 0);
  PrintStmt(Node->getSafelen(), Out, nullptr, Policy, 0);
  Out << ")";
  
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPSafelenClause>";
  }
}

void OMPClausePrinter::VisitOMPCollapseClause(OMPCollapseClause *Node) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPCollapseClause>";
  }
  
  Out << "collapse(";
//  Node->getNumForLoops()->printPretty(Out, nullptr, Policy, 0);
  PrintStmt(Node->getNumForLoops(), Out, nullptr, Policy, 0);
  Out << ")";
  
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPCollapseClause>";
  }
}

void OMPClausePrinter::VisitOMPDefaultClause(OMPDefaultClause *Node) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPDefaultClause>";
  }
  
  Out << "default("
     << getOpenMPSimpleClauseTypeName(OMPC_default, Node->getDefaultKind())
     << ")";

  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPDefaultClause>";
  }
}

void OMPClausePrinter::VisitOMPProcBindClause(OMPProcBindClause *Node) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPProcBindClause>";
  }
  
  Out << "proc_bind("
     << getOpenMPSimpleClauseTypeName(OMPC_proc_bind, Node->getProcBindKind())
     << ")";
     
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPProcBindClause>";
  }
}

void OMPClausePrinter::VisitOMPScheduleClause(OMPScheduleClause *Node) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPScheduleClause>";
  }
  
  Out << "schedule("
     << getOpenMPSimpleClauseTypeName(OMPC_schedule, Node->getScheduleKind());
  if (Node->getChunkSize()) {
    Out << ", ";
//    Node->getChunkSize()->printPretty(Out, nullptr, Policy);
    PrintStmt(Node->getChunkSize(), Out, nullptr, Policy, 0);
  }
  Out << ")";
  
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPScheduleClause>";
  }
}

void OMPClausePrinter::VisitOMPOrderedClause(OMPOrderedClause *) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPOrderedClause>";
  }
  
  Out << "ordered";
  
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPOrderedClause>";
  }
}

void OMPClausePrinter::VisitOMPNowaitClause(OMPNowaitClause *) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPNowaitClause>";
  }
  
  Out << "nowait";
  
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPNowaitClause>";
  }
}

void OMPClausePrinter::VisitOMPUntiedClause(OMPUntiedClause *) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPUntiedClause>";
  }
  
  Out << "untied";
  
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPUntiedClause>";
  }
}

void OMPClausePrinter::VisitOMPMergeableClause(OMPMergeableClause *) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPMergeableClause>";
  }
  
  Out << "mergeable";
  
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPMergeableClause>";
  }
}

void OMPClausePrinter::VisitOMPReadClause(OMPReadClause *)
{
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPReadClause>";
  }
  Out << "read";
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPReadClause>";
  }
}

void OMPClausePrinter::VisitOMPWriteClause(OMPWriteClause *)
{
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPWriteClause>";
  }
 Out << "write";
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPWriteClause>";
  }
 }

void OMPClausePrinter::VisitOMPUpdateClause(OMPUpdateClause *) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPUpdateClause>";
  }
  Out << "update";
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPUpdateClause>";
  }
}

void OMPClausePrinter::VisitOMPCaptureClause(OMPCaptureClause *) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPCaptureClause>";
  }
  Out << "capture";
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPCaptureClause>";
  }
}

void OMPClausePrinter::VisitOMPSeqCstClause(OMPSeqCstClause *) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPSeqCstClause>";
  }
  Out << "seq_cst";
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPSeqCstClause>";
  }
}

template<typename T>
void OMPClausePrinter::VisitOMPClauseList(T *Node, char StartSym) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPClauseList>";
  }
  for (typename T::varlist_iterator I = Node->varlist_begin(),
                                    E = Node->varlist_end();
         I != E; ++I) {
    assert(*I && "Expected non-null Stmt");
    if (DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(*I)) {
      Out << (I == Node->varlist_begin() ? StartSym : ',');
      cast<NamedDecl>(DRE->getDecl())->printQualifiedName(Out);
    } else {
      Out << (I == Node->varlist_begin() ? StartSym : ',');
//      (*I)->printPretty(Out, nullptr, Policy, 0);
      PrintStmt((*I), Out, nullptr, Policy, 0);
    }
  }
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPClauseList>";
  }
}

void OMPClausePrinter::VisitOMPPrivateClause(OMPPrivateClause *Node) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPPrivateClause>";
  }
  
  if (!Node->varlist_empty()) {
    Out << "private";
    VisitOMPClauseList(Node, '(');
    Out << ")";
  }
  
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPPrivateClause>";
  }
}

void OMPClausePrinter::VisitOMPFirstprivateClause(OMPFirstprivateClause *Node) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPFirstprivateClause>";
  }
  
  if (!Node->varlist_empty()) {
    Out << "firstprivate";
    VisitOMPClauseList(Node, '(');
    Out << ")";
  }
  
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPFirstprivateClause>";
  }
}

void OMPClausePrinter::VisitOMPLastprivateClause(OMPLastprivateClause *Node) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPLastprivateClause>";
  }
  
  if (!Node->varlist_empty()) {
    Out << "lastprivate";
    VisitOMPClauseList(Node, '(');
    Out << ")";
  }
  
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPLastprivateClause>";
  }
}

void OMPClausePrinter::VisitOMPSharedClause(OMPSharedClause *Node) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPSharedClause>";
  }
  
  if (!Node->varlist_empty()) {
    Out << "shared";
    VisitOMPClauseList(Node, '(');
    Out << ")";
  }
  
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPSharedClause>";
  }
}

void OMPClausePrinter::VisitOMPReductionClause(OMPReductionClause *Node) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPReductionClause>";
  }
  
  if (!Node->varlist_empty()) {
    Out << "reduction(";
    NestedNameSpecifier *QualifierLoc =
        Node->getQualifierLoc().getNestedNameSpecifier();
    OverloadedOperatorKind OOK =
        Node->getNameInfo().getName().getCXXOverloadedOperator();
    if (QualifierLoc == nullptr && OOK != OO_None) {
      // Print reduction identifier in C format
      Out << getOperatorSpelling(OOK);
    } else {
      // Use C++ format
      if (QualifierLoc != nullptr)
        QualifierLoc->print(Out, Policy);
      Out << Node->getNameInfo();
    }
    Out << ":";
    VisitOMPClauseList(Node, ' ');
    Out << ")";
  }
  
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPReductionClause>";
  }
}

void OMPClausePrinter::VisitOMPLinearClause(OMPLinearClause *Node) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPLinearClause>";
  }
  
  if (!Node->varlist_empty()) {
    Out << "linear";
    VisitOMPClauseList(Node, '(');
    if (Node->getStep() != nullptr) {
      Out << ": ";
//      Node->getStep()->printPretty(Out, nullptr, Policy, 0);
      PrintStmt(Node->getStep(), Out, nullptr, Policy, 0);
    }
    Out << ")";
  }
  
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPLinearClause>";
  }
}

void OMPClausePrinter::VisitOMPAlignedClause(OMPAlignedClause *Node) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPAlignedClause>";
  }
  
  if (!Node->varlist_empty()) {
    Out << "aligned";
    VisitOMPClauseList(Node, '(');
    if (Node->getAlignment() != nullptr) {
      Out << ": ";
//      Node->getAlignment()->printPretty(Out, nullptr, Policy, 0);
      PrintStmt(Node->getAlignment(), Out, nullptr, Policy, 0);
    }
    Out << ")";
  }
  
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPAlignedClause>";
  }
}

void OMPClausePrinter::VisitOMPCopyinClause(OMPCopyinClause *Node) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPCopyinClause>";
  }
  
  if (!Node->varlist_empty()) {
    Out << "copyin";
    VisitOMPClauseList(Node, '(');
    Out << ")";
  }
  
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPCopyinClause>";
  }
}

void OMPClausePrinter::VisitOMPCopyprivateClause(OMPCopyprivateClause *Node) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPCopyprivateClause>";
  }
  
  if (!Node->varlist_empty()) {
    Out << "copyprivate";
    VisitOMPClauseList(Node, '(');
    Out << ")";
  }
  
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPCopyprivateClause>";
  }
}

void OMPClausePrinter::VisitOMPFlushClause(OMPFlushClause *Node) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPFlushClause>";
  }
  
  if (!Node->varlist_empty()) {
    VisitOMPClauseList(Node, '(');
    Out << ")";
  }
  
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPFlushClause>";
  }
}

//}//namespace

//===----------------------------------------------------------------------===//
//  OpenMP directives printing methods
//===----------------------------------------------------------------------===//

void StmtPrinter::PrintOMPExecutableDirective(OMPExecutableDirective *S) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPExecutableDirective>";
  }
  
  OMPClausePrinter Printer(Out, Policy);
  ArrayRef<OMPClause *> Clauses = S->clauses();
  for (ArrayRef<OMPClause *>::iterator I = Clauses.begin(), E = Clauses.end();
       I != E; ++I)
    if (*I && !(*I)->isImplicit()) {
      Printer.Visit(*I);
      Out << ' ';
    }
  Out << "\n";
  if (S->hasAssociatedStmt() && S->getAssociatedStmt()) {
    assert(isa<CapturedStmt>(S->getAssociatedStmt()) &&
           "Expected captured statement!");
    Stmt *CS = cast<CapturedStmt>(S->getAssociatedStmt())->getCapturedStmt();
    PrintStmt(CS);
  }
  
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPExecutableDirective>";
  }
}

void StmtPrinter::VisitOMPParallelDirective(OMPParallelDirective *Node) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPParallelDirective>";
  }
  
  Indent() << "#pragma omp parallel ";
  PrintOMPExecutableDirective(Node);
  
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPParallelDirective>";
  }
}

void StmtPrinter::VisitOMPSimdDirective(OMPSimdDirective *Node) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPSimdDirective>";
  }
  
  Indent() << "#pragma omp simd ";
  PrintOMPExecutableDirective(Node);
  
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPSimdDirective>";
  }
}

void StmtPrinter::VisitOMPForDirective(OMPForDirective *Node) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPForDirective>";
  }
  
  Indent() << "#pragma omp for ";
  PrintOMPExecutableDirective(Node);
  
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPForDirective>";
  }
}

void StmtPrinter::VisitOMPSectionsDirective(OMPSectionsDirective *Node) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPSectionsDirective>";
  }
  
  Indent() << "#pragma omp sections ";
  PrintOMPExecutableDirective(Node);
  
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPSectionsDirective>";
  }
}

void StmtPrinter::VisitOMPSectionDirective(OMPSectionDirective *Node) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPSectionDirective>";
  }
  
  Indent() << "#pragma omp section";
  PrintOMPExecutableDirective(Node);
  
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPSectionDirective>";
  }
}

void StmtPrinter::VisitOMPSingleDirective(OMPSingleDirective *Node) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPSingleDirective>";
  }
  
  Indent() << "#pragma omp single ";
  PrintOMPExecutableDirective(Node);
  
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPSingleDirective>";
  }
}

void StmtPrinter::VisitOMPMasterDirective(OMPMasterDirective *Node) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPMasterDirective>";
  }
  
  Indent() << "#pragma omp master";
  PrintOMPExecutableDirective(Node);
  
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPMasterDirective>";
  }
}

void StmtPrinter::VisitOMPCriticalDirective(OMPCriticalDirective *Node) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPCriticalDirective>";
  }
  
  Indent() << "#pragma omp critical";
  if (Node->getDirectiveName().getName()) {
    Out << " (";
    Node->getDirectiveName().printName(Out);
    Out << ")";
  }
  PrintOMPExecutableDirective(Node);
  
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPCriticalDirective>";
  }
}

void StmtPrinter::VisitOMPParallelForDirective(OMPParallelForDirective *Node) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPParallelForDirective>";
  }
  
  Indent() << "#pragma omp parallel for ";
  PrintOMPExecutableDirective(Node);
  
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPParallelForDirective>";
  }
}

void StmtPrinter::VisitOMPParallelSectionsDirective(
    OMPParallelSectionsDirective *Node) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPParallelSectionsDirective>";
  }
  
  Indent() << "#pragma omp parallel sections ";
  PrintOMPExecutableDirective(Node);
  
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPParallelSectionsDirective>";
  }
}

void StmtPrinter::VisitOMPTaskDirective(OMPTaskDirective *Node) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPTaskDirective>";
  }
  
  Indent() << "#pragma omp task ";
  PrintOMPExecutableDirective(Node);
  
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPTaskDirective>";
  }
}

void StmtPrinter::VisitOMPTaskyieldDirective(OMPTaskyieldDirective *Node) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPTaskyieldDirective>";
  }
  
  Indent() << "#pragma omp taskyield";
  PrintOMPExecutableDirective(Node);
  
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPTaskyieldDirective>";
  }
}

void StmtPrinter::VisitOMPBarrierDirective(OMPBarrierDirective *Node) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPBarrierDirective>";
  }
  
  Indent() << "#pragma omp barrier";
  PrintOMPExecutableDirective(Node);
  
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPBarrierDirective>";
  }
}

void StmtPrinter::VisitOMPTaskwaitDirective(OMPTaskwaitDirective *Node) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPTaskwaitDirective>";
  }
  
  Indent() << "#pragma omp taskwait";
  PrintOMPExecutableDirective(Node);
  
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPTaskwaitDirective>";
  }
}

void StmtPrinter::VisitOMPFlushDirective(OMPFlushDirective *Node) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPFlushDirective>";
  }
  
  Indent() << "#pragma omp flush ";
  PrintOMPExecutableDirective(Node);
  
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPFlushDirective>";
  }
}

void StmtPrinter::VisitOMPOrderedDirective(OMPOrderedDirective *Node) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPOrderedDirective>";
  }
  
  Indent() << "#pragma omp ordered";
  PrintOMPExecutableDirective(Node);
  
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPOrderedDirective>";
  }
}

void StmtPrinter::VisitOMPAtomicDirective(OMPAtomicDirective *Node) {
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"<OMPAtomicDirective>";
  }
  
  Indent() << "#pragma omp atomic ";
  PrintOMPExecutableDirective(Node);
  
  if(Policy.SuppressOMPTag == false)
  {
  	Out<<"</OMPAtomicDirective>";
  }
}

//===----------------------------------------------------------------------===//
//  Expr printing methods.
//===----------------------------------------------------------------------===//

void StmtPrinter::VisitDeclRefExpr(DeclRefExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<DeclRefExpr>";
  }
  
  if (NestedNameSpecifier *Qualifier = Node->getQualifier())
    Qualifier->print(Out, Policy);
  if (Node->hasTemplateKeyword())
    Out << "template ";
  Out << Node->getNameInfo();
  if (Node->hasExplicitTemplateArgs())
    TemplateSpecializationType::PrintTemplateArgumentList(
        Out, Node->getTemplateArgs(), Node->getNumTemplateArgs(), Policy);
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</DeclRefExpr>";
  }
}

void StmtPrinter::VisitDependentScopeDeclRefExpr(
                                           DependentScopeDeclRefExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<DependentScopeDeclRefExpr>";
  }
  
  if (NestedNameSpecifier *Qualifier = Node->getQualifier())
    Qualifier->print(Out, Policy);
  if (Node->hasTemplateKeyword())
    Out << "template ";
  Out << Node->getNameInfo();
  if (Node->hasExplicitTemplateArgs())
    TemplateSpecializationType::PrintTemplateArgumentList(
        Out, Node->getTemplateArgs(), Node->getNumTemplateArgs(), Policy);
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</DependentScopeDeclRefExpr>";
  }
}

void StmtPrinter::VisitUnresolvedLookupExpr(UnresolvedLookupExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<UnresolvedLookupExpr>";
  }
  
  if (Node->getQualifier())
    Node->getQualifier()->print(Out, Policy);
  if (Node->hasTemplateKeyword())
    Out << "template ";
  Out << Node->getNameInfo();
  if (Node->hasExplicitTemplateArgs())
    TemplateSpecializationType::PrintTemplateArgumentList(
        Out, Node->getTemplateArgs(), Node->getNumTemplateArgs(), Policy);
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</UnresolvedLookupExpr>";
  }
}

void StmtPrinter::VisitObjCIvarRefExpr(ObjCIvarRefExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<ObjCIvarRefExpr>";
  }
  
  if (Node->getBase()) {
    PrintExpr(Node->getBase());
    Out << (Node->isArrow() ? "->" : ".");
  }
  Out << *Node->getDecl();
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</ObjCIvarRefExpr>";
  }
}

void StmtPrinter::VisitObjCPropertyRefExpr(ObjCPropertyRefExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<ObjCPropertyRefExpr>";
  }
  
  if (Node->isSuperReceiver())
    Out << "super.";
  else if (Node->isObjectReceiver() && Node->getBase()) {
    PrintExpr(Node->getBase());
    Out << ".";
  } else if (Node->isClassReceiver() && Node->getClassReceiver()) {
    Out << Node->getClassReceiver()->getName() << ".";
  }

  if (Node->isImplicitProperty())
    Node->getImplicitPropertyGetter()->getSelector().print(Out);
  else
    Out << Node->getExplicitProperty()->getName();
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</ObjCPropertyRefExpr>";
  }
}

void StmtPrinter::VisitObjCSubscriptRefExpr(ObjCSubscriptRefExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<ObjCSubscriptRefExpr>";
  }
  
  PrintExpr(Node->getBaseExpr());
  Out << "[";
  PrintExpr(Node->getKeyExpr());
  Out << "]";
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</ObjCSubscriptRefExpr>";
  }
}

void StmtPrinter::VisitPredefinedExpr(PredefinedExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<PredefinedExpr>";
  }
  
  switch (Node->getIdentType()) {
    default:
      llvm_unreachable("unknown case");
    case PredefinedExpr::Func:
      Out << "__func__";
      break;
    case PredefinedExpr::Function:
      Out << "__FUNCTION__";
      break;
    case PredefinedExpr::FuncDName:
      Out << "__FUNCDNAME__";
      break;
    case PredefinedExpr::FuncSig:
      Out << "__FUNCSIG__";
      break;
    case PredefinedExpr::LFunction:
      Out << "L__FUNCTION__";
      break;
    case PredefinedExpr::PrettyFunction:
      Out << "__PRETTY_FUNCTION__";
      break;
  }
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</PredefinedExpr>";
  }
}

void StmtPrinter::VisitCharacterLiteral(CharacterLiteral *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<CharacterLiteral>";
  }
  
  unsigned value = Node->getValue();

  switch (Node->getKind()) {
  case CharacterLiteral::Ascii: break; // no prefix.
  case CharacterLiteral::Wide:  Out << 'L'; break;
  case CharacterLiteral::UTF16: Out << 'u'; break;
  case CharacterLiteral::UTF32: Out << 'U'; break;
  }

  switch (value) {
  case '\\':
    Out << "'\\\\'";
    break;
  case '\'':
    Out << "'\\''";
    break;
  case '\a':
    // TODO: K&R: the meaning of '\\a' is different in traditional C
    Out << "'\\a'";
    break;
  case '\b':
    Out << "'\\b'";
    break;
  // Nonstandard escape sequence.
  /*case '\e':
    Out << "'\\e'";
    break;*/
  case '\f':
    Out << "'\\f'";
    break;
  case '\n':
    Out << "'\\n'";
    break;
  case '\r':
    Out << "'\\r'";
    break;
  case '\t':
    Out << "'\\t'";
    break;
  case '\v':
    Out << "'\\v'";
    break;
  default:
    if (value < 256 && isPrintable((unsigned char)value))
      Out << "'" << (char)value << "'";
    else if (value < 256)
      Out << "'\\x" << llvm::format("%02x", value) << "'";
    else if (value <= 0xFFFF)
      Out << "'\\u" << llvm::format("%04x", value) << "'";
    else
      Out << "'\\U" << llvm::format("%08x", value) << "'";
  }
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</CharacterLiteral>";
  }
}

void StmtPrinter::VisitIntegerLiteral(IntegerLiteral *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<IntegerLiteral>";
  }
  
  bool isSigned = Node->getType()->isSignedIntegerType();
  Out << Node->getValue().toString(10, isSigned);

  // Emit suffixes.  Integer literals are always a builtin integer type.
  switch (Node->getType()->getAs<BuiltinType>()->getKind()) {
  default: llvm_unreachable("Unexpected type for integer literal!");
  case BuiltinType::SChar:     Out << "i8"; break;
  case BuiltinType::UChar:     Out << "Ui8"; break;
  case BuiltinType::Short:     Out << "i16"; break;
  case BuiltinType::UShort:    Out << "Ui16"; break;
  case BuiltinType::Int:       break; // no suffix.
  case BuiltinType::UInt:      Out << 'U'; break;
  case BuiltinType::Long:      Out << 'L'; break;
  case BuiltinType::ULong:     Out << "UL"; break;
  case BuiltinType::LongLong:  Out << "LL"; break;
  case BuiltinType::ULongLong: Out << "ULL"; break;
  case BuiltinType::Int128:    Out << "i128"; break;
  case BuiltinType::UInt128:   Out << "Ui128"; break;
  }
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</IntegerLiteral>";
  }
}

static void PrintFloatingLiteral(raw_ostream &Out, FloatingLiteral *Node,
                                 bool PrintSuffix) {
  SmallString<16> Str;
  Node->getValue().toString(Str);
  Out << Str;
  if (Str.find_first_not_of("-0123456789") == StringRef::npos)
    Out << '.'; // Trailing dot in order to separate from ints.

  if (!PrintSuffix)
    return;

  // Emit suffixes.  Float literals are always a builtin float type.
  switch (Node->getType()->getAs<BuiltinType>()->getKind()) {
  default: llvm_unreachable("Unexpected type for float literal!");
  case BuiltinType::Half:       break; // FIXME: suffix?
  case BuiltinType::Double:     break; // no suffix.
  case BuiltinType::Float:      Out << 'F'; break;
  case BuiltinType::LongDouble: Out << 'L'; break;
  }
}

void StmtPrinter::VisitFloatingLiteral(FloatingLiteral *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<FloatingLiteral>";
  }
  
  PrintFloatingLiteral(Out, Node, /*PrintSuffix=*/true);
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</FloatingLiteral>";
  }
}

void StmtPrinter::VisitImaginaryLiteral(ImaginaryLiteral *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<ImaginaryLiteral>";
  }
  
  PrintExpr(Node->getSubExpr());
  Out << "i";
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</ImaginaryLiteral>";
  }
}

void StmtPrinter::VisitStringLiteral(StringLiteral *Str) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<StringLiteral>";
  }
  
  Str->outputString(Out);
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</StringLiteral>";
  }
}
void StmtPrinter::VisitParenExpr(ParenExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<ParenExpr>";
  }
  
  Out << "(";
  PrintExpr(Node->getSubExpr());
  Out << ")";
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</ParenExpr>";
  }
}
void StmtPrinter::VisitUnaryOperator(UnaryOperator *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<UnaryOperator>";
  }
  
  if (!Node->isPostfix()) {
    Out << UnaryOperator::getOpcodeStr(Node->getOpcode());

    // Print a space if this is an "identifier operator" like __real, or if
    // it might be concatenated incorrectly like '+'.
    switch (Node->getOpcode()) {
    default: break;
    case UO_Real:
    case UO_Imag:
    case UO_Extension:
      Out << ' ';
      break;
    case UO_Plus:
    case UO_Minus:
      if (isa<UnaryOperator>(Node->getSubExpr()))
        Out << ' ';
      break;
    }
  }
  PrintExpr(Node->getSubExpr());

  if (Node->isPostfix())
    Out << UnaryOperator::getOpcodeStr(Node->getOpcode());
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</UnaryOperator>";
  }
}

void StmtPrinter::VisitOffsetOfExpr(OffsetOfExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<OffsetOfExpr>";
  }
  
  Out << "__builtin_offsetof(";
  Node->getTypeSourceInfo()->getType().print(Out, Policy);
  Out << ", ";
  bool PrintedSomething = false;
  for (unsigned i = 0, n = Node->getNumComponents(); i < n; ++i) {
    OffsetOfExpr::OffsetOfNode ON = Node->getComponent(i);
    if (ON.getKind() == OffsetOfExpr::OffsetOfNode::Array) {
      // Array node
      Out << "[";
      PrintExpr(Node->getIndexExpr(ON.getArrayExprIndex()));
      Out << "]";
      PrintedSomething = true;
      continue;
    }

    // Skip implicit base indirections.
    if (ON.getKind() == OffsetOfExpr::OffsetOfNode::Base)
      continue;

    // Field or identifier node.
    IdentifierInfo *Id = ON.getFieldName();
    if (!Id)
      continue;
    
    if (PrintedSomething)
      Out << ".";
    else
      PrintedSomething = true;
    Out << Id->getName();    
  }
  Out << ")";
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</OffsetOfExpr>";
  }
}

void StmtPrinter::VisitUnaryExprOrTypeTraitExpr(UnaryExprOrTypeTraitExpr *Node){
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<UnaryExprOrTypeTraitExpr>";
  }
  
  switch(Node->getKind()) {
  case UETT_SizeOf:
    Out << "sizeof";
    break;
  case UETT_AlignOf:
    if (Policy.LangOpts.CPlusPlus)
      Out << "alignof";
    else if (Policy.LangOpts.C11)
      Out << "_Alignof";
    else
      Out << "__alignof";
    break;
  case UETT_VecStep:
    Out << "vec_step";
    break;
  }
  if (Node->isArgumentType()) {
    Out << '(';
    Node->getArgumentType().print(Out, Policy);
    Out << ')';
  } else {
    Out << " ";
    PrintExpr(Node->getArgumentExpr());
  }
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</UnaryExprOrTypeTraitExpr>";
  }
}

void StmtPrinter::VisitGenericSelectionExpr(GenericSelectionExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<GenericSelectionExpr>";
  }
  
  Out << "_Generic(";
  PrintExpr(Node->getControllingExpr());
  for (unsigned i = 0; i != Node->getNumAssocs(); ++i) {
    Out << ", ";
    QualType T = Node->getAssocType(i);
    if (T.isNull())
      Out << "default";
    else
      T.print(Out, Policy);
    Out << ": ";
    PrintExpr(Node->getAssocExpr(i));
  }
  Out << ")";
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</GenericSelectionExpr>";
  }
}

void StmtPrinter::VisitArraySubscriptExpr(ArraySubscriptExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<ArraySubscriptExpr>";
  }
  
  PrintExpr(Node->getLHS());
  Out << "[";
  PrintExpr(Node->getRHS());
  Out << "]";
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</ArraySubscriptExpr>";
  }
}

void StmtPrinter::PrintCallArgs(CallExpr *Call) {
  for (unsigned i = 0, e = Call->getNumArgs(); i != e; ++i) {
    if (isa<CXXDefaultArgExpr>(Call->getArg(i))) {
      // Don't print any defaulted arguments
      break;
    }

    if (i) Out << ", ";
    PrintExpr(Call->getArg(i));
  }
}

void StmtPrinter::VisitCallExpr(CallExpr *Call) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<CallExpr>";
  }
  
  PrintExpr(Call->getCallee());
  Out << "(";
  PrintCallArgs(Call);
  Out << ")";
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</CallExpr>";
  }
}
void StmtPrinter::VisitMemberExpr(MemberExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<MemberExpr>";
  }
  
  // FIXME: Suppress printing implicit bases (like "this")
  PrintExpr(Node->getBase());

  MemberExpr *ParentMember = dyn_cast<MemberExpr>(Node->getBase());
  FieldDecl  *ParentDecl   = ParentMember
    ? dyn_cast<FieldDecl>(ParentMember->getMemberDecl()) : nullptr;

  if (!ParentDecl || !ParentDecl->isAnonymousStructOrUnion())
    Out << (Node->isArrow() ? "->" : ".");

  if (FieldDecl *FD = dyn_cast<FieldDecl>(Node->getMemberDecl()))
    if (FD->isAnonymousStructOrUnion())
      return;

  if (NestedNameSpecifier *Qualifier = Node->getQualifier())
    Qualifier->print(Out, Policy);
  if (Node->hasTemplateKeyword())
    Out << "template ";
  Out << Node->getMemberNameInfo();
  if (Node->hasExplicitTemplateArgs())
    TemplateSpecializationType::PrintTemplateArgumentList(
        Out, Node->getTemplateArgs(), Node->getNumTemplateArgs(), Policy);
 
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</MemberExpr>";
  }
}
void StmtPrinter::VisitObjCIsaExpr(ObjCIsaExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<ObjCIsaExpr>";
  }
  
  PrintExpr(Node->getBase());
  Out << (Node->isArrow() ? "->isa" : ".isa");
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</ObjCIsaExpr>";
  }
}

void StmtPrinter::VisitExtVectorElementExpr(ExtVectorElementExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<ExtVectorElementExpr>";
  }
  
  PrintExpr(Node->getBase());
  Out << ".";
  Out << Node->getAccessor().getName();
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</ExtVectorElementExpr>";
  }
}
void StmtPrinter::VisitCStyleCastExpr(CStyleCastExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<CStyleCastExpr>";
  }
  
  Out << '(';
  Node->getTypeAsWritten().print(Out, Policy);
  Out << ')';
  PrintExpr(Node->getSubExpr());
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</CStyleCastExpr>";
  }
}
void StmtPrinter::VisitCompoundLiteralExpr(CompoundLiteralExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<CompoundLiteralExpr>";
  }
  
  Out << '(';
  Node->getType().print(Out, Policy);
  Out << ')';
  PrintExpr(Node->getInitializer());
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</CompoundLiteralExpr>";
  }
}
void StmtPrinter::VisitImplicitCastExpr(ImplicitCastExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<ImplicitCastExpr>";
  }
  
  // No need to print anything, simply forward to the subexpression.
  PrintExpr(Node->getSubExpr());
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</ImplicitCastExpr>";
  }
}
void StmtPrinter::VisitBinaryOperator(BinaryOperator *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<BinaryOperator>";
  }
  
  PrintExpr(Node->getLHS());
  Out << " " << BinaryOperator::getOpcodeStr(Node->getOpcode()) << " ";
  PrintExpr(Node->getRHS());
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</BinaryOperator>";
  }
}
void StmtPrinter::VisitCompoundAssignOperator(CompoundAssignOperator *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<CompoundAssignOperator>";
  }
  
  PrintExpr(Node->getLHS());
  Out << " " << BinaryOperator::getOpcodeStr(Node->getOpcode()) << " ";
  PrintExpr(Node->getRHS());
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</CompoundAssignOperator>";
  }
}
void StmtPrinter::VisitConditionalOperator(ConditionalOperator *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<ConditionalOperator>";
  }
  
  PrintExpr(Node->getCond());
  Out << " ? ";
  PrintExpr(Node->getLHS());
  Out << " : ";
  PrintExpr(Node->getRHS());
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</ConditionalOperator>";
  }
}

// GNU extensions.

void
StmtPrinter::VisitBinaryConditionalOperator(BinaryConditionalOperator *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<BinaryConditionalOperator>";
  }
  
  PrintExpr(Node->getCommon());
  Out << " ?: ";
  PrintExpr(Node->getFalseExpr());
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</BinaryConditionalOperator>";
  }
}

void StmtPrinter::VisitAddrLabelExpr(AddrLabelExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<AddrLabelExpr>";
  }
  
  Out << "&&" << Node->getLabel()->getName();
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</AddrLabelExpr>";
  }
}

void StmtPrinter::VisitStmtExpr(StmtExpr *E) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<StmtExpr>";
  }
  
  Out << "(";
  PrintRawCompoundStmt(E->getSubStmt());
  Out << ")";
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</StmtExpr>";
  }
}

void StmtPrinter::VisitChooseExpr(ChooseExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<ChooseExpr>";
  }
  
  Out << "__builtin_choose_expr(";
  PrintExpr(Node->getCond());
  Out << ", ";
  PrintExpr(Node->getLHS());
  Out << ", ";
  PrintExpr(Node->getRHS());
  Out << ")";
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</ChooseExpr>";
  }
}

void StmtPrinter::VisitGNUNullExpr(GNUNullExpr *) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<GNUNullExpr>";
  }
  
  Out << "__null";
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</GNUNullExpr>";
  }
}

void StmtPrinter::VisitShuffleVectorExpr(ShuffleVectorExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<ShuffleVectorExpr>";
  }
  
  Out << "__builtin_shufflevector(";
  for (unsigned i = 0, e = Node->getNumSubExprs(); i != e; ++i) {
    if (i) Out << ", ";
    PrintExpr(Node->getExpr(i));
  }
  Out << ")";
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</ShuffleVectorExpr>";
  }
}

void StmtPrinter::VisitConvertVectorExpr(ConvertVectorExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<ConvertVectorExpr>";
  }
  
  Out << "__builtin_convertvector(";
  PrintExpr(Node->getSrcExpr());
  Out << ", ";
  Node->getType().print(Out, Policy);
  Out << ")";
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</ConvertVectorExpr>";
  }
}

void StmtPrinter::VisitInitListExpr(InitListExpr* Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<InitListExpr>";
  }
  
  if (Node->getSyntacticForm()) {
    Visit(Node->getSyntacticForm());
    return;
  }

  Out << "{ ";
  for (unsigned i = 0, e = Node->getNumInits(); i != e; ++i) {
    if (i) Out << ", ";
    if (Node->getInit(i))
      PrintExpr(Node->getInit(i));
    else
      Out << "0";
  }
  Out << " }";
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</InitListExpr>";
  }
}

void StmtPrinter::VisitParenListExpr(ParenListExpr* Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<ParenListExpr>";
  }
  
  Out << "( ";
  for (unsigned i = 0, e = Node->getNumExprs(); i != e; ++i) {
    if (i) Out << ", ";
    PrintExpr(Node->getExpr(i));
  }
  Out << " )";
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</ParenListExpr>";
  }
}

void StmtPrinter::VisitDesignatedInitExpr(DesignatedInitExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<DesignatedInitExpr>";
  }
  
  for (DesignatedInitExpr::designators_iterator D = Node->designators_begin(),
                      DEnd = Node->designators_end();
       D != DEnd; ++D) {
    if (D->isFieldDesignator()) {
      if (D->getDotLoc().isInvalid()) {
        if (IdentifierInfo *II = D->getFieldName())
          Out << II->getName() << ":";
      } else {
        Out << "." << D->getFieldName()->getName();
      }
    } else {
      Out << "[";
      if (D->isArrayDesignator()) {
        PrintExpr(Node->getArrayIndex(*D));
      } else {
        PrintExpr(Node->getArrayRangeStart(*D));
        Out << " ... ";
        PrintExpr(Node->getArrayRangeEnd(*D));
      }
      Out << "]";
    }
  }

  Out << " = ";
  PrintExpr(Node->getInit());
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</DesignatedInitExpr>";
  }
}

void StmtPrinter::VisitImplicitValueInitExpr(ImplicitValueInitExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<ImplicitValueInitExpr>";
  }
  
  if (Policy.LangOpts.CPlusPlus) {
    Out << "/*implicit*/";
    Node->getType().print(Out, Policy);
    Out << "()";
  } else {
    Out << "/*implicit*/(";
    Node->getType().print(Out, Policy);
    Out << ')';
    if (Node->getType()->isRecordType())
      Out << "{}";
    else
      Out << 0;
  }
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</ImplicitValueInitExpr>";
  }
}

void StmtPrinter::VisitVAArgExpr(VAArgExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<VAArgExpr>";
  }
  
  Out << "__builtin_va_arg(";
  PrintExpr(Node->getSubExpr());
  Out << ", ";
  Node->getType().print(Out, Policy);
  Out << ")";
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</VAArgExpr>";
  }
}

void StmtPrinter::VisitPseudoObjectExpr(PseudoObjectExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<PseudoObjectExpr>";
  }
  
  PrintExpr(Node->getSyntacticForm());
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</PseudoObjectExpr>";
  }
}

void StmtPrinter::VisitAtomicExpr(AtomicExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<AtomicExpr>";
  }
  
  const char *Name = nullptr;
  switch (Node->getOp()) {
#define BUILTIN(ID, TYPE, ATTRS)
#define ATOMIC_BUILTIN(ID, TYPE, ATTRS) \
  case AtomicExpr::AO ## ID: \
    Name = #ID "("; \
    break;
#include "clang/Basic/Builtins.def"
  }
  Out << Name;

  // AtomicExpr stores its subexpressions in a permuted order.
  PrintExpr(Node->getPtr());
  if (Node->getOp() != AtomicExpr::AO__c11_atomic_load &&
      Node->getOp() != AtomicExpr::AO__atomic_load_n) {
    Out << ", ";
    PrintExpr(Node->getVal1());
  }
  if (Node->getOp() == AtomicExpr::AO__atomic_exchange ||
      Node->isCmpXChg()) {
    Out << ", ";
    PrintExpr(Node->getVal2());
  }
  if (Node->getOp() == AtomicExpr::AO__atomic_compare_exchange ||
      Node->getOp() == AtomicExpr::AO__atomic_compare_exchange_n) {
    Out << ", ";
    PrintExpr(Node->getWeak());
  }
  if (Node->getOp() != AtomicExpr::AO__c11_atomic_init) {
    Out << ", ";
    PrintExpr(Node->getOrder());
  }
  if (Node->isCmpXChg()) {
    Out << ", ";
    PrintExpr(Node->getOrderFail());
  }
  Out << ")";
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</AtomicExpr>";
  }
}

// C++
void StmtPrinter::VisitCXXOperatorCallExpr(CXXOperatorCallExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<CXXOperatorCallExpr>";
  }
  
  const char *OpStrings[NUM_OVERLOADED_OPERATORS] = {
    "",
#define OVERLOADED_OPERATOR(Name,Spelling,Token,Unary,Binary,MemberOnly) \
    Spelling,
#include "clang/Basic/OperatorKinds.def"
  };

  OverloadedOperatorKind Kind = Node->getOperator();
  if (Kind == OO_PlusPlus || Kind == OO_MinusMinus) {
    if (Node->getNumArgs() == 1) {
      Out << OpStrings[Kind] << ' ';
      PrintExpr(Node->getArg(0));
    } else {
      PrintExpr(Node->getArg(0));
      Out << ' ' << OpStrings[Kind];
    }
  } else if (Kind == OO_Arrow) {
    PrintExpr(Node->getArg(0));
  } else if (Kind == OO_Call) {
    PrintExpr(Node->getArg(0));
    Out << '(';
    for (unsigned ArgIdx = 1; ArgIdx < Node->getNumArgs(); ++ArgIdx) {
      if (ArgIdx > 1)
        Out << ", ";
      if (!isa<CXXDefaultArgExpr>(Node->getArg(ArgIdx)))
        PrintExpr(Node->getArg(ArgIdx));
    }
    Out << ')';
  } else if (Kind == OO_Subscript) {
    PrintExpr(Node->getArg(0));
    Out << '[';
    PrintExpr(Node->getArg(1));
    Out << ']';
  } else if (Node->getNumArgs() == 1) {
    Out << OpStrings[Kind] << ' ';
    PrintExpr(Node->getArg(0));
  } else if (Node->getNumArgs() == 2) {
    PrintExpr(Node->getArg(0));
    Out << ' ' << OpStrings[Kind] << ' ';
    PrintExpr(Node->getArg(1));
  } else {
    llvm_unreachable("unknown overloaded operator");
  }
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</CXXOperatorCallExpr>";
  }
}

void StmtPrinter::VisitCXXMemberCallExpr(CXXMemberCallExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<CXXMemberCallExpr>";
  }
  
  // If we have a conversion operator call only print the argument.
  CXXMethodDecl *MD = Node->getMethodDecl();
  if (MD && isa<CXXConversionDecl>(MD)) {
    PrintExpr(Node->getImplicitObjectArgument());
    return;
  }
  VisitCallExpr(cast<CallExpr>(Node));
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</CXXMemberCallExpr>";
  }
}

void StmtPrinter::VisitCUDAKernelCallExpr(CUDAKernelCallExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<CUDAKernelCallExpr>";
  }
  
  PrintExpr(Node->getCallee());
  Out << "<<<";
  PrintCallArgs(Node->getConfig());
  Out << ">>>(";
  PrintCallArgs(Node);
  Out << ")";
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</CUDAKernelCallExpr>";
  }
}

void StmtPrinter::VisitCXXNamedCastExpr(CXXNamedCastExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<CXXNamedCastExpr>";
  }
  
  Out << Node->getCastName() << '<';
  Node->getTypeAsWritten().print(Out, Policy);
  Out << ">(";
  PrintExpr(Node->getSubExpr());
  Out << ")";
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</CXXNamedCastExpr>";
  }
}

void StmtPrinter::VisitCXXStaticCastExpr(CXXStaticCastExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<CXXStaticCastExpr>";
  }
  
  VisitCXXNamedCastExpr(Node);
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</CXXStaticCastExpr>";
  }
}

void StmtPrinter::VisitCXXDynamicCastExpr(CXXDynamicCastExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<CXXDynamicCastExpr>";
  }
  
  VisitCXXNamedCastExpr(Node);
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</CXXDynamicCastExpr>";
  }
}

void StmtPrinter::VisitCXXReinterpretCastExpr(CXXReinterpretCastExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<CXXReinterpretCastExpr>";
  }
  
  VisitCXXNamedCastExpr(Node);
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</CXXReinterpretCastExpr>";
  }
}

void StmtPrinter::VisitCXXConstCastExpr(CXXConstCastExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<CXXConstCastExpr>";
  }
  VisitCXXNamedCastExpr(Node);
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</CXXConstCastExpr>";
  }
}

void StmtPrinter::VisitCXXTypeidExpr(CXXTypeidExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<CXXTypeidExpr>";
  }
  
  Out << "typeid(";
  if (Node->isTypeOperand()) {
    Node->getTypeOperandSourceInfo()->getType().print(Out, Policy);
  } else {
    PrintExpr(Node->getExprOperand());
  }
  Out << ")";
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</CXXTypeidExpr>";
  }
}

void StmtPrinter::VisitCXXUuidofExpr(CXXUuidofExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<CXXUuidofExpr>";
  }
  
  Out << "__uuidof(";
  if (Node->isTypeOperand()) {
    Node->getTypeOperandSourceInfo()->getType().print(Out, Policy);
  } else {
    PrintExpr(Node->getExprOperand());
  }
  Out << ")";
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</CXXUuidofExpr>";
  }
}

void StmtPrinter::VisitMSPropertyRefExpr(MSPropertyRefExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<MSPropertyRefExpr>";
  }
  
  PrintExpr(Node->getBaseExpr());
  if (Node->isArrow())
    Out << "->";
  else
    Out << ".";
  if (NestedNameSpecifier *Qualifier =
      Node->getQualifierLoc().getNestedNameSpecifier())
    Qualifier->print(Out, Policy);
  Out << Node->getPropertyDecl()->getDeclName();
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</MSPropertyRefExpr>";
  }
}

void StmtPrinter::VisitUserDefinedLiteral(UserDefinedLiteral *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<UserDefinedLiteral>";
  }
  
  switch (Node->getLiteralOperatorKind()) {
  case UserDefinedLiteral::LOK_Raw:
    Out << cast<StringLiteral>(Node->getArg(0)->IgnoreImpCasts())->getString();
    break;
  case UserDefinedLiteral::LOK_Template: {
    DeclRefExpr *DRE = cast<DeclRefExpr>(Node->getCallee()->IgnoreImpCasts());
    const TemplateArgumentList *Args =
      cast<FunctionDecl>(DRE->getDecl())->getTemplateSpecializationArgs();
    assert(Args);
    const TemplateArgument &Pack = Args->get(0);
    for (const auto &P : Pack.pack_elements()) {
      char C = (char)P.getAsIntegral().getZExtValue();
      Out << C;
    }
    break;
  }
  case UserDefinedLiteral::LOK_Integer: {
    // Print integer literal without suffix.
    IntegerLiteral *Int = cast<IntegerLiteral>(Node->getCookedLiteral());
    Out << Int->getValue().toString(10, /*isSigned*/false);
    break;
  }
  case UserDefinedLiteral::LOK_Floating: {
    // Print floating literal without suffix.
    FloatingLiteral *Float = cast<FloatingLiteral>(Node->getCookedLiteral());
    PrintFloatingLiteral(Out, Float, /*PrintSuffix=*/false);
    break;
  }
  case UserDefinedLiteral::LOK_String:
  case UserDefinedLiteral::LOK_Character:
    PrintExpr(Node->getCookedLiteral());
    break;
  }
  Out << Node->getUDSuffix()->getName();
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</UserDefinedLiteral>";
  }
}

void StmtPrinter::VisitCXXBoolLiteralExpr(CXXBoolLiteralExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<CXXBoolLiteralExpr>";
  }
  
  Out << (Node->getValue() ? "true" : "false");
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</CXXBoolLiteralExpr>";
  }
}

void StmtPrinter::VisitCXXNullPtrLiteralExpr(CXXNullPtrLiteralExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<CXXNullPtrLiteralExpr>";
  }
  
  Out << "nullptr";
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</CXXNullPtrLiteralExpr>";
  }
}

void StmtPrinter::VisitCXXThisExpr(CXXThisExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<CXXThisExpr>";
  }
  
  Out << "this";
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</CXXThisExpr>";
  }
}

void StmtPrinter::VisitCXXThrowExpr(CXXThrowExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<CXXThrowExpr>";
  }
  
  if (!Node->getSubExpr())
    Out << "throw";
  else {
    Out << "throw ";
    PrintExpr(Node->getSubExpr());
  }
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</CXXThrowExpr>";
  }
}

void StmtPrinter::VisitCXXDefaultArgExpr(CXXDefaultArgExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<CXXDefaultArgExpr>";
  }
  // Nothing to print: we picked up the default argument.
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</CXXDefaultArgExpr>";
  }
}

void StmtPrinter::VisitCXXDefaultInitExpr(CXXDefaultInitExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<CXXDefaultInitExpr>";
  }
  // Nothing to print: we picked up the default initializer.
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</CXXDefaultInitExpr>";
  }
}

void StmtPrinter::VisitCXXFunctionalCastExpr(CXXFunctionalCastExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<CXXFunctionalCastExpr>";
  }
  
  Node->getType().print(Out, Policy);
  Out << "(";
  PrintExpr(Node->getSubExpr());
  Out << ")";
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</CXXFunctionalCastExpr>";
  }
}

void StmtPrinter::VisitCXXBindTemporaryExpr(CXXBindTemporaryExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<CXXBindTemporaryExpr>";
  }
  
  PrintExpr(Node->getSubExpr());
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</CXXBindTemporaryExpr>";
  }
}

void StmtPrinter::VisitCXXTemporaryObjectExpr(CXXTemporaryObjectExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<CXXTemporaryObjectExpr>";
  }
  
  Node->getType().print(Out, Policy);
  Out << "(";
  for (CXXTemporaryObjectExpr::arg_iterator Arg = Node->arg_begin(),
                                         ArgEnd = Node->arg_end();
       Arg != ArgEnd; ++Arg) {
    if (Arg->isDefaultArgument())
      break;
    if (Arg != Node->arg_begin())
      Out << ", ";
    PrintExpr(*Arg);
  }
  Out << ")";
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</CXXTemporaryObjectExpr>";
  }
}

void StmtPrinter::VisitLambdaExpr(LambdaExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<LambdaExpr>";
  }
  
  Out << '[';
  bool NeedComma = false;
  switch (Node->getCaptureDefault()) {
  case LCD_None:
    break;

  case LCD_ByCopy:
    Out << '=';
    NeedComma = true;
    break;

  case LCD_ByRef:
    Out << '&';
    NeedComma = true;
    break;
  }
  for (LambdaExpr::capture_iterator C = Node->explicit_capture_begin(),
                                 CEnd = Node->explicit_capture_end();
       C != CEnd;
       ++C) {
    if (NeedComma)
      Out << ", ";
    NeedComma = true;

    switch (C->getCaptureKind()) {
    case LCK_This:
      Out << "this";
      break;

    case LCK_ByRef:
      if (Node->getCaptureDefault() != LCD_ByRef || C->isInitCapture())
        Out << '&';
      Out << C->getCapturedVar()->getName();
      break;

    case LCK_ByCopy:
      Out << C->getCapturedVar()->getName();
      break;
    }

    if (C->isInitCapture())
      PrintExpr(C->getCapturedVar()->getInit());
  }
  Out << ']';

  if (Node->hasExplicitParameters()) {
    Out << " (";
    CXXMethodDecl *Method = Node->getCallOperator();
    NeedComma = false;
    for (auto P : Method->params()) {
      if (NeedComma) {
        Out << ", ";
      } else {
        NeedComma = true;
      }
      std::string ParamStr = P->getNameAsString();
      P->getOriginalType().print(Out, Policy, ParamStr);
    }
    if (Method->isVariadic()) {
      if (NeedComma)
        Out << ", ";
      Out << "...";
    }
    Out << ')';

    if (Node->isMutable())
      Out << " mutable";

    const FunctionProtoType *Proto
      = Method->getType()->getAs<FunctionProtoType>();
    Proto->printExceptionSpecification(Out, Policy);

    // FIXME: Attributes

    // Print the trailing return type if it was specified in the source.
    if (Node->hasExplicitResultType()) {
      Out << " -> ";
      Proto->getReturnType().print(Out, Policy);
    }
  }

  // Print the body.
  CompoundStmt *Body = Node->getBody();
  Out << ' ';
  PrintStmt(Body);
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</LambdaExpr>";
  }
}

void StmtPrinter::VisitCXXScalarValueInitExpr(CXXScalarValueInitExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<CXXScalarValueInitExpr>";
  }
  
  if (TypeSourceInfo *TSInfo = Node->getTypeSourceInfo())
    TSInfo->getType().print(Out, Policy);
  else
    Node->getType().print(Out, Policy);
  Out << "()";
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</CXXScalarValueInitExpr>";
  }
}

void StmtPrinter::VisitCXXNewExpr(CXXNewExpr *E) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<CXXNewExpr>";
  }
  
  if (E->isGlobalNew())
    Out << "::";
  Out << "new ";
  unsigned NumPlace = E->getNumPlacementArgs();
  if (NumPlace > 0 && !isa<CXXDefaultArgExpr>(E->getPlacementArg(0))) {
    Out << "(";
    PrintExpr(E->getPlacementArg(0));
    for (unsigned i = 1; i < NumPlace; ++i) {
      if (isa<CXXDefaultArgExpr>(E->getPlacementArg(i)))
        break;
      Out << ", ";
      PrintExpr(E->getPlacementArg(i));
    }
    Out << ") ";
  }
  if (E->isParenTypeId())
    Out << "(";
  std::string TypeS;
  if (Expr *Size = E->getArraySize()) {
    llvm::raw_string_ostream s(TypeS);
    s << '[';
//    Size->printPretty(s, Helper, Policy);
    PrintStmt(Size, s, Helper, Policy, 0);
    s << ']';
  }
  E->getAllocatedType().print(Out, Policy, TypeS);
  if (E->isParenTypeId())
    Out << ")";

  CXXNewExpr::InitializationStyle InitStyle = E->getInitializationStyle();
  if (InitStyle) {
    if (InitStyle == CXXNewExpr::CallInit)
      Out << "(";
    PrintExpr(E->getInitializer());
    if (InitStyle == CXXNewExpr::CallInit)
      Out << ")";
  }
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</CXXNewExpr>";
  }
}

void StmtPrinter::VisitCXXDeleteExpr(CXXDeleteExpr *E) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<CXXDeleteExpr>";
  }
  
  if (E->isGlobalDelete())
    Out << "::";
  Out << "delete ";
  if (E->isArrayForm())
    Out << "[] ";
  PrintExpr(E->getArgument());
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</CXXDeleteExpr>";
  }
}

void StmtPrinter::VisitCXXPseudoDestructorExpr(CXXPseudoDestructorExpr *E) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<CXXPseudoDestructorExpr>";
  }
  
  PrintExpr(E->getBase());
  if (E->isArrow())
    Out << "->";
  else
    Out << '.';
  if (E->getQualifier())
    E->getQualifier()->print(Out, Policy);
  Out << "~";

  if (IdentifierInfo *II = E->getDestroyedTypeIdentifier())
    Out << II->getName();
  else
    E->getDestroyedType().print(Out, Policy);
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</CXXPseudoDestructorExpr>";
  }
}

void StmtPrinter::VisitCXXConstructExpr(CXXConstructExpr *E) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<CXXConstructExpr>";
  }
  
  if (E->isListInitialization())
    Out << "{ ";

  for (unsigned i = 0, e = E->getNumArgs(); i != e; ++i) {
    if (isa<CXXDefaultArgExpr>(E->getArg(i))) {
      // Don't print any defaulted arguments
      break;
    }

    if (i) Out << ", ";
    PrintExpr(E->getArg(i));
  }

  if (E->isListInitialization())
    Out << " }";
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</CXXConstructExpr>";
  }
}

void StmtPrinter::VisitCXXStdInitializerListExpr(CXXStdInitializerListExpr *E) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<CXXStdInitializerListExpr>";
  }
  
  PrintExpr(E->getSubExpr());
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</CXXStdInitializerListExpr>";
  }
}

void StmtPrinter::VisitExprWithCleanups(ExprWithCleanups *E) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<ExprWithCleanups>";
  }
  
  // Just forward to the subexpression.
  PrintExpr(E->getSubExpr());
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</ExprWithCleanups>";
  }
}

void StmtPrinter::VisitCXXUnresolvedConstructExpr(
                                           CXXUnresolvedConstructExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<CXXUnresolvedConstructExpr>";
  }
  
  Node->getTypeAsWritten().print(Out, Policy);
  Out << "(";
  for (CXXUnresolvedConstructExpr::arg_iterator Arg = Node->arg_begin(),
                                             ArgEnd = Node->arg_end();
       Arg != ArgEnd; ++Arg) {
    if (Arg != Node->arg_begin())
      Out << ", ";
    PrintExpr(*Arg);
  }
  Out << ")";
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</CXXUnresolvedConstructExpr>";
  }
}

void StmtPrinter::VisitCXXDependentScopeMemberExpr(
                                         CXXDependentScopeMemberExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<CXXDependentScopeMemberExpr>";
  }
  
  if (!Node->isImplicitAccess()) {
    PrintExpr(Node->getBase());
    Out << (Node->isArrow() ? "->" : ".");
  }
  if (NestedNameSpecifier *Qualifier = Node->getQualifier())
    Qualifier->print(Out, Policy);
  if (Node->hasTemplateKeyword())
    Out << "template ";
  Out << Node->getMemberNameInfo();
  if (Node->hasExplicitTemplateArgs())
    TemplateSpecializationType::PrintTemplateArgumentList(
        Out, Node->getTemplateArgs(), Node->getNumTemplateArgs(), Policy);
        
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</CXXDependentScopeMemberExpr>";
  }
}

void StmtPrinter::VisitUnresolvedMemberExpr(UnresolvedMemberExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<UnresolvedMemberExpr>";
  }
  
  if (!Node->isImplicitAccess()) {
    PrintExpr(Node->getBase());
    Out << (Node->isArrow() ? "->" : ".");
  }
  if (NestedNameSpecifier *Qualifier = Node->getQualifier())
    Qualifier->print(Out, Policy);
  if (Node->hasTemplateKeyword())
    Out << "template ";
  Out << Node->getMemberNameInfo();
  if (Node->hasExplicitTemplateArgs())
    TemplateSpecializationType::PrintTemplateArgumentList(
        Out, Node->getTemplateArgs(), Node->getNumTemplateArgs(), Policy);
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</UnresolvedMemberExpr>";
  }
}

static const char *getTypeTraitName(TypeTrait TT) {
  switch (TT) {
#define TYPE_TRAIT_1(Spelling, Name, Key) \
case clang::UTT_##Name: return #Spelling;
#define TYPE_TRAIT_2(Spelling, Name, Key) \
case clang::BTT_##Name: return #Spelling;
#define TYPE_TRAIT_N(Spelling, Name, Key) \
  case clang::TT_##Name: return #Spelling;
#include "clang/Basic/TokenKinds.def"
  }
  llvm_unreachable("Type trait not covered by switch");
}

static const char *getTypeTraitName(ArrayTypeTrait ATT) {
  switch (ATT) {
  case ATT_ArrayRank:        return "__array_rank";
  case ATT_ArrayExtent:      return "__array_extent";
  }
  llvm_unreachable("Array type trait not covered by switch");
}

static const char *getExpressionTraitName(ExpressionTrait ET) {
  switch (ET) {
  case ET_IsLValueExpr:      return "__is_lvalue_expr";
  case ET_IsRValueExpr:      return "__is_rvalue_expr";
  }
  llvm_unreachable("Expression type trait not covered by switch");
}

void StmtPrinter::VisitTypeTraitExpr(TypeTraitExpr *E) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<TypeTraitExpr>";
  }
  
  Out << getTypeTraitName(E->getTrait()) << "(";
  for (unsigned I = 0, N = E->getNumArgs(); I != N; ++I) {
    if (I > 0)
      Out << ", ";
    E->getArg(I)->getType().print(Out, Policy);
  }
  Out << ")";
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</TypeTraitExpr>";
  }
}

void StmtPrinter::VisitArrayTypeTraitExpr(ArrayTypeTraitExpr *E) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<ArrayTypeTraitExpr>";
  }
  
  Out << getTypeTraitName(E->getTrait()) << '(';
  E->getQueriedType().print(Out, Policy);
  Out << ')';
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</ArrayTypeTraitExpr>";
  }
}

void StmtPrinter::VisitExpressionTraitExpr(ExpressionTraitExpr *E) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<ExpressionTraitExpr>";
  }
  
  Out << getExpressionTraitName(E->getTrait()) << '(';
  PrintExpr(E->getQueriedExpression());
  Out << ')';
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</ExpressionTraitExpr>";
  }
}

void StmtPrinter::VisitCXXNoexceptExpr(CXXNoexceptExpr *E) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<CXXNoexceptExpr>";
  }
  
  Out << "noexcept(";
  PrintExpr(E->getOperand());
  Out << ")";
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</CXXNoexceptExpr>";
  }
}

void StmtPrinter::VisitPackExpansionExpr(PackExpansionExpr *E) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<PackExpansionExpr>";
  }
  
  PrintExpr(E->getPattern());
  Out << "...";
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</PackExpansionExpr>";
  }
}

void StmtPrinter::VisitSizeOfPackExpr(SizeOfPackExpr *E) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<SizeOfPackExpr>";
  }
  
  Out << "sizeof...(" << *E->getPack() << ")";
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</SizeOfPackExpr>";
  }
}

void StmtPrinter::VisitSubstNonTypeTemplateParmPackExpr(
                                       SubstNonTypeTemplateParmPackExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<SubstNonTypeTemplateParmPackExpr>";
  }
  
  Out << *Node->getParameterPack();
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</SubstNonTypeTemplateParmPackExpr>";
  }
}

void StmtPrinter::VisitSubstNonTypeTemplateParmExpr(
                                       SubstNonTypeTemplateParmExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<SubstNonTypeTemplateParmExpr>";
  }
  
  Visit(Node->getReplacement());
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</SubstNonTypeTemplateParmExpr>";
  }
}

void StmtPrinter::VisitFunctionParmPackExpr(FunctionParmPackExpr *E) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<FunctionParmPackExpr>";
  }
  
  Out << *E->getParameterPack();
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</FunctionParmPackExpr>";
  }
}

void StmtPrinter::VisitMaterializeTemporaryExpr(MaterializeTemporaryExpr *Node){
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<MaterializeTemporaryExpr>";
  }
  
  PrintExpr(Node->GetTemporaryExpr());
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</MaterializeTemporaryExpr>";
  }
}

// Obj-C

void StmtPrinter::VisitObjCStringLiteral(ObjCStringLiteral *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<ObjCStringLiteral>";
  }
  
  Out << "@";
  VisitStringLiteral(Node->getString());
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</ObjCStringLiteral>";
  }
}

void StmtPrinter::VisitObjCBoxedExpr(ObjCBoxedExpr *E) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<ObjCBoxedExpr>";
  }
  
  Out << "@";
  Visit(E->getSubExpr());
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</ObjCBoxedExpr>";
  }
}

void StmtPrinter::VisitObjCArrayLiteral(ObjCArrayLiteral *E) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<ObjCArrayLiteral>";
  }
  
  Out << "@[ ";
  StmtRange ch = E->children();
  if (ch.first != ch.second) {
    while (1) {
      Visit(*ch.first);
      ++ch.first;
      if (ch.first == ch.second) break;
      Out << ", ";
    }
  }
  Out << " ]";
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</ObjCArrayLiteral>";
  }
}

void StmtPrinter::VisitObjCDictionaryLiteral(ObjCDictionaryLiteral *E) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<ObjCDictionaryLiteral>";
  }
  
  Out << "@{ ";
  for (unsigned I = 0, N = E->getNumElements(); I != N; ++I) {
    if (I > 0)
      Out << ", ";
    
    ObjCDictionaryElement Element = E->getKeyValueElement(I);
    Visit(Element.Key);
    Out << " : ";
    Visit(Element.Value);
    if (Element.isPackExpansion())
      Out << "...";
  }
  Out << " }";
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</ObjCDictionaryLiteral>";
  }
}

void StmtPrinter::VisitObjCEncodeExpr(ObjCEncodeExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<ObjCEncodeExpr>";
  }
  
  Out << "@encode(";
  Node->getEncodedType().print(Out, Policy);
  Out << ')';
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</ObjCEncodeExpr>";
  }
}

void StmtPrinter::VisitObjCSelectorExpr(ObjCSelectorExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<ObjCSelectorExpr>";
  }
  
  Out << "@selector(";
  Node->getSelector().print(Out);
  Out << ')';
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</ObjCSelectorExpr>";
  }
}

void StmtPrinter::VisitObjCProtocolExpr(ObjCProtocolExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<ObjCProtocolExpr>";
  }
  
  Out << "@protocol(" << *Node->getProtocol() << ')';
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</ObjCProtocolExpr>";
  }
}

void StmtPrinter::VisitObjCMessageExpr(ObjCMessageExpr *Mess) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<ObjCMessageExpr>";
  }
  
  Out << "[";
  switch (Mess->getReceiverKind()) {
  case ObjCMessageExpr::Instance:
    PrintExpr(Mess->getInstanceReceiver());
    break;

  case ObjCMessageExpr::Class:
    Mess->getClassReceiver().print(Out, Policy);
    break;

  case ObjCMessageExpr::SuperInstance:
  case ObjCMessageExpr::SuperClass:
    Out << "Super";
    break;
  }

  Out << ' ';
  Selector selector = Mess->getSelector();
  if (selector.isUnarySelector()) {
    Out << selector.getNameForSlot(0);
  } else {
    for (unsigned i = 0, e = Mess->getNumArgs(); i != e; ++i) {
      if (i < selector.getNumArgs()) {
        if (i > 0) Out << ' ';
        if (selector.getIdentifierInfoForSlot(i))
          Out << selector.getIdentifierInfoForSlot(i)->getName() << ':';
        else
           Out << ":";
      }
      else Out << ", "; // Handle variadic methods.

      PrintExpr(Mess->getArg(i));
    }
  }
  Out << "]";
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</ObjCMessageExpr>";
  }
}

void StmtPrinter::VisitObjCBoolLiteralExpr(ObjCBoolLiteralExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<ObjCBoolLiteralExpr>";
  }
  
  Out << (Node->getValue() ? "__objc_yes" : "__objc_no");
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</ObjCBoolLiteralExpr>";
  }
}

void StmtPrinter::VisitObjCIndirectCopyRestoreExpr(ObjCIndirectCopyRestoreExpr *E) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<ObjCIndirectCopyRestoreExpr>";
  }
  
  PrintExpr(E->getSubExpr());
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</ObjCIndirectCopyRestoreExpr>";
  }
}

void StmtPrinter::VisitObjCBridgedCastExpr(ObjCBridgedCastExpr *E) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<ObjCBridgedCastExpr>";
  }
  
  Out << '(' << E->getBridgeKindName();
  E->getType().print(Out, Policy);
  Out << ')';
  PrintExpr(E->getSubExpr());
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</ObjCBridgedCastExpr>";
  }
}

void StmtPrinter::VisitBlockExpr(BlockExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<BlockExpr>";
  }
  
  BlockDecl *BD = Node->getBlockDecl();
  Out << "^";

  const FunctionType *AFT = Node->getFunctionType();

  if (isa<FunctionNoProtoType>(AFT)) {
    Out << "()";
  } else if (!BD->param_empty() || cast<FunctionProtoType>(AFT)->isVariadic()) {
    Out << '(';
    for (BlockDecl::param_iterator AI = BD->param_begin(),
         E = BD->param_end(); AI != E; ++AI) {
      if (AI != BD->param_begin()) Out << ", ";
      std::string ParamStr = (*AI)->getNameAsString();
      (*AI)->getType().print(Out, Policy, ParamStr);
    }

    const FunctionProtoType *FT = cast<FunctionProtoType>(AFT);
    if (FT->isVariadic()) {
      if (!BD->param_empty()) Out << ", ";
      Out << "...";
    }
    Out << ')';
  }
  Out << "{ }";
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</BlockExpr>";
  }
}

void StmtPrinter::VisitOpaqueValueExpr(OpaqueValueExpr *Node) { 
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<OpaqueValueExpr>";
  }
  
  PrintExpr(Node->getSourceExpr());
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</OpaqueValueExpr>";
  }
}

void StmtPrinter::VisitAsTypeExpr(AsTypeExpr *Node) {
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"<AsTypeExpr>";
  }
  
  Out << "__builtin_astype(";
  PrintExpr(Node->getSrcExpr());
  Out << ", ";
  Node->getType().print(Out, Policy);
  Out << ")";
  
  if(Policy.SuppressExprTag == false)
  {
  	Out<<"</AsTypeExpr>";
  }
}


}//namespace another_printer


